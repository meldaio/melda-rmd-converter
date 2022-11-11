import pandas as pd
import json
import os

# from yaml import safe_load
from datetime import datetime, timezone
with open('./parameters.json') as params:
    factors = json.load(params)

# closing annoying stuff that slows down the script.
pd.options.mode.chained_assignment = None # default='warn'



def calc_missing_time(user_email: str, leave_count: int) -> int:
    user_join_times = joins[joins['user_email'] == user_email]['timestamp']
    user_leave_times = leaves[leaves['user_email'] == user_email]['timestamp']
    
    # if the guy didn't come back after leaving:
    if user_join_times.count() == user_leave_times.count(): 
        leave_count = leave_count - 1
        no_come_back = True
    else: no_come_back = False

    lost_time = 0
    for i in range(leave_count):
        lost_time += pd.Timedelta(user_join_times.iloc[i+1] -
                                  user_leave_times.iloc[i]).seconds

    # if the guy didn't come back and the meeting still going on
    # might get fucked here if Esra doesn't solve the bug where
    # every left meeting sends and xAPI stetement, even if mf is a student.
    if no_come_back and meet_end.empty:
        lost_time += pd.Timedelta(datetime.now(timezone.utc) -
                                  user_leave_times.iloc[leave_count]).seconds
    # if he left for good and the meeting ended
    elif no_come_back:
        lost_time += pd.Timedelta(meet_end.iloc[0] -
                                  user_leave_times.iloc[leave_count]).seconds
    else: pass

    return lost_time

data = pd.read_csv('./output.csv', index_col=0)
# get rid of the difference between moodle and bbb data.
data['timestamp'] = pd.to_datetime(data['timestamp'], utc=True)
data.sort_values('timestamp', inplace=True)
data.reset_index(drop=True, inplace=True)

# Let's create a proof of concept first though by working on a single course.
# We'll check out the various aspects of this problem one-by-one later.
class_name = os.environ['class_name']
# class_name = "DATA SCIENCE"
class_data = data[data['course_name'] == class_name]

# Slicing a particular meeting.
meet_start = class_data[class_data['verb'] == 'create'].iloc[-1:, 0]
meet_start_iloc = meet_start.index[0]

meet_end = class_data[class_data['verb'] == 'adjourned'].iloc[-1:, 0]
# maybe another meeting from the same class was ended before.
# don't get those data. 
if meet_end.index[0] < meet_start_iloc: meet_end = pd.Series([], dtype='object')
else: pass
# +1 for including ajourned, if exists, or the last data for a meeting. 
if meet_end.empty: meet_end_iloc = class_data.iloc[-1:, :].index[0] + 1
else: meet_end_iloc = meet_end.index[0] + 1

# sm stands for "single meeting".
sm = data.iloc[meet_start_iloc:meet_end_iloc]

#- Meeting participation
# This one is a little tricky. We don't know how many times
# a student would leave a meeting and come back.
joins = sm[sm['verb'] == 'join']
leaves = sm[sm['verb'] == 'leave']

jvc = joins['user_email'].value_counts()
lvc = leaves['user_email'].value_counts()

lvs_dict = {}
for user_email in jvc.index:
    first_join = joins[joins['user_email'] == user_email]['timestamp'].iloc[0]
    late = pd.Timedelta(first_join - meet_start.iloc[0]).seconds
    lvs_dict[user_email] = late

for user_email in lvc.index:
    lvs_dict[user_email] += calc_missing_time(user_email, lvc.loc[user_email])

lost_seconds = pd.Series(lvs_dict, name="participationScore")
participationScore = lost_seconds.min() / lost_seconds
participationScore = participationScore * factors['participation']


#- Are you there from Melda
responded = sm[sm['verb'] == 'responded']
response_means = responded.groupby('user_email').mean()['response_time']
responseScore = response_means.min() / response_means
responseScore = responseScore * factors['response']
responseScore.rename('responseScore', inplace=True)

#- Raise hand
raiseHandCount = sm[sm['verb'] == 'requested-attention']['user_email'].value_counts()
raiseHandScore = raiseHandCount / raiseHandCount.max()
raiseHandScore = raiseHandScore * factors['raiseHand']
raiseHandScore.rename('raiseHandScore', inplace=True)

#- BBB chat
messageCount = sm[sm['verb'] == 'commented']['user_email'].value_counts()
messageScore = messageCount / messageCount.max()
messageScore = messageScore * factors['message']
messageScore.rename('messageScore', inplace=True)

#- Audio
talkTime = sm[sm['verb'] == 'talked-with'].groupby('user_email').sum()['talk_time']
talkScore = talkTime / talkTime.max()
talkScore = talkScore * factors['talk']
talkScore.rename('talkScore', inplace=True)

#- Questions
answered = sm[sm['verb'] == 'answered']

e = pd.DataFrame()
for name in answered['interaction_name'].unique():
    filter_questions = answered[answered['interaction_name'] == name]
    successful_answers = filter_questions[filter_questions['score_scaled'] > 0]
    if successful_answers.empty: answer_dur = 0
    else: answer_dur = successful_answers['answer_duration'].min()
    filter_questions['scaled_answer_time'] = successful_answers['answer_duration'].min() / filter_questions['answer_duration']
    e = pd.concat([e, filter_questions])

if e.empty: 
    questions = pd.DataFrame()
    questionScore = pd.DataFrame()
else:
    e['score_scaled_w_A_dur'] = e['scaled_answer_time'] * e['score_scaled']
    questions = pd.DataFrame(e[['user_email', 'interaction_name', 'score_scaled',
                                'answer_duration']]).set_index('user_email')
    questionScore = e.groupby('user_email').mean()['score_scaled_w_A_dur']
    questionScore = questionScore * factors['question']
    questionScore.rename('questionScore', inplace=True)

###----------
##- Webcam
#webcamopens = class_data[class_data['verb'] == 'frame/entered'].sort_values('timestamp', ascending=False)
#webcamopens = webcamopens[['user_email', 'webcam_toggle']]
#webcamcloses = class_data[class_data['verb'] == 'frame/exited'].sort_values('timestamp', ascending=False)
## More webcam closes than there are webcamopens. That's fucked.
## Gotta think of some other way to generate xAPI statements for webcam opens.
#webcamcloses = webcamcloses[['user_email', 'webcam_toggle']].iloc[[0,1,3]]
#
## webcamTime = pd.Series(webcamcloses['webcam_toggle'].values - webcamopens['webcam_toggle'].values)
## webcamTime_ = pd.concat([webcamcloses['user_email'], webcamTime], axis=0)
###----------

# make the algorithm resilient by filling NaN values with 0.
if e.empty:
    score_table_ = pd.DataFrame.from_dict([participationScore, 
                                      talkScore, responseScore, raiseHandScore,
                                      messageScore]).fillna(0)
else:
    score_table_ = pd.DataFrame.from_dict([participationScore, questionScore, 
                                          talkScore, responseScore, raiseHandScore,
                                          messageScore]).fillna(0)

score_table = score_table_.transpose()

scores = score_table.sum(axis=1)
scaled_scores = (scores / scores.max()) * 100

#- Preparing data for the dashboard
# Global meeting info
ms = sm[sm['verb'] == 'create'].iloc[0]
meet_not_ended = meet_end.empty
if meet_not_ended:
    meet_ended = False
    meet_end_time = 0
else:
    meet_ended = True
    meet_end_time = str(meet_end.values[0])

# Last interaction data
unique_mails = sm[(sm['verb'] != 'create') & 
                  (sm['verb'] != 'adjourned')]['user_email'].unique()

l = {}
for user_email in unique_mails:
    last_action = sm[sm['user_email'] == user_email].iloc[-1]
    l[last_action['user_email']] = str(last_action['timestamp'])


# Question
q = {}
for user_email in questions.index.unique():
    suq = questions[questions.index == user_email]
    suq['score_scaled'] = suq['score_scaled'] * 100
    q[user_email] = suq.values.tolist()


json_ = {
    "classroom": {
        "name": ms['course_name'],
        "mid": ms['course_id'],
        "instructor": ms['user_name']
    },
    "meeting": {
        "start": str(meet_start.values[0]),
        "end": meet_end_time,
        "isEnded": meet_ended
    },
    # "students": {
        # "last_interaction_time": l,
        # "questions": q,
        # "absent_seconds": lost_seconds.astype(int).to_dict(), 
        # "total_talk_in_sec": talkTime.astype(int).to_dict(),
        # "mean_response_time_in_sec": response_means.astype(int).to_dict(),
        # "raiseHand": raiseHandCount.to_dict(),
        # "messages": messageCount.to_dict(),
        # "scores": score_table_.astype(int).to_dict(),
        # "totalScores": scaled_scores.astype(int).to_dict()
    # }
    "students": {
        "last_interaction_time": l,
        "questions": q,
        "absent_seconds": lost_seconds.to_dict(), 
        "total_talk_in_sec": talkTime.to_dict(),
        "mean_response_time_in_sec": response_means.to_dict(),
        "raiseHand": raiseHandCount.to_dict(),
        "messages": messageCount.to_dict(),
        "scores": score_table_.to_dict(),
        "totalScores": scaled_scores.to_dict()
    }
}

json_ = json.dumps(json_)
