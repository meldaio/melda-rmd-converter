library("RestRserve")
library(reticulate)

reticulate::import('pandas')
reticulate::import('os')
py_run_string("import os")

source("parseXML.R")
# source("packageFinder.R")

app = Application$new()
app$add_get(
  path = "/hello",
  FUN = function(request, response) {
    response$set_body("Hello from RestRserve")
  })
app$add_post(
  path = "/convert",
  FUN = function(request, response) {
    write(request$body$file,"new.Rmd")
    response$set_body(convert("new.Rmd"))
  })
app$add_get(
  path = "/score",
  FUN = function(request, response) {
    source("fetchXapi.R")
    py_run_string(paste("os.environ['class_name']='", request$parameters_query, "'", sep=""))
    source_python('algorithm.py')
    response$set_body(py$json_)
  }
)

backend = BackendRserve$new()
backend$start(app, http_port = 6000)

