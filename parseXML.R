library("xml2")
library("xslt")
library("XML")
library("RJSONIO")

convert <- function(input) {
  ##Importing rmd file and turn into a list
  xml_file <- tinkr::to_xml(input)
  title <- xml_file[["yaml"]][2]
  title <- strsplit(title, " title: ")
  title <- gsub("title: ", "", title[[1]])
  title <- paste0(gsub('"', "", title[[1]]))
  xml_file <- as_xml_document(xml_file$body)
  blocks <- xmlParse(xml_file)
  list <- xmlToList(blocks)
  ##Loop for obtain content and language information
  z <- fromJSON("template-5.json")
  c <- fromJSON("code-test.json")
  codes = data.frame()
  languages = data.frame()
  index <- names(list)
  z[["name"]] = title[[1]]
  z[["project"]][["title"]] = title
  z[["project"]][["stages"]][[1]][["title"]] = title
  
  for (i in 1:length(list)) {
    cell <- list[i]
    if (index[i] == "code_block") {
      code <- c[["project"]][["stages"]][[1]][["cells"]][[1]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = code
      codes <-
        rbind(codes,
              paste0(cell$code_block$text, sep = ''),
              stringsAsFactors = FALSE)
      languages <- rbind(languages,
                         paste0(cell[["code_block"]][[".attrs"]]@.Data[2], sep =
                                  ''),
                         stringsAsFactors = FALSE)
      
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = paste(cell$code_block$text)
      
      if (cell$code_block$.attrs[["language"]] == "r") {
        z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = "R"
      }
      else if (cell$code_block$.attrs[["language"]] == "python") {
        z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = "Python"
        
      } else {
        z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = "Markdown"
      }
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["output"]] = NULL
    }
    else if (index[i] == "heading") {
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = z[["project"]][["stages"]][[1]][["cells"]][[2]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = 'Markdown'
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = paste("#", cell$heading$text$text)
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["output"]] = NULL
    }
    else if (index[i] == "block_quote") {
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = z[["project"]][["stages"]][[1]][["cells"]][[2]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = 'Markdown'
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = paste(cell$block_quote$paragraph$text$text)
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["output"]] = NULL
      
    }
    else if (index[i] == "html_block") {
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = z[["project"]][["stages"]][[1]][["cells"]][[2]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = 'HTML'
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = paste(cell$html_block$text)
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["output"]] = NULL
    }
    else if (index[i] == "table") {
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = z[["project"]][["stages"]][[1]][["cells"]][[2]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = 'Markdown'
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = paste(
        cell$table$table_header$table_cell$text$text,
        cell$table$table_row$table_cell$text$text,
        sep = "-"
      )
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["output"]] = NULL
    }
    else if (index[i] == "list") {
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = z[["project"]][["stages"]][[1]][["cells"]][[2]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = 'Markdown'
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      list2 <- list[[i]]
      output <- vector()
      index2 <- names(list2)
      
      for (j in 1:length(list2)) {
        list3 <- list2[[j]]
        index3 <- names(list3)
        cell2 <- list2[j]
        for (k in 1:length(list3)) {
          output_item <- vector()
          if (index3[k] == "paragraph") {
            list4 <- list3[[k]]
            index4 <- names(list4)
            for (x in 1:length(list4)) {
              cell3 <- list4[x]
              
              if (index4[x] == "text") {
                output_item <- append(output_item, paste0(cell3[["text"]][["text"]]))
              }
              else if (index4[x] == "link") {
                output_item <-
                  append(output_item,
                         paste0("[", cell3[["link"]][["text"]][["text"]], "]"
                                , "(", cell3[["link"]][[".attrs"]][[1]], ")"))
              }
              else if (index4[x] == "image") {
                output_item <-
                  append(output_item,
                         paste0("\n ![", cell3[["image"]][["text"]][["text"]], "]"
                                , "(", cell3[["image"]][[".attrs"]][[1]], ")"))
              }
              else if (index4[x] == "emph") {
                output_item <- append(output_item, paste0(cell3[["emph"]][["text"]][["text"]]))
              }
              else if (index4[x] == "html_inline") {
                output_item <-
                  append(output_item, paste0(cell3[["html_inline"]][["text"]]))
              }
              else{
                print(index4[x])
              }
            }
          }
        }
        output <- append(output, paste(output_item, collapse = ""))
        
      }
      output <- sprintf(paste("\n*  ", output, collapse = ' '))
      
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = output
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["output"]] = NULL
    }
    else if (index[i] == "paragraph") {
      list2 <- list[[i]]
      index2 <- names(list2)
      output <- vector()
      
      for (j in 1:length(list2)) {
        cell2 <- list2[j]
        if (index2[j] == "text") {
          output <- append(output, paste0(cell2[["text"]][["text"]]))
        }
        else if (index2[j] == "link") {
          output <-
            append(output, paste0("[", cell2[["link"]][["text"]][["text"]], "]"
                                  , "(", cell2[["link"]][[".attrs"]][[1]], ")"))
        }
        else if (index2[j] == "image") {
          output <-
            append(output,
                   paste0("\n ![", cell2[["image"]][["text"]][["text"]], "]"
                          , "(", cell2[["image"]][[".attrs"]][[1]], ")"))
        }
        else if (index2[j] == "emph") {
          output2 <- append(output, paste0(cell2[["emph"]][["text"]][["text"]]))
        }
        else if (index2[j] == "html_inline") {
          output <- append(output, paste0(cell2[["html_inline"]][["text"]]))
        }
        else{
          print(index2[j])
        }
      }
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = z[["project"]][["stages"]][[1]][["cells"]][[2]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = 'Markdown'
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = paste(unlist(output), collapse =
                                                                          '')
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["output"]] = NULL
    }
    else {
      z[["project"]][["stages"]][[1]][["cells"]][[i]] = z[["project"]][["stages"]][[1]][["cells"]][[2]]
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["language"]] = 'Markdown'
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["index"]] = i
      z[["project"]][["stages"]][[1]][["cells"]][[i]][["code"]] = "--"
    }
  }
  #write(toJSON(z),"a.json")
  return(toJSON(z))
}