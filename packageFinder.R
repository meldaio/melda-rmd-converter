library(CodeDepends)

melda.findDependency <- function(x){

  tools::package_dependencies(x)

}

#' Load a melda.io json
#'
#This function takes melda.io json file. It gives R code blocks of melda.io json file as output
#'
#' @param meldaJson is filepath.
#' @return returns R code blocks of melda.io json file as a data frame
#' @export
melda.findPackages <- function(meldaJson){
  json <- jsonlite::fromJSON(meldaJson,simplifyDataFrame = FALSE)
  temp <- vector(mode = "list",  length = length(json$project$stages))
  # print("Json file is read")
  packageList <- list()
  tryCatch({
    for(i in 1:length(json$project$stages)){
      z <- 1
      for(j in 1:length(json[["project"]][["stages"]][[i]][["cells"]])){
        if(json[["project"]][["stages"]][[i]][["cells"]][[j]][["language"]] == "R"){
          temp[[i]][[z]] <-  json[["project"]][["stages"]][[i]][["cells"]][[j]][["code"]]
          packageList <- append(packageList, melda.searchLibrary(json[["project"]][["stages"]][[i]][["cells"]][[j]][["code"]]))
          temp[[i]][[z]] <- as.list(temp[[i]][[z]]) # print( paste( "Stage number is:", i, "cell number is ", j))
          z <- z + 1
        }
      }
    }
  },error = function(e) {
    # print(paste("Error in stage number:", i,"and cell number:" , j,e ))
  })
  return(unique(packageList))
}

melda.findMethods <- function(meldaJson){
  json <- jsonlite::fromJSON(meldaJson,simplifyDataFrame = FALSE)
  temp <- vector(mode = "list",  length = length(json$project$stages))
  # print("Json file is read")
  methodList <- list()

  tryCatch({
    for(i in 1:length(json$project$stages)){
      z <- 1
      for(j in 1:length(json[["project"]][["stages"]][[i]][["cells"]])){
        if(json[["project"]][["stages"]][[i]][["cells"]][[j]][["language"]] == "R"){
          temp[[i]][[z]] <-  json[["project"]][["stages"]][[i]][["cells"]][[j]][["code"]]
          temp[[i]][[z]] <- as.list(temp[[i]][[z]]) # print( paste( "Stage number is:", i, "cell number is ", j))
          result <- melda.findFunctionName(json[["project"]][["stages"]][[i]][["cells"]][[j]][["code"]])
          for(k in 1:length(result)) {
            methodList <- append(methodList, melda.findLibrary(result[k]))
          }
          z <- z + 1
        }
      }
    }
  },error = function(e) {
  })
  return(unique(methodList))
}

melda.findCellMethods <- function(cell) {
  packages <- melda.findCellPackages(cell)
  methodList <- list()
  returnedList <- list()
  tryCatch({
    result <- melda.findFunctionName(cell)
    for(k in result) {
      if(!grepl("[^A-Za-z_.]", k)) {
        methodList <- append(methodList, melda.findLibrary(k))
        if(length(packages) > 0) {
          for (j in packages) {
            for (l in methodList) {
              if(paste0(j,'::',k) == l) {
                returnedList <- append(returnedList, l)
              }
            }
          }
        } else {
          returnedList <- methodList
        }
      }
    }
  },error = function(e) {
  })
  return(unique(returnedList))
}

melda.findCellPackages<- function(cell) {
  cell <- strsplit(cell, "\n")
  methodList <- list()
  for (i in cell) {
    tryCatch({
        methodList <- append(methodList, melda.searchLibrary(i))
    },error = function(e) {
    })
  }
  return(unique(methodList))
}

rem_dup.one <- function(x){
  paste(unique(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T)))),collapse = " ")
}

#' Load a Matrix
#'
#This function takes expression as input.
#'
#' @param expr is R expression   in chr
#' @return loads the functions' library if it is not loaded.
#' @export
melda.searchLibrary <- function(expr){
  if(!is.null(expr) && length(expr) != 0){ #debugging

    chr <- expr
    temp <- ""
    #print(length(chr))
    if( (length(chr) == 1 && grepl("library\\(",chr)) || grepl("require\\(",chr)|| grepl("devtools",chr) || grepl("install.packages\\(",chr)){
      y <- regexpr( "\\(.*?\\)" , chr , perl = TRUE)
      div <- regmatches( chr,y )
      div <- rem_dup.one(div)
      div <- gsub("'","",div)
      return(div)

    }else if( length(chr) > 1){

      for(a in 1:length(chr)){

        if( grepl("library\\(",chr[[a]]) || grepl("require\\(",chr[[a]]) || grepl("devtools",chr[[a]]) ||  grepl("install.packages\\(",chr[[a]])){
          y <- regexpr( "\\(.*?\\)" , chr[[a]] , perl = TRUE)
          div <- regmatches( chr[[a]],y )
          temp <- paste(temp,div,sep = " \n ")
          temp <- sub("\n"," ",temp)
        }
      }
      temp <- rem_dup.one(temp)
      temp <- gsub('"',"",temp)
      temp <- strsplit(temp," ")[[1]]
      return( temp[-1] )
    }else{
      return(NULL)
    }
  }else{
    return(NULL)
  }
}

#' Load a Character Vector
#'
#This function takes function names as input.
#'
#' @param x takes  character vector  as an input
#' @return returns function names as list
#' @export
melda.findFunctionName <- function(chr){
  chr <-getInputs(parse(text = chr))
  if(length(chr@functions) == 0){
    return(NULL)
  }else{
    a <- data.frame()
    chr <- chr@functions
    chr <- cbind(a$deneme, names(chr))
    return(chr)
  }
}

#' Load a Matrix
#'
#This function takes function names as input.
#'
#' @param input function name as a string
#' @return loads the functions' library if it is not loaded.
#' @export
melda.findLibrary <- function(input,load = FALSE, dblcolon = TRUE){
  defaultLibs <- sessionInfo()
  defaultLibs <- c(defaultLibs$basePkgs)
  tryCatch(
    {
      input <-gsub("\\[","\\\\[",input)
      df <- help.search(input)
      df <- df$matches
      x  <- unique(df$Package)
      x <- setdiff(x,defaultLibs)
      },error = function(e){
      return(NULL)
    }
  )
  if(length(x) > 1){
    if(load == TRUE){
      cat(  paste("1. is",x[[1]],"\n","2. is",x[[2]],"\n"), sep = "")
      userInput <- as.numeric( readline(prompt =("Type 1 or 2:  ")))
      userLibrary <- as.character(paste(x[[userInput]]) , sep = "")
    }
    if(dblcolon == T ){
      for(i in 1:length(x)){
        x[[i]] <- paste(x[[i]],"::",input,sep ="")
      }
      return(x)
    }
    return(x)
  }else if(length(x) == 1){
    x <- paste(x,"::",input,sep ="")
    if(load  == TRUE){
      userLibrary <- x[[1]]
    }
    if(dblcolon == T ){
    }
    return(x)
  }else{
    return(NULL)
  }
}


#' Load a Matrix
#'
#This function takes function names as input.
#'
#' @param funcName function name as a string
#' @return loads the functions' library if it is not loaded.
#' @export
melda.findLibraryInDefPkgs <- function(funcName){
  defaultLibs <- sessionInfo()
  defaultLibs <- c(defaultLibs$basePkgs,names(defaultLibs$otherPkgs))
  libName <- melda.findLibrary( funcName )
  if( !is.null(libName) && length(libName) == 1){
    libName <- libName
    funcName <- paste(libName, "::", funcName,sep = "")
  }else if( !is.null(libName) && length(libName) > 1){
    for( m in 1:length(libName)){
      tryCatch({
        if(length(grep( libName[[m]],defaultLibs)) == 0) {
          libName[[m]] <- NA
        }else{
          ind <- grep(libName[[m]],defaultLibs)
          libName <- defaultLibs[[ind]]
          funcName <- paste(defaultLibs[[ind]], "::",funcName,sep = "")
          break()
        }
      },error = function(e){
      })
    }
    libName <- libName[!unlist(lapply(libName, is.na))]

  }else{
    libName <- "-"
    funcName <- funcName
  }
  if(length(libName) == 0){
    libName <- "-"
  }
  return(data.frame(libName = as.character(libName),
                    funcName =as.character(funcName),
                    stringsAsFactors = F))
}




#' Load an R expression
#'
#This function takes R expresions as input.
#'
#' @param expr is a R expression
#' @return detailed benchmark results..
#' @export
melda.benchmark <-function(expr){
  tryCatch({
    b <- bench::mark( { parse(text = expr)})
    return(b[c("min","median","itr/sec","n_gc")])
  },error = function(e){
    return(NULL)
  })

}


#' Load an R expression
#'
#This function takes R expresions as input.
#'
#' @param expr is a R expression
#' @return loads the functions' library if it is not loaded.
#' @export
melda.findDataFiles <- function(expr){
  if(!is.null(expr) && length(expr) != 0){ #debugging
    chr <- strsplit(expr, "\n" )[[1]]
    temp <- ""
    for(a in 1:length(chr)){
      if(  grepl("read.*\\(",chr) || grepl( "read\\_",chr) ){
        y <- regexpr( "\\(.*?\\)" , chr[[a]] , perl = TRUE)
        div <- regmatches( chr[[a]],y )
        if( grepl("\\,",div)){
          y <- regexpr( "\\(.*?\\," , chr[[a]] , perl = TRUE)
          div <- regmatches( chr[[a]],y )
          div <- sub("\\(","",div)
          div <- sub("\\,","",div)
        }
        div <- sub("\\(","",div)
        div <- sub("\\)","",div)
        temp <- paste(temp,div,sep ="\n")
        temp <- sub("\n"," ",temp)
      }
    }
    temp <- gsub('"',"",temp)
    temp <- strsplit(temp," ")[[1]]
    print( temp[-1] )

  }else{
    print(NULL)
  }

}


#' Load an R expression
#'
#This function takes R expresions as input.
#'
#' @param expr is a R expression
#' @return loaded functions.
#' @export
melda.findDependencies <- function(input){
  defaultLibs <- sessionInfo()
  defaultLibs <- c(defaultLibs$basePkgs,names(defaultLibs$otherPkgs))
  funcNames <- melda.findFunctionName( input )
  cellType <- "R"
  allDeps <- list()
  for(func in funcNames){
    libName <- melda.findLibraryInDefPkgs(func)$libName
    funcName <- melda.findLibraryInDefPkgs(func)$funcName
    allDeps <- append(allDeps,list(list(funcName  = funcName ,
                                        libName = libName,
                                        cellType = cellType
    )))
  }
  if(length(allDeps) == 0 ){
    return(list())
  }else{
    #toJSON(allDeps,auto_unbox = T,pretty = T)
    toJSON(allDeps)
  }
}
