# Init functions:
# These functions help to initialize the project without
# leaving artifacts in the workspace

#####################################################
#Loading and installing packages
init.pacs <- function(package.list){
  check <- unlist(lapply(package.list, require, character.only = TRUE))
  #Install if not in default library
  if(any(!check)){
    for(pac in package.list[!check]){
      install.packages(pac)
    }
    lapply(package.list, require, character.only = TRUE)
  }
}
#unit test
#init.pacs(c("scales"))


#####################################################
#Run all scripts in a directory
run.script <- function(dir.name){
  #check whether directory exists
  if(dir.exists(dir.name)){
    if(!is.null(dir(dir.name,pattern = ".R"))){
      invisible(lapply(dir(dir.name,pattern = ".R",full.names = T),source))
    }
  } else {
    stop("Invalid directory name")
  }
}
#unit test
#run.script("functions")

####################################################
#Load data from cache or build out from raw data
#note that data is attached into a workspace named after the 
#cache file name
cache.or.build <- function(){
  if(!dir.exists("cache")){
    dir.create("cache")
  }
  cache.files <- dir("cache",pattern = ".Rdata") 
  if(length(cache.files)>0){
    for(cache in cache.files){
      message(str_c("Loading ",cache," from cache..."))
      load(str_c("cache/",cache),envir = .GlobalEnv)
    }
    message("To rebuild data from scratch, empty cache folder.")
  } else {
    for(munge in dir("munge",pattern = ".R")){
      message(str_c("Running ",str_c("munge/",munge),"... "))
      source(str_c("munge/",munge))
    }
  }

}
