#This script builds the Smoke Avoidance project.  
#It should be run once each time a user begins a 
#new R session to work on the project.

########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
init.pacs(c("tidyverse","lubridate","foreign","mice",
          "sf","USAboundaries","viridis","scales"))

#########################
#Loading project helper functions
run.script("functions")


#################
if(!dir.exists("data")){
  dir.create("data")
  message("The data folder has been created. Download the data and place it in the data folder.")
}
#Load cached data if exists otherwise build data
cache.or.build()
