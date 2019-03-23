#This script builds the Smoke Avoidance project.  
#It should be run once each time a user begins a 
#new R session to work on the project.

########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
init.pacs(c("tidyverse","lubridate","foreign","mice","foreach","doSNOW",
          "sf","raster","USAboundaries","leaflet","viridis","scales","rgdal",
          "conflicted","htmlwidgets"))


#Defining conflict preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

#########################
#Setting project map projections
readin.proj=4269 #because it works with the lat and lons provided
plot.proj=2163  #2227  - in feet
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
