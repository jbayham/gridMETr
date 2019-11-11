#This script initializes the project and should be run at the beginning of each
#session

#########################
#Load init functions
source("functions/init_functions.R")

#Loading and installing packages
init.pacs(c("tidyverse",      #shortcut to many useful packages (eg, dplyr, ggplot)
            "conflicted",     #resolves function conflict across packages
            "lubridate",      #working with dates
            "sf",             #for GIS
            "USAboundaries",  #easily access US maps in sf
            #"googledrive",    #for accessing googledrive
            #"furrr","foreach","doSNOW",
            "progress",       #for progress bars
            #"raster",         #for working with raster data
            "rgdal",          #GIS processing
            "ncdf4",          #accessing netcdf data
            "haven"           #other stat datatypes
))


#Setting package::function priority with conflicted package
conflict_prefer("filter", "dplyr")
conflict_prefer("between", "dplyr")
#########################
#Loading project helper functions (all scripts within folder)
run.script("functions")

#load geographies
load("_ref/geographies/geos.Rdata")
##########################################
##########################################
#Function to download the project data (on first run, google should prompt you to login with credentials)
#if data folder doesn't exist, build data
#get_data("url")


#folder.setup()
