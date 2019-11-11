#This implementation of gridmetr compiles weather data for the project
#investigating the impact of pollution on farm worker productivity

#Project setup
source("project_init.R")

#Define variables to use to construct data (see _ref/variables_reference.csv)
g.vars <- c("pr","rmin","rmax","tmmn","tmmx","vs","th")

#Define years to construct data
g.years <- seq.int(2010,2015)
#########################################################################
#Get netcdf data from source and cache copies in data folder 
gridmetr_download(variables = g.vars,
                  years = g.years)  



