#This script downloads the newest data

setwd("G:/Weather/gridMETr/")
source("project_init.R")


##################################
##################################
#User-defined variables (use vector of string names or "." for wildcard):

#Define folders (variables) to extract -
folder.names <- c("pr","rmin","rmax","tmmn","tmmx","vs","srad")
#Define set of years 
filter.years <- c(2020) #seq.int(2006,2018)
##################################
list.files("data",recursive = T,pattern = ".nc",full.names = T) %>%
  str_subset(.,str_c(year(today()),collapse = "|")) %>%
  str_subset(.,str_c(folder.names,collapse = "|")) %>%
  file.remove()

gridmetr_download(folder.names,filter.years)