source("functions/download_data.R")

gridmetr_download(variables = c("pr", 
                                "rmin", 
                                "rmax", 
                                "tmmn", 
                                "tmmx", 
                                "vs"),
                  years = 2018)  #seq.int(2000,2001)
