rm(list = ls())
variables <- c("tmmn","tmmx","pr")
years <- 2003:2019

target.file <- "data/pdsi_test.nc"
x <- 1

y <- 2014

gridmetr_download <- 
  function(variables,years){
    # Downloads data from gridMET
    #
    # Args: 
    #   variables: (character) vector of variables names (see variables_reference)
    #   years: (numeric) the years of data that you want to download
    #
    # Returns:
    #   No objects but it does create a data folder and saves netcdf files in it
    #
    
    #libraries
    require(dplyr)
    require(lubridate)
    require(stringr)
    require(readr)
    require(purrr)
    #require(furrr)
    #require(future)
    require(downloader)
    #############################
    # Error handling:
    # Check variable
    var_ref <- suppressMessages(read_csv("_ref/variables_reference.csv")) %>%
      mutate(variable=str_to_lower(variable))
    map(variables,function(v){
      if (!(str_to_lower(v) %in% var_ref$variable)) {
        stop(str_c("Variable ",v," not valid gridMET variable"))
      }
    })
    
    #Check years
    yr.diff <- dplyr::setdiff(years,seq.int(1979,as.numeric(year(today()))))
    if (length(yr.diff)>=1) {
      stop(str_c("Years not available: ",str_c(yr.diff,collapse = ", ")))
    }
    
    ############################
    #Construct a list of files from the variables and years  
    file.list <- expand.grid(variables,years,stringsAsFactors = F) %>% 
      rename(var=Var1,year=Var2) %>%
      mutate(var=str_to_lower(var)) %>% 
      arrange(var) %>%
      mutate(file.name = ifelse(var == 'pdsi',"pdsi.nc",
                                str_c(var,"_",year,".nc"))) %>% 
      filter(var == 'pdsi' & year == max(year)
             | var != 'pdsi')
    
     #Create destination folder(s) if they don't exist
    map(str_c("data/",variables),
        function(x){
          if(!dir.exists(x)){
            dir.create(x,recursive = T)}
        }  )
    
    options(timeout = max(300, getOption("timeout")))
    #Download all files in list
    map(1:dim(file.list)[1],
               function(x){
                #Check if the file exists in the target directory
                target.file <- str_c("data/",file.list$var[x],"/",file.list$file.name[x])
                if (file.exists(target.file)){
                  message("Target file exists")
                } else {
                  #Download the file
                  download(url=str_c("http://www.northwestknowledge.net/metdata/data/",file.list$file.name[x]),
                           destfile = target.file,
                           mode = 'wb')
                }
                
                # if (file.list$var[x] == 'pdsi'){
                #   
                #   nc <- nc_open(target.file)
                #   date.vector <- tibble("all.dates" = as_date(nc$dim$day$vals,origin="1900-01-01")) %>% 
                #     rownames_to_column() %>% 
                #     mutate(year = str_sub(all.dates,1,4),
                #            rowname = as.numeric(rowname))
                #   
                #   map(years,function(y){
                #     
                #     date.keep <- date.vector %>% 
                #       filter(year == as.character(y))
                #     
                #     slice <- brick(target.file)[[date.keep$rowname]]
                #     
                #     # writeRaster(slice, 
                #     #             str_c("data/",file.list$var[x],"/",file.list$var[x],"_",y,".nc"), 
                #     #             overwrite=TRUE,
                #     #             format="CDF",
                #     #             varname="pdsi",
                #     #             varunit="index",
                #     #             longname="pdsi -- raster stack to netCDF",
                #     #             xname="Longitude",
                #     #             yname="Latitude",
                #     #             zname="Time")
                #     
                #   })
                  
                
                
              }
            )
    
}


#Would like to make this parallel in the future.  Currently, future doesn't
#work well when plan specified within a function 
#plan(multiprocess(workers = parallel.workers))


gridmetr_download(variables,years)
