gridmetr_download <- 
  function(variables,years,parallel.workers=1){
    # Downloads data from gridMET
    #
    # Args: 
    #   variables: (character) vector of variables names (see variables_reference)
    #   years: (numeric) the years of data that you want to download
    #   parallel.workers: (numeric) the number of workers that you want to use (default is 1)
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
    var_ref <- suppressMessages(read_csv("inputs/_ref/variables_reference.csv")) %>%
      mutate(variable=str_to_lower(variable))
    map(variables,function(v){
      if (!(v %in% var_ref$variable)) {
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
      arrange(var) %>%
      mutate(file.name=str_c(var,"_",year,".nc")) 
    
    #Create destination folder(s) if they don't exist
    map(str_c("inputs/data/",variables),
        function(x){
          if(!dir.exists(x)){
            dir.create(x,recursive = T)}
        }  )
    
    #Download all files in list
    map(1:dim(file.list)[1],
               function(x){
                #Check if the file exists in the target directory
                target.file <- str_c("inputs/data/",file.list$var[x],"/",file.list$file.name[x])
                if (file.exists(target.file)){
                  message("Target file exists")
                } else {
                  #Download the file
                  download(url=str_c("http://www.northwestknowledge.net/metdata/data/",file.list$file.name[x]),
                           destfile = target.file,
                           mode = 'wb')
                }
              }
            )
    
    #Would like to make this parallel in the future.  Currently, future doesn't
    #work well when plan specified within a function 
    #plan(multiprocess(workers = parallel.workers))

}
  

gridmetr_download_pdsi <- function(){
  if(!dir.exists("inputs/data/pdsi")) dir.create("inputs/data/pdsi",recursive = T)
  download(url="http://www.northwestknowledge.net/metdata/data/pdsi.nc",
           destfile = "inputs/data/pdsi/pdsi.nc",
           mode = 'wb')
}
