# ------------------------------------------
# this script uses the yearly bridges made in 
#  the previous script (01) to extract the data 
#  from the downloaded netcdfs for each day 
#  and summarized for the polygons made.
# >> uses objects loaded in (01)
# >> saves as yearly files for each variable
# ------------------------------------------

source('project_init.R')
rm(list = ls())

# bridgeslocal <- list.files("data/Landscan_Bridges", full.names = T)
# bridgesRSTOR <- paste0("/RSTOR/Gridmet_Landscan_Bridges/",str_sub(bridgeslocal,23,-1))
# for (i in 1:length(bridgeslocal)) {
#   file.copy(bridgeslocal[i], bridgesRSTOR[i])
# }

# ------------------------------------------
# Jude, confirm inputs below and then source
# ------------------------------------------

folder.names <- c("pr","tmmn","tmmx") # all variables needed
years <- 2018:2021 # all years needed

rawlocation <- "data" # directory with raw netCDFs
bridgelocation <- "/RSTOR/Gridmet_Landscan_Bridges" # directory with landscan bridges (in RSTOR)
outputlocation <- "data/Landscan_Weighted_Output" # directory where you want output

#####
# prep
#####

file.names <- str_subset(
  str_subset(list.files(paste0(rawlocation,"/",folder.names),
                                    recursive = T,pattern = ".nc",full.names = T),
                         paste0(years,collapse = "|")),
  paste0(folder.names,collapse = "|"))

bridgename <- "CBG"

bridge <- tibble(bridgefile = str_subset(list.files(bridgelocation, full.names = T),
                                         "CBG"),
                      state = str_extract(bridgefile,"\\d+"),
                      polygon = str_sub(bridgefile,23,-10)) %>% 
  filter(state!=2018)

if(!dir.exists(outputlocation)){dir.create(outputlocation)}
if(!dir.exists(paste0(outputlocation,"/",bridgename))){
  dir.create(paste0(outputlocation,"/",bridgename))}

#####
# extracting data and rolling up to cbsa w weights
#####
nc <- nc_open(file.names[1])
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)

fl <- file.names[1]
bb <- bridge$bridgefile[1]

# plan(multisession, workers = 2)
# options(future.globals.maxSize= 1024^2*1000*8)
# options(globals.maxSize= 1024^2*1000*8)
  
map(file.names, function(fl){
  
  nc <- nc_open(fl)
  var.id=names(nc$var)
  
  #Get dates
  date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
  
  #Read in year of observations and collapse to matrix with lat/lon on rows and dates as columns
  nc.data.v <- ncvar_get(nc = nc, varid = var.id)[,,]
  
  nc.data <- array(nc.data.v,dim=c(prod(dim(nc.data.v)[1:2]),dim(nc.data.v)[3])) %>%
    as_tibble(.name_repair = "universal") %>%
    rename_all(~str_c(date.vector))
  
  #Organize nc.data into dataframe 
  nc.df <- bind_cols(nc.coords,nc.data) %>% 
    mutate(lon = round(lon,3),
           lat = round(lat,3)) %>% 
    as.data.table()
  
  map(bridge$bridgefile,function(bb){
    
    bridge.ready <- read_csv(bb) %>% 
      mutate(lon = round(lon,3),
             lat = round(lat,3)) %>% 
      as.data.table()
    
    var.polygon <- bridge.ready[nc.df, on=c("lat","lon")] %>% 
      melt(id.vars = c("lon","lat","UID","totalindex"),
           variable.name = "date",
           value.name = "value") %>% 
      .[!is.na(UID)] %>% 
      .[, by = .(UID,date),
        .(check=sum(totalindex), #default option for index
          value=sum(totalindex*value, na.rm = T))] %>% #default aggregator
      .[, date := ymd(date)]
      
    gc()
    
    write_rds(var.polygon,
              paste0(outputlocation,"/",bridgename,"/",str_sub(bb,27,28),"_",
                     str_extract(str_sub(fl,6,-1),".{2,4}(?=/)"),"_",
                     str_sub(fl,-7,-4),".rds"))
    
  })
})




