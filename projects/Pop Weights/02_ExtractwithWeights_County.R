# ------------------------------------------
# this script uses the yearly bridges made in 
#  the previous script (01) to extract the data 
#  from the downloaded netcdfs for each day 
#  and summarized for the polygons made.
# >> uses objects loaded in (01)
# >> saves as yearly files for each variable
# ------------------------------------------

# ------------------------------------------
# Jude, confirm inputs below and then source
# ------------------------------------------

folder.names <- c("pr","tmmn","tmmx") # all variables needed
years <- 2018:2021 # all years needed

rawlocation <- "data" # directory with raw netCDFs
bridgelocation <- "data/LandScan_Bridges" # directory with landscan bridges (in RSTOR)
outputlocation <- "data/Landscan_Weighted_Output" # directory where you want output

#####
# prep
#####

file.names <- list.files(rawlocation,recursive = T,pattern = ".nc",full.names = T)
bridgename <- "County"

bridge <- tibble(bridgefile = list.files(bridgelocation, full.names = T),
                      year = str_extract(bridgefile,"\\d+"),
                      polygon = str_sub(bridgefile,23,-10))  %>% 
  filter(year == 2018,
         polygon == bridgename)

bridge.ready <- read_csv(bridge$bridgefile) %>% 
  mutate(lon = round(lon,3),
         lat = round(lat,3))

if(!dir.exists(outputlocation)){dir.create(outputlocation)}
if(!dir.exists(paste0(outputlocation,bridgename))){
  dir.create(paste0(outputlocation,bridgename))}

#####
# extracting data and rolling up to cbsa w weights
#####
nc <- nc_open(file.names[1])
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)

# fldr <- folder.names[1]
# files.all <- str_subset(file.names,fldr)
fl <- file.names[1]

# plan(multisession, workers = 2)
# options(future.globals.maxSize= 1024^2*1000*8)
gridmet.out <- map(file.names, function(fl){
    
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
             lat = round(lat,3))
    
    var.polygon <- left_join(bridge.ready,
                             nc.df,
                             by=c("lat","lon")) %>% 
      pivot_longer(-c(lon,lat,UID,totalindex),
                   names_to = "date",
                   values_to = "value") %>% 
      mutate(valueweighted = totalindex*value) %>% 
      group_by(UID,date) %>%
      summarize(check=sum(totalindex), #default option for index
                value=sum(valueweighted, na.rm = T)) %>% #default aggregator
      ungroup() %>%
      mutate(date=ymd(date))
    
    write_rds(var.polygon,
              paste0(outputlocation,"/",bridgename,"/",
                     str_extract(str_sub(fl,6,-1),".{2,4}(?=/)"),"_",
                     str_sub(fl,-7,-4),".rds"))
    
})

