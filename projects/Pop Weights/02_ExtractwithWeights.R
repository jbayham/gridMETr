############################################
# this script uses the yearly bridges made in 
#  the previous script (01) to extract the data 
#  from the downloaded netcdfs for each day 
#  and summarized for the polygons made.
# >> uses objects loaded in (01)
# >> saves as yearly files for each variable
############################################


#####
# prep
#####
folder.names <- c("pr","tmmn","tmmx")
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T)
years <- 2003:2019
bridgename <- "County" # name to create reference

bridges.all <- tibble(bridgefile = list.files("Data/LandScan_Bridges", full.names = T),
                      year = str_extract(bridgefile,"\\d+"),
                      polygon = str_sub(bridgefile,23,-10)) 

if(!dir.exists("data/Landscan_Weighted_Output")){dir.create("data/Landscan_Weighted_Output")}
if(!dir.exists(paste0("data/Landscan_Weighted_Output/",bridgename))){
  dir.create(paste0("data/Landscan_Weighted_Output/",bridgename))}

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

plan(multisession, workers = 2)
options(future.globals.maxSize= 1024^2*1000*8)
gridmet.out <- map(file.names[45:54], function(fl){
    
    nc <- nc_open(fl)
    var.id=names(nc$var)
    
    #Get dates
    date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
    
    #choose and load correct bridge
    if (unique(year(date.vector)) > 2018) {
      
      bridge <- bridges.all %>% 
        filter(year == 2018,
               polygon == bridgename)
      
    } else {
      
      bridge <- bridges.all %>% 
        filter(year == unique(year(date.vector)),
               polygon == bridgename)
      
    }
    
    bridge.ready <- read_csv(bridge$bridgefile) %>% 
      mutate(lon = round(lon,3),
             lat = round(lat,3))
    
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
              paste0("data/Landscan_Weighted_Output/",bridgename,"/",
                     str_extract(str_sub(fl,6,-1),".{2,4}(?=/)"),"_",
                     str_sub(fl,-7,-4),".rds"))
    
})


# t <- var.cbsa %>% 
#   group_by(cbsa,date) %>%
#   summarize(check = sum(totalindex),
#             value=sum(valueweighted)) %>%
#   ungroup() %>%
#   mutate(date=ymd(date)) %>% 
#   filter(check = min(check))
# 
# 
#   # filter(cbsa == 43620)
# 
# t2 <- t %>% 
#   filter(date == as_date("2000-01-01")) %>% 
#   mutate(valueweighted = totalindex*value) %>% 
#   summarize(value=sum(valueweighted))
#           


