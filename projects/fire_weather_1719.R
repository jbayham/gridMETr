source("project_init.R")
rm(list = ls())
library(RANN)

# download
gridmetr_download(c("pr","rmin","rmax","tmmn","tmmx","vs","BI","erc","pdsi","fm1000"),
                  seq(2014,2019))

#Define folders (variables) to extract - 
folder.names <- c("pr","rmin","rmax","tmmn","tmmx","vs",
                  "BI","ERC","pdsi","FM1000")
years <- 2014:2019
file.names <- tibble(files = list.files("data",recursive = T,pattern = ".nc",full.names = T)) %>% 
  mutate(file.year = str_extract(files,"\\d+")) %>% 
  filter(file.year %in% years)


fires <- read_csv("data/fires.csv") %>% 
  mutate(start = as.Date(start, "%m/%d/%Y"),
         end = as.Date(end, "%m/%d/%Y"),
         weather_date = as.Date(weather_date, "%m/%d/%Y"))

#####
# mapping of cells to fires
#####

fl <- file.names[1]
slice <- brick(fl)[[1]]
slicecoords <- tibble(lon = xyFromCell(slice, 1:ncell(slice))[,1],
                      lat = xyFromCell(slice, 1:ncell(slice))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell),
         ID = row_number()) 

gmcoords <- slicecoords %>% #for knn
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_transform(5070) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(ID = row_number(),
         XY = paste0(X,",",Y))
gmcoordsact <- gmcoords %>% 
  dplyr::select(X,Y)

firecoords <- read_csv("data/fires.csv") %>% 
  st_as_sf(coords = c("poo_longitude","poo_latitude"), crs = 4326) %>% 
  st_transform(5070) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  mutate(ID = row_number(),
         XY = paste0(X,",",Y)) 
firecoordsact <- firecoords %>% 
  dplyr::select(X,Y)

nn <- nn2(gmcoordsact, firecoordsact, k=1, searchtype = "radius", radius = 10000)

nn.df <- sapply(nn, cbind) %>% 
  as_tibble() %>% 
  bind_cols(fires,.) %>% 
  rename(loc.ID = nn.idx,
         Dist = nn.dists) 

# nodata <- nn.df %>% 
#   filter(loc.ID == 0) %>% 
#   st_as_sf(coords = c("poo_longitude","poo_latitude"), crs = 4326) 
# mapview(nodata)

fires.sitecodes <- nn.df %>% 
  left_join(., slicecoords, by=c("loc.ID" = "ID")) %>% 
  filter(loc.ID != 0) %>% 
  distinct(paste0(incident_number,incident_name,start,end),.keep_all = T) %>% 
  dplyr::select(incident_number,incident_name,start,end,weather_date,loc.ID,Dist,lon,lat) %>% 
  mutate(lon = round(lon,3),
         lat = round(lat,3),
         start = as.Date(start, "%m/%d/%Y"),
         end = as.Date(end, "%m/%d/%Y"),
         weather_date = as.Date(weather_date, "%m/%d/%Y"))

write_csv(fires.sitecodes,"projects/bridge_fires.csv")
fires.sitecodes <- read_csv("projects/bridge_fires.csv")

#####
# extracting data and rolling up to cbsa w weights
#####
nc <- nc_open(file.names$files[1])
nc <- nc_open(fl)
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
  as.data.frame() 

fldr <- folder.names[9]
files.all <- str_subset(file.names$files,fldr)
fl <- list.files("data/pdsi", full.names = T)
pdsi <- file.names %>% 
  filter(str_detect(files,"pdsi"))

options(future.globals.maxSize= 1024^2*1000*8)
gridmet.out <- future_map(pdsi$files, function(fl){
  
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
  
  if(var.id == "palmer_drought_severity_index"){
    var.fires <- left_join(fires.sitecodes,
                           nc.df,
                           by=c("lat","lon")) %>% 
      pivot_longer(-c(incident_number,incident_name,start,end,weather_date,loc.ID,Dist,lon,lat),
                   names_to = "date",
                   values_to = "value") %>% 
      mutate(weather_date = as.Date(weather_date, "%m/%d/%Y"),
             end = as.Date(weather_date, "%m/%d/%Y"),
             date = ymd(date)) %>% 
      filter(date >= weather_date,
             date <= end+11)
  } else {
    var.fires <- left_join(fires.sitecodes,
                                 nc.df,
                                 by=c("lat","lon")) %>% 
      pivot_longer(-c(incident_number,incident_name,start,end,weather_date,loc.ID,Dist,lon,lat),
                   names_to = "date",
                   values_to = "value") %>% 
      mutate(weather_date = as.Date(weather_date, "%m/%d/%Y"),
             end = as.Date(weather_date, "%m/%d/%Y"),
             date = ymd(date)) %>% 
      filter(date >= weather_date,
             date <= end)
  }
  
  
  write_rds(var.fires,
            paste0("data/fires_subsetted/",
                   str_extract(str_sub(fl,6,-1),".{2,4}(?=/)"),".rds"))
  
}, .progress = T)

test <- nc.df %>% 
  dplyr::select(1:3) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  sample_n(10000)

mapview(test)

outfiles <- list.files("data/fires_subsetted", full.names = T)
outact <- str_subset(outfiles,"pdsi", negate = T)
r <- read_rds("data/fires_subsetted/bi_2014.rds")

allout <- map_dfr(outact,function(o){
  
  r <- read_rds(o) %>% 
    mutate(var = str_sub(o,22,-10),
           start = as.Date(start, "%m/%d/%Y"),
           date = ymd(date)) %>% 
    dplyr::select(-c(loc.ID,lon,lat,Dist))
  
})

pdsi <- read_rds(str_subset(outfiles,"pdsi", negate = F)) %>% 
  mutate(var = "pdsi",
         start = as.Date(start, "%m/%d/%Y"),
         date = ymd(date)) %>% 
  dplyr::select(-c(loc.ID,lon,lat,Dist))

allout2 <- bind_rows(allout,pdsi) %>% 
  filter(!is.na(value))

write_csv(allout2,"data/fires_weather.csv")



allout <- read_csv("data/fires_weather.csv") %>% 
  filter(var == "pdsi")

zerona <- allout2 %>%  
  group_by(var) %>%
  summarise(na = sum(is.na(value)),
            zero = sum(value == 0, na.rm = T),
            obs = n())


fires <- read_csv("data/fires.csv") %>% 
  mutate(start = as.Date(start, "%m/%d/%Y"),
         end = as.Date(end, "%m/%d/%Y"),
         weather_date = as.Date(weather_date, "%m/%d/%Y")) %>% 
  dplyr::select(incident_number,incident_name,start,end,poo_longitude,poo_latitude)
  
nodataw <- allout %>% 
  filter(is.na(value),
         var == "bi") %>% 
  distinct(paste0(incident_number,incident_name,start,end),.keep_all = T) %>% 
  left_join(.,fires,by=c("incident_number","incident_name","start","end")) %>% 
  dplyr::select(incident_number,incident_name,start,end,poo_longitude,poo_latitude) %>% 
  st_as_sf(coords = c("poo_longitude","poo_latitude"), crs = 4326) %>% 
  st_transform(5070) %>% 
  mutate(ID = row_number()) 

gmcoords2 <- slicecoords %>% #for knn
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>% 
  st_transform(5070) %>% 
  mutate(ID = row_number())

bridge2 <- st_intersection(st_buffer(nodataw,30000),gmcoords2)
  
mapview(bridge2)



