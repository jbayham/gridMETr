#This implementation of gridmetr compiles weather data for the project
#investigating the impact of pollution on farm worker productivity

#Project setup
setwd("~/git_projects/gridMETr")
source("project_init.R")

library(pacman)
p_load(tidyverse,lubridate,data.table,sf,ncdf4,janitor,raster,exactextractr,furrr,conflicted,measurements)

conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")
######################
#Download gridmet data

#Define variables to use to construct data (see _ref/variables_reference.csv)
g.vars <- c("pr","rmin","rmax","tmmn","tmmx","vs","th")

#Define years to construct data
g.years <- seq.int(2010,2015)
#########################################################################
#Get netcdf data from source and cache copies in data folder 
gridmetr_download(variables = g.vars,
                  years = g.years)  


##############################################################
#load farms
farms <- st_read("cache/farms.geojson") %>%
  st_transform(4326)

#Open the connection to the netCDF file
nc <- nc_open("inputs/data/tmmx/tmmx_2010.nc")


#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>%
  mutate(cells=row_number())

readin.proj=4326 #because it works with the lat and lons provided

#Alt method reading as raster then using exact_extract
tmmx <- raster("inputs/data/tmmx/tmmx_2010.nc")
crs(tmmx)<-CRS("+init=epsg:4326")
#mapview::mapview(tmmx)

cell_index <- extract(tmmx,farms,cellnumbers=T,df=T)

#check coverage of tribal areas - confirmed that all conus geometries are included
# check_set <- aiannh$geoid[!(unique(aiannh$geoid) %in% unique(cell_index$geoid))]
# aiannh %>%
#   filter(geoid %in% check_set) %>%
#   mapview::mapview()


bridge <- inner_join(cell_index,nc.coords) %>%
  select(cells,lon,lat) 
bridge$name <- farms$name

###################################################

file.list <- expand.grid(g.vars,g.years,stringsAsFactors = F) %>% 
  rename(var=Var1,year=Var2) %>%
  mutate(var=str_to_lower(var),
         file.name = str_c(var,"_",year,".nc")) %>% 
  arrange(var) 

allheat <- paste0("inputs/data/",file.list$var,"/",file.list$file.name)
heat1 <- allheat[1]

plan(multisession(workers = 15))

future_map(allheat, 
           function(heat1){
             
             #Construct the dataframe with all days and cells
             nc <- nc_open(heat1)
             var.id=names(nc$var)
             date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
             
             nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
             
             nc.data.df <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
               as_tibble(.name_repair = "universal") %>%
               rename_all(~str_c(date.vector))
             
             nc_lat <- ncvar_get(nc = nc, varid = "lat")
             nc_lon <- ncvar_get(nc = nc, varid = "lon")
             nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
               mutate(lon = round(lon,5),
                      lat = round(lat,5),
                      cells=row_number())
             
             nc.df <- bind_cols(nc.coords,nc.data.df)
             
             #Join gridmet data with bridge to tribe ids
             var.block <- inner_join(bridge %>% select(name,cells),
                                     nc.df %>% select(-c(lat,lon)),
                                     by=c("cells")) %>%
               select(-c(cells)) %>%
               as.data.table() %>% 
               melt(., id.vars = c("name"),
                    variable.name = "date",
                    value.name = "value") 
             
             dir_name = str_c("cache/",str_split(heat1,"/")[[1]][3])
             if(!dir.exists(dir_name)) dir.create(dir_name)
             write_rds(var.block,paste0(dir_name,"/",str_split(heat1,"/")[[1]][3],"_",str_sub(heat1,-7,-4),".rds"))
             
           },.progress = T)


############################################################
#Read in all individual years

farm_weather <- dir("cache",pattern = ".rds",recursive = T,full.names = F) %>%
  map_dfr(function(x){
    out <- read_rds(str_c("cache/",x)) %>%
      mutate(var=dirname(x))
  })

farm_weather %>% 
  filter(year(date)==2012) %>%
  mutate(date=as_date(date)) %>%
  mutate(value=ifelse(str_detect(var,"vs"),   #Windspeed m/s to mph
                      measurements::conv_unit(value,"m_per_sec","mph"),
                      value),
         value=ifelse(str_detect(var,"pr"),   #precip mm to inch
                      measurements::conv_unit(value,"mm","inch"),
                      value),
         value=ifelse(str_detect(var,"tmm"),             #Temp Kelvin to F
                      measurements::conv_unit(value,"K","F"),
                      value)
  ) %>%
  ggplot(aes(x=date,y=value,color=name)) +
  geom_point() +
  scale_x_date(date_breaks = "3 months",date_labels = "%b") +
  facet_wrap(~var,scales = "free")

farm_weather %>%
  mutate(date=as_date(date)) %>%
  mutate(value=ifelse(str_detect(var,"vs"),   #Windspeed m/s to mph
                      measurements::conv_unit(value,"m_per_sec","mph"),
                      value),
         value=ifelse(str_detect(var,"pr"),   #precip mm to inch
                      measurements::conv_unit(value,"mm","inch"),
                      value),
         value=ifelse(str_detect(var,"tmm"),             #Temp Kelvin to F
                      measurements::conv_unit(value,"K","F"),
                      value)
  ) %>%
  pivot_wider(names_from = var,values_from = value) %>%
  write_csv("cache/farm_weather.csv")

mapview::mapview(farms)
