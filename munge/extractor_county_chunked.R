#This script provides an example for one way to extract data from netcdf files

setwd("G:/Weather/gridMETr/")
source("project_init.R")

##################################
##################################
#User-defined variables (use vector of string names or "." for wildcard):

#Define folders (variables) to extract -
folder.names <- c("sph","vpd","pr","rmin","rmax","tmmn","tmmx",
                  "vs","th","srad","pdsi","pet","etr",
                  "erc","bi","fm100","fm1000")
#Define set of years 
filter.years <- seq.int(1979,2019)
##################################
# list.files("data",recursive = T,pattern = ".nc",full.names = T) %>%
#   str_subset(.,str_c(year(today()),collapse = "|")) %>%
#   str_subset(.,str_c(folder.names,collapse = "|")) %>%
#   file.remove()

gridmetr_download(folder.names,filter.years)
##################################


#All gridmet files are on the same grid of lat and lons so grabbing one
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T) %>%
  str_subset(.,str_c(filter.years,collapse = "|")) %>%
  str_subset(.,str_c(folder.names,collapse = "|"))

#Open the connection to the netCDF file
nc <- nc_open(file.names[1])


#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)

#Load variable names and descriptions
all_names <- read_csv("_ref/variables_reference.csv") %>%
  as.data.table()


##############################################
#Use GIS tools to aggregate data by chosen geography
##############################################
#Choose a projection to be used by all geographic files
readin.proj=4269 #because it works with the lat and lons provided

#Converting nc coordinates from vector form to simple feature (sf)
g.nc.coords <- st_as_sf(nc.coords, coords = c("lon","lat")) %>% 
  st_set_crs(readin.proj)


#Attaching geographic data to netcdf grid -- ignore warning
bridge.county <- st_join(g.nc.coords,us_co,left=T) %>% 
  dplyr::select(county=geoid) %>%
  mutate(lon=as.vector(st_coordinates(.)[,1]),
         lat=as.vector(st_coordinates(.)[,2])) %>%
  st_set_geometry(NULL)




#######################
#For parallelization with future
#plan(multiprocess(workers = 2))
# future_map_dfr(...)
##########################
pb <- progress_bar$new(
  format = " [:bar] :percent eta: :eta \n",
  total = length(file.names), clear = FALSE, width= 60)
#Begin loop over variables (folders)

map(str_subset(dir("data"),pattern=str_c(folder.names,collapse = "|")),
    function(fn){
      message(str_c("Beginning ",fn,"..."))
      
      #Finding the names of all files in the directory
      file.names <- dir(str_c("data/",fn)) %>%
        str_subset(pattern=str_c(filter.years,collapse = "|"))
      
      #Setting static local folder name (probably unecessary)
      #folder.temp=fn
      
      #Extracting variable name for organization below
      #Open the connection to the netCDF file
      nc <- nc_open(str_c("data/",fn,"/",file.names[1]))
      
      #Set variable name
      #var.id=names(nc$var)
      var.id <- all_names[]

      #Begin loop over years (files)
      year.temp <- 
        map_dfr(file.names,
          function(f.names){
            pb$tick()
            #Open the connection to the netCDF file
            nc <- nc_open(str_c("data/",fn,"/",f.names))
            
            #Get dates
            date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
            
            #Read in year of observations and collapse to matrix with lat/lon on rows and dates as columns
            nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
            
            nc.data <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
              as_tibble(.name_repair = "universal") %>%
              rename_all(~str_c(date.vector))
        
            #Organize nc.data into dataframe 
            nc.df <- bind_cols(nc.coords,nc.data) %>%
              gather(-one_of("lon","lat"),key="date",value="value")
            
            var.year <- bind_cols(bridge.county,nc.data) %>%
              dplyr::filter(!is.na(county)) %>%
              gather(-one_of("lon","lat","county"),key="date",value="value") %>%
              group_by(county,date) %>%
              summarize(value=base::mean(value,na.rm=T)) %>%
              ungroup() %>%
              mutate(date=ymd(date))
            
            nc_close(nc)
            return(var.year)
            
          }) %>% 
        mutate(variable=str_c(fn,"_",var.id))  #adding the variable name
      
      return(year.temp)
  }) %>%
dplyr::filter(!is.na(value))

date.range <- unique(gridmet.county$date)

#save(gridmet.county,file = str_c("output/gridmet_county_daily_",min(date.range),"_",max(date.range),".Rdata"))


gm.wide <- gridmet.county %>%
  pivot_wider(id_cols = c(county,date),
              names_from = "variable",
              values_from = "value") %>%
  mutate_at(vars(tmmn_air_temperature,tmmx_air_temperature),~conv_unit(.,"K","F")) %>%
  mutate(vs_wind_speed = conv_unit(vs_wind_speed,"m_per_sec","mph")) %>%
  rename(precip=pr_precipitation_amount,
         rmax=rmax_relative_humidity,
         rmin=rmin_relative_humidity,
         tmin=tmmn_air_temperature,
         tmax=tmmx_air_temperature,
         srad=srad_surface_downwelling_shortwave_flux_in_air,
         wind_speed=vs_wind_speed)


fwrite(gm.wide,"output/weather_county_2020-01-01_yesterday.csv.gz")



file.copy(from = "output/weather_county_2020-01-01_yesterday.csv.gz",
          to = "L:/My Drive/data_not_synced/gridmet_share/weather_county_2020-01-01_yesterday.csv.gz",
          overwrite = T)


# drive_update(file = as_id("https://drive.google.com/open?id=1u7MUokF-1ipmFJNEl4xyRVy5ubcQ2OWh"),
#              media = str_c("output/weather_county_",min(date.range),"-",max(date.range),".zip"))


