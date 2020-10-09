#This chunk compiles Jeff's data.
#######################################
year.list <- (2006:2018)
file.names <- list.files("../../../../RSTOR/pierce_pm/krigedPM25",recursive = T,pattern = ".nc",full.names = T)
outer.shell <- vector("list",length(file.names))
pb <- progress_bar$new(
    format = " [:bar] :percent eta: :eta",
    total = length(year.list)*365, clear = FALSE, width= 60)

readin.proj=4269 #because it works with the lat and lons provided

pm_nc <- nc_open(file.names[1])
#bulk extract all data in single year in one call -- will extract from array in next loop
pm_lat <- ncvar_get(nc = pm_nc, varid = "lat")
pm_lon <- ncvar_get(nc = pm_nc, varid = "lon")

t <- as.vector(pm_lat)

#Converting nc coordinates from vector form to simple feature (sf)
g.nc.coords <- tibble("lon" = as.vector(pm_lon),
                      "lat" = as.vector(pm_lat)) %>%
  st_as_sf(coords = c("lon","lat")) %>% 
  st_set_crs(readin.proj)

#Attaching geographic data to netcdf grid -- ignore warning
bridge.cbsa <- st_join(g.nc.coords,us_cbsa,left=T) %>% 
  dplyr::select(cbsa=geoid) %>%
  mutate(lon=as.vector(st_coordinates(.)[,1]),
         lat=as.vector(st_coordinates(.)[,2])) %>%
  st_set_geometry(NULL) %>%
  filter(!is.na(cbsa))

pop <- read_csv("/RSTOR/pierce_pm/2015-popdensity_us_grid.csv") %>% 
  left_join(bridge.cbsa,.)

poptotals <- pop %>% 
  group_by(cbsa) %>% 
  summarise(poptotal = sum(popden2015)) %>% 
  ungroup() %>% 
  left_join(pop,.,by=c('cbsa')) %>% 
  rowwise %>% 
  mutate(popweight = popden2015/poptotal) %>% 
  ungroup() %>% 
  select(cbsa,lon,lat,popweight)

write_rds(poptotals,"/RSTOR/pierce_pm/cbsa_popweights.rds")

j <- 1
i <- 1
for(j in 1:length(outer.shell)){
  # read pm nc
  pm_nc <- nc_open(file.names[j])
  
  #bulk extract all data in single year in one call -- will extract from array in next loop
  pm_lat <- ncvar_get(nc = pm_nc, varid = "lat")
  pm_lon <- ncvar_get(nc = pm_nc, varid = "lon")
  hms.cube <- ncvar_get(nc = pm_nc, varid = "HMS Smoke")[,,1:365]
  pm25.cube <- ncvar_get(nc = pm_nc, varid = "PM25")[,,1:365]
  pmback.cube <- ncvar_get(nc = pm_nc, varid = "Background PM25")[,,1:365] #Seasonal PM25 Background (JF, MAM, JJA, SON)
  
  
  inner.shell <- vector("list",365)
  for(i in 1:length(inner.shell)){
    pb$tick()
    
    g.date <- lubridate::ymd("2006-01-01") + 365*(j-1) + i - 1

    # compile pm data in dataframe
    pm_location_val <- as_data_frame(cbind(as.vector(pm_lon), 
                                           as.vector(pm_lat),
                                           as.vector(pm25.cube[,,i]),
                                           as.vector(pmback.cube[,,i]),
                                           as.vector(hms.cube[,,i]))) %>% 
      rename(lon = V1, lat = V2, pm25 = V3, pm25back = V4, hms = V5) %>% 
      left_join(poptotals,.,by=c("lon","lat")) %>% 
      group_by(cbsa) %>% 
      summarise(weightedpm25 = sum(popweight*pm25),
                weightedpm25b = sum(popweight*pm25back),
                weightedhms = sum(popweight*hms)) %>% 
      ungroup()
    
    
    #Building list where each element is the gridded pollution data for a single day
    inner.shell[[i]] <- pm_location_val %>%
      mutate(date=g.date)
   
  }
  
outer.shell[[j]] <- dplyr::bind_rows(inner.shell)
}

pierce.weighted.data <- dplyr::bind_rows(outer.shell)
write_rds(pierce.weighted.data,"../../../../RSTOR/pierce_pm/weighteddata.rds")


coords <- outer.shell[[1]] %>% 
  distinct(lat,lon)



