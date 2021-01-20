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
  dplyr::select(cbsa,lon,lat,popweight)

write_rds(poptotals,"/RSTOR/pierce_pm/cbsa_popweights.rds")

j <- 3
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
    # pb$tick()
    
    g.date <- lubridate::ymd("2006-01-01") + 365*(j-1) + i - 1
    
    # compile pm data in dataframe
    pm_location_val <- as_data_frame(cbind(as.vector(pm_lon), 
                                           as.vector(pm_lat),
                                           as.vector(pm25.cube[,,i]),
                                           as.vector(pmback.cube[,,i]),
                                           as.vector(hms.cube[,,i]))) %>% 
      rename(lon = V1, lat = V2, pm25 = V3, pm25back = V4, hms = V5) %>% 
      left_join(poptotals,.,by=c("lon","lat")) 
    
    #Building list where each element is the gridded pollution data for a single day
    inner.shell[[i]] <- pm_location_val %>%
      mutate(date=g.date)
    
  }
  
  bg <- dplyr::bind_rows(inner.shell) %>% 
    group_by(lon,lat) %>% 
    mutate(Count = 1-hms,
           PMb = RcppRoll::roll_mean(x = pm25*Count, n = 61, fill = NA),
           Month = month(date)) %>% 
    ungroup()
  
  bgsd <- bg %>% 
    group_by(lon,lat) %>% 
    summarise(SD = sd(pm25*Count)) %>%
    ungroup()
  
  bgfill <- bg %>% 
    filter(is.na(PMb)) %>% 
    group_by(lon,lat,Month) %>% 
    summarise(PMb2 = mean(pm25*Count)) %>% 
    ungroup()
  
  background.final <- left_join(bg,bgfill,by=c("lon","lat","Month")) %>% 
    left_join(.,bgsd,by=c("lon","lat")) %>% 
    mutate(PMb = ifelse(is.na(PMb),PMb2,PMb),
           PMover = ifelse(hms == 1 &
                             pm25 > 1.645*SD + PMb,1,0)) %>% 
    dplyr::select(-c("Month","PMb2")) %>%
    group_by(cbsa,date) %>%
    summarise(weightedpm25 = sum(popweight*pm25),
              weightedpmbnew = sum(popweight*PMb),
              weightedpmover = sum(popweight*PMover),
              weightedhms = sum(popweight*hms)) %>%
    ungroup()

  write_rds(background.final,paste0("data/pierce_bg/",paste0(2006+j-1),".rds"))
  
}

fl <- list.files("data/pierce_bg", full.names = T)
alltogether <- map_dfr(fl,function(f){
  
  t <- read_rds(f)
  
})

write_rds(alltogether,"../../../../RSTOR/pierce_pm/weighteddata2.rds")





#####
# very long, fewer rolling mean issues
#####
k <- 1

bg.shell <- vector("list",58401)
bg.do <- expand_grid(dim1 = 1:309,
                     dim2 = 1:189)
length(outer.shell)
nrow(bg.do)

map(1:10, function(k){
  
  backgroundrecreate <- map_dfr(1:length(outer.shell),function(j){
    
    pm_nc <- nc_open(file.names[j])
    pm_lat <- ncvar_get(nc = pm_nc, varid = "lat") 
    pm_lon <- ncvar_get(nc = pm_nc, varid = "lon")
    hms.cube <- ncvar_get(nc = pm_nc, varid = "HMS Smoke")[,,1:365]
    pm25.cube <- ncvar_get(nc = pm_nc, varid = "PM25")[,,1:365]
    
    dim1 <- bg.do$dim1[k]
    dim2 <- bg.do$dim2[k]
    
    if ((2006+j-1) %% 4 == 0){
      temp <- as_data_frame(cbind(as.vector(pm25.cube[dim1,dim2,]),
                                  as.vector(hms.cube[dim1,dim2,]))) %>% 
        mutate(lat = pm_lat[dim1,dim2],
               lon = pm_lat[dim1,dim2],
               date = seq(lubridate::ymd(paste0(2006+j-1,"-01-01")),
                          lubridate::ymd(paste0(2006+j-1,"-12-31")), by = 1)[-60]) 
    } else {
      temp <- as_data_frame(cbind(as.vector(pm25.cube[dim1,dim2,]),
                                  as.vector(hms.cube[dim1,dim2,]))) %>% 
        mutate(lat = pm_lat[dim1,dim2],
               lon = pm_lat[dim1,dim2],
               date = seq(lubridate::ymd(paste0(2006+j-1,"-01-01")),
                          lubridate::ymd(paste0(2006+j-1,"-12-31")), by = 1)) 
    }
  })
  
  background <- backgroundrecreate %>% 
    rename(PM = V1, HMS = V2) %>% 
    mutate(Count = 1-HMS,
           PMb = RcppRoll::roll_mean(x = PM*Count, n = 61, fill = NA),
           Year = year(date))
  
  bgsd <- background %>% 
    group_by(Year) %>% 
    summarise(SD = sd(PM*Count)) %>% 
    ungroup()
  
  bgfill <- background %>% 
    filter(is.na(PMb)) %>% 
    group_by(Year) %>% 
    summarise(PMb2 = mean(PM*Count)) %>% 
    ungroup()
  
  background.final <- left_join(background,bgfill,by=c("Year")) %>% 
    left_join(.,bgsd,by=c("Year")) %>% 
    mutate(PMb = ifelse(is.na(PMb),PMb2,PMb),
           PMover = ifelse(HMS == 1 &
                             PM > 1.645*SD + PMb,1,0)) %>% 
    dplyr::select(-c("Year","PMb2"))
  
  write_rds(background.final,paste0("data/pierce_bg/",k,".rds"))
  
  gc()
  
})
