############################################
# this script creates unique mapping 
#  of each gridmet cell to cbsa, along with 
#  a weight from (1) the area of cell in
#  cbsa and (2) population in the cell,
#  using landscan data for population
############################################

#####
# prep
#####

rm(list = ls())

#Define folders (variables) to extract - 
folder.names <- c("pr","rmin","rmax","tmmn","tmmx","vs")
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T)
years <- 2000:2018

landscan <- raster("/RSTOR/landscan/LandScan Global 2018/lspop2018/w001001.adf")
load("_ref/geographies/geos.Rdata")

#####
# mapping of cells to CBSA w weights 
#####

slice <- brick(fl)[[1]]
slicecoords <- tibble(lon = xyFromCell(slice, 1:ncell(slice))[,1],
                      lat = xyFromCell(slice, 1:ncell(slice))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

# landscan data
uslandscan <- crop(landscan,extent(us_cbsa))

# matching them up
uslandscan_res <- resample(uslandscan, slice, method = "bilinear")
together <- raster::mosaic(slice,uslandscan_res,fun=sum)
extract1 <- raster::extract(together,us_cbsa,
                            weights = T, normalizeWeights = T, cellnumbers = T)

cb <- 1

bridge.raster.all <- map_dfr(1:964,function(cb){
  
  temp <- extract1[[cb]] %>% 
    as.data.table() %>% 
    left_join(.,slicecoords,by="cell") %>% 
    mutate(totalindex = value*weight/(sum(value)*max(weight)),
           cbsa = us_cbsa[cb,]$geoid) %>% 
    dplyr::select(cbsa,lon,lat,totalindex)
  
})

write_csv(bridge.raster.all,"projects/bridge_raster_cbsa.csv")
bridge.raster.all <- read_csv("projects/bridge_raster_cbsa.csv") %>% 
  mutate(lon = round(lon,3),
         lat = round(lat,3))


#####
# extracting data and rolling up to cbsa w weights
#####
fldr <- folder.names[1]
fl <- files.all[1]

grimet.out <- map(folder.names, function(fldr){
  
  files.all <- str_subset(file.names,fldr)
  
  year.single <- map(files.all, function(fl){
    
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
    
    var.cbsa <- left_join(bridge.raster.all,
                          nc.df,
                          by=c("lat","lon")) %>% 
      pivot_longer(-c(lon,lat,cbsa,totalindex),
                   names_to = "date",
                   values_to = "value") %>% 
      group_by(cbsa,date) %>%
      summarize(value=sum(prod(totalindex*value, 
                               na.rm = T),na.rm = T)) %>%
      ungroup() %>%
      mutate(date=ymd(date))
    
    write_rds(var.cbsa,
              paste0("projects/output/",
                     var.id,"_",
                     str_sub(fl,-7,-4),".rds"))
    
  })
})



######


















#####
# fuhgetaboutit
#####


fn <- folder.names[1]
fl <- file.names[1]

d <- 1
cb <- 1

bridge.test <- bridge.raster.all %>% 
  filter(cbsa == 43620)






















#####
# sol 1 | raster trans on whole country
#####
slicegeneral <- brick(fl)[[1]]
slice_cbsa <- crop(slicegeneral,extent(us_cbsa))
slicecoords <- tibble(lon = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,1],
                      lat = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,2],
                      gridmet = slice_cbsa@data@values) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

# landscan data
uslandscan <- crop(landscan,extent(us_cbsa))

# matching them up
uslandscan_res <- resample(uslandscan, slice_cbsa, method = "bilinear")
together <- raster::mosaic(slice_cbsa,uslandscan_res,fun=sum)
extract1 <- raster::extract(together,us_cbsa,
                          weights = T, normalizeWeights = T, cellnumbers = T) 

extract2 <- map_dfr(1:length(extract1),function(e){
  
  temp <- extract1[[e]] %>% 
    as.data.table() %>% 
    mutate(cbsa = us_cbsa[e,]$geoid)
  
})

t <- extract2 %>% 
  left_join(.,slicecoords,by="cell") %>% 
  group_by(cbsa) %>% 
  mutate(areaindex = weight/max(weight),
         totalindex = (value*areaindex)/sum(value)) %>% 
  summarise(gridmet = sum(totalindex*gridmet)) %>% 
  ungroup()

  
#####
# sol2 | by cbsa, slice
#####
gridmet.out <- map(file.names,function(fl){
  
  slicegeneral <- brick(fl)[[1]]
  
  cbsa.out <- map_dfr(1:964,function(cb){
    
    slice_cbsa <- crop(slicegeneral,extent(us_cbsa[cb,]))
    
    # landscan data
    uslandscan <- crop(landscan,extent(us_cbsa[cb,]))
    
    # matching them up
    uslandscan_res <- resample(uslandscan, slice_cbsa, method = "bilinear")
    together <- raster::mosaic(slice_cbsa,uslandscan_res,fun=sum)
    extract1 <- raster::extract(together,us_cbsa[cb,],
                                weights = T, normalizeWeights = T, cellnumbers = T) %>%
      as.data.frame() %>%
      mutate(areaindex = weight/max(weight),
             totalindex = (value*areaindex)/sum(value))
    
    days <- map_dfr(1:nlayers(brick(fl)), function(d){
      
      # slice of netcdf
      slice_day <- brick(fl)[[d]]
      slice_cbsa <- crop(slice_day,extent(us_cbsa[cb,]))
      slicecoords <- tibble(lon = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,1],
                            lat = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,2],
                            gridmet = slice_cbsa@data@values) %>%
        rownames_to_column(var = "cell") %>% 
        mutate(cell = as.numeric(cell))
      crs(slice_cbsa) <- crs(uslandscan)
      
      values <- extract1 %>%
        left_join(.,slicecoords,by=c("cell")) %>%
        summarise(value = sum(totalindex*gridmet)) 
      
    }) 
    
    #Get dates
    date.vector <- as.vector(as_date(nc_open(fl)$dim$day$vals,origin="1900-01-01"))
    
    var.cbsa <- bind_cols("Date" = date.vector,days) %>% 
      mutate(var = names(nc_open(fl)$var),
             Date = as_date(Date),
             cbsa = slice(us_cbsa,cb)$geoid)
    
    write_rds(var.cbsa,
              paste0("projects/cache/",
                     str_sub(fl,-7,-4),"_",
                     slice(us_cbsa,cb)$geoid,"_",
                     names(nc_open(fl)$var)))
   
    
  })
  
})


#####
# sol 3 | by cbsa, brick?
#####
cb <- 1

gridmet = slice_cbsa@data@values

slice_brick <- brick(fl)
slice_cbsa <- crop(slice_brick,extent(us_cbsa[cb,]))
slicecoords <- tibble(lon = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,1],
                      lat = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))
crs(slice_cbsa) <- crs(uslandscan)

uslandscan <- crop(landscan,extent(us_cbsa[cb,]))
uslandscan_res <- resample(uslandscan, slice_cbsa, method = "bilinear")
together <- raster::mosaic(slice_cbsa[[1]],uslandscan_res,fun=sum)

extract1 <- raster::extract(together,us_cbsa[cb,],
                            weights = T, normalizeWeights = T, cellnumbers = T) %>%
  as.data.frame() %>%
  mutate(areaindex = weight/max(weight),
         totalindex = (value*areaindex)/sum(value))



slicecoords <- tibble(lon = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,1],
                      lat = xyFromCell(slice_cbsa, 1:ncell(slice_cbsa))[,2],
                      gridmet = slice_cbsa@data@values) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))
crs(slice_cbsa) <- crs(uslandscan)

values <- extract1 %>%
  left_join(.,slicecoords,by=c("cell")) %>%
  summarise(value = sum(totalindex*gridmet)) 
  


#Get dates
date.vector <- as.vector(as_date(nc_open(fl)$dim$day$vals,origin="1900-01-01"))

var.cbsa <- bind_cols("Date" = date.vector,days) %>% 
  mutate(var = names(nc_open(fl)$var),
         Date = as_date(Date),
         cbsa = slice(us_cbsa,cb)$geoid)

write_rds(var.cbsa,
          paste0("projects/cache/",
                 str_sub(fl,-7,-4),"_",
                 slice(us_cbsa,cb)$geoid,"_",
                 names(nc_open(fl)$var)))






values <- extract1 %>%
  left_join(.,slicecoords,by=c("cell")) %>%
  summarise(value = sum(totalindex*gridmet)) 
      
#Get dates
date.vector <- as.vector(as_date(nc_open(fl)$dim$day$vals,origin="1900-01-01"))

var.cbsa <- bind_cols("Date" = date.vector,days) %>% 
  mutate(var = names(nc_open(fl)$var),
         Date = as_date(Date),
         cbsa = slice(us_cbsa,cb)$geoid)

write_rds(var.cbsa,
          paste0("projects/cache/",
                 str_sub(fl,-7,-4),"_",
                 slice(us_cbsa,cb)$geoid,"_",
                 names(nc_open(fl)$var)))
    
















  
  
  












  #Read in year of observations and collapse to matrix with lat/lon on rows and dates as columns
  nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
  
  nc.data <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
    as_tibble(.name_repair = "universal") %>%
    rename_all(~str_c(date.vector))
  
  #Organize nc.data into dataframe 
  nc.df <- bind_cols(nc.coords,nc.data) 
  
  var.cbsa <- inner_join(bridge.cbsa,
                         nc.df,
                         by=c("lat","lon")) %>%
    pivot_longer(-c(lon,lat,cbsa),
                 names_to = "date",
                 values_to = "value") %>% 
    inner_join(.,extract1,by=c("lat","lon"))
  
})

t <- var.cbsa %>% 
  filter(date == ymd("2000-01-01"))



#Begin loop over variables (folders)
gridmet.out <- 
  map(folder.names,
      function(fn){
        message(str_c("Beginning ",fn,"..."))
        
        #Finding the names of all files in the directory
        file.names <- dir(str_c("data/",fn)) %>%
          str_subset(pattern=str_c(filter.years,collapse = "|"))
        
        #Setting static local folder name (probably unecessary)
        #folder.temp=fn
        
        #Extracting variable name for organization below
        #Open the connection to the netCDF file
        nc <- nc_open(fl)
        var.id=names(nc$var)

        #Begin loop over years (files)
        
        year.temp <- 
          map(file.names,
            function(f.names){
              
              
              
              
              #Open the connection to the netCDF file
              nc <- nc_open(f.names)
              
              #Get dates
              date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
              
              #Read in year of observations and collapse to matrix with lat/lon on rows and dates as columns
              nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
              
              nc.data <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
                as_tibble(.name_repair = "universal") %>%
                rename_all(~str_c(date.vector))
          
              #Organize nc.data into dataframe 
              nc.df <- bind_cols(nc.coords,nc.data) 
              
              var.cbsa <- inner_join(bridge.cbsa,
                                     nc.df,
                                     by=c("lat","lon")) %>%
                pivot_longer(-c(lon,lat,cbsa),
                             names_to = "date",
                             values_to = "value") 
              
              
              %>% 
                group_by(cbsa,date) %>%
                summarize(value=base::mean(value,na.rm=T)) %>%
                ungroup() %>%
                mutate(date=ymd(date))
              
              # write_rds(var.cbsa,paste0("data/cbsa_filtered/",str_remove(f.names,"\\.nc"),".rds"))
              
            }) 
        
        write_rds(year.temp,paste0("data/cbsa_filtered/summed_",str_remove(file.names,"_\\d+\\.nc")[1],".rds"))
        # county.out <- map(year.temp,"county") %>%
        #   map_dfr(~mutate(.,variable=str_c(fn,"_",var.id)))
        
        # cbsa.out <- map(year.temp,"cbsa") %>%
        #   map_dfr(~mutate(.,variable=str_c(fn,"_",var.id)))
        # 
        # return(list(county=county.out,
        #             cbsa=cbsa.out))    
        }) 

# gridmet.out.county <- map_dfr(gridmet.out,"county")

test <- reduce(year.temp,bind_rows)

gridmet.out.cbsa <- map_dfr(year.temp,"cbsa")

yr.range <- unique(year(gridmet.out.county$date))

save(gridmet.out.county,file = str_c("output/gridmet_county_daily_",min(yr.range),"-",max(yr.range),".Rdata"))

save(gridmet.out.cbsa,file = str_c("output/gridmet_cbsa_daily_",min(yr.range),"-",max(yr.range),".Rdata"))


