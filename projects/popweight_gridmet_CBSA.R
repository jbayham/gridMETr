library(raster)

folder.names <- c("pr","rmin","rmax","tmmn","tmmx","vs")
filter.years <- seq(2000,2018)

#All gridmet files are on the same grid of lat and lons so grabbing one
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T)
#Open the connection to the netCDF file
nc <- nc_open(file.names[1])

#####
# option to try to pull as a raster directly
#####
raster <- brick(file.names[1])
slice <- raster[[1]]

crs(slice) <- crs(uslandscan)



library(mapview)
mapview(slice)

landscan <- raster("/RSTOR/landscan/LandScan Global 2018/lspop2018/w001001.adf")
uslandscan <- crop(landscan,extent(us_cbsa[1,]))
testslice <- crop(slice,extent(us_cbsa[1,]))
slicecoords <- xyFromCell(testslice, 1:ncell(testslice))

testsample <- resample(uslandscan, testslice, method = "bilinear")
coords <- xyFromCell(testsample, 1:ncell(testsample))

# coords, values(testsample)
# coords -> sf cbsa intersection

t <- read_rds("data/cbsa_filtered/daily/pr_2000.rds")


hijude <- raster::mosaic(slice,uslandscan)

mapview(testsample)
mapview(uslandscan)




plot(slice)

#back to sf, now polygon
grid.blankr <- slice %>%
  rasterToPolygons() %>%
  st_as_sf() 


#####
# option to pull as flat first
#####
#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)
readin.proj=4269 

#Converting nc coordinates from vector form to simple feature (sf)
g.nc.coords <- st_as_sf(nc.coords, coords = c("lon","lat")) %>% 
  st_set_crs(readin.proj) %>% 
  rownames_to_column() %>% 
  mutate(rowname = as.numeric(rowname))

bridge.cbsa <- st_join(g.nc.coords,us_cbsa,left=T) %>% 
  dplyr::select(cbsa=geoid) %>%
  mutate(lon=as.vector(st_coordinates(.)[,1]),
         lat=as.vector(st_coordinates(.)[,2])) %>%
  filter(!is.na(cbsa)) 

#make raster
r_template <- raster(extent(g.nc.coords),
                     resolution = 1/24,
                     crs = st_crs(g.nc.coords)$proj4string)
grid <- rasterize(g.nc.coords,
                  r_template,
                  field = 'rowname') 

#back to sf, now polygon
grid.sfpoly <- grid %>%
  rasterToPolygons() %>%
  st_as_sf() 

grid.cbsa <- grid.sfpoly %>% 
  st_transform(4269) %>% 
  st_intersection(us_cbsa,.)

map <- grid.cbsa %>% 
  filter(name == "Seneca Falls, NY")
map2 <- bridge.cbsa %>% 
  filter(cbsa == 42900)
  
  

library(mapview)
mapview(map) + mapview(map2)

map <- grid.blankr %>% 
  slice(10000:15000)
mapview(map)


ls <- read
