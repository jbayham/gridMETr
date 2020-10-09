library(raster)

folder.names <- c("pr","rmin","rmax","tmmn","tmmx","vs")
#Define set of years 
filter.years <- seq(2000,2018)
##################################
##################################

#All gridmet files are on the same grid of lat and lons so grabbing one
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T)

#Open the connection to the netCDF file
nc <- nc_open(file.names[1])

#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

pft <- raster(file.names[1])

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)

readin.proj=4269 #because it works with the lat and lons provided

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





ls <- read
