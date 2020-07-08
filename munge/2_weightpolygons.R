# source code, from PM

library(raster)
library(tidyverse)
library(sf)
library(tigris)

# pull and save grid centroids
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T)
nc <- nc_open(file.names[1])
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)
readin.proj=4269 
g.nc.coords <- st_as_sf(nc.coords, coords = c("lon","lat")) %>% 
  st_set_crs(readin.proj)
write_rds(g.nc.coords,"munge/gridpts.rds")

# load centroids
grid <- read_rds("munge/gridpts.rds") %>%
  st_transform(5070) %>% 
  rownames_to_column('Rownum') %>% 
  mutate(Rownum = as.numeric(Rownum))

# make raster
r_template <- raster(extent(grid),
                     resolution = 16000,
                     crs = st_crs(grid)$proj4string)
grid.r <- rasterize(grid,
                    r_template,
                    field = 'Rownum') 

# back to sf, now polygon
# grid.poly <- grid.r %>%
#   rasterToPolygons() %>%
#   st_as_sf()

write_rds(grid.poly,"munge/gridpoly.rds")



# test with colorado

cobg <- block_groups("CO") %>% 
  st_as_sf(crs = 4269) %>% 
  st_transform(5070)

cbg.extracted <- raster::extract(grid.r,cobg) %>% 
  tibble(grids = .) 

cbg.long <- cbg.extracted %>% 
  rownames_to_column('Rownum') %>% 
  mutate(Rownum = as.numeric(Rownum)) %>% 
  unnest(grids)



# landscan for weights
vat <- raster("data/LandScan Global 2018_1/LandScan Global 2018/lspop2018/vat.adf")

grid.ls <- rasterize(levels(vat),
                     vat,
                     field = 'Count')






# extract with raster and shapefile
tracts.adams.sp <- tracts.adams.sf %>%
  st_collection_extract(., type = "POLYGON") %>%
  as_Spatial()

result <- extract(cogrid.adams.r,tracts.adams.sp) %>% 
  tibble(grids = .) 

tracts.adams.sf2 <- tracts.adams.sp %>% 
  st_as_sf() %>%
  rowid_to_column() %>% 
  mutate(tract.area = st_area(geometry))

tract.piece <- st_join(tracts.adams.sf2,cogrid.adams.sfpoly)

acton.tract <- tracts.adams.sf2$rowid

# weight
tract.output <- map_dfr(acton.tract,function(rid){
  
  temp.tractpiece <- tract.piece %>% 
    filter(rowid == rid) 
  
  layers <- cogrid.adams.sfpoly %>% 
    filter(layer %in% temp.tractpiece$layer) %>% 
    rowid_to_column() %>% 
    rename(layer.num = rowid) 
  
  acton.layers <- layers$layer.num
  partial.areas <- map_dfr(acton.layers,function(grid){
    
    temp.gridtract <- st_intersection(layers[grid,],temp.tractpiece[1,]) %>% 
      st_area() %>% 
      as_tibble() %>% 
      mutate(layer.num := `grid`,
             rowid := `rid`) 
  })
  
  temp.joined <- layers %>% 
    st_set_geometry(NULL) %>% 
    left_join(x=.,y=partial.areas,by='layer.num')
  
  temp.tracts.output <- tract.piece %>% 
    st_set_geometry(NULL) %>% 
    left_join(x=.,y=temp.joined, by = c("rowid","layer")) %>% 
    filter(rowid == rid)
  
}) %>% 
  mutate(percent.area = value/tract.area) %>% 
  rename(partial.area = value)

spotcheck <- tract.output %>% 
  group_by(rowid) %>% 
  summarise(partialsum = round(sum(percent.area),2)) %>% 
  filter(partialsum != 1)

write_csv(tracts.output,path = "data/tracts_w_gridpercents.csv")


# test <- st_join(tracts.adams.sf2,cogrid.adams.sfpoly) %>% 
#   filter(OBJECTID == 164) 
# fest <- cogrid.adams.sfpoly %>% 
#   filter(layer %in% test$layer)
# pest <- st_intersection(fest[1,],test[1,])%>% 
#   st_area()


# districts.all <- st_read("data/CDPHE_CDOE_School_District_Boundaries/CDPHE_CDOE_School_District_Boundaries.shp") %>%
#   st_transform(5070) 
# tracts.all <- st_read("data/Colorado_Census_Tract_Boundaries/Colorado_Census_Tract_Boundaries.shp") %>%
#   st_transform(5070)


# #filter, for now
# adams <- districts.all %>%
#   filter(NAME == "Northglenn-Thornton School District 12")
# tracts.adams.sf <- st_intersection(tracts.all,adams) %>%
#   select(OBJECTID,geometry)
# cogrid.adams.sfpts <- st_intersection(cogrid,st_buffer(adams,2000))

# #1 | weighted intersect of two polygons... does not work
# t <- tracts.adams.sf %>%
#   st_collection_extract(., type = "POLYGON")
# 
# tracts.adams.weighted = st_interpolate_aw(cogrid.adams.sfpoly[,"layer"],
#                                           t,
#                                           extensive = FALSE) 
