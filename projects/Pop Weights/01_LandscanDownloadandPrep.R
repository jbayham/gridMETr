############################################
# this script creates unique yearly mappings
#  of each gridmet cell to any polygons, 
#  along with a weight from (1) the area 
#  of cell in the polygon and (2) population 
#  in the cell, using landscan data 
#  for population in that year
############################################

rm(list = ls())

#####
# prep
#####

# define variables (folders) and years (files) to extract 
folder.names <- c("pr")
years <- 2000:2018

####################

file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T)
load("_ref/geographies/geos.Rdata")

# Landscan Download 
if(!dir.exists("data/Landscan")){dir.create("data/Landscan")}
if(!dir.exists("data/Landscan_Bridges")){dir.create("data/Landscan_Bridges")}

map(years,function(y){
  if(!dir.exists(paste0("data/Landscan/LandScan Global ",y))){
  
    drive_download(paste0("Landscan/LandScan Global ",y,".zip"),
                   path = paste0("data/Landscan/",y,".zip"))  
    
    unzip("data/Landscan/2018.zip",
          exdir = "data/Landscan")
    
    file.remove(paste0("data/Landscan/",y,".zip"))
    
  }
})


#####
# mapping of cells to CBSA w weights.
#  requires st data with polygons (multipolygons ok) with a UID column
#####
 
# define polygons 
df <- us_co # data to use
id <- "geoid" # existing column in data for UID
bridgename <- "County" # name to create reference

####################

polygonstomap <- df %>% 
  mutate(UID = geoid)

# prep netcdf coords
fl <- file.names[1]
slice <- brick(fl)[[1]] #can ignore warning
slicecoords <- tibble(lon = xyFromCell(slice, 1:ncell(slice))[,1],
                      lat = xyFromCell(slice, 1:ncell(slice))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

# landscan coords | expect ~ 8-20 min / year
map(years,function(y){
  
  landscan <- raster(paste0("data/Landscan/LandScan Global ",y,"/lspop",y,"/w001001.adf"))
  uslandscan <- crop(landscan,extent(polygonstomap))
  
  # matching them up
  uslandscan_res <- resample(uslandscan, slice, method = "bilinear")
  together <- raster::mosaic(slice,uslandscan_res,fun=sum)
  extract1 <- raster::extract(together,polygonstomap,
                              weights = T, 
                              # normalizeWeights = T, 
                              cellnumbers = T)
  
  # create the bridge
  message("\n \n Rasters loaded and aligned for ",bridgename," polygons \n Creating the bridge \n")
  
  bridge.raster.all <- map_dfr(1:nrow(polygonstomap),function(p2m){
    
    temp <- extract1[[p2m]] %>% 
      as.data.table() %>% 
      left_join(.,slicecoords,by="cell") %>% 
      mutate(areaindex = weight/max(weight),
             popcount = value*areaindex,
             totalindex = popcount/sum(popcount)) %>% 
      rowwise() %>% 
      mutate(UID = polygonstomap[p2m,]$UID) %>% 
      ungroup() %>% 
      dplyr::select(UID,lon,lat,totalindex)
    
  })
  
  # check 
  t <- bridge.raster.all %>% 
    group_by(UID) %>% 
    summarise(check = as.character(sum(totalindex)))
  
  message("\n Bridge succesfully created with proper weighting of ",
          paste0(scales::percent(sum(t$check == "1")/nrow(t))),
          " of polygons \n from ",bridgename," for area and population in ",y,
          "\n ----- ----- ----- -----")
  
  write_csv(bridge.raster.all,paste0("Data/LandScan_Bridges/",bridgename,"_",y,".csv"))
  
})







