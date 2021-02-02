############################################
# this script creates unique yearly mappings
#  of each gridmet cell to any polygons, 
#  along with a weight from (1) the area 
#  of cell in the polygon and (2) population 
#  in the cell, using landscan data 
#  for population in that year
############################################
source('project_init.R')
rm(list = ls())

#####
# prep
#####

# define variables (folders) and years (files) to extract 
folder.names <- c("pr")
years <- 2018

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
st <- state.abb[3]
df <- map(state.abb,function(st){
  
  t <- tigris::block_groups(state = st)
  
})

dfall <- reduce(df,bind_rows)

# df <- us_co # data to use
id <- "GEOID" # existing column in data for UID
bridgename <- "CBG" # name to create reference

####################

polygonstomap <- dfall %>% 
  filter(!STATEFP %in% c("02","15")) %>% 
  rename(UID = id)

# prep netcdf coords
fl <- file.names[1]
slice <- brick(fl)[[1]] #can ignore warning
slicecoords <- tibble(lon = xyFromCell(slice, 1:ncell(slice))[,1],
                      lat = xyFromCell(slice, 1:ncell(slice))[,2]) %>%
  rownames_to_column(var = "cell") %>% 
  mutate(cell = as.numeric(cell))

y <- 2018

# landscan coords | expect ~ 8-20 min / year
map(years,function(y){
  
  landscan <- raster(paste0("data/Landscan/LandScan Global ",y,"/lspop",y,"/w001001.adf"))
  uslandscan <- crop(landscan,extent(polygonstomap))
  
  # matching them up
  uslandscan_res <- resample(uslandscan, slice, method = "bilinear") #same extent as netcdf
  together <- raster::mosaic(slice,uslandscan_res,fun=sum) # aggregate to netcdf
  
  # states
  p2m2 <- polygonstomap %>% 
    mutate(fipnum = as.numeric(STATEFP)) %>% 
    filter(fipnum > 12) %>% 
    group_split(STATEFP)
  
  p <- p2m2[[1]]
  
  map(p2m2,function(p){
    
    extract1 <- raster::extract(together,p, # roll up to polygon of interest
                                weights = T, 
                                # normalizeWeights = T, 
                                cellnumbers = T)
    
    # create the bridge
    message("\n \n Rasters loaded and aligned for state #",p$STATEFP[1]," CBG polygons \n Creating the bridge \n")
    
    # p2m <- p$UID[[1]]
    # e1 <- extract1[[1]]
    
    # l <- 438
    # bridge.raster.TEST <- map_dfr(1:length(extract1),function(l){
    #   
    #   temp <- extract1[l] %>% 
    #     as.data.table() %>% 
    #     left_join(.,slicecoords,by="cell") %>% 
    #     mutate(areaindex = weight/max(weight),
    #            popcount = value*areaindex,
    #            totalindex = popcount/sum(popcount)) %>% 
    #     rowwise() %>% 
    #     mutate(UID = p$UID[[l]]) %>% 
    #     ungroup() %>% 
    #     dplyr::select(UID,lon,lat,totalindex)
    #   
    #   print(l)
    # })
    
    bridge.raster.all <- map2_dfr(extract1,p$UID,function(e1,p2m){
      
      if (length(e1) > 0 ){
        temp <- e1 %>% 
          as.data.table() %>% 
          left_join(.,slicecoords,by="cell") %>% 
          mutate(areaindex = weight/max(weight),
                 popcount = value*areaindex,
                 totalindex = popcount/sum(popcount)) %>% 
          rowwise() %>% 
          mutate(UID = p2m) %>% 
          ungroup() %>% 
          dplyr::select(UID,lon,lat,totalindex)
      } else {
        temp <- tibble(UID = p2m)
      }
      
    })
    
    # check 
    t <- bridge.raster.all %>% 
      group_by(UID) %>% 
      summarise(check = as.character(sum(totalindex, na.rm = T)))
    
    message("\n Bridge succesfully created with proper weighting of ",
            paste0(scales::percent(sum(t$check == "1")/nrow(t))),
            " of polygons \n from state",p$STATEFP[1]," for area and population in ",y,
            "\n ----- ----- ----- -----")
    
    write_csv(bridge.raster.all,paste0("Data/LandScan_Bridges/",bridgename,"_",p$STATEFP[1],"_",y,".csv"))
    
  })
  
  
 })


allweightfiles <- str_subset(list.files("data/Landscan_Bridges", full.names = T),
                         "CBG")

allweights <- map_dfr(allweightfiles,function(f){
  
  t <- read_csv(f) %>% 
    mutate(UID = as.numeric(UID))
  
})

write_csv(allweights,"data/Landscan_Bridges/CBG_2018.csv")

