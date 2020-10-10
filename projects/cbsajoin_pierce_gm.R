pmsmoke <- read_rds("../../../../RSTOR/pierce_pm/weighteddata.rds")

flgrid <- str_subset(list.files("data/cbsa_filtered", full.names = T),"summed")
gm <- map_dfr(flgrid,function(f){
  
  r <- read_rds(f)
  
})
