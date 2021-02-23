############################################
# this script uses the yearly bridges made in 
#  the previous script (01) to extract the data 
#  from the downloaded netcdfs for each day 
#  and summarized for the polygons made.
# >> uses objects loaded in (01)
# >> saves as yearly files for each variable
############################################


#####
# prep
#####
downloader::download(url=str_c("http://www.northwestknowledge.net/metdata/data/pdsi.nc"),
         destfile = "data/pdsi/pdsi.nc",
         mode = 'wb')

folder.names <- c("pdsi")
file.names <- str_subset(list.files("data",recursive = T,pattern = ".nc",full.names = T),"pdsi")
years <- 2003:2019
bridgename <- "County" # name to create reference

bridges.all <- tibble(bridgefile = list.files("Data/LandScan_Bridges", full.names = T),
                      year = str_extract(bridgefile,"\\d+"),
                      polygon = str_sub(bridgefile,23,-10)) 

if(!dir.exists("data/Landscan_Weighted_Output")){dir.create("data/Landscan_Weighted_Output")}
if(!dir.exists(paste0("data/Landscan_Weighted_Output/",bridgename))){
  dir.create(paste0("data/Landscan_Weighted_Output/",bridgename))}

#####
# extracting data and rolling up to cbsa w weights
#####
nc <- nc_open(file.names[1])
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)

# fldr <- folder.names[1]
# files.all <- str_subset(file.names,fldr)
var.id=names(nc$var)

y <- 2003
map(years,function(y){
  
  #choose and load correct bridge
  if (y > 2018) {
    
    bridge <- bridges.all %>% 
      filter(year == 2018,
             polygon == bridgename)
    
  } else {
    
    bridge <- bridges.all %>% 
      filter(year == y,
             polygon == bridgename)
    
  }
  
  bridge.ready <- read_csv(bridge$bridgefile) %>% 
    mutate(lon = round(lon,3),
           lat = round(lat,3))
  
  date.vector <- tibble(dates = as_date(nc$dim$day$vals,origin="1900-01-01")) %>% 
    rownames_to_column() %>% 
    filter(year(dates) == y)
  
  slice <- brick(file.names[1])[[as.numeric(date.vector$rowname[1]:date.vector$rowname[nrow(date.vector)])]]
  vals <- raster::values(slice)
  
  nc.df <- vals %>% 
    as_data_frame() %>% 
    rename_all(~str_c(date.vector$dates)) %>% 
    bind_cols(nc.coords,.) %>% 
    mutate(lon = round(lon,3),
           lat = round(lat,3))
  
  var.polygon <- left_join(bridge.ready,
                           nc.df,
                           by=c("lat","lon")) %>% 
    pivot_longer(-c(lon,lat,UID,totalindex),
                 names_to = "date",
                 values_to = "value") %>% 
    mutate(valueweighted = totalindex*value) %>% 
    group_by(UID,date) %>%
    summarize(check=sum(totalindex, na.rm = T), #default option for index
              value=sum(valueweighted, na.rm = T)) %>% #default aggregator
    ungroup() %>%
    mutate(date=ymd(date))
  
  write_rds(var.polygon,
            paste0("data/Landscan_Weighted_Output/",bridgename,"/pdsi_",y,".rds"))
  
})
