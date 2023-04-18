#This script provides an example for one way to extract data from netcdf files



##################################
##################################
#User-defined variables (use vector of string names or "." for wildcard):

#Define folders (variables) to extract - 
folder.names <- c("pdsi")
#Define set of years 
#filter.years <- seq.int(1980,2022,1)
##################################
# gridmetr_download(variables = folder.names,
#                   years = filter.years)

#gridmetr_download_pdsi()
##################################


#All gridmet files are on the same grid of lat and lons so grabbing one
file.names <- list.files("inputs/data/pdsi",recursive = T,pattern = ".nc",full.names = T)


if(!file.exists("cache/bridge_county.rds")){
  #Open the connection to the netCDF file
  nc <- nc_open(file.names[1])
  
  
  #Extract lat and lon vectors
  nc_lat <- ncvar_get(nc = nc, varid = "lat")
  nc_lon <- ncvar_get(nc = nc, varid = "lon")
  
  #Use the lat and lon vectors to create a grid represented as two vectors (note:
  #lon must go first to match with netcdf data)
  nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
    mutate(lon = round(lon,5),
           lat = round(lat,5),
           cells=row_number())
  
  
  ##############################################
  #Use GIS tools to aggregate data by chosen geography
  ##############################################
  #Choose a projection to be used by all geographic files
  readin.proj=4326 #because it works with the lat and lons provided
  
  #Converting nc coordinates from vector form to simple feature (sf)
  g.nc.coords <- st_as_sf(nc.coords, coords = c("lon","lat")) %>% 
    st_set_crs(readin.proj)
  
  #ensure that county polygons are also in 4326
  us_co <- st_transform(us_co,4326)
  
  #Attaching geographic data to netcdf grid -- ignore warning
  bridge.county <- st_join(g.nc.coords,us_co,left=T) %>%
    dplyr::select(county=geoid,cells) %>%
    sfc_as_cols(.,names = c("lon","lat")) %>%
    st_set_geometry(NULL)
  
  saveRDS(bridge.county,"cache/bridge_county.rds")
} else {
  bridge.county <- readRDS("cache/bridge_county.rds")
}



#######################

fy = file.names

#Begin loop over variables (folders)
#plan(multisession(workers = 15))
# future_walk(
#   file.list,
#   function(fy){
#     message(str_c("Beginning ",fy,"..."))
    
    vname=str_split(fy,"/")[[1]][3]
    
    #Construct the dataframe with all days and cells
    nc <- nc_open(fy)
    var.id=names(nc$var)[1]
    date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
    
    nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
    
    nc.data.df <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
      as_tibble(.name_repair = "universal") %>%
      rename_all(~str_c(date.vector))
    
    nc_lat <- ncvar_get(nc = nc, varid = "lat")
    nc_lon <- ncvar_get(nc = nc, varid = "lon")
    nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
      mutate(lon = round(lon,5),
             lat = round(lat,5),
             cells=row_number())
    
    nc.df <- bind_cols(nc.coords,nc.data.df)
    
    #Join gridmet data with bridge 
    var.block <- inner_join(bridge.county %>% select(county,cells),
                            nc.df %>% select(-c(lat,lon)),
                            by=c("cells")) %>%
      drop_na(county) %>%
      select(-c(cells)) %>%
      as.data.table() %>% 
      melt(., id.vars = c("county"),
           variable.name = "date",
           value.name = "value",
           variable.factor=FALSE) 
      
    out <- var.block[,.(value=base::mean(value,na.rm=T)),by=.(county,date)][,`:=`(var=vname)]
    
    dir_name = str_c("cache/",vname)
    if(!dir.exists(dir_name)) dir.create(dir_name)
    write_parquet(out,paste0(dir_name,"/",vname,".parquet"))
    
  # },.progress = T)
    

 
# for.dale <- gridmet.county %>%
#   mutate(quarter=quarter(date,with_year = T)) %>%
#   group_by(county,quarter) %>%
#   summarize(value=base::mean(value,na.rm=T)) 
#   
# write_csv(for.dale,
#           path = str_c("_Rdata/pdsi_quarter_county_",min(yr.range),"-",max(yr.range),".csv"))
# 
# haven::write_dta(for.dale,
#                  path = str_c("_Rdata/pdsi_quarter_county_",min(yr.range),"-",max(yr.range),".dta"))
  
#######################################
#Plot the data to see if fits intuition
# us_st <- us_states(resolution = "low") %>% st_transform(readin.proj) %>% dplyr::filter(!(state_abbr %in% c("AK","HI","PR")))
# g.rmin <- st_as_sf(nc.df, coords = c("lon","lat")) %>% st_set_crs(readin.proj)
# #plot data in CA
# test <- ggplot() +
#   geom_sf(data = g.rmin %>% st_transform(5070),aes(color=value)) +
#   geom_sf(data = us_st %>% st_transform(5070),color="white",fill=NA)
# #scale_color_gradient(limits=c(0,20)) +
# #labs(title="",caption="")
# 
# ggsave(filename = "temp.pdf",plot=test, width = 11, height = 9, units = "in")

########################################

