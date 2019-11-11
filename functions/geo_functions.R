
get_grid <- function(nc.file=NULL){
  #Description:  This function extracts the lat and lon coordinates from a
  #gridmet netcdf file.  All gridmet files are on the same grid of lat and lons
  #so grabbing one is sufficient for calculating the index of all files.
  
  #inputs:
  #nc.file: relative path to a specific netcdf file used to extract the 
  #gridpoints (e.g.: "data/pr/pr_2006.nc").  If no file is specified, the 
  #function finds the first netcdf file in the data directory.
  
  #outputs:
  #coordinates data frame
  
  require(ncdf4)
  #######################################################
  
  if(is.null(nc.file)){
    nc.file <- list.files("data",recursive = T,pattern = ".nc",full.names = T)[1]
  }
  #Open the connection to the netCDF file
  nc <- nc_open(nc.file)
  
  
  #Extract lat and lon vectors
  nc_lat <- ncvar_get(nc = nc, varid = "lat")
  nc_lon <- ncvar_get(nc = nc, varid = "lon")
  
  #Use the lat and lon vectors to create a grid represented as two vectors (note:
  #lon must go first to match with netcdf data)
  nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)
  
  return(nc.coords)
}

################################################################################
#unit test
#test.coords <- get_grid()
#test.coords <- get_grid(nc.file = "data/tmmn/tmmn_2006.nc")

################################################################################
################################################################################
################################################################################

geo_index_points <- function(nc.coords,query.points){
  #This function constructs an index associating gridmet coordinates with
  #user-defined points
  
  #Arguments:
    #nc.coords: a dataframe with at least two columns called "lon" and "lat".  
    #All other columns will be ignored.
  
    #query.points: a dataframe of points defined by two columns "lon" and "lat".
      #future version should accept sf and matrix objects
    
  #Output:
    # index to query points from gridmet netcdf files
  
  require(sf)
  require(dplyr)
  require(lubridate)
  
  #Choose a projection to be used by all geographic files
  readin.proj=4269 #because it works with the lat and lons provided
  
  ########################################################
  #Converting nc coordinates from dataframe form to simple feature (sf)
  g.nc.coords <- 
    nc.coords %>%
    select(lon,lat) %>%
    st_as_sf(coords = c("lon","lat")) %>% 
    st_set_crs(readin.proj)
  
  #Converting query.points coordinates from dataframe form to simple feature (sf)
  g.query.points <- 
    query.points %>%
    select(lon,lat) %>%
    st_as_sf(coords = c("lon","lat")) %>% 
    st_set_crs(readin.proj)
  
  #Find nearest nc.coord point to each query.point
  nc.coord.index <- 
    st_nearest_feature(g.query.points,g.nc.coords) %>%
    nc.coords[.,]
  
  #Construct dataframe of indices for use in nc_get query
  out.index <- data.frame(id=seq.int(1,dim(query.points)[1]),
                          lon.index=match(nc.coord.index[,1],nc_lon),
                          lat.index=match(nc.coord.index[,2],nc_lat)) 
  
  
  temp.out <- map_dfr(split(out.index,row(out.index)),
                      function(x){
                        df.out <- data.frame(id=x$id,
                                             date=as_date(nc$dim$day$vals,origin="1900-01-01"),
                                             value = ncvar_get(nc = nc, varid = "precipitation_amount", start = c(x$lon.index,x$lat.index,1),count = c(1,1,-1)))
                      }
  )
  
  #Decision: whether to keep nc_get calls here or move them to own function.  I
  #think move to own function but need to catalog what information needs to be
  #passed around between functions.
  
  return(out.index)

  }
# tictoc::tic()
# temp.nc.layer <- ncvar_get(nc = nc, varid = "precipitation_amount", start = c(25,40,1),count = c(1,1,-1))
# tictoc::toc()
# 
# farm.loc <- tibble(name=c("NCA","SCA_R1","SCA_R2","SCA_R3","new farm"),
#                    lon=c(-121.789879,-120.539803,-120.579262,-120.605598,-119.138268),
#                    lat=c(36.823952,35.003971,34.975948,35.028221,34.177818))
# 
# query.points = farm.loc



# geo_index_polygons <- function(){
#   #This function constructs an index that maps the gridmet coordinates to 
#   #user-defined polygons
#   
#   
#   
# }