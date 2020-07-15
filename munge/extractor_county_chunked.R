#This script provides an example for one way to extract data from netcdf files

#setwd("G:/Weather/gridMETr/")
source("project_init.R")

##################################
##################################
#User-defined variables (use vector of string names or "." for wildcard):

#Define folders (variables) to extract -
folder.names <- c("sph","vpd","pr","rmin","rmax","tmmn","tmmx",
                  "vs","th","srad","pdsi","pet","etr",
                  "erc","bi","fm100","fm1000")
#Define set of years 
filter.years <- seq.int(1979,2019)
##################################
# list.files("data",recursive = T,pattern = ".nc",full.names = T) %>%
#   str_subset(.,str_c(year(today()),collapse = "|")) %>%
#   str_subset(.,str_c(folder.names,collapse = "|")) %>%
#   file.remove()

gridmetr_download(folder.names,filter.years)
##################################


#All gridmet files are on the same grid of lat and lons so grabbing one
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T) %>%
  str_subset(.,str_c(filter.years,collapse = "|")) %>%
  str_subset(.,str_c(folder.names,collapse = "|"))

#Open the connection to the netCDF file
nc <- nc_open(file.names[1])


#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)

#Load variable names and descriptions
all_names <- read_csv("_ref/variables_reference.csv") %>%
  as.data.table()


##############################################
#Use GIS tools to aggregate data by chosen geography
##############################################
#Choose a projection to be used by all geographic files
readin.proj=4269 #because it works with the lat and lons provided

#Converting nc coordinates from vector form to simple feature (sf)
g.nc.coords <- st_as_sf(nc.coords, coords = c("lon","lat")) %>% 
  st_set_crs(readin.proj)


#Attaching geographic data to netcdf grid -- ignore warning
bridge.county <- st_join(g.nc.coords,us_co,left=T) %>% 
  dplyr::select(county=geoid) %>%
  mutate(lon=as.vector(st_coordinates(.)[,1]),
         lat=as.vector(st_coordinates(.)[,2])) %>%
  st_set_geometry(NULL)




#######################
#For parallelization with future
#plan(multiprocess(workers = 2))
# future_map_dfr(...)
##########################
pb <- progress_bar$new(
  format = " [:bar] :percent eta: :eta \n",
  total = length(file.names), clear = FALSE, width= 60)
#Begin loop over variables (folders)

map(str_subset(dir("data"),pattern=str_c(folder.names,collapse = "|")),
    function(fn){
      message(str_c("Beginning ",fn,"..."))
      
      #Finding the names of all files in the directory
      file.names <- dir(str_c("data/",fn)) %>%
        str_subset(pattern=str_c(filter.years,collapse = "|"))
      
      #Setting static local folder name (probably unecessary)
      #folder.temp=fn
      
      #Extracting variable name for organization below
      #Open the connection to the netCDF file
      nc <- nc_open(str_c("data/",fn,"/",file.names[1]))
      
      #Set variable name
      var.id=names(nc$var)
      var.new.name <- all_names[variable==fn,var_name]
      
      nc_close(nc)

      #Begin loop over years (files)
      #year.temp <- 
        map(file.names,
          function(f.names){
            pb$tick()
            
            if(file.exists(str_c("outputs/county/",file_path_sans_ext(f.names),".csv.gz"))){
              return()
            } else {
              
            #Open the connection to the netCDF file
            nc <- nc_open(str_c("data/",fn,"/",f.names))
            
            #Get dates
            date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
            
            #Read in year of observations and collapse to matrix with lat/lon on rows and dates as columns
            nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
            
            nc.data.array <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
              as.data.table()
            
            #nc.data2 <- as.data.table(nc.data)
            setnames(nc.data.array,as.character(date.vector))
            
            nc.df <- as.data.table(cbind(bridge.county,nc.data.array)) %>%
              melt(.,
                   id.vars = c("lon","lat","county"),
                   variable.name = "date",
                   value.name = "value",
                   variable.factor = FALSE) 
            
            nc.final <- nc.df[!is.na(county),
                              .(value=mean(value)),
                              keyby=.(county,date)] %>%
              .[,"date":=as_date(date)]
            
            setnames(nc.final,old = "value",new = var.new.name)
              
            fwrite(nc.final,file = str_c("outputs/county/",file_path_sans_ext(f.names),".csv.gz"))
            
            nc_close(nc)
            } #ends if statement
          }) 
      
  }) 

