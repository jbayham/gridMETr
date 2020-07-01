##################################
##################################
#User-defined variables (use vector of string names or "." for wildcard):

#Define area: State(s)
state.entry <- c("CO","AZ")
  
# OR
# Define area: County(ies)
county.entry <- c(".")

# Define number of years / file 
years.per <- 4

# Define number of var / file 
var.per <- 3
  
##################################
##################################


#All gridmet files are on the same grid of lat and lons so grabbing one
file.names <- list.files("data",recursive = T,pattern = ".nc",full.names = T)

#Open the connection to the netCDF file
nc <- nc_open(file.names[1])

#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat)


##############################################
#Use GIS tools to aggregate data by chosen geography
##############################################

#Choose a projection to be used by all geographic files
readin.proj=4269 
#because it works with the lat and lons provided

#Converting nc coordinates from vector form to simple feature (sf)
g.nc.coords <- st_as_sf(nc.coords, coords = c("lon","lat")) %>% 
  st_set_crs(readin.proj)


#Set area for bridge
#PICK BASED ON USER-DEFINED##########################
area.act <- us_states(states = state.entry) %>%
  st_transform(4269)
area.act <- us_states(states = county.entry) %>%
  st_transform(4269)
#####################################################


#Attaching geographic data to netcdf grid -- ignore warning
bridge.co <- st_join(g.nc.coords,area.act,left=T) %>% 
  dplyr::select(state=geoid) %>%
  mutate(lon=as.vector(st_coordinates(.)[,1]),
         lat=as.vector(st_coordinates(.)[,2])) %>%
  st_set_geometry(NULL)


#Set map units
#PICK BASED ON USER-DEFINED##########################
folder.names.1 <- c("pr","tmmn","tmmx")
folder.names.2 <- c("th","vs")
folder.names.3 <- c("rmax","rmin","sph")




filter.years.1 <- filter.years[1:4]
filter.years.2 <- filter.years[5:8]
filter.years.3 <- filter.years[9:12]
filter.years.4 <- filter.years[13:16]
filter.years.5 <- filter.years[17:20]
#####################################################

#######################
#For parallelization with future
#plan(multiprocess(workers = 2))
# future_map_dfr(...)
##########################

# set up ticks
pb <- progress_bar$new(
  format = " [:bar] :percent eta: :eta \n",
  total = length(file.names), clear = FALSE, width= 60)

#Begin loop over variables (folders)
gridmet.co <- 
  map_dfr(str_subset(dir("data"),pattern=str_c(folder.names,collapse = "|")),
          function(fn){
            message(str_c("Beginning ",fn,"..."))
            
            #Finding the names of all files in the directory
            file.names <- dir(str_c("data/",fn)) %>%
              str_subset(pattern=str_c(filter.years,collapse = "|"))
            
            #Setting static local folder name (probably unecessary)
            #folder.temp=fn
            
            #Extracting variable name for organization below
            #Open the connection to the netCDF file
            nc <- nc_open(str_c("data/",fn,"/",file.names))
            var.id=names(nc$var)
            
            #Begin loop over years (files)
            year.temp <- 
              map_dfr(file.names,
                      function(f.names){
                        
                        pb$tick()
                        
                        #Open the connection to the netCDF file
                        nc <- nc_open(str_c("data/",fn,"/",f.names))
                        
                        #Get dates
                        date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
                        
                        #Read in year of observations and collapse to matrix with lat/lon on rows and dates as columns
                        nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
                        
                        nc.data <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
                          as_tibble(.name_repair = "universal") %>%
                          rename_all(~str_c(date.vector))
                        
                        #Organize nc.data into dataframe 
                        nc.df <- bind_cols(nc.coords,nc.data) %>%
                          pivot_longer(-c("lon","lat"), names_to = "date", values_to = "value")
                        
                        var.year <- bind_cols(bridge.co,nc.data) %>%
                          dplyr::filter(!is.na(state)) %>%
                          # pivot_longer(-c("lon","lat"), names_to = "date", values_to = "value")
                          gather(-one_of("lon","lat","state"),key="date",value="value") %>%
                          mutate(date=ymd(date))
                        # group_by(county,date) %>%
                        # summarize(value=base::mean(value,na.rm=T)) %>%
                        # ungroup() %>%
                        
                        return(var.year)
                        
                      }) %>% 
              mutate(variable=str_c(fn,"_",var.id))  #adding the variable name
            
            return(year.temp)
          }) %>%
  dplyr::filter(!is.na(value))

yr.range <- unique(year(gridmet.co$date))

save(gridmet.co,file = str_c("output/gridmet_co_daily_",
                             "rmax_rmin_sph_",
                             min(yr.range),"-",
                             max(yr.range),".Rdata"))


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

