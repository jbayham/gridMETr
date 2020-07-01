#This script provides an example for one way to extract data from netcdf files


################
#The intermediate steps should probably be written into a DB to spare accumulated space in RAM

###############

##################################
##################################
#User-defined variables (use vector of string names or "." for wildcard):

#Define folders (variables) to extract -
folder.names <- c("pr","rmin","rmax","tmmn","tmmx","vs","th","srad")
#Define set of years 
filter.years <- seq.int(2006,2018)
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

#Attaching geographic data to netcdf grid -- ignore warning
bridge.cbsa <- st_join(g.nc.coords,us_cbsa,left=T) %>% 
  dplyr::select(cbsa=geoid) %>%
  mutate(lon=as.vector(st_coordinates(.)[,1]),
         lat=as.vector(st_coordinates(.)[,2])) %>%
  st_set_geometry(NULL) %>%
  distinct(.,lat,lon,.keep_all = T)


#######################
#For parallelization with future
#plan(multiprocess(workers = 2))
# future_map_dfr(...)
##########################
pb <- progress_bar$new(
  format = " [:bar] :percent eta: :eta \n",
  total = length(file.names), clear = FALSE, width= 60)

#Begin loop over variables (folders)
gridmet.out <- 
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
        var.id=names(nc$var)

        #Begin loop over years (files)
        year.temp <- 
          map(file.names,
            function(f.names){
              pb$tick()
              #Open the connection to the netCDF file
              nc <- nc_open(str_c("data/",fn,"/",f.names))
              
              #Get dates
              date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
              
              #Read in year of observations and collapse to matrix with lat/lon on rows and dates as columns
              nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
              
              nc.data <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
                as_tibble(.name_repair = ~str_c(date.vector))
          
              
              #Calculate mean of cells in county boundary
              var.county <- data.table(bind_cols(bridge.county, nc.data)) %>%
                .[!is.na(county), ] %>%
                melt(.,
                     id.vars = c("county", "lon", "lat"),
                     variable.name = "date") %>%
                .[, .(value = mean(value, na.rm = T)), by = .(county, date)] %>%
                .[order(county, date), .(county, date = ymd(date), value)] %>%
                as_tibble()
            
              
              #Calculate mean of cells in cbsa boundary
              var.cbsa <- data.table(bind_cols(bridge.cbsa, nc.data)) %>%
                .[!is.na(cbsa), ] %>%
                melt(.,
                     id.vars = c("cbsa", "lon", "lat"),
                     variable.name = "date") %>%
                .[, .(value = mean(value, na.rm = T)), by = .(cbsa, date)] %>%
                .[order(cbsa, date), .(cbsa, date = ymd(date), value)] %>%
                as_tibble()
              
              return(list(county=var.county,
                          cbsa=var.cbsa))
              
            }) 
        
        county.out <- map(year.temp,"county") %>%
          map_dfr(~mutate(.,variable=str_c(fn,"_",var.id)))
        
        cbsa.out <- map(year.temp,"cbsa") %>%
          map_dfr(~mutate(.,variable=str_c(fn,"_",var.id)))
        
        return(list(county=county.out,
                    cbsa=cbsa.out))    
        }) 

gridmet.out.county <- map_dfr(gridmet.out,"county")

gridmet.out.cbsa <- map_dfr(gridmet.out,"cbsa")

yr.range <- unique(year(gridmet.out.county$date))

save(gridmet.out.county,file = str_c("output/gridmet_county_daily_",min(yr.range),"-",max(yr.range),".Rdata"))

save(gridmet.out.cbsa,file = str_c("output/gridmet_cbsa_daily_",min(yr.range),"-",max(yr.range),".Rdata"))


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

