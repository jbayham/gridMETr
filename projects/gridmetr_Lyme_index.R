#This implementation of gridmetr compiles weather data for the project
#investigating the impact of pollution on farm worker productivity

#Project setup
source("project_init.R")

# #Define variables to use to construct data (see _ref/variables_reference.csv)
# g.vars <- c("pr","rmin","rmax","tmmn","tmmx","vs","th")
# 
# #Define years to construct data
# g.years <- seq.int(2003,2018)
# #########################################################################
# #Get netcdf data from source and cache copies in data folder 
# gridmetr_download(variables = g.vars,
#                   years = g.years)  
# 

#Loading output data
load("output/gridmet_cbsa_daily_2003-2018.Rdata")

load("output/gridmet_county_daily_2003-2018.Rdata")


##############################
#Calculating monthly means
gridmet.monthly.cbsa <- gridmet.out.cbsa %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(variable,cbsa,year,month) %>%
  summarize(min=min(value, na.rm = T),
            mean=mean(value, na.rm = T),
            median=median(value, na.rm = T),
            max=max(value, na.rm = T)) %>%
  ungroup() %>%
  gather(-variable,-cbsa,-month,-year,
         key="measure",value = "value") %>%
  mutate(variable=str_c(variable,"_",measure)) %>%
  select(-measure) %>%
  spread(key = variable,
         value = value)


gridmet.monthly.county <- gridmet.out.county %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by(variable,county,year,month) %>%
  summarize(min=min(value, na.rm = T),
            mean=mean(value, na.rm = T),
            median=median(value, na.rm = T),
            max=max(value, na.rm = T)) %>%
  ungroup() %>%
  gather(-variable,-county,-month,-year,
         key="measure",value = "value") %>%
  mutate(variable=str_c(variable,"_",measure)) %>%
  select(-measure) %>%
  spread(key = variable,
         value = value)

save(gridmet.monthly.cbsa,gridmet.monthly.county,
     file = "output/monthly_weather.Rdata")




