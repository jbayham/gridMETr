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
measurements::conv_unit(2.53,"m_per_sec","mph")

gridmet.out.cbsa %>%
  mutate(value=ifelse(str_detect(variable,"vs"),   #Windspeed m/s to mph
                      measurements::conv_unit(value,"m_per_sec","mph"),
                      value),
         value=ifelse(str_detect(variable,"precipitation"),   #precip mm to inch
                      measurements::conv_unit(value,"mm","inch"),
                      value),
         value=ifelse(str_detect(variable,"tmm"),             #Temp Kelvin to C
                      measurements::conv_unit(value,"K","C"),
                      value)
         ) %>%
  filter(variable!="th_wind_from_direction")
unique(gridmet.out.cbsa$variable)
#Loading output data
load("output/gridmet_cbsa_daily_2003-2018.Rdata")

load("output/gridmet_county_daily_2003-2018.Rdata")


##############################
#Calculating monthly means
gridmet.year.cbsa <- gridmet.out.cbsa %>%
  mutate(value=ifelse(str_detect(variable,"vs"),   #Windspeed m/s to mph
                      measurements::conv_unit(value,"m_per_sec","mph"),
                      value),
         value=ifelse(str_detect(variable,"precipitation"),   #precip mm to inch
                      measurements::conv_unit(value,"mm","inch"),
                      value),
         value=ifelse(str_detect(variable,"tmm"),             #Temp Kelvin to C
                      measurements::conv_unit(value,"K","C"),
                      value)
  ) %>%
  filter(variable!="th_wind_from_direction") %>%
  mutate(month = month(date),
         year = year(date)) %>%
  filter(between(month,5,10)) %>%
  group_by(variable,cbsa,year) %>%
  summarize(min=min(value, na.rm = T),
            mean=mean(value, na.rm = T),
            median=median(value, na.rm = T),
            max=max(value, na.rm = T)) %>%
  ungroup() %>%
  gather(-variable,-cbsa,-year,
         key="measure",value = "value") %>%
  mutate(variable=str_c(variable,"_",measure)) %>%
  select(-measure) %>%
  spread(key = variable,
         value = value)


gridmet.year.county <- gridmet.out.county %>%
  mutate(value=ifelse(str_detect(variable,"vs"),   #Windspeed m/s to mph
                      measurements::conv_unit(value,"m_per_sec","mph"),
                      value),
         value=ifelse(str_detect(variable,"precipitation"),   #precip mm to inch
                      measurements::conv_unit(value,"mm","inch"),
                      value),
         value=ifelse(str_detect(variable,"tmm"),             #Temp Kelvin to C
                      measurements::conv_unit(value,"K","C"),
                      value)
  ) %>%
  filter(variable!="th_wind_from_direction") %>%
  mutate(month = month(date),
         year = year(date)) %>%
  filter(between(month,5,10)) %>%
  group_by(variable,county,year) %>%
  summarize(min=min(value, na.rm = T),
            mean=mean(value, na.rm = T),
            median=median(value, na.rm = T),
            max=max(value, na.rm = T)) %>%
  ungroup() %>%
  gather(-variable,-county,-year,
         key="measure",value = "value") %>%
  mutate(variable=str_c(variable,"_",measure)) %>%
  select(-measure) %>%
  spread(key = variable,
         value = value)

gridmet.year <- bind_rows(gridmet.year.cbsa %>% mutate(ident=as.numeric(cbsa)),
                          gridmet.year.county %>% mutate(ident=as.numeric(county)))

save(gridmet.year,file = "output/year_weather.Rdata")


