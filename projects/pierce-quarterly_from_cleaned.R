library(pacman)
p_load(tidyverse,data.table,lubridate,measurements,tigris,janitor,sf,conflicted)

conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")
###################################################
#Construct metro labels
metro <- read_sf("data/cbsa19/tl_2019_us_cbsa.shp") %>% 
  filter(LSAD=="M1") %>%
  st_set_geometry(NULL)

# cbsa <- core_based_statistical_areas(cb=F,class="sf",year = 2019) %>%
#   clean_names()

# metro <- cbsa %>%
#   filter(lsad=="M1") %>%
#   st_set_geometry(NULL)

metro_names <- select(metro,GEOID,NAME) %>%
  mutate(geoid= as.numeric(GEOID))


#Original data prepped by Gal
pd <- fread("../../../../RSTOR/pierce_pm/cbsa_daily_weightedpmsmoke_weightedgm.csv.gz",keepLeadingZeros = T) 

#New PM data using adjustment
pmdata <- read_rds("../../../../RSTOR/pierce_pm/weighteddata2_census2019.rds")




##########################
#Combine weather and pm

#Check discrepency
resid <- anti_join(pd %>% distinct(cbsa,date) %>% mutate(date=as_date(date)),
                   pmdata %>% distinct(cbsa,date) %>% mutate(cbsa=as.integer(cbsa))) %>%
  left_join(metro_names %>% mutate(cbsa=as.integer(geoid)))

distinct(resid,cbsa,NAME)

####################################
#Quarterly means
pd_q <- pd %>%
  group_by(cbsa,quarter=lubridate::quarter(date,with_year = T)) %>%
  summarize(across(c(rmax:vs,weightedpm25),~mean(.,na.rm = T)),
            across(c(pr,weightedhms),~sum(.,na.rm = T))) %>%
  mutate(across(c(tmmn,tmmx),~conv_unit(.,"K","F")),
         across(vs,~conv_unit(.,"m_per_sec","mph")),
         across(pr,~conv_unit(.,"mm","inch"))) %>%
  inner_join(metro_names,by=c("cbsa"="geoid")) %>%
  drop_na() %>%
  select(cbsa,NAME,starts_with("weighted"),everything())

fwrite(pd_q,file = "../../../../RSTOR/pierce_pm/pm_weather_quarterly_mean.csv")

#Quarterly quantiles
pd_q_quan <- pd %>%
  drop_na() %>%
  mutate(across(c(tmmn,tmmx),~conv_unit(.,"K","F")),
         across(vs,~conv_unit(.,"m_per_sec","mph")),
         across(pr,~conv_unit(.,"mm","inch"))) %>%
  group_by(cbsa,quarter=lubridate::quarter(date,with_year = T)) %>%
  summarize(across(c(rmax:vs,weightedpm25),list(mean=~mean(.),
                                                min=~min(.),
                                                q25=~quantile(.,probs = .25),
                                                median=~quantile(.,probs = .5),
                                                q75=~quantile(.,probs = .75),
                                                max=~max(.))),
            across(c(pr,weightedhms),sum)) %>%
  inner_join(metro_names,by=c("cbsa"="geoid")) %>%
  select(cbsa,NAME,quarter,starts_with("weighted"),everything())

fwrite(pd_q_quan,file = "../../../../RSTOR/pierce_pm/pm_weather_quarterly_quartiles.csv")


#pd_q_quan <- fread("L:/My Drive/Projects/EV_pollution/data_outputs/pm_weather_quarterly_quartiles.csv")
