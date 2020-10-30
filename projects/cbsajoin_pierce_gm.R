library(tidyverse)
library(lubridate)

pmsmoke <- read_rds("../../../../RSTOR/pierce_pm/weighteddata.rds") %>% 
  mutate(cbsa = as.numeric(cbsa))

flgrid <- tibble(files = list.files("data/gridmetoutput_weighted", full.names = T),
                 var = str_extract(str_sub(files,29,-10),".{2,4}(?=_)"))
vars <- unique(flgrid$var)
# v <- vars[1]
# f <- flgrid.act$files[1]

allcbsa <- map(vars,function(v){
  
  flgrid.act <- flgrid %>% 
    filter(var == v)
  
  singlevar <- map_dfr(flgrid.act$files,function(f){
    
    temp <- read_rds(f) %>% 
      rename(!!v := value)
    
  })
})

gm <- reduce(allcbsa, left_join, by =c("cbsa","date")) 
all <- left_join(pmsmoke,gm,by=c("cbsa","date")) %>% 
  mutate(Month = month(date)) %>% 
  group_by(Month,cbsa) %>% 
  summarise()



write_csv(all,"../../../../RSTOR/pierce_pm/cbsa_daily_weightedpmsmoke_weightedgm.csv.gz")

unweighted <- read_csv("../../../../RSTOR/pierce_pm/cbsa_daily_weightedpmsmoke_unweightedgm.csv")

compar <- left_join(all,unweighted, by = c("cbsa","date"))
