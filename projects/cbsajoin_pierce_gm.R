library(tidyverse)


pmsmoke <- read_rds("../../../../RSTOR/pierce_pm/weighteddata.rds") 

flgrid <- str_subset(list.files("data/cbsa_filtered", full.names = T),"summed")
fg <- flgrid[2]

r1 <- read_rds(flgrid[1]) %>% 
  rename(!!str_sub(flgrid[1],27,-5) := value) 

r2 <- map(flgrid[2:6],function(fg){
  
  temp <- read_rds(fg) %>% 
    reduce(.,bind_rows) %>% 
    rename(!!str_sub(fg,27,-5) := value) 
  
})

gm <- reduce(r2, left_join, by =c("cbsa","date")) %>% 
  left_join(., r1, by=c("cbsa","date"))

all <- left_join(gm,pmsmoke,by=c("cbsa","date")) 

write_csv(all,"../../../../RSTOR/pierce_pm/cbsa_daily_weightedpmsmoke_unweightedgm.csv")

check <- all %>% 
  filter(date >= as.Date("2006-01-01"),
         !is.na(weightedpm25))

