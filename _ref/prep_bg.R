#get tiger block groups
library(XML)
library(RCurl)


map(1:100,
    function(x){
      tryCatch({
        file.number <- str_pad(x,2,"left",0)
        download.file(str_c("https://www2.census.gov/geo/tiger/TIGER2019/BG/tl_2019_",file.number,"_bg.zip"),
                      destfile = str_c("_ref/geographies/bg/tl_2019_",file.number,"_bg.zip"))
      }, error = function(e){
        message(e)
      }
      )
    })

bg.list <- map(1:100,
    function(x){
      tryCatch({
        file.number <- str_pad(x,2,"left",0)
        #read in
        temp <- read_sf(str_c("_ref/geographies/bg/tl_2019_",file.number,"_bg.shp")) %>%
          select(geoid=GEOID)
        
        #if not Multipolygon, make it
        if(any(!st_is(temp,'MULTIPOLYGON'))){
          temp <- st_cast(temp,'MULTIPOLYGON')
        }
        return(temp)
      }, error = function(e){
        message(e)
      }
      )
    }) %>%
  compact()

us_bg <- st_as_sf(rbindlist(bg.list)) %>%
  st_transform(4269)

save(us_bg,file="_ref/geographies/block_groups.Rdata")
