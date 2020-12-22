# https://github.com/r-spatial/rgee
# http://amazeone.com.br/barebra/pandora/rgeebookT1eng.pdf
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET#description

#####
#####

# STEP 1 | install python
# https://www.python.org/downloads/

# STEP 2 | sign up for google earth engine
# sign up for google earth engine
# https://earthengine.google.com

# STEP 3 | download rgee
library(remotes)
remotes::install_github("r-spatial/rgee")
library(rgee)

# STEP 4 | install and initialize
ee_install()
ee_Initialize()
ee_check()

#####
#####
library(rgee)
ee_Initialize()
library(sf)
library(tidyverse)

# test 1 | example recreate
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x) x$reproject("EPSG:4326")$select("pr"))

fire <- ee$ImageCollection("MODIS/006/MOD14A1")$
  filterDate("2020-08-01", "2020-09-01")$
  map(function(x) x$reproject("EPSG:4326"))

coco <- tigris::counties(state = "co") %>% 
  filter(NAME == "Larimer")

ee_co_fire <- ee_extract(x = fire, y = coco, sf = FALSE)


colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME




ee_nc_rain2 <- ee_extract(x = gridmet, y = coco, sf = FALSE,fun = ee$Reducer$sum())
colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME


# test 2 | gridmet CO
co <- tigris::states() %>% 
  filter(STUSPS == "CO") %>% 
  select(NAME) %>% 
  st_transform(4326)

copoint <- co %>% 
  st_centroid() %>% 
  st_coordinates()

gridmet <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET")$
  filterDate("2001-01-01", "2002-01-02")$
  # filterBounds(ee$Geometry$Point(copoint[1,"X"],copoint[1,"Y"]))$
  map(function(x) x$reproject("EPSG:4326")$select("pr"))

ee_co_gridmet <- ee_extract(x = gridmet, y = co, sf = FALSE)
colnames(ee_co_gridmet) <- sprintf("%02d", 1:12)
ee_co_gridmet$name <- co$NAME

# $select("pr"))
