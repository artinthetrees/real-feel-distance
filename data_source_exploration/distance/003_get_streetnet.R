# https://github.com/ATFutures/dodgr
# http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# https://rspatialdata.github.io/osm.html
# https://stackoverflow.com/questions/52248394/get-census-tract-from-lat-lon-using-tigris
# https://stackoverflow.com/questions/29872109/binning-longitude-latitude-labeled-data-by-census-block-id
# https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-open-shapefiles-r

# https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html

# https://stackoverflow.com/questions/7477003/calculating-new-longitude-latitude-from-old-n-meters
# 10km ~ 0.1 degree lat/long; 1km ~ 0.01 degree; .1km ~ 0.001 degree

# library(dodgr)
# 
# library(tidyverse)
# library(osmdata) # package for working with streets
# library(showtext) # for custom fonts
# library(ggmap)
# library(rvest)
# library(sf)
# 
# library(rgdal)
# library(sp)
# library(raster)

# # get polygon that adds a 5km buffer to the chicago cityboundary polygon
# # this will be used to get a bounding box for grabbing street network for chicago and immediate surroundings
# chicago.city.buffer.polygon.sf.utm <-
#   sf::st_buffer(chicago.city.utm, dist = 2000)
# 
# chicago.city.buffer.polygon.sf.lonlat <-
#   sf::st_buffer(chicago.city, dist = 2000)

# dat_sf <- dodgr::dodgr_streetnet(bbox = sf::st_bbox(chicago.city.buffer.polygon.sf.lonlat))
# dat_sf <- dodgr::dodgr_streetnet("cook county illinois")

# dat_sf <- dodgr::dodgr_streetnet("chicago") 177992

# # get polygon that adds a 2km buffer to the chicago boundary polygon
# # this will be used to grab street network for chicago and 2km surrounding
# chicago.city.buffer.polygon.sf.lonlat <-
#   sf::st_buffer(chicago.city, dist = 2000)
# 
# chicago.city.buffer.bbox <- sf::st_bbox(chicago.city.buffer.polygon.sf.lonlat)

# dat_sf <- 
#   osmdata::opq(bbox = chicago.city.buffer.bbox) %>%
#   osmdata::add_osm_feature(key = 'highway') %>%
#   osmdata::osmdata_sf ()

# dat_sf <- 
#   dodgr::dodgr_streetnet(bbox = chicago.city.buffer.bbox)

dat_sf <- dodgr::dodgr_streetnet("chicago") 

graph <- dodgr::weight_streetnet (dat_sf, wt_profile = "foot")

v <- dodgr::dodgr_vertices (graph)
head (v)

sf::st_crs(dat_sf)$proj4string
sf::st_crs(dat_sf)$units

dat_sf.utm <-
  sf::st_transform(dat_sf, 
                   crs = "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80") 

sf::st_crs(dat_sf.utm)$proj4string




