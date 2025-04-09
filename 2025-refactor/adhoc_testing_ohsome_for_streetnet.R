library(ohsome)
library(mapview)
library(tidyverse)

mapview(tracts.within.city.2km.buffer.utm)
sample_tract <- tracts.within.city.2km.buffer.utm[811,]
sample_tract.lonlat <- sample_tract %>% sf::st_transform(crs=crs_lonlat)
mapview(sample_tract.lonlat)

sample_tract_streets_2019 <- 
  ohsome_elements_geometry(
  boundary = sample_tract.lonlat, 
  filter = "highway=*", 
  time = "2019-06-01",
  properties = "tags", 
  clipGeometry = FALSE
) |>
  ohsome_post() %>% 
  select("@osmId",name,highway,footway,geometry) %>% 
  rename(osm_id = "@osmId") %>% 
  rowwise() %>% mutate(osm_id = str_split(osm_id,"/")) %>% 
  rowwise() %>% mutate(osm_type = osm_id[1], osm_id = osm_id[2])

way_type <- 
  sample_tract_streets_2019 %>% 
  sf::st_drop_geometry() %>% 
  group_by(highway) %>%
  summarise(n=n())

sample_tract_sidewalks_2019 <- 
  sample_tract_streets_2019 %>% 
  filter(highway=="footway"|footway=="sidewalk")

mapview(sample_tract.lonlat) + sample_tract_sidewalks_2019 






