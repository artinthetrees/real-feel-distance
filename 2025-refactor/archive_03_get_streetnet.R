#https://github.com/GIScience/ohsome-r

library(ohsome)

hd_test_2 <- 
ohsome_elements_geometry(
  boundary = place_bb, 
  filter = "highway=* and type:way", 
  time = "2021-12-01",
  properties = "tags", 
  clipGeometry = FALSE
) |>
  ohsome_post()

hd_test_3 <- 
  ohsome_elements_geometry(
    boundary = place_bb, 
    filter = "highway=*", 
    time = "2021-12-01",
    properties = "tags", 
    clipGeometry = FALSE
  ) |>
  ohsome_post()

hd_test_4 <- 
  ohsome_elements_geometry(
    boundary = place_bb, 
    filter = "highway=*", 
    time = "2025-02-02",
    properties = "tags", 
    clipGeometry = FALSE
  ) |>
  ohsome_post()
hd_test_4_sub <- hd_test_4 %>% select("@osmId",name,highway,paved,smoothness,lit,type,geometry)
hd_test_4_process <- hd_test_4_sub %>% rename(osm_id = "@osmId") %>% rowwise() %>% mutate(osm_id = str_split(osm_id,"/")) %>% rowwise() %>% mutate(osm_id = osm_id[2])

hd_test_5 <- 
  ohsome_elements_geometry(
    boundary = place_bb, 
    filter = "highway=* and type:way", 
    time = "2025-02-02",
    properties = "tags", 
    clipGeometry = FALSE
  ) |>
  ohsome_post()
hd_test_5_sub <- hd_test_5 %>% select("@osmId",name,highway,paved,smoothness,lit,type,geometry)
hd_test_5_process <- hd_test_5_sub %>% rename(osm_id = "@osmId") %>% rowwise() %>% mutate(osm_id = str_split(osm_id,"/")) %>% rowwise() %>% mutate(osm_id = osm_id[2])

hd_test_6 <- 
  ohsome_elements_geometry(
    boundary = city.2km.buffer.utm, 
    filter = "highway=* and type:way", 
    time = "2025-02-02",
    properties = "tags", 
    clipGeometry = FALSE
  ) |>
  ohsome_post()
hd_test_5_sub <- hd_test_5 %>% select("@osmId",name,highway,paved,smoothness,lit,type,geometry)
hd_test_5_process <- hd_test_5_sub %>% rename(osm_id = "@osmId") %>% rowwise() %>% mutate(osm_id = str_split(osm_id,"/")) %>% rowwise() %>% mutate(osm_id = osm_id[2])


test_streetnet_sub <- test_streetnet %>% select(osm_id,name,highway,paved,smoothness,lit,type,geometry)






place_bb <- osmdata::getbb(place_name=paste(city_string,state_string,sep=", "), featuretype = "city")
print(place_bb)

test_streetnet <- dodgr::dodgr_streetnet(bbox = place_bb, expand = 0.05)
test_streetnet_geometry <- test_streetnet %>% select(osm_id,name,highway,geometry) 
test_streetnet_geometry.utm <- test_streetnet_geometry %>% sf::st_transform(crs=crs_utm)

check.streetnet.cover.city.2km.buffer.plot <- ggplot() + geom_sf(data=test_streetnet_geometry.utm) + geom_sf(data=city.2km.buffer.utm, fill=NA, color="red") + geom_sf(data=counties.within.city.2km.buffer.utm, fill=NA, color="yellow")

streetnet.clipped.by.city.2km.buffer.utm <- 
  rmapshaper::ms_clip(target=test_streetnet_geometry.utm, clip=city.2km.buffer.utm, remove_slivers = FALSE)

check.clipped.streetnet.cover.city.2km.buffer.plot <- ggplot() + geom_sf(data=streetnet.clipped.by.city.2km.buffer.utm) + geom_sf(data=city.2km.buffer.utm, fill=NA, color="red") + geom_sf(data=counties.within.city.2km.buffer.utm, fill=NA, color="yellow")

########################################################################


streetnet.lonlat <- dodgr_streetnet_update(bbox = place_bb, datetime = "2020-06-01T00:00:00Z")


graph <- dodgr::weight_streetnet (dat_sf, wt_profile = "foot")

v <- dodgr::dodgr_vertices (graph)
head (v)

sf::st_crs(dat_sf)$proj4string
sf::st_crs(dat_sf)$units

dat_sf.utm <-
  sf::st_transform(dat_sf, 
                   crs = crs_utm) 

sf::st_crs(dat_sf.utm)$proj4string




