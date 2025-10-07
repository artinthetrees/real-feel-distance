# https://cran.r-project.org/web/packages/tidytransit/vignettes/frequency.html
# https://r-transit.github.io/tidytransit/articles/introduction.html#finding-more-gtfs-feeds

library(sf)
library(leaflet)
#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","gtfs_data",city_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step
################################################################################

crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
print(crs_utm)

crs_lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_lonlat)
print(crs_lonlat)

city.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = city.utm)
city.2km.buffer.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = city.2km.buffer.utm)
################################################################################

## define fx to create polygon from bbox coordinates
bbox_polygon = function(lon_min, lon_max, lat_min, lat_max) {
  corner_coords = matrix(
    c(lon_min, lat_min,
      lon_min, lat_max,
      lon_max, lat_max,
      lon_max, lat_min,
      lon_min, lat_min),
    ncol = 2, byrow = TRUE
  )
  polyg = st_polygon(list(corner_coords))
  return(st_sfc(polyg, crs = 4326))
}

## define url at which to find documentation of gtfs feeds
mbd_url = "https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media"

################################################################################

MobilityData.csv = read.csv(mbd_url)

MobilityData_feedlist = MobilityData.csv %>% 
  as_tibble() %>% 
  filter(data_type == "gtfs" & !status %in% c("deprecated","inactive"))

MobilityData_sf = MobilityData_feedlist %>% 
  filter(!is.na(location.bounding_box.minimum_longitude)) %>% 
  #filter(location.bounding_box.minimum_latitude > -89) %>%
  filter(location.bounding_box.minimum_latitude > 0) %>%
  #filter(location.country_code == "US" & location.subdivision_name == "Illinois") %>%
  filter(location.country_code == "US") %>%
  group_by(mdb_source_id) %>% 
  mutate(geometry = bbox_polygon(location.bounding_box.minimum_longitude,
                                 location.bounding_box.maximum_longitude,
                                 location.bounding_box.minimum_latitude,
                                 location.bounding_box.maximum_latitude)) %>% 
  ungroup() %>% 
  st_as_sf()


# check if produced bounding boxes overlap with area of interest plus 2km buffer and keep if yes - use st_intersects
# https://gis.stackexchange.com/questions/437047/identify-overlapping-polygons-within-a-single-multipolygon
# https://stackoverflow.com/questions/57014381/how-to-filter-an-r-simple-features-collection-using-sf-methods-like-st-intersect

MobilityData_sf <- sf::st_transform(MobilityData_sf,crs = crs_utm) 
#MobilityData_sf <- MobilityData_sf %>% rowwise() %>% mutate(servesAreaOfInterest=sf::st_intersects(city.2km.buffer.utm,.,sparse = FALSE)[1,1])
servesAreaOfInterest <- sf::st_intersects(city.2km.buffer.utm,MobilityData_sf,sparse = FALSE)
MobilityData_sf$servesAreaOfInterest <- servesAreaOfInterest[1,]
MobilityData_sf <- MobilityData_sf %>% filter(servesAreaOfInterest)

## uncomment to visualize
# leaflet() %>% 
#   addProviderTiles(provider = providers$CartoDB.Positron) %>% 
#   addPolygons(data = MobilityData_sf %>% sf::st_transform(crs = crs_lonlat), weight = 2, 
#               fillOpacity = 0.1, label = substr(MobilityData_sf$provider, 0, 60))

for (i in 1:nrow(MobilityData_sf)) {
  fname <- paste0(MobilityData_sf$provider[i],".zip")
  local_path <- Gmisc::pathJoin(output_dir,"intermediate","r5r_routing_input_data",city_string,fname)
  print(local_path)
  download.file(MobilityData_sf$urls.direct_download[i],local_path)
}

## uncomment to check that gtfs data has been downloaded properly
# test_amtrak <- tidytransit::read_gtfs("./2025-refactor/output_data/intermediate/r5r_routing_input_data/Chicago/Amtrak.zip")
# summary(test_amtrak)



