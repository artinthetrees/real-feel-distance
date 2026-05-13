#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","street_network",city_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
crs_lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_lonlat)

print(crs_utm)
print(crs_lonlat)

boundary.utm <- 
  get_obj_from_rdata(
    rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),
    obj_name = city.2km.buffer.utm
    )

#################################################################
# work for this step

boundary.lonlat <- boundary.utm %>% sf::st_transform(crs=crs_lonlat)

# create bounding box vector of coordinates from boundary lonlat sf - named vector in xmin, ymin, xmax, ymax order
sf_bb <- sf::st_bbox(boundary.lonlat)

# convert bounding box vector of coordinates to sf >> input to ohsome streetnet fx
sf_bb_to_sf <- bbox_coords_to_sf(bbox_coords = sf_bb,crs=crs_lonlat)

# convert bounding box vector of coordinates to osmdata bounding box matrix format >> input to dodgr streetnet fx
sf_bb_to_osmdata_bb <- bbox_coords_to_osmdata_bb(bbox_coords = sf_bb)

streetnet_query_date <- Sys.Date()
streetnet_sf.lonlat <- get_streetnet_sf_dodgr(osmdata_bb = sf_bb_to_osmdata_bb, expand = 0)

#################################################################
# save for this step

save(
  sf_bb,
  sf_bb_to_sf,
  sf_bb_to_osmdata_bb,
  streetnet_query_date,
  streetnet_sf.lonlat,
  file = output_path
)

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################


# streetnet_ohsome_latest <- get_streetnet_sf_ohsome(boundary_map = sf_bb_to_sf)
# save(
#   streetnet_ohsome_latest,
#   file = "./2025-refactor/chicago_streetnet_ohsome_latest.Rdata"
# )

# streetnet_dodgr_latest <- get_streetnet_sf_dodgr(osmdata_bb = sf_bb_to_osmdata_bb, expand = 0)
# save(
#   streetnet_dodgr_latest,
#   file = "./2025-refactor/chicago_streetnet_dodgr_latest.Rdata"
# )
# 
# streetnet_ohsome_latest_mini <- 
#   process_streetnet_ohsome(streetnet_sf = streetnet_ohsome_latest) %>%
#   select(osm_id,name,highway,geometry) %>%
#   get_geom_type_col()
# 
# streetnet_dodgr_latest_mini <- 
#   streetnet_dodgr_latest %>%
#   select(osm_id,name,highway,geometry) %>%
#   get_geom_type_col()
# 
# dodgr_ids <- streetnet_dodgr_latest_mini$osm_id
# ohsome_ids <- streetnet_ohsome_latest_mini$osm_id
#   
# only_dodgr_ids <- setdiff(dodgr_ids,ohsome_ids)
# only_ohsome_ids <- setdiff(ohsome_ids,dodgr_ids)
# 
# ohsome_only <-
#   streetnet_ohsome_latest_mini %>% sf::st_drop_geometry() %>%
#   anti_join(streetnet_dodgr_latest_mini %>% sf::st_drop_geometry(), by = join_by(osm_id))
# 
# dodgr_only <-
#   streetnet_dodgr_latest_mini %>% sf::st_drop_geometry() %>%
#   anti_join(streetnet_ohsome_latest_mini %>% sf::st_drop_geometry(), by = join_by(osm_id))
