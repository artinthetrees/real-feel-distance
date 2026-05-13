#library(tidyverse)
#library(sf)
library(osmextract)
#library(tmap)

#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

sub_output_dir <- Gmisc::pathJoin(output_dir,"intermediate","r5r_routing_input_data",city_string)
print(output_dir)

if (!dir.exists(sub_output_dir)) {dir.create(sub_output_dir,recursive = TRUE)}

#################################################################
# pull in objs needed from previous parts of pipeline for this step
################################################################################
# crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
# print(crs_utm)
# 
# crs_lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_lonlat)
# print(crs_lonlat)
# 
# city.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = city.utm)
# city.2km.buffer.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = city.2km.buffer.utm)
################################################################################

osm_extract_match <- osmextract::oe_match_pattern(city_string)

if ("bbbike" %in% names(osm_extract_match)){
  provider <- "bbbike"
} else if ("geofabrik" %in% names(osm_extract_match)){
  provider <- "geofabrik"
} else {
  provider <- NULL
}

# $bbbike
# [1] "Chicago"

#osmextract::oe_match_pattern("Illinois")

# $geofabrik
# [1] "us/illinois"
# 
# $openstreetmap_fr
# [1] "Illinois"

# chicago = osmextract::oe_get(
#   place = "Chicago",
#   provider = "bbbike", # Indicates the provider; default is geofabrik
#   layer = "lines", # Default; returns linestring geometries (highways, waterways, aerialways) 
#   force_download = TRUE, # Updates the previously downloaded .osm.pbf file (default is FALSE)
#   force_vectortranslate = TRUE, # Forces the vectorization of a .pbf file to .gpbf even if there is a .gpbf file with the same name (default = FALSE),
#   download_directory = "C:/Users/tentner-andrea/project-repositories/real-feel-distance/2025-refactor/output_data/intermediate/osm_data/2022",
#   version = "20220101"
# )

if (!is.null(provider)){
  osmextract::oe_get(
    place = city_string,
    provider = provider, # Indicates the provider; default is geofabrik
    layer = "lines", # Default; returns linestring geometries (highways, waterways, aerialways) 
    #force_download = TRUE, # Updates the previously downloaded .osm.pbf file (default is FALSE)
    #force_vectortranslate = TRUE, # Forces the vectorization of a .pbf file to .gpbf even if there is a .gpbf file with the same name (default = FALSE),
    download_directory = sub_output_dir
  )
}

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################