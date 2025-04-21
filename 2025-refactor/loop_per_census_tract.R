library(tidyverse)
#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","distance_to_grocery",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
print(crs_utm)
crs_lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_lonlat)
print(crs_utm)

streetnet_vertices <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","street_network_products",city_output_fname),obj_name = v)
streetnet_graph <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","street_network_products",city_output_fname),obj_name = graph)

grocery_sf.lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","grocery",city_year_output_fname),obj_name = grocery_within_sf.lonlat)
grocery_sf.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","grocery",city_year_output_fname),obj_name = grocery_within_sf.utm)
grocery_df <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","grocery",city_year_output_fname),obj_name = grocery_within_df)

tracts.within.city.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = tracts.within.city.utm)
city.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = tracts.within.city.utm)

#################################################################
# work for this step

distances_df_list <- list()
summary_distances_df_list <- list()

for (i in 1:nrow(tracts.within.city.utm)){
  one_tract_sf.utm <- tracts.within.city.utm[i,]
  
  tract_id <- i
  print(tract_id)
  
  my_distances <- 
    get_distances_df(
      one_tract_sf.utm = one_tract_sf.utm,
      grocery_sf.utm = grocery_sf.utm, 
      streetnet_vertices = streetnet_vertices, 
      streetnet_graph = streetnet_graph, 
      crs_lonlat = crs_lonlat, 
      crs_utm = crs_utm
    )
  
  
  
  
}
  
  one_tract_sf.utm <- tracts.within.city.utm[i,]
  
  print(i)
  print(one_tract_sf.utm$NAMELSAD)
  print(one_tract_sf.utm$GEOID)
  
  
  
  summary_dist_to_grocery.list[[i]] <- summary_dist_to_grocery
  d.df.list[[i]] <- d.df
  

i <- 1

one_tract_sf.utm <- tracts.within.city.utm[i,]
one_tract_2km_buffer_sf.utm <- 
  sf::st_buffer(one_tract_sf.utm,dist=2000)


##### optional 
# streetnet_sf.lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","street_network",city_output_fname),obj_name = streetnet_sf.lonlat)
# 
# streetnet_sf.utm <- 
#   sf::st_transform(streetnet_sf.lonlat,
#                    crs = crs_utm) 
# 
# one_tract_2km_buffer_streetnet_sf.utm_prelim <- 
#   sf::st_intersection(one_tract_2km_buffer_sf.utm,streetnet_sf.utm)
# 
# one_tract_2km_buffer_streetnet_sf.utm <- 
#   streetnet_sf.utm %>% 
#   filter(osm_id %in% one_tract_2km_buffer_streetnet_sf.utm_prelim$osm_id)

###############################################
###############################################
# get "from" points in this census tract - i.e. residences

one_tract_pts <- 
  get_regularly_spaced_points(one_tract_sf.utm, 
                              crs_lonlat=crs_lonlat, 
                              crs_utm=crs_utm, 
                              dist_between_pnts=100
                              )

one_tract_pts_sf.utm <- one_tract_pts$pts_sf.utm
one_tract_pts_array.lonlat <- one_tract_pts$pts_array.lonlat

# snap the from points to closest point on street network - 
# from points are points generated at regularly spaced intervals within the census tract polygon
# so they may not be on the street network - snapping to street network will make distance calculation possible/more accurate
from_streetnet_vertices_idxs <- dodgr::match_pts_to_verts(streetnet_vertices, one_tract_pts_array.lonlat, connected = TRUE)

from_streetnet_vertices_df <- streetnet_vertices[from_streetnet_vertices_idxs,]

from_streetnet_vertices_sf.lonlat <- 
  from_streetnet_vertices_df %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(crs_lonlat) 

from_streetnet_vertices_sf.utm <- 
  from_streetnet_vertices_sf.lonlat %>% 
  sf::st_transform(crs_utm)

from_streetnet_vertices_ids <- streetnet_vertices$id [from_streetnet_vertices_idxs] 

###############################################
# get "to" points in this census tract - i.e. grocery stores

# get grocery stores within 2km of census tract
grocery_one_tract_2km_buffer_sf.utm <- 
  grocery_sf.utm %>%
  sf::st_filter(one_tract_sf.utm, 
                .predicate = sf::st_is_within_distance,
                dist = 2000)

grocery_one_tract_2km_buffer_sf.lonlat <- 
  sf::st_transform(grocery_one_tract_2km_buffer_sf.utm,
                   crs = crs_lonlat) 

grocery_one_tract_2km_buffer_array.lonlat <-
  sf::st_coordinates(grocery_one_tract_2km_buffer_sf.lonlat)

# snap the to points to closest point on street network - grocery stores should already be on street network so this shouldn't change much
to_streetnet_vertices_idxs <- dodgr::match_pts_to_verts(streetnet_vertices, grocery_one_tract_2km_buffer_array.lonlat, connected = TRUE)

to_streetnet_vertices_df <- streetnet_vertices[to_streetnet_vertices_idxs,]

to_streetnet_vertices_sf.lonlat <- 
  to_streetnet_vertices_df %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(crs_lonlat) 

to_streetnet_vertices_sf.utm <- 
  to_streetnet_vertices_sf.lonlat %>% 
  sf::st_transform(crs_utm)

to_streetnet_vertices_ids <- streetnet_vertices$id [to_streetnet_vertices_idxs] 
###############################################
# get shortest distance between every from and to point

# distances_df has one row per from point and one col per to point
distances <- dodgr::dodgr_dists (graph = streetnet_graph, from = from_streetnet_vertices_ids, to = to_streetnet_vertices_ids)
distances_df <- data.frame(distances)

###############################################
# process distances df

process_distances <- 
  process_distances_df(
    distances_df = distances_df, 
    from_streetnet_vertices_ids = from_streetnet_vertices_ids, 
    to_streetnet_vertices_ids = to_streetnet_vertices_ids
    )

# if (nrow(distances_df) == 1){
#   
#   distances_df$min_dist <- min(distances,na.rm = TRUE)
#   
# } else if(nrow(distances_df) > 1){
#   
#   distances_df$min_dist <- apply(distances,1,function(x) min(x, na.rm = TRUE))
#   
# }
# 
# 
# # get the index of any 'from' points that have no path to any grocery store
# distances_df.no_path.index <- which(distances_df$min_dist == Inf)
# distances_df.yes_path.index <- which(distances_df$min_dist != Inf)
# 
# distances_df.no_path <- distances_df %>% filter(min_dist == Inf)
# 
# # filter out any 'from' points that have no path to any grocery store
# distances_df <- distances_df %>% filter(min_dist != Inf)
# distances <- distances[distances_df.yes_path.index,]
# 
# if (nrow(distances_df) == 1){
#   
#   distances_df$min_dist_index <- which(distances == min(distances, na.rm = TRUE))
#   
# } else if (nrow(distances_df) > 1){
#   
#   distances_df$min_dist_index <- apply(distances,1,function(x) which(x==min(x, na.rm = TRUE))[1])
#   
# }
# 
# distances_df$min_dist_v_id <- to_streetnet_vertices_ids[distances_df$min_dist_index]
# distances_df$from_v_id <- from_streetnet_vertices_ids[distances_df.yes_path.index]
# 
# ever_closest_grocery <- unique(distances_df$min_dist_v_id)
# ever_closest_grocery_df <- streetnet_vertices %>% filter(id %in% ever_closest_grocery)
# 
# ever_closest_grocery_sf.lonlat <- 
#   ever_closest_grocery_df %>% 
#   sf::st_as_sf(coords = c("x","y")) %>% 
#   sf::st_set_crs(crs_lonlat) 
# 
# ever_closest_grocery_sf.utm <- 
#   ever_closest_grocery_sf.lonlat %>% 
#   sf::st_transform(crs_utm)
# 
# 
# mean_dist_to_grocery <- mean(distances_df$min_dist)
# median_dist_to_grocery <- median(distances_df$min_dist)
# max_dist_to_grocery <- max(distances_df$min_dist)
# min_dist_to_grocery <- min(distances_df$min_dist)
# quartiles_dist_to_grocery_25p <- quantile(distances_df$min_dist)[2]
# quartiles_dist_to_grocery_50p <- quantile(distances_df$min_dist)[3]
# quartiles_dist_to_grocery_75p <- quantile(distances_df$min_dist)[4]
# n_from_pnts <- nrow(distances_df)
# n_from_pnts_no_path <- nrow(distances_df.no_path)
# 
# distances_df$max_shortest_path <- ifelse(distances_df$min_dist == max_dist_to_grocery,1,0)
# distances_df$min_shortest_path <- ifelse(distances_df$min_dist == min_dist_to_grocery,1,0)
# 
# distances_df$diff_from_mean_shortest_path <- abs(distances_df$min_dist - mean_dist_to_grocery)
# distances_df$example_mean_shortest_path <- ifelse(distances_df$diff_from_mean_shortest_path == min(distances_df$diff_from_mean_shortest_path),1,0)
# 
# distances_df$diff_from_median_shortest_path <- abs(distances_df$min_dist - median_dist_to_grocery)
# distances_df$example_median_shortest_path <- ifelse(distances_df$diff_from_median_shortest_path == min(distances_df$diff_from_median_shortest_path),1,0)
# 
# summary_dist_to_grocery <- 
#   data.frame(i,
#              mean_dist_to_grocery,
#              median_dist_to_grocery, 
#              max_dist_to_grocery,
#              min_dist_to_grocery, 
#              quartiles_dist_to_grocery_25p,
#              quartiles_dist_to_grocery_50p,
#              quartiles_dist_to_grocery_75p,
#              n_from_pnts,
#              n_from_pnts_no_path)
# 
# row.names(summary_dist_to_grocery) <- NULL



















summary_dist_to_grocery.list <- list()
d.df.list <- list()


# start_tract_id <- 732
# end_tract_id <- nrow(chicago.city.tracts.utm)

start_tract_id <- 1
end_tract_id <- 100

for (i in start_tract_id:end_tract_id){
  
  print(paste0("tract id ",i))
  
  # i <- 380  
  # i <- 138
  one_tract.utm <- chicago.city.tracts.utm[i,]
  filename_safe_tract_name <- gsub(" ","_",gsub(", |\\.", " ", one_tract.utm$Name))
  
  print(one_tract.utm$Name)
  print(filename_safe_tract_name)
    
  source("C:/Users/Andrea/Desktop/Real Feel Distance/loop_001_map_grocery_per_census_tract.R")
  source("C:/Users/Andrea/Desktop/Real Feel Distance/loop_002_generate_regularly_spaced_points.R")
  source("C:/Users/Andrea/Desktop/Real Feel Distance/loop_003_map_regularly_spaced_pnts.R")
  source("C:/Users/Andrea/Desktop/Real Feel Distance/loop_004_calculate_distance.R")
  
  summary_dist_to_grocery.list[[i]] <- summary_dist_to_grocery
  d.df.list[[i]] <- d.df
  
  source("C:/Users/Andrea/Desktop/Real Feel Distance/loop_005_get_example_closest_paths.R")
  
  print("done with example paths")
  
  # this will take a while - save every once in a while so don't lose work
  if (i%%100 == 0){
    
    print(paste0("saving workspace at i=",i))
    
    summary_dist_to_grocery.collect_lists.df <- 
      data.table::rbindlist(summary_dist_to_grocery.list,
                            use.names = TRUE,
                            fill = TRUE)
    new_objects_list <- ls()[!(ls() %in% orig_objects_list)]
    
    save(list = c("summary_dist_to_grocery.collect_lists.df"),
         file = paste0("C:/Users/Andrea/Desktop/Real Feel Distance/save_i_",
                      start_tract_id,
                      "to_",
                      i,
                      "_small.RData"))
    
    
    
  }
  
  
  
}

summary_dist_to_grocery.collect_lists.df <- 
  data.table::rbindlist(summary_dist_to_grocery.list, 
                        use.names = TRUE,
                        fill = TRUE)




#################################################################
# save for this step

# save(
#   file = output_path
# )
save(list = c("summary_dist_to_grocery.collect_lists.df"),
     file = paste0("C:/Users/Andrea/Desktop/Real Feel Distance/save_i_",
                   start_tract_id,
                   "_to_",
                   i,
                   "_small_final.RData"))

save(list = c(small_save_list_basic),
     file = paste0("C:/Users/Andrea/Desktop/Real Feel Distance/save_i_",
                   start_tract_id,
                   "_to_",
                   i,
                   "_final.RData"))
#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################


