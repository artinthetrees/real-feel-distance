library(tidyverse)


pts_to_streetnet_vertices_ids <- function(streetnet_vertices,pts_array.lonlat){
  
  my_streetnet_vertices_idxs <- dodgr::match_pts_to_verts(streetnet_vertices, pts_array.lonlat, connected = TRUE)
  
  my_streetnet_vertices_df <- streetnet_vertices[my_streetnet_vertices_idxs,]
  
  my_streetnet_vertices_ids <- streetnet_vertices$id [my_streetnet_vertices_idxs]
  
  return(list(
    my_streetnet_vertices_idxs=my_streetnet_vertices_idxs,
    my_streetnet_vertices_df=my_streetnet_vertices_df,
    my_streetnet_vertices_ids=my_streetnet_vertices_ids
    ))
  
}


process_distances_df <- function(distances, distances_df, from_streetnet_vertices_ids, to_streetnet_vertices_ids){
  
  if (nrow(distances_df) == 1){
    
    distances_df$min_dist <- min(distances,na.rm = TRUE)
    
  } else if(nrow(distances_df) > 1){
    
    distances_df$min_dist <- apply(distances,1,function(x) min(x, na.rm = TRUE))
    
  }
  
  # get the index of any 'from' points that have no path to any grocery store
  distances_df.no_path.index <- which(distances_df$min_dist == Inf)
  distances_df.yes_path.index <- which(distances_df$min_dist != Inf)
  
  distances_df.no_path <- distances_df %>% filter(min_dist == Inf)
  
  # filter out any 'from' points that have no path to any grocery store
  distances_df <- distances_df %>% filter(min_dist != Inf)
  distances <- distances[distances_df.yes_path.index,]
  
  if (nrow(distances_df) == 1){
    
    distances_df$min_dist_index <- which(distances == min(distances, na.rm = TRUE))
    
  } else if (nrow(distances_df) > 1){
    
    distances_df$min_dist_index <- apply(distances,1,function(x) which(x==min(x, na.rm = TRUE))[1])
    
  }
  
  distances_df$min_dist_v_id <- to_streetnet_vertices_ids[distances_df$min_dist_index]
  distances_df$from_v_id <- from_streetnet_vertices_ids[distances_df.yes_path.index]
  
  mean_dist_to_grocery <- mean(distances_df$min_dist)
  median_dist_to_grocery <- median(distances_df$min_dist)
  max_dist_to_grocery <- max(distances_df$min_dist)
  min_dist_to_grocery <- min(distances_df$min_dist)
  quartiles_dist_to_grocery_25p <- quantile(distances_df$min_dist)[2]
  quartiles_dist_to_grocery_50p <- quantile(distances_df$min_dist)[3]
  quartiles_dist_to_grocery_75p <- quantile(distances_df$min_dist)[4]
  n_from_pnts <- nrow(distances_df)
  n_from_pnts_no_path <- nrow(distances_df.no_path)
  
  distances_df$max_shortest_path <- ifelse(distances_df$min_dist == max_dist_to_grocery,1,0)
  distances_df$min_shortest_path <- ifelse(distances_df$min_dist == min_dist_to_grocery,1,0)
  
  distances_df$diff_from_mean_shortest_path <- abs(distances_df$min_dist - mean_dist_to_grocery)
  distances_df$example_mean_shortest_path <- ifelse(distances_df$diff_from_mean_shortest_path == min(distances_df$diff_from_mean_shortest_path),1,0)
  
  distances_df$diff_from_median_shortest_path <- abs(distances_df$min_dist - median_dist_to_grocery)
  distances_df$example_median_shortest_path <- ifelse(distances_df$diff_from_median_shortest_path == min(distances_df$diff_from_median_shortest_path),1,0)
  
  summary_distances_df <- 
    data.frame(mean_dist_to_grocery,
               median_dist_to_grocery, 
               max_dist_to_grocery,
               min_dist_to_grocery, 
               quartiles_dist_to_grocery_25p,
               quartiles_dist_to_grocery_50p,
               quartiles_dist_to_grocery_75p,
               n_from_pnts,
               n_from_pnts_no_path)
  
  row.names(summary_distances_df) <- NULL
  
  return(list(distances_df=distances_df,summary_distances_df=summary_distances_df))
  
}

get_distances_df <- function(one_tract_sf.utm, grocery_sf.utm, streetnet_vertices, streetnet_graph, crs_lonlat, crs_utm){
  
  tract_name <- one_tract_sf.utm$NAMELSAD
  tract_geoid <- one_tract_sf.utm$GEOID 
  
  print(tract_name)
  print(tract_geoid)
  
  one_tract_2km_buffer_sf.utm <- 
    sf::st_buffer(one_tract_sf.utm,dist=2000)
  
  ###############################################
  ###############################################
  # get "from" points in this census tract - i.e. residences
  
  one_tract_pts <- 
    get_regularly_spaced_points(boundary_map_in_utm = one_tract_sf.utm, 
                                crs_lonlat=crs_lonlat, 
                                crs_utm=crs_utm, 
                                dist_between_pnts=100)
  
  #one_tract_pts_sf.utm <- one_tract_pts$pts_sf.utm
  one_tract_pts_array.lonlat <- one_tract_pts$pts_array.lonlat
  
  # snap the from points to closest point on street network - 
  # from points are points generated at regularly spaced intervals within the census tract polygon
  # so they may not be on the street network - snapping to street network will make distance calculation possible/more accurate
  from_pnts <- pts_to_streetnet_vertices_ids(streetnet_vertices = streetnet_vertices, pts_array.lonlat = one_tract_pts_array.lonlat)
  from_streetnet_vertices_ids <- from_pnts$my_streetnet_vertices_ids
  
  ###############################################
  # get "to" points in this census tract - i.e. grocery stores
  
  # get grocery stores within 2km of census tract
  grocery_one_tract_2km_buffer_sf.utm <- 
    grocery_sf.utm %>%
    sf::st_filter(one_tract_sf.utm, 
                  .predicate = sf::st_is_within_distance,
                  dist = 2000)
  
  if (nrow(grocery_one_tract_2km_buffer_sf.utm) == 0){
    print("no grocery stores within 2km of tract; will not calculate distances; recording tract and exiting")
    return(list(no_distances=TRUE,tract_name=tract_name,tract_geoid=tract_geoid))
  }
  
  grocery_one_tract_2km_buffer_sf.lonlat <- 
    sf::st_transform(grocery_one_tract_2km_buffer_sf.utm,
                     crs = crs_lonlat) 
  
  grocery_one_tract_2km_buffer_array.lonlat <-
    sf::st_coordinates(grocery_one_tract_2km_buffer_sf.lonlat)
  
  # snap the to points to closest point on street network - grocery stores should already be on street network so this shouldn't change much
  to_pnts <- pts_to_streetnet_vertices_ids(streetnet_vertices = streetnet_vertices, pts_array.lonlat = grocery_one_tract_2km_buffer_array.lonlat)
  to_streetnet_vertices_ids <- to_pnts$my_streetnet_vertices_ids
  
  ###############################################
  # get shortest distance between every from and to point
  
  # distances_df has one row per from point and one col per to point
  distances <- dodgr::dodgr_dists (graph = streetnet_graph, from = from_streetnet_vertices_ids, to = to_streetnet_vertices_ids)
  distances_df <- data.frame(distances)
  
  ###############################################
  # process distances df
  
  process_distances <- 
    process_distances_df(
      distances = distances,
      distances_df = distances_df, 
      from_streetnet_vertices_ids = from_streetnet_vertices_ids, 
      to_streetnet_vertices_ids = to_streetnet_vertices_ids
    )
  
  distances_df <- process_distances$distances_df
  distances_df$tract_name <- tract_name
  distances_df$tract_geoid <- tract_geoid
  
  summary_distances_df <- process_distances$summary_distances_df
  summary_distances_df$tract_name <- tract_name
  summary_distances_df$tract_geoid <- tract_geoid
  
  return(list(
    no_distances=FALSE,
    distances_df=distances_df,
    summary_distances_df=summary_distances_df
    ))
  
  
}


















