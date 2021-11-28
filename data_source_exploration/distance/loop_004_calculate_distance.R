# https://github.com/ATFutures/dodgr
# http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# https://rspatialdata.github.io/osm.html
# https://stackoverflow.com/questions/52248394/get-census-tract-from-lat-lon-using-tigris
# https://stackoverflow.com/questions/29872109/binning-longitude-latitude-labeled-data-by-census-block-id
# https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-open-shapefiles-r
# https://geocompr.robinlovelace.net/adv-map.html

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
# dat_sf <- dodgr::dodgr_streetnet("chicago")
# graph <- dodgr::weight_streetnet (dat_sf, wt_profile = "foot")
# 
# v <- dodgr::dodgr_vertices (graph)
# head (v)

# from <- sample (v$id, size = 20)
# to <- sample (v$id, size = 50)
# d <- dodgr::dodgr_dists (graph = graph, from = from, to = to)
# dim (d)
# 
# 
# from_x <- min (graph$from_lon) + runif (20) * diff (range (graph$from_lon))
# from_y <- min (graph$from_lat) + runif (20) * diff (range (graph$from_lat))
# to_x <- min (graph$from_lon) + runif (50) * diff (range (graph$from_lon))
# to_y <- min (graph$from_lat) + runif (50) * diff (range (graph$from_lat))
# 
# from <- cbind (from_x, from_y)
# to <- cbind (to_x, to_y)
# 
# from_df <- data.frame(from)
# to_df <- data.frame(to)
# 
# d <- dodgr::dodgr_dists (graph = graph, from = from, to = to)

########################################################################
########################################################################


# snap the from points to closest point on street network - 
# from points are points generated at regularly spaced intervals within the census tract polygon
# so they may not be on the street network - snapping to street network will make distance calculation possible/more accurate
from <- dodgr::match_points_to_graph (v, pts_lonlat.array, connected = TRUE)

# convert crs from utm back to lonlat
sf::st_crs(grocery_one_tract.buffer.sf.utm)$proj4string

grocery_one_tract.buffer.sf.lonlat <- 
  sf::st_transform(grocery_one_tract.buffer.sf.utm,
                   crs = "+proj=longlat +datum=NAD83 +ellps=GRS80") 

sf::st_crs(grocery_one_tract.buffer.sf.lonlat)$proj4string

grocery_one_tract.buffer.array.lonlat <-
  sf::st_coordinates(grocery_one_tract.buffer.sf.lonlat)

# snap the to points to closest point on street network - grocery stores should already be on street network so this shouldn't change much
to <- dodgr::match_points_to_graph (v, grocery_one_tract.buffer.array.lonlat, connected = TRUE)


from_v <- v[from,]
from_v.sf.utm <- 
  from_v %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs("+proj=longlat +datum=NAD83") %>% 
  sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")

from <- v$id [from] # or from <- v [from, c ("x", "y")]

to_v <- v[to,]
to_v.sf.utm <- 
  to_v %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs("+proj=longlat +datum=NAD83") %>% 
  sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")

to <- v$id [to]

d <- dodgr::dodgr_dists (graph = graph, from = from, to = to)

d.df <- data.frame(d)

if (nrow(d.df) == 1){
  
  d.df$min_dist <- min(d,na.rm = TRUE)
  
} else if(nrow(d.df) > 1){
  
  d.df$min_dist <- apply(d,1,function(x) min(x, na.rm = TRUE))
  
}


# get the index of any 'from' points that have no path to any grocery store
d.df.no_path.index <- which(d.df$min_dist == Inf)
d.df.yes_path.index <- which(d.df$min_dist != Inf)

d.df.no_path <- d.df %>% filter(min_dist == Inf)

# filter out any 'from' points that have no path to any grocery store
d.df <- d.df %>% filter(min_dist != Inf)
d <- d[d.df.yes_path.index,]

if (nrow(d.df) == 1){
  
  d.df$min_dist_index <- which(d == min(d, na.rm = TRUE))
  
} else if (nrow(d.df) > 1){
  
  d.df$min_dist_index <- apply(d,1,function(x) which(x==min(x, na.rm = TRUE))[1])
  
}

d.df$min_dist_v_id <- to[d.df$min_dist_index]
d.df$from_v_id <- from[d.df.yes_path.index]

ever_closest_grocery <- unique(d.df$min_dist_v_id)
ever_closest_grocery_v <- v %>% filter(v$id %in% ever_closest_grocery)
ever_closest_grocery_v.sf.utm <- 
  ever_closest_grocery_v %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs("+proj=longlat +datum=NAD83") %>% 
  sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")


mean_dist_to_grocery <- mean(d.df$min_dist)
median_dist_to_grocery <- median(d.df$min_dist)
max_dist_to_grocery <- max(d.df$min_dist)
min_dist_to_grocery <- min(d.df$min_dist)
quartiles_dist_to_grocery_25p <- quantile(d.df$min_dist)[2]
quartiles_dist_to_grocery_50p <- quantile(d.df$min_dist)[3]
quartiles_dist_to_grocery_75p <- quantile(d.df$min_dist)[4]
n_from_pnts <- nrow(from_v.sf.utm)
n_from_pnts_no_path <- nrow(d.df.no_path)

d.df$max_shortest_path <- ifelse(d.df$min_dist == max_dist_to_grocery,1,0)
d.df$min_shortest_path <- ifelse(d.df$min_dist == min_dist_to_grocery,1,0)

d.df$diff_from_mean_shortest_path <- abs(d.df$min_dist - mean_dist_to_grocery)
d.df$example_mean_shortest_path <- ifelse(d.df$diff_from_mean_shortest_path == min(d.df$diff_from_mean_shortest_path),1,0)

d.df$diff_from_median_shortest_path <- abs(d.df$min_dist - median_dist_to_grocery)
d.df$example_median_shortest_path <- ifelse(d.df$diff_from_median_shortest_path == min(d.df$diff_from_median_shortest_path),1,0)

summary_dist_to_grocery <- 
  data.frame(i,
             mean_dist_to_grocery,
             median_dist_to_grocery, 
             max_dist_to_grocery,
             min_dist_to_grocery, 
             quartiles_dist_to_grocery_25p,
             quartiles_dist_to_grocery_50p,
             quartiles_dist_to_grocery_75p,
             n_from_pnts,
             n_from_pnts_no_path)

row.names(summary_dist_to_grocery) <- NULL

# # get the shortest path between all from/to points
# dp <- dodgr::dodgr_paths (graph = graph, from = from, to = to)

# get the shortest path between all from points and their single closest to point (closest grocery store)
#dp <- dodgr::dodgr_paths (graph = graph, from = d.df$from_v_id, to = d.df$min_dist_v_id,pairwise = TRUE)

# get_paths <- 
#   c(which(d.df$min_shortest_path == 1)[1],
#     which(d.df$max_shortest_path == 1)[1],
#     which(d.df$example_mean_shortest_path == 1)[1],
#     which(d.df$example_median_shortest_path == 1)[1])
# 
# #get shortest path between just the from/to points that represent the min dist, max dist, mean/median dist
# dp <- dodgr::dodgr_paths (graph = graph, from = d.df$from_v_id[get_paths], to = d.df$min_dist_v_id[get_paths],pairwise = TRUE)
# 
# example_paths <- list()
# example_paths.sf.utm <- list()
# #k <- 0
# 
# for (j in 1:length(get_paths)){
#   
#   #k <- k + 1
#   
#   path_v <- v[match (dp [[j]] [[1]], v$id), ]
#   path_v$path_order <- seq(1,nrow(path_v),1)
#   
#   path_v.lines <- points_to_line(data = path_v, 
#                              long = "x", 
#                              lat = "y", 
#                              sort_field = "path_order",
#                              id_value = paste0("path ",j))
#   
#   path_v.lines.sf.utm <-
#     path_v.lines %>%
#     sf::st_as_sf() %>%
#     sf::st_set_crs("+proj=longlat +datum=NAD83") %>%
#     sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")
#   
#   # path_v.sf.utm <- 
#   #   path_v %>% 
#   #   sf::st_as_sf(coords = c("x","y")) %>% 
#   #   sf::st_set_crs("+proj=longlat +datum=NAD83") %>% 
#   #   sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")
#   # 
#   # path_v.connected.sf.utm <- 
#   #   path_v.sf.utm %>% 
#   #   sf::st_coordinates() %>% 
#   #   sf::st_linestring()
#   
#   #example_paths[[k]] <- path_v.connected.sf.utm
#   example_paths[[j]] <- path_v.lines
#   example_paths.sf.utm[[j]] <- path_v.lines.sf.utm
#   
# }

## Using tmap package for points with labels (sp object)
# myTmap <- 
#   tmap::tm_shape(example_paths[[1]])  + 
#   tmap::tm_lines(col = "red",lwd=2,lty="dashed") +
#   tmap::tm_shape(example_paths[[2]])  + 
#   tmap::tm_lines(col = "green",lwd=2)
#   tmap::tm_shape(example_paths[[3]])  + 
#   tmap::tm_lines(col = "blue",lwd=2)
#   
#   tmap::tm_shape(example_paths.sf.utm[[1]])  + 
#   tmap::tm_lines(lwd=2, col="red", alpha =0.2) + 
#   tmap::tm_shape(example_paths.sf.utm[[2]])  + 
#   tmap::tm_lines(lwd=2, col="lightblue", alpha = 0.2)
#   tmap::tm_shape(example_paths.sf.utm[[3]])  + 
#   tmap::tm_lines(lwd=2, col="green", alpha =0.5) + 
#   tmap::tm_shape(example_paths.sf.utm[[4]])  + 
#   tmap::tm_lines(lwd=2, col="blue", alpha = 0.2)
# 
# print(myTmap)

# path_v <- v[match (dp [[67]] [[1]], v$id), ]
# head (path_v)
# 
# path_v.sf.utm <- 
#   path_v %>% 
#   sf::st_as_sf(coords = c("x","y")) %>% 
#   sf::st_set_crs("+proj=longlat +datum=NAD83") %>% 
#   sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")
# 
# path_v.connected.sf.utm <- 
#   path_v.sf.utm %>% 
#   sf::st_coordinates() %>% 
#   sf::st_linestring()
# 
# # https://github.com/r-spatial/sf/issues/321
# l <- pts_sf %>% st_coordinates() %>% st_linestring()



# keep d.df for each census tract (including rowMin) when looping through

# ##############################
# 
# snap_from_points_to_streets_zoom_even_more <-
#   tmap::tm_shape(chicago.city.utm, bbox = zoom_even_more_bbox) +
#   tmap::tm_polygons() +
#   tmap::tm_shape(dat_sf.utm, bbox = zoom_even_more_bbox) +
#   tmap::tm_lines() +
#   tmap::tm_shape(one_tract.utm, bbox = zoom_even_more_bbox) +
#   tmap::tm_borders(lwd = 2, col = "red") +
#   #tmap::tm_polygons() +
#   # tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
#   # tmap::tm_dots(col="red", size = 1) +
#   tmap::tm_shape(grocery_one_tract.buffer.sf.utm, bbox = zoom_even_more_bbox) +
#   tmap::tm_dots(col="yellow", size = 1) +
#   tmap::tm_shape(pts_lonlat.sf.utm, bbox = zoom_even_more_bbox) +
#   tmap::tm_dots(col="white", size = .08) +
#   tmap::tm_shape(from_v.sf.utm, bbox = zoom_even_more_bbox) +
#   tmap::tm_dots(col="blue", size = .08)
# 
# snap_from_points_to_streets_zoom_even_more

# https://stackoverflow.com/questions/48400758/add-lines-between-labelled-location-points
# https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#color-palette
# http://www.wvview.org/spatial_analytics/Visualizing_Spatial_Data/_site/Visualize_Spatial_Data.html#content

# example_paths_plot <-
# tmap::tm_shape(chicago.city.utm, bbox = zoom_more_bbox) +
#   tmap::tm_polygons() +
#   tmap::tm_shape(dat_sf.utm, bbox = zoom_more_bbox) +
#   tmap::tm_lines() +
#   tmap::tm_shape(one_tract.utm, bbox = zoom_more_bbox) +
#   tmap::tm_borders(lwd = 2, col = "red") +
#   #tmap::tm_polygons() +
#   tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
#   tmap::tm_dots(col="red", size = 1) +
#   tmap::tm_shape(ever_closest_grocery_v.sf.utm, bbox = zoom_more_bbox) +
#   tmap::tm_dots(col="blue", size = .08) +
#   tmap::tm_shape(from_v.sf.utm, bbox = zoom_more_bbox) +
#   tmap::tm_dots(col="blue", size = .08) +
#   tmap::tm_shape(example_paths.sf.utm[[2]], bbox = zoom_more_bbox) +
#   tmap::tm_lines(lty = "dashed", col = "lightblue", lwd = 2, alpha = .5) +
#   #tmap::tm_shape(example_paths.sf.utm[[4]], bbox = zoom_more_bbox) +
#   #tmap::tm_lines(lty = "dashed", col = "purple", lwd = 2, alpha = .2)
# 
# example_paths_plot



##############################


# library(ggmap)
# library(ggplot2)
# 
# 
# # retrieving map of chicago
# chicago_map <- ggmap::get_map(osmdata::getbb("Chicago Illinois United States"), maptype = "roadmap")
# 
# 
# ggmap(chicago_map) + 
#   geom_point(data=from_df, 
#              aes(x=from_x, y=from_y), 
#              size = 2, 
#              alpha=.8, 
#              fill="firebrick4", 
#              color="white", 
#              pch=21, 
#              inherit.aes = F) + 
#   geom_point(data=to_df, 
#              aes(x=to_x, y=to_y), 
#              size = 2, 
#              alpha=.8, 
#              fill="blue", 
#              color="white", 
#              pch=21, 
#              inherit.aes = F)







