load("C:/Users/Andrea/Desktop/Real Feel Distance/save_i_732_to_804_final.RData")
load("C:/Users/Andrea/Desktop/Real Feel Distance/thru_get_streetnet_3.RData")

# # get the shortest path between all from/to points
# dp <- dodgr::dodgr_paths (graph = graph, from = from, to = to)

d.df <- d.df.list[[100]]

# get the shortest path between all from points and their single closest to point (closest grocery store)
dp <- dodgr::dodgr_paths (graph = graph, from = d.df$from_v_id, to = d.df$min_dist_v_id,pairwise = TRUE)

# get_paths <- 
#   c(which(d.df$min_shortest_path == 1)[1],
#     which(d.df$max_shortest_path == 1)[1],
#     which(d.df$example_mean_shortest_path == 1)[1],
#     which(d.df$example_median_shortest_path == 1)[1])
# 
# #get shortest path between just the from/to points that represent the min dist, max dist, mean/median dist
# dp <- dodgr::dodgr_paths (graph = graph, from = d.df$from_v_id[get_paths], to = d.df$min_dist_v_id[get_paths],pairwise = TRUE)

example_paths <- list()
example_paths.sf.utm <- list()
#k <- 0

for (j in 1:length(get_paths)){
  
  #k <- k + 1
  
  path_v <- v[match (dp [[j]] [[1]], v$id), ]
  path_v$path_order <- seq(1,nrow(path_v),1)
  
  path_v.lines <- points_to_line(data = path_v, 
                                 long = "x", 
                                 lat = "y", 
                                 sort_field = "path_order",
                                 id_value = paste0("path ",j))
  
  path_v.lines.sf.utm <-
    path_v.lines %>%
    sf::st_as_sf() %>%
    sf::st_set_crs("+proj=longlat +datum=NAD83") %>%
    sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")
  
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
  
  #example_paths[[k]] <- path_v.connected.sf.utm
  example_paths[[j]] <- path_v.lines
  example_paths.sf.utm[[j]] <- path_v.lines.sf.utm
  
}

## Using tmap package for points with labels (sp object)
myTmap <- 
  tmap::tm_shape(example_paths[[1]])  + 
  tmap::tm_lines(col = "red",lwd=2,lty="dashed") +
  tmap::tm_shape(example_paths[[2]])  + 
  tmap::tm_lines(col = "green",lwd=2)
tmap::tm_shape(example_paths[[3]])  + 
  tmap::tm_lines(col = "blue",lwd=2)

tmap::tm_shape(example_paths.sf.utm[[1]])  + 
  tmap::tm_lines(lwd=2, col="red", alpha =0.2) + 
  tmap::tm_shape(example_paths.sf.utm[[2]])  + 
  tmap::tm_lines(lwd=2, col="lightblue", alpha = 0.2)
tmap::tm_shape(example_paths.sf.utm[[3]])  + 
  tmap::tm_lines(lwd=2, col="green", alpha =0.5) + 
  tmap::tm_shape(example_paths.sf.utm[[4]])  + 
  tmap::tm_lines(lwd=2, col="blue", alpha = 0.2)

print(myTmap)

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

##############################

snap_from_points_to_streets_zoom_even_more <-
  tmap::tm_shape(chicago.city.utm, bbox = zoom_even_more_bbox) +
  tmap::tm_polygons() +
  tmap::tm_shape(dat_sf.utm, bbox = zoom_even_more_bbox) +
  tmap::tm_lines() +
  tmap::tm_shape(one_tract.utm, bbox = zoom_even_more_bbox) +
  tmap::tm_borders(lwd = 2, col = "red") +
  #tmap::tm_polygons() +
  # tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
  # tmap::tm_dots(col="red", size = 1) +
  tmap::tm_shape(grocery_one_tract.buffer.sf.utm, bbox = zoom_even_more_bbox) +
  tmap::tm_dots(col="yellow", size = 1) +
  tmap::tm_shape(pts_lonlat.sf.utm, bbox = zoom_even_more_bbox) +
  tmap::tm_dots(col="white", size = .08) +
  tmap::tm_shape(from_v.sf.utm, bbox = zoom_even_more_bbox) +
  tmap::tm_dots(col="blue", size = .08)

snap_from_points_to_streets_zoom_even_more

# https://stackoverflow.com/questions/48400758/add-lines-between-labelled-location-points
# https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#color-palette
# http://www.wvview.org/spatial_analytics/Visualizing_Spatial_Data/_site/Visualize_Spatial_Data.html#content

example_paths_plot <-
  tmap::tm_shape(chicago.city.utm, bbox = zoom_more_bbox) +
  tmap::tm_polygons() +
  tmap::tm_shape(dat_sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_lines() +
  tmap::tm_shape(one_tract.utm, bbox = zoom_more_bbox) +
  tmap::tm_borders(lwd = 2, col = "red") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="red", size = 1) +
  tmap::tm_shape(ever_closest_grocery_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="blue", size = .08) +
  tmap::tm_shape(from_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="blue", size = .08) +
  tmap::tm_shape(example_paths.sf.utm[[2]], bbox = zoom_more_bbox) +
  tmap::tm_lines(lty = "dashed", col = "lightblue", lwd = 2, alpha = .5) +
  #tmap::tm_shape(example_paths.sf.utm[[4]], bbox = zoom_more_bbox) +
  #tmap::tm_lines(lty = "dashed", col = "purple", lwd = 2, alpha = .2)
  
  example_paths_plot



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







