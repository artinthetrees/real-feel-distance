## required per census tract: d.df
## required overall: graph, v, crs_lonlat, crs_utm


# # get the shortest path between all from/to points
# dp <- dodgr::dodgr_paths (graph = graph, from = from, to = to)

# get the shortest path between all from points and their single closest to point (closest grocery store)
#dp <- dodgr::dodgr_paths (graph = graph, from = d.df$from_v_id, to = d.df$min_dist_v_id,pairwise = TRUE)

get_paths <- 
  c(which(d.df$min_shortest_path == 1)[1],
    which(d.df$max_shortest_path == 1)[1],
    which(d.df$example_mean_shortest_path == 1)[1],
    which(d.df$example_median_shortest_path == 1)[1])

get_paths_label <- 
  c("min shortest path",
    "max shortest path",
    "mean shortest path",
    "median shortest path")

#get shortest path between just the from/to points that represent the min , max , mean/median shortest path dists
dp <- dodgr::dodgr_paths (graph = graph, from = d.df$from_v_id[get_paths], to = d.df$min_dist_v_id[get_paths],pairwise = TRUE)

example_paths <- list()
example_paths.sf.utm <- list()
#k <- 0

print(paste0("getting ",
             length(get_paths),
             " example paths"))

for (j in 1:length(get_paths)){
  
  #k <- k + 1
  print(paste0("path ",j))
  
  path_v <- v[match (dp [[j]] [[1]], v$id), ]
  path_v$path_order <- seq(1,nrow(path_v),1)
  
  path_v.lines <- points_to_line(data = path_v, 
                                 long = "x", 
                                 lat = "y", 
                                 sort_field = "path_order",
                                 id_value = get_paths_label[j])
  
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
  
  if (j == 1){
    example_paths.bind <- 
      path_v.lines
  } else if (j > 1){
    example_paths.bind <- 
      maptools::spRbind(example_paths.bind,path_v.lines)
  }
  
}


# https://stackoverflow.com/questions/33151556/creating-spatiallinesdataframe-from-spatiallines-object-and-basic-df?rq=1
# http://www.wvview.org/spatial_analytics/Visualizing_Spatial_Data/_site/Visualize_Spatial_Data.html

get_paths_label.df <- data.frame(get_paths_label)
row.names(get_paths_label.df) <- get_paths_label.df$get_paths_label
names(get_paths_label.df) <- "path_label"

#sp::proj4string(example_paths.bind) <- sp::CRS("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")
example_paths.bind.spdf <- sp::SpatialLinesDataFrame(example_paths.bind, data = get_paths_label.df)

example_paths.bind.sf.utm <- 
  example_paths.bind.spdf %>% 
  sf::st_as_sf() %>% 
  sf::st_set_crs("+proj=longlat +datum=NAD83") %>% 
  sf::st_transform("+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80")

tmap::tm_shape(example_paths.bind.spdf)  + 
  tmap::tm_lines(lty = "dashed", lwd=2, col="path_label",style = "cat", palette = c("red","blue","lightblue","green"), alpha =0.5) 

tmap::tm_shape(example_paths.bind.sf.utm)  + 
  tmap::tm_lines(lty = "dashed", lwd=2, col="path_label",style = "cat", palette = c("red","blue","lightblue","green"), alpha =0.5) 

##############################
# # https://github.com/r-spatial/sf/issues/321
##############################

if (make_snap_from_points_to_streets_maps){
  
  print("getting snap from points to streets plot")
  
  snap_from_points_to_streets_zoom_even_more_plot <-
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
  
  if (print_snap_from_points_to_streets_maps){
    
    print("printing and saving snap from points to streets plot")
    
    snap_from_points_to_streets_maps_filename <- 
      paste0("C:/Users/Andrea/Desktop/Real Feel Distance/snap_from_points_to_streets_maps/",
             filename_safe_tract_name,
             ".png")
    
    tmap::tmap_save(snap_from_points_to_streets_zoom_even_more_plot,
                    snap_from_points_to_streets_maps_filename,
                    units = "in",
                    width = 6, height = 5)
    
  } else if (!print_snap_from_points_to_streets_maps){
    
    snap_from_points_to_street_maps.list[[i]] <- snap_from_points_to_streets_zoom_even_more_plot
    
  }
  
  
  
}



#snap_from_points_to_streets_zoom_even_more


# https://stackoverflow.com/questions/48400758/add-lines-between-labelled-location-points
# https://spatialanalysis.github.io/lab_tutorials/4_R_Mapping.html#color-palette
# http://www.wvview.org/spatial_analytics/Visualizing_Spatial_Data/_site/Visualize_Spatial_Data.html#content

# example_paths_base_1_plot <-
#   tmap::tm_shape(chicago.city.utm, bbox = zoom_more_bbox) +
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
#   tmap::tm_dots(col="blue", size = .08)
#   
# example_paths_base_1_plot



if (make_example_paths_base_maps){
  
  print("getting example paths base plot..this may take a while")
  
  example_paths_base_zoom_more_plot <-
    tmap::tm_shape(chicago.city.utm, bbox = zoom_more_bbox) +
    tmap::tm_polygons() +
    tmap::tm_shape(dat_sf.utm, bbox = zoom_more_bbox) +
    tmap::tm_lines(alpha = 0.2) +
    #tmap::tm_shape(one_tract.utm, bbox = zoom_more_bbox) +
    #tmap::tm_borders(lwd = 2, col = "red") +
    #tmap::tm_polygons() +
    tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
    tmap::tm_dots(col="red", size = 1, alpha = 0.2) +
    tmap::tm_shape(ever_closest_grocery_v.sf.utm, bbox = zoom_more_bbox) +
    tmap::tm_dots(col="green", size = 1, alpha = 0.3) +
    tmap::tm_shape(from_v.sf.utm, bbox = zoom_more_bbox) +
    tmap::tm_dots(col="blue", size = .08, alpha = 0.2) +
    tmap::tm_scale_bar(position=c("right", "top"))
  
  if(print_example_paths_base_maps){
    
    print("saving example paths base plot..this may take a while")
    
    example_paths_base_maps_filename <- 
      paste0("C:/Users/Andrea/Desktop/Real Feel Distance/example_paths_base_maps/",
             filename_safe_tract_name,
             ".png")
    
    tmap::tmap_save(example_paths_base_zoom_more_plot,
                    example_paths_base_maps_filename,
                    units = "in",
                    width = 6, height = 5)
    
  } else if (!print_example_paths_base_maps){
    
    example_paths_base_maps.list[[i]] <- example_paths_base_zoom_more_plot
    
  }
  
  
  
  
}





# example_paths_plot <- 
#   example_paths_base_2_plot + 
#   tmap::tm_shape(example_paths.bind.spdf)  + 
#   tmap::tm_lines(lty = "dashed", lwd=2, col="path_label",style = "cat", palette = c("red","blue","lightblue","green"), alpha =0.3) 

# example_paths_plot <- 
#   example_paths_base_2_plot + 
#   tmap::tm_shape(example_paths.bind.sf.utm)  + 
#   tmap::tm_lines(lty = "dashed", lwd=2, col="path_label",style = "cat", palette = c("red","blue","lightblue","green"), alpha =0.3) 
# 
# example_paths_plot

# example_paths_min_plot <- 
#   example_paths_base_2_plot + 
#   tmap::tm_shape(example_paths.bind.sf.utm %>% filter(path_label == "min shortest path"))  + 
#   tmap::tm_lines(lty = "dashed", lwd=2, col="red", alpha =0.5) 
# 
# example_paths_min_plot <- 
#   example_paths_min_plot +
#   tmap::tm_scale_bar(position=c("right", "top"))
# 
# example_paths_min_plot


##############################

example_paths_min_zoom_more_plot <- 
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="red", size = .1, alpha = 0.2) +
  tmap::tm_shape(ever_closest_grocery_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="green", size = .1, alpha = 0.3) +
  tmap::tm_shape(from_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="blue", size = .08, alpha = 0.2) + 
  tmap::tm_shape(example_paths.bind.sf.utm %>% filter(path_label == "min shortest path"))  + 
  tmap::tm_lines(lty = "dashed", lwd=2, col="red", alpha =0.5) +
  tmap::tm_scale_bar(position=c("right", "top")) +
  tmap::tm_layout(main.title = paste0("Min path, ",as.character(round(summary_dist_to_grocery$min_dist_to_grocery/1000,3)),"km"), 
                  title.size = 1, 
                  title.position = c("right", "top"), 
                  legend.outside=FALSE)

example_paths_max_zoom_more_plot <- 
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="red", size = .1, alpha = 0.2) +
  tmap::tm_shape(ever_closest_grocery_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="green", size = .1, alpha = 0.3) +
  tmap::tm_shape(from_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="blue", size = .08, alpha = 0.2) + 
  tmap::tm_shape(example_paths.bind.sf.utm %>% filter(path_label == "max shortest path"))  + 
  tmap::tm_lines(lty = "dashed", lwd=2, col="red", alpha =0.5) +
  tmap::tm_scale_bar(position=c("right", "top")) +
  tmap::tm_layout(main.title = paste0("Max path, ",as.character(round(summary_dist_to_grocery$max_dist_to_grocery/1000,3)),"km"), 
                  title.size = 1, 
                  title.position = c("right", "top"), 
                  legend.outside=FALSE)

example_paths_mean_zoom_more_plot <- 
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="red", size = .1, alpha = 0.2) +
  tmap::tm_shape(ever_closest_grocery_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="green", size = .1, alpha = 0.3) +
  tmap::tm_shape(from_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="blue", size = .08, alpha = 0.2) + 
  tmap::tm_shape(example_paths.bind.sf.utm %>% filter(path_label == "mean shortest path"))  + 
  tmap::tm_lines(lty = "dashed", lwd=2, col="red", alpha =0.5) +
  tmap::tm_scale_bar(position=c("right", "top")) +
  tmap::tm_layout(main.title = paste0("Mean path, ",as.character(round(summary_dist_to_grocery$mean_dist_to_grocery/1000,3)),"km"), 
                  title.size = 1, 
                  title.position = c("right", "top"), 
                  legend.outside=FALSE)

example_paths_median_zoom_more_plot <- 
tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="red", size = .1, alpha = 0.2) +
  tmap::tm_shape(ever_closest_grocery_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="green", size = .1, alpha = 0.3) +
  tmap::tm_shape(from_v.sf.utm, bbox = zoom_more_bbox) +
  tmap::tm_dots(col="blue", size = .08, alpha = 0.2) + 
  tmap::tm_shape(example_paths.bind.sf.utm %>% filter(path_label == "median shortest path"))  + 
  tmap::tm_lines(lty = "dashed", lwd=2, col="red", alpha =0.5) +
  tmap::tm_scale_bar(position=c("right", "top")) +
  tmap::tm_layout(main.title = paste0("Median path, ",as.character(round(summary_dist_to_grocery$median_dist_to_grocery/1000,3)),"km"), 
            title.size = 1, 
            title.position = c("right", "top"), 
            legend.outside=FALSE)

example_closest_paths_map_compound_title <- paste0("Example closest paths to grocery in: ",one_tract.utm$Name)
example_closest_paths_map_compound_filename <- 
  paste0("C:/Users/Andrea/Desktop/Real Feel Distance/example_closest_paths_maps/",
         filename_safe_tract_name,
         ".png")

png(example_closest_paths_map_compound_filename, antialias = "cleartype", bg = "transparent", width = 9 , height = 9, units = "in", res = 300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(
  nrow = 3,
  ncol = 2,
  heights = c(0.04, 0.48, 0.48))))
grid.text(example_closest_paths_map_compound_title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(example_paths_min_zoom_more_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(example_paths_max_zoom_more_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(example_paths_mean_zoom_more_plot, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
print(example_paths_median_zoom_more_plot, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

dev.off()

