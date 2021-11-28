

#######################################
#######################################

pts_lonlat_map_zoom <- 
  tmap::tm_shape(chicago.city.tracts.utm, bbox = zoom_bbox) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm, bbox = zoom_bbox) +
  tmap::tm_borders(lwd = 2, col = "black") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_bbox) +  
  tmap::tm_dots(col="red", size = .08) +
  tmap::tm_shape(grocery_one_tract.buffer.sf.utm, bbox = zoom_bbox) +  
  tmap::tm_dots(col="blue", size = .08) +
  tmap::tm_shape(pts_lonlat.sf.utm, bbox = zoom_bbox) +  
  tmap::tm_dots(col="black", size = .05)

pts_lonlat_map_zoom

if(nrow(grocery_one_tract.inside.sf.utm) != 0){
  pts_lonlat_map_zoom <- 
    pts_lonlat_map_zoom +
    tmap::tm_shape(grocery_one_tract.inside.sf.utm, bbox = zoom_bbox) +  
    tmap::tm_dots(col="yellow", size = .08)
}

pts_lonlat_map_zoom <- pts_lonlat_map_zoom + tmap::tm_scale_bar(position=c("right", "top"))
pts_lonlat_map_zoom

##############################

pts_lonlat_map_zoom_more <- 
  tmap::tm_shape(chicago.city.tracts.utm, bbox = zoom_more_bbox) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm, bbox = zoom_more_bbox) +
  tmap::tm_borders(lwd = 2, col = "black") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +  
  tmap::tm_dots(col="red", size = .2) +
  tmap::tm_shape(grocery_one_tract.buffer.sf.utm, bbox = zoom_more_bbox) +  
  tmap::tm_dots(col="blue", size = .2) +
  tmap::tm_shape(pts_lonlat.sf.utm, bbox = zoom_bbox) +  
  tmap::tm_dots(col="black", size = .05)

pts_lonlat_map_zoom_more

if(nrow(grocery_one_tract.inside.sf.utm) != 0){
  pts_lonlat_map_zoom_more <- 
    pts_lonlat_map_zoom_more +
    tmap::tm_shape(grocery_one_tract.inside.sf.utm, bbox = zoom_more_bbox) +  
    tmap::tm_dots(col="yellow", size = .2)
}

pts_lonlat_map_zoom_more <- pts_lonlat_map_zoom_more + tmap::tm_scale_bar(position=c("right", "top"))
pts_lonlat_map_zoom_more

##############################

# pts_lonlat_and_street_map_zoom_more <-
# tmap::tm_shape(chicago.city.utm, bbox = zoom_more_bbox) +
#   tmap::tm_polygons() +
#   tmap::tm_shape(dat_sf.utm, bbox = zoom_more_bbox) +
#   tmap::tm_lines() +
#   tmap::tm_shape(one_tract.utm, bbox = zoom_bbox) +
#   tmap::tm_borders(lwd = 2, col = "red") +
#   #tmap::tm_polygons() +
#   tmap::tm_shape(grocery.sf.utm, bbox = zoom_bbox) +
#   tmap::tm_dots(col="red", size = 1) +
#   tmap::tm_shape(grocery_one_tract.buffer.sf.utm, bbox = zoom_bbox) +
#   tmap::tm_dots(col="blue", size = .08) +
#   tmap::tm_shape(pts_lonlat.sf.utm, bbox = zoom_bbox) +  
#   tmap::tm_dots(col="red", size = .05)
# 
# pts_lonlat_and_street_map_zoom_more



##############################



##############################
# grocery_map_compound <- tmap::tmap_arrange(grocery_map,grocery_map_zoom) 
# grocery_map_compound
# tmap::tmap_save(grocery_map_compound, filename = "world_map.png")

tmap::tmap_arrange(pts_lonlat_map_zoom,pts_lonlat_map_zoom_more)

#library(grid)

pts_lonlat_map_compound_title <- paste0("Grocery Stores relative to regularly spaced grid points in: ",one_tract.utm$Name)
pts_lonlat_map_compound_filename <- 
  paste0("C:/Users/Andrea/Desktop/Real Feel Distance/census_tract_grocery_and_grid_maps/",
         filename_safe_tract_name,
         ".png")

png(pts_lonlat_map_compound_filename, antialias = "cleartype", bg = "transparent", width = 8.5 , height = 4.5, units = "in", res = 300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(
  nrow = 2,
  ncol = 2,
  heights = c(0.04, 0.96))))
grid.text(pts_lonlat_map_compound_title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(pts_lonlat_map_zoom, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(pts_lonlat_map_zoom_more, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

# png(grocery_map_compound_filename, antialias = "cleartype", bg = "transparent", width = 6 , height = 4.5, units = "in", res = 300)
# 
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(
#   nrow = 3,
#   ncol = 2,
#   heights = c(0.02, 0.49, 0.49))))
# grid.text(grocery_map_compound_title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
# print(census_tract_map, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
# print(grocery_map, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
# print(grocery_map_zoom, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
# print(grocery_map_zoom_more, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

dev.off()
