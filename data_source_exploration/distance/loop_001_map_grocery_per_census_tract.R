#######################################################



# plot chicago city tracts, highlighting the single current census tract
census_tract_map <-
  tmap::tm_shape(chicago.city.tracts.utm) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm) +
  tmap::tm_borders(lwd = 2, col = "red")

census_tract_map <- census_tract_map + tmap::tm_scale_bar(position=c("left", "bottom"))
census_tract_map

# plot boundary of chicago city
# inside, show outline of a single census tract in the city at a time
# plus grocery stores in chicago and in cook within 1 mile of city limits
tmap::tm_shape(chicago.city.utm) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm) +
  tmap::tm_borders(lwd = 2, col = "black") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm) +  
  tmap::tm_dots(col="red")

# get just the grocery stores that are within 5km of the 
# one census tract
grocery_one_tract.buffer.sf.utm <- 
  grocery.sf.utm %>%
  sf::st_filter(one_tract.utm, 
                .predicate = sf::st_is_within_distance,
                dist = 5000)

grocery.buffer <- 5000

if (nrow(grocery_one_tract.buffer.sf.utm) < 2){
  grocery_one_tract.buffer.sf.utm <- 
    grocery.sf.utm %>%
    sf::st_filter(one_tract.utm, 
                  .predicate = sf::st_is_within_distance,
                  dist = 7000)
  grocery.buffer <- 7000
}

if (nrow(grocery_one_tract.buffer.sf.utm) < 2){
  grocery_one_tract.buffer.sf.utm <- 
    grocery.sf.utm %>%
    sf::st_filter(one_tract.utm, 
                  .predicate = sf::st_is_within_distance,
                  dist = 10000)
  grocery.buffer <- 10000
}

# get just the grocery stores that are inside of the 
# one census tract
grocery_one_tract.inside.sf.utm <- 
  grocery.sf.utm %>%
  sf::st_filter(one_tract.utm, 
                .predicate = sf::st_intersects)

# get polygon that adds a 2km buffer to the grocery stores within 5km of the one census tract 
# this will be used to make a map that is zoomed in on each census tract and surrounding grocery stores
grocery_one_tract.buffer.polygon.sf.utm <-
  sf::st_buffer(grocery_one_tract.buffer.sf.utm, dist = 2000)

if (chicago.city.tracts.utm$grocery_count_2km[i] > 1){
  # get polygon that adds a 2km buffer to the census tract polygon
  # this will be used to make a map that is even more zoomed in on each census tract and surrounding grocery stores
  one_tract.buffer.polygon.sf.utm <-
    sf::st_buffer(one_tract.utm, dist = 2000)
} else{
  
  if (chicago.city.tracts.utm$grocery_count_5km[i] > 1){
    # get polygon that adds a 2km buffer to the census tract polygon
    # this will be used to make a map that is even more zoomed in on each census tract and surrounding grocery stores
    one_tract.buffer.polygon.sf.utm <-
      sf::st_buffer(one_tract.utm, dist = 5000)
  } else {
    if (chicago.city.tracts.utm$grocery_count_7km[i] > 1){
      # get polygon that adds a 2km buffer to the census tract polygon
      # this will be used to make a map that is even more zoomed in on each census tract and surrounding grocery stores
      one_tract.buffer.polygon.sf.utm <-
        sf::st_buffer(one_tract.utm, dist = 7000)
    } else {
      if (chicago.city.tracts.utm$grocery_count_10km[i] > 1){
        # get polygon that adds a 2km buffer to the census tract polygon
        # this will be used to make a map that is even more zoomed in on each census tract and surrounding grocery stores
        one_tract.buffer.polygon.sf.utm <-
          sf::st_buffer(one_tract.utm, dist = 10000)
      }
    }
    
  }
  
}
  




# # add number of grocery stores inside each chicago census tract to chicago census tracts spatial file
# chicago.city.tracts.utm$grocery_count <- 
#   lengths(sf::st_intersects(chicago.city.tracts.utm, grocery.sf.utm))
# 
# # add number of grocery stores within .5, 1, 2, 5km of each chicago census tract to chicago census tracts spatial file
# chicago.city.tracts.utm$grocery_count_500m <- 
#   lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 500))
# 
# chicago.city.tracts.utm$grocery_count_1km <- 
#   lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 1000))
# 
# chicago.city.tracts.utm$grocery_count_2km <- 
#   lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 2000))
# 
# chicago.city.tracts.utm$grocery_count_5km <- 
#   lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 5000))

# # add area in m^2 of each chicago census tract to chicago census tracts spatial file
# chicago.city.tracts.utm$area <- 
#   sf::st_area(chicago.city.tracts.utm)


# plot boundary of chicago city
# inside, show outline of a single census tract in the city at a time
# plus grocery stores in chicago and in cook within 1 mile of city limits that are also within 5km of the census tract
tmap::tm_shape(chicago.city.utm) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm) +
  tmap::tm_borders(lwd = 2, col = "black") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery_one_tract.buffer.sf.utm) +  
  tmap::tm_dots(col="red")

# plot boundary of chicago city
# inside, show outline of a single census tract in the city at a time
# plus grocery stores in chicago and in cook within 1 mile of city limits 
# highlight grocery stores within 5km of the census tract
grocery_map <- 
  tmap::tm_shape(chicago.city.utm) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm) +
  tmap::tm_borders(lwd = 2, col = "black") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm) +  
  tmap::tm_dots(col="red") +
  tmap::tm_shape(grocery_one_tract.buffer.sf.utm) +  
  tmap::tm_dots(col="blue")

grocery_map

if(nrow(grocery_one_tract.inside.sf.utm) != 0){
  grocery_map <- 
    grocery_map +
    tmap::tm_shape(grocery_one_tract.inside.sf.utm) +  
    tmap::tm_dots(col="yellow")
}

grocery_map <- grocery_map + tmap::tm_scale_bar(position=c("left", "bottom"))

grocery_map


##############################

# plot boundary of chicago city
# inside, show outline of a single census tract in the city at a time
# plus grocery stores in chicago and in cook within 1 mile of city limits 
# highlight grocery stores within 5km of the census tract

zoom_bbox <- sf::st_bbox(grocery_one_tract.buffer.polygon.sf.utm)
zoom_more_bbox <- sf::st_bbox(one_tract.buffer.polygon.sf.utm)
zoom_even_more_bbox <- sf::st_bbox(one_tract.utm)

grocery_map_zoom <- 
  tmap::tm_shape(chicago.city.tracts.utm, bbox = zoom_bbox) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm, bbox = zoom_bbox) +
  tmap::tm_borders(lwd = 2, col = "black") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_bbox) +  
  tmap::tm_dots(col="red", size = .08) +
  tmap::tm_shape(grocery_one_tract.buffer.sf.utm, bbox = zoom_bbox) +  
  tmap::tm_dots(col="blue", size = .08)

grocery_map_zoom

if(nrow(grocery_one_tract.inside.sf.utm) != 0){
  grocery_map_zoom <- 
    grocery_map_zoom +
    tmap::tm_shape(grocery_one_tract.inside.sf.utm, bbox = zoom_bbox) +  
    tmap::tm_dots(col="yellow", size = .08)
}

grocery_map_zoom <- grocery_map_zoom + tmap::tm_scale_bar(position=c("right", "top"))
grocery_map_zoom

##############################

grocery_map_zoom_more <- 
  tmap::tm_shape(chicago.city.tracts.utm, bbox = zoom_more_bbox) + 
  tmap::tm_polygons() +
  tmap::tm_shape(one_tract.utm, bbox = zoom_more_bbox) +
  tmap::tm_borders(lwd = 2, col = "black") +
  #tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm, bbox = zoom_more_bbox) +  
  tmap::tm_dots(col="red", size = .2) +
  tmap::tm_shape(grocery_one_tract.buffer.sf.utm, bbox = zoom_more_bbox) +  
  tmap::tm_dots(col="blue", size = .2)

grocery_map_zoom_more

if(nrow(grocery_one_tract.inside.sf.utm) != 0){
  grocery_map_zoom_more <- 
    grocery_map_zoom_more +
    tmap::tm_shape(grocery_one_tract.inside.sf.utm, bbox = zoom_more_bbox) +  
    tmap::tm_dots(col="yellow", size = .2)
}

grocery_map_zoom_more <- grocery_map_zoom_more + tmap::tm_scale_bar(position=c("right", "top"))
grocery_map_zoom_more

##############################

# grocery_and_street_map_zoom_more <-
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
#   tmap::tm_dots(col="blue", size = 1)
# 
# grocery_and_street_map_zoom_more



##############################
# grocery_map_compound <- tmap::tmap_arrange(grocery_map,grocery_map_zoom) 
# grocery_map_compound
# tmap::tmap_save(grocery_map_compound, filename = "world_map.png")

tmap::tmap_arrange(census_tract_map,grocery_map,grocery_map_zoom,grocery_map_zoom_more)

library(grid)

grocery_map_compound_title <- paste0("Grocery Stores in and around: ",one_tract.utm$Name)

grocery_map_compound_filename <- 
  paste0("C:/Users/Andrea/Desktop/Real Feel Distance/census_tract_grocery_maps/",
         filename_safe_tract_name,
         ".png")

png(grocery_map_compound_filename, antialias = "cleartype", bg = "transparent", width = 8.5 , height = 4.5, units = "in", res = 300)

grid.newpage()
pushViewport(viewport(layout = grid.layout(
  nrow = 2,
  ncol = 2,
  heights = c(0.04, 0.96))))
grid.text(grocery_map_compound_title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(grocery_map, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(grocery_map_zoom, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
#print(grocery_and_street_map_zoom_more, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))

# png(grocery_map_compound_filename, antialias = "cleartype", bg = "transparent", width = 6 , height = 9, units = "in", res = 300)
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
