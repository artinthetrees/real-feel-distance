
library(dplyr)

repository_path <- "C:/Users/Andrea/Desktop/repositories/real-feel-distance/"

load(paste0(repository_path,"data_source_exploration/maps/get_boundary_maps.RData"))

real_feel <-
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.daily_data.real_feel.csv",
           stringsAsFactors = FALSE)



dfs.list <- list()

for (i in 1:length(unique(real_feel$date))){
  
  dfs.list[[i]] <-
    left_join(chicago.city.tracts.utm %>% mutate(ID = as.numeric(row.names(.))),
                real_feel %>% filter(date == unique(real_feel$date)[[i]]) %>% select(-X.1,-X.2),
              by = c("ID" = "X")) 
  
  print(min(dfs.list[[i]]$heat_index_f))
  print(max(dfs.list[[i]]$heat_index_f))
  
  
}

sf::st_crs(dfs.list[[1]])$proj4string

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_fill("heat_index_f",title="Heat Index",n=4,style="jenks")  +
  tmap::tm_borders() +
  tmap::tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_fill("raw_dist",title="Heat Index",n=4,style="quantile")  +
  tmap::tm_borders() +
  tmap::tm_layout(title = "Quantile Map", title.position = c("right","bottom"))

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_fill("real_feel_dist",title="Heat Index",n=4,style="quantile")  +
  tmap::tm_borders() +
  tmap::tm_layout(title = "Quantile Map", title.position = c("right","bottom"))

dfs.list[[4]]$avoid_trip_cat <- ifelse(dfs.list[[4]]$raw_dist > 1000,"avoid trip","take trip")
dfs.list[[4]]$avoid_trip_cat <- ifelse(dfs.list[[4]]$newly_avoid_trip == 1,"newly avoid trip",dfs.list[[4]]$avoid_trip_cat)
dfs.list[[4]]$avoid_trip_cat <- as.factor(dfs.list[[4]]$avoid_trip_cat)

dfs.list[[4]]$dummy_color <- ifelse(dfs.list[[4]]$raw_dist > 1000,"orange","pink")
dfs.list[[4]]$dummy_color <- ifelse(dfs.list[[4]]$newly_avoid_trip == 1,"red",dfs.list[[4]]$dummy_color)

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_fill("dummy_color")  +
  tmap::tm_borders() +
  tmap::tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_fill("avoid_trip_cat",style="cat",palette="Paired") 



tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_fill("raw_dist",title="Raw Distance (m)", midpoint = 1000, style = "quantile", n = 12, palette = )  +
  tmap::tm_borders() +
  tmap::tm_layout(title = "Quantile Map", title.position = c("right","bottom"))

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_fill("real_feel_dist",title="Real Feel Distance (m)", midpoint = 1000, style = "quantile", n = 12)  +
  tmap::tm_borders() +
  tmap::tm_layout(title = "Quantile Map", title.position = c("right","bottom"))

pal <- RColorBrewer::brewer.pal(8,"PiYG")

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_polygons(col = "raw_dist", 
              style = "fixed",
              breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 2000, max(dfs.list[[4]]$raw_dist)),
              midpoint = 1000,
              palette = rev(pal),
              legend.hist = TRUE) +
  tmap::tm_layout(legend.outside = TRUE) 

tmap::tm_shape(dfs.list[[4]]) +
  tmap::tm_polygons(col = "real_feel_dist", 
                    style = "fixed",
                    breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 2000, max(dfs.list[[4]]$real_feel_dist)),
                    midpoint = 1000,
                    palette = rev(pal),
                    legend.hist = TRUE) +
  tmap::tm_layout(legend.outside = TRUE) 








