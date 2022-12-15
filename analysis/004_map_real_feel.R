
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

my_df <- dfs.list[[4]]

maps_path <- paste0(repository_path,"intermediate_data_products/maps/date_",my_df$date[1],"/")

if (!dir.exists(maps_path)){
  dir.create(maps_path)
}

sf::st_crs(my_df)$proj4string

# tmap::tm_shape(my_df) +
#   tmap::tm_fill("heat_index_f",title="Heat Index",n=4,style="jenks")  +
#   tmap::tm_borders() +
#   tmap::tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

########################################################
# map env variables in F
########################################################
png(paste0(maps_path,"tmax_F_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("tmax_f",title="Temperature (F)",n=10,style="quantile", palette="Reds")  +
  tmap::tm_borders()

dev.off()

png(paste0(maps_path,"tdmean_F_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("tdmean_f",title="Dew Point Temperature (F)",n=10,style="quantile", palette="Reds")  +
  tmap::tm_borders()

dev.off()

png(paste0(maps_path,"heat_index_F_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("heat_index_f",title="Heat Index (F)",n=10,style="quantile", palette="Reds")  +
  tmap::tm_borders()

dev.off()

########################################################
# map env variables in C
########################################################
png(paste0(maps_path,"tmax_C_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("tmax",title="Temperature (C)",n=10,style="quantile", palette="Reds")  +
  tmap::tm_borders()

dev.off()

png(paste0(maps_path,"tdmean_C_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("tdmean",title="Dew Point Temperature (C)",n=10,style="quantile", palette="Reds")  +
  tmap::tm_borders()

dev.off()

png(paste0(maps_path,"heat_index_C_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("heat_index",title="Heat Index (C)",n=10,style="quantile", palette="Reds")  +
  tmap::tm_borders()

dev.off()

########################################################
# map prevalence of vulnerability condition
########################################################

png(paste0(maps_path,"p_older_adult_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("p_older_adult",title="% Older Adult",n=10,style="quantile", palette="Greens")  +
  tmap::tm_borders()

dev.off()

png(paste0(maps_path,"n_older_adult_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("tpop_older_adult",title="# of Older Adults",n=10,style="quantile", palette="Greens")  +
  tmap::tm_borders()

dev.off()

########################################################
# map newly avoided trips prevalence of vulnerability condition
########################################################


my_df$avoid_trip_cat <- ifelse(my_df$raw_dist > 1000,"avoid trip","take trip")
my_df$avoid_trip_cat <- ifelse(my_df$newly_avoid_trip == 1,"newly avoid trip",my_df$avoid_trip_cat)
my_df$avoid_trip_cat <- as.factor(my_df$avoid_trip_cat)

# my_df$dummy_color <- ifelse(my_df$raw_dist > 1000,"orange","pink")
# my_df$dummy_color <- ifelse(my_df$newly_avoid_trip == 1,"red",my_df$dummy_color)
# 
# tmap::tm_shape(my_df) +
#   tmap::tm_fill("dummy_color")  +
#   tmap::tm_borders() +
#   tmap::tm_layout(title = "Natural Breaks Map", title.position = c("right","bottom"))

#pal <- RColorBrewer::brewer.pal(3,"PiYG")

png(paste0(maps_path,"avoided_trips_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_fill("avoid_trip_cat",style="cat",palette=c("pink","red","green")) 

dev.off()



# tmap::tm_shape(my_df) +
#   tmap::tm_fill("raw_dist",title="Raw Distance (m)", midpoint = 1000, style = "quantile", n = 12, palette = )  +
#   tmap::tm_borders() +
#   tmap::tm_layout(title = "Quantile Map", title.position = c("right","bottom"))
# 
# tmap::tm_shape(my_df) +
#   tmap::tm_fill("real_feel_dist",title="Real Feel Distance (m)", midpoint = 1000, style = "quantile", n = 12)  +
#   tmap::tm_borders() +
#   tmap::tm_layout(title = "Quantile Map", title.position = c("right","bottom"))

########################################################
# map distance and real feel distance to food source
########################################################

pal <- RColorBrewer::brewer.pal(10,"PiYG")

png(paste0(maps_path,"raw_distance_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_polygons(col = "raw_dist", 
              style = "fixed",
              breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 2000, 3000, 5000, max(my_df$raw_dist)),
              midpoint = 1000,
              palette = rev(pal),
              legend.hist = TRUE) +
  tmap::tm_layout(legend.outside = TRUE) + 
  tmap::tm_scale_bar(position=c("left", "bottom"))

dev.off()

png(paste0(maps_path,"real_feel_distance_per_census_tract.png"))

tmap::tm_shape(my_df) +
  tmap::tm_polygons(col = "real_feel_dist", 
                    style = "fixed",
                    breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 2000, 3000, 5000, max(my_df$real_feel_dist)),
                    midpoint = 1000,
                    palette = rev(pal),
                    legend.hist = TRUE) +
  tmap::tm_layout(legend.outside = TRUE) + 
  tmap::tm_scale_bar(position=c("left", "bottom"))

dev.off()


impact_real_feel_df <- 
  my_df %>%
  summarize(tpop = sum(tpop),
            tpop_older_adult = sum(tpop_older_adult),
            tpop_older_adult_nonwhite = sum(tpop_older_adult_nonwhite),
            p_older_adult = tpop_older_adult/tpop,
            t_pop_avoid_walk = sum(my_df$tpop[my_df$avoid_trip_cat == "avoid trip"], na.rm = TRUE),
            t_pop_older_adult_avoid_walk = sum(my_df$tpop_older_adult[my_df$avoid_trip_cat == "avoid trip"], na.rm = TRUE),
            t_pop_older_adult_newly_avoid_walk = sum(my_df$tpop_older_adult[my_df$avoid_trip_cat == "newly avoid trip"], na.rm = TRUE),
            
            t_pop_older_adult_nonwhite_newly_avoid_walk = sum(my_df$tpop_older_adult_nonwhite[my_df$avoid_trip_cat == "newly avoid trip"], na.rm = TRUE),
            
            t_pop_older_adult_poverty_reported_newly_avoid_walk = sum(my_df$tpop_older_adult_poverty_reported[my_df$avoid_trip_cat == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_below_poverty_newly_avoid_walk = sum(my_df$tpop_older_adult_below_poverty[my_df$avoid_trip_cat == "newly avoid trip"], na.rm = TRUE),
            
            t_pop_older_adult_living_arrange_reported_newly_avoid_walk = sum(my_df$tpop_older_adult_living_arrange_reported[my_df$avoid_trip_cat == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_live_alone_newly_avoid_walk = sum(my_df$tpop_older_adult_live_alone[my_df$avoid_trip_cat == "newly avoid trip"], na.rm = TRUE),
            
            p_pop_older_adult_nonwhite_newly_avoid_walk = t_pop_older_adult_nonwhite_newly_avoid_walk/t_pop_older_adult_newly_avoid_walk,
            p_pop_older_adult_below_poverty_newly_avoid_walk = t_pop_older_adult_below_poverty_newly_avoid_walk/t_pop_older_adult_poverty_reported_newly_avoid_walk,
            p_pop_older_adult_live_alone_newly_avoid_walk = t_pop_older_adult_live_alone_newly_avoid_walk/t_pop_older_adult_living_arrange_reported_newly_avoid_walk
            
            ) 

# remove geometry from this df before saving as csv
impact_real_feel_df <- sf::st_set_geometry(impact_real_feel_df,NULL)

write.csv(impact_real_feel_df,paste0(maps_path,"impact_real_feel_df.csv"))



