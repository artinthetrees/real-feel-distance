
library(tidyverse)

source("C:/Users/Andrea/Desktop/Real Feel Distance/points_to_line.R")
# source("C:/Users/Andrea/Desktop/Real Feel Distance/001_clean_grocery_long_lat.R")
# source("C:/Users/Andrea/Desktop/Real Feel Distance/002_map_grocery.R")
# source("C:/Users/Andrea/Desktop/Real Feel Distance/003_get_streetnet.R")

#save.image("C:/Users/Andrea/Desktop/Real Feel Distance/thru_get_streetnet.RData")
#save.image("C:/Users/Andrea/Desktop/Real Feel Distance/thru_get_streetnet_2.RData")
#save.image("C:/Users/Andrea/Desktop/Real Feel Distance/thru_get_streetnet_3.RData")

#load("C:/Users/Andrea/Desktop/Real Feel Distance/thru_get_streetnet.RData")
#load("C:/Users/Andrea/Desktop/Real Feel Distance/thru_get_streetnet_2.RData")
load("C:/Users/Andrea/Desktop/Real Feel Distance/thru_get_streetnet_3.RData")

orig_objects_list <- ls()

small_save_list_basic <-
  c("summary_dist_to_grocery.collect_lists.df",
    "d.df.list")

make_snap_from_points_to_streets_maps <- FALSE
print_snap_from_points_to_streets_maps <- FALSE
make_example_paths_base_maps <- FALSE
print_example_paths_base_maps <- FALSE

summary_dist_to_grocery.list <- list()
d.df.list <- list()

if (make_snap_from_points_to_streets_maps & !print_snap_from_points_to_streets_maps){
  snap_from_points_to_street_maps.list <- list()
  small_save_list_plus <- c(small_save_list_basic,"snap_from_points_to_street_maps.list")
}

if (make_example_paths_base_maps & !print_example_paths_base_maps){
  example_paths_base_maps.list <- list()
  small_save_list_plus <- c(small_save_list_basic,"example_paths_base_maps.list")
}

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
new_objects_list <- ls()[!(ls() %in% orig_objects_list)]

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

#rm(list= ls()[!(ls() %in% orig_objects_list)])


