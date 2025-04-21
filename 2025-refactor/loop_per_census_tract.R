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

#grocery_sf.lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","grocery",city_year_output_fname),obj_name = grocery_within_sf.lonlat)
grocery_sf.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","grocery",city_year_output_fname),obj_name = grocery_within_sf.utm)
#grocery_df <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","grocery",city_year_output_fname),obj_name = grocery_within_df)

tracts.within.city.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = tracts.within.city.utm)

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
  
  if (my_distances$no_distances){ # this will happen if there are no from or to points
    
    distances_df <- 
      data.frame(
        tract_name=character(),
        tract_geoid=character(),
        no_distances=integer()
        )
    
    distances_df$tract_name <- my_distances$tract_name
    distances_df$tract_geoid <- my_distances$tract_geoid
    distances_df$no_distances <- 1
    distances_df$tract_id <- tract_id
    distances_df <- distances_df %>% select(tract_id,tract_name,tract_geoid,no_distances)
    
    summary_distances_df <- 
      data.frame(
        tract_name=character(),
        tract_geoid=character(),
        no_distances=integer()
      )
    
    summary_distances_df$tract_name <- my_distances$tract_name
    summary_distances_df$tract_geoid <- my_distances$tract_geoid
    summary_distances_df$no_distances <- 1
    summary_distances_df$tract_id <- tract_id  
    summary_distances_df <- summary_distances_df %>% select(tract_id,tract_name,tract_geoid,no_distances)
    
  } else {
    
    distances_df <- my_distances$distances_df 
    distances_df$no_distances <- 0
    distances_df$tract_id <- tract_id
    distances_df <- distances_df %>% select(tract_id,tract_name,tract_geoid,no_distances,everything())
    
    summary_distances_df <- my_distances$summary_distances_df 
    summary_distances_df$no_distances <- 0
    summary_distances_df$tract_id <- tract_id  
    summary_distances_df <- summary_distances_df %>% select(tract_id,tract_name,tract_geoid,no_distances,everything())
    
  }
  
  
  distances_df_list[[i]] <- distances_df
  summary_distances_df_list[[i]] <- summary_distances_df
  
  
}
  
summary_distances_df <- 
  data.table::rbindlist(summary_distances_df_list, 
                        use.names = TRUE,
                        fill = TRUE)




#################################################################
# save for this step

save(
  summary_distances_df,
  distances_df_list,
  file = output_path
)

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################


