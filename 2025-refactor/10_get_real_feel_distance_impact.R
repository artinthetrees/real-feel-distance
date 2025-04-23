library(tidyverse)
#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","real_feel_distance_impact",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

real_feel_distance_df <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","real_feel_distance_to_grocery",city_year_output_fname),obj_name = real_feel_distance_df)

state_tracts_acs_estimates_df <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","acs_data",city_year_output_fname),obj_name = state_tracts_acs_estimates_df)

crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
crs_lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_lonlat)

print(crs_utm)
print(crs_lonlat)

tracts_within_city_sf.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = tracts.within.city.utm)

#################################################################
# work for this step

tracts_acs_and_geom <- 
  tracts_within_city_sf.utm %>% select(GEOID,ALAND,AWATER,geometry) %>%
  left_join(.,
            state_tracts_acs_estimates_df,
            by = c("GEOID" = "GEOID")) %>%
  mutate(pop_per_km2 = tpop/(ALAND/1000000))


impact_df <- 
  real_feel_distance_df %>% 
  mutate(week = lubridate::week(lubridate::ymd(date))) %>%
  select(c(tract_id,
           tract_geoid,
           tract_name,
           date,
           week,
           no_distances,
           n_from_pnts,
           n_from_pnts_no_path,
           n_to_points,
           tmax,
           tdmean,
           heat_index,
           tmax_f,
           tdmean_f,
           heat_index_f,
           temp_dummy,
           dp_dummy,
           hi_dummy,
           raw_dist,
           starts_with("newly_avoid_trip"))) 
  
impact_by_week <- 
  impact_df %>% 
  group_by(tract_id,tract_geoid,tract_name,week)%>%
  summarise()
  my_df$avoid_trip_cat <- ifelse(my_df$raw_dist > 1000,"avoid trip","take trip")
my_df$avoid_trip_cat <- ifelse(my_df$newly_avoid_trip == 1,"newly avoid trip",my_df$avoid_trip_cat)
my_df$avoid_trip_cat <- as.factor(my_df$avoid_trip_cat)
#################################################################
# save for this step

save(
  real_feel_distance_df,
  file = output_path
)

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################















