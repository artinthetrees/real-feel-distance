library(tidyverse)
#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","real_feel_distance_to_grocery",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

collect_data_df <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","collected_data",city_year_output_fname),obj_name = collect_data_df)

#################################################################
# work for this step

age_dummy <- 1
age_dummy_young <- 0
# https://www.healthline.com/health/exercise-fitness/average-walking-speed#average-speed-by-age
age_penalty <- 0.15

# if temp is above ideal environmental temp for humans (70F) then set this dummy to 1
temp_ref_f <- 70
temp_abv70F_dummy <- quote(ifelse(tmax >= weathermetrics::fahrenheit.to.celsius(temp_ref_f, round = 2),1,0))
temp_abv70F_penalty <- 0.0056 
temp_abv70F_penalty_young <- 0.0035 
# 5% decrease in aerobic capacity for every 10 degrees celsius >> 0.5% increase in 'distance' for every 1 degree celsius

dp_ref_f <- 55
dp_abv55F_dummy <- quote(ifelse(tdmean >= weathermetrics::fahrenheit.to.celsius(dp_ref_f, round = 2),1,0)) 
dp_abv55F_penalty <- 0.055
dp_abv55F_penalty_young <- 0.077

# https://www.sciencedirect.com/science/article/pii/S0160412021004591
# https://www.sciencedirect.com/science/article/pii/S0160412021004591
# https://ehjournal.biomedcentral.com/articles/10.1186/s12940-016-0167-3
# https://ieeexplore.ieee.org/abstract/document/8254354
# https://www.sciencedirect.com/science/article/pii/S1555415506008889?casa_token=0wRrFTicIZUAAAAA:0hqN1TNm9DyCY-dDsIZkLKtrN2mV9T5gsPsv65QkZrMEPiqvWjjhaVqorG6GRCDvrdhBDsloGg
# https://www.scielo.br/j/aabc/a/TPZpmfGmztHbBjJVnqJLNBv/?lang=en
# https://repositorio-aberto.up.pt/bitstream/10216/114841/1/MonteiroCarvalho2013.pdf
hi_ref_f <- 80
hi_abv80F_dummy <- quote(ifelse(heat_index >= weathermetrics::fahrenheit.to.celsius(hi_ref_f, round = 2),1,0)) 
hi_abv80F_penalty <- 0.075
hi_abv80F_penalty_young <- 0.025

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3377942/
walking_dist <- 1000


real_feel_distance_df <- 
  collect_data_df %>%
  mutate(temp_dummy = eval(temp_abv70F_dummy),
         dp_dummy = eval(dp_abv55F_dummy),
         hi_dummy = eval(hi_abv80F_dummy),
         
         age_dist = dist_to_grocery*age_dummy*age_penalty,
         temp_dist = dist_to_grocery*temp_dummy*temp_abv70F_penalty*(tmax - weathermetrics::fahrenheit.to.celsius(temp_ref_f, round = 2)),
         dp_dist = dist_to_grocery*dp_dummy*dp_abv55F_penalty*(tdmean - weathermetrics::fahrenheit.to.celsius(dp_ref_f, round = 2)),
         hi_dist = dist_to_grocery*hi_dummy*hi_abv80F_penalty*(heat_index - weathermetrics::fahrenheit.to.celsius(hi_ref_f, round = 2)),
         
         age_dist_young = dist_to_grocery*age_dummy_young*age_penalty,
         temp_dist_young = dist_to_grocery*temp_dummy*temp_abv70F_penalty_young*(tmax - weathermetrics::fahrenheit.to.celsius(temp_ref_f, round = 2)),
         dp_dist_young = dist_to_grocery*dp_dummy*dp_abv55F_penalty_young*(tdmean - weathermetrics::fahrenheit.to.celsius(dp_ref_f, round = 2)),
         hi_dist_young = dist_to_grocery*hi_dummy*hi_abv80F_penalty_young*(heat_index - weathermetrics::fahrenheit.to.celsius(hi_ref_f, round = 2)),
         
         raw_dist = dist_to_grocery,
         avoid_trip = ifelse(raw_dist > walking_dist,1,0),
         
         real_feel_dist_age_temp_dp_hi = 
           dist_to_grocery + 
           age_dist +
           temp_dist + 
           dp_dist + 
           hi_dist,
         real_feel_dist_age_hi = 
           dist_to_grocery + 
           age_dist + 
           hi_dist,
         real_feel_dist_temp_dp_hi = 
           dist_to_grocery + 
           temp_dist + 
           dp_dist + 
           hi_dist,
         real_feel_dist_hi = 
           dist_to_grocery + 
           hi_dist,
         real_feel_dist_age = 
           dist_to_grocery + 
           age_dist,
         
         real_feel_dist_age_temp_dp_hi_young = 
           dist_to_grocery + 
           age_dist_young +
           temp_dist_young + 
           dp_dist_young + 
           hi_dist_young,
         real_feel_dist_age_hi_young = 
           dist_to_grocery + 
           age_dist_young + 
           hi_dist_young,
         real_feel_dist_temp_dp_hi_young = 
           dist_to_grocery + 
           temp_dist_young + 
           dp_dist_young + 
           hi_dist_young,
         real_feel_dist_hi_young = 
           dist_to_grocery + 
           hi_dist_young,
         real_feel_dist_age_young = 
           dist_to_grocery + 
           age_dist_young,
         
         p_increase_age_temp_dp_hi = real_feel_dist_age_temp_dp_hi/raw_dist - 1,
         more_than_double_age_temp_dp_hi = ifelse(p_increase_age_temp_dp_hi > 1,1,0),
         newly_avoid_trip_age_temp_dp_hi = ifelse(raw_dist <= walking_dist & real_feel_dist_age_temp_dp_hi > walking_dist,1,0),
         avoid_trip_cat_age_temp_dp_hi = case_when(
           raw_dist <= walking_dist & real_feel_dist_age_temp_dp_hi <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_age_temp_dp_hi > walking_dist ~ "newly avoid trip",
           ),
         
         p_increase_age_hi = real_feel_dist_age_hi/raw_dist - 1,
         more_than_double_age_hi = ifelse(p_increase_age_hi > 1,1,0),
         newly_avoid_trip_age_hi = ifelse(raw_dist <= walking_dist & real_feel_dist_age_hi > walking_dist,1,0),
         avoid_trip_cat_age_hi = case_when(
           raw_dist <= walking_dist & real_feel_dist_age_hi <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_age_hi > walking_dist ~ "newly avoid trip",
         ),
         
         p_increase_temp_dp_hi = real_feel_dist_temp_dp_hi/raw_dist - 1,
         more_than_double_temp_dp_hi = ifelse(p_increase_temp_dp_hi > 1,1,0),
         newly_avoid_trip_temp_dp_hi = ifelse(raw_dist <= walking_dist & real_feel_dist_temp_dp_hi > walking_dist,1,0),
         avoid_trip_cat_temp_dp_hi = case_when(
           raw_dist <= walking_dist & real_feel_dist_temp_dp_hi <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_temp_dp_hi > walking_dist ~ "newly avoid trip",
         ),
         
         p_increase_hi = real_feel_dist_hi/raw_dist - 1,
         more_than_double_hi = ifelse(p_increase_hi > 1,1,0),
         newly_avoid_trip_hi = ifelse(raw_dist <= walking_dist & real_feel_dist_hi > walking_dist,1,0),
         avoid_trip_cat_hi = case_when(
           raw_dist <= walking_dist & real_feel_dist_hi <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_hi > walking_dist ~ "newly avoid trip",
         ),
         
         p_increase_age = real_feel_dist_age/raw_dist - 1,
         more_than_double_age = ifelse(p_increase_age > 1,1,0),
         newly_avoid_trip_age = ifelse(raw_dist <= walking_dist & real_feel_dist_age > walking_dist,1,0),
         avoid_trip_cat_age = case_when(
           raw_dist <= walking_dist & real_feel_dist_age <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_age > walking_dist ~ "newly avoid trip",
         ),
         #
         p_increase_age_temp_dp_hi_young = real_feel_dist_age_temp_dp_hi_young/raw_dist - 1,
         more_than_double_age_temp_dp_hi_young = ifelse(p_increase_age_temp_dp_hi_young > 1,1,0),
         newly_avoid_trip_age_temp_dp_hi_young = ifelse(raw_dist <= walking_dist & real_feel_dist_age_temp_dp_hi_young > walking_dist,1,0),
         avoid_trip_cat_age_temp_dp_hi_young = case_when(
           raw_dist <= walking_dist & real_feel_dist_age_temp_dp_hi_young <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_age_temp_dp_hi_young > walking_dist ~ "newly avoid trip",
         ),
         
         p_increase_age_hi_young = real_feel_dist_age_hi_young/raw_dist - 1,
         more_than_double_age_hi_young = ifelse(p_increase_age_hi_young > 1,1,0),
         newly_avoid_trip_age_hi_young = ifelse(raw_dist <= walking_dist & real_feel_dist_age_hi_young > walking_dist,1,0),
         avoid_trip_cat_age_hi_young = case_when(
           raw_dist <= walking_dist & real_feel_dist_age_hi_young <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_age_hi_young > walking_dist ~ "newly avoid trip",
         ),
         
         p_increase_temp_dp_hi_young = real_feel_dist_temp_dp_hi_young/raw_dist - 1,
         more_than_double_temp_dp_hi_young = ifelse(p_increase_temp_dp_hi_young > 1,1,0),
         newly_avoid_trip_temp_dp_hi_young = ifelse(raw_dist <= walking_dist & real_feel_dist_temp_dp_hi_young > walking_dist,1,0),
         avoid_trip_cat_temp_dp_hi_young = case_when(
           raw_dist <= walking_dist & real_feel_dist_temp_dp_hi_young <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_temp_dp_hi_young > walking_dist ~ "newly avoid trip",
         ),
         
         p_increase_hi_young = real_feel_dist_hi_young/raw_dist - 1,
         more_than_double_hi_young = ifelse(p_increase_hi_young > 1,1,0),
         newly_avoid_trip_hi_young = ifelse(raw_dist <= walking_dist & real_feel_dist_hi_young > walking_dist,1,0),
         avoid_trip_cat_hi_young = case_when(
           raw_dist <= walking_dist & real_feel_dist_hi_young <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_hi_young > walking_dist ~ "newly avoid trip",
         ),
         
         p_increase_age_young = real_feel_dist_age_young/raw_dist - 1,
         more_than_double_age_young = ifelse(p_increase_age_young > 1,1,0),
         newly_avoid_trip_age_young = ifelse(raw_dist <= walking_dist & real_feel_dist_age_young > walking_dist,1,0),
         avoid_trip_cat_age_young = case_when(
           raw_dist <= walking_dist & real_feel_dist_age_young <= walking_dist ~ "take trip",
           raw_dist > walking_dist ~ "avoid trip",
           raw_dist <= walking_dist & real_feel_dist_age_young > walking_dist ~ "newly avoid trip",
         ),
         
         )

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















