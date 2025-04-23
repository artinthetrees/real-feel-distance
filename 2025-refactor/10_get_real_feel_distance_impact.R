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

# tracts_acs_and_geom <- 
#   tracts_within_city_sf.utm %>% select(GEOID,ALAND,AWATER,geometry) %>%
#   left_join(.,
#             state_tracts_acs_estimates_df,
#             by = c("GEOID" = "GEOID")) %>%
#   mutate(pop_per_km2 = tpop/(ALAND/1000000))


impact_df <- 
  real_feel_distance_df %>% 
  mutate(week = lubridate::week(lubridate::ymd(date)),
         avoid_trip = ifelse(raw_dist > 1000,1,0)) %>%
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
           starts_with("avoid_trip"),
           starts_with("newly_avoid_trip"))) %>% 
  left_join(.,
            state_tracts_acs_estimates_df,
            by = c("tract_geoid" = "GEOID"))

################################ impact by day

impact_by_day_df <- 
  impact_df %>%
  group_by(date) %>%
  summarize(t_pop_avoid_walk = sum(tpop[avoid_trip == 1],na.rm = TRUE),
            t_pop_older_adult_avoid_walk = sum(tpop_older_adult[avoid_trip == 1], na.rm = TRUE),
            t_pop_older_adult_nonwhite_avoid_walk = sum(tpop_older_adult_nonwhite[avoid_trip == 1], na.rm = TRUE),
            
            ## newly avoid trip - counts
            
            # older adult
            t_pop_older_adult_newly_avoid_walk_age_temp_dp_hi = sum(tpop_older_adult[avoid_trip_cat_age_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_newly_avoid_walk_age_hi = sum(tpop_older_adult[avoid_trip_cat_age_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_newly_avoid_walk_temp_dp_hi = sum(tpop_older_adult[avoid_trip_cat_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_newly_avoid_walk_hi = sum(tpop_older_adult[avoid_trip_cat_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_newly_avoid_walk_age = sum(tpop_older_adult[avoid_trip_cat_age == "newly avoid trip"], na.rm = TRUE),
            
            # older adult nonwhite
            t_pop_older_adult_nonwhite_newly_avoid_walk_age_temp_dp_hi = sum(tpop_older_adult_nonwhite[avoid_trip_cat_age_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_nonwhite_newly_avoid_walk_age_hi = sum(tpop_older_adult_nonwhite[avoid_trip_cat_age_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_nonwhite_newly_avoid_walk_temp_dp_hi = sum(tpop_older_adult_nonwhite[avoid_trip_cat_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_nonwhite_newly_avoid_walk_hi = sum(tpop_older_adult_nonwhite[avoid_trip_cat_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_nonwhite_newly_avoid_walk_age = sum(tpop_older_adult_nonwhite[avoid_trip_cat_age == "newly avoid trip"], na.rm = TRUE),
            
            # older adult below poverty line
            t_pop_older_adult_poverty_reported_newly_avoid_walk_age_temp_dp_hi = sum(tpop_older_adult_poverty_reported[avoid_trip_cat_age_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_below_poverty_newly_avoid_walk_age_temp_dp_hi = sum(tpop_older_adult_below_poverty[avoid_trip_cat_age_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            
            t_pop_older_adult_poverty_reported_newly_avoid_walk_age_hi = sum(tpop_older_adult_poverty_reported[avoid_trip_cat_age_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_below_poverty_newly_avoid_walk_age_hi = sum(tpop_older_adult_below_poverty[avoid_trip_cat_age_hi == "newly avoid trip"], na.rm = TRUE),
            
            t_pop_older_adult_poverty_reported_newly_avoid_walk_temp_dp_hi = sum(tpop_older_adult_poverty_reported[avoid_trip_cat_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_below_poverty_newly_avoid_walk_temp_dp_hi = sum(tpop_older_adult_below_poverty[avoid_trip_cat_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            
            t_pop_older_adult_poverty_reported_newly_avoid_walk_hi = sum(tpop_older_adult_poverty_reported[avoid_trip_cat_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_below_poverty_newly_avoid_walk_hi = sum(tpop_older_adult_below_poverty[avoid_trip_cat_hi == "newly avoid trip"], na.rm = TRUE),
            
            t_pop_older_adult_poverty_reported_newly_avoid_walk_age = sum(tpop_older_adult_poverty_reported[avoid_trip_cat_age == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_below_poverty_newly_avoid_walk_age = sum(tpop_older_adult_below_poverty[avoid_trip_cat_age == "newly avoid trip"], na.rm = TRUE),
            
            # older adult live alone
            t_pop_older_adult_living_arrange_reported_newly_avoid_walk_age_temp_dp_hi = sum(tpop_older_adult_living_arrange_reported[avoid_trip_cat_age_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            t_pop_older_adult_live_alone_newly_avoid_walk_age_temp_dp_hi = sum(tpop_older_adult_live_alone[avoid_trip_cat_age_temp_dp_hi == "newly avoid trip"], na.rm = TRUE),
            
            ## 
            
            tpop = sum(tpop),
            tpop_older_adult = sum(tpop_older_adult),
            tpop_older_adult_nonwhite = sum(tpop_older_adult_nonwhite),
            
            p_older_adult = tpop_older_adult/tpop,
            p_older_adult_nonwhite = tpop_older_adult_nonwhite/tpop,
            
            ## newly avoid trip - percentages
            
            p_pop_older_adult_nonwhite_newly_avoid_walk_age_temp_dp_hi = t_pop_older_adult_nonwhite_newly_avoid_walk_age_temp_dp_hi/t_pop_older_adult_newly_avoid_walk_age_temp_dp_hi,
            p_pop_older_adult_below_poverty_newly_avoid_walk_age_temp_dp_hi = t_pop_older_adult_below_poverty_newly_avoid_walk_age_temp_dp_hi/t_pop_older_adult_poverty_reported_newly_avoid_walk_age_temp_dp_hi,
            p_pop_older_adult_live_alone_newly_avoid_walk_age_temp_dp_hi = t_pop_older_adult_live_alone_newly_avoid_walk_age_temp_dp_hi/t_pop_older_adult_living_arrange_reported_newly_avoid_walk_age_temp_dp_hi
            
            ## 
            ) %>%
  ungroup()
            

            
            
            
 

# ggplot() + 
#   geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age_temp_dp_hi),colour = "blue") + 
#   geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age_hi),colour = "red") + 
#   geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_temp_dp_hi),colour = "lightblue") + 
#   geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_hi),colour = "pink") + 
#   geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age),colour = "gray") +
#   xlab(NULL) +
#   ylab("# Older Adults Avoiding Walking Trips")

by_day <- 
ggplot() + 
  geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age_temp_dp_hi,color = "RF - full")) + 
  geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age_hi,color = "RF - heat index")) + 
  geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_temp_dp_hi,color = "RF - full, minus age")) + 
  geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_hi,color = "RF - heat index, minus age")) + 
  geom_point(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age,color = "RF - age only")) +
  scale_color_manual(
    name = 'Real Feel Type',
    breaks = c("RF - full", "RF - heat index", "RF - full, minus age", "RF - heat index, minus age","RF - age only"),
    values = c("RF - full"="blue", "RF - heat index"="red", "RF - full, minus age"="lightblue", "RF - heat index, minus age"="pink","RF - age only"="gray")
  ) +
  #theme(legend.title = element_text(size = 20), legend.text = element_text(size = 14)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("# Older Adults Newly Avoiding Walking Trips")

by_day_smoothed <- 
ggplot() + 
  geom_smooth(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age_temp_dp_hi,color = "RF - full")) + 
  geom_smooth(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age_hi,color = "RF - heat index")) + 
  geom_smooth(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_temp_dp_hi,color = "RF - full, minus age")) + 
  geom_smooth(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_hi,color = "RF - heat index, minus age")) + 
  geom_smooth(data=impact_by_day_df,aes(x=lubridate::ymd(date),y=t_pop_older_adult_newly_avoid_walk_age,color = "RF - age only")) +
  scale_color_manual(
    name = 'Real Feel Type',
    breaks = c("RF - full", "RF - heat index", "RF - full, minus age", "RF - heat index, minus age","RF - age only"),
    values = c("RF - full"="blue", "RF - heat index"="red", "RF - full, minus age"="lightblue", "RF - heat index, minus age"="pink","RF - age only"="gray")
  ) +
  #theme(legend.title = element_text(size = 20), legend.text = element_text(size = 14)) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("# Older Adults Newly Avoiding Walking Trips")

by_day_patchwork <- 
  by_day/by_day_smoothed + plot_annotation(tag_levels = 'A')

################################ impact by week - focus on summer months

# filter to just the summer months
# for each week in the summer months, 
# add up the number of days in that week where a trip is avoided (raw distance) or newly avoided based on real feel
# drop the first and last week of the summer months period do get rid of possible partial weeks

impact_by_week <- 
  impact_df %>%
  filter(lubridate::month(lubridate::ymd(date)) %in% c(6,7,8,9)) %>%
  group_by(tract_id,tract_geoid,tract_name,week)%>%
  summarise(
    avoid_trip_sum = sum(avoid_trip),
    across(starts_with("newly_avoid_trip"),.fns=list(sum = ~sum(.x)))
    ) %>%
  ungroup() %>%
  group_by(tract_id) %>%
  filter(week != max(week) & week != min(week)) %>%
  ungroup()

n_weeks <- impact_by_week %>% group_by(tract_id) %>% summarise(n=n())
n_weeks <- n_weeks$n[1]

impact_by_week_by_tract <- 
  impact_by_week %>%
  group_by(tract_id,tract_geoid,tract_name) %>%
  summarise(
    avoid_trip_sum = sum(avoid_trip_sum==7),
    across(starts_with("newly_avoid_trip"),.fns=list(sum = ~sum(.x==7)))
  ) %>%
  ungroup() %>%
  mutate(
    across(-c(tract_id,tract_geoid,tract_name),.fns = list(
      g_than_1w = ~ifelse(.x>1,1,0)
    ))
  )

hi <- 
  tracts_within_city_sf.utm %>% 
  left_join(.,
            impact_by_tract,
            by = c("GEOID"="tract_geoid"))

impact_by_week_by_tract_summary <- 
  impact_by_week_by_tract %>%
  summarise(
    n=n(),
    n_avoid_trip = sum(avoid_trip_sum == 52,na.rm = TRUE),
    across(starts_with("newly_avoid_trip"),.fns=list(sum = ~sum(.x > 1,na.rm = TRUE)))
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















