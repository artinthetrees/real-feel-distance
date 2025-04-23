library(tidyverse)
#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","collected_data",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

prism_var <- "tdmean"
tdmean_by_tract <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","environmental_data",prism_var,city_year_output_fname),obj_name = prism_by_boundary.aw_mean.df)

prism_var <- "tmax"
tmax_by_tract <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","environmental_data",prism_var,city_year_output_fname),obj_name = prism_by_boundary.aw_mean.df)

summary_distances_df <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","distance_to_grocery",city_year_output_fname),obj_name = summary_distances_df)

#################################################################
# work for this step

# names(tdmean_by_tract) <- stringr::str_sub(names(tdmean_by_tract), start= -8)
# names(tmax_by_tract) <- stringr::str_sub(names(tmax_by_tract), start= -8)

tdmean_by_tract$tract_id <- as.integer(rownames(tdmean_by_tract))
tdmean_by_tract <- tdmean_by_tract %>% select(tract_id,everything())

tmax_by_tract$tract_id <- as.integer(rownames(tmax_by_tract))
tmax_by_tract <- tmax_by_tract %>% select(tract_id,everything())

# Reshape data from wide to long format
tdmean_by_tract_long <- 
  pivot_longer(tdmean_by_tract, cols = starts_with(as.character(year_num)), names_to = "date", values_to = "tdmean")

tmax_by_tract_long <- 
  pivot_longer(tmax_by_tract, cols = starts_with(as.character(year_num)), names_to = "date", values_to = "tmax")

collect_data_df <- 
  tdmean_by_tract_long %>%
  left_join(.,
            tmax_by_tract_long,
            by = c("tract_id" = "tract_id","date" = "date")) %>%
  left_join(.,
            summary_distances_df,
            by = c("tract_id" = "tract_id")) %>%
  mutate(dist_to_grocery = median_dist_to_grocery,
         heat_index = weathermetrics::heat.index(t = tmax,
                                                 dp = tdmean, 
                                                 temperature.metric = "celsius", 
                                                 output.metric = "celsius"),
         tmax_f = weathermetrics::celsius.to.fahrenheit(tmax, round = 2),
         tdmean_f = weathermetrics::celsius.to.fahrenheit(tdmean, round = 2),
         heat_index_f = weathermetrics::heat.index(t = tmax_f,
                                                   dp = tdmean_f, 
                                                   temperature.metric = "fahrenheit", 
                                                   output.metric = "fahrenheit"),
         heat_index_invalid = ifelse(tmax < 27 | tdmean < 12,1,0)
         )


#################################################################
# save for this step

save(
  collect_data_df,
  file = output_path
)

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################















