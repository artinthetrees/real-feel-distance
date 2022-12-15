
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4730508/
#Heat and Humidity in the City: Neighborhood Heat Index Variability in a Mid-Sized City in the Southeastern United States

library(dplyr)

tmax_summary <- 
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.tmax_by_tract.aw_mean.summary.csv",
           stringsAsFactors = FALSE)

tdmean_summary <- 
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.tdmean_by_tract.aw_mean.summary.csv",
           stringsAsFactors = FALSE)

range_summary <- 
  left_join(tmax_summary %>% select(key, min_f, max_f, range_f) %>% setNames(paste0('tmax.', names(.))),
                   tdmean_summary %>% select(key, min_f, max_f, range_f) %>% setNames(paste0('tdmean.', names(.))),
                   by = c("tmax.key" = "tdmean.key")) %>%
  arrange(desc(tmax.range_f),desc(tdmean.range_f)) 

range_summary$heat_index_max <- 
  weathermetrics::heat.index(t = range_summary$tmax.max_f, 
                             dp = range_summary$tdmean.max_f, 
                             temperature.metric = "fahrenheit", 
                             output.metric = "fahrenheit")

range_summary$heat_index_min <- 
  weathermetrics::heat.index(t = range_summary$tmax.min_f, 
                             dp = range_summary$tdmean.min_f, 
                             temperature.metric = "fahrenheit", 
                             output.metric = "fahrenheit")

range_summary$heat_index_range <- 
  range_summary$heat_index_max - range_summary$heat_index_min

range_summary <- 
  range_summary %>% 
  mutate(heat_index_max_lvl = 
           case_when(heat_index_max < 80 ~ 0,
                     heat_index_max <= 90 ~ 1,
                     heat_index_max <= 103 ~ 2,
                     heat_index_max <= 124 ~ 3,
                     heat_index_max > 124 ~ 4),
         heat_index_min_lvl = 
           case_when(heat_index_min < 80 ~ 0,
                     heat_index_min <= 90 ~ 1,
                     heat_index_min <= 103 ~ 2,
                     heat_index_min <= 124 ~ 3,
                     heat_index_min > 124 ~ 4),
         heat_index_max_min_lvl_diff = heat_index_max_lvl - heat_index_min_lvl) %>%
  arrange(desc(heat_index_max_min_lvl_diff),
          desc(heat_index_range))
    


range_summary_select <- 
  range_summary %>% 
  filter(heat_index_range > 10) %>%
  arrange(desc(heat_index_range))

# > max(range_summary$tmax_range_f)
# [1] 12.34
# > max(range_summary$tdmean_range_f)
# [1] 5.29

write.csv(range_summary_select %>% rename(date = tmax.key),
          "C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/select_days.csv")















