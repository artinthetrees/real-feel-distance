
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4730508/
#Heat and Humidity in the City: Neighborhood Heat Index Variability in a Mid-Sized City in the Southeastern United States

library(dplyr)

select_days <- 
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/select_days.csv",
           stringsAsFactors = FALSE)

select_days_char <- as.character(select_days$date)

dist_to_grocery <- 
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/dist_to_grocery.csv",
           stringsAsFactors = FALSE)

chicago.city.tmax_by_tract <- 
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.tmax_by_tract.aw_mean.csv",
           stringsAsFactors = FALSE)

chicago.city.tdmean_by_tract <- 
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.tdmean_by_tract.aw_mean.csv",
           stringsAsFactors = FALSE)

names(chicago.city.tdmean_by_tract) <- stringr::str_sub(names(chicago.city.tdmean_by_tract), start= -8)
names(chicago.city.tmax_by_tract) <- stringr::str_sub(names(chicago.city.tmax_by_tract), start= -8)

dfs.list <- list()

for (i in 1:length(select_days_char)){
  
  dfs.list[[i]] <- 
    dist_to_grocery %>% 
    select(X,median_dist_to_grocery) %>%
    rename(dist_to_grocery = median_dist_to_grocery) %>%
    left_join(.,
              chicago.city.tmax_by_tract %>% 
                select(X,.data[[select_days_char[i]]]) %>%
                rename(tmax = .data[[select_days_char[i]]]),
              by = c("X" = "X")) %>%
    left_join(.,
              chicago.city.tdmean_by_tract %>%
                select(X,.data[[select_days_char[i]]]) %>%
                rename(tdmean = .data[[select_days_char[i]]]),
              by = c("X" = "X")
    ) %>%
    mutate(heat_index = weathermetrics::heat.index(t = tmax,
                                                   dp = tdmean, 
                                                   temperature.metric = "celsius", 
                                                   output.metric = "celsius"),
           tmax_f = weathermetrics::celsius.to.fahrenheit(tmax, round = 2),
           tdmean_f = weathermetrics::celsius.to.fahrenheit(tdmean, round = 2),
           heat_index_f = weathermetrics::heat.index(t = tmax_f,
                                                     dp = tdmean_f, 
                                                     temperature.metric = "fahrenheit", 
                                                     output.metric = "fahrenheit"))
  
  
}

names(dfs.list) <- select_days_char

dfs.bind <- data.table::rbindlist(dfs.list,idcol = "date")

write.csv(dfs.bind,
          "C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.daily_data.csv")



