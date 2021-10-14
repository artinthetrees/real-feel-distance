packages <- c("tidyverse", "rvest", "RSelenium", "xml2","broom","foreign", "janitor","readxl" ,"XML","httr","stringr","parallel","furrr", "tictoc", "httr","stringr","parallel", "tm", "osmdata", "ggmap", "sf", "tidygeocoder")

lapply(packages, require, character.only=TRUE)

######### Combining Dataframes into One #############
gc()
#source("G:/My Drive/Venture Fund - Food Deserts/Data Sources and Review/AoT/_csv/Array of Things/data/aot_cut.R")
gc()
library(data.table)

cut_path <- "G:/My Drive/Venture Fund - Food Deserts/Data Sources and Review/AoT/_csv/Array of Things/data/pm_cut" 

setwd(cut_path)

files <- list.files(pattern = "*.csv")
gc()
combined_files <- bind_rows(lapply(files, fread)) 

by_day <- combined_files %>% 
  mutate(value_hrf=as.numeric(value_hrf)) %>% 
  filter(!is.na(value_hrf)) 

rm(combined_files)
gc()
by_day_nest <- by_day %>% 
  mutate(node_id2=node_id) %>% 
  group_by(node_id) %>% 
  nest()
rm(by_day)
restrict <- function(df) {
  
  
  df <- df %>% 
    pivot_wider(names_from = c("sensor","parameter"), values_from = value_hrf)
  
} 


library(furrr)
plan(multisession)

by_day_nest <- by_day_nest %>% 
  mutate(clean = future_map(data, restrict)) %>% 
  select(-data)
rm(by_day)
gc()
by_date <- by_day_nest %>% 
  unnest()

nodes <- read.csv("G:/My Drive/Venture Fund - Food Deserts/Data Sources and Review/AoT/_csv/Array of Things/nodes.csv")

x <- full_join(by_date, nodes) 

mean_na <- function(x){
  mean(x, na.rm=T)
}

data_points <- function(x){
  length(which(!is.na(x)))
}

min_na <- function(x){
  min(x, na.rm=T)
}

max_na <- function(x){
  max(x, na.rm=T)
}


y <- x %>% 
  group_by(node_id) %>% 
  summarise_if(is.numeric, list(mean=mean_na, num_point=data_points, min=min_na, max=max_na) ) 



write.csv(y, "G:/My Drive/Venture Fund - Food Deserts/Data Sources and Review/AoT/_csv/Array of Things/metsense_summerAoT.csv")


