#####

packages <- c("formula.tools","tidyverse", "rvest", "RSelenium", "xml2","broom","foreign", "janitor","readxl" ,"XML","httr","stringr","parallel","furrr", "tictoc", "httr","stringr","parallel", "tm",  "osmdata", "ggmap", "sf", "tidygeocoder")

lapply(packages, require, character.only=TRUE)

library("tibbletime")


library("lubridate")

my_path <- "G:/My Drive/Venture Fund - Food Deserts/Data Sources and Review/AoT/_csv/Array of Things/data/"

setwd(my_path)
aot_cut <- function(date){
  require(lubridate); require(tidyverse)  
path <- "G:/My Drive/Venture Fund - Food Deserts/Data Sources and Review/AoT/_csv/Array of Things/data/"
  
  day <- read.csv(paste0(path, date, ".csv")) %>%
    mutate(datetime=ymd_hms(timestamp))
  
  
  int <- interval(ymd_hms(paste0(date, " 12:00:00")), ymd_hms(paste0(date, " 16:00:00")))
  
  
  day <- day %>%
    filter(datetime %within% int) %>% 
    mutate(date=ymd(date)) %>% 
    filter(subsystem %in% c("lightsense")) %>% #### Add filter for parameters upon agreement
    mutate(value_raw=as.numeric(value_raw), value_hrf=as.numeric(value_hrf)) %>% 
    select(-value_raw) 
  
  write.csv(day, paste0(path, "/lightsense/", date,"_cut.csv"))
}


files <- list.files(my_path, pattern = "*.csv")
dates <- as.list(str_remove(files, ".csv"))
#aot_cut(path=my_path, "2019-06-22")
library(furrr)
plan(multisession)
future_map(dates, aot_cut)
