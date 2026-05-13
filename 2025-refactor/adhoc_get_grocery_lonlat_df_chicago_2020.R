library(dplyr)

path_to_repo <- "./"

# Bring in points
grocery_1.df <- read.csv(paste0(path_to_repo, "grocery_data/Grocery_Store_Status.csv"), stringsAsFactors = FALSE)
grocery_1.df$Longitude <- as.numeric(sapply(grocery_1.df$Location, function(x) strsplit(strsplit(x," ")[[1]][2],"\\(")[[1]][2]))
grocery_1.df$Latitude <- as.numeric(sapply(grocery_1.df$Location, function(x) strsplit(strsplit(x," ")[[1]][3],"\\)")[[1]][1]))

grocery_2.df <- read.csv(paste0(path_to_repo, "grocery_data/Nearby_Cook_County_Grocery_Store_Chains.csv"), stringsAsFactors = FALSE)
grocery_2.df$Address <- sapply(grocery_2.df$LOCATION, function(x) stringr::str_trim(strsplit(x,"\\(")[[1]][1]))
grocery_2.df$LongLat <- sapply(grocery_2.df$LOCATION, function(x) strsplit(x,"\\(")[[1]][2])
grocery_2.df$Longitude <- as.numeric(sapply(grocery_2.df$LongLat, function(x) strsplit(strsplit(x," ")[[1]][2],"\\)")[[1]][1]))
grocery_2.df$Latitude <- as.numeric(sapply(grocery_2.df$LongLat, function(x) strsplit(x,",")[[1]][1]))

grocery_3.df <- read.csv(paste0(path_to_repo, "grocery_data/Nearby_Independent_Cook_County_Grocery_Stores.csv"), stringsAsFactors = FALSE)
grocery_3.df$Longitude <- as.numeric(sapply(grocery_3.df$LOCATION, function(x) strsplit(strsplit(x," ")[[1]][2],"\\)")[[1]][1]))
grocery_3.df$Latitude <- as.numeric(sapply(grocery_3.df$LOCATION, function(x) strsplit(strsplit(x,",")[[1]][1],"\\(")[[1]][2]))

grocery.df <-
  rbind(grocery_1.df %>% dplyr::filter(New.status == "OPEN")  %>% dplyr::select(Longitude,Latitude) %>% mutate(group = "chicago"),
        grocery_2.df %>% dplyr::select(Longitude,Latitude) %>% mutate(group = "cook"),
        grocery_3.df %>% dplyr::select(Longitude,Latitude) %>% mutate(group = "cook"))


write.csv(grocery.df,"./2025-refactor/input_data/grocery_data_test/Chicago_2020.csv",row.names = FALSE)


