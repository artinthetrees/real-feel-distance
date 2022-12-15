library(dplyr)

path_to_repo <- "C:/Users/tentner-andrea/project_repositories/real-feel-distance/"

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
  rbind(grocery_1.df %>% dplyr::select(Longitude,Latitude) %>% mutate(group = "chicago"),
        grocery_2.df %>% dplyr::select(Longitude,Latitude) %>% mutate(group = "cook"),
        grocery_3.df %>% dplyr::select(Longitude,Latitude) %>% mutate(group = "cook"))

# check to make sure that the as.numeric didn't actually round the lat/longs
# print(grocery.df$Longitude, digits = 20)

# clean any points without long/lat
grocery.df <- grocery.df %>% filter(!is.na(Longitude))

# "+proj=utm +zone=16N +datum=WGS84"

# set a crs for the grocery lat/long coordinates
grocery.sf <- 
  sf::st_as_sf(grocery.df, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

# check that the crs was set and check the units (will be null for longlat projection)
sf::st_crs(grocery.sf)$proj4string
sf::st_crs(grocery.sf)$units

# convert crs to use utm which measures distance in meters, use zone 16N for chicago
grocery.sf.utm <- 
  sf::st_transform(grocery.sf,
               crs = "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80") 

# check that the crs was set and check the units (will be meters for utm projection)
sf::st_crs(grocery.sf.utm)$proj4string
sf::st_crs(grocery.sf.utm)$units



