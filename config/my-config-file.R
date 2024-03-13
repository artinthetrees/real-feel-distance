library(dplyr)

#-----------------------------------
repository_path <- "C:/Users/tentner-andrea/project_repositories/real-feel-distance/"
prism_base_path <- "P:/LABO.10.32/Common/prism-climate-data/"
#-----------------------------------
prism_var <- "tmax"

state <- "IL"
county <- "Cook"
city <- "Chicago"
year <- 2020
#-----------------------------------

county_string <- paste(county,"County")
state_string <- state

tidycensus::census_api_key(my_census_api_key)

fips_cd_df <-
    tidycensus::fips_codes %>%
    filter(county == county_string & state == state_string)

county_fips_cd <- sprintf("%03s", (fips_cd_df[1, "county_code"]))
state_fips_cd <- sprintf("%02s", fips_cd_df[1, "state_code"])

county_centroid <- 
    tigris::counties(state = state, resolution = "20m") %>%
    sf::st_centroid() %>%
    filter(COUNTYFP == county_fips_cd)

county_centroid <- 
    county_centroid %>%
    mutate(lat = unlist(purrr::map(county_centroid$geometry,2)),
           lon = unlist(purrr::map(county_centroid$geometry,1))) %>% 
    sf::st_drop_geometry()

county_centroid_lon <- county_centroid[1,"lon"] 
county_utm_zone <- floor((county_centroid_lon + 180) / 6) + 1

#-----------------------------------
crs_lonlat <- "+proj=longlat +datum=NAD83"
#crs_utm <- "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80"
#utm_zone <- ggmap::geocode(city)
crs_utm <- paste0("+proj=utm +zone=",as.character(county_utm_zone)," +datum=NAD83")

