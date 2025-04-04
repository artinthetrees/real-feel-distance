# state_string <- "IL"
# county_string <- "Cook County"
# city_string <- "Chicago"
# year_num <- 2021

# source("./secrets.R")
# source("./utils.R")

crs_data <- get_utm_zone(
    my_census_api_key = my_census_api_key,
    county_string = county_string,
    state_string = state_string,
    year_num = year_num
)

print(crs_data)

crs_lonlat <- crs_data$crs_lonlat
crs_utm <- crs_data$crs_utm