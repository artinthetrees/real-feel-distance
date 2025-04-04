my_font_size <- 10
options(tigris_use_cache = TRUE)

install_census_api_key <- TRUE
overwrite_existing_census_api_key <- TRUE

state_string <- "IL"
county_string <- "Cook County"
city_string <- "Chicago"
year_num <- 2021

environmental_data <- c("prism")

source("./2025-refactor/secrets.R")
source("./2025-refactor/utils.R")

#source("./2025-refactor/01_get_crs.R")
#source("./2025-refactor/02_get_boundary_maps.R")