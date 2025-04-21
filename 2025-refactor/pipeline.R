###########################################
state_string <- "IL"
county_string <- "Cook County"
city_string <- "Chicago"
year_num <- 2020

###########################################
input_dir <- "./2025-refactor/input_data/"
output_dir <- "./2025-refactor/output_data/"
###########################################
crs_datum <- "WGS84" # other option is NAD83; use WGS84 when using dodgr
###########################################
options(tigris_use_cache = TRUE)
###########################################

source("./2025-refactor/secrets.R")
source("./2025-refactor/utils.R")
source("./2025-refactor/utils_get_crs.R")
source("./2025-refactor/utils_get_boundary_maps.R")
source("./2025-refactor/utils_get_streetnet.R")
source("./2025-refactor/utils_get_prism_data.R")
source("./2025-refactor/utils_get_distance.R")
source("./01-b-street-network/utils_dodgr.R")
source("./data_source_exploration/distance/points_to_line.R")
city_year_output_fname <- get_city_year_output_filename(city_string=city_string,year_num=year_num,save_file_type="Rdata")
city_output_fname <- get_city_output_filename(city_string = city_string,save_file_type = "Rdata")

source("./2025-refactor/01_get_crs.R")
source("./2025-refactor/02_get_boundary_maps.R")
source("./2025-refactor/04_get_prism_data.R")