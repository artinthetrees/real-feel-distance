###########################################
state_string <- "IL"
county_string <- "Cook County"
city_string <- "Chicago"
year_num <- 2021

###########################################
crs_datum <- "WGS84" # other option is NAD83; use WGS84 when using dodgr
###########################################
my_font_size <- 10
options(tigris_use_cache = TRUE)
###########################################
source("./2025-refactor/secrets.R")
source("./2025-refactor/utils.R")
source("./01-b-street-network/utils_dodgr.R")
source("./2025-refactor/01_get_crs.R")

source("./2025-refactor/02_get_boundary_maps.R")
