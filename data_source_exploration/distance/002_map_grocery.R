# https://crd230.github.io/lab4a.html
# https://mran.microsoft.com/snapshot/2016-06-30/web/packages/tmap/vignettes/tmap-nutshell.html
# https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#distance-and-proximity-analysis
# https://github.com/r-tmap/tmap/issues/511


# library(sf)
# library(tidyverse)
# library(units)
# library(tmap)
# library(tidycensus)
# library(tigris)
# library(rmapshaper)
# library(tidygeocoder)
# library(leaflet)
# library(osmdata)
# library(conflicted)

library(tidyverse)

# census_api_key("c422eb7d8cbb6fab74fe1c0180711dab008a2251", install = TRUE)
# readRenviron("~/.Renviron")

# v19 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)
# View(v19)

options(tigris_use_cache = TRUE)

crs_lonlat <- "+proj=longlat +datum=NAD83"
crs_utm <- "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80"

# Bring in census tract data. 
il.tracts <- 
  tidycensus::get_acs(geography = "tract", 
                      year = 2019,
                      variables = c(tpop = "B01003_001", 
                                    tpopr = "B03002_001",
                                    nhwhite = "B03002_003", 
                                    nhblk = "B03002_004",
                                    nhasn = "B03002_006", 
                                    hisp = "B03002_012"),
                      state = "IL",
                      survey = "acs5",
                      output = "wide",
                      geometry = TRUE)

# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
il.tracts <- 
  il.tracts %>% 
  dplyr::rename(Name = NAME) %>%
  dplyr::rename_with(~ sub("E$", "", .x), everything()) %>%
  dplyr::mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr, 
                pnhblk = nhblk/tpopr, phisp = hisp/tpopr) %>%
  dplyr::select(c(GEOID,Name,tpop, pnhwhite, pnhasn, pnhblk, phisp))  

# Bring in city boundary data
pl <- 
  tigris::places(state = "IL", cb = TRUE)

# Keep Chicago
chicago.city <- dplyr::filter(pl, NAME == "Chicago")

# convert crs to use utm which measures distance in meters, use zone 16N for chicago
chicago.city.utm <-
  sf::st_transform(chicago.city, 
                   crs = "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80") 

# check the crs and check the units (will be meters for utm projection)
sf::st_crs(chicago.city.utm)$proj4string
sf::st_crs(chicago.city.utm)$units

# Clip tracts using Chicago boundary
chicago.city.tracts <- rmapshaper::ms_clip(target = il.tracts, clip = chicago.city, remove_slivers = TRUE)

# check if crs for grocery long/lat points is equal to crs for the chicago census tract map - it's not
sf::st_crs(grocery.sf) == sf::st_crs(chicago.city.tracts)

# check the crs and check the units (will be null for longlat projection)
sf::st_crs(chicago.city.tracts)$proj4string
sf::st_crs(chicago.city.tracts)$units

# convert crs to use utm which measures distance in meters, use zone 16N for chicago
chicago.city.tracts.utm <-
  sf::st_transform(chicago.city.tracts, 
                   crs = "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80") 

# check the crs and check the units (will be meters for utm projection)
sf::st_crs(chicago.city.tracts.utm)$proj4string
sf::st_crs(chicago.city.tracts.utm)$units

# plot chicago with clipped tract boundaries (only show part of tract within chicago city limits)
# plus grocery stores in chicago and in cook within 1 mile of city limits
tmap::tm_shape(chicago.city.tracts.utm) +
  tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm) +  
  tmap::tm_dots(col="red")

cook.county <- 
  tidycensus::get_acs(geography = "county", 
                      year = 2019,
                      variables = c(tpop = "B01003_001", 
                                    tpopr = "B03002_001",
                                    nhwhite = "B03002_003", 
                                    nhblk = "B03002_004",
                                    nhasn = "B03002_006", 
                                    hisp = "B03002_012"),
                      state = "IL",
                      county = "Cook",
                      survey = "acs5",
                      output = "wide",
                      geometry = TRUE)

# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
cook.county <- 
  cook.county %>% 
  dplyr::rename(Name = NAME) %>%
  dplyr::rename_with(~ sub("E$", "", .x), everything()) %>%
  dplyr::mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr, 
                pnhblk = nhblk/tpopr, phisp = hisp/tpopr) %>%
  dplyr::select(c(GEOID,Name,tpop, pnhwhite, pnhasn, pnhblk, phisp)) 

# convert crs to use utm which measures distance in meters, use zone 16N for chicago
cook.county.utm <-
  sf::st_transform(cook.county, 
                   crs = "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80") 

# check the crs and check the units (will be meters for utm projection)
sf::st_crs(cook.county.utm)$proj4string
sf::st_crs(cook.county.utm)$units

# plot boundary of cook county
# inside, show chicago with clipped tract boundaries (only show part of tract within chicago city limits)
# plus grocery stores in chicago and in cook within 1 mile of city limits
tmap::tm_shape(cook.county.utm) + 
  tmap::tm_polygons() +
  tmap::tm_shape(chicago.city.tracts.utm) +
  tmap::tm_polygons() +
  tmap::tm_shape(grocery.sf.utm) +  
  tmap::tm_dots(col="red")

# add number of grocery stores inside each chicago census tract to chicago census tracts spatial file
chicago.city.tracts.utm$grocery_count <- 
  lengths(sf::st_intersects(chicago.city.tracts.utm, grocery.sf.utm))

# add number of grocery stores within .5, 1, 2, 5km of each chicago census tract to chicago census tracts spatial file
chicago.city.tracts.utm$grocery_count_500m <- 
  lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 500))

chicago.city.tracts.utm$grocery_count_1km <- 
  lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 1000))

chicago.city.tracts.utm$grocery_count_2km <- 
  lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 2000))

chicago.city.tracts.utm$grocery_count_5km <- 
  lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 5000))

chicago.city.tracts.utm$grocery_count_7km <- 
  lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 7000))

chicago.city.tracts.utm$grocery_count_10km <- 
  lengths(sf::st_is_within_distance(chicago.city.tracts.utm, grocery.sf.utm,dist = 10000))

# add area in m^2 of each chicago census tract to chicago census tracts spatial file
chicago.city.tracts.utm$area <- 
  sf::st_area(chicago.city.tracts.utm)
