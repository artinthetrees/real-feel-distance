# https://crd230.github.io/lab4a.html
# https://mran.microsoft.com/snapshot/2016-06-30/web/packages/tmap/vignettes/tmap-nutshell.html
# https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#distance-and-proximity-analysis
# https://github.com/r-tmap/tmap/issues/511


library(tidyverse)

# v19 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)
# View(v19)

options(tigris_use_cache = TRUE)

crs_lonlat <- "+proj=longlat +datum=NAD83"
crs_utm <- "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80"

# Bring in census tract data. 
il.tracts.utm <- 
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
                      geometry = TRUE) %>%
  sf::st_transform(crs = crs_utm)

# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
il.tracts.utm <- 
  il.tracts.utm %>% 
  dplyr::rename(Name = NAME) %>%
  dplyr::rename_with(~ sub("E$", "", .x), everything()) %>%
  dplyr::mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr, 
                pnhblk = nhblk/tpopr, phisp = hisp/tpopr) %>%
  dplyr::select(c(GEOID,Name,tpop, pnhwhite, pnhasn, pnhblk, phisp))  

# Bring in city boundary data
pl <- 
  tigris::places(state = "IL", cb = TRUE)

# Keep Chicago - get boundary for chicago
chicago.city.utm <- 
  dplyr::filter(pl, NAME == "Chicago") %>%
  sf::st_transform(crs = crs_utm)
  
# Get boundary for 2km buffer around chicago
chicago.city.2km.buffer.utm <- 
  sf::st_buffer(chicago.city.utm, dist = 2000)

# Clip IL tracts using Chicago boundary - get boundary for chicago + boundary for each census tract within chicago
chicago.city.tracts.utm <- 
  rmapshaper::ms_clip(target = il.tracts.utm, clip = chicago.city.utm, remove_slivers = TRUE)

# Clip IL tracts using Chicago + 2km buffer boundary - get boundary for chicago + 2km buffer + boundary for each census tract within chicago + 2km buffer
chicago.city.2km.buffer.tracts.utm <- 
  rmapshaper::ms_clip(target = il.tracts.utm, clip = chicago.city.2km.buffer.utm, remove_slivers = TRUE)

# plot chicago with clipped tract boundaries (only show part of tract within chicago city limits)
tmap::tm_shape(chicago.city.tracts.utm) +
  tmap::tm_polygons() 

# plot chicago with clipped tract boundaries (only show part of tract within chicago city limits)
# plus 2km buffer around chicago with tracts
tmap::tm_shape(chicago.city.2km.buffer.tracts.utm) +
  tmap::tm_polygons(col="red", alpha = .5) +
tmap::tm_shape(chicago.city.tracts.utm) +
  tmap::tm_polygons() 
  
cook.county.utm <- 
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
                      geometry = TRUE) %>%
  sf::st_transform(crs = crs_utm)

# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
cook.county.utm <- 
  cook.county.utm %>% 
  dplyr::rename(Name = NAME) %>%
  dplyr::rename_with(~ sub("E$", "", .x), everything()) %>%
  dplyr::mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr, 
                pnhblk = nhblk/tpopr, phisp = hisp/tpopr) %>%
  dplyr::select(c(GEOID,Name,tpop, pnhwhite, pnhasn, pnhblk, phisp)) 

# Clip IL tracts using Cook county boundary - get boundary for cook county + boundary for each census tract within cook county
cook.county.tracts.utm <- 
  rmapshaper::ms_clip(target = il.tracts.utm, clip = cook.county.utm, remove_slivers = TRUE)


# plot boundary of cook county
# inside, show chicago with clipped tract boundaries (only show part of tract within chicago city limits)
cook_and_chicago_tracts_map <- 
tmap::tm_shape(cook.county.tracts.utm) + 
  tmap::tm_polygons(col = "red", alpha = 0.25) +
  tmap::tm_shape(chicago.city.2km.buffer.tracts.utm) +
  tmap::tm_polygons(col = "red", alpha = 0.5) +
  tmap::tm_shape(chicago.city.tracts.utm) +
  tmap::tm_polygons()

tmap::tmap_save(cook_and_chicago_tracts_map,
                "C:/Users/Andrea/Desktop/repositories/real-feel-distance/data_source_exploration/maps/cook_and_chicago_tracts_map.png",
                units = "in",
                width = 4,
                height = 6)

# add area in m^2 of each chicago census tract to chicago census tracts spatial file
chicago.city.tracts.utm$area <- 
  sf::st_area(chicago.city.tracts.utm)

# convert utm to prism crs (lonlat)
chicago.city.tracts.sf.lonlat <- 
  chicago.city.tracts.utm %>%
  sf::st_transform(crs = crs_lonlat)

chicago.city.2km.buffer.tracts.sf.lonlat <- 
  chicago.city.2km.buffer.tracts.utm %>%
  sf::st_transform(crs = crs_lonlat)

# convert sf to spatvector
chicago.city.tracts.spatvect.lonlat <- 
  terra::vect(chicago.city.tracts.sf.lonlat)

chicago.city.2km.buffer.tracts.spatvect.lonlat <- 
  terra::vect(chicago.city.2km.buffer.tracts.sf.lonlat)

save.image("C:/Users/Andrea/Desktop/repositories/real-feel-distance/data_source_exploration/maps/get_boundary_maps.RData")
