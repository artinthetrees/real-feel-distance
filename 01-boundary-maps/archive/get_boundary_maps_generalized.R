#hello!

# state = "IL"
# county = "Cook"
# city = "Chicago"
# year = 2019

state = "OR"
county = "Multnomah"
city = "Portland"
year = 2019


repository_path <- "C:/Users/tentner-andrea/project_repositories/real-feel-distance/"

crs_lonlat <- "+proj=longlat +datum=NAD83"
crs_utm <- "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80"


# https://crd230.github.io/lab4a.html
# https://mran.microsoft.com/snapshot/2016-06-30/web/packages/tmap/vignettes/tmap-nutshell.html
# https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#distance-and-proximity-analysis
# https://github.com/r-tmap/tmap/issues/511



library(dplyr)
options(tigris_use_cache = TRUE)



# Bring in census tract data. 
state.tracts.utm <- 
  tidycensus::get_acs(geography = "tract", 
                      year = year,
                      variables = c(tpop = "B01003_001",
                                    
                                    # older adult here is over 65
                                    tpop_sex_age = "B01001_001",
                                    tpop_sex_age_male = "B01001_002",
                                    tpop_sex_age_male_older_adult_1 = "B01001_020",
                                    tpop_sex_age_male_older_adult_2 = "B01001_021",
                                    tpop_sex_age_male_older_adult_3 = "B01001_022",
                                    tpop_sex_age_male_older_adult_4 = "B01001_023",
                                    tpop_sex_age_male_older_adult_5 = "B01001_024",
                                    tpop_sex_age_male_older_adult_6 = "B01001_025",
                                    tpop_sex_age_female = "B01001_026",
                                    tpop_sex_age_female_older_adult_1 = "B01001_044",
                                    tpop_sex_age_female_older_adult_2 = "B01001_045",
                                    tpop_sex_age_female_older_adult_3 = "B01001_046",
                                    tpop_sex_age_female_older_adult_4 = "B01001_047",
                                    tpop_sex_age_female_older_adult_5 = "B01001_048",
                                    tpop_sex_age_female_older_adult_6 = "B01001_049",
                                    
                                    # older adult here is over 65
                                    tpop_sex_age_white = "B01001H_001",
                                    tpop_sex_age_white_male = "B01001H_002",
                                    tpop_sex_age_white_male_older_adult_1 = "B01001H_014",
                                    tpop_sex_age_white_male_older_adult_2 = "B01001H_015",
                                    tpop_sex_age_white_male_older_adult_3 = "B01001H_016",
                                    tpop_sex_age_white_female = "B01001H_017",
                                    tpop_sex_age_white_female_older_adult_1 = "B01001H_029",
                                    tpop_sex_age_white_female_older_adult_2 = "B01001H_030",
                                    tpop_sex_age_white_female_older_adult_3 = "B01001H_031",
                                    
                                    # older adult here is over 65
                                    tpop_age_transport_to_work = "B08101_001",
                                    tpop_age_transport_to_work_walk = "B08101_033",
                                    tpop_age_transport_to_work_older_adult = "B08101_008",
                                    tpop_age_transport_to_work_walk_older_adult = "B08101_040",
                                    
                                    # older adult here is over 65
                                    tpop_age_living_arrange = "B09021_001",
                                    tpop_age_living_arrange_live_alone = "B09021_002",
                                    tpop_age_living_arrange_older_adult = "B09021_022",
                                    tpop_age_living_arrange_live_alone_older_adult = "B09021_023",
                                    
                                    # older adult here is over 65
                                    tpop_sex_age_poverty = "B17001_001",
                                    tpop_sex_age_poverty_below_poverty = "B17001_002",
                                    tpop_sex_age_poverty_below_poverty_male = "B17001_003",
                                    tpop_sex_age_poverty_below_poverty_male_older_adult_1 = "B17001_015",
                                    tpop_sex_age_poverty_below_poverty_male_older_adult_2 = "B17001_016",
                                    tpop_sex_age_poverty_at_or_above_poverty_male_older_adult_1 = "B17001_044",
                                    tpop_sex_age_poverty_at_or_above_poverty_male_older_adult_2 = "B17001_045",
                                    tpop_sex_age_poverty_below_poverty_female = "B17001_017",
                                    tpop_sex_age_poverty_below_poverty_female_older_adult_1 = "B17001_029",
                                    tpop_sex_age_poverty_below_poverty_female_older_adult_2 = "B17001_030",
                                    tpop_sex_age_poverty_at_or_above_poverty_female_older_adult_1 = "B17001_058",
                                    tpop_sex_age_poverty_at_or_above_poverty_female_older_adult_2 = "B17001_059",
                                    
                                    # older adult here is over 60
                                    tpop_age_poverty = "B17020_001",
                                    tpop_age_poverty_below_poverty = "B17020_002",
                                    tpop_age_poverty_below_poverty_older_adult_1 = "B17020_007",
                                    tpop_age_poverty_below_poverty_older_adult_2 = "B17020_008",
                                    tpop_age_poverty_below_poverty_older_adult_3 = "B17020_009",
                                    
                                    tpop_race = "B03002_001",
                                    tpop_race_nonhisp_white = "B03002_003", 
                                    tpop_race_nonhisp_blk = "B03002_004",
                                    tpop_race_nonhisp_asn = "B03002_006", 
                                    tpop_race_hisp = "B03002_012"),
                      state = state,
                      survey = "acs5",
                      output = "wide",
                      geometry = TRUE) %>%
  sf::st_transform(crs = crs_utm)

# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
state.tracts.utm <-
  state.tracts.utm %>% 
  # get rid of margin of error vars for now; just keep estimate vars
  select(!ends_with("M")) %>%
  #select(tpopE, tpop_raceE, tpop_sex_ageE, tpop_age_living_arrangeE, tpop_sex_age_povertyE, tpop_age_povertyE, tpop_age_transport_to_workE)
  dplyr::rename(Name = NAME) %>%
  dplyr::rename_with(~ sub("E$", "", .x), everything()) %>%
  rowwise() %>%
  mutate(tpop_male_older_adult = sum(c_across(starts_with("tpop_sex_age_male_older_adult_"))),
         tpop_female_older_adult = sum(c_across(starts_with("tpop_sex_age_female_older_adult_"))),
         
         tpop_white_male_older_adult = sum(c_across(starts_with("tpop_sex_age_white_male_older_adult_"))),
         tpop_white_female_older_adult = sum(c_across(starts_with("tpop_sex_age_white_female_older_adult_"))),
         
         tpop_male_older_adult_below_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_below_poverty_male_older_adult_"))),
         tpop_female_older_adult_below_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_below_poverty_female_older_adult_"))),
         tpop_male_older_adult_at_or_above_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_at_or_above_poverty_male_older_adult_"))),
         tpop_female_older_adult_at_or_above_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_at_or_above_poverty_female_older_adult_"))),
         
         ) %>%
  ungroup() %>%
  mutate(tpop_older_adult = tpop_male_older_adult + tpop_female_older_adult,
         tpop_older_adult_white = tpop_white_male_older_adult + tpop_white_female_older_adult,
         tpop_older_adult_nonwhite = tpop_older_adult - tpop_older_adult_white,
         
         tpop_older_adult_below_poverty = tpop_male_older_adult_below_poverty + tpop_female_older_adult_below_poverty,
         tpop_older_adult_at_or_above_poverty = tpop_male_older_adult_at_or_above_poverty + tpop_female_older_adult_at_or_above_poverty,
         tpop_older_adult_poverty_reported = tpop_older_adult_below_poverty + tpop_older_adult_at_or_above_poverty,
         
         tpop_older_adult_walk_to_work = tpop_age_transport_to_work_walk_older_adult,
         tpop_older_adult_transport_to_work_reported = tpop_age_transport_to_work_older_adult,
         
         tpop_older_adult_live_alone = tpop_age_living_arrange_live_alone_older_adult,
         tpop_older_adult_living_arrange_reported = tpop_age_living_arrange_older_adult,
         
         
         p_older_adult = tpop_older_adult/tpop_sex_age, # percent of census tract pop that is 65+
         p_older_adult_nonwhite = tpop_older_adult_nonwhite/tpop_older_adult, # percent of older adults that are nonwhite (among those who report race)
         p_older_adult_walk_to_work = tpop_age_transport_to_work_walk_older_adult/tpop_age_transport_to_work_older_adult, # percent of older adults who walk to work (among those who report going to work)
         p_older_adult_live_alone = tpop_age_living_arrange_live_alone_older_adult/tpop_age_living_arrange_older_adult, # percent of older adults who live alone (among those who report a living arrangement)
         p_older_adult_below_poverty = tpop_older_adult_below_poverty/(tpop_older_adult_below_poverty + tpop_older_adult_at_or_above_poverty), # percent of older adults in poverty (among those who report on income)
         
         p_nonhispanic_white = tpop_race_nonhisp_white/tpop_race,
         p_nonhispanic_asn = tpop_race_nonhisp_asn/tpop_race, 
         p_nonhispanic_blk = tpop_race_nonhisp_blk/tpop_race, 
         p_hisp = tpop_race_hisp/tpop_race
         
  ) %>%
  select(GEOID,
         Name,
         
         tpop, 
         tpop_race, 
         tpop_sex_age,
         tpop_age_living_arrange, 
         tpop_sex_age_poverty, 
         tpop_age_poverty, 
         tpop_age_transport_to_work,
         
         tpop_older_adult,
         tpop_older_adult_white,
         tpop_older_adult_nonwhite,
         
         tpop_older_adult_poverty_reported,
         tpop_older_adult_below_poverty,
         tpop_older_adult_at_or_above_poverty,
         
         tpop_older_adult_transport_to_work_reported,
         tpop_older_adult_walk_to_work,
         
         tpop_older_adult_living_arrange_reported,
         tpop_older_adult_live_alone,
         
         p_older_adult,
         p_older_adult_nonwhite,
         p_older_adult_walk_to_work,
         p_older_adult_live_alone,
         p_older_adult_below_poverty,
         
         p_nonhispanic_white,
         p_nonhispanic_asn, 
         p_nonhispanic_blk, 
         p_hisp,
         
         geometry
         
         )



# Bring in city boundary data for the state
pl <- 
  tigris::places(state = state, cb = TRUE)

# Keep just the city of interest - get boundary for city of interest
city.utm <- 
  dplyr::filter(pl, NAME == city) %>%
  sf::st_transform(crs = crs_utm)
  
# Get boundary for 2km buffer around city of interest
city.2km.buffer.utm <- 
  sf::st_buffer(city.utm, dist = 2000)

# Clip state tracts using city of interest boundary - get boundary for city of interest + boundary for each census tract within city of interest
city.tracts.utm <- 
  rmapshaper::ms_clip(target = state.tracts.utm, clip = city.utm, remove_slivers = TRUE)

# Clip state tracts using city + 2km buffer boundary - get boundary for city + 2km buffer + boundary for each census tract within city + 2km buffer
city.2km.buffer.tracts.utm <- 
  rmapshaper::ms_clip(target = state.tracts.utm, clip = city.2km.buffer.utm, remove_slivers = TRUE)

# plot city with clipped tract boundaries (only show part of tract within chicago city limits)
tmap::tm_shape(city.tracts.utm) +
  tmap::tm_polygons() 

# plot city with clipped tract boundaries (only show part of tract within city of interest city limits)
# plus 2km buffer around city with tracts
tmap::tm_shape(city.2km.buffer.tracts.utm) +
  tmap::tm_polygons(col="red", alpha = .5) +
tmap::tm_shape(city.tracts.utm) +
  tmap::tm_polygons() 
  
county.utm <- 
  tidycensus::get_acs(geography = "county", 
                      year = year,
                      variables = c(tpop = "B01003_001", 
                                    tpopr = "B03002_001",
                                    nhwhite = "B03002_003", 
                                    nhblk = "B03002_004",
                                    nhasn = "B03002_006", 
                                    hisp = "B03002_012"),
                      state = state,
                      county = county,
                      survey = "acs5",
                      output = "wide",
                      geometry = TRUE) %>%
  sf::st_transform(crs = crs_utm)

# Make the data tidy, calculate percent race/ethnicity, and keep essential vars.
county.utm <- 
  county.utm %>% 
  dplyr::rename(Name = NAME) %>%
  dplyr::rename_with(~ sub("E$", "", .x), everything()) %>%
  dplyr::mutate(pnhwhite = nhwhite/tpopr, pnhasn = nhasn/tpopr, 
                pnhblk = nhblk/tpopr, phisp = hisp/tpopr) %>%
  dplyr::select(c(GEOID,Name,tpop, pnhwhite, pnhasn, pnhblk, phisp)) 

# Clip state tracts using county boundary - get boundary for county + boundary for each census tract within county
county.tracts.utm <- 
  rmapshaper::ms_clip(target = state.tracts.utm, clip = county.utm, remove_slivers = TRUE)


# plot boundary of county
# inside, show city with clipped tract boundaries (only show part of tract within city limits)
county_and_city_tracts_map <- 
tmap::tm_shape(county.tracts.utm) + 
  tmap::tm_polygons(col = "red", alpha = 0.25) +
  tmap::tm_shape(city.2km.buffer.tracts.utm) +
  tmap::tm_polygons(col = "red", alpha = 0.5) +
  tmap::tm_shape(city.tracts.utm) +
  tmap::tm_polygons()

tmap::tmap_save(county_and_city_tracts_map,
                paste(repository_path, "intermediate-data-products/boundary-maps/county_and_city_tracts_map_",state,"_",county,"_",city,"_",year,".png", sep = ""),
                units = "in",
                width = 4,
                height = 6)

# add area in m^2 of each city census tract to city census tracts spatial file
city.tracts.utm$area <- 
  sf::st_area(city.tracts.utm)

# # convert utm to prism crs (lonlat)
# chicago.city.tracts.sf.lonlat <- 
#   chicago.city.tracts.utm %>%
#   sf::st_transform(crs = crs_lonlat)
# 
# chicago.city.2km.buffer.tracts.sf.lonlat <- 
#   chicago.city.2km.buffer.tracts.utm %>%
#   sf::st_transform(crs = crs_lonlat)
# 
# # convert sf to spatvector
# chicago.city.tracts.spatvect.lonlat <- 
#   terra::vect(chicago.city.tracts.sf.lonlat)
# 
# chicago.city.2km.buffer.tracts.spatvect.lonlat <- 
#   terra::vect(chicago.city.2km.buffer.tracts.sf.lonlat)

save.image(paste0(repository_path,"intermediate-data-products/boundary-maps/get_boundary_maps_",state,"_",county,"_",city,"_",year,".RData"))
