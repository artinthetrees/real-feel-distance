
#library(tidyverse)

# prism data:

# Parameter name	Description
# tmean:	Mean temperature
# tmax:	Maximum temperature
# tmin:	Minimum temperature
# tdmean:	Mean dew point temperature
# pp:	Total precipitation (rain and snow)
# vpdmin:	Daily minimum vapor pressure deficit
# vpdmax:	Daily maximum vapor pressure deficit

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra, # handle raster data
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  tidyr, # data wrangling
  data.table, # data wrangling
  prism, # download PRISM data
  tictoc, # timing codes
  tigris, # to get county sf
  tmap # for mapping
)


##############################################
# Chicago - census tracts
##############################################

# #--- download PRISM tmax data ---#
# get_prism_monthlys(
#   type = "tmax", 
#   years = 2019,
#   mon = c(7,8,9),
#   #resolution = "800m",
#   keepZip = FALSE 
# )

#--- the file name of the PRISM data just downloaded ---#
prism_file_1 <- "C:/Users/Andrea/Desktop/prism_daily_data/July_2019/July 2019/tmax/prism_tmax_us_30s_20190701.bil"




#--- read in the prism data ---#
prism_rast <- terra::rast(prism_file_1)
# prism_tmax_201908_sr <- rast(prism_file_2)
# prism_tmax_201909_sr <- rast(prism_file_3)

plot(prism_rast)

#--- Illinois Cook County boundary (sf) ---#
IL_Cook_tracts_sf <- 
  #--- get Kansas county boundary ---
  tigris::tracts(state = "Illinois", county = "Cook", cb = TRUE) %>% 
  #--- sp to sf ---#
  sf::st_as_sf() %>% 
  #--- transform using the CRS of the PRISM tmax data  ---#
  sf::st_transform(terra::crs(prism_rast))

plot(IL_Cook_tracts_sf%>%select(geometry))

#####
#####

# crop prism raster file to the Cook county borders
prism_IL_Cook_tracts_rast <- terra::crop(prism_rast, IL_Cook_tracts_sf)
# prism_tmax_201908_ILCook_sr <- terra::crop(prism_tmax_201908_sr, IL_Cook_tracts_sf)
# prism_tmax_201909_ILCook_sr <- terra::crop(prism_tmax_201909_sr, IL_Cook_tracts_sf)
plot(prism_IL_Cook_tracts_rast)


#--- Cook County boundary (convert sf to SpatVector) ---#
IL_Cook_tracts_sv <- terra::vect(IL_Cook_tracts_sf)

#--- extract values from the raster for each tract ---#
tmax_by_tract <- terra::extract(prism_rast, IL_Cook_tracts_sv) 

#--- get mean tmax per tract ---#
mean_tmax <- tmax_by_tract %>% 
  group_by(ID) %>% 
  summarize_all(mean)

(
  IL_Cook_tracts_sf <- 
    #--- back to sf ---#
    st_as_sf(IL_Cook_tracts_sv) %>% 
    #--- define ID ---#
    mutate(ID := seq_len(nrow(.))) %>% 
    #--- merge by ID ---#
    left_join(., mean_tmax, by = "ID")
)

plot(IL_Cook_tracts_sf %>% select(prism_tmax_us_30s_20190701,geometry))

# https://mgimond.github.io/Spatial/mapping-data-in-r.html

tm_shape(prism_rast) + 
  tm_raster(style = "quantile", n = 12, title = "tmax (C)",
            palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
            legend.hist = TRUE)+
  tm_legend(outside = TRUE, hist.width = 2)

# extract data from a stack of raster data 
#--- create a multi-layer SpatRaster ---#
#prism_tmax_stack <- c(prism_tmax_201907_ILCook_sr, prism_tmax_201908_ILCook_sr, prism_tmax_201909_ILCook_sr)
prism_files <- 
  list.files(path = "C:/Users/Andrea/Desktop/prism_daily_data/July_2019/July 2019/tmax/", pattern = ".bil$", all.files = FALSE,
             full.names = TRUE, recursive = FALSE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#--- extract from a multi-layer raster object ---#
# get all raster values per polygon
tmax_by_tract_from_stack <- 
  terra::extract(
    prism_tmax_stack, 
    IL_Cook_tracts_sv
  ) 

# get the mean of all raster values per polygon directly using the fun parameter
tmax_by_tract_from_stack_mean <- 
  terra::extract(
    prism_tmax_stack, 
    IL_Cook_tracts_sv,
    fun = mean
  ) 

# get all raster values per polygon PLUS the fraction of each raster within polygon 
# to find an area-weighted summary later
tmax_by_tract_from_stack_exact <- 
  terra::extract(
    prism_tmax_stack, 
    IL_Cook_tracts_sv,
    exact = TRUE
  ) 

#--- take a look ---#
head(tmax_by_tract_from_stack)
head(tmax_by_tract_from_stack_mean)
head(tmax_by_tract_from_stack_exact)

# calculate area-weighted summary
tmax_by_tract_from_stack_area_weighted <- 
  tmax_by_tract_from_stack_exact %>% 
  group_by(ID) %>% 
  summarize(
    tmax_201907 = sum(fraction * PRISM_tmax_stable_4kmM3_201907_bil) / sum(fraction),
    tmax_201908 = sum(fraction * PRISM_tmax_stable_4kmM3_201908_bil) / sum(fraction),
    tmax_201909 = sum(fraction * PRISM_tmax_stable_4kmM3_201909_bil) / sum(fraction),
  )

(
  IL_Cook_tracts_sf <- 
    #--- back to sf ---#
    st_as_sf(IL_Cook_tracts_sv) %>% 
    #--- define ID ---#
    mutate(ID := seq_len(nrow(.))) %>% 
    #--- merge by ID ---#
    left_join(., mean_tmax, by = "ID") %>%
    left_join(., tmax_by_tract_from_stack_area_weighted, by = "ID")
)

plot(IL_Cook_tracts_sf %>% select(tmax_201907, tmax_201908, tmax_201909, geometry))