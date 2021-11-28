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

prism_set_dl_dir("C:/Users/tentner-andrea/Desktop/distance to food/prism_data/")

#--- download PRISM precipitation data ---#
get_prism_dailys(
  type = "tmax", 
  date = "2018-07-01", 
  keepZip = FALSE 
)

#--- the file name of the PRISM data just downloaded ---#
prism_file <- "C:/Users/tentner-andrea/Desktop/distance to food/prism_data/PRISM_tmax_stable_4kmD2_20180701_bil/PRISM_tmax_stable_4kmD2_20180701_bil.bil"


#--- read in the prism data ---#
prism_tmax_0701_sr <- rast(prism_file) 

#--- Kansas boundary (sf) ---#
KS_county_sf <- 
  #--- get Kansas county boundary ---
  tigris::counties(state = "Kansas", cb = TRUE) %>% 
  #--- sp to sf ---#
  st_as_sf() %>% 
  #--- transform using the CRS of the PRISM tmax data  ---#
  st_transform(terra::crs(prism_tmax_0701_sr)) 

# crop prism raster file to the KS borders
prism_tmax_0701_KS_sr <- terra::crop(prism_tmax_0701_sr, KS_county_sf)

### repeat process done above for 7/1/2018 also now for 7/2/2018
#--- download PRISM precipitation data ---#
get_prism_dailys(
  type = "tmax", 
  date = "2018-07-02", 
  keepZip = FALSE 
)

#--- the file name of the PRISM data just downloaded ---#
prism_file <- "C:/Users/tentner-andrea/Desktop/distance to food/prism_data/PRISM_tmax_stable_4kmD2_20180702_bil/PRISM_tmax_stable_4kmD2_20180702_bil.bil"

#--- read in the prism data and crop it to Kansas state border ---#
prism_tmax_0702_KS_sr <- rast(prism_file) %>% 
  terra::crop(KS_county_sf)

#--- Kansas boundary (convert sf to SpatVector) ---#
KS_county_sv <- vect(KS_county_sf)

#--- extract values from the raster for each county ---#
tmax_by_county <- terra::extract(prism_tmax_0701_KS_sr, KS_county_sv) 

#--- get mean tmax per county ---#
mean_tmax <- tmax_by_county %>% 
  group_by(ID) %>% 
  summarize(tmax = mean(PRISM_tmax_stable_4kmD2_20180701_bil))

(
  KS_county_sf <- 
    #--- back to sf ---#
    st_as_sf(KS_county_sv) %>% 
    #--- define ID ---#
    mutate(ID := seq_len(nrow(.))) %>% 
    #--- merge by ID ---#
    left_join(., mean_tmax, by = "ID")
)

plot(KS_county_sf %>% select(tmax,geometry))

# extract data from a stack of raster data 
#--- create a multi-layer SpatRaster ---#
prism_tmax_stack <- c(prism_tmax_0701_KS_sr, prism_tmax_0702_KS_sr)

#--- extract from a multi-layer raster object ---#
# get all raster values per polygon
tmax_by_county_from_stack <- 
  terra::extract(
    prism_tmax_stack, 
    KS_county_sv
  ) 

# get the mean of all raster values per polygon directly using the fun parameter
tmax_by_county_from_stack_mean <- 
  terra::extract(
    prism_tmax_stack, 
    KS_county_sv,
    fun = mean
  ) 

# get all raster values per polygon PLUS the fraction of each raster within polygon 
# to find an area-weighted summary later
tmax_by_county_from_stack_exact <- 
  terra::extract(
    prism_tmax_stack, 
    KS_county_sv,
    exact = TRUE
  ) 

#--- take a look ---#
head(tmax_by_county_from_stack)
head(tmax_by_county_from_stack_mean)
head(tmax_by_county_from_stack_exact)

# calculate area-weighted summary
tmax_by_county_from_stack_area_weighted <- 
  tmax_by_county_from_stack_exact %>% 
  group_by(ID) %>% 
  summarize(
    tmax_0701 = sum(fraction * PRISM_tmax_stable_4kmD2_20180701_bil) / sum(fraction),
    tmax_0702 = sum(fraction * PRISM_tmax_stable_4kmD2_20180702_bil) / sum(fraction)
  )


##############################################
# Chicago - census tracts
##############################################

#--- download PRISM tmax data ---#
get_prism_monthlys(
  type = "tmax", 
  years = 2019,
  mon = c(7,8,9),
  #resolution = "800m",
  keepZip = FALSE 
)

#--- the file name of the PRISM data just downloaded ---#
prism_file_1 <- "C:/Users/tentner-andrea/Desktop/distance to food/prism_data/PRISM_tmax_stable_4kmM3_201907_bil/PRISM_tmax_stable_4kmM3_201907_bil.bil"
prism_file_2 <- "C:/Users/tentner-andrea/Desktop/distance to food/prism_data/PRISM_tmax_stable_4kmM3_201908_bil/PRISM_tmax_stable_4kmM3_201908_bil.bil"
prism_file_3 <- "C:/Users/tentner-andrea/Desktop/distance to food/prism_data/PRISM_tmax_stable_4kmM3_201909_bil/PRISM_tmax_stable_4kmM3_201909_bil.bil"

#--- read in the prism data ---#
prism_tmax_201907_sr <- rast(prism_file_1)
prism_tmax_201908_sr <- rast(prism_file_2)
prism_tmax_201909_sr <- rast(prism_file_3)

plot(prism_tmax_201907_sr)

#--- Illinois Cook County boundary (sf) ---#
IL_Cook_tracts_sf <- 
  #--- get Kansas county boundary ---
  tigris::tracts(state = "Illinois", county = "Cook", cb = TRUE) %>% 
  #--- sp to sf ---#
  st_as_sf() %>% 
  #--- transform using the CRS of the PRISM tmax data  ---#
  st_transform(terra::crs(prism_tmax_201907_sr))

plot(IL_Cook_tracts_sf%>%select(geometry))

#####
#####

# crop prism raster file to the Cook county borders
prism_tmax_201907_ILCook_sr <- terra::crop(prism_tmax_201907_sr, IL_Cook_tracts_sf)
prism_tmax_201908_ILCook_sr <- terra::crop(prism_tmax_201908_sr, IL_Cook_tracts_sf)
prism_tmax_201909_ILCook_sr <- terra::crop(prism_tmax_201909_sr, IL_Cook_tracts_sf)
plot(prism_tmax_201907_ILCook_sr)


#--- Cook County boundary (convert sf to SpatVector) ---#
IL_Cook_tracts_sv <- vect(IL_Cook_tracts_sf)

#--- extract values from the raster for each tract ---#
tmax_by_tract <- terra::extract(prism_tmax_201907_ILCook_sr, IL_Cook_tracts_sv) 

#--- get mean tmax per tract ---#
mean_tmax <- tmax_by_tract %>% 
  group_by(ID) %>% 
  summarize(tmax = mean(PRISM_tmax_stable_4kmM3_201907_bil))

(
  IL_Cook_tracts_sf <- 
    #--- back to sf ---#
    st_as_sf(IL_Cook_tracts_sv) %>% 
    #--- define ID ---#
    mutate(ID := seq_len(nrow(.))) %>% 
    #--- merge by ID ---#
    left_join(., mean_tmax, by = "ID")
)

plot(IL_Cook_tracts_sf %>% select(tmax,geometry))

# extract data from a stack of raster data 
#--- create a multi-layer SpatRaster ---#
prism_tmax_stack <- c(prism_tmax_201907_ILCook_sr, prism_tmax_201908_ILCook_sr, prism_tmax_201909_ILCook_sr)

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