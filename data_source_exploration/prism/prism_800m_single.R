
# https://mgimond.github.io/Spatial/mapping-data-in-r.html

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

repository_path <- "C:/Users/Andrea/Desktop/repositories/real-feel-distance/"
prism_base_path <- "C:/Users/Andrea/Desktop/prism_daily_data/July_2019/July 2019/"
prism_var <- "tmax"

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





{
if (file.exists(paste0(repository_path,"data_source_exploration/maps/get_boundary_maps.RData"))){
  
  load(paste0(repository_path,"data_source_exploration/maps/get_boundary_maps.RData"))
  
} else {
  
  stop("run get_boundary_maps.R script to get boundary maps")
  
}
}

if (!dir.exists(paste0(repository_path,"intermediate_data_products/"))){
  dir.create(paste0(repository_path,"intermediate_data_products/"))
}


#--- the file name of the PRISM data just downloaded ---#
prism_files <- 
  list.files(path = paste0(prism_base_path,prism_var,"/"), pattern = ".bil$",
             full.names = TRUE)
#prism_file <- prism_files[1]

chicago.city.prism_var_by_tract.mean.list <- list()
chicago.city.prism_var_by_tract.aw_mean.list <- list()
chicago.city.2km.buffer.prism_var_by_tract.mean.list <- list()
chicago.city.2km.buffer.prism_var_by_tract.aw_mean.list <- list()

my_vars <- c()
my_vars.list <- list()

for (p in 1:length(prism_files)){
  
  print(paste0("p = ",p))
  
  prism_file <- prism_files[p]
  #--- read in the prism data ---#
  prism_rast <- terra::rast(prism_file)
  
  # only need to do this once
  if (!exists("chicago.city.tracts.sf.lonlat")){
    
    print('converting chicago maps')
    
    
    ##############################################
    # Chicago - census tracts
    ##############################################
    
    # convert utm to prism crs (lonlat)
    chicago.city.tracts.sf.lonlat <- 
      chicago.city.tracts.utm %>%
      sf::st_transform(terra::crs(prism_rast))
    
    chicago.city.2km.buffer.tracts.sf.lonlat <- 
      chicago.city.2km.buffer.tracts.utm %>%
      sf::st_transform(terra::crs(prism_rast))
    
    # convert sf to spatvector
    chicago.city.tracts.spatvect.lonlat <- 
      terra::vect(chicago.city.tracts.sf.lonlat)
    
    chicago.city.2km.buffer.tracts.spatvect.lonlat <- 
      terra::vect(chicago.city.2km.buffer.tracts.sf.lonlat)
   
  }
  
  # crop prism raster file to the chicago city borders
  prism_rast.chicago.city.tracts.lonlat <- 
    terra::crop(prism_rast, chicago.city.tracts.sf.lonlat)
  
  # crop prism raster file to the chicago city + 2km borders
  prism_rast.chicago.city.2km.buffer.tracts.lonlat <- 
    terra::crop(prism_rast, chicago.city.2km.buffer.tracts.sf.lonlat)
  
  print("hello")
  
  #--- extract values from the raster for each tract ---#
  # get all raster values per polygon PLUS the fraction of each raster within polygon 
  # to find an area-weighted summary later
  chicago.city.prism_var_by_tract <- 
    terra::extract(prism_rast.chicago.city.tracts.lonlat, 
                   chicago.city.tracts.spatvect.lonlat, 
                   exact = TRUE) 
  
  chicago.city.2km.buffer.prism_var_by_tract <- 
    terra::extract(prism_rast.chicago.city.2km.buffer.tracts.lonlat, 
                   chicago.city.2km.buffer.tracts.spatvect.lonlat, 
                   exact = TRUE) 
  
  print("hi")
  
  my_var <- sym(names(chicago.city.prism_var_by_tract)[2])
  my_vars <- c(my_vars,names(chicago.city.prism_var_by_tract)[2])
  my_vars.list[[p]] <- names(chicago.city.prism_var_by_tract)[2]
  
  chicago.city.prism_var_by_tract.mean <- 
    chicago.city.prism_var_by_tract %>%
    group_by(ID) %>%
    summarize(prism_var = mean(!!my_var),
              aw_prism_var = sum(fraction * !!my_var)/sum(fraction))
  
  chicago.city.prism_var_by_tract.mean.list[[p]] <- chicago.city.prism_var_by_tract.mean$prism_var
  chicago.city.prism_var_by_tract.aw_mean.list[[p]] <- chicago.city.prism_var_by_tract.mean$aw_prism_var
  
  chicago.city.2km.buffer.prism_var_by_tract.mean <- 
    chicago.city.2km.buffer.prism_var_by_tract %>%
    group_by(ID) %>%
    summarize(prism_var = mean(!!my_var),
              aw_prism_var = sum(fraction * !!my_var)/sum(fraction))
  
  chicago.city.2km.buffer.prism_var_by_tract.mean.list[[p]] <- chicago.city.2km.buffer.prism_var_by_tract.mean$prism_var
  chicago.city.2km.buffer.prism_var_by_tract.aw_mean.list[[p]] <- chicago.city.2km.buffer.prism_var_by_tract.mean$aw_prism_var
  
 
}

chicago.city.prism_var_by_tract.mean.df <- as.data.frame(do.call(cbind, chicago.city.prism_var_by_tract.mean.list))
chicago.city.prism_var_by_tract.aw_mean.df <- as.data.frame(do.call(cbind, chicago.city.prism_var_by_tract.aw_mean.list))
chicago.city.2km.buffer.prism_var_by_tract.mean.df <- as.data.frame(do.call(cbind, chicago.city.2km.buffer.prism_var_by_tract.mean.list))
chicago.city.2km.buffer.prism_var_by_tract.aw_mean.df <- as.data.frame(do.call(cbind, chicago.city.2km.buffer.prism_var_by_tract.aw_mean.list))

my_vars_clean <- sapply(strsplit(my_vars, split= "_", fixed = TRUE), tail, 1L)

names(chicago.city.prism_var_by_tract.mean.df) <- my_vars_clean
names(chicago.city.prism_var_by_tract.aw_mean.df) <- my_vars_clean
names(chicago.city.2km.buffer.prism_var_by_tract.mean.df) <- my_vars_clean
names(chicago.city.2km.buffer.prism_var_by_tract.aw_mean.df) <- my_vars_clean

chicago.city.prism_var_by_tract.mean.summary.df <-
  gather(chicago.city.prism_var_by_tract.mean.df) %>%
  group_by(key) %>%
  summarize(across(.fns = list(mean=mean,min=min,max=max))) %>%
  mutate(max_f = weathermetrics::celsius.to.fahrenheit(value_max),
         min_f = weathermetrics::celsius.to.fahrenheit(value_min),
         range_f = max_f - min_f)

chicago.city.prism_var_by_tract.aw_mean.summary.df <-
  gather(chicago.city.prism_var_by_tract.aw_mean.df) %>%
  group_by(key) %>%
  summarize(across(.fns = list(mean=mean,min=min,max=max))) %>%
  mutate(max_f = weathermetrics::celsius.to.fahrenheit(value_max),
         min_f = weathermetrics::celsius.to.fahrenheit(value_min),
         range_f = max_f - min_f)

write.csv(chicago.city.prism_var_by_tract.aw_mean.df,
          paste0(repository_path,"intermediate_data_products/chicago.city.",prism_var,"_by_tract.aw_mean.csv"))

write.csv(chicago.city.2km.buffer.prism_var_by_tract.aw_mean.df,
          paste0(repository_path,"intermediate_data_products/chicago.city.2km.buffer.",prism_var,"_by_tract.aw_mean.csv"))

write.csv(chicago.city.prism_var_by_tract.aw_mean.summary.df,
          paste0(repository_path,"intermediate_data_products/chicago.city.",prism_var,"_by_tract.aw_mean.summary.csv"))



##################################################################
##################################################################



# IL_Cook_tracts_sf <- 
#   #--- back to sf ---#
#   st_as_sf(IL_Cook_tracts_sv) %>% 
#   #--- define ID ---#
#   mutate(ID := seq_len(nrow(.))) %>% 
#   #--- merge by ID ---#
#   left_join(., mean_tmax, by = "ID") %>%
#   left_join(., tmax_by_tract_from_stack_area_weighted, by = "ID")
# )
# 
# tmap::tm_shape(prism_rast) + 
#   tmap::tm_raster(style = "quantile", n = 12, title = "tmax (C)",
#             palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
#             legend.hist = TRUE)+
#   tmap::tm_legend(outside = TRUE, hist.width = 2)
# 
# 
# 
# tmap::tm_shape(prism_rast.chicago.city.tracts.lonlat) + 
#   tmap::tm_raster(style = "quantile", n = 12, title = "tmax (C)",
#                   palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
#                   legend.hist = TRUE)+
#   tmap::tm_legend(outside = TRUE, hist.width = 2) + 
#   tmap::tm_shape(chicago.city.tracts.sf.lonlat) + 
#   tmap::tm_polygons(alpha = .10)
# 
# tmap::tm_shape(prism_rast.chicago.city.2km.buffer.tracts.lonlat) + 
#   tmap::tm_raster(style = "quantile", n = 12, title = "tmax (C)",
#                   palette = colorRampPalette( c("darkolivegreen4","yellow", "brown"))(12),
#                   legend.hist = TRUE)+
#   tmap::tm_legend(outside = TRUE, hist.width = 2) + 
#   tmap::tm_shape(chicago.city.2km.buffer.tracts.sf.lonlat) + 
#   tmap::tm_polygons(border.col = "blue", alpha = .10) +
#   tmap::tm_shape(chicago.city.tracts.sf.lonlat) + 
#   tmap::tm_polygons(alpha = .10)
