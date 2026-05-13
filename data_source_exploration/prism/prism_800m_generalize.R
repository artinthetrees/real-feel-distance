
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

#repository_path <- "C:/Users/Andrea/Desktop/repositories/real-feel-distance/"
# prism_base_path <- "C:/Users/Andrea/Desktop/prism_daily_data/July_2019/July 2019/"
# prism_var <- "tmax"

sub_path <- paste0(state,"_",county,"_",city,"_",year)

boundary_maps_path <- Gmisc::pathJoin(repository_path,"intermediate-data-products","boundary-maps",sub_path,"get_boundary_maps.RData")
load(boundary_maps_path)

prism_output_dir <- Gmisc::pathJoin(repository_path,"intermediate-data-products","prism-data")
ifelse(!dir.exists(prism_output_dir), dir.create(prism_output_dir), FALSE)

prism_output_sub_dir <- Gmisc::pathJoin(prism_output_dir,sub_path)
ifelse(!dir.exists(prism_output_sub_dir), dir.create(prism_output_sub_dir), FALSE)

prism_output_path <- Gmisc::pathJoin(prism_output_sub_dir,"get_prism_data.RData")






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


prism_base_path <- "P:/LABO.10.32/Common/prism-climate-data/"
print(prism_base_path)
dir1 <- list.dirs(path=prism_base_path, recursive = FALSE)
print(dir1)

dir2 <- dir1[ grepl(prism_var, basename(dir1)) ]
print(dir2)

finalDir <- Gmisc::pathJoin(dir2,prism_var,"daily",year)
print(finalDir)

prism_files <- 
  list.files(path = finalDir, 
             pattern = ".bil$",
             full.names = TRUE)
print(prism_files)

prism_files <- prism_files[c(1:10)]



# {
# if (file.exists(paste0(repository_path,"data_source_exploration/maps/get_boundary_maps.RData"))){
#   
#   load(paste0(repository_path,"data_source_exploration/maps/get_boundary_maps.RData"))
#   
# } else {
#   
#   stop("run get_boundary_maps.R script to get boundary maps")
#   
# }
# }
# 
# if (!dir.exists(paste0(repository_path,"intermediate_data_products/"))){
#   dir.create(paste0(repository_path,"intermediate_data_products/"))
# }


# #--- the file name of the PRISM data just downloaded ---#
# prism_files <- 
#   list.files(path = paste0(prism_base_path,prism_var,"/"), pattern = ".bil$",
#              full.names = TRUE)
# #prism_file <- prism_files[1]

city.prism_var_by_tract.mean.list <- list()
city.prism_var_by_tract.aw_mean.list <- list()
city.2km.buffer.prism_var_by_tract.mean.list <- list()
city.2km.buffer.prism_var_by_tract.aw_mean.list <- list()

my_vars <- c()
my_vars.list <- list()

for (p in 1:length(prism_files)){
  
  print(paste0("p = ",p))
  
  prism_file <- prism_files[p]
  print(prism_file)
  #--- read in the prism data ---#
  prism_rast <- terra::rast(prism_file)
  
  # only need to do this once
  if (!exists("city.tracts.sf.lonlat")){
    
    print('converting city maps')
    
    
    ##############################################
    # City - census tracts
    ##############################################
    
    # convert utm to prism crs (lonlat)
    city.tracts.sf.lonlat <- 
      city.tracts.utm %>%
      sf::st_transform(terra::crs(prism_rast))
    
    city.2km.buffer.tracts.sf.lonlat <- 
      city.2km.buffer.tracts.utm %>%
      sf::st_transform(terra::crs(prism_rast))
    
    # convert sf to spatvector
    city.tracts.spatvect.lonlat <- 
      terra::vect(city.tracts.sf.lonlat)
    
    city.2km.buffer.tracts.spatvect.lonlat <- 
      terra::vect(city.2km.buffer.tracts.sf.lonlat)
   
  }
  
  # crop prism raster file to the city borders
  prism_rast.city.tracts.lonlat <- 
    terra::crop(prism_rast, city.tracts.sf.lonlat)
  
  # crop prism raster file to the city + 2km borders
  prism_rast.city.2km.buffer.tracts.lonlat <- 
    terra::crop(prism_rast, city.2km.buffer.tracts.sf.lonlat)
  
  print("hello")
  
  #--- extract values from the raster for each tract ---#
  # get all raster values per polygon PLUS the fraction of each raster within polygon 
  # to find an area-weighted summary later
  city.prism_var_by_tract <- 
    terra::extract(prism_rast.city.tracts.lonlat, 
                   city.tracts.spatvect.lonlat, 
                   exact = TRUE) 
  
  city.2km.buffer.prism_var_by_tract <- 
    terra::extract(prism_rast.city.2km.buffer.tracts.lonlat, 
                   city.2km.buffer.tracts.spatvect.lonlat, 
                   exact = TRUE) 
  
  print("hi")
  
  my_var <- sym(names(city.prism_var_by_tract)[2])
  my_vars <- c(my_vars,names(city.prism_var_by_tract)[2])
  my_vars.list[[p]] <- names(city.prism_var_by_tract)[2]
  
  city.prism_var_by_tract.mean <- 
    city.prism_var_by_tract %>%
    group_by(ID) %>%
    summarize(prism_var = mean(!!my_var),
              aw_prism_var = sum(fraction * !!my_var)/sum(fraction))
  
  city.prism_var_by_tract.mean.list[[p]] <- city.prism_var_by_tract.mean$prism_var
  city.prism_var_by_tract.aw_mean.list[[p]] <- city.prism_var_by_tract.mean$aw_prism_var
  
  city.2km.buffer.prism_var_by_tract.mean <- 
    city.2km.buffer.prism_var_by_tract %>%
    group_by(ID) %>%
    summarize(prism_var = mean(!!my_var),
              aw_prism_var = sum(fraction * !!my_var)/sum(fraction))
  
  city.2km.buffer.prism_var_by_tract.mean.list[[p]] <- city.2km.buffer.prism_var_by_tract.mean$prism_var
  city.2km.buffer.prism_var_by_tract.aw_mean.list[[p]] <- city.2km.buffer.prism_var_by_tract.mean$aw_prism_var
  
 
}

city.prism_var_by_tract.mean.df <- as.data.frame(do.call(cbind, city.prism_var_by_tract.mean.list))
city.prism_var_by_tract.aw_mean.df <- as.data.frame(do.call(cbind, city.prism_var_by_tract.aw_mean.list))
city.2km.buffer.prism_var_by_tract.mean.df <- as.data.frame(do.call(cbind, city.2km.buffer.prism_var_by_tract.mean.list))
city.2km.buffer.prism_var_by_tract.aw_mean.df <- as.data.frame(do.call(cbind, city.2km.buffer.prism_var_by_tract.aw_mean.list))

my_vars_clean <- sapply(strsplit(my_vars, split= "_", fixed = TRUE), tail, 1L)

names(city.prism_var_by_tract.mean.df) <- my_vars_clean
names(city.prism_var_by_tract.aw_mean.df) <- my_vars_clean
names(city.2km.buffer.prism_var_by_tract.mean.df) <- my_vars_clean
names(city.2km.buffer.prism_var_by_tract.aw_mean.df) <- my_vars_clean

city.prism_var_by_tract.mean.summary.df <-
  gather(city.prism_var_by_tract.mean.df) %>%
  group_by(key) %>%
  summarize(across(.fns = list(mean=mean,min=min,max=max))) %>%
  mutate(max_f = weathermetrics::celsius.to.fahrenheit(value_max),
         min_f = weathermetrics::celsius.to.fahrenheit(value_min),
         range_f = max_f - min_f)

city.prism_var_by_tract.aw_mean.summary.df <-
  gather(city.prism_var_by_tract.aw_mean.df) %>%
  group_by(key) %>%
  summarize(across(.fns = list(mean=mean,min=min,max=max))) %>%
  mutate(max_f = weathermetrics::celsius.to.fahrenheit(value_max),
         min_f = weathermetrics::celsius.to.fahrenheit(value_min),
         range_f = max_f - min_f)

# write.csv(city.prism_var_by_tract.aw_mean.df,
#           paste0(repository_path,"intermediate_data_products/chicago.city.",prism_var,"_by_tract.aw_mean.csv"))

# write.csv(city.2km.buffer.prism_var_by_tract.aw_mean.df,
#           paste0(repository_path,"intermediate_data_products/chicago.city.2km.buffer.",prism_var,"_by_tract.aw_mean.csv"))

# write.csv(city.prism_var_by_tract.aw_mean.summary.df,
#           paste0(repository_path,"intermediate_data_products/chicago.city.",prism_var,"_by_tract.aw_mean.summary.csv"))

save(city.prism_var_by_tract.aw_mean.df,
     city.2km.buffer.prism_var_by_tract.aw_mean.df,
     city.prism_var_by_tract.aw_mean.summary.df,
    file = prism_output_path)

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

# tmap::tm_shape(prism_rast.chicago.city.tracts.lonlat) + 
#   tmap::tm_raster(style = "cont", title = "Tmax (C)",
#             palette = terrain.colors(64))+
#   tmap::tm_legend(outside = TRUE) +
#   tmap::tm_shape(chicago.city.tracts.sf.lonlat) + 
#   tmap::tm_borders()
