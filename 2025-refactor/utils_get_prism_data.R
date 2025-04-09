# prism data:

# Parameter name	Description
# tmean:	Mean temperature
# tmax:	Maximum temperature
# tmin:	Minimum temperature
# tdmean:	Mean dew point temperature
# ppt:	Total precipitation (rain and snow)
# vpdmin:	Daily minimum vapor pressure deficit
# vpdmax:	Daily maximum vapor pressure deficit

get_norc_prism_file_path <- function(prism_var,year_num){
  
  data_available_year=c("2019","2020","2021","2022","2023")
  data_available_prism_var=c("tmean","tmax","tmin","tdmean","ppt","vpdmin","vpdmax")
  
  year_str <- as.character(year_num)
  
  if (!year_str %in% data_available_year){
    print("data is available only for the following years: ")
    print(data_available_year)
    return(NULL)
  }
  
  if (!prism_var %in% data_available_prism_var){
    print("data is available only for the following prism variables: ")
    print(data_available_prism_var)
    return(NULL)
  }
  
  if (year_str %in% c("2019","2020")){
    ########################
    #P:\LABO.10.32\Common\prism-climate-data\an81_daily_ppt_tmax_tmin_2019_2020\tmax\daily\2019\prism_tmax_us_30s_20190101
    #an81_daily_ppt_tmax_tmin_2019_2020\tmax\daily\2019\prism_tmax_us_30s_20190101
    ########################
    
    prism_base_path <- "P:/LABO.10.32/Common/prism-climate-data/"
    print(prism_base_path)
    
    dir1 <- list.dirs(path=prism_base_path, recursive = FALSE)
    print(dir1)
    
    dir2 <- dir1[ grepl(prism_var, basename(dir1)) ]
    print(dir2)
    
    finalDir <- Gmisc::pathJoin(dir2,prism_var,"daily",year_str)
    print(finalDir)
    
    
  } else if (year_str %in% c("2021","2022","2023")){
    ########################
    #P:\LABO.10.32\Common\prism-climate-data\PRISM_data2\an\tdmean\daily\2021\prism_tdmean_us_30s_20210101
    #tdmean\daily\2021\prism_tdmean_us_30s_20210101
    ########################
    prism_base_path <- "P:/LABO.10.32/Common/prism-climate-data/PRISM_data2/an"
    print(prism_base_path)
    
    finalDir <- Gmisc::pathJoin(prism_base_path,prism_var,"daily",year_str)
    
  }

  prism_files <- 
    list.files(path = finalDir, 
               pattern = ".bil$",
               full.names = TRUE)
  
  
  return(list(prism_file_dir=finalDir,prism_file_list=prism_files))
  
}

get_prism_crs <- function(prism_file){
  print(prism_file)
  
  #--- read in the prism data ---#
  prism_rast <- terra::rast(prism_file)
  prism_crs <- sf::st_crs(prism_file)
  return(prism_crs)
}

get_prism_per_boundary <- function(prism_file_list,prism_crs,boundary_map,return_type="minimal"){
  
  # convert boundary map to prism crs to ensure compatibility
  prism_crs <- get_prism_crs(prism_file = prism_file_list[1])
  boundary_map <- boundary_map %>% sf::st_transform(crs = prism_crs)
  boundary_map_spatvect <- terra::vect(boundary_map)
  
  prism_rast_boundary_extent.list <- list()
  prism_by_boundary.list <- list()
  prism_by_boundary.meanANDaw_mean.list <- list()
  
  prism_by_boundary.mean.list <- list()
  prism_by_boundary.aw_mean.list <- list()
  my_vars <- c()
  my_vars.list <- list()
  
  for (p in 1:length(prism_files)){
    
    print(paste0("p = ",p))
    prism_file <- prism_files[p]
    print(prism_file)
    
    # read in the prism file as raster
    prism_rast <- terra::rast(prism_file)
    
    # crop prism raster file to the boundary map extent
    prism_rast_boundary_extent <- 
      terra::crop(prism_rast, boundary_map)
    
    #--- extract values from the raster for each tract ---#
    # get all raster values per polygon PLUS the fraction of each raster within polygon 
    # to find an area-weighted summary later
    prism_by_boundary <- 
      terra::extract(prism_rast_boundary_extent, 
                     boundary_map_spatvect, 
                     exact = TRUE) 
    
    my_var <- sym(names(prism_by_boundary)[2])
    my_vars <- c(my_vars,names(prism_by_boundary)[2])
    my_vars.list[[p]] <- names(prism_by_boundary)[2]
    
    prism_by_boundary.mean <- 
      prism_by_boundary %>%
      group_by(ID) %>%
      summarize(
        prism_var = mean(!!my_var),
        aw_prism_var = weighted.mean(!!my_var,fraction)
        #aw_prism_var = sum(fraction * !!my_var)/sum(fraction)
      )
    
    prism_rast_boundary_extent.list[[p]] <- prism_rast_boundary_extent 
    prism_by_boundary.list[[p]] <- prism_by_boundary
    prism_by_boundary.meanANDaw_mean.list[[p]] <- prism_by_boundary.mean
    
    prism_by_boundary.mean.list[[p]] <- prism_by_boundary.mean$prism_var
    prism_by_boundary.aw_mean.list[[p]] <- prism_by_boundary.mean$aw_prism_var
    
  }
  
  prism_by_boundary.mean.df <- as.data.frame(do.call(cbind, prism_by_boundary.mean.list))
  prism_by_boundary.aw_mean.df <- as.data.frame(do.call(cbind, prism_by_boundary.aw_mean.list))
  
  my_vars_clean <- sapply(strsplit(my_vars, split= "_", fixed = TRUE), tail, 1L)
  
  names(prism_by_boundary.mean.df) <- my_vars_clean
  names(prism_by_boundary.aw_mean.df) <- my_vars_clean
  
  if (return_type == "verbose"){
    return(list(
      prism_by_boundary.mean.df=prism_by_boundary.mean.df,
      prism_by_boundary.aw_mean.df=prism_by_boundary.aw_mean.df,
      prism_rast_boundary_extent.list=prism_rast_boundary_extent.list,
      prism_by_boundary.list=prism_by_boundary.list,
      prism_by_boundary.meanANDaw_mean.list=prism_by_boundary.meanANDaw_mean.list,
      prism_by_boundary.mean.list=prism_by_boundary.mean.list,
      prism_by_boundary.aw_mean.list=prism_by_boundary.aw_mean.list
      
      )
    )
    
  } else {
    return(list(
      prism_by_boundary.mean.df=prism_by_boundary.mean.df,
      prism_by_boundary.aw_mean.df=prism_by_boundary.aw_mean.df
      )
    )
    
  }
  
}


##################################################################
##################################################################

# city.prism_var_by_tract.mean.summary.df <-
#   gather(city.prism_var_by_tract.mean.df) %>%
#   group_by(key) %>%
#   summarize(across(,.fns = list(mean=mean,min=min,max=max))) %>%
#   mutate(max_f = weathermetrics::celsius.to.fahrenheit(value_max),
#          min_f = weathermetrics::celsius.to.fahrenheit(value_min),
#          range_f = max_f - min_f)
# 
# city.prism_var_by_tract.aw_mean.summary.df <-
#   gather(city.prism_var_by_tract.aw_mean.df) %>%
#   group_by(key) %>%
#   summarize(across(,.fns = list(mean=mean,min=min,max=max))) %>%
#   mutate(max_f = weathermetrics::celsius.to.fahrenheit(value_max),
#          min_f = weathermetrics::celsius.to.fahrenheit(value_min),
#          range_f = max_f - min_f)


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
