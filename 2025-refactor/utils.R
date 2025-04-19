library(tidyverse)

tidycensus::census_api_key(my_census_api_key)


get_city_year_output_filename <- function(city_string,year_num,save_file_type="Rdata"){
  fname <- paste0(city_string,"_",as.character(year_num),".",save_file_type)
  return(fname)
}

get_city_output_filename <- function(city_string,save_file_type="Rdata"){
  fname <- paste0(city_string,".",save_file_type)
  return(fname)
}

get_obj_from_rdata <- function(rdata_file_path,obj_name=NULL){
  attach(rdata_file_path)
  obj <- obj_name
  detach()
  return(obj)
}

points_df_to_sf <- function(points_df,lat_var,lon_var,crs_lonlat,crs_utm){
  
  # points_df is a simple dataframe (NOT an sf object) that minimally includes a column with longitude and a column with latitude;
  #    where each row is a lonlat point location (e.g. location of a store) 
  # lat_var is the name of the df column with latitude
  # lon_var is the name of the df column with longitude
  # crs_lonlat is a string that specifies your crs for lonlat maps
  # crs_utm is a string that specifies your crs for utm maps
  
  # clean any points without long/lat
  r <- which(is.na(points_df[lon_var]))
  points_df <- points_df[-r,]
  
  # set a crs for the grocery lat/long coordinates
  points_sf.lonlat <- 
    sf::st_as_sf(points_df, coords = c(lon_var, lat_var), crs = crs_lonlat)
  
  # check that the crs was set and check the units (will be null for longlat projection)
  print(sf::st_crs(points_sf.lonlat)$proj4string)
  print(sf::st_crs(points_sf.lonlat)$units)
  
  # convert crs to use utm which measures distance in meters
  points_sf.utm <- 
    sf::st_transform(points_sf.lonlat, crs = crs_utm) 
  
  # check that the crs was set and check the units (will be meters for utm projection)
  print(sf::st_crs(points_sf.utm)$proj4string)
  print(sf::st_crs(points_sf.utm)$units)
  
  return(list(points_sf.lonlat=points_sf.lonlat,points_sf.utm=points_sf.utm))
}

get_points_within_boundary <- function(points_df,lat_var, lon_var, boundary_map_in_utm, crs_lonlat, crs_utm){

    # points_df is a simple dataframe (NOT an sf object) that minimally includes a column with longitude and a column with latitude;
    #    where each row is a lonlat point location (e.g. location of a store) 
    # lat_var is the name of the df column with latitude
    # lon_var is the name of the df column with longitude
    # boundary_map_in_utm is an sf object in utm of your boundary polygon; the function will determine which points in points_df are in this boundary
    # crs_lonlat is a string that specifies your crs for lonlat maps
    # crs_utm is a string that specifies your crs for utm maps
    
    points_sf <- points_df_to_sf(points_df = points_df, lat_var = lat_var, lon_var = lon_var, crs_lonlat = crs_lonlat, crs_utm = crs_utm)
    points_sf.lonlat <- points_sf$points_sf.lonlat
    points_sf.utm <- points_sf$points_sf.utm
    
    # p_df_lonlat <- sf::st_as_sf(points_df,coords=c(lon_var,lat_var),crs=crs_lonlat)
    # p_df_utm <- sf::st_transform(p_df_lonlat, crs=crs_utm)

    p_intersects <- sf::st_intersects(points_sf.utm,boundary_map_in_utm)
    p_within_boolean <- sapply(p_intersects, function(x) !is_empty(x))

    p_within_df <- points_df[p_within_boolean, ]
    p_within_sf.lonlat <- points_sf.lonlat[p_within_boolean, ]
    p_within_sf.utm <- points_sf.utm[p_within_boolean, ]

    return(list(points_within_df=p_within_df,points_within_sf.lonlat=p_within_sf.lonlat,points_within_sf.utm=p_within_sf.utm))
}
