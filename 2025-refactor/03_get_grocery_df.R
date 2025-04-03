# start with grocery df that includes one col for latitude and one col for longitude as well as 



# get_lonlat_points_within_boundary <- function(points_df,lat_var, lon_var,boundary_map_in_utm,crs_lonlat, crs_utm){

#     # points_df is a simple dataframe (NOT an sf object) that minimally includes a column with longitude and a column with latitude;
#     #    where each row is a lonlat point location (e.g. location of a store) 
#     # lat_var is the name of the df column with latitude
#     # lon_var is the name of the df column with longitude
#     # boundary_map_in_utm is an sf object in utm of your boundary polygon; the function will determine which points in points_df are in this boundary
#     # crs_lonlat is a string that specifies your crs for lonlat maps
#     # crs_utm is a string that specifies your crs for utm maps
    

#     p_df_lonlat <- sf::st_as_sf(points_df,coords=c(lon_var,lat_var),crs=crs_lonlat)
#     p_df_utm <- sf::st_transform(p_df_lonlat, crs=crs_utm)

#     p_intersects <- sf::st_intersects(p_df_utm,boundary_map_in_utm)
#     p_within_boolean <- sapply(p_intersects, function(x) !is_empty(x))

#     p_within_orig <- points_df[p_within_boolean, ]
#     p_within_lonlat <- p_df_lonlat[p_within_boolean, ]
#     p_within_utm <- p_df_utm[p_within_boolean, ]

#     return(list(points_within_orig=p_within_orig,points_within_lonlat=p_within_lonlat,points_within_utm=p_within_utm))
# }