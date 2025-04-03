# start with df that includes one col for latitude and one col for longitude as well as 

get_grocery_points <- function(grocery_df,lat_var, lon_var, crs_lonlat, crs_utm, city.2km.buffer.utm){
    g_df_lonlat <- sf::st_as_sf(grocery_df,coords=c(lon_var,lat_var),crs=crs_lonlat)
    g_df_utm <- sf::st_transform(g_df_lonlat, crs=crs_utm)

    g_intersects <- sf::st_intersects(g_df_utm,city.2km.buffer.utm)
    g_within_tf <- sapply(g_intersects, function(x) !is_empty(x))

    g_within_pts_orig <- grocery_df[g_within_tf, ]
    g_within_pts_lonlat <- g_df_lonlat[g_within_tf, ]
    g_within_pts_utm <- g_df_utm[g_within_tf, ]

    return(list(grocery_within_orig=g_within_pts_orig,grocery_within_lonlat=g_within_pts_lonlat,grocery_within_utm=g_within_pts_utm))
}