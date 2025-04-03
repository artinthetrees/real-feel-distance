library(tidyverse)

get_utm_zone <- function(my_census_api_key,county_string,state_string,year_num){
    tidycensus::census_api_key(my_census_api_key)

    fips_cd_df <-
        tidycensus::fips_codes %>%
        filter(county == county_string & state == state_string)

    county_fips_cd <- sprintf("%03s", (fips_cd_df[1, "county_code"]))
    state_fips_cd <- sprintf("%02s", fips_cd_df[1, "state_code"])

    county_centroid <- 
        tigris::counties(state = state_string, resolution = "20m", year = year_num) %>%
        sf::st_centroid() %>%
        filter(COUNTYFP == county_fips_cd)

    county_centroid <- 
        county_centroid %>%
        mutate(lat = unlist(purrr::map(county_centroid$geometry,2)),
            lon = unlist(purrr::map(county_centroid$geometry,1))) %>% 
        sf::st_drop_geometry()

    county_centroid_lon <- county_centroid[1,"lon"] 
    county_utm_zone <- floor((county_centroid_lon + 180) / 6) + 1

    #-----------------------------------
    crs_lonlat <- "+proj=longlat +datum=NAD83"
    #crs_utm <- "+proj=utm +zone=16N +datum=NAD83 +ellps=GRS80"
    #utm_zone <- ggmap::geocode(city)
    crs_utm <- paste0("+proj=utm +zone=",as.character(county_utm_zone)," +datum=NAD83")

    return(list(crs_lonlat=crs_lonlat, crs_utm=crs_utm, county_utm_zone=county_utm_zone))
}

get_lonlat_points_within_boundary <- function(points_df,lat_var, lon_var,boundary_map_in_utm,crs_lonlat, crs_utm){

    # points_df is a simple dataframe (NOT an sf object) that minimally includes a column with longitude and a column with latitude;
    #    where each row is a lonlat point location (e.g. location of a store) 
    # lat_var is the name of the df column with latitude
    # lon_var is the name of the df column with longitude
    # boundary_map_in_utm is an sf object in utm of your boundary polygon; the function will determine which points in points_df are in this boundary
    # crs_lonlat is a string that specifies your crs for lonlat maps
    # crs_utm is a string that specifies your crs for utm maps
    

    p_df_lonlat <- sf::st_as_sf(points_df,coords=c(lon_var,lat_var),crs=crs_lonlat)
    p_df_utm <- sf::st_transform(p_df_lonlat, crs=crs_utm)

    p_intersects <- sf::st_intersects(p_df_utm,boundary_map_in_utm)
    p_within_boolean <- sapply(p_intersects, function(x) !is_empty(x))

    p_within_orig <- points_df[p_within_boolean, ]
    p_within_lonlat <- p_df_lonlat[p_within_boolean, ]
    p_within_utm <- p_df_utm[p_within_boolean, ]

    return(list(points_within_orig=p_within_orig,points_within_lonlat=p_within_lonlat,points_within_utm=p_within_utm))
}
