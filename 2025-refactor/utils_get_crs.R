paste0(city_string,"_",as.character(year_num),".Rdata")

get_crs <- function(county_string,state_string,year_num,crs_datum,output_path=NULL){
  
  fips_cd_df <-
    tidycensus::fips_codes %>%
    #get(data(fips_codes)) %>%
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
  crs_lonlat <- paste0("+proj=longlat +datum=",crs_datum)
  crs_utm <- paste0("+proj=utm +zone=",as.character(county_utm_zone)," +datum=",crs_datum)
  
  if (!is.null(output_path)){
    save(crs_lonlat,crs_utm,county_utm_zone,file=output_path)
  }
  
  crs_data <- list(
    crs_lonlat=crs_lonlat, 
    crs_utm=crs_utm, 
    county_utm_zone=county_utm_zone
    )
  print(crs_data)
  return(crs_data)
}