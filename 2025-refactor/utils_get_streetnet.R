

get_streetnet_dodgr <- function(city_string, state_string, expand=0.05, wt_profile="foot"){
  
  place_bb <- osmdata::getbb(place_name=paste(city_string,state_string,sep=", "), featuretype = "city")
  print(place_bb)
  
  dat_sf <- dodgr::dodgr_streetnet(bbox = place_bb, expand = expand)
  graph <- dodgr::weight_streetnet (dat_sf, wt_profile = wt_profile)
  v <- dodgr::dodgr_vertices (graph)
  
}
  









