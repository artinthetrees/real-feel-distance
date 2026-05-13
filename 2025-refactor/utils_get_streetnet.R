library(tidyverse)
#library(dodgr)
#library(ohsome)

process_bbox_coords <- function(bbox_coords){
  # coords must be a vector in xmin, ymin, xmax, ymax order
  # coords may already be a named vector but does not have to be
  # if named, names must be c("xmin","ymin","xmax","ymax")
  
  my_names <- c("xmin","ymin","xmax","ymax")
  
  if (is.null(names(bbox_coords))){
    
    names(bbox_coords) = my_names
    
  } else {
    
    if (!all(names(bbox_coords) == my_names)){
      print("bbox coords must be a vector in xmin, ymin, xmax, ymax order")
      return(NULL)
    } 
    
  }
  
  return(bbox_coords)
  
}

bbox_coords_to_sf <- function(bbox_coords,crs){
  # coords must be a vector in xmin, ymin, xmax, ymax order
  # coords may already be a named vector but does not have to be
  # if named, names must be c("xmin","ymin","xmax","ymax")
  
  bbox_coords <- process_bbox_coords(bbox_coords = bbox_coords)
  
  if (is.null(bbox_coords)){
    return(NULL)
  }
  
  bbp = sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(bbox_coords)), crs=crs)
  return(bbp)
  
}

bbox_coords_to_osmdata_bb <- function(bbox_coords){
  # coords must be a vector in xmin, ymin, xmax, ymax order
  # coords may already be a named vector but does not have to be
  # if named, names must be c("xmin","ymin","xmax","ymax")
  
  bbox_coords <- process_bbox_coords(bbox_coords = bbox_coords)
  
  if (is.null(bbox_coords)){
    return(NULL)
  }
  
  a = matrix(bbox_coords,nrow = 2,ncol = 2,byrow = FALSE)
  rownames(a) <- c("x","y")
  colnames(a) <- c("min","max")
  
  return(a)
}

get_streetnet_sf_dodgr <- function(osmdata_bb=NULL, city_string=NULL, state_string=NULL, expand=0.05, outpath=NULL){
  
  if (is.null(osmdata_bb)){
    if (is.null(city_string) | is.null(state_string)){
      
      print("must provide either osmdata_bb or BOTH city_string and state_string parameter")
      return(NULL)
      
    } else {
      
      osmdata_bb <- osmdata::getbb(place_name=paste(city_string,state_string,sep=", "), featuretype = "city")
      
    }
  }
  
  print(osmdata_bb)
  
  streetnet_sf <- dodgr::dodgr_streetnet(bbox = osmdata_bb, expand = expand)
  return(streetnet_sf)
  
  
}

get_ohsome_latest_to_date <- function(){
  md <- ohsome::ohsome_get_metadata()
  to_date <- str_split(md$extractRegion$temporalExtent$toTimestamp,"T")[[1]][1]
  
  return(to_date)
}

get_streetnet_sf_ohsome <- function(boundary_map,year_num=NULL,date=NULL,outpath=NULL){
  if (!is.null(year_num) & !is.null(date)){
    print("supply only one of year_num or date parameters")
    return 
  }
  
  if (!is.null(year_num)){
    time <- paste0(as.character(year_num),"-06-01")
  } else if (!is.null(date)){
    time <- date
  } else {
    time <- get_ohsome_latest_to_date()
  }
  
  streetnet_sf <- 
    ohsome::ohsome_elements_geometry(
      boundary = boundary_map, 
      filter = "highway=*",
      time = time,
      properties = "tags", 
      clipGeometry = FALSE
    ) |>
    ohsome::ohsome_post()
    
  return(streetnet_sf)

}

process_streetnet_ohsome <- function(streetnet_sf){
  
  streetnet_sf <-
    streetnet_sf %>%
    rename(osm_id = "@osmId") %>%
    rowwise() %>% mutate(osm_id = str_split(osm_id,"/")) %>%
    rowwise() %>% mutate(osm_id_type = osm_id[1], osm_id = osm_id[2])
    
  return(streetnet_sf)
  
}

get_geom_type_col <- function(sf){
  sf <- 
    sf %>%
    mutate(
      geom_str = sf::st_as_text(geometry),
      geom_str_split = strsplit(geom_str," ")
      ) %>% 
    rowwise() %>% 
    mutate(geom_type = str_trim(geom_str_split[1])) %>%
    select(!c(geom_str,geom_str_split))
  
  return(sf)
}


get_streetnet_weighted_graph_and_vertices <- function(streetnet_sf,wt_profile="foot",outpath=NULL){
  
  graph <- dodgr::weight_streetnet (streetnet_sf, wt_profile = wt_profile)
  v <- dodgr::dodgr_vertices (dodgr_graph)
  
  if (!is.null(outpath)){
    save(
      graph,
      v,
      file=outpath)
  }
  
  return(list(graph=graph,v=v))
  
}
  









