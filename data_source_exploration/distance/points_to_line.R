# https://stackoverflow.com/questions/48400758/add-lines-between-labelled-location-points

points_to_line <- function(data, long, lat, id_value = NULL, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  sp::coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    if(is.null(id_value)){
      
      lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(data)), "id")))
      
      return(lines)
      
    } else if(!is.null(id_value)){
      
      lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(data)), id_value)))
      
      return(lines)
      
    }
    
    
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- sp::SpatialLines(list(sp::Lines(list(sp::Line(paths[[p]])), id)))
      sp_lines <- maptools::spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}