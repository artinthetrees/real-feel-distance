library(tidyverse)
library(ggplot2)
library(patchwork)

states.utm <- 
    tigris::states(cb=TRUE, year=year_num, resolution="20m") %>%
    tigris::shift_geometry() %>%
    sf::st_transform(crs=crs_utm)

##########################################################################################################################################################
# get state boundary and state + buffer boundary - state + buffer will be used to identify the states surrounding the state of interest
state.utm <- 
    states.utm %>% dplyr::filter(STUSPS == state_string)

state.2km.buffer.utm <- 
    sf::st_buffer(state.utm, dist = 2000)

###########################################################################################################################################################
# get all states surrounding state of interest - list and map
states.clipped.by.state.2km.buffer.utm <- 
    rmapshaper::ms_clip(target=states.utm, clip=state.2km.buffer.utm, remove_slivers = FALSE)

# get list
states.within.state.2km.buffer <- 
    list(states.clipped.by.state.2km.buffer.utm$STUSPS)[[1]]

# print list
print(states.within.state.2km.buffer)

# get map
states.within.state.2km.buffer.utm <- 
    states.utm %>% dplyr::filter(STUSPS %in% states.within.state.2km.buffer)

###########################################################################################################################################################
# get city boundary and city + buffer boundary

city.utm <- 
    tigris::places(cb=TRUE, state=state_string, year=year_num) %>%
    dplyr::filter(NAME==city_string) %>%
    sf::st_transform(crs=crs_utm)

city.2km.buffer.utm <- 
    sf::st_buffer(city.utm,dist=2000)

###########################################################################################################################################################
# get all states within city + buffer boundary - list and map
states.clipped.by.city.2km.buffer.utm <- 
    rmapshaper::ms_clip(target=states.utm, clip=city.2km.buffer.utm, remove_slivers = FALSE)

# get list
states.within.city.2km.buffer <- 
    list(states.clipped.by.city.2km.buffer.utm$STUSPS)[[1]]

# print list
print(states.within.city.2km.buffer)

# get map
states.within.city.2km.buffer.utm <- 
    states.utm %>% dplyr::filter(STUSPS %in% states.within.city.2km.buffer)

###########################################################################################################################################################
# get all counties within city + buffer boundary - list and map

# note: tigris counties function can take a list of states and return a concatenated df across all states in list
counties.utm <- 
    tigris::counties(cb=TRUE,state=states.within.city.2km.buffer,year=year_num) %>%
    sf::st_transform(crs=crs_utm)

counties.clipped.by.city.2km.buffer.utm <- 
    rmapshaper::ms_clip(target=counties.utm, clip=city.2km.buffer.utm, remove_slivers = FALSE)

# get list
counties.within.city.2km.buffer <- 
    list(counties.clipped.by.city.2km.buffer.utm$GEOID)[[1]]

# print list
print(counties.within.city.2km.buffer)

# get map
counties.within.city.2km.buffer.utm <- 
    counties.utm %>% dplyr::filter(GEOID %in% counties.within.city.2km.buffer)

###########################################################################################################################################################
# get all census tracts within city + buffer boundary - list and map

# note: tigris tracts function can NOT take a list of states and return a concatenated df across all states in list
# so we have to iterate through states instead
t_list = list()
for (i in 1:length(states.within.city.2km.buffer)) {
    t <- 
        tigris::tracts(cb=TRUE,state=states.within.city.2km.buffer[i],year=year_num) %>%
        sf::st_transform(crs=crs_utm)
    t_list[[i]] <- t
}
tracts.utm <- dplyr::bind_rows(t_list)

tracts.clipped.by.city.2km.buffer.utm <- 
    rmapshaper::ms_clip(target=tracts.utm, clip=city.2km.buffer.utm, remove_slivers = FALSE)

# get list
tracts.within.city.2km.buffer <- 
    list(tracts.clipped.by.city.2km.buffer.utm$GEOID)[[1]]

# print list
print(tracts.within.city.2km.buffer)

# get map
tracts.within.city.2km.buffer.utm <- 
    tracts.utm %>% dplyr::filter(GEOID %in% tracts.within.city.2km.buffer)

###########################################################################################################################################################
# plots

# raw plots - counties overview
counties.within.city.2km.buffer.plot <- 
    ggplot() +
    geom_sf(data=counties.within.city.2km.buffer.utm, aes(fill=STUSPS)) +
    geom_sf_text(data=counties.within.city.2km.buffer.utm, aes(label=NAME),colour = "white",size=my_font_size/.pt) +
    coord_sf(datum=sf::st_crs(city.utm)) +
    theme_void() +
    theme(legend.position = "none")

counties.within.city.2km.buffer.plus.city.plot <- 
    ggplot() +
    geom_sf(data=counties.within.city.2km.buffer.utm, aes(fill=STUSPS)) +
    geom_sf(data=city.utm) +
    geom_sf_text(data=city.utm, aes(label=NAME),size=my_font_size/.pt) +
    coord_sf(datum=sf::st_crs(city.utm)) +
    theme_void() +
    labs(fill="State")

# raw plots - tracts overview
tracts.within.city.2km.buffer.plus.city.plot.0 <- 
    ggplot() +
    geom_sf(data=counties.within.city.2km.buffer.utm, fill=NA) +
    geom_sf(data=tracts.within.city.2km.buffer.utm, aes(fill=STUSPS)) +
    geom_sf(data=city.utm,alpha=0.5,color=NA) +
    geom_sf_text(data=city.utm, aes(label=NAME),size=my_font_size/.pt) +
    coord_sf(datum=sf::st_crs(city.utm)) +
    theme_void() +
    labs(fill="State")

tracts.within.city.2km.buffer.plus.city.plot.1 <- 
    ggplot() +
    geom_sf(data=tracts.within.city.2km.buffer.utm, aes(fill=STUSPS)) +
    geom_sf(data=city.2km.buffer.utm, fill=NA, color="blue") +
    geom_sf(data=city.utm,alpha=0.5,color=NA) +
    geom_sf_text(data=city.utm, aes(label=NAME),size=my_font_size/.pt) +
    coord_sf(datum=sf::st_crs(city.utm)) +
    theme_void() +
    labs(fill="State")

tracts.within.city.2km.buffer.plus.city.plot.2 <- 
    ggplot() +
    geom_sf(data=tracts.within.city.2km.buffer.utm, aes(fill=NAMELSADCO)) +
    geom_sf(data=city.2km.buffer.utm, fill=NA, color="blue") +
    geom_sf(data=city.utm,alpha=0.5,color=NA) +
    geom_sf_text(data=city.utm, aes(label=NAME),size=my_font_size/.pt) +
    coord_sf(datum=sf::st_crs(city.utm)) +
    theme_void() +
    labs(fill="County")

# final plots - counties overview
counties.overview.plot <- 
    counties.within.city.2km.buffer.plot +
    counties.within.city.2km.buffer.plus.city.plot +
    plot_annotation(
        tag_levels = "A"
    )

# final plots - tracts overview
tracts.overview.plot <- 
    tracts.within.city.2km.buffer.plus.city.plot.0 +
    tracts.within.city.2km.buffer.plus.city.plot.1 +
    tracts.within.city.2km.buffer.plus.city.plot.2 +
    plot_annotation(
        tag_levels = "A"
    )