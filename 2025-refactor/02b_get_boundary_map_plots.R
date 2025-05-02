library(ggplot2)
#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","boundary_map_plots",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
print(crs_utm)

city.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = city.utm)
city.2km.buffer.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = city.2km.buffer.utm)

counties.within.city.2km.buffer.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = counties.within.city.2km.buffer.utm)
tracts.within.city.2km.buffer.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = tracts.within.city.2km.buffer.utm)

#################################################################
# work for this step

my_font_size = 10

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

#################################################################
# save for this step

save(
  counties.overview.plot,
  tracts.overview.plot,
  counties.within.city.2km.buffer.plot,
  counties.within.city.2km.buffer.plus.city.plot,
  tracts.within.city.2km.buffer.plus.city.plot.0,
  tracts.within.city.2km.buffer.plus.city.plot.1,
  tracts.within.city.2km.buffer.plus.city.plot.2,
  file = output_path
)

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################








