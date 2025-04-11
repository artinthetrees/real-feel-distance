prism_file <- "P:/LABO.10.32/Common/prism-climate-data/an81_daily_ppt_tmax_tmin_2019_2020/tmax/daily/2020/prism_tmax_us_30s_20201002.bil"

prism_rast <- terra::rast(prism_file)
terra::plot(prism_rast)

boundary_map <- tracts.within.city.utm
boundary_map <- boundary_map %>% sf::st_transform(terra::crs(prism_rast))
boundary_map_spatvect <- terra::vect(boundary_map)
prism_rast_boundary_extent <-
  terra::crop(prism_rast, boundary_map)

terra::plot(prism_rast_boundary_extent)
terra::plot(boundary_map_spatvect)

mapview::mapview(prism_rast_boundary_extent) + mapview::mapview(boundary_map_spatvect,alpha.regions=.2)


hi_df <- cbind(tracts.within.city.utm,prism_by_boundary.aw_mean.df)
mapview::mapview(hi_df,zcol="X20200701")

# how to save a mapview image to file
# https://r-spatial.github.io/mapview/reference/mapshot.html#:~:text=mapshot%20can%20be%20used%20to,or%20png%20files%20or%20both.