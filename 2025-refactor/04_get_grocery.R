#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define inpath for this step

input_fname <- paste0(city_string,"_",as.character(year_num),".csv")
input_path <- Gmisc::pathJoin(input_dir,"grocery_data_test",input_fname)

if (!file.exists(input_path)){
  stop("required input file does not exist - exiting")
} else {
  print("required input file found - proceeding")
}

lon_var = "Longitude"
lat_var = "Latitude"
#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","grocery",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
crs_lonlat <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_lonlat)

print(crs_utm)
print(crs_lonlat)

boundary.utm <- 
  get_obj_from_rdata(
    rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),
    obj_name = city.2km.buffer.utm
  )

grocery_df <- read.csv(input_path, stringsAsFactors = FALSE)

#################################################################
# work for this step

grocery_within <- 
get_points_within_boundary(
  points_df=grocery_df,
  lat_var=lat_var, 
  lon_var=lon_var, 
  boundary_map_in_utm=boundary.utm, 
  crs_lonlat=crs_lonlat, 
  crs_utm=crs_utm)

grocery_within_df <- grocery_within$points_within_df
grocery_within_sf.lonlat <- grocery_within$points_within_sf.lonlat
grocery_within_sf.utm <- grocery_within$points_within_sf.utm

#################################################################
# save for this step

save(
  grocery_within_df,
  grocery_within_sf.lonlat,
  grocery_within_sf.utm,
  file = output_path
)

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################















