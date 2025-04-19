#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
print(crs_utm)

#################################################################
# work for this step

get_boundary_maps(
  city_string=city_string, 
  county_string=county_string, 
  state_string=state_string, 
  year_num=year_num, 
  crs_utm=crs_utm, 
  output_path=output_path, 
  my_font_size=10
)

#################################################################
# save for this step

# save(
#   file = output_path
# )

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################








