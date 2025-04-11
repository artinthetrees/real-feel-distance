
crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
output_path <- Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname)

print(crs_utm)
print(output_path)

get_boundary_maps(
  city_string=city_string, 
  county_string=county_string, 
  state_string=state_string, 
  year_num=year_num, 
  crs_utm=crs_utm, 
  output_path=output_path, 
  my_font_size=10
)










