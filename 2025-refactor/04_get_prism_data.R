crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
print(crs_utm)

tracts.within.city.utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","boundary_maps",city_year_output_fname),obj_name = tracts.within.city.utm)


## get prism tmax data
prism_var <- "tmax"
output_path <- Gmisc::pathJoin(output_dir,"intermediate","environmental_data",prism_var,city_year_output_fname)
print(output_path)

prism_file_list <- get_norc_prism_file_path(prism_var=prism_var,year_num=year_num)
prism_file_list <- prism_file_list$prism_file_list
print(prism_file_list[1])

get_prism_per_boundary(
  prism_file_list=prism_file_list,
  boundary_map=tracts.within.city.utm,
  return_type="verbose",
  output_path=output_path
)

## get prism tdmean data
prism_var <- "tdmean"
output_path <- Gmisc::pathJoin(output_dir,"intermediate","environmental_data",prism_var,city_year_output_fname)
print(output_path)

prism_file_list <- get_norc_prism_file_path(prism_var=prism_var,year_num=year_num)
prism_file_list <- prism_file_list$prism_file_list

get_prism_per_boundary(
  prism_file_list=prism_file_list,
  boundary_map=tracts.within.city.utm,
  return_type="verbose",
  output_path=output_path
)

