library(tidyverse)
#################################################################
# list all objs in workspace at start

ws_objs <- ls()

#################################################################
# define outpath for this step

output_path <- Gmisc::pathJoin(output_dir,"intermediate","acs_data",city_year_output_fname)
print(output_path)

#################################################################
# pull in objs needed from previous parts of pipeline for this step

# crs_utm <- get_obj_from_rdata(rdata_file_path = Gmisc::pathJoin(output_dir,"intermediate","crs",city_year_output_fname),obj_name = crs_utm)
# print(crs_utm)

#################################################################
# work for this step

acs_codebook <- tidycensus::load_variables(year_num, "acs5", cache = TRUE)

state_tracts_acs_df <-
  tidycensus::get_acs(geography = "tract",
                      year = year_num,
                      variables = c(tpop = "B01003_001",

                                    # older adult here is over 65
                                    tpop_sex_age = "B01001_001",
                                    tpop_sex_age_male = "B01001_002",
                                    tpop_sex_age_male_older_adult_1 = "B01001_020",
                                    tpop_sex_age_male_older_adult_2 = "B01001_021",
                                    tpop_sex_age_male_older_adult_3 = "B01001_022",
                                    tpop_sex_age_male_older_adult_4 = "B01001_023",
                                    tpop_sex_age_male_older_adult_5 = "B01001_024",
                                    tpop_sex_age_male_older_adult_6 = "B01001_025",
                                    tpop_sex_age_female = "B01001_026",
                                    tpop_sex_age_female_older_adult_1 = "B01001_044",
                                    tpop_sex_age_female_older_adult_2 = "B01001_045",
                                    tpop_sex_age_female_older_adult_3 = "B01001_046",
                                    tpop_sex_age_female_older_adult_4 = "B01001_047",
                                    tpop_sex_age_female_older_adult_5 = "B01001_048",
                                    tpop_sex_age_female_older_adult_6 = "B01001_049",

                                    # older adult here is over 65
                                    tpop_sex_age_white = "B01001H_001",
                                    tpop_sex_age_white_male = "B01001H_002",
                                    tpop_sex_age_white_male_older_adult_1 = "B01001H_014",
                                    tpop_sex_age_white_male_older_adult_2 = "B01001H_015",
                                    tpop_sex_age_white_male_older_adult_3 = "B01001H_016",
                                    tpop_sex_age_white_female = "B01001H_017",
                                    tpop_sex_age_white_female_older_adult_1 = "B01001H_029",
                                    tpop_sex_age_white_female_older_adult_2 = "B01001H_030",
                                    tpop_sex_age_white_female_older_adult_3 = "B01001H_031",

                                    # older adult here is over 65
                                    tpop_age_transport_to_work = "B08101_001",
                                    tpop_age_transport_to_work_walk = "B08101_033",
                                    tpop_age_transport_to_work_older_adult = "B08101_008",
                                    tpop_age_transport_to_work_walk_older_adult = "B08101_040",

                                    # older adult here is over 65
                                    tpop_age_living_arrange = "B09021_001",
                                    tpop_age_living_arrange_live_alone = "B09021_002",
                                    tpop_age_living_arrange_older_adult = "B09021_022",
                                    tpop_age_living_arrange_live_alone_older_adult = "B09021_023",

                                    # older adult here is over 65
                                    tpop_sex_age_poverty = "B17001_001",
                                    tpop_sex_age_poverty_below_poverty = "B17001_002",
                                    tpop_sex_age_poverty_below_poverty_male = "B17001_003",
                                    tpop_sex_age_poverty_below_poverty_male_older_adult_1 = "B17001_015",
                                    tpop_sex_age_poverty_below_poverty_male_older_adult_2 = "B17001_016",
                                    tpop_sex_age_poverty_at_or_above_poverty_male_older_adult_1 = "B17001_044",
                                    tpop_sex_age_poverty_at_or_above_poverty_male_older_adult_2 = "B17001_045",
                                    tpop_sex_age_poverty_below_poverty_female = "B17001_017",
                                    tpop_sex_age_poverty_below_poverty_female_older_adult_1 = "B17001_029",
                                    tpop_sex_age_poverty_below_poverty_female_older_adult_2 = "B17001_030",
                                    tpop_sex_age_poverty_at_or_above_poverty_female_older_adult_1 = "B17001_058",
                                    tpop_sex_age_poverty_at_or_above_poverty_female_older_adult_2 = "B17001_059",

                                    # older adult here is over 60
                                    tpop_age_poverty = "B17020_001",
                                    tpop_age_poverty_below_poverty = "B17020_002",
                                    tpop_age_poverty_below_poverty_older_adult_1 = "B17020_007",
                                    tpop_age_poverty_below_poverty_older_adult_2 = "B17020_008",
                                    tpop_age_poverty_below_poverty_older_adult_3 = "B17020_009",

                                    tpop_race = "B03002_001",
                                    tpop_race_nonhisp_white = "B03002_003",
                                    tpop_race_nonhisp_blk = "B03002_004",
                                    tpop_race_nonhisp_asn = "B03002_006",
                                    tpop_race_hisp = "B03002_012"),
                      state = state_string,
                      survey = "acs5",
                      output = "wide",#"tidy"
                      moe_level = 90,
                      geometry = FALSE,
                      key = my_census_api_key)

state_tracts_acs_estimates_df <-
  state_tracts_acs_df %>% 
  # get rid of margin of error vars for now; just keep estimate vars
  select(!ends_with("M")) %>%
  #select(tpopE, tpop_raceE, tpop_sex_ageE, tpop_age_living_arrangeE, tpop_sex_age_povertyE, tpop_age_povertyE, tpop_age_transport_to_workE)
  dplyr::rename(Name = NAME) %>%
  dplyr::rename_with(~ sub("E$", "", .x), everything()) %>%
  rowwise() %>%
  mutate(tpop_male_older_adult = sum(c_across(starts_with("tpop_sex_age_male_older_adult_"))),
         tpop_female_older_adult = sum(c_across(starts_with("tpop_sex_age_female_older_adult_"))),
         
         tpop_white_male_older_adult = sum(c_across(starts_with("tpop_sex_age_white_male_older_adult_"))),
         tpop_white_female_older_adult = sum(c_across(starts_with("tpop_sex_age_white_female_older_adult_"))),
         
         tpop_male_older_adult_below_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_below_poverty_male_older_adult_"))),
         tpop_female_older_adult_below_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_below_poverty_female_older_adult_"))),
         tpop_male_older_adult_at_or_above_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_at_or_above_poverty_male_older_adult_"))),
         tpop_female_older_adult_at_or_above_poverty = sum(c_across(starts_with("tpop_sex_age_poverty_at_or_above_poverty_female_older_adult_"))),
         
  ) %>%
  ungroup() %>%
  mutate(tpop_older_adult = tpop_male_older_adult + tpop_female_older_adult,
         tpop_older_adult_white = tpop_white_male_older_adult + tpop_white_female_older_adult,
         tpop_older_adult_nonwhite = tpop_older_adult - tpop_older_adult_white,
         
         tpop_older_adult_below_poverty = tpop_male_older_adult_below_poverty + tpop_female_older_adult_below_poverty,
         tpop_older_adult_at_or_above_poverty = tpop_male_older_adult_at_or_above_poverty + tpop_female_older_adult_at_or_above_poverty,
         tpop_older_adult_poverty_reported = tpop_older_adult_below_poverty + tpop_older_adult_at_or_above_poverty,
         
         tpop_older_adult_walk_to_work = tpop_age_transport_to_work_walk_older_adult,
         tpop_older_adult_transport_to_work_reported = tpop_age_transport_to_work_older_adult,
         
         tpop_older_adult_live_alone = tpop_age_living_arrange_live_alone_older_adult,
         tpop_older_adult_living_arrange_reported = tpop_age_living_arrange_older_adult,
         
         
         p_older_adult = tpop_older_adult/tpop_sex_age, # percent of census tract pop that is 65+
         p_older_adult_nonwhite = tpop_older_adult_nonwhite/tpop_older_adult, # percent of older adults that are nonwhite (among those who report race)
         p_older_adult_walk_to_work = tpop_age_transport_to_work_walk_older_adult/tpop_age_transport_to_work_older_adult, # percent of older adults who walk to work (among those who report going to work)
         p_older_adult_live_alone = tpop_age_living_arrange_live_alone_older_adult/tpop_age_living_arrange_older_adult, # percent of older adults who live alone (among those who report a living arrangement)
         p_older_adult_below_poverty = tpop_older_adult_below_poverty/(tpop_older_adult_below_poverty + tpop_older_adult_at_or_above_poverty), # percent of older adults in poverty (among those who report on income)
         
         p_nonhispanic_white = tpop_race_nonhisp_white/tpop_race,
         p_nonhispanic_asn = tpop_race_nonhisp_asn/tpop_race, 
         p_nonhispanic_blk = tpop_race_nonhisp_blk/tpop_race, 
         p_hisp = tpop_race_hisp/tpop_race
         
  ) %>%
  select(GEOID,
         Name,
         
         tpop, 
         tpop_race, 
         tpop_sex_age,
         tpop_age_living_arrange, 
         tpop_sex_age_poverty, 
         tpop_age_poverty, 
         tpop_age_transport_to_work,
         
         tpop_older_adult,
         tpop_older_adult_white,
         tpop_older_adult_nonwhite,
         
         tpop_older_adult_poverty_reported,
         tpop_older_adult_below_poverty,
         tpop_older_adult_at_or_above_poverty,
         
         tpop_older_adult_transport_to_work_reported,
         tpop_older_adult_walk_to_work,
         
         tpop_older_adult_living_arrange_reported,
         tpop_older_adult_live_alone,
         
         p_older_adult,
         p_older_adult_nonwhite,
         p_older_adult_walk_to_work,
         p_older_adult_live_alone,
         p_older_adult_below_poverty,
         
         p_nonhispanic_white,
         p_nonhispanic_asn, 
         p_nonhispanic_blk, 
         p_hisp,
         
         #geometry
         
  )

#################################################################
# save for this step

save(
  acs_codebook,
  state_tracts_acs_df,
  state_tracts_acs_estimates_df,
  file = output_path
)

#################################################################
# clean up workspace - remove any objs created during this step

rm(list = setdiff(ls(),ws_objs))

#################################################################








