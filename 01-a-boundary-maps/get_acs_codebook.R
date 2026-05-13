# 
# For the ACS, use either "acs1" or "acs5" for the ACS detailed tables, and append /profile for the Data Profile and /subject for the Subject Tables. 
# To browse these variables, assign the result of this function to a variable and use the View function in RStudio. 
# An optional argument cache = TRUE will cache the dataset on your computer for future use.

# load the codebook for 2015-2019 5-year ACS
# released 12/10/20
# https://www.census.gov/newsroom/press-kits/2020/acs-5-year.html
v19 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)

# load the codebook for 2016-2020 5-year ACS
# released 3/17/22
# https://www.census.gov/newsroom/press-kits/2021/acs-5-year.html
v20 <- tidycensus::load_variables(2020, "acs5", cache = TRUE)

# load the codebook for 2017-2021 5-year ACS
# released 12/8/22
# https://www.census.gov/newsroom/press-kits/2022/acs-5-year.html
v21 <- tidycensus::load_variables(2021, "acs5", cache = TRUE)

# load the codebook for 2018-2022 5-year ACS
# released N/A
# N/A
#v22 <- tidycensus::load_variables(2022, "acs5", cache = TRUE)







