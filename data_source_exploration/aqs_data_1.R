# downloaded hourly_44201_2021.zip file from https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw
# this is hourly ozone measurement data 
# saved in H:/sparkathon_2021/
# unzipped manually using 7zip into H:/sparkathon_2021/hourly_44201_2021/hourly_44201_2021.csv

setwd("H:/sparkathon_2021/")

aqs_hourly <- readr::read_csv(file = "hourly_44201_2021/hourly_44201_2021.csv")

library(dplyr)
library(ggmap)

# get the count of monitors in cook county and the lat/long of those monitors
## 9 monitors in cook county measuring ozone on at least an hourly basis
hi3 <- aqs_hourly %>% filter(`State Name`== 'Illinois', `County Name` == 'Cook') %>% group_by(Latitude,Longitude) %>% tally()

## basic mapping 
# check how many are in chicago proper versus larger cook county
# https://cfss.uchicago.edu/notes/raster-maps-with-ggmap/
  
# store bounding box coordinates
chi_bb <- c(
  left = -87.936287,
  bottom = 41.679835,
  right = -87.447052,
  top = 42.000835
)

cook_bb <- c(
  left = -88,
  bottom = 41.660,
  right = -87.447052,
  top = 42.15
)

chicago <- get_stamenmap(
  bbox = chi_bb,
  zoom = 11
)

cook <- get_stamenmap(
  bbox = cook_bb,
  zoom = 11
)


ggmap(chicago) +
  geom_point(
    data = hi3,
    mapping = aes(
      x = Longitude,
      y = Latitude
    ),
    size = 5,
    alpha = 1
  )

ggmap(cook) +
  geom_point(
    data = hi3,
    mapping = aes(
      x = Longitude,
      y = Latitude
    ),
    size = 5,
    alpha = 1
  )

