# https://rpubs.com/DanielSLee/censusMap
# https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
# https://rpubs.com/quarcs-lab/tutorial-maps-in-r




#https://www.treeequityscore.org/map/#11/41.8337/-87.7224
#https://www.treeequityscore.org/data/
#https://www.earthdefine.com/treemap/

#https://www.sciencedirect.com/science/article/pii/S161886671630348X?casa_token=NRxGZeXlahYAAAAA:Bv2FzfgsZtD_YbuTX7N-I3lzR83hoT1jVWcRoQJdXatdlWMKMnfFcG1HqT88GW7uC-Wmf-vYhw
#https://www.sciencedirect.com/science/article/pii/S1618866715001247?casa_token=tRURE-VqPcwAAAAA:57_9t1h_HQaTTcDlAxDWLxqk6HzAHZlM7UFt2_KiWgZq0QysCyft6xireXCIy2tmchULu2j_WQ
#https://acsess.onlinelibrary.wiley.com/doi/full/10.2134/jeq2015.01.0039

library(dplyr)

tree_equity <- 
  sf::st_read("C:/Users/Andrea/Desktop/il.zip/il.shp") %>% filter(county == "Cook County" & incorpname == "Chicago")

tree_equity$GEOID_tract <- substr(tree_equity$geoid,6,11)

sf::st_crs(tree_equity)$proj4string
plot(tree_equity %>% select(tc_gap,geometry))
plot(tree_equity %>% select(tes,geometry))
plot(tree_equity %>% select(avg_temp,geometry))


