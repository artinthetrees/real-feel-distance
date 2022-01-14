# Real feel distance (older adults) =
# raw distance +
# [raw distance * (0.15 * age_dummy) +
# raw distance * 0.005/C (∆temp(ref15C)*temp_abv15_dummy) +
# raw distance  *0.065/10C (∆temp(ref15C)*temp_abv20_dummy*heat_emergency_dummy) +
# raw distance * 0.091/DewPoint (∆humid(ref13C)) +
# raw distance * 0.118/DewPoint (∆humid(ref13C)*humid_extreme_dummy) +
# (interaction term between temp and humidity)]

# raw distance +
# [raw distance * (0.15 * age_dummy) +
# raw distance * 0.005/C (∆temp(ref70F)*temp_abv70F_dummy) +
# raw distance  *0.065/10C (∆temp(ref15C)*temp_abv20_dummy*heat_emergency_dummy) +
# raw distance * 0.091/DewPoint (∆humid(ref13C)) +
# raw distance * 0.118/DewPoint (∆humid(ref13C)*humid_extreme_dummy) +
# (interaction term between temp and humidity)]

age_dummy <- 1
# https://www.healthline.com/health/exercise-fitness/average-walking-speed#average-speed-by-age
age_penalty <- 0.15

# if temp is above ideal environmental temp for humans (70F) then set this dummy to 1
temp_ref_f <- 70
temp_abv70F_dummy <- quote(ifelse(tmax >= weathermetrics::fahrenheit.to.celsius(temp_ref_f, round = 2),1,0))
temp_abv70F_penalty <- 0.0056 
temp_abv70F_penalty_young <- 0.0035 
# 5% decrease in aerobic capacity for every 10 degrees celsius >> 0.5% increase in 'distance' for every 1 degree celsius

dp_ref_f <- 55
dp_abv55F_dummy <- quote(ifelse(tdmean >= weathermetrics::fahrenheit.to.celsius(dp_ref_f, round = 2),1,0)) 
dp_abv55F_penalty <- 0.055
dp_abv55F_penalty_young <- 0.077

# https://www.sciencedirect.com/science/article/pii/S0160412021004591
# https://www.sciencedirect.com/science/article/pii/S0160412021004591
# https://ehjournal.biomedcentral.com/articles/10.1186/s12940-016-0167-3
# https://ieeexplore.ieee.org/abstract/document/8254354
# https://www.sciencedirect.com/science/article/pii/S1555415506008889?casa_token=0wRrFTicIZUAAAAA:0hqN1TNm9DyCY-dDsIZkLKtrN2mV9T5gsPsv65QkZrMEPiqvWjjhaVqorG6GRCDvrdhBDsloGg
# https://www.scielo.br/j/aabc/a/TPZpmfGmztHbBjJVnqJLNBv/?lang=en
# https://repositorio-aberto.up.pt/bitstream/10216/114841/1/MonteiroCarvalho2013.pdf
hi_ref_f <- 80
hi_abv80F_dummy <- quote(ifelse(heat_index >= weathermetrics::fahrenheit.to.celsius(hi_ref_f, round = 2),1,0)) 
hi_abv80F_penalty <- 0.075
hi_abv80F_penalty_young <- 0.025

# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3377942/
walking_dist <- 1000
  
library(dplyr)

chicago.city.daily_data <- 
  read.csv("C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.daily_data.csv")

chicago.city.daily_data <- 
  chicago.city.daily_data %>%
  mutate(temp_dummy = eval(temp_abv70F_dummy),
         dp_dummy = eval(dp_abv55F_dummy),
         hi_dummy = eval(hi_abv80F_dummy),
         age_dist = dist_to_grocery*age_dummy*age_penalty,
         temp_dist = dist_to_grocery*eval(temp_abv70F_dummy)*temp_abv70F_penalty*(tmax - weathermetrics::fahrenheit.to.celsius(temp_ref_f, round = 2)),
         dp_dist = dist_to_grocery*eval(dp_abv55F_dummy)*dp_abv55F_penalty*(tdmean - weathermetrics::fahrenheit.to.celsius(dp_ref_f, round = 2)),
         hi_dist = dist_to_grocery*eval(hi_abv80F_dummy)*hi_abv80F_penalty*(heat_index - weathermetrics::fahrenheit.to.celsius(hi_ref_f, round = 2)),
         raw_dist = dist_to_grocery,
         real_feel_dist_young = 
           dist_to_grocery + 
           dist_to_grocery*eval(temp_abv70F_dummy)*temp_abv70F_penalty_young*(tmax - weathermetrics::fahrenheit.to.celsius(temp_ref_f, round = 2)),
           dist_to_grocery*eval(dp_abv55F_dummy)*dp_abv55F_penalty_young*(tdmean - weathermetrics::fahrenheit.to.celsius(dp_ref_f, round = 2)),
           dist_to_grocery*eval(hi_abv80F_dummy)*hi_abv80F_penalty_young*(heat_index - weathermetrics::fahrenheit.to.celsius(hi_ref_f, round = 2)),
         real_feel_dist = 
           dist_to_grocery + 
           age_dist +
           temp_dist + 
           dp_dist + 
           hi_dist,
         p_increase = real_feel_dist/raw_dist - 1,
         more_than_double = ifelse(p_increase > 1,1,0),
         newly_avoid_trip = ifelse(raw_dist <= walking_dist & real_feel_dist > walking_dist,1,0))

write.csv(chicago.city.daily_data,
          "C:/Users/Andrea/Desktop/repositories/real-feel-distance/intermediate_data_products/chicago.city.daily_data.real_feel.csv")






#https://www.treeequityscore.org/map/#11/41.8337/-87.7224
#https://www.treeequityscore.org/data/
#https://www.earthdefine.com/treemap/







