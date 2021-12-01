
repository_path <- "C:/Users/Andrea/Desktop/repositories/real-feel-distance/"

load("C:/Users/Andrea/Desktop/Real Feel Distance/save_i_1_to_100_small_final.RData")
dist_to_grocery_1 <- summary_dist_to_grocery.collect_lists.df

load("C:/Users/Andrea/Desktop/Real Feel Distance/save_i_732_to_804_small_final.RData")
dist_to_grocery_2 <- summary_dist_to_grocery.collect_lists.df

dist_to_grocery <- rbind(dist_to_grocery_1[1:99,],dist_to_grocery_2)
write.csv(dist_to_grocery,paste0(repository_path,"intermediate_data_products/dist_to_grocery.csv"))












