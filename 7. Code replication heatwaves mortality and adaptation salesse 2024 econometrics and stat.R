



#econometrics and stat des



###############




library(feols)



summary(feolss1 <-  feols(taux_mortalite_total~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#good



summary(feolss2 <-  feols(taux_mortalite_80_plus~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#good

summary(feolss3 <-  feols(taux_mortalite_75_79~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#good



summary(feolss4 <-  feols(taux_mortalite_70_74~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#good



summary(feolss5 <-  feols(taux_mortalite_65_69~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#good




summary(feolss6 <-  feols(taux_mortalite_60_64~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif



summary(feolss7 <-  feols(taux_mortalite_40_59~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif






summary(feolss8 <-  feols(taux_mortalite_20_39~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif




summary(feolss9 <-  feols(taux_mortalite_10_19~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif



summary(feolss10 <-  feols(taux_mortalite_0_9~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                           +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                           +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                           +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                           data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif




etable(feolss1, feolss2,feolss3,feolss4,feolss5,feolss6,feolss7,feolss8,feolss9,feolss10, tex = TRUE)


################################full mortalite canicule_binaire weight




summary(feolss1 <-  feols(taux_mortalite_total~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#good



summary(feolss2 <-  feols(taux_mortalite_80_plus~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#good

summary(feolss3 <-  feols(taux_mortalite_75_79~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#good



summary(feolss4 <-  feols(taux_mortalite_70_74~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#good



summary(feolss5 <-  feols(taux_mortalite_65_69~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#good




summary(feolss6 <-  feols(taux_mortalite_60_64~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif



summary(feolss7 <-  feols(taux_mortalite_40_59~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif






summary(feolss8 <-  feols(taux_mortalite_20_39~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif




summary(feolss9 <-  feols(taux_mortalite_10_19~ canicule_binaire 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif



summary(feolss10 <-  feols(taux_mortalite_0_9~ canicule_binaire 
                           +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                           +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                           +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                           data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#pas significatif




etable(feolss1, feolss2,feolss3,feolss4,feolss5,feolss6,feolss7,feolss8,feolss9,feolss10, tex = TRUE)




###########################################



base_urbain_pre2003<-filter(communes_dates_1980_2022_temperature_final_mois, year<2004)
base_urbain_post2003<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2004)




summary(feolss1 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss2 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))








summary(feolss3 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss4 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))












summary(feolss5 <-  feols(taux_mortalite_75_79~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss6 <-  feols(taux_mortalite_75_79~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))











summary(feolss7 <-  feols(taux_mortalite_homme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss8 <-  feols(taux_mortalite_homme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))














summary(feolss9 <-  feols(taux_mortalite_femme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss10 <-  feols(taux_mortalite_femme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                           +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                           +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                           +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                           data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






etable(feolss1, feolss2,feolss3,feolss4,feolss5,feolss6,feolss7,feolss8,feolss9,feolss10, tex = TRUE)
















summary(feolss5 <-  feols(taux_mortalite_40_59~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss6 <-  feols(taux_mortalite_40_59~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss1 <-  feols(taux_mortalite_40_59~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss1 <-  feols(taux_mortalite_0_9~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss1 <-  feols(taux_mortalite_60_64~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






#################################### evolution year interaction ####################







summary(feolss1 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#good



summary(feolss2 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30) * year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#pas significatif

summary(feolss3 <-  feols(taux_mortalite_75_79~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#tres peu significatif 


summary(feolss4 <-  feols(taux_mortalite_70_74~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#tres peu significatif 


summary(feolss5 <-  feols(taux_mortalite_homme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#tres peu significatif 


summary(feolss6 <-  feols(taux_mortalite_femme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)* year
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#tres peu significatif 




etable(feolss1, feolss2,feolss3,feolss4,feolss5,feolss6, tex = TRUE)









#evolution density #########################################




summary(feolss1 <-  feols(taux_mortalite_total~ 
                            
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*log(value_estimated_sum_densite)+
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))
#




summary(feolss1 <-  feols(taux_mortalite_total~ 
                            
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*log(value_estimated_sum_densite)+
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





######################################





summary(feolss1 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)*log(value_estimated_sum_densite)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss3 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)*log(value_estimated_sum_densite)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))







summary(feolss5 <-  feols(taux_mortalite_75_79~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)*log(value_estimated_sum_densite)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))







summary(feolss7 <-  feols(taux_mortalite_homme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)*log(value_estimated_sum_densite)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))








summary(feolss9 <-  feols(taux_mortalite_femme~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)*log(value_estimated_sum_densite)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





etable(feolss1,feolss3,feolss5,feolss7,feolss9, tex = TRUE)



###################### Medditeranean ##########################


#post 2003


summary(feolss1 <-  feols(taux_mortalite_total~ 
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




summary(feolss2 <-  feols(taux_mortalite_80_plus~ 
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss3 <-  feols(taux_mortalite_75_79~ 
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss4 <-  feols(taux_mortalite_homme~ 
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss5 <-  feols(taux_mortalite_femme~ 
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





etable(feolss1,feolss2,feolss3,feolss4,feolss5, tex = TRUE)







############# ROBUSTNESS LAG ############




library(dplyr)


data <- arrange(communes_dates_1980_2022_temperature_final_mois, year, mois, COM)


data <- data %>% 
  group_by(COM) %>% 
  mutate(minus_5_to_0_lag = dplyr::lag(minus_5_to_0, n = 1),
         minus_10_to_minus_5_lag = dplyr::lag(minus_10_to_minus_5, n = 1),
         minus_15_to_minus_10_lag = dplyr::lag(minus_15_to_minus_10, n = 1),
         minus_20_to_minus_15_lag = dplyr::lag(minus_20_to_minus_15, n = 1),
         zero_to_5_lag = dplyr::lag(zero_to_5, n = 1),
         five_to_10_lag = dplyr::lag(five_to_10, n = 1),
         ten_to_15_lag = dplyr::lag(ten_to_15, n = 1),
         temperature_bin_15_20_part_lag = dplyr::lag(temperature_bin_15_20_part, n = 1),
         twenty_to_25_lag = dplyr::lag(twenty_to_25, n = 1),
         twentyfive_to_28_lag = dplyr::lag(twentyfive_to_28, n = 1),
         twenty_eight_to_30_lag = dplyr::lag(twenty_eight_to_30, n = 1),
         above_30_lag = dplyr::lag(above_30, n = 1),
         below_minus_20_lag = dplyr::lag(below_minus_20, n = 1),
         
         
         
         humidity_bin_moins_20_part_lag = dplyr::lag(humidity_bin_moins_20_part, n = 1),
         humidity_bin_20_40_part_lag = dplyr::lag(humidity_bin_20_40_part, n = 1),
         humidity_bin_40_60_part_lag = dplyr::lag(humidity_bin_40_60_part, n = 1),
         humidity_bin_60_80_part_lag = dplyr::lag(humidity_bin_60_80_part, n = 1),
         humidity_bin_plus_80_part_lag = dplyr::lag(humidity_bin_plus_80_part, n = 1),
         rain_bin_0_3_part_lag = dplyr::lag(rain_bin_0_3_part, n = 1),
         rain_bin_3_10_part_lag = dplyr::lag(rain_bin_3_10_part, n = 1),
         rain_bin_10_100_part_lag = dplyr::lag(rain_bin_10_100_part, n = 1),
         rain_bin_plus_100_part_lag = dplyr::lag(rain_bin_plus_100_part, n = 1),
         rain_bin_zero_part_lag = dplyr::lag(rain_bin_zero_part, n = 1),
         wind_bin_0_3_part_lag = dplyr::lag(wind_bin_0_3_part, n = 1),
         wind_bin_3_10_part_lag = dplyr::lag(wind_bin_3_10_part, n = 1),
         wind_bin_10_20_part_lag = dplyr::lag(wind_bin_10_20_part, n = 1),
         wind_bin_plus_20_part_lag = dplyr::lag(wind_bin_plus_20_part, n = 1))






#avec les controles en lag
summary(feolss1 <-  feols(taux_mortalite_total~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 +
                            below_minus_20_lag+    minus_20_to_minus_15_lag +   minus_15_to_minus_10_lag +  minus_10_to_minus_5_lag +        minus_5_to_0_lag+     zero_to_5_lag+ five_to_10_lag +  ten_to_15_lag  +  twenty_to_25_lag +  twentyfive_to_28_lag + twenty_eight_to_30_lag +  above_30_lag  
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois+year^mois
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





#avec les controles en lag
summary(feolss2 <-  feols(taux_mortalite_80_plus~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 +
                            below_minus_20_lag+    minus_20_to_minus_15_lag +   minus_15_to_minus_10_lag +  minus_10_to_minus_5_lag +        minus_5_to_0_lag+     zero_to_5_lag+ five_to_10_lag +  ten_to_15_lag  +  twenty_to_25_lag +  twentyfive_to_28_lag + twenty_eight_to_30_lag +  above_30_lag  
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois+year^mois
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




#avec les controles en lag
summary(feolss3 <-  feols(taux_mortalite_75_79~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 +
                            below_minus_20_lag+    minus_20_to_minus_15_lag +   minus_15_to_minus_10_lag +  minus_10_to_minus_5_lag +        minus_5_to_0_lag+     zero_to_5_lag+ five_to_10_lag +  ten_to_15_lag  +  twenty_to_25_lag +  twentyfive_to_28_lag + twenty_eight_to_30_lag +  above_30_lag  
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois+year^mois
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




#avec les controles en lag
summary(feolss4 <-  feols(taux_mortalite_homme~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 +
                            below_minus_20_lag+    minus_20_to_minus_15_lag +   minus_15_to_minus_10_lag +  minus_10_to_minus_5_lag +        minus_5_to_0_lag+     zero_to_5_lag+ five_to_10_lag +  ten_to_15_lag  +  twenty_to_25_lag +  twentyfive_to_28_lag + twenty_eight_to_30_lag +  above_30_lag  
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois+year^mois
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




#avec les controles en lag
summary(feolss5 <-  feols(taux_mortalite_femme~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 +
                            below_minus_20_lag+    minus_20_to_minus_15_lag +   minus_15_to_minus_10_lag +  minus_10_to_minus_5_lag +        minus_5_to_0_lag+     zero_to_5_lag+ five_to_10_lag +  ten_to_15_lag  +  twenty_to_25_lag +  twentyfive_to_28_lag + twenty_eight_to_30_lag +  above_30_lag  
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois+year^mois
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




etable(feolss1,feolss2,feolss3,feolss4,feolss5, tex = TRUE)












######### generate  stat des #########

names(communes_dates_1980_2022_temperature_final_mois)
stat_des<-communes_dates_1980_2022_temperature_final_mois[,c(14:25,27:53,91,92)]

stat_des<-communes_dates_1980_2022_temperature_final_mois[,c(93:101,103:115)]


data <- stat_des
data<-as.data.frame(data)
library(stargazer)
variable_names <- names(data)

for (var_name in variable_names) {
  isolated_data <- na.omit(data[, var_name])
  
  isolated_data <- as.data.frame(isolated_data)
  
  stargazer(isolated_data, title = var_name, type = "latex", header = FALSE)
}









#########  robustess  canicule_binaire  fixed effect ###########




summary(feolss1 <-  feols(taux_mortalite_total~ 
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*log(value_estimated_sum_densite)+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss2 <-  feols(taux_mortalite_80_plus~ 
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*log(value_estimated_sum_densite)+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






summary(feolss3 <-  feols(taux_mortalite_total~ 
                            (canicule_binaire):log(value_estimated_sum_densite)+log(value_estimated_sum_densite)+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




summary(feolss4 <-  feols(taux_mortalite_80_plus~ 
                            (canicule_binaire):log(value_estimated_sum_densite)+log(value_estimated_sum_densite)+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





etable(feolss1,feolss2,feolss3,feolss4, tex = TRUE)





etable(feolss1,feolss2, tex = TRUE)




library(readxl)
percent_clim_by_department_and_year_lite <- read_excel("/clim data/percent_clim_by_department_and_year_lite.xlsx", 
                                                       col_types = c("text", "numeric", "numeric"))




percent_clim_by_department_and_year_lite$department <- ifelse(nchar(percent_clim_by_department_and_year_lite$department ) == 1, paste0("0", percent_clim_by_department_and_year_lite$department ), percent_clim_by_department_and_year_lite$department )

names(percent_clim_by_department_and_year_lite)[names(percent_clim_by_department_and_year_lite)=="department"]<-"DEP"

percent_clim_by_department_and_year_lite<-filter(percent_clim_by_department_and_year_lite, year==2021)
percent_clim_by_department_and_year_lite<-percent_clim_by_department_and_year_lite[,c(1,3)]


percent_clim_by_department_and_year_lite$clim_binary<-ifelse(percent_clim_by_department_and_year_lite$percent_clim >= 20, 1, 0)


base_2014<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2003)


base_2014<-left_join(base_2014,percent_clim_by_department_and_year_lite)






summary(feolss1 <-  feols(taux_mortalite_total~   (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )+
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 ): clim_binary
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss2 <-  feols(taux_mortalite_80_plus~   (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )+
                            (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 ): clim_binary
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






summary(feolss3 <-  feols(taux_mortalite_total~ 
                            (canicule_binaire):clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




summary(feolss4 <-  feols(taux_mortalite_80_plus~ 
                            (canicule_binaire):clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois[canicule_binaire]+year^mois[canicule_binaire],
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




etable(feolss1,feolss2, tex = TRUE)





etable(feolss1,feolss2, tex = TRUE)







#################### temperature maximum #####################


communes_dates_1980_2022_temperature_final_mois$temp_max_bin_inférieur_moins_10_to_0_tempmax_part<-communes_dates_1980_2022_temperature_final_mois$temp_max_bin_inférieur_moins_10_tempmax_part+ communes_dates_1980_2022_temperature_final_mois$temp_max_bin_moins10_0_tempmax_part




summary(feolss1 <-  feols(taux_mortalite_total~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






summary(feolss2 <-  feols(taux_mortalite_80_plus~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




summary(feolss3 <-  feols(taux_mortalite_75_79~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss4 <-  feols(taux_mortalite_homme~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss5 <-  feols(taux_mortalite_femme~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))







etable(feolss1, feolss2,feolss3,feolss4,feolss5, tex = TRUE)






base_urbain_post2003<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2004)



summary(feolss1 <-  feols(taux_mortalite_total~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






summary(feolss2 <-  feols(taux_mortalite_80_plus~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




summary(feolss2 <-  feols(taux_mortalite_75_plus~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




etable(feolss1, feolss2, tex = TRUE)


rm(feolss1,feolss2,feolss3,feolss4,feolss5,feolss6,feolss7,feolss8,feolss9,feolss10)






#robustness #############






summary(feolss1 <-  feols(taux_mortalite_total~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#good



summary(feolss2 <-  feols(taux_mortalite_80_plus~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




etable(feolss1, feolss2, tex = TRUE)








summary(feolss1 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)*log(value_estimated_sum_densite)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss2 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)*log(value_estimated_sum_densite)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


etable(feolss1, feolss2, tex = TRUE)






library(dplyr)


data <- arrange(communes_dates_1980_2022_temperature_final_mois, year, mois, COM)


data <- data %>% 
  group_by(COM) %>% 
  mutate(minus_5_to_0_lag = dplyr::lag(minus_5_to_0, n = 1),
         minus_10_to_minus_5_lag = dplyr::lag(minus_10_to_minus_5, n = 1),
         minus_15_to_minus_10_lag = dplyr::lag(minus_15_to_minus_10, n = 1),
         minus_20_to_minus_15_lag = dplyr::lag(minus_20_to_minus_15, n = 1),
         zero_to_5_lag = dplyr::lag(zero_to_5, n = 1),
         five_to_10_lag = dplyr::lag(five_to_10, n = 1),
         ten_to_15_lag = dplyr::lag(ten_to_15, n = 1),
         temperature_bin_15_20_part_lag = dplyr::lag(temperature_bin_15_20_part, n = 1),
         twenty_to_25_lag = dplyr::lag(twenty_to_25, n = 1),
         twentyfive_to_28_lag = dplyr::lag(twentyfive_to_28, n = 1),
         twenty_eight_to_30_lag = dplyr::lag(twenty_eight_to_30, n = 1),
         above_30_lag = dplyr::lag(above_30, n = 1),
         below_minus_20_lag = dplyr::lag(below_minus_20, n = 1),
         
         
         
         humidity_bin_moins_20_part_lag = dplyr::lag(humidity_bin_moins_20_part, n = 1),
         humidity_bin_20_40_part_lag = dplyr::lag(humidity_bin_20_40_part, n = 1),
         humidity_bin_40_60_part_lag = dplyr::lag(humidity_bin_40_60_part, n = 1),
         humidity_bin_60_80_part_lag = dplyr::lag(humidity_bin_60_80_part, n = 1),
         humidity_bin_plus_80_part_lag = dplyr::lag(humidity_bin_plus_80_part, n = 1),
         rain_bin_0_3_part_lag = dplyr::lag(rain_bin_0_3_part, n = 1),
         rain_bin_3_10_part_lag = dplyr::lag(rain_bin_3_10_part, n = 1),
         rain_bin_10_100_part_lag = dplyr::lag(rain_bin_10_100_part, n = 1),
         rain_bin_plus_100_part_lag = dplyr::lag(rain_bin_plus_100_part, n = 1),
         rain_bin_zero_part_lag = dplyr::lag(rain_bin_zero_part, n = 1),
         wind_bin_0_3_part_lag = dplyr::lag(wind_bin_0_3_part, n = 1),
         wind_bin_3_10_part_lag = dplyr::lag(wind_bin_3_10_part, n = 1),
         wind_bin_10_20_part_lag = dplyr::lag(wind_bin_10_20_part, n = 1),
         wind_bin_plus_20_part_lag = dplyr::lag(wind_bin_plus_20_part, n = 1))





#avec les controles en lag
summary(feolss1 <-  feols(taux_mortalite_total~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 +
                            below_minus_20_lag+    minus_20_to_minus_15_lag +   minus_15_to_minus_10_lag +  minus_10_to_minus_5_lag +        minus_5_to_0_lag+     zero_to_5_lag+ five_to_10_lag +  ten_to_15_lag  +  twenty_to_25_lag +  twentyfive_to_28_lag + twenty_eight_to_30_lag +  above_30_lag  
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois+DEP^year
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





#avec les controles en lag
summary(feolss2 <-  feols(taux_mortalite_80_plus~ below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 +
                            below_minus_20_lag+    minus_20_to_minus_15_lag +   minus_15_to_minus_10_lag +  minus_10_to_minus_5_lag +        minus_5_to_0_lag+     zero_to_5_lag+ five_to_10_lag +  ten_to_15_lag  +  twenty_to_25_lag +  twentyfive_to_28_lag + twenty_eight_to_30_lag +  above_30_lag  
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          
                          +humidity_bin_moins_20_part_lag+humidity_bin_40_60_part_lag+humidity_bin_60_80_part_lag+humidity_bin_plus_80_part_lag
                          
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          
                          +rain_bin_0_3_part_lag+rain_bin_10_100_part_lag+rain_bin_plus_100_part_lag+rain_bin_zero_part_lag
                          
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part
                          
                          +wind_bin_0_3_part_lag+wind_bin_10_20_part_lag+wind_bin_plus_20_part_lag
                          
                          | COM^mois+DEP^year
                          ,
                          data = data,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


etable(feolss1, feolss2, tex = TRUE)




### evolution 




base_urbain_pre2003<-filter(communes_dates_1980_2022_temperature_final_mois, year<2004)
base_urbain_post2003<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2004)




summary(feolss1 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss2 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))








summary(feolss3 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





summary(feolss4 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+DEP^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






etable(feolss1, feolss2,feolss3,feolss4, tex = TRUE)

















################### stat des


communes_dates_1980_2022_temperature_final_mois$test<- rowSums(communes_dates_1980_2022_temperature_final_mois[,c(117,105:115)])   
summary(communes_dates_1980_2022_temperature_final_mois$test)
#ok 30

communes_dates_1980_2022_temperature_final_mois <- communes_dates_1980_2022_temperature_final_mois %>%
  mutate(spring_month = ifelse(mois == 07 | mois == 08, 1, 0))


donnees_moyenne <- communes_dates_1980_2022_temperature_final_mois %>%
  group_by(spring_month) %>%
  summarize(moyenne_jours_30_plus = weighted.mean(above_30,value_estimated_population),
            moyenne_jours_28_30 = weighted.mean(twenty_eight_to_30,value_estimated_population),
            moyenne_jours_25_28 = weighted.mean(twentyfive_to_28,value_estimated_population),
            moyenne_jours_20_25 = weighted.mean(twenty_to_25,value_estimated_population),
            moyenne_jours_15_20 = weighted.mean(fifteen_to_20,value_estimated_population),
            moyenne_jours_10_15 = weighted.mean(ten_to_15,value_estimated_population),
            moyenne_jours_5_10 = weighted.mean(five_to_10,value_estimated_population),
            moyenne_jours_0_5 = weighted.mean(zero_to_5,value_estimated_population),
            moyenne_jours_minus5_0 = weighted.mean(minus_5_to_0,value_estimated_population),
            moyenne_jours_minus10_minus5 = weighted.mean(minus_10_to_minus_5,value_estimated_population),
            moyenne_jours_minus15_minus10 = weighted.mean(minus_15_to_minus_10,value_estimated_population),
            moyenne_jours_below_minus20_minus15 = weighted.mean(below_minus_20_to_minus_15,value_estimated_population))


row.names(donnees_moyenne)<-donnees_moyenne$spring_month

data_bar_ag<-donnees_moyenne[,c(2:13)]

#donnees_moyenne$test<- rowSums(donnees_moyenne[,c(2:13)])   
#ok

data_bar_ag<-data_bar_ag[,c(12,11,10,9,8,7,6,5,4,3,2,1)]


names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_below_minus20_minus15"]<-"<-20 to -15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus15_minus10"]<-"-15 to -10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus10_minus5"]<-"-10 to -5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus5_0"]<-"-5 to 0"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_0_5"]<-"0 to 5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_5_10"]<-"5 to 10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_10_15"]<-"10 to 15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_15_20"]<-"15 to 20"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_20_25"]<-"20 to 25"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_25_28"]<-"25 to 28"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_28_30"]<-"28 to 30"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_30_plus"]<-">30"

data_bar_ag<-as.matrix(data_bar_ag)


library(MetBrewer)



barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,16), col = met.brewer("Hokusai1", 2))


rownames(data_bar_ag)<-c("Other Months","July & August")


barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,16), col = met.brewer("Hokusai1", 2),ylab = "Average number of days per month")


################## month by zone ###############



communes_dates_1980_2022_temperature_final_mois <- communes_dates_1980_2022_temperature_final_mois %>%
  mutate(departement_high_temperature = ifelse(DEP == "06" | DEP == "83" |DEP == "13" |DEP == "84" |DEP == "30" |DEP == "34" |DEP == "11" |DEP == "66" |DEP == "2A" |DEP == "2B" , 1, 0))


donnees_moyenne <- communes_dates_1980_2022_temperature_final_mois %>%
  group_by(departement_high_temperature) %>%
  summarize(moyenne_jours_30_plus = weighted.mean(above_30,value_estimated_population),
            moyenne_jours_28_30 = weighted.mean(twenty_eight_to_30,value_estimated_population),
            moyenne_jours_25_28 = weighted.mean(twentyfive_to_28,value_estimated_population),
            moyenne_jours_20_25 = weighted.mean(twenty_to_25,value_estimated_population),
            moyenne_jours_15_20 = weighted.mean(fifteen_to_20,value_estimated_population),
            moyenne_jours_10_15 = weighted.mean(ten_to_15,value_estimated_population),
            moyenne_jours_5_10 = weighted.mean(five_to_10,value_estimated_population),
            moyenne_jours_0_5 = weighted.mean(zero_to_5,value_estimated_population),
            moyenne_jours_minus5_0 = weighted.mean(minus_5_to_0,value_estimated_population),
            moyenne_jours_minus10_minus5 = weighted.mean(minus_10_to_minus_5,value_estimated_population),
            moyenne_jours_minus15_minus10 = weighted.mean(minus_15_to_minus_10,value_estimated_population),
            moyenne_jours_below_minus20_minus15 = weighted.mean(below_minus_20_to_minus_15,value_estimated_population))


row.names(donnees_moyenne)<-donnees_moyenne$departement_high_temperature

data_bar_ag<-donnees_moyenne[,c(2:13)]

#donnees_moyenne$test<- rowSums(donnees_moyenne[,c(2:13)])   
#ok

data_bar_ag<-data_bar_ag[,c(12,11,10,9,8,7,6,5,4,3,2,1)]


names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_below_minus20_minus15"]<-"<-20 to -15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus15_minus10"]<-"-15 to -10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus10_minus5"]<-"-10 to -5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus5_0"]<-"-5 to 0"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_0_5"]<-"0 to 5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_5_10"]<-"5 to 10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_10_15"]<-"10 to 15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_15_20"]<-"15 to 20"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_20_25"]<-"20 to 25"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_25_28"]<-"25 to 28"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_28_30"]<-"28 to 30"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_30_plus"]<-">30"

data_bar_ag<-as.matrix(data_bar_ag)


library(MetBrewer)



barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,16), col = met.brewer("Hokusai1", 2))


rownames(data_bar_ag)<-c("Other Départements","Mediterranean départements")


barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,10), col = met.brewer("Hokusai1", 2),ylab = "Average number of days per month")


############### month global ##############



################## month by zone ###############




donnees_moyenne <- communes_dates_1980_2022_temperature_final_mois %>%
  summarize(moyenne_jours_30_plus = weighted.mean(above_30,value_estimated_population),
            moyenne_jours_28_30 = weighted.mean(twenty_eight_to_30,value_estimated_population),
            moyenne_jours_25_28 = weighted.mean(twentyfive_to_28,value_estimated_population),
            moyenne_jours_20_25 = weighted.mean(twenty_to_25,value_estimated_population),
            moyenne_jours_15_20 = weighted.mean(fifteen_to_20,value_estimated_population),
            moyenne_jours_10_15 = weighted.mean(ten_to_15,value_estimated_population),
            moyenne_jours_5_10 = weighted.mean(five_to_10,value_estimated_population),
            moyenne_jours_0_5 = weighted.mean(zero_to_5,value_estimated_population),
            moyenne_jours_minus5_0 = weighted.mean(minus_5_to_0,value_estimated_population),
            moyenne_jours_minus10_minus5 = weighted.mean(minus_10_to_minus_5,value_estimated_population),
            moyenne_jours_minus15_minus10 = weighted.mean(minus_15_to_minus_10,value_estimated_population),
            moyenne_jours_below_minus20_minus15 = weighted.mean(below_minus_20_to_minus_15,value_estimated_population))



data_bar_ag<-donnees_moyenne[,c(1:12)]

#donnees_moyenne$test<- rowSums(donnees_moyenne[,c(2:13)])   
#ok

data_bar_ag<-data_bar_ag[,c(12,11,10,9,8,7,6,5,4,3,2,1)]


names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_below_minus20_minus15"]<-"<-20 to -15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus15_minus10"]<-"-15 to -10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus10_minus5"]<-"-10 to -5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus5_0"]<-"-5 to 0"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_0_5"]<-"0 to 5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_5_10"]<-"5 to 10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_10_15"]<-"10 to 15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_15_20"]<-"15 to 20"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_20_25"]<-"20 to 25"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_25_28"]<-"25 to 28"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_28_30"]<-"28 to 30"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_30_plus"]<-">30"

data_bar_ag<-as.matrix(data_bar_ag)


library(MetBrewer)



barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,10), col = met.brewer("Degas", 1))



barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,10), col = met.brewer("Degas", 1),ylab = "Average number of days per month")







############### month periode ##############





communes_dates_1980_2022_temperature_final_mois <- communes_dates_1980_2022_temperature_final_mois %>%
  mutate(periode_high_temperature = ifelse(year>2000 , 1, 0))


donnees_moyenne <- communes_dates_1980_2022_temperature_final_mois %>%
  group_by(periode_high_temperature) %>%
  summarize(moyenne_jours_30_plus = weighted.mean(above_30,value_estimated_population),
            moyenne_jours_28_30 = weighted.mean(twenty_eight_to_30,value_estimated_population),
            moyenne_jours_25_28 = weighted.mean(twentyfive_to_28,value_estimated_population),
            moyenne_jours_20_25 = weighted.mean(twenty_to_25,value_estimated_population),
            moyenne_jours_15_20 = weighted.mean(fifteen_to_20,value_estimated_population),
            moyenne_jours_10_15 = weighted.mean(ten_to_15,value_estimated_population),
            moyenne_jours_5_10 = weighted.mean(five_to_10,value_estimated_population),
            moyenne_jours_0_5 = weighted.mean(zero_to_5,value_estimated_population),
            moyenne_jours_minus5_0 = weighted.mean(minus_5_to_0,value_estimated_population),
            moyenne_jours_minus10_minus5 = weighted.mean(minus_10_to_minus_5,value_estimated_population),
            moyenne_jours_minus15_minus10 = weighted.mean(minus_15_to_minus_10,value_estimated_population),
            moyenne_jours_below_minus20_minus15 = weighted.mean(below_minus_20_to_minus_15,value_estimated_population))


row.names(donnees_moyenne)<-donnees_moyenne$periode_high_temperature

data_bar_ag<-donnees_moyenne[,c(2:13)]

#donnees_moyenne$test<- rowSums(donnees_moyenne[,c(2:13)])   
#ok

data_bar_ag<-data_bar_ag[,c(12,11,10,9,8,7,6,5,4,3,2,1)]


names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_below_minus20_minus15"]<-"<-20 to -15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus15_minus10"]<-"-15 to -10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus10_minus5"]<-"-10 to -5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_minus5_0"]<-"-5 to 0"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_0_5"]<-"0 to 5"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_5_10"]<-"5 to 10"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_10_15"]<-"10 to 15"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_15_20"]<-"15 to 20"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_20_25"]<-"20 to 25"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_25_28"]<-"25 to 28"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_28_30"]<-"28 to 30"
names(data_bar_ag)[names(data_bar_ag)=="moyenne_jours_30_plus"]<-">30"

data_bar_ag<-as.matrix(data_bar_ag)


library(MetBrewer)



barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,10), col = met.brewer("Hokusai1", 2))


rownames(data_bar_ag)<-c("1980-2000","2001-2018")


barplot(data_bar_ag, legend = rownames(data_bar_ag), beside = TRUE, ylim = c(0,10), col = met.brewer("Hokusai1", 2),ylab = "Average number of days per month")















base_ag<-communes_dates_1980_2022_temperature_final_mois[,c(1,5,117,105:115)]

base_ag<-aggregate(.~COM+year,base_ag,sum)

base_ag$test<- rowSums(base_ag[,c(3:14)])   
#
base_ag<-filter(base_ag, base_ag$test>359.9)


base_ag$test<- rowSums(base_ag[,c(3:14)])   
summary(base_ag$test)
#ok

base_ag_2<-communes_dates_1980_2022_temperature_final_mois[,c(1,5,4)]
base_ag2<-aggregate(.~COM+year,base_ag_2,mean)

base_ag<-left_join(base_ag,base_ag2)





base_ag_mean <- weighted.mean(base_ag$below_minus_20_to_minus_15, base_ag$value_estimated_population)
base_ag_mean<-as.data.frame(base_ag_mean)

base_ag_mean$minus_15_to_minus_10 <- weighted.mean(base_ag$minus_15_to_minus_10, base_ag$value_estimated_population)
base_ag_mean$minus_10_to_minus_5 <- weighted.mean(base_ag$minus_10_to_minus_5, base_ag$value_estimated_population)
base_ag_mean$minus_5_to_0 <- weighted.mean(base_ag$minus_5_to_0, base_ag$value_estimated_population)
base_ag_mean$zero_to_5 <- weighted.mean(base_ag$zero_to_5, base_ag$value_estimated_population)
base_ag_mean$five_to_10 <- weighted.mean(base_ag$five_to_10, base_ag$value_estimated_population)
base_ag_mean$ten_to_15 <- weighted.mean(base_ag$ten_to_15, base_ag$value_estimated_population)
base_ag_mean$fifteen_to_20 <- weighted.mean(base_ag$fifteen_to_20, base_ag$value_estimated_population)
base_ag_mean$twenty_to_25 <- weighted.mean(base_ag$twenty_to_25, base_ag$value_estimated_population)
base_ag_mean$twentyfive_to_28 <- weighted.mean(base_ag$twentyfive_to_28, base_ag$value_estimated_population)
base_ag_mean$twenty_eight_to_30 <- weighted.mean(base_ag$twenty_eight_to_30, base_ag$value_estimated_population)
base_ag_mean$above_30 <- weighted.mean(base_ag$above_30, base_ag$value_estimated_population)

names(base_ag_mean)[names(base_ag_mean)=="base_ag_mean"]<-"below_minus_20_to_minus_15"

result<- rowSums(base_ag_mean[,c(1:12)])   
#c'est bon 360


names(base_ag_mean)[names(base_ag_mean)=="below_minus_20_to_minus_15"]<-"<-20 to -15"
names(base_ag_mean)[names(base_ag_mean)=="minus_15_to_minus_10"]<-"-15 to -10"
names(base_ag_mean)[names(base_ag_mean)=="minus_10_to_minus_5"]<-"-10 to -5"
names(base_ag_mean)[names(base_ag_mean)=="minus_5_to_0"]<-"-5 to 0"
names(base_ag_mean)[names(base_ag_mean)=="zero_to_5"]<-"0 to 5"
names(base_ag_mean)[names(base_ag_mean)=="five_to_10"]<-"5 to 10"
names(base_ag_mean)[names(base_ag_mean)=="ten_to_15"]<-"10 to 15"
names(base_ag_mean)[names(base_ag_mean)=="fifteen_to_20"]<-"15 to 20"
names(base_ag_mean)[names(base_ag_mean)=="twenty_to_25"]<-"20 to 25"
names(base_ag_mean)[names(base_ag_mean)=="twentyfive_to_28"]<-"25 to 28"
names(base_ag_mean)[names(base_ag_mean)=="twenty_eight_to_30"]<-"28 to 30"
names(base_ag_mean)[names(base_ag_mean)=="above_30"]<-">30"

base_ag_mean<-as.matrix(base_ag_mean)


library(MetBrewer)



barplot(base_ag_mean, legend = rownames(base_ag_mean), beside = TRUE, ylim = c(0,100), col = met.brewer("Demuth", 1), ylab = "Average number of days per year")

































################# modele de graphique pour coefficients 


summary(feolss1 <-  feols(taux_mortalite_total~ below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss1 <-  feols(taux_mortalite_80_plus~ below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#good

#good

communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_10<-communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_15+communes_dates_1980_2022_temperature_final_mois$minus_15_to_minus_10

#communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_5<-communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_10+communes_dates_1980_2022_temperature_final_mois$minus_10_to_minus_5


#communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_5<-communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_10+communes_dates_1980_2022_temperature_final_mois$minus_10_to_minus_5

#names
#communes_dates_1980_2022_temperature_final_mois$test<- rowSums(communes_dates_1980_2022_temperature_final_mois[,c(106:115,118)])   
#30 OK 


#coefplot(list(feolss1, feolss2,feolss3,feolss4,feolss5,feolss6), keep=c("temperature_bin_25_28_part","temperature_bin_28_30_part","temperature_bin_superieur_30_part"),main = "Effect on __mortality__")


#coefplot(list(feolss1), keep=c("twentyfive_to_28","twenty_eight_to_30","above_30"),main = "Effect on __mortality__")


plot<-coefplot(list(feolss1, feolss2,feolss3,feolss4,feolss5,feolss6), keep=c("twentyfive_to_28","twenty_eight_to_30","above_30"),main = "Effect on __mortality__")

legend("topright", col = 1:6, pch = 20, lwd = 1, lty = 1:6,
       legend = c("Total", "80 +", "75-79", " 70-74","65-69", "60-64"), title = "Motality")




#legend("topright", col = 1:6, pch = 20, lwd = 1, lty = 1:6,
#       legend = c("total", "80 plus", "75-79", " 70-74","65-69", "60-64"), title = "motality")






library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("below_minus_20to_minus10", names(coefficients)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2]
)

coef_df<-coef_df[c(1:10),]

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0)
coef_df<-rbind(coef_df,nouvelle_ligne)


coef_df<-coef_df[c(1,2,3,4,5,6,11,7,8,9,10),]

coef_df <- coef_df %>%
  mutate(Variable = recode(Variable,
                           "above_30" = ">30",
                           "below_minus_20to_minus10" = "<-20 to -10",
                           "five_to_10" = "5 to 10",
                           "minus_10_to_minus_5" = "-10 to -5",
                           "minus_5_to_0" = "-5 to 0",
                           "twenty_eight_to_30" = "28 to 30",
                           "twenty_to_25" = "20 to 25",
                           "twentyfive_to_28" = "25 to 28",
                           "zero_to_5" = "0 to 5",
                           "ten_to_15" = "10 to 15",
  ))

library(ggplot2)


library(ggplot2)


ggplot(coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient)) +
  geom_point() +  # Points plus gros et de couleur bleue
  geom_line(aes(group = 1), linetype = "dashed") +  # Ligne reliant les points (en pointillés)
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, group = 1) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








################### temp max coef ############





summary(feolss1 <-  feols(taux_mortalite_80_plus~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




summary(feolss1 <-  feols(taux_mortalite_total~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = communes_dates_1980_2022_temperature_final_mois,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("temp_max_bin_inférieur_moins_10_to_0_tempmax_part", names(coefficients)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2]
)

coef_df<-coef_df[c(1:7),]

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0)
coef_df<-rbind(coef_df,nouvelle_ligne)


coef_df<-coef_df[c(1,2,3,8,4,5,6,7),]

coef_df <- coef_df %>%
  mutate(Variable = recode(Variable,
                           "temp_max_bin_inférieur_moins_10_to_0_tempmax_part" = "<-10 to 0",
                           "temp_max_bin_0_10_tempmax_part" = "0 to 10",
                           "temp_max_bin_10_15_tempmax_part" = "10 to 15",
                           "temp_max_bin_20_25_tempmax_part" = "20 to 25",
                           "temp_max_bin_25_30_tempmax_part" = "25 to 30",
                           "temp_max_bin_30_35_tempmax_part" = "30 to 35",
                           "temp_max_bin_plus_35_part" = ">35"
                           
  ))

library(ggplot2)


library(ggplot2)


ggplot(coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient)) +
  geom_point() +  # Points plus gros et de couleur bleue
  geom_line(aes(group = 1), linetype = "dashed") +  # Ligne reliant les points (en pointillés)
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, group = 1) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








######### coef 2 sous echantillons ############



base_urbain_pre2003<-filter(communes_dates_1980_2022_temperature_final_mois, year<2004)
base_urbain_post2003<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2004)




summary(feolss1 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss2 <-  feols(taux_mortalite_total~ (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))


#marche tres bien






summary(feolss1 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss2 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("below_minus_20_to_minus_10", names(coefficients)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2],
  Regression = "Régression 1"
)

coef_df<-coef_df[c(1:10),]






library(fixest)
library(ggplot2)


coefficients2 <- coef(feolss2)
conf_int2 <- confint(feolss2)

coef_df2 <- data.frame(
  Variable = c("below_minus_20_to_minus_10", names(coefficients2)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients2,
  Lower_CI = conf_int2[, 1],
  Upper_CI = conf_int2[, 2],
  Regression = "Régression 2"
)

coef_df2<-coef_df2[c(1:10),]

combined_coef_df <- rbind(coef_df, coef_df2)


combined_coef_df <- combined_coef_df %>%
  mutate(Variable = recode(Variable,
                           "above_30" = ">30",
                           "below_minus_20_to_minus_10" = "<-20 to -10",
                           "five_to_10" = "5 to 10",
                           "minus_10_to_minus_5" = "-10 to -5",
                           "minus_5_to_0" = "-5 to 0",
                           "twenty_eight_to_30" = "28 to 30",
                           "twenty_to_25" = "20 to 25",
                           "twentyfive_to_28" = "25 to 28",
                           "zero_to_5" = "0 to 5",
                           "ten_to_15" = "10 to 15",
  ))



combined_coef_df <- combined_coef_df %>%
  mutate(Regression = recode(Regression,
                             "Régression 1" = "1980-2003",
                             "Régression 2" = "2004-2019"
                             
  ))


nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="1980-2003")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="2004-2019")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)


combined_coef_df<-combined_coef_df[c(1,2,3,4,5,6,21,7,8,9,10,11,12,13,22,14,15,16,17,18,19,20),]


library(ggplot2)


library(ggplot2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression)) +
  geom_point() +
  geom_line(aes(group = Regression), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")













#### coef plot AC and density #####

communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_10<-communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_15+communes_dates_1980_2022_temperature_final_mois$minus_15_to_minus_10

base_2014<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2003)


base_2014<-left_join(base_2014,percent_clim_by_department_and_year_lite)


base_2014_clim<-filter(base_2014, clim_binary==1)
base_2014_clim2<-filter(base_2014, clim_binary==0)



summary(feolss1 <-  feols(taux_mortalite_80_plus~ 
                            (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014_clim,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss2 <-  feols(taux_mortalite_80_plus~ 
                            (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014_clim2,se = "cluster", cluster = ~COM,weights=~value_estimated_population))





#moins de bins 



communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_0<-communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_10+communes_dates_1980_2022_temperature_final_mois$minus_10_to_minus_5+communes_dates_1980_2022_temperature_final_mois$minus_5_to_0
communes_dates_1980_2022_temperature_final_mois$zero_to_15<-communes_dates_1980_2022_temperature_final_mois$zero_to_5+communes_dates_1980_2022_temperature_final_mois$five_to_10+communes_dates_1980_2022_temperature_final_mois$ten_to_15
communes_dates_1980_2022_temperature_final_mois$twenty_to_28<-communes_dates_1980_2022_temperature_final_mois$twenty_to_25+communes_dates_1980_2022_temperature_final_mois$twentyfive_to_28
communes_dates_1980_2022_temperature_final_mois$above_twenty_eight<-communes_dates_1980_2022_temperature_final_mois$twenty_eight_to_30+communes_dates_1980_2022_temperature_final_mois$above_30


base_2014<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2003)


base_2014<-left_join(base_2014,percent_clim_by_department_and_year_lite)


base_2014_clim<-filter(base_2014, clim_binary==1)
base_2014_clim2<-filter(base_2014, clim_binary==0)



summary(feolss1 <-  feols(taux_mortalite_80_plus~ 
                            (below_minus_20_to_0+zero_to_15+twenty_to_28+above_twenty_eight)+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014_clim,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss2 <-  feols(taux_mortalite_80_plus~ 
                            (below_minus_20_to_0+zero_to_15+twenty_to_28+above_twenty_eight)+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014_clim2,se = "cluster", cluster = ~COM,weights=~value_estimated_population))







library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("below_minus_20_to_0", names(coefficients)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2],
  Regression = "Régression 1"
)

coef_df<-coef_df[c(1:4),]






library(fixest)
library(ggplot2)


coefficients2 <- coef(feolss2)
conf_int2 <- confint(feolss2)

coef_df2 <- data.frame(
  Variable = c("below_minus_20_to_0", names(coefficients2)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients2,
  Lower_CI = conf_int2[, 1],
  Upper_CI = conf_int2[, 2],
  Regression = "Régression 2"
)

coef_df2<-coef_df2[c(1:4),]

combined_coef_df <- rbind(coef_df, coef_df2)


combined_coef_df <- combined_coef_df %>%
  mutate(Variable = recode(Variable,
                           "below_minus_20_to_0" = "<-20 to 0",
                           "zero_to_15" = "0 to 15",
                           "twenty_to_28" = "20 to 28",
                           "above_twenty_eight" = ">28",
                           
  ))



combined_coef_df <- combined_coef_df %>%
  mutate(Regression = recode(Regression,
                             "Régression 1" = "Mediterranean",
                             "Régression 2" = "Other"
                             
  ))


nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="Mediterranean")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="Other")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)


combined_coef_df<-combined_coef_df[c(1,2,9,3,4,5,6,10,7,8,9),]


library(ggplot2)


library(ggplot2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression)) +
  geom_point() +
  geom_line(aes(group = Regression), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")















library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("below_minus_20_to_minus_10", names(coefficients)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2],
  Regression = "Régression 1"
)

coef_df<-coef_df[c(1:10),]






library(fixest)
library(ggplot2)


coefficients2 <- coef(feolss2)
conf_int2 <- confint(feolss2)

coef_df2 <- data.frame(
  Variable = c("below_minus_20_to_minus_10", names(coefficients2)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients2,
  Lower_CI = conf_int2[, 1],
  Upper_CI = conf_int2[, 2],
  Regression = "Régression 2"
)

coef_df2<-coef_df2[c(1:10),]

combined_coef_df <- rbind(coef_df, coef_df2)


combined_coef_df <- combined_coef_df %>%
  mutate(Variable = recode(Variable,
                           "above_30" = ">30",
                           "below_minus_20_to_minus_10" = "<-20 to -10",
                           "five_to_10" = "5 to 10",
                           "minus_10_to_minus_5" = "-10 to -5",
                           "minus_5_to_0" = "-5 to 0",
                           "twenty_eight_to_30" = "28 to 30",
                           "twenty_to_25" = "20 to 25",
                           "twentyfive_to_28" = "25 to 28",
                           "zero_to_5" = "0 to 5",
                           "ten_to_15" = "10 to 15",
  ))



combined_coef_df <- combined_coef_df %>%
  mutate(Regression = recode(Regression,
                             "Régression 1" = "Mediterranean",
                             "Régression 2" = "Other"
                             
  ))


nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="Mediterranean")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="Other")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)


combined_coef_df<-combined_coef_df[c(1,2,3,4,5,6,21,7,8,9,10,11,12,13,22,14,15,16,17,18,19,20),]


library(ggplot2)


library(ggplot2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression)) +
  geom_point() +
  geom_line(aes(group = Regression), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")










#avec moins de bins 


#### coef density sous echantillon ####

summary(log(communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite))
communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_10<-communes_dates_1980_2022_temperature_final_mois$below_minus_20_to_minus_15+communes_dates_1980_2022_temperature_final_mois$minus_15_to_minus_10

base_low_density<-filter(communes_dates_1980_2022_temperature_final_mois, log(communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite)<2.841)
base_high_density<-filter(communes_dates_1980_2022_temperature_final_mois, log(communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite)>4.378)
#1er et 3eme quartile
#non

#dense = 1500 habitants par km2
# dense inter = >300
#insee : https://www.insee.fr/fr/information/2114627

base_high_density<-filter(communes_dates_1980_2022_temperature_final_mois, communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite>300)
base_low_density<-filter(communes_dates_1980_2022_temperature_final_mois, communes_dates_1980_2022_temperature_final_mois$value_estimated_sum_densite<300)

summary(feolss1 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_low_density,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss2 <-  feols(taux_mortalite_80_plus~ (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_high_density,se = "cluster", cluster = ~COM,weights=~value_estimated_population))







library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("below_minus_20_to_minus_10", names(coefficients)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2],
  Regression = "Régression 1"
)

coef_df<-coef_df[c(1:10),]






library(fixest)
library(ggplot2)


coefficients2 <- coef(feolss2)
conf_int2 <- confint(feolss2)

coef_df2 <- data.frame(
  Variable = c("below_minus_20_to_minus_10", names(coefficients2)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients2,
  Lower_CI = conf_int2[, 1],
  Upper_CI = conf_int2[, 2],
  Regression = "Régression 2"
)

coef_df2<-coef_df2[c(1:10),]

combined_coef_df <- rbind(coef_df, coef_df2)


combined_coef_df <- combined_coef_df %>%
  mutate(Variable = recode(Variable,
                           "above_30" = ">30",
                           "below_minus_20_to_minus_10" = "<-20 to -10",
                           "five_to_10" = "5 to 10",
                           "minus_10_to_minus_5" = "-10 to -5",
                           "minus_5_to_0" = "-5 to 0",
                           "twenty_eight_to_30" = "28 to 30",
                           "twenty_to_25" = "20 to 25",
                           "twentyfive_to_28" = "25 to 28",
                           "zero_to_5" = "0 to 5",
                           "ten_to_15" = "10 to 15",
  ))



combined_coef_df <- combined_coef_df %>%
  mutate(Regression = recode(Regression,
                             "Régression 1" = "Low Density",
                             "Régression 2" = "High Density"
                             
  ))


nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="Low Density")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="High Density")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)


combined_coef_df<-combined_coef_df[c(1,2,3,4,5,6,21,7,8,9,10,11,12,13,22,14,15,16,17,18,19,20),]


library(ggplot2)


library(ggplot2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression)) +
  geom_point() +
  geom_line(aes(group = Regression), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")




























summary(feolss1 <-  feols(taux_mortalite_80_plus~ 
                            (below_minus_20_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30 )*clim_binary+
                            
                            +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_2014,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




library(fixest)
library(ggplot2)

coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

temp_names <- c("below_minus_20_to_minus_10", "minus_10_to_minus_5", "minus_5_to_0", "zero_to_5", 
                "five_to_10", "ten_to_15", "twenty_to_25", "twentyfive_to_28", "twenty_eight_to_30", "above_30")

temp_coefs <- coefficients[temp_names]
temp_conf_int <- conf_int[temp_names, ]

interaction_names <- paste0(temp_names, ":clim_binary")
interaction_coefs <- coefficients[interaction_names]
interaction_conf_int <- conf_int[interaction_names, ]

coef_df <- data.frame(
  Variable = temp_names,
  Coefficient_clim_0 = temp_coefs,
  Lower_CI_clim_0 = temp_conf_int[, 1],
  Upper_CI_clim_0 = temp_conf_int[, 2],
  Coefficient_clim_1 = temp_coefs + interaction_coefs,
  Lower_CI_clim_1 = temp_conf_int[, 1] + interaction_conf_int[, 1],
  Upper_CI_clim_1 = temp_conf_int[, 2] + interaction_conf_int[, 2]
)

# Ajouter une ligne pour la référence
coef_df <- rbind(coef_df, data.frame(
  Variable = "15 to 20 (REF)",
  Coefficient_clim_0 = 0,
  Lower_CI_clim_0 = 0,
  Upper_CI_clim_0 = 0,
  Coefficient_clim_1 = 0,
  Lower_CI_clim_1 = 0,
  Upper_CI_clim_1 = 0
))

coef_df$Variable <- factor(coef_df$Variable, levels = c(temp_names, "15 to 20 (REF)"))

combined_coef_df <- rbind(
  data.frame(Variable = coef_df$Variable, Coefficient = coef_df$Coefficient_clim_0, 
             Lower_CI = coef_df$Lower_CI_clim_0, Upper_CI = coef_df$Upper_CI_clim_0, 
             Regression = "clim_binary = 0"),
  data.frame(Variable = coef_df$Variable, Coefficient = coef_df$Coefficient_clim_1, 
             Lower_CI = coef_df$Lower_CI_clim_1, Upper_CI = coef_df$Upper_CI_clim_1, 
             Regression = "clim_binary = 1")
)
combined_coef_df<-combined_coef_df[c(1,2,3,4,5,6,11,7,8,9,10,12,13,14,15,16,17,22,18,19,20,21),]

library(ggplot2)


library(ggplot2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression)) +
  geom_point() +
  geom_line(aes(group = Regression), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")


######### coef 2 sous echantillons temp max ############



base_urbain_pre2003<-filter(communes_dates_1980_2022_temperature_final_mois, year<2004)
base_urbain_post2003<-filter(communes_dates_1980_2022_temperature_final_mois, year>=2004)





summary(feolss1 <-  feols(taux_mortalite_80_plus~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))



summary(feolss2 <-  feols(taux_mortalite_80_plus~ 
                            (temp_max_bin_inférieur_moins_10_to_0_tempmax_part+temp_max_bin_0_10_tempmax_part+temp_max_bin_10_15_tempmax_part+temp_max_bin_20_25_tempmax_part+temp_max_bin_25_30_tempmax_part+temp_max_bin_30_35_tempmax_part+temp_max_bin_plus_35_part)
                          
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+year^mois,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))






library(fixest)
library(ggplot2)


coefficients <- coef(feolss1)
conf_int <- confint(feolss1)

coef_df <- data.frame(
  Variable = c("temp_max_bin_inférieur_moins_10_to_0_tempmax_part", names(coefficients)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients,
  Lower_CI = conf_int[, 1],
  Upper_CI = conf_int[, 2],
  Regression = "Régression 1"
)

coef_df<-coef_df[c(1:7),]






library(fixest)
library(ggplot2)


coefficients2 <- coef(feolss2)
conf_int2 <- confint(feolss2)

coef_df2 <- data.frame(
  Variable = c("temp_max_bin_inférieur_moins_10_to_0_tempmax_part", names(coefficients2)[-1]),  # Noms des variables explicatives incluant l'intercept
  Coefficient = coefficients2,
  Lower_CI = conf_int2[, 1],
  Upper_CI = conf_int2[, 2],
  Regression = "Régression 2"
)

coef_df2<-coef_df2[c(1:7),]

combined_coef_df <- rbind(coef_df, coef_df2)


combined_coef_df <- combined_coef_df %>%
  mutate(Variable = recode(Variable,
                           "temp_max_bin_inférieur_moins_10_to_0_tempmax_part" = "<-10 to 0",
                           "temp_max_bin_0_10_tempmax_part" = "0 to 10",
                           "temp_max_bin_10_15_tempmax_part" = "10 to 15",
                           "temp_max_bin_20_25_tempmax_part" = "20 to 25",
                           "temp_max_bin_25_30_tempmax_part" = "25 to 30",
                           "temp_max_bin_30_35_tempmax_part" = "30 to 35",
                           "temp_max_bin_plus_35_part" = ">35"
  ))



combined_coef_df <- combined_coef_df %>%
  mutate(Regression = recode(Regression,
                             "Régression 1" = "1980-2003",
                             "Régression 2" = "2004-2019"
                             
  ))


nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="1980-2003")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)

nouvelle_ligne <- data.frame(Variable = "15 to 20 (REF)", Coefficient = 0,Lower_CI=0,Upper_CI=0,Regression="2004-2019")
combined_coef_df<-rbind(combined_coef_df,nouvelle_ligne)


combined_coef_df<-combined_coef_df[c(1,2,3,15,4,5,6,7,8,9,10,16,11,12,13,14),]


library(ggplot2)


library(ggplot2)

ggplot(combined_coef_df, aes(x = factor(Variable, levels = unique(Variable)), y = Coefficient, fill = Regression)) +
  geom_point() +
  geom_line(aes(group = Regression), linetype = "dashed") +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI, group = Regression), alpha = 0.2) +
  labs(title = "",
       x = "Temperature bins (°C)",
       y = "Mortality rate per 10,000") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black")





###################### MAP  ##############



library(readr)

library(rgdal)
library(sp)
library(tmap)
#shp <- readOGR(dsn = "/shapefile france/france 2019",
#               layer = "communes-20190101")

shp<-readOGR(dsn=path.expand("/shape"),
             layer="communes-20210101")




base_ag<-communes_dates_1980_2022_temperature_final_mois[,c(1,5,117,105:115)]

base_ag<-na.omit(base_ag)

base_ag<-aggregate(.~COM+year,base_ag,sum)



#aggreger a l'année puis moyenne sur 2015-2018


base_ag<-filter(base_ag, base_ag$year=="2018"|base_ag$year=="2017"|base_ag$year=="2016"|base_ag$year=="2015"|base_ag$year=="2019")

base_ag<-aggregate(.~COM,base_ag,mean)

base_ag$test<- rowSums(base_ag[,c(3:14)])   

base_ag<-filter(base_ag, base_ag$test>359.9)
#il manque des mois dans certain commmune on les sort (5%)

commune_2022_dep <- read_csv("/commune_2022_dep.csv")
commune_2022_dep<-commune_2022_dep[,c("COM","DEP","REG")]
commune_2022_dep<-na.omit(commune_2022_dep)

data2<-left_join(base_ag,commune_2022_dep)

names(data2)[names(data2)=="COM"]<-"insee"

shp1<-merge(shp,data2)


#shp1@data$REG[is.na(shp1@data$REG)]<-"01"

shp1@data$insee <- gsub("^2A", "29", shp1@data$insee)
shp1@data$insee <- gsub("^2B", "29", shp1@data$insee)


#shp1@data$DEP[shp1@data$insee=="75056"]<-"75"
#shp1@data$REG[shp1@data$insee=="75056"]<-"11"
#shp1@data$DEP[shp1@data$insee=="13055"]<-"13"
#shp1@data$REG[shp1@data$insee=="13055"]<-"93"
#shp1@data$DEP[shp1@data$insee=="69123"]<-"69"
#shp1@data$REG[shp1@data$insee=="69123"]<-"84"


#shp1 = subset(shp1, REG != "01")

#shp1 = subset(shp1, REG != "02")

#shp1 = subset(shp1, REG != "03")

#shp1 = subset(shp1, REG != "04")

#shp1 = subset(shp1, REG != "06")

shp1@data$insee<-as.numeric(shp1@data$insee)

shp1 = subset(shp1, insee<97000)




#tm1 <- tm_shape(shp1) + tm_fill("above_30", showNA = 0, title = " ", style = "pretty", n = 10)
#tm1




#tm1 <- tm_shape(shp1) + tm_fill("twenty_eight_to_30", showNA = 0, title = " ", style = "quantile", n = 10)
tm1




#tm1 <- tm_shape(shp1) + tm_fill("twentyfive_to_28", showNA = 0, title = " ", style = "quantile", n = 10)  
tm1



shp1@data$twentyfive_to_above_30<-round(shp1@data$twentyfive_to_above_30,0)

#tm1 <- tm_shape(shp1) + tm_fill("twentyfive_to_above_30", showNA = 1, title = " ", style = "quantile", n = 10)  
tm1






shp1@data$twentyfive_to_above_30<-shp1@data$twentyfive_to_28+shp1@data$twenty_eight_to_30+shp1@data$above_30

shp1@data$twentyfive_to_above_30<-round(shp1@data$twentyfive_to_above_30,0)

tm1 <- tm_shape(shp1) + tm_fill("twentyfive_to_above_30", showNA = 1, title = " ", style = "pretty", n = 10, palette = met.brewer("OKeeffe2", 10))  
tm1






library(readxl)
percent_clim_by_department_and_year_lite <- read_excel("/clim data/percent_clim_by_department_and_year_lite.xlsx", 
                                                       col_types = c("text", "numeric", "numeric"))




percent_clim_by_department_and_year_lite$department <- ifelse(nchar(percent_clim_by_department_and_year_lite$department ) == 1, paste0("0", percent_clim_by_department_and_year_lite$department ), percent_clim_by_department_and_year_lite$department )

names(percent_clim_by_department_and_year_lite)[names(percent_clim_by_department_and_year_lite)=="department"]<-"DEP"

percent_clim_by_department_and_year_lite<-filter(percent_clim_by_department_and_year_lite, year==2021)
percent_clim_by_department_and_year_lite<-percent_clim_by_department_and_year_lite[,c(1,3)]

names(percent_clim_by_department_and_year_lite)[names(percent_clim_by_department_and_year_lite)=="DEP"]<-"code"


library(sf)

chemin_fichier <- "/departement shape/contour-des-departements.geojson"

donnees_spatiales <- st_read(chemin_fichier)

donnees_spatiales<-left_join(donnees_spatiales,percent_clim_by_department_and_year_lite)

selected_polygons <- subset(donnees_spatiales, code == "06" | code == "83" |code == "13" |code == "84" |code == "30" |code == "34" |code == "11" |code == "66"  )  # Remplacez "condition" par votre critère de sélection

selected_layer <- tm_shape(selected_polygons) +
  tm_borders(lwd = 2, col = "red")  # Réglez la largeur de la ligne (lwd) et la couleur (col) comme vous le souhaitez

tm_base <- tm_shape(donnees_spatiales)+
  tm_fill("percent_clim", showNA = 1, title = " ", style = "pretty", n = 6) +tm_borders()
final_map <- tm_base + selected_layer

final_map











##### MAP drias ##############



library(readr)

library(rgdal)
library(sp)
library(tmap)
#shp <- readOGR(dsn = "/shapefile france/france 2019",
#               layer = "communes-20190101")

shp<-readOGR(dsn=path.expand("/shape"),
             layer="communes-20210101")




base_ag<-projection_drias_h1_final[,c(1,2)]

base_ag<-na.omit(base_ag)



commune_2022_dep <- read_csv("/commune_2022_dep.csv")
commune_2022_dep<-commune_2022_dep[,c("COM","DEP","REG")]
commune_2022_dep<-na.omit(commune_2022_dep)

names(commune_2022_dep)[names(commune_2022_dep)=="COM"]<-"insee"

data2<-left_join(base_ag,commune_2022_dep)


shp1<-merge(shp,data2)


#shp1@data$REG[is.na(shp1@data$REG)]<-"01"

shp1@data$insee <- gsub("^2A", "29", shp1@data$insee)
shp1@data$insee <- gsub("^2B", "29", shp1@data$insee)


#shp1@data$DEP[shp1@data$insee=="75056"]<-"75"
#shp1@data$REG[shp1@data$insee=="75056"]<-"11"
#shp1@data$DEP[shp1@data$insee=="13055"]<-"13"
#shp1@data$REG[shp1@data$insee=="13055"]<-"93"
#shp1@data$DEP[shp1@data$insee=="69123"]<-"69"
#shp1@data$REG[shp1@data$insee=="69123"]<-"84"


#shp1 = subset(shp1, REG != "01")

#shp1 = subset(shp1, REG != "02")

#shp1 = subset(shp1, REG != "03")

#shp1 = subset(shp1, REG != "04")

#shp1 = subset(shp1, REG != "06")

shp1@data$insee<-as.numeric(shp1@data$insee)

shp1 = subset(shp1, insee<97000)





tm1 <- tm_shape(shp1) + tm_fill("jours_temperature_max_35_h1", showNA = 1, title = " ",style = "fixed",breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24), palette = met.brewer("OKeeffe2", 6))  
tm1







##### carte drias ##############



library(readr)

library(rgdal)
library(sp)
library(tmap)
#shp <- readOGR(dsn = "/shapefile france/france 2019",
#               layer = "communes-20190101")

shp<-readOGR(dsn=path.expand("/shape"),
             layer="communes-20210101")




base_ag<-projection_drias_h2_final[,c(1,2)]

base_ag<-na.omit(base_ag)



commune_2022_dep <- read_csv("/commune_2022_dep.csv")
commune_2022_dep<-commune_2022_dep[,c("COM","DEP","REG")]
commune_2022_dep<-na.omit(commune_2022_dep)

names(commune_2022_dep)[names(commune_2022_dep)=="COM"]<-"insee"

data2<-left_join(base_ag,commune_2022_dep)


shp1<-merge(shp,data2)


#shp1@data$REG[is.na(shp1@data$REG)]<-"01"

shp1@data$insee <- gsub("^2A", "29", shp1@data$insee)
shp1@data$insee <- gsub("^2B", "29", shp1@data$insee)


#shp1@data$DEP[shp1@data$insee=="75056"]<-"75"
#shp1@data$REG[shp1@data$insee=="75056"]<-"11"
#shp1@data$DEP[shp1@data$insee=="13055"]<-"13"
#shp1@data$REG[shp1@data$insee=="13055"]<-"93"
#shp1@data$DEP[shp1@data$insee=="69123"]<-"69"
#shp1@data$REG[shp1@data$insee=="69123"]<-"84"


#shp1 = subset(shp1, REG != "01")

#shp1 = subset(shp1, REG != "02")

#shp1 = subset(shp1, REG != "03")

#shp1 = subset(shp1, REG != "04")

#shp1 = subset(shp1, REG != "06")

shp1@data$insee<-as.numeric(shp1@data$insee)

shp1 = subset(shp1, insee<97000)





tm1 <- tm_shape(shp1) + tm_fill("jours_temperature_max_35_h2", showNA = 1, title = " ",style = "fixed",breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24), palette = met.brewer("OKeeffe2", 6))  
tm1









##### carte drias ##############



library(readr)

library(rgdal)
library(sp)
library(tmap)
#shp <- readOGR(dsn = "/shapefile france/france 2019",
#               layer = "communes-20190101")

shp<-readOGR(dsn=path.expand("/shape"),
             layer="communes-20210101")




base_ag<-projection_drias_reference_final[,c(1,2)]

base_ag<-na.omit(base_ag)



commune_2022_dep <- read_csv("/commune_2022_dep.csv")
commune_2022_dep<-commune_2022_dep[,c("COM","DEP","REG")]
commune_2022_dep<-na.omit(commune_2022_dep)

names(commune_2022_dep)[names(commune_2022_dep)=="COM"]<-"insee"

data2<-left_join(base_ag,commune_2022_dep)


shp1<-merge(shp,data2)


#shp1@data$REG[is.na(shp1@data$REG)]<-"01"

shp1@data$insee <- gsub("^2A", "29", shp1@data$insee)
shp1@data$insee <- gsub("^2B", "29", shp1@data$insee)


#shp1@data$DEP[shp1@data$insee=="75056"]<-"75"
#shp1@data$REG[shp1@data$insee=="75056"]<-"11"
#shp1@data$DEP[shp1@data$insee=="13055"]<-"13"
#shp1@data$REG[shp1@data$insee=="13055"]<-"93"
#shp1@data$DEP[shp1@data$insee=="69123"]<-"69"
#shp1@data$REG[shp1@data$insee=="69123"]<-"84"


#shp1 = subset(shp1, REG != "01")

#shp1 = subset(shp1, REG != "02")

#shp1 = subset(shp1, REG != "03")

#shp1 = subset(shp1, REG != "04")

#shp1 = subset(shp1, REG != "06")

shp1@data$insee<-as.numeric(shp1@data$insee)

shp1 = subset(shp1, insee<97000)



#shp1@data$jours_temperature_max_35_reference<-round(shp1@data$jours_temperature_max_35_reference,0)

tm1 <- tm_shape(shp1) + tm_fill("jours_temperature_max_35_reference", showNA = 1, title = " ",style = "fixed",breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24), palette = met.brewer("OKeeffe2", 12))  
tm1






















summary(feolss1 <-  feols(taux_mortalite_75_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_pre2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))

#ok




summary(feolss2 <-  feols(taux_mortalite_75_plus~ (below_minus_20_to_minus_15+minus_15_to_minus_10+minus_10_to_minus_5+minus_5_to_0+zero_to_5+five_to_10+ten_to_15+twenty_to_25+twentyfive_to_28+twenty_eight_to_30+above_30)
                          +humidity_bin_moins_20_part+humidity_bin_40_60_part+humidity_bin_60_80_part+humidity_bin_plus_80_part
                          +rain_bin_0_3_part+rain_bin_10_100_part+rain_bin_plus_100_part+rain_bin_zero_part
                          +wind_bin_0_3_part+wind_bin_10_20_part+wind_bin_plus_20_part| COM^mois+mois^year,
                          data = base_urbain_post2003,se = "cluster", cluster = ~COM,weights=~value_estimated_population))




