





#database creation. For each year I compile all the data 








################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)



communes_dates_1980_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1980-01-01")
end_date <- as.Date("1980-12-31")

communes_dates_1980_2022_temperature_final_1980 <- communes_dates_1980_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1980_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1980_age_sexe.csv")


communes_dates_1980_2022_temperature_final_1980$date<-as.Date(communes_dates_1980_2022_temperature_final_1980$date)
deces.1980_age_sexe$date<-as.Date(deces.1980_age_sexe$date)



communes_dates_1980_2022_temperature_final_1980<-left_join(communes_dates_1980_2022_temperature_final_1980,deces.1980_age_sexe)

communes_dates_1980_2022_temperature_final_1980[is.na(communes_dates_1980_2022_temperature_final_1980)]<-0


communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 < -20]<-"<-20"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= -20 & communes_dates_1980_2022_temperature_final_1980$value1 < -15]<-"-20_-15"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= -15 & communes_dates_1980_2022_temperature_final_1980$value1 < -10]<-"-15_-10"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= -10 & communes_dates_1980_2022_temperature_final_1980$value1 < -5]<-"-10_-5"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= -5 & communes_dates_1980_2022_temperature_final_1980$value1 < 0]<-"-5_0"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 0 & communes_dates_1980_2022_temperature_final_1980$value1 < 5]<-"0_5"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 5 & communes_dates_1980_2022_temperature_final_1980$value1 < 10]<-"5_10"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 10 & communes_dates_1980_2022_temperature_final_1980$value1 < 15]<-"10_15"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 15 & communes_dates_1980_2022_temperature_final_1980$value1 < 20]<-"15_20"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 20 & communes_dates_1980_2022_temperature_final_1980$value1 < 25]<-"20_25"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 25 & communes_dates_1980_2022_temperature_final_1980$value1 < 28]<-"25_28"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 28 & communes_dates_1980_2022_temperature_final_1980$value1 < 30]<-"28_30"

communes_dates_1980_2022_temperature_final_1980$temperature_bin[communes_dates_1980_2022_temperature_final_1980$value1 >= 30]<-">30"


#test<-filter(communes_dates_1980_2022_temperature_final_1980, is.na(temperature_bin))
#table(communes_dates_1980_2022_temperature_final_1980$temperature_bin)

library(fastDummies)
communes_dates_1980_2022_temperature_final_1980  <- communes_dates_1980_2022_temperature_final_1980  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1980_2022_temperature_final_1980 <- communes_dates_1980_2022_temperature_final_1980 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1980_2022_temperature_final_1980 <- communes_dates_1980_2022_temperature_final_1980 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1980_2022_temperature_final_1980$same_value[is.na(communes_dates_1980_2022_temperature_final_1980$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1980_2022_temperature_final_1980$same_value <- ifelse(communes_dates_1980_2022_temperature_final_1980$temperature_bin != ">30", 0, communes_dates_1980_2022_temperature_final_1980$same_value)



communes_dates_1980_2022_temperature_final_1980$mois<-substring(communes_dates_1980_2022_temperature_final_1980$date,6,7)

communes_dates_1980_2022_temperature_final_1980<-communes_dates_1980_2022_temperature_final_1980[,-c("date","value1","temperature_bin")]

communes_dates_1980_2022_temperature_final_1980<-aggregate(.~COM+mois,communes_dates_1980_2022_temperature_final_1980,sum)



RP_1980_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1980_age_sexe_final_2")

RP_1980_age_sexe_final_2<-RP_1980_age_sexe_final_2[,c(2:15)]

names(RP_1980_age_sexe_final_2)[names(RP_1980_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1980_2022_temperature_final_1980<-left_join(communes_dates_1980_2022_temperature_final_1980,RP_1980_age_sexe_final_2)

#tests<-filter(communes_dates_1980_2022_temperature_final_1980, COM=="01001")
#tests<-filter(communes_dates_1980_2022_temperature_final_1980, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980, !is.na(value_estimated_sum_homme) )

RP_1980_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1980_CSP_final_2")

RP_1980_CSP_final_2<-RP_1980_CSP_final_2[,c(2:12)]
names(RP_1980_CSP_final_2)[names(RP_1980_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1980_CSP_final_2)[names(RP_1980_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1980_2022_temperature_final_1980<-left_join(communes_dates_1980_2022_temperature_final_1980,RP_1980_CSP_final_2)


communes_dates_1980_2022_temperature_final_1980$part_agriculteur<-communes_dates_1980_2022_temperature_final_1980$value_estimated_agriculteur/communes_dates_1980_2022_temperature_final_1980$population_actif_25_54

communes_dates_1980_2022_temperature_final_1980$part_artisan_commercant_chef_entreprise<-communes_dates_1980_2022_temperature_final_1980$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1980_2022_temperature_final_1980$population_actif_25_54

communes_dates_1980_2022_temperature_final_1980$part_cadre<-communes_dates_1980_2022_temperature_final_1980$value_estimated_cadre/communes_dates_1980_2022_temperature_final_1980$population_actif_25_54

communes_dates_1980_2022_temperature_final_1980$part_profession_intermediaire<-communes_dates_1980_2022_temperature_final_1980$value_estimated_profession_intermediaire/communes_dates_1980_2022_temperature_final_1980$population_actif_25_54

communes_dates_1980_2022_temperature_final_1980$part_employe<-communes_dates_1980_2022_temperature_final_1980$value_estimated_employe/communes_dates_1980_2022_temperature_final_1980$population_actif_25_54

communes_dates_1980_2022_temperature_final_1980$part_ouvrier<-communes_dates_1980_2022_temperature_final_1980$value_estimated_ouvrier/communes_dates_1980_2022_temperature_final_1980$population_actif_25_54

communes_dates_1980_2022_temperature_final_1980$part_chomage<-communes_dates_1980_2022_temperature_final_1980$value_estimated_au_chomage/communes_dates_1980_2022_temperature_final_1980$population_actif_25_54






communes_dates_1980_2022_temperature_final_1980$taux_mortalite_homme<-communes_dates_1980_2022_temperature_final_1980$Homme/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_homme

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_femme<-communes_dates_1980_2022_temperature_final_1980$Femme/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_femme


communes_dates_1980_2022_temperature_final_1980$taux_mortalite_0_9<-communes_dates_1980_2022_temperature_final_1980$`0-9`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_0_9_h_f

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_10_19<-communes_dates_1980_2022_temperature_final_1980$`10-19`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_10_19_h_f



communes_dates_1980_2022_temperature_final_1980$taux_mortalite_20_39<-communes_dates_1980_2022_temperature_final_1980$`20-39`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_20_39_h_f




communes_dates_1980_2022_temperature_final_1980$taux_mortalite_40_59<-communes_dates_1980_2022_temperature_final_1980$`40-59`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_40_59_h_f





communes_dates_1980_2022_temperature_final_1980$taux_mortalite_60_64<-communes_dates_1980_2022_temperature_final_1980$`60-64`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_60_64_h_f



communes_dates_1980_2022_temperature_final_1980$taux_mortalite_65_69<-communes_dates_1980_2022_temperature_final_1980$`65-69`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_65_69_h_f


communes_dates_1980_2022_temperature_final_1980$taux_mortalite_70_74<-communes_dates_1980_2022_temperature_final_1980$`70-74`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_70_74_h_f


communes_dates_1980_2022_temperature_final_1980$taux_mortalite_75_79<-communes_dates_1980_2022_temperature_final_1980$`75-79`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_75_79_h_f


communes_dates_1980_2022_temperature_final_1980$taux_mortalite_80_plus<-communes_dates_1980_2022_temperature_final_1980$`80+`/communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_80_plus_h_f


#communes_dates_1980_2022_temperature_final_1980$taux_mortalite_60_70<-(communes_dates_1980_2022_temperature_final_1980$`60-64`+communes_dates_1980_2022_temperature_final_1980$`65-69`)/(communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_60_64_h_f+communes_dates_1980_2022_temperature_final_1980$value_estimated_sum_65_69_h_f)



communes_dates_1980_2022_temperature_final_1980$mort_total<-communes_dates_1980_2022_temperature_final_1980$Femme+communes_dates_1980_2022_temperature_final_1980$Homme


communes_dates_1980_2022_temperature_final_1980$taux_mortalite_total<-communes_dates_1980_2022_temperature_final_1980$mort_total/communes_dates_1980_2022_temperature_final_1980$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980, communes_dates_1980_2022_temperature_final_1980$value_estimated_population>0)
communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980, communes_dates_1980_2022_temperature_final_1980$population_actif_25_54>0)




communes_dates_1980_2022_temperature_final_1980<- communes_dates_1980_2022_temperature_final_1980[ , !names(communes_dates_1980_2022_temperature_final_1980) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]




communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_femme))


communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(part_agriculteur))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(part_cadre))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(part_profession_intermediaire))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(part_employe))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(part_ouvrier))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(part_chomage))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_homme))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_0_9))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_10_19))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_20_39))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_40_59))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_60_64))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_65_69))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_70_74))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_75_79))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  !is.infinite(taux_mortalite_total))





#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  part_agriculteur<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_10_19<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  part_cadre<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  part_profession_intermediaire<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  part_employe<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  part_ouvrier<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  part_chomage<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_homme<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_femme<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_0_9<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_20_39<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_40_59<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_60_64<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_65_69<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_70_74<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_75_79<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_80_plus<=1)

#communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980,  taux_mortalite_total<=1)




communes_dates_1980_2022_temperature_final_1980$taux_mortalite_homme[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_homme>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_femme[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_femme>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_0_9[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_0_9>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_10_19[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_10_19>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_20_39[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_20_39>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_40_59[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_40_59>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_60_64[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_60_64>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_65_69[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_65_69>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_70_74[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_70_74>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_75_79[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_75_79>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_80_plus[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_80_plus>1]<-NA   

communes_dates_1980_2022_temperature_final_1980$taux_mortalite_total[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_total>1]<-NA   





summary(communes_dates_1980_2022_temperature_final_1980$part_agriculteur)

summary(communes_dates_1980_2022_temperature_final_1980$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1980_2022_temperature_final_1980$part_cadre)

summary(communes_dates_1980_2022_temperature_final_1980$part_profession_intermediaire)

summary(communes_dates_1980_2022_temperature_final_1980$part_employe)

summary(communes_dates_1980_2022_temperature_final_1980$part_ouvrier)

summary(communes_dates_1980_2022_temperature_final_1980$part_chomage)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_homme)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_femme)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_0_9)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_10_19)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_20_39)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_40_59)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_60_64)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_65_69)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_70_74)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_75_79)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_80_plus)

summary(communes_dates_1980_2022_temperature_final_1980$taux_mortalite_total)






fwrite(communes_dates_1980_2022_temperature_final_1980,"/données communes années/données mortalité temperature final mois new/communes_dates_1980_temperature_deces_mois.csv")

















################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1981_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1981-01-01")
end_date <- as.Date("1981-12-31")

communes_dates_1981_2022_temperature_final_1981 <- communes_dates_1981_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1981_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1981_age_sexe.csv")



communes_dates_1981_2022_temperature_final_1981$date<-as.Date(communes_dates_1981_2022_temperature_final_1981$date)
deces.1981_age_sexe$date<-as.Date(deces.1981_age_sexe$date)





communes_dates_1981_2022_temperature_final_1981<-left_join(communes_dates_1981_2022_temperature_final_1981,deces.1981_age_sexe)

communes_dates_1981_2022_temperature_final_1981[is.na(communes_dates_1981_2022_temperature_final_1981)]<-0


communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 < -20]<-"<-20"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= -20 & communes_dates_1981_2022_temperature_final_1981$value1 < -15]<-"-20_-15"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= -15 & communes_dates_1981_2022_temperature_final_1981$value1 < -10]<-"-15_-10"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= -10 & communes_dates_1981_2022_temperature_final_1981$value1 < -5]<-"-10_-5"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= -5 & communes_dates_1981_2022_temperature_final_1981$value1 < 0]<-"-5_0"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 0 & communes_dates_1981_2022_temperature_final_1981$value1 < 5]<-"0_5"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 5 & communes_dates_1981_2022_temperature_final_1981$value1 < 10]<-"5_10"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 10 & communes_dates_1981_2022_temperature_final_1981$value1 < 15]<-"10_15"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 15 & communes_dates_1981_2022_temperature_final_1981$value1 < 20]<-"15_20"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 20 & communes_dates_1981_2022_temperature_final_1981$value1 < 25]<-"20_25"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 25 & communes_dates_1981_2022_temperature_final_1981$value1 < 28]<-"25_28"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 28 & communes_dates_1981_2022_temperature_final_1981$value1 < 30]<-"28_30"

communes_dates_1981_2022_temperature_final_1981$temperature_bin[communes_dates_1981_2022_temperature_final_1981$value1 >= 30]<-">30"


#test<-filter(communes_dates_1981_2022_temperature_final_1981, is.na(temperature_bin))
#table(communes_dates_1981_2022_temperature_final_1981$temperature_bin)

library(fastDummies)
communes_dates_1981_2022_temperature_final_1981  <- communes_dates_1981_2022_temperature_final_1981  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1981_2022_temperature_final_1981 <- communes_dates_1981_2022_temperature_final_1981 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1981_2022_temperature_final_1981 <- communes_dates_1981_2022_temperature_final_1981 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1981_2022_temperature_final_1981$same_value[is.na(communes_dates_1981_2022_temperature_final_1981$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1981_2022_temperature_final_1981$same_value <- ifelse(communes_dates_1981_2022_temperature_final_1981$temperature_bin != ">30", 0, communes_dates_1981_2022_temperature_final_1981$same_value)



communes_dates_1981_2022_temperature_final_1981$mois<-substring(communes_dates_1981_2022_temperature_final_1981$date,6,7)

communes_dates_1981_2022_temperature_final_1981<-communes_dates_1981_2022_temperature_final_1981[,-c("date","value1","temperature_bin")]

communes_dates_1981_2022_temperature_final_1981<-aggregate(.~COM+mois,communes_dates_1981_2022_temperature_final_1981,sum)



RP_1981_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1981_age_sexe_final_2")

RP_1981_age_sexe_final_2<-RP_1981_age_sexe_final_2[,c(2:15)]

names(RP_1981_age_sexe_final_2)[names(RP_1981_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1981_2022_temperature_final_1981<-left_join(communes_dates_1981_2022_temperature_final_1981,RP_1981_age_sexe_final_2)

#tests<-filter(communes_dates_1981_2022_temperature_final_1981, COM=="01001")
#tests<-filter(communes_dates_1981_2022_temperature_final_1981, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981, !is.na(value_estimated_sum_homme) )

RP_1981_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1981_CSP_final_2")

RP_1981_CSP_final_2<-RP_1981_CSP_final_2[,c(2:12)]
names(RP_1981_CSP_final_2)[names(RP_1981_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1981_CSP_final_2)[names(RP_1981_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1981_2022_temperature_final_1981<-left_join(communes_dates_1981_2022_temperature_final_1981,RP_1981_CSP_final_2)


communes_dates_1981_2022_temperature_final_1981$part_agriculteur<-communes_dates_1981_2022_temperature_final_1981$value_estimated_agriculteur/communes_dates_1981_2022_temperature_final_1981$population_actif_25_54

communes_dates_1981_2022_temperature_final_1981$part_artisan_commercant_chef_entreprise<-communes_dates_1981_2022_temperature_final_1981$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1981_2022_temperature_final_1981$population_actif_25_54

communes_dates_1981_2022_temperature_final_1981$part_cadre<-communes_dates_1981_2022_temperature_final_1981$value_estimated_cadre/communes_dates_1981_2022_temperature_final_1981$population_actif_25_54

communes_dates_1981_2022_temperature_final_1981$part_profession_intermediaire<-communes_dates_1981_2022_temperature_final_1981$value_estimated_profession_intermediaire/communes_dates_1981_2022_temperature_final_1981$population_actif_25_54

communes_dates_1981_2022_temperature_final_1981$part_employe<-communes_dates_1981_2022_temperature_final_1981$value_estimated_employe/communes_dates_1981_2022_temperature_final_1981$population_actif_25_54

communes_dates_1981_2022_temperature_final_1981$part_ouvrier<-communes_dates_1981_2022_temperature_final_1981$value_estimated_ouvrier/communes_dates_1981_2022_temperature_final_1981$population_actif_25_54

communes_dates_1981_2022_temperature_final_1981$part_chomage<-communes_dates_1981_2022_temperature_final_1981$value_estimated_au_chomage/communes_dates_1981_2022_temperature_final_1981$population_actif_25_54






communes_dates_1981_2022_temperature_final_1981$taux_mortalite_homme<-communes_dates_1981_2022_temperature_final_1981$Homme/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_homme

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_femme<-communes_dates_1981_2022_temperature_final_1981$Femme/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_femme


communes_dates_1981_2022_temperature_final_1981$taux_mortalite_0_9<-communes_dates_1981_2022_temperature_final_1981$`0-9`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_0_9_h_f

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_10_19<-communes_dates_1981_2022_temperature_final_1981$`10-19`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_10_19_h_f



communes_dates_1981_2022_temperature_final_1981$taux_mortalite_20_39<-communes_dates_1981_2022_temperature_final_1981$`20-39`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_20_39_h_f




communes_dates_1981_2022_temperature_final_1981$taux_mortalite_40_59<-communes_dates_1981_2022_temperature_final_1981$`40-59`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_40_59_h_f





communes_dates_1981_2022_temperature_final_1981$taux_mortalite_60_64<-communes_dates_1981_2022_temperature_final_1981$`60-64`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_60_64_h_f



communes_dates_1981_2022_temperature_final_1981$taux_mortalite_65_69<-communes_dates_1981_2022_temperature_final_1981$`65-69`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_65_69_h_f


communes_dates_1981_2022_temperature_final_1981$taux_mortalite_70_74<-communes_dates_1981_2022_temperature_final_1981$`70-74`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_70_74_h_f


communes_dates_1981_2022_temperature_final_1981$taux_mortalite_75_79<-communes_dates_1981_2022_temperature_final_1981$`75-79`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_75_79_h_f


communes_dates_1981_2022_temperature_final_1981$taux_mortalite_80_plus<-communes_dates_1981_2022_temperature_final_1981$`80+`/communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_80_plus_h_f


#communes_dates_1981_2022_temperature_final_1981$taux_mortalite_60_70<-(communes_dates_1981_2022_temperature_final_1981$`60-64`+communes_dates_1981_2022_temperature_final_1981$`65-69`)/(communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_60_64_h_f+communes_dates_1981_2022_temperature_final_1981$value_estimated_sum_65_69_h_f)



communes_dates_1981_2022_temperature_final_1981$mort_total<-communes_dates_1981_2022_temperature_final_1981$Femme+communes_dates_1981_2022_temperature_final_1981$Homme


communes_dates_1981_2022_temperature_final_1981$taux_mortalite_total<-communes_dates_1981_2022_temperature_final_1981$mort_total/communes_dates_1981_2022_temperature_final_1981$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981, communes_dates_1981_2022_temperature_final_1981$value_estimated_population>0)
communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981, communes_dates_1981_2022_temperature_final_1981$population_actif_25_54>0)




communes_dates_1981_2022_temperature_final_1981<- communes_dates_1981_2022_temperature_final_1981[ , !names(communes_dates_1981_2022_temperature_final_1981) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_femme))


communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(part_agriculteur))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(part_cadre))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(part_profession_intermediaire))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(part_employe))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(part_ouvrier))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(part_chomage))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_homme))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_0_9))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_10_19))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_20_39))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_40_59))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_60_64))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_65_69))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_70_74))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_75_79))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1981_2022_temperature_final_1981<-communes_dates_1981_2022_temperature_final_1981[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  part_agriculteur<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_10_19<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  part_cadre<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  part_profession_intermediaire<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  part_employe<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  part_ouvrier<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  part_chomage<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_homme<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_femme<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_0_9<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_20_39<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_40_59<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_60_64<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_65_69<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_70_74<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_75_79<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_80_plus<=1)

#communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981,  taux_mortalite_total<=1)




communes_dates_1981_2022_temperature_final_1981$taux_mortalite_homme[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_homme>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_femme[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_femme>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_0_9[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_0_9>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_10_19[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_10_19>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_20_39[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_20_39>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_40_59[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_40_59>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_60_64[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_60_64>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_65_69[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_65_69>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_70_74[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_70_74>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_75_79[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_75_79>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_80_plus[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_80_plus>1]<-NA   

communes_dates_1981_2022_temperature_final_1981$taux_mortalite_total[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_total>1]<-NA   





summary(communes_dates_1981_2022_temperature_final_1981$part_agriculteur)

summary(communes_dates_1981_2022_temperature_final_1981$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1981_2022_temperature_final_1981$part_cadre)

summary(communes_dates_1981_2022_temperature_final_1981$part_profession_intermediaire)

summary(communes_dates_1981_2022_temperature_final_1981$part_employe)

summary(communes_dates_1981_2022_temperature_final_1981$part_ouvrier)

summary(communes_dates_1981_2022_temperature_final_1981$part_chomage)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_homme)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_femme)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_0_9)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_10_19)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_20_39)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_40_59)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_60_64)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_65_69)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_70_74)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_75_79)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_80_plus)

summary(communes_dates_1981_2022_temperature_final_1981$taux_mortalite_total)






fwrite(communes_dates_1981_2022_temperature_final_1981,"/données communes années/données mortalité temperature final mois new/communes_dates_1981_temperature_deces_mois.csv")


















################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1982_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1982-01-01")
end_date <- as.Date("1982-12-31")

communes_dates_1982_2022_temperature_final_1982 <- communes_dates_1982_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1982_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1982_age_sexe.csv")



communes_dates_1982_2022_temperature_final_1982$date<-as.Date(communes_dates_1982_2022_temperature_final_1982$date)
deces.1982_age_sexe$date<-as.Date(deces.1982_age_sexe$date)





communes_dates_1982_2022_temperature_final_1982<-left_join(communes_dates_1982_2022_temperature_final_1982,deces.1982_age_sexe)

communes_dates_1982_2022_temperature_final_1982[is.na(communes_dates_1982_2022_temperature_final_1982)]<-0


communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 < -20]<-"<-20"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= -20 & communes_dates_1982_2022_temperature_final_1982$value1 < -15]<-"-20_-15"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= -15 & communes_dates_1982_2022_temperature_final_1982$value1 < -10]<-"-15_-10"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= -10 & communes_dates_1982_2022_temperature_final_1982$value1 < -5]<-"-10_-5"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= -5 & communes_dates_1982_2022_temperature_final_1982$value1 < 0]<-"-5_0"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 0 & communes_dates_1982_2022_temperature_final_1982$value1 < 5]<-"0_5"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 5 & communes_dates_1982_2022_temperature_final_1982$value1 < 10]<-"5_10"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 10 & communes_dates_1982_2022_temperature_final_1982$value1 < 15]<-"10_15"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 15 & communes_dates_1982_2022_temperature_final_1982$value1 < 20]<-"15_20"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 20 & communes_dates_1982_2022_temperature_final_1982$value1 < 25]<-"20_25"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 25 & communes_dates_1982_2022_temperature_final_1982$value1 < 28]<-"25_28"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 28 & communes_dates_1982_2022_temperature_final_1982$value1 < 30]<-"28_30"

communes_dates_1982_2022_temperature_final_1982$temperature_bin[communes_dates_1982_2022_temperature_final_1982$value1 >= 30]<-">30"


#test<-filter(communes_dates_1982_2022_temperature_final_1982, is.na(temperature_bin))
#table(communes_dates_1982_2022_temperature_final_1982$temperature_bin)

library(fastDummies)
communes_dates_1982_2022_temperature_final_1982  <- communes_dates_1982_2022_temperature_final_1982  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1982_2022_temperature_final_1982 <- communes_dates_1982_2022_temperature_final_1982 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1982_2022_temperature_final_1982 <- communes_dates_1982_2022_temperature_final_1982 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1982_2022_temperature_final_1982$same_value[is.na(communes_dates_1982_2022_temperature_final_1982$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1982_2022_temperature_final_1982$same_value <- ifelse(communes_dates_1982_2022_temperature_final_1982$temperature_bin != ">30", 0, communes_dates_1982_2022_temperature_final_1982$same_value)



communes_dates_1982_2022_temperature_final_1982$mois<-substring(communes_dates_1982_2022_temperature_final_1982$date,6,7)

communes_dates_1982_2022_temperature_final_1982<-communes_dates_1982_2022_temperature_final_1982[,-c("date","value1","temperature_bin")]

communes_dates_1982_2022_temperature_final_1982<-aggregate(.~COM+mois,communes_dates_1982_2022_temperature_final_1982,sum)



RP_1982_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final_2")

RP_1982_age_sexe_final_2<-RP_1982_age_sexe_final_2[,c(2:15)]

names(RP_1982_age_sexe_final_2)[names(RP_1982_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1982_2022_temperature_final_1982<-left_join(communes_dates_1982_2022_temperature_final_1982,RP_1982_age_sexe_final_2)

#tests<-filter(communes_dates_1982_2022_temperature_final_1982, COM=="01001")
#tests<-filter(communes_dates_1982_2022_temperature_final_1982, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982, !is.na(value_estimated_sum_homme) )

RP_1982_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1982_CSP_final_2")

RP_1982_CSP_final_2<-RP_1982_CSP_final_2[,c(2:12)]
names(RP_1982_CSP_final_2)[names(RP_1982_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1982_CSP_final_2)[names(RP_1982_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1982_2022_temperature_final_1982<-left_join(communes_dates_1982_2022_temperature_final_1982,RP_1982_CSP_final_2)


communes_dates_1982_2022_temperature_final_1982$part_agriculteur<-communes_dates_1982_2022_temperature_final_1982$value_estimated_agriculteur/communes_dates_1982_2022_temperature_final_1982$population_actif_25_54

communes_dates_1982_2022_temperature_final_1982$part_artisan_commercant_chef_entreprise<-communes_dates_1982_2022_temperature_final_1982$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1982_2022_temperature_final_1982$population_actif_25_54

communes_dates_1982_2022_temperature_final_1982$part_cadre<-communes_dates_1982_2022_temperature_final_1982$value_estimated_cadre/communes_dates_1982_2022_temperature_final_1982$population_actif_25_54

communes_dates_1982_2022_temperature_final_1982$part_profession_intermediaire<-communes_dates_1982_2022_temperature_final_1982$value_estimated_profession_intermediaire/communes_dates_1982_2022_temperature_final_1982$population_actif_25_54

communes_dates_1982_2022_temperature_final_1982$part_employe<-communes_dates_1982_2022_temperature_final_1982$value_estimated_employe/communes_dates_1982_2022_temperature_final_1982$population_actif_25_54

communes_dates_1982_2022_temperature_final_1982$part_ouvrier<-communes_dates_1982_2022_temperature_final_1982$value_estimated_ouvrier/communes_dates_1982_2022_temperature_final_1982$population_actif_25_54

communes_dates_1982_2022_temperature_final_1982$part_chomage<-communes_dates_1982_2022_temperature_final_1982$value_estimated_au_chomage/communes_dates_1982_2022_temperature_final_1982$population_actif_25_54






communes_dates_1982_2022_temperature_final_1982$taux_mortalite_homme<-communes_dates_1982_2022_temperature_final_1982$Homme/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_homme

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_femme<-communes_dates_1982_2022_temperature_final_1982$Femme/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_femme


communes_dates_1982_2022_temperature_final_1982$taux_mortalite_0_9<-communes_dates_1982_2022_temperature_final_1982$`0-9`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_0_9_h_f

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_10_19<-communes_dates_1982_2022_temperature_final_1982$`10-19`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_10_19_h_f



communes_dates_1982_2022_temperature_final_1982$taux_mortalite_20_39<-communes_dates_1982_2022_temperature_final_1982$`20-39`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_20_39_h_f




communes_dates_1982_2022_temperature_final_1982$taux_mortalite_40_59<-communes_dates_1982_2022_temperature_final_1982$`40-59`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_40_59_h_f





communes_dates_1982_2022_temperature_final_1982$taux_mortalite_60_64<-communes_dates_1982_2022_temperature_final_1982$`60-64`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_60_64_h_f



communes_dates_1982_2022_temperature_final_1982$taux_mortalite_65_69<-communes_dates_1982_2022_temperature_final_1982$`65-69`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_65_69_h_f


communes_dates_1982_2022_temperature_final_1982$taux_mortalite_70_74<-communes_dates_1982_2022_temperature_final_1982$`70-74`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_70_74_h_f


communes_dates_1982_2022_temperature_final_1982$taux_mortalite_75_79<-communes_dates_1982_2022_temperature_final_1982$`75-79`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_75_79_h_f


communes_dates_1982_2022_temperature_final_1982$taux_mortalite_80_plus<-communes_dates_1982_2022_temperature_final_1982$`80+`/communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_80_plus_h_f


#communes_dates_1982_2022_temperature_final_1982$taux_mortalite_60_70<-(communes_dates_1982_2022_temperature_final_1982$`60-64`+communes_dates_1982_2022_temperature_final_1982$`65-69`)/(communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_60_64_h_f+communes_dates_1982_2022_temperature_final_1982$value_estimated_sum_65_69_h_f)



communes_dates_1982_2022_temperature_final_1982$mort_total<-communes_dates_1982_2022_temperature_final_1982$Femme+communes_dates_1982_2022_temperature_final_1982$Homme


communes_dates_1982_2022_temperature_final_1982$taux_mortalite_total<-communes_dates_1982_2022_temperature_final_1982$mort_total/communes_dates_1982_2022_temperature_final_1982$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982, communes_dates_1982_2022_temperature_final_1982$value_estimated_population>0)
communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982, communes_dates_1982_2022_temperature_final_1982$population_actif_25_54>0)




communes_dates_1982_2022_temperature_final_1982<- communes_dates_1982_2022_temperature_final_1982[ , !names(communes_dates_1982_2022_temperature_final_1982) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_femme))


communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(part_agriculteur))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(part_cadre))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(part_profession_intermediaire))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(part_employe))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(part_ouvrier))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(part_chomage))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_homme))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_0_9))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_10_19))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_20_39))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_40_59))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_60_64))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_65_69))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_70_74))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_75_79))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1982_2022_temperature_final_1982<-communes_dates_1982_2022_temperature_final_1982[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  part_agriculteur<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_10_19<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  part_cadre<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  part_profession_intermediaire<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  part_employe<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  part_ouvrier<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  part_chomage<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_homme<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_femme<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_0_9<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_20_39<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_40_59<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_60_64<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_65_69<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_70_74<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_75_79<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_80_plus<=1)

#communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982,  taux_mortalite_total<=1)




communes_dates_1982_2022_temperature_final_1982$taux_mortalite_homme[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_homme>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_femme[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_femme>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_0_9[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_0_9>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_10_19[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_10_19>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_20_39[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_20_39>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_40_59[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_40_59>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_60_64[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_60_64>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_65_69[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_65_69>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_70_74[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_70_74>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_75_79[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_75_79>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_80_plus[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_80_plus>1]<-NA   

communes_dates_1982_2022_temperature_final_1982$taux_mortalite_total[communes_dates_1982_2022_temperature_final_1982$taux_mortalite_total>1]<-NA   





summary(communes_dates_1982_2022_temperature_final_1982$part_agriculteur)

summary(communes_dates_1982_2022_temperature_final_1982$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1982_2022_temperature_final_1982$part_cadre)

summary(communes_dates_1982_2022_temperature_final_1982$part_profession_intermediaire)

summary(communes_dates_1982_2022_temperature_final_1982$part_employe)

summary(communes_dates_1982_2022_temperature_final_1982$part_ouvrier)

summary(communes_dates_1982_2022_temperature_final_1982$part_chomage)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_homme)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_femme)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_0_9)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_10_19)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_20_39)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_40_59)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_60_64)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_65_69)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_70_74)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_75_79)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_80_plus)

summary(communes_dates_1982_2022_temperature_final_1982$taux_mortalite_total)






fwrite(communes_dates_1982_2022_temperature_final_1982,"/données communes années/données mortalité temperature final mois new/communes_dates_1982_temperature_deces_mois.csv")



















################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1983_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1983-01-01")
end_date <- as.Date("1983-12-31")

communes_dates_1983_2022_temperature_final_1983 <- communes_dates_1983_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1983_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1983_age_sexe.csv")




communes_dates_1983_2022_temperature_final_1983$date<-as.Date(communes_dates_1983_2022_temperature_final_1983$date)
deces.1983_age_sexe$date<-as.Date(deces.1983_age_sexe$date)






communes_dates_1983_2022_temperature_final_1983<-left_join(communes_dates_1983_2022_temperature_final_1983,deces.1983_age_sexe)

communes_dates_1983_2022_temperature_final_1983[is.na(communes_dates_1983_2022_temperature_final_1983)]<-0


communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 < -20]<-"<-20"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= -20 & communes_dates_1983_2022_temperature_final_1983$value1 < -15]<-"-20_-15"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= -15 & communes_dates_1983_2022_temperature_final_1983$value1 < -10]<-"-15_-10"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= -10 & communes_dates_1983_2022_temperature_final_1983$value1 < -5]<-"-10_-5"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= -5 & communes_dates_1983_2022_temperature_final_1983$value1 < 0]<-"-5_0"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 0 & communes_dates_1983_2022_temperature_final_1983$value1 < 5]<-"0_5"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 5 & communes_dates_1983_2022_temperature_final_1983$value1 < 10]<-"5_10"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 10 & communes_dates_1983_2022_temperature_final_1983$value1 < 15]<-"10_15"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 15 & communes_dates_1983_2022_temperature_final_1983$value1 < 20]<-"15_20"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 20 & communes_dates_1983_2022_temperature_final_1983$value1 < 25]<-"20_25"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 25 & communes_dates_1983_2022_temperature_final_1983$value1 < 28]<-"25_28"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 28 & communes_dates_1983_2022_temperature_final_1983$value1 < 30]<-"28_30"

communes_dates_1983_2022_temperature_final_1983$temperature_bin[communes_dates_1983_2022_temperature_final_1983$value1 >= 30]<-">30"


#test<-filter(communes_dates_1983_2022_temperature_final_1983, is.na(temperature_bin))
#table(communes_dates_1983_2022_temperature_final_1983$temperature_bin)

library(fastDummies)
communes_dates_1983_2022_temperature_final_1983  <- communes_dates_1983_2022_temperature_final_1983  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1983_2022_temperature_final_1983 <- communes_dates_1983_2022_temperature_final_1983 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1983_2022_temperature_final_1983 <- communes_dates_1983_2022_temperature_final_1983 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1983_2022_temperature_final_1983$same_value[is.na(communes_dates_1983_2022_temperature_final_1983$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1983_2022_temperature_final_1983$same_value <- ifelse(communes_dates_1983_2022_temperature_final_1983$temperature_bin != ">30", 0, communes_dates_1983_2022_temperature_final_1983$same_value)



communes_dates_1983_2022_temperature_final_1983$mois<-substring(communes_dates_1983_2022_temperature_final_1983$date,6,7)

communes_dates_1983_2022_temperature_final_1983<-communes_dates_1983_2022_temperature_final_1983[,-c("date","value1","temperature_bin")]

communes_dates_1983_2022_temperature_final_1983<-aggregate(.~COM+mois,communes_dates_1983_2022_temperature_final_1983,sum)



RP_1983_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1983_age_sexe_final_2")

RP_1983_age_sexe_final_2<-RP_1983_age_sexe_final_2[,c(2:15)]

names(RP_1983_age_sexe_final_2)[names(RP_1983_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1983_2022_temperature_final_1983<-left_join(communes_dates_1983_2022_temperature_final_1983,RP_1983_age_sexe_final_2)

#tests<-filter(communes_dates_1983_2022_temperature_final_1983, COM=="01001")
#tests<-filter(communes_dates_1983_2022_temperature_final_1983, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983, !is.na(value_estimated_sum_homme) )

RP_1983_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1983_CSP_final_2")

RP_1983_CSP_final_2<-RP_1983_CSP_final_2[,c(2:12)]
names(RP_1983_CSP_final_2)[names(RP_1983_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1983_CSP_final_2)[names(RP_1983_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1983_2022_temperature_final_1983<-left_join(communes_dates_1983_2022_temperature_final_1983,RP_1983_CSP_final_2)


communes_dates_1983_2022_temperature_final_1983$part_agriculteur<-communes_dates_1983_2022_temperature_final_1983$value_estimated_agriculteur/communes_dates_1983_2022_temperature_final_1983$population_actif_25_54

communes_dates_1983_2022_temperature_final_1983$part_artisan_commercant_chef_entreprise<-communes_dates_1983_2022_temperature_final_1983$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1983_2022_temperature_final_1983$population_actif_25_54

communes_dates_1983_2022_temperature_final_1983$part_cadre<-communes_dates_1983_2022_temperature_final_1983$value_estimated_cadre/communes_dates_1983_2022_temperature_final_1983$population_actif_25_54

communes_dates_1983_2022_temperature_final_1983$part_profession_intermediaire<-communes_dates_1983_2022_temperature_final_1983$value_estimated_profession_intermediaire/communes_dates_1983_2022_temperature_final_1983$population_actif_25_54

communes_dates_1983_2022_temperature_final_1983$part_employe<-communes_dates_1983_2022_temperature_final_1983$value_estimated_employe/communes_dates_1983_2022_temperature_final_1983$population_actif_25_54

communes_dates_1983_2022_temperature_final_1983$part_ouvrier<-communes_dates_1983_2022_temperature_final_1983$value_estimated_ouvrier/communes_dates_1983_2022_temperature_final_1983$population_actif_25_54

communes_dates_1983_2022_temperature_final_1983$part_chomage<-communes_dates_1983_2022_temperature_final_1983$value_estimated_au_chomage/communes_dates_1983_2022_temperature_final_1983$population_actif_25_54






communes_dates_1983_2022_temperature_final_1983$taux_mortalite_homme<-communes_dates_1983_2022_temperature_final_1983$Homme/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_homme

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_femme<-communes_dates_1983_2022_temperature_final_1983$Femme/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_femme


communes_dates_1983_2022_temperature_final_1983$taux_mortalite_0_9<-communes_dates_1983_2022_temperature_final_1983$`0-9`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_0_9_h_f

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_10_19<-communes_dates_1983_2022_temperature_final_1983$`10-19`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_10_19_h_f



communes_dates_1983_2022_temperature_final_1983$taux_mortalite_20_39<-communes_dates_1983_2022_temperature_final_1983$`20-39`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_20_39_h_f




communes_dates_1983_2022_temperature_final_1983$taux_mortalite_40_59<-communes_dates_1983_2022_temperature_final_1983$`40-59`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_40_59_h_f





communes_dates_1983_2022_temperature_final_1983$taux_mortalite_60_64<-communes_dates_1983_2022_temperature_final_1983$`60-64`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_60_64_h_f



communes_dates_1983_2022_temperature_final_1983$taux_mortalite_65_69<-communes_dates_1983_2022_temperature_final_1983$`65-69`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_65_69_h_f


communes_dates_1983_2022_temperature_final_1983$taux_mortalite_70_74<-communes_dates_1983_2022_temperature_final_1983$`70-74`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_70_74_h_f


communes_dates_1983_2022_temperature_final_1983$taux_mortalite_75_79<-communes_dates_1983_2022_temperature_final_1983$`75-79`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_75_79_h_f


communes_dates_1983_2022_temperature_final_1983$taux_mortalite_80_plus<-communes_dates_1983_2022_temperature_final_1983$`80+`/communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_80_plus_h_f


#communes_dates_1983_2022_temperature_final_1983$taux_mortalite_60_70<-(communes_dates_1983_2022_temperature_final_1983$`60-64`+communes_dates_1983_2022_temperature_final_1983$`65-69`)/(communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_60_64_h_f+communes_dates_1983_2022_temperature_final_1983$value_estimated_sum_65_69_h_f)



communes_dates_1983_2022_temperature_final_1983$mort_total<-communes_dates_1983_2022_temperature_final_1983$Femme+communes_dates_1983_2022_temperature_final_1983$Homme


communes_dates_1983_2022_temperature_final_1983$taux_mortalite_total<-communes_dates_1983_2022_temperature_final_1983$mort_total/communes_dates_1983_2022_temperature_final_1983$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983, communes_dates_1983_2022_temperature_final_1983$value_estimated_population>0)
communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983, communes_dates_1983_2022_temperature_final_1983$population_actif_25_54>0)




communes_dates_1983_2022_temperature_final_1983<- communes_dates_1983_2022_temperature_final_1983[ , !names(communes_dates_1983_2022_temperature_final_1983) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_femme))


communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(part_agriculteur))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(part_cadre))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(part_profession_intermediaire))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(part_employe))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(part_ouvrier))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(part_chomage))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_homme))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_0_9))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_10_19))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_20_39))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_40_59))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_60_64))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_65_69))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_70_74))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_75_79))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1983_2022_temperature_final_1983<-communes_dates_1983_2022_temperature_final_1983[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  part_agriculteur<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_10_19<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  part_cadre<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  part_profession_intermediaire<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  part_employe<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  part_ouvrier<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  part_chomage<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_homme<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_femme<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_0_9<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_20_39<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_40_59<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_60_64<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_65_69<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_70_74<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_75_79<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_80_plus<=1)

#communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983,  taux_mortalite_total<=1)




communes_dates_1983_2022_temperature_final_1983$taux_mortalite_homme[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_homme>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_femme[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_femme>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_0_9[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_0_9>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_10_19[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_10_19>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_20_39[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_20_39>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_40_59[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_40_59>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_60_64[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_60_64>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_65_69[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_65_69>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_70_74[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_70_74>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_75_79[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_75_79>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_80_plus[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_80_plus>1]<-NA   

communes_dates_1983_2022_temperature_final_1983$taux_mortalite_total[communes_dates_1983_2022_temperature_final_1983$taux_mortalite_total>1]<-NA   





summary(communes_dates_1983_2022_temperature_final_1983$part_agriculteur)

summary(communes_dates_1983_2022_temperature_final_1983$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1983_2022_temperature_final_1983$part_cadre)

summary(communes_dates_1983_2022_temperature_final_1983$part_profession_intermediaire)

summary(communes_dates_1983_2022_temperature_final_1983$part_employe)

summary(communes_dates_1983_2022_temperature_final_1983$part_ouvrier)

summary(communes_dates_1983_2022_temperature_final_1983$part_chomage)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_homme)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_femme)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_0_9)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_10_19)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_20_39)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_40_59)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_60_64)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_65_69)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_70_74)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_75_79)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_80_plus)

summary(communes_dates_1983_2022_temperature_final_1983$taux_mortalite_total)






fwrite(communes_dates_1983_2022_temperature_final_1983,"/données communes années/données mortalité temperature final mois new/communes_dates_1983_temperature_deces_mois.csv")











################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1984_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1984-01-01")
end_date <- as.Date("1984-12-31")

communes_dates_1984_2022_temperature_final_1984 <- communes_dates_1984_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1984_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1984_age_sexe.csv")




communes_dates_1984_2022_temperature_final_1984$date<-as.Date(communes_dates_1984_2022_temperature_final_1984$date)
deces.1984_age_sexe$date<-as.Date(deces.1984_age_sexe$date)





communes_dates_1984_2022_temperature_final_1984<-left_join(communes_dates_1984_2022_temperature_final_1984,deces.1984_age_sexe)

communes_dates_1984_2022_temperature_final_1984[is.na(communes_dates_1984_2022_temperature_final_1984)]<-0


communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 < -20]<-"<-20"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= -20 & communes_dates_1984_2022_temperature_final_1984$value1 < -15]<-"-20_-15"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= -15 & communes_dates_1984_2022_temperature_final_1984$value1 < -10]<-"-15_-10"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= -10 & communes_dates_1984_2022_temperature_final_1984$value1 < -5]<-"-10_-5"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= -5 & communes_dates_1984_2022_temperature_final_1984$value1 < 0]<-"-5_0"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 0 & communes_dates_1984_2022_temperature_final_1984$value1 < 5]<-"0_5"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 5 & communes_dates_1984_2022_temperature_final_1984$value1 < 10]<-"5_10"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 10 & communes_dates_1984_2022_temperature_final_1984$value1 < 15]<-"10_15"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 15 & communes_dates_1984_2022_temperature_final_1984$value1 < 20]<-"15_20"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 20 & communes_dates_1984_2022_temperature_final_1984$value1 < 25]<-"20_25"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 25 & communes_dates_1984_2022_temperature_final_1984$value1 < 28]<-"25_28"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 28 & communes_dates_1984_2022_temperature_final_1984$value1 < 30]<-"28_30"

communes_dates_1984_2022_temperature_final_1984$temperature_bin[communes_dates_1984_2022_temperature_final_1984$value1 >= 30]<-">30"


#test<-filter(communes_dates_1984_2022_temperature_final_1984, is.na(temperature_bin))
#table(communes_dates_1984_2022_temperature_final_1984$temperature_bin)

library(fastDummies)
communes_dates_1984_2022_temperature_final_1984  <- communes_dates_1984_2022_temperature_final_1984  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1984_2022_temperature_final_1984 <- communes_dates_1984_2022_temperature_final_1984 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1984_2022_temperature_final_1984 <- communes_dates_1984_2022_temperature_final_1984 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1984_2022_temperature_final_1984$same_value[is.na(communes_dates_1984_2022_temperature_final_1984$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1984_2022_temperature_final_1984$same_value <- ifelse(communes_dates_1984_2022_temperature_final_1984$temperature_bin != ">30", 0, communes_dates_1984_2022_temperature_final_1984$same_value)



communes_dates_1984_2022_temperature_final_1984$mois<-substring(communes_dates_1984_2022_temperature_final_1984$date,6,7)

communes_dates_1984_2022_temperature_final_1984<-communes_dates_1984_2022_temperature_final_1984[,-c("date","value1","temperature_bin")]

communes_dates_1984_2022_temperature_final_1984<-aggregate(.~COM+mois,communes_dates_1984_2022_temperature_final_1984,sum)



RP_1984_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1984_age_sexe_final_2")

RP_1984_age_sexe_final_2<-RP_1984_age_sexe_final_2[,c(2:15)]

names(RP_1984_age_sexe_final_2)[names(RP_1984_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1984_2022_temperature_final_1984<-left_join(communes_dates_1984_2022_temperature_final_1984,RP_1984_age_sexe_final_2)

#tests<-filter(communes_dates_1984_2022_temperature_final_1984, COM=="01001")
#tests<-filter(communes_dates_1984_2022_temperature_final_1984, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984, !is.na(value_estimated_sum_homme) )

RP_1984_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1984_CSP_final_2")

RP_1984_CSP_final_2<-RP_1984_CSP_final_2[,c(2:12)]
names(RP_1984_CSP_final_2)[names(RP_1984_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1984_CSP_final_2)[names(RP_1984_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1984_2022_temperature_final_1984<-left_join(communes_dates_1984_2022_temperature_final_1984,RP_1984_CSP_final_2)


communes_dates_1984_2022_temperature_final_1984$part_agriculteur<-communes_dates_1984_2022_temperature_final_1984$value_estimated_agriculteur/communes_dates_1984_2022_temperature_final_1984$population_actif_25_54

communes_dates_1984_2022_temperature_final_1984$part_artisan_commercant_chef_entreprise<-communes_dates_1984_2022_temperature_final_1984$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1984_2022_temperature_final_1984$population_actif_25_54

communes_dates_1984_2022_temperature_final_1984$part_cadre<-communes_dates_1984_2022_temperature_final_1984$value_estimated_cadre/communes_dates_1984_2022_temperature_final_1984$population_actif_25_54

communes_dates_1984_2022_temperature_final_1984$part_profession_intermediaire<-communes_dates_1984_2022_temperature_final_1984$value_estimated_profession_intermediaire/communes_dates_1984_2022_temperature_final_1984$population_actif_25_54

communes_dates_1984_2022_temperature_final_1984$part_employe<-communes_dates_1984_2022_temperature_final_1984$value_estimated_employe/communes_dates_1984_2022_temperature_final_1984$population_actif_25_54

communes_dates_1984_2022_temperature_final_1984$part_ouvrier<-communes_dates_1984_2022_temperature_final_1984$value_estimated_ouvrier/communes_dates_1984_2022_temperature_final_1984$population_actif_25_54

communes_dates_1984_2022_temperature_final_1984$part_chomage<-communes_dates_1984_2022_temperature_final_1984$value_estimated_au_chomage/communes_dates_1984_2022_temperature_final_1984$population_actif_25_54






communes_dates_1984_2022_temperature_final_1984$taux_mortalite_homme<-communes_dates_1984_2022_temperature_final_1984$Homme/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_homme

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_femme<-communes_dates_1984_2022_temperature_final_1984$Femme/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_femme


communes_dates_1984_2022_temperature_final_1984$taux_mortalite_0_9<-communes_dates_1984_2022_temperature_final_1984$`0-9`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_0_9_h_f

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_10_19<-communes_dates_1984_2022_temperature_final_1984$`10-19`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_10_19_h_f



communes_dates_1984_2022_temperature_final_1984$taux_mortalite_20_39<-communes_dates_1984_2022_temperature_final_1984$`20-39`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_20_39_h_f




communes_dates_1984_2022_temperature_final_1984$taux_mortalite_40_59<-communes_dates_1984_2022_temperature_final_1984$`40-59`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_40_59_h_f





communes_dates_1984_2022_temperature_final_1984$taux_mortalite_60_64<-communes_dates_1984_2022_temperature_final_1984$`60-64`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_60_64_h_f



communes_dates_1984_2022_temperature_final_1984$taux_mortalite_65_69<-communes_dates_1984_2022_temperature_final_1984$`65-69`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_65_69_h_f


communes_dates_1984_2022_temperature_final_1984$taux_mortalite_70_74<-communes_dates_1984_2022_temperature_final_1984$`70-74`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_70_74_h_f


communes_dates_1984_2022_temperature_final_1984$taux_mortalite_75_79<-communes_dates_1984_2022_temperature_final_1984$`75-79`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_75_79_h_f


communes_dates_1984_2022_temperature_final_1984$taux_mortalite_80_plus<-communes_dates_1984_2022_temperature_final_1984$`80+`/communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_80_plus_h_f


#communes_dates_1984_2022_temperature_final_1984$taux_mortalite_60_70<-(communes_dates_1984_2022_temperature_final_1984$`60-64`+communes_dates_1984_2022_temperature_final_1984$`65-69`)/(communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_60_64_h_f+communes_dates_1984_2022_temperature_final_1984$value_estimated_sum_65_69_h_f)



communes_dates_1984_2022_temperature_final_1984$mort_total<-communes_dates_1984_2022_temperature_final_1984$Femme+communes_dates_1984_2022_temperature_final_1984$Homme


communes_dates_1984_2022_temperature_final_1984$taux_mortalite_total<-communes_dates_1984_2022_temperature_final_1984$mort_total/communes_dates_1984_2022_temperature_final_1984$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984, communes_dates_1984_2022_temperature_final_1984$value_estimated_population>0)
communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984, communes_dates_1984_2022_temperature_final_1984$population_actif_25_54>0)




communes_dates_1984_2022_temperature_final_1984<- communes_dates_1984_2022_temperature_final_1984[ , !names(communes_dates_1984_2022_temperature_final_1984) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_femme))


communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(part_agriculteur))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(part_cadre))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(part_profession_intermediaire))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(part_employe))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(part_ouvrier))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(part_chomage))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_homme))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_0_9))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_10_19))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_20_39))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_40_59))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_60_64))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_65_69))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_70_74))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_75_79))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1984_2022_temperature_final_1984<-communes_dates_1984_2022_temperature_final_1984[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  part_agriculteur<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_10_19<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  part_cadre<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  part_profession_intermediaire<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  part_employe<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  part_ouvrier<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  part_chomage<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_homme<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_femme<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_0_9<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_20_39<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_40_59<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_60_64<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_65_69<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_70_74<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_75_79<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_80_plus<=1)

#communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984,  taux_mortalite_total<=1)




communes_dates_1984_2022_temperature_final_1984$taux_mortalite_homme[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_homme>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_femme[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_femme>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_0_9[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_0_9>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_10_19[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_10_19>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_20_39[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_20_39>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_40_59[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_40_59>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_60_64[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_60_64>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_65_69[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_65_69>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_70_74[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_70_74>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_75_79[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_75_79>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_80_plus[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_80_plus>1]<-NA   

communes_dates_1984_2022_temperature_final_1984$taux_mortalite_total[communes_dates_1984_2022_temperature_final_1984$taux_mortalite_total>1]<-NA   





summary(communes_dates_1984_2022_temperature_final_1984$part_agriculteur)

summary(communes_dates_1984_2022_temperature_final_1984$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1984_2022_temperature_final_1984$part_cadre)

summary(communes_dates_1984_2022_temperature_final_1984$part_profession_intermediaire)

summary(communes_dates_1984_2022_temperature_final_1984$part_employe)

summary(communes_dates_1984_2022_temperature_final_1984$part_ouvrier)

summary(communes_dates_1984_2022_temperature_final_1984$part_chomage)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_homme)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_femme)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_0_9)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_10_19)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_20_39)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_40_59)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_60_64)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_65_69)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_70_74)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_75_79)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_80_plus)

summary(communes_dates_1984_2022_temperature_final_1984$taux_mortalite_total)






fwrite(communes_dates_1984_2022_temperature_final_1984,"/données communes années/données mortalité temperature final mois new/communes_dates_1984_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1985_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1985-01-01")
end_date <- as.Date("1985-12-31")

communes_dates_1985_2022_temperature_final_1985 <- communes_dates_1985_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1985_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1985_age_sexe.csv")





communes_dates_1985_2022_temperature_final_1985$date<-as.Date(communes_dates_1985_2022_temperature_final_1985$date)
deces.1985_age_sexe$date<-as.Date(deces.1985_age_sexe$date)





communes_dates_1985_2022_temperature_final_1985<-left_join(communes_dates_1985_2022_temperature_final_1985,deces.1985_age_sexe)

communes_dates_1985_2022_temperature_final_1985[is.na(communes_dates_1985_2022_temperature_final_1985)]<-0


communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 < -20]<-"<-20"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= -20 & communes_dates_1985_2022_temperature_final_1985$value1 < -15]<-"-20_-15"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= -15 & communes_dates_1985_2022_temperature_final_1985$value1 < -10]<-"-15_-10"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= -10 & communes_dates_1985_2022_temperature_final_1985$value1 < -5]<-"-10_-5"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= -5 & communes_dates_1985_2022_temperature_final_1985$value1 < 0]<-"-5_0"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 0 & communes_dates_1985_2022_temperature_final_1985$value1 < 5]<-"0_5"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 5 & communes_dates_1985_2022_temperature_final_1985$value1 < 10]<-"5_10"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 10 & communes_dates_1985_2022_temperature_final_1985$value1 < 15]<-"10_15"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 15 & communes_dates_1985_2022_temperature_final_1985$value1 < 20]<-"15_20"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 20 & communes_dates_1985_2022_temperature_final_1985$value1 < 25]<-"20_25"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 25 & communes_dates_1985_2022_temperature_final_1985$value1 < 28]<-"25_28"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 28 & communes_dates_1985_2022_temperature_final_1985$value1 < 30]<-"28_30"

communes_dates_1985_2022_temperature_final_1985$temperature_bin[communes_dates_1985_2022_temperature_final_1985$value1 >= 30]<-">30"


#test<-filter(communes_dates_1985_2022_temperature_final_1985, is.na(temperature_bin))
#table(communes_dates_1985_2022_temperature_final_1985$temperature_bin)

library(fastDummies)
communes_dates_1985_2022_temperature_final_1985  <- communes_dates_1985_2022_temperature_final_1985  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1985_2022_temperature_final_1985 <- communes_dates_1985_2022_temperature_final_1985 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1985_2022_temperature_final_1985 <- communes_dates_1985_2022_temperature_final_1985 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1985_2022_temperature_final_1985$same_value[is.na(communes_dates_1985_2022_temperature_final_1985$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1985_2022_temperature_final_1985$same_value <- ifelse(communes_dates_1985_2022_temperature_final_1985$temperature_bin != ">30", 0, communes_dates_1985_2022_temperature_final_1985$same_value)



communes_dates_1985_2022_temperature_final_1985$mois<-substring(communes_dates_1985_2022_temperature_final_1985$date,6,7)

communes_dates_1985_2022_temperature_final_1985<-communes_dates_1985_2022_temperature_final_1985[,-c("date","value1","temperature_bin")]

communes_dates_1985_2022_temperature_final_1985<-aggregate(.~COM+mois,communes_dates_1985_2022_temperature_final_1985,sum)



RP_1985_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1985_age_sexe_final_2")

RP_1985_age_sexe_final_2<-RP_1985_age_sexe_final_2[,c(2:15)]

names(RP_1985_age_sexe_final_2)[names(RP_1985_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1985_2022_temperature_final_1985<-left_join(communes_dates_1985_2022_temperature_final_1985,RP_1985_age_sexe_final_2)

#tests<-filter(communes_dates_1985_2022_temperature_final_1985, COM=="01001")
#tests<-filter(communes_dates_1985_2022_temperature_final_1985, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985, !is.na(value_estimated_sum_homme) )

RP_1985_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1985_CSP_final_2")

RP_1985_CSP_final_2<-RP_1985_CSP_final_2[,c(2:12)]
names(RP_1985_CSP_final_2)[names(RP_1985_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1985_CSP_final_2)[names(RP_1985_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1985_2022_temperature_final_1985<-left_join(communes_dates_1985_2022_temperature_final_1985,RP_1985_CSP_final_2)


communes_dates_1985_2022_temperature_final_1985$part_agriculteur<-communes_dates_1985_2022_temperature_final_1985$value_estimated_agriculteur/communes_dates_1985_2022_temperature_final_1985$population_actif_25_54

communes_dates_1985_2022_temperature_final_1985$part_artisan_commercant_chef_entreprise<-communes_dates_1985_2022_temperature_final_1985$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1985_2022_temperature_final_1985$population_actif_25_54

communes_dates_1985_2022_temperature_final_1985$part_cadre<-communes_dates_1985_2022_temperature_final_1985$value_estimated_cadre/communes_dates_1985_2022_temperature_final_1985$population_actif_25_54

communes_dates_1985_2022_temperature_final_1985$part_profession_intermediaire<-communes_dates_1985_2022_temperature_final_1985$value_estimated_profession_intermediaire/communes_dates_1985_2022_temperature_final_1985$population_actif_25_54

communes_dates_1985_2022_temperature_final_1985$part_employe<-communes_dates_1985_2022_temperature_final_1985$value_estimated_employe/communes_dates_1985_2022_temperature_final_1985$population_actif_25_54

communes_dates_1985_2022_temperature_final_1985$part_ouvrier<-communes_dates_1985_2022_temperature_final_1985$value_estimated_ouvrier/communes_dates_1985_2022_temperature_final_1985$population_actif_25_54

communes_dates_1985_2022_temperature_final_1985$part_chomage<-communes_dates_1985_2022_temperature_final_1985$value_estimated_au_chomage/communes_dates_1985_2022_temperature_final_1985$population_actif_25_54






communes_dates_1985_2022_temperature_final_1985$taux_mortalite_homme<-communes_dates_1985_2022_temperature_final_1985$Homme/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_homme

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_femme<-communes_dates_1985_2022_temperature_final_1985$Femme/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_femme


communes_dates_1985_2022_temperature_final_1985$taux_mortalite_0_9<-communes_dates_1985_2022_temperature_final_1985$`0-9`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_0_9_h_f

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_10_19<-communes_dates_1985_2022_temperature_final_1985$`10-19`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_10_19_h_f



communes_dates_1985_2022_temperature_final_1985$taux_mortalite_20_39<-communes_dates_1985_2022_temperature_final_1985$`20-39`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_20_39_h_f




communes_dates_1985_2022_temperature_final_1985$taux_mortalite_40_59<-communes_dates_1985_2022_temperature_final_1985$`40-59`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_40_59_h_f





communes_dates_1985_2022_temperature_final_1985$taux_mortalite_60_64<-communes_dates_1985_2022_temperature_final_1985$`60-64`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_60_64_h_f



communes_dates_1985_2022_temperature_final_1985$taux_mortalite_65_69<-communes_dates_1985_2022_temperature_final_1985$`65-69`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_65_69_h_f


communes_dates_1985_2022_temperature_final_1985$taux_mortalite_70_74<-communes_dates_1985_2022_temperature_final_1985$`70-74`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_70_74_h_f


communes_dates_1985_2022_temperature_final_1985$taux_mortalite_75_79<-communes_dates_1985_2022_temperature_final_1985$`75-79`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_75_79_h_f


communes_dates_1985_2022_temperature_final_1985$taux_mortalite_80_plus<-communes_dates_1985_2022_temperature_final_1985$`80+`/communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_80_plus_h_f


#communes_dates_1985_2022_temperature_final_1985$taux_mortalite_60_70<-(communes_dates_1985_2022_temperature_final_1985$`60-64`+communes_dates_1985_2022_temperature_final_1985$`65-69`)/(communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_60_64_h_f+communes_dates_1985_2022_temperature_final_1985$value_estimated_sum_65_69_h_f)



communes_dates_1985_2022_temperature_final_1985$mort_total<-communes_dates_1985_2022_temperature_final_1985$Femme+communes_dates_1985_2022_temperature_final_1985$Homme


communes_dates_1985_2022_temperature_final_1985$taux_mortalite_total<-communes_dates_1985_2022_temperature_final_1985$mort_total/communes_dates_1985_2022_temperature_final_1985$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985, communes_dates_1985_2022_temperature_final_1985$value_estimated_population>0)
communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985, communes_dates_1985_2022_temperature_final_1985$population_actif_25_54>0)




communes_dates_1985_2022_temperature_final_1985<- communes_dates_1985_2022_temperature_final_1985[ , !names(communes_dates_1985_2022_temperature_final_1985) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_femme))


communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(part_agriculteur))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(part_cadre))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(part_profession_intermediaire))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(part_employe))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(part_ouvrier))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(part_chomage))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_homme))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_0_9))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_10_19))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_20_39))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_40_59))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_60_64))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_65_69))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_70_74))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_75_79))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1985_2022_temperature_final_1985<-communes_dates_1985_2022_temperature_final_1985[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  part_agriculteur<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_10_19<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  part_cadre<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  part_profession_intermediaire<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  part_employe<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  part_ouvrier<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  part_chomage<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_homme<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_femme<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_0_9<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_20_39<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_40_59<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_60_64<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_65_69<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_70_74<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_75_79<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_80_plus<=1)

#communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985,  taux_mortalite_total<=1)




communes_dates_1985_2022_temperature_final_1985$taux_mortalite_homme[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_homme>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_femme[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_femme>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_0_9[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_0_9>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_10_19[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_10_19>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_20_39[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_20_39>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_40_59[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_40_59>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_60_64[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_60_64>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_65_69[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_65_69>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_70_74[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_70_74>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_75_79[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_75_79>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_80_plus[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_80_plus>1]<-NA   

communes_dates_1985_2022_temperature_final_1985$taux_mortalite_total[communes_dates_1985_2022_temperature_final_1985$taux_mortalite_total>1]<-NA   





summary(communes_dates_1985_2022_temperature_final_1985$part_agriculteur)

summary(communes_dates_1985_2022_temperature_final_1985$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1985_2022_temperature_final_1985$part_cadre)

summary(communes_dates_1985_2022_temperature_final_1985$part_profession_intermediaire)

summary(communes_dates_1985_2022_temperature_final_1985$part_employe)

summary(communes_dates_1985_2022_temperature_final_1985$part_ouvrier)

summary(communes_dates_1985_2022_temperature_final_1985$part_chomage)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_homme)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_femme)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_0_9)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_10_19)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_20_39)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_40_59)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_60_64)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_65_69)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_70_74)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_75_79)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_80_plus)

summary(communes_dates_1985_2022_temperature_final_1985$taux_mortalite_total)






fwrite(communes_dates_1985_2022_temperature_final_1985,"/données communes années/données mortalité temperature final mois new/communes_dates_1985_temperature_deces_mois.csv")








################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1986_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1986-01-01")
end_date <- as.Date("1986-12-31")

communes_dates_1986_2022_temperature_final_1986 <- communes_dates_1986_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1986_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1986_age_sexe.csv")




communes_dates_1986_2022_temperature_final_1986$date<-as.Date(communes_dates_1986_2022_temperature_final_1986$date)
deces.1986_age_sexe$date<-as.Date(deces.1986_age_sexe$date)






communes_dates_1986_2022_temperature_final_1986<-left_join(communes_dates_1986_2022_temperature_final_1986,deces.1986_age_sexe)

communes_dates_1986_2022_temperature_final_1986[is.na(communes_dates_1986_2022_temperature_final_1986)]<-0


communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 < -20]<-"<-20"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= -20 & communes_dates_1986_2022_temperature_final_1986$value1 < -15]<-"-20_-15"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= -15 & communes_dates_1986_2022_temperature_final_1986$value1 < -10]<-"-15_-10"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= -10 & communes_dates_1986_2022_temperature_final_1986$value1 < -5]<-"-10_-5"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= -5 & communes_dates_1986_2022_temperature_final_1986$value1 < 0]<-"-5_0"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 0 & communes_dates_1986_2022_temperature_final_1986$value1 < 5]<-"0_5"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 5 & communes_dates_1986_2022_temperature_final_1986$value1 < 10]<-"5_10"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 10 & communes_dates_1986_2022_temperature_final_1986$value1 < 15]<-"10_15"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 15 & communes_dates_1986_2022_temperature_final_1986$value1 < 20]<-"15_20"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 20 & communes_dates_1986_2022_temperature_final_1986$value1 < 25]<-"20_25"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 25 & communes_dates_1986_2022_temperature_final_1986$value1 < 28]<-"25_28"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 28 & communes_dates_1986_2022_temperature_final_1986$value1 < 30]<-"28_30"

communes_dates_1986_2022_temperature_final_1986$temperature_bin[communes_dates_1986_2022_temperature_final_1986$value1 >= 30]<-">30"


#test<-filter(communes_dates_1986_2022_temperature_final_1986, is.na(temperature_bin))
#table(communes_dates_1986_2022_temperature_final_1986$temperature_bin)

library(fastDummies)
communes_dates_1986_2022_temperature_final_1986  <- communes_dates_1986_2022_temperature_final_1986  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1986_2022_temperature_final_1986 <- communes_dates_1986_2022_temperature_final_1986 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1986_2022_temperature_final_1986 <- communes_dates_1986_2022_temperature_final_1986 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1986_2022_temperature_final_1986$same_value[is.na(communes_dates_1986_2022_temperature_final_1986$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1986_2022_temperature_final_1986$same_value <- ifelse(communes_dates_1986_2022_temperature_final_1986$temperature_bin != ">30", 0, communes_dates_1986_2022_temperature_final_1986$same_value)



communes_dates_1986_2022_temperature_final_1986$mois<-substring(communes_dates_1986_2022_temperature_final_1986$date,6,7)

communes_dates_1986_2022_temperature_final_1986<-communes_dates_1986_2022_temperature_final_1986[,-c("date","value1","temperature_bin")]

communes_dates_1986_2022_temperature_final_1986<-aggregate(.~COM+mois,communes_dates_1986_2022_temperature_final_1986,sum)



RP_1986_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1986_age_sexe_final_2")

RP_1986_age_sexe_final_2<-RP_1986_age_sexe_final_2[,c(2:15)]

names(RP_1986_age_sexe_final_2)[names(RP_1986_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1986_2022_temperature_final_1986<-left_join(communes_dates_1986_2022_temperature_final_1986,RP_1986_age_sexe_final_2)

#tests<-filter(communes_dates_1986_2022_temperature_final_1986, COM=="01001")
#tests<-filter(communes_dates_1986_2022_temperature_final_1986, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986, !is.na(value_estimated_sum_homme) )

RP_1986_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1986_CSP_final_2")

RP_1986_CSP_final_2<-RP_1986_CSP_final_2[,c(2:12)]
names(RP_1986_CSP_final_2)[names(RP_1986_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1986_CSP_final_2)[names(RP_1986_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1986_2022_temperature_final_1986<-left_join(communes_dates_1986_2022_temperature_final_1986,RP_1986_CSP_final_2)


communes_dates_1986_2022_temperature_final_1986$part_agriculteur<-communes_dates_1986_2022_temperature_final_1986$value_estimated_agriculteur/communes_dates_1986_2022_temperature_final_1986$population_actif_25_54

communes_dates_1986_2022_temperature_final_1986$part_artisan_commercant_chef_entreprise<-communes_dates_1986_2022_temperature_final_1986$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1986_2022_temperature_final_1986$population_actif_25_54

communes_dates_1986_2022_temperature_final_1986$part_cadre<-communes_dates_1986_2022_temperature_final_1986$value_estimated_cadre/communes_dates_1986_2022_temperature_final_1986$population_actif_25_54

communes_dates_1986_2022_temperature_final_1986$part_profession_intermediaire<-communes_dates_1986_2022_temperature_final_1986$value_estimated_profession_intermediaire/communes_dates_1986_2022_temperature_final_1986$population_actif_25_54

communes_dates_1986_2022_temperature_final_1986$part_employe<-communes_dates_1986_2022_temperature_final_1986$value_estimated_employe/communes_dates_1986_2022_temperature_final_1986$population_actif_25_54

communes_dates_1986_2022_temperature_final_1986$part_ouvrier<-communes_dates_1986_2022_temperature_final_1986$value_estimated_ouvrier/communes_dates_1986_2022_temperature_final_1986$population_actif_25_54

communes_dates_1986_2022_temperature_final_1986$part_chomage<-communes_dates_1986_2022_temperature_final_1986$value_estimated_au_chomage/communes_dates_1986_2022_temperature_final_1986$population_actif_25_54






communes_dates_1986_2022_temperature_final_1986$taux_mortalite_homme<-communes_dates_1986_2022_temperature_final_1986$Homme/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_homme

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_femme<-communes_dates_1986_2022_temperature_final_1986$Femme/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_femme


communes_dates_1986_2022_temperature_final_1986$taux_mortalite_0_9<-communes_dates_1986_2022_temperature_final_1986$`0-9`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_0_9_h_f

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_10_19<-communes_dates_1986_2022_temperature_final_1986$`10-19`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_10_19_h_f



communes_dates_1986_2022_temperature_final_1986$taux_mortalite_20_39<-communes_dates_1986_2022_temperature_final_1986$`20-39`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_20_39_h_f




communes_dates_1986_2022_temperature_final_1986$taux_mortalite_40_59<-communes_dates_1986_2022_temperature_final_1986$`40-59`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_40_59_h_f





communes_dates_1986_2022_temperature_final_1986$taux_mortalite_60_64<-communes_dates_1986_2022_temperature_final_1986$`60-64`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_60_64_h_f



communes_dates_1986_2022_temperature_final_1986$taux_mortalite_65_69<-communes_dates_1986_2022_temperature_final_1986$`65-69`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_65_69_h_f


communes_dates_1986_2022_temperature_final_1986$taux_mortalite_70_74<-communes_dates_1986_2022_temperature_final_1986$`70-74`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_70_74_h_f


communes_dates_1986_2022_temperature_final_1986$taux_mortalite_75_79<-communes_dates_1986_2022_temperature_final_1986$`75-79`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_75_79_h_f


communes_dates_1986_2022_temperature_final_1986$taux_mortalite_80_plus<-communes_dates_1986_2022_temperature_final_1986$`80+`/communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_80_plus_h_f


#communes_dates_1986_2022_temperature_final_1986$taux_mortalite_60_70<-(communes_dates_1986_2022_temperature_final_1986$`60-64`+communes_dates_1986_2022_temperature_final_1986$`65-69`)/(communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_60_64_h_f+communes_dates_1986_2022_temperature_final_1986$value_estimated_sum_65_69_h_f)



communes_dates_1986_2022_temperature_final_1986$mort_total<-communes_dates_1986_2022_temperature_final_1986$Femme+communes_dates_1986_2022_temperature_final_1986$Homme


communes_dates_1986_2022_temperature_final_1986$taux_mortalite_total<-communes_dates_1986_2022_temperature_final_1986$mort_total/communes_dates_1986_2022_temperature_final_1986$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986, communes_dates_1986_2022_temperature_final_1986$value_estimated_population>0)
communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986, communes_dates_1986_2022_temperature_final_1986$population_actif_25_54>0)




communes_dates_1986_2022_temperature_final_1986<- communes_dates_1986_2022_temperature_final_1986[ , !names(communes_dates_1986_2022_temperature_final_1986) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_femme))


communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(part_agriculteur))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(part_cadre))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(part_profession_intermediaire))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(part_employe))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(part_ouvrier))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(part_chomage))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_homme))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_0_9))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_10_19))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_20_39))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_40_59))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_60_64))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_65_69))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_70_74))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_75_79))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1986_2022_temperature_final_1986<-communes_dates_1986_2022_temperature_final_1986[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  part_agriculteur<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_10_19<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  part_cadre<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  part_profession_intermediaire<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  part_employe<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  part_ouvrier<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  part_chomage<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_homme<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_femme<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_0_9<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_20_39<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_40_59<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_60_64<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_65_69<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_70_74<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_75_79<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_80_plus<=1)

#communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986,  taux_mortalite_total<=1)




communes_dates_1986_2022_temperature_final_1986$taux_mortalite_homme[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_homme>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_femme[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_femme>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_0_9[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_0_9>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_10_19[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_10_19>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_20_39[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_20_39>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_40_59[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_40_59>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_60_64[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_60_64>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_65_69[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_65_69>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_70_74[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_70_74>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_75_79[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_75_79>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_80_plus[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_80_plus>1]<-NA   

communes_dates_1986_2022_temperature_final_1986$taux_mortalite_total[communes_dates_1986_2022_temperature_final_1986$taux_mortalite_total>1]<-NA   





summary(communes_dates_1986_2022_temperature_final_1986$part_agriculteur)

summary(communes_dates_1986_2022_temperature_final_1986$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1986_2022_temperature_final_1986$part_cadre)

summary(communes_dates_1986_2022_temperature_final_1986$part_profession_intermediaire)

summary(communes_dates_1986_2022_temperature_final_1986$part_employe)

summary(communes_dates_1986_2022_temperature_final_1986$part_ouvrier)

summary(communes_dates_1986_2022_temperature_final_1986$part_chomage)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_homme)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_femme)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_0_9)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_10_19)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_20_39)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_40_59)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_60_64)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_65_69)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_70_74)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_75_79)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_80_plus)

summary(communes_dates_1986_2022_temperature_final_1986$taux_mortalite_total)






fwrite(communes_dates_1986_2022_temperature_final_1986,"/données communes années/données mortalité temperature final mois new/communes_dates_1986_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1987_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1987-01-01")
end_date <- as.Date("1987-12-31")

communes_dates_1987_2022_temperature_final_1987 <- communes_dates_1987_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1987_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1987_age_sexe.csv")






communes_dates_1987_2022_temperature_final_1987$date<-as.Date(communes_dates_1987_2022_temperature_final_1987$date)
deces.1987_age_sexe$date<-as.Date(deces.1987_age_sexe$date)







communes_dates_1987_2022_temperature_final_1987<-left_join(communes_dates_1987_2022_temperature_final_1987,deces.1987_age_sexe)

communes_dates_1987_2022_temperature_final_1987[is.na(communes_dates_1987_2022_temperature_final_1987)]<-0


communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 < -20]<-"<-20"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= -20 & communes_dates_1987_2022_temperature_final_1987$value1 < -15]<-"-20_-15"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= -15 & communes_dates_1987_2022_temperature_final_1987$value1 < -10]<-"-15_-10"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= -10 & communes_dates_1987_2022_temperature_final_1987$value1 < -5]<-"-10_-5"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= -5 & communes_dates_1987_2022_temperature_final_1987$value1 < 0]<-"-5_0"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 0 & communes_dates_1987_2022_temperature_final_1987$value1 < 5]<-"0_5"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 5 & communes_dates_1987_2022_temperature_final_1987$value1 < 10]<-"5_10"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 10 & communes_dates_1987_2022_temperature_final_1987$value1 < 15]<-"10_15"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 15 & communes_dates_1987_2022_temperature_final_1987$value1 < 20]<-"15_20"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 20 & communes_dates_1987_2022_temperature_final_1987$value1 < 25]<-"20_25"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 25 & communes_dates_1987_2022_temperature_final_1987$value1 < 28]<-"25_28"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 28 & communes_dates_1987_2022_temperature_final_1987$value1 < 30]<-"28_30"

communes_dates_1987_2022_temperature_final_1987$temperature_bin[communes_dates_1987_2022_temperature_final_1987$value1 >= 30]<-">30"


#test<-filter(communes_dates_1987_2022_temperature_final_1987, is.na(temperature_bin))
#table(communes_dates_1987_2022_temperature_final_1987$temperature_bin)

library(fastDummies)
communes_dates_1987_2022_temperature_final_1987  <- communes_dates_1987_2022_temperature_final_1987  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1987_2022_temperature_final_1987 <- communes_dates_1987_2022_temperature_final_1987 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1987_2022_temperature_final_1987 <- communes_dates_1987_2022_temperature_final_1987 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1987_2022_temperature_final_1987$same_value[is.na(communes_dates_1987_2022_temperature_final_1987$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1987_2022_temperature_final_1987$same_value <- ifelse(communes_dates_1987_2022_temperature_final_1987$temperature_bin != ">30", 0, communes_dates_1987_2022_temperature_final_1987$same_value)



communes_dates_1987_2022_temperature_final_1987$mois<-substring(communes_dates_1987_2022_temperature_final_1987$date,6,7)

communes_dates_1987_2022_temperature_final_1987<-communes_dates_1987_2022_temperature_final_1987[,-c("date","value1","temperature_bin")]

communes_dates_1987_2022_temperature_final_1987<-aggregate(.~COM+mois,communes_dates_1987_2022_temperature_final_1987,sum)



RP_1987_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1987_age_sexe_final_2")

RP_1987_age_sexe_final_2<-RP_1987_age_sexe_final_2[,c(2:15)]

names(RP_1987_age_sexe_final_2)[names(RP_1987_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1987_2022_temperature_final_1987<-left_join(communes_dates_1987_2022_temperature_final_1987,RP_1987_age_sexe_final_2)

#tests<-filter(communes_dates_1987_2022_temperature_final_1987, COM=="01001")
#tests<-filter(communes_dates_1987_2022_temperature_final_1987, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987, !is.na(value_estimated_sum_homme) )

RP_1987_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1987_CSP_final_2")

RP_1987_CSP_final_2<-RP_1987_CSP_final_2[,c(2:12)]
names(RP_1987_CSP_final_2)[names(RP_1987_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1987_CSP_final_2)[names(RP_1987_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1987_2022_temperature_final_1987<-left_join(communes_dates_1987_2022_temperature_final_1987,RP_1987_CSP_final_2)


communes_dates_1987_2022_temperature_final_1987$part_agriculteur<-communes_dates_1987_2022_temperature_final_1987$value_estimated_agriculteur/communes_dates_1987_2022_temperature_final_1987$population_actif_25_54

communes_dates_1987_2022_temperature_final_1987$part_artisan_commercant_chef_entreprise<-communes_dates_1987_2022_temperature_final_1987$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1987_2022_temperature_final_1987$population_actif_25_54

communes_dates_1987_2022_temperature_final_1987$part_cadre<-communes_dates_1987_2022_temperature_final_1987$value_estimated_cadre/communes_dates_1987_2022_temperature_final_1987$population_actif_25_54

communes_dates_1987_2022_temperature_final_1987$part_profession_intermediaire<-communes_dates_1987_2022_temperature_final_1987$value_estimated_profession_intermediaire/communes_dates_1987_2022_temperature_final_1987$population_actif_25_54

communes_dates_1987_2022_temperature_final_1987$part_employe<-communes_dates_1987_2022_temperature_final_1987$value_estimated_employe/communes_dates_1987_2022_temperature_final_1987$population_actif_25_54

communes_dates_1987_2022_temperature_final_1987$part_ouvrier<-communes_dates_1987_2022_temperature_final_1987$value_estimated_ouvrier/communes_dates_1987_2022_temperature_final_1987$population_actif_25_54

communes_dates_1987_2022_temperature_final_1987$part_chomage<-communes_dates_1987_2022_temperature_final_1987$value_estimated_au_chomage/communes_dates_1987_2022_temperature_final_1987$population_actif_25_54






communes_dates_1987_2022_temperature_final_1987$taux_mortalite_homme<-communes_dates_1987_2022_temperature_final_1987$Homme/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_homme

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_femme<-communes_dates_1987_2022_temperature_final_1987$Femme/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_femme


communes_dates_1987_2022_temperature_final_1987$taux_mortalite_0_9<-communes_dates_1987_2022_temperature_final_1987$`0-9`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_0_9_h_f

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_10_19<-communes_dates_1987_2022_temperature_final_1987$`10-19`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_10_19_h_f



communes_dates_1987_2022_temperature_final_1987$taux_mortalite_20_39<-communes_dates_1987_2022_temperature_final_1987$`20-39`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_20_39_h_f




communes_dates_1987_2022_temperature_final_1987$taux_mortalite_40_59<-communes_dates_1987_2022_temperature_final_1987$`40-59`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_40_59_h_f





communes_dates_1987_2022_temperature_final_1987$taux_mortalite_60_64<-communes_dates_1987_2022_temperature_final_1987$`60-64`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_60_64_h_f



communes_dates_1987_2022_temperature_final_1987$taux_mortalite_65_69<-communes_dates_1987_2022_temperature_final_1987$`65-69`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_65_69_h_f


communes_dates_1987_2022_temperature_final_1987$taux_mortalite_70_74<-communes_dates_1987_2022_temperature_final_1987$`70-74`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_70_74_h_f


communes_dates_1987_2022_temperature_final_1987$taux_mortalite_75_79<-communes_dates_1987_2022_temperature_final_1987$`75-79`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_75_79_h_f


communes_dates_1987_2022_temperature_final_1987$taux_mortalite_80_plus<-communes_dates_1987_2022_temperature_final_1987$`80+`/communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_80_plus_h_f


#communes_dates_1987_2022_temperature_final_1987$taux_mortalite_60_70<-(communes_dates_1987_2022_temperature_final_1987$`60-64`+communes_dates_1987_2022_temperature_final_1987$`65-69`)/(communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_60_64_h_f+communes_dates_1987_2022_temperature_final_1987$value_estimated_sum_65_69_h_f)



communes_dates_1987_2022_temperature_final_1987$mort_total<-communes_dates_1987_2022_temperature_final_1987$Femme+communes_dates_1987_2022_temperature_final_1987$Homme


communes_dates_1987_2022_temperature_final_1987$taux_mortalite_total<-communes_dates_1987_2022_temperature_final_1987$mort_total/communes_dates_1987_2022_temperature_final_1987$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987, communes_dates_1987_2022_temperature_final_1987$value_estimated_population>0)
communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987, communes_dates_1987_2022_temperature_final_1987$population_actif_25_54>0)




communes_dates_1987_2022_temperature_final_1987<- communes_dates_1987_2022_temperature_final_1987[ , !names(communes_dates_1987_2022_temperature_final_1987) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_femme))


communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(part_agriculteur))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(part_cadre))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(part_profession_intermediaire))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(part_employe))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(part_ouvrier))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(part_chomage))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_homme))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_0_9))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_10_19))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_20_39))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_40_59))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_60_64))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_65_69))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_70_74))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_75_79))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1987_2022_temperature_final_1987<-communes_dates_1987_2022_temperature_final_1987[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  part_agriculteur<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_10_19<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  part_cadre<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  part_profession_intermediaire<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  part_employe<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  part_ouvrier<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  part_chomage<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_homme<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_femme<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_0_9<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_20_39<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_40_59<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_60_64<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_65_69<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_70_74<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_75_79<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_80_plus<=1)

#communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987,  taux_mortalite_total<=1)




communes_dates_1987_2022_temperature_final_1987$taux_mortalite_homme[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_homme>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_femme[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_femme>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_0_9[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_0_9>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_10_19[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_10_19>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_20_39[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_20_39>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_40_59[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_40_59>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_60_64[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_60_64>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_65_69[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_65_69>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_70_74[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_70_74>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_75_79[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_75_79>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_80_plus[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_80_plus>1]<-NA   

communes_dates_1987_2022_temperature_final_1987$taux_mortalite_total[communes_dates_1987_2022_temperature_final_1987$taux_mortalite_total>1]<-NA   





summary(communes_dates_1987_2022_temperature_final_1987$part_agriculteur)

summary(communes_dates_1987_2022_temperature_final_1987$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1987_2022_temperature_final_1987$part_cadre)

summary(communes_dates_1987_2022_temperature_final_1987$part_profession_intermediaire)

summary(communes_dates_1987_2022_temperature_final_1987$part_employe)

summary(communes_dates_1987_2022_temperature_final_1987$part_ouvrier)

summary(communes_dates_1987_2022_temperature_final_1987$part_chomage)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_homme)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_femme)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_0_9)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_10_19)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_20_39)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_40_59)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_60_64)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_65_69)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_70_74)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_75_79)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_80_plus)

summary(communes_dates_1987_2022_temperature_final_1987$taux_mortalite_total)






fwrite(communes_dates_1987_2022_temperature_final_1987,"/données communes années/données mortalité temperature final mois new/communes_dates_1987_temperature_deces_mois.csv")








################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1988_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1988-01-01")
end_date <- as.Date("1988-12-31")

communes_dates_1988_2022_temperature_final_1988 <- communes_dates_1988_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1988_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1988_age_sexe.csv")





communes_dates_1988_2022_temperature_final_1988$date<-as.Date(communes_dates_1988_2022_temperature_final_1988$date)
deces.1988_age_sexe$date<-as.Date(deces.1988_age_sexe$date)




communes_dates_1988_2022_temperature_final_1988<-left_join(communes_dates_1988_2022_temperature_final_1988,deces.1988_age_sexe)

communes_dates_1988_2022_temperature_final_1988[is.na(communes_dates_1988_2022_temperature_final_1988)]<-0


communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 < -20]<-"<-20"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= -20 & communes_dates_1988_2022_temperature_final_1988$value1 < -15]<-"-20_-15"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= -15 & communes_dates_1988_2022_temperature_final_1988$value1 < -10]<-"-15_-10"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= -10 & communes_dates_1988_2022_temperature_final_1988$value1 < -5]<-"-10_-5"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= -5 & communes_dates_1988_2022_temperature_final_1988$value1 < 0]<-"-5_0"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 0 & communes_dates_1988_2022_temperature_final_1988$value1 < 5]<-"0_5"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 5 & communes_dates_1988_2022_temperature_final_1988$value1 < 10]<-"5_10"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 10 & communes_dates_1988_2022_temperature_final_1988$value1 < 15]<-"10_15"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 15 & communes_dates_1988_2022_temperature_final_1988$value1 < 20]<-"15_20"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 20 & communes_dates_1988_2022_temperature_final_1988$value1 < 25]<-"20_25"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 25 & communes_dates_1988_2022_temperature_final_1988$value1 < 28]<-"25_28"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 28 & communes_dates_1988_2022_temperature_final_1988$value1 < 30]<-"28_30"

communes_dates_1988_2022_temperature_final_1988$temperature_bin[communes_dates_1988_2022_temperature_final_1988$value1 >= 30]<-">30"


#test<-filter(communes_dates_1988_2022_temperature_final_1988, is.na(temperature_bin))
#table(communes_dates_1988_2022_temperature_final_1988$temperature_bin)

library(fastDummies)
communes_dates_1988_2022_temperature_final_1988  <- communes_dates_1988_2022_temperature_final_1988  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1988_2022_temperature_final_1988 <- communes_dates_1988_2022_temperature_final_1988 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1988_2022_temperature_final_1988 <- communes_dates_1988_2022_temperature_final_1988 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1988_2022_temperature_final_1988$same_value[is.na(communes_dates_1988_2022_temperature_final_1988$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1988_2022_temperature_final_1988$same_value <- ifelse(communes_dates_1988_2022_temperature_final_1988$temperature_bin != ">30", 0, communes_dates_1988_2022_temperature_final_1988$same_value)



communes_dates_1988_2022_temperature_final_1988$mois<-substring(communes_dates_1988_2022_temperature_final_1988$date,6,7)

communes_dates_1988_2022_temperature_final_1988<-communes_dates_1988_2022_temperature_final_1988[,-c("date","value1","temperature_bin")]

communes_dates_1988_2022_temperature_final_1988<-aggregate(.~COM+mois,communes_dates_1988_2022_temperature_final_1988,sum)



RP_1988_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1988_age_sexe_final_2")

RP_1988_age_sexe_final_2<-RP_1988_age_sexe_final_2[,c(2:15)]

names(RP_1988_age_sexe_final_2)[names(RP_1988_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1988_2022_temperature_final_1988<-left_join(communes_dates_1988_2022_temperature_final_1988,RP_1988_age_sexe_final_2)

#tests<-filter(communes_dates_1988_2022_temperature_final_1988, COM=="01001")
#tests<-filter(communes_dates_1988_2022_temperature_final_1988, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988, !is.na(value_estimated_sum_homme) )

RP_1988_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1988_CSP_final_2")

RP_1988_CSP_final_2<-RP_1988_CSP_final_2[,c(2:12)]
names(RP_1988_CSP_final_2)[names(RP_1988_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1988_CSP_final_2)[names(RP_1988_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1988_2022_temperature_final_1988<-left_join(communes_dates_1988_2022_temperature_final_1988,RP_1988_CSP_final_2)


communes_dates_1988_2022_temperature_final_1988$part_agriculteur<-communes_dates_1988_2022_temperature_final_1988$value_estimated_agriculteur/communes_dates_1988_2022_temperature_final_1988$population_actif_25_54

communes_dates_1988_2022_temperature_final_1988$part_artisan_commercant_chef_entreprise<-communes_dates_1988_2022_temperature_final_1988$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1988_2022_temperature_final_1988$population_actif_25_54

communes_dates_1988_2022_temperature_final_1988$part_cadre<-communes_dates_1988_2022_temperature_final_1988$value_estimated_cadre/communes_dates_1988_2022_temperature_final_1988$population_actif_25_54

communes_dates_1988_2022_temperature_final_1988$part_profession_intermediaire<-communes_dates_1988_2022_temperature_final_1988$value_estimated_profession_intermediaire/communes_dates_1988_2022_temperature_final_1988$population_actif_25_54

communes_dates_1988_2022_temperature_final_1988$part_employe<-communes_dates_1988_2022_temperature_final_1988$value_estimated_employe/communes_dates_1988_2022_temperature_final_1988$population_actif_25_54

communes_dates_1988_2022_temperature_final_1988$part_ouvrier<-communes_dates_1988_2022_temperature_final_1988$value_estimated_ouvrier/communes_dates_1988_2022_temperature_final_1988$population_actif_25_54

communes_dates_1988_2022_temperature_final_1988$part_chomage<-communes_dates_1988_2022_temperature_final_1988$value_estimated_au_chomage/communes_dates_1988_2022_temperature_final_1988$population_actif_25_54






communes_dates_1988_2022_temperature_final_1988$taux_mortalite_homme<-communes_dates_1988_2022_temperature_final_1988$Homme/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_homme

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_femme<-communes_dates_1988_2022_temperature_final_1988$Femme/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_femme


communes_dates_1988_2022_temperature_final_1988$taux_mortalite_0_9<-communes_dates_1988_2022_temperature_final_1988$`0-9`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_0_9_h_f

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_10_19<-communes_dates_1988_2022_temperature_final_1988$`10-19`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_10_19_h_f



communes_dates_1988_2022_temperature_final_1988$taux_mortalite_20_39<-communes_dates_1988_2022_temperature_final_1988$`20-39`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_20_39_h_f




communes_dates_1988_2022_temperature_final_1988$taux_mortalite_40_59<-communes_dates_1988_2022_temperature_final_1988$`40-59`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_40_59_h_f





communes_dates_1988_2022_temperature_final_1988$taux_mortalite_60_64<-communes_dates_1988_2022_temperature_final_1988$`60-64`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_60_64_h_f



communes_dates_1988_2022_temperature_final_1988$taux_mortalite_65_69<-communes_dates_1988_2022_temperature_final_1988$`65-69`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_65_69_h_f


communes_dates_1988_2022_temperature_final_1988$taux_mortalite_70_74<-communes_dates_1988_2022_temperature_final_1988$`70-74`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_70_74_h_f


communes_dates_1988_2022_temperature_final_1988$taux_mortalite_75_79<-communes_dates_1988_2022_temperature_final_1988$`75-79`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_75_79_h_f


communes_dates_1988_2022_temperature_final_1988$taux_mortalite_80_plus<-communes_dates_1988_2022_temperature_final_1988$`80+`/communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_80_plus_h_f


#communes_dates_1988_2022_temperature_final_1988$taux_mortalite_60_70<-(communes_dates_1988_2022_temperature_final_1988$`60-64`+communes_dates_1988_2022_temperature_final_1988$`65-69`)/(communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_60_64_h_f+communes_dates_1988_2022_temperature_final_1988$value_estimated_sum_65_69_h_f)



communes_dates_1988_2022_temperature_final_1988$mort_total<-communes_dates_1988_2022_temperature_final_1988$Femme+communes_dates_1988_2022_temperature_final_1988$Homme


communes_dates_1988_2022_temperature_final_1988$taux_mortalite_total<-communes_dates_1988_2022_temperature_final_1988$mort_total/communes_dates_1988_2022_temperature_final_1988$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988, communes_dates_1988_2022_temperature_final_1988$value_estimated_population>0)
communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988, communes_dates_1988_2022_temperature_final_1988$population_actif_25_54>0)




communes_dates_1988_2022_temperature_final_1988<- communes_dates_1988_2022_temperature_final_1988[ , !names(communes_dates_1988_2022_temperature_final_1988) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_femme))


communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(part_agriculteur))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(part_cadre))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(part_profession_intermediaire))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(part_employe))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(part_ouvrier))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(part_chomage))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_homme))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_0_9))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_10_19))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_20_39))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_40_59))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_60_64))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_65_69))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_70_74))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_75_79))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1988_2022_temperature_final_1988<-communes_dates_1988_2022_temperature_final_1988[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  part_agriculteur<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_10_19<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  part_cadre<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  part_profession_intermediaire<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  part_employe<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  part_ouvrier<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  part_chomage<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_homme<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_femme<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_0_9<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_20_39<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_40_59<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_60_64<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_65_69<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_70_74<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_75_79<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_80_plus<=1)

#communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988,  taux_mortalite_total<=1)




communes_dates_1988_2022_temperature_final_1988$taux_mortalite_homme[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_homme>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_femme[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_femme>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_0_9[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_0_9>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_10_19[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_10_19>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_20_39[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_20_39>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_40_59[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_40_59>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_60_64[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_60_64>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_65_69[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_65_69>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_70_74[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_70_74>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_75_79[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_75_79>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_80_plus[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_80_plus>1]<-NA   

communes_dates_1988_2022_temperature_final_1988$taux_mortalite_total[communes_dates_1988_2022_temperature_final_1988$taux_mortalite_total>1]<-NA   





summary(communes_dates_1988_2022_temperature_final_1988$part_agriculteur)

summary(communes_dates_1988_2022_temperature_final_1988$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1988_2022_temperature_final_1988$part_cadre)

summary(communes_dates_1988_2022_temperature_final_1988$part_profession_intermediaire)

summary(communes_dates_1988_2022_temperature_final_1988$part_employe)

summary(communes_dates_1988_2022_temperature_final_1988$part_ouvrier)

summary(communes_dates_1988_2022_temperature_final_1988$part_chomage)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_homme)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_femme)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_0_9)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_10_19)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_20_39)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_40_59)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_60_64)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_65_69)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_70_74)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_75_79)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_80_plus)

summary(communes_dates_1988_2022_temperature_final_1988$taux_mortalite_total)






fwrite(communes_dates_1988_2022_temperature_final_1988,"/données communes années/données mortalité temperature final mois new/communes_dates_1988_temperature_deces_mois.csv")







################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1989_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1989-01-01")
end_date <- as.Date("1989-12-31")

communes_dates_1989_2022_temperature_final_1989 <- communes_dates_1989_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1989_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1989_age_sexe.csv")




communes_dates_1989_2022_temperature_final_1989$date<-as.Date(communes_dates_1989_2022_temperature_final_1989$date)
deces.1989_age_sexe$date<-as.Date(deces.1989_age_sexe$date)





communes_dates_1989_2022_temperature_final_1989<-left_join(communes_dates_1989_2022_temperature_final_1989,deces.1989_age_sexe)

communes_dates_1989_2022_temperature_final_1989[is.na(communes_dates_1989_2022_temperature_final_1989)]<-0


communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 < -20]<-"<-20"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= -20 & communes_dates_1989_2022_temperature_final_1989$value1 < -15]<-"-20_-15"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= -15 & communes_dates_1989_2022_temperature_final_1989$value1 < -10]<-"-15_-10"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= -10 & communes_dates_1989_2022_temperature_final_1989$value1 < -5]<-"-10_-5"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= -5 & communes_dates_1989_2022_temperature_final_1989$value1 < 0]<-"-5_0"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 0 & communes_dates_1989_2022_temperature_final_1989$value1 < 5]<-"0_5"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 5 & communes_dates_1989_2022_temperature_final_1989$value1 < 10]<-"5_10"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 10 & communes_dates_1989_2022_temperature_final_1989$value1 < 15]<-"10_15"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 15 & communes_dates_1989_2022_temperature_final_1989$value1 < 20]<-"15_20"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 20 & communes_dates_1989_2022_temperature_final_1989$value1 < 25]<-"20_25"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 25 & communes_dates_1989_2022_temperature_final_1989$value1 < 28]<-"25_28"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 28 & communes_dates_1989_2022_temperature_final_1989$value1 < 30]<-"28_30"

communes_dates_1989_2022_temperature_final_1989$temperature_bin[communes_dates_1989_2022_temperature_final_1989$value1 >= 30]<-">30"


#test<-filter(communes_dates_1989_2022_temperature_final_1989, is.na(temperature_bin))
#table(communes_dates_1989_2022_temperature_final_1989$temperature_bin)

library(fastDummies)
communes_dates_1989_2022_temperature_final_1989  <- communes_dates_1989_2022_temperature_final_1989  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1989_2022_temperature_final_1989 <- communes_dates_1989_2022_temperature_final_1989 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1989_2022_temperature_final_1989 <- communes_dates_1989_2022_temperature_final_1989 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1989_2022_temperature_final_1989$same_value[is.na(communes_dates_1989_2022_temperature_final_1989$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1989_2022_temperature_final_1989$same_value <- ifelse(communes_dates_1989_2022_temperature_final_1989$temperature_bin != ">30", 0, communes_dates_1989_2022_temperature_final_1989$same_value)



communes_dates_1989_2022_temperature_final_1989$mois<-substring(communes_dates_1989_2022_temperature_final_1989$date,6,7)

communes_dates_1989_2022_temperature_final_1989<-communes_dates_1989_2022_temperature_final_1989[,-c("date","value1","temperature_bin")]

communes_dates_1989_2022_temperature_final_1989<-aggregate(.~COM+mois,communes_dates_1989_2022_temperature_final_1989,sum)



RP_1989_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1989_age_sexe_final_2")

RP_1989_age_sexe_final_2<-RP_1989_age_sexe_final_2[,c(2:15)]

names(RP_1989_age_sexe_final_2)[names(RP_1989_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1989_2022_temperature_final_1989<-left_join(communes_dates_1989_2022_temperature_final_1989,RP_1989_age_sexe_final_2)

#tests<-filter(communes_dates_1989_2022_temperature_final_1989, COM=="01001")
#tests<-filter(communes_dates_1989_2022_temperature_final_1989, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989, !is.na(value_estimated_sum_homme) )

RP_1989_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1989_CSP_final_2")

RP_1989_CSP_final_2<-RP_1989_CSP_final_2[,c(2:12)]
names(RP_1989_CSP_final_2)[names(RP_1989_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1989_CSP_final_2)[names(RP_1989_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1989_2022_temperature_final_1989<-left_join(communes_dates_1989_2022_temperature_final_1989,RP_1989_CSP_final_2)


communes_dates_1989_2022_temperature_final_1989$part_agriculteur<-communes_dates_1989_2022_temperature_final_1989$value_estimated_agriculteur/communes_dates_1989_2022_temperature_final_1989$population_actif_25_54

communes_dates_1989_2022_temperature_final_1989$part_artisan_commercant_chef_entreprise<-communes_dates_1989_2022_temperature_final_1989$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1989_2022_temperature_final_1989$population_actif_25_54

communes_dates_1989_2022_temperature_final_1989$part_cadre<-communes_dates_1989_2022_temperature_final_1989$value_estimated_cadre/communes_dates_1989_2022_temperature_final_1989$population_actif_25_54

communes_dates_1989_2022_temperature_final_1989$part_profession_intermediaire<-communes_dates_1989_2022_temperature_final_1989$value_estimated_profession_intermediaire/communes_dates_1989_2022_temperature_final_1989$population_actif_25_54

communes_dates_1989_2022_temperature_final_1989$part_employe<-communes_dates_1989_2022_temperature_final_1989$value_estimated_employe/communes_dates_1989_2022_temperature_final_1989$population_actif_25_54

communes_dates_1989_2022_temperature_final_1989$part_ouvrier<-communes_dates_1989_2022_temperature_final_1989$value_estimated_ouvrier/communes_dates_1989_2022_temperature_final_1989$population_actif_25_54

communes_dates_1989_2022_temperature_final_1989$part_chomage<-communes_dates_1989_2022_temperature_final_1989$value_estimated_au_chomage/communes_dates_1989_2022_temperature_final_1989$population_actif_25_54






communes_dates_1989_2022_temperature_final_1989$taux_mortalite_homme<-communes_dates_1989_2022_temperature_final_1989$Homme/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_homme

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_femme<-communes_dates_1989_2022_temperature_final_1989$Femme/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_femme


communes_dates_1989_2022_temperature_final_1989$taux_mortalite_0_9<-communes_dates_1989_2022_temperature_final_1989$`0-9`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_0_9_h_f

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_10_19<-communes_dates_1989_2022_temperature_final_1989$`10-19`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_10_19_h_f



communes_dates_1989_2022_temperature_final_1989$taux_mortalite_20_39<-communes_dates_1989_2022_temperature_final_1989$`20-39`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_20_39_h_f




communes_dates_1989_2022_temperature_final_1989$taux_mortalite_40_59<-communes_dates_1989_2022_temperature_final_1989$`40-59`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_40_59_h_f





communes_dates_1989_2022_temperature_final_1989$taux_mortalite_60_64<-communes_dates_1989_2022_temperature_final_1989$`60-64`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_60_64_h_f



communes_dates_1989_2022_temperature_final_1989$taux_mortalite_65_69<-communes_dates_1989_2022_temperature_final_1989$`65-69`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_65_69_h_f


communes_dates_1989_2022_temperature_final_1989$taux_mortalite_70_74<-communes_dates_1989_2022_temperature_final_1989$`70-74`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_70_74_h_f


communes_dates_1989_2022_temperature_final_1989$taux_mortalite_75_79<-communes_dates_1989_2022_temperature_final_1989$`75-79`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_75_79_h_f


communes_dates_1989_2022_temperature_final_1989$taux_mortalite_80_plus<-communes_dates_1989_2022_temperature_final_1989$`80+`/communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_80_plus_h_f


#communes_dates_1989_2022_temperature_final_1989$taux_mortalite_60_70<-(communes_dates_1989_2022_temperature_final_1989$`60-64`+communes_dates_1989_2022_temperature_final_1989$`65-69`)/(communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_60_64_h_f+communes_dates_1989_2022_temperature_final_1989$value_estimated_sum_65_69_h_f)



communes_dates_1989_2022_temperature_final_1989$mort_total<-communes_dates_1989_2022_temperature_final_1989$Femme+communes_dates_1989_2022_temperature_final_1989$Homme


communes_dates_1989_2022_temperature_final_1989$taux_mortalite_total<-communes_dates_1989_2022_temperature_final_1989$mort_total/communes_dates_1989_2022_temperature_final_1989$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989, communes_dates_1989_2022_temperature_final_1989$value_estimated_population>0)
communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989, communes_dates_1989_2022_temperature_final_1989$population_actif_25_54>0)




communes_dates_1989_2022_temperature_final_1989<- communes_dates_1989_2022_temperature_final_1989[ , !names(communes_dates_1989_2022_temperature_final_1989) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_femme))


communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(part_agriculteur))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(part_cadre))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(part_profession_intermediaire))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(part_employe))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(part_ouvrier))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(part_chomage))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_homme))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_0_9))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_10_19))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_20_39))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_40_59))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_60_64))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_65_69))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_70_74))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_75_79))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1989_2022_temperature_final_1989<-communes_dates_1989_2022_temperature_final_1989[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  part_agriculteur<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_10_19<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  part_cadre<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  part_profession_intermediaire<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  part_employe<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  part_ouvrier<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  part_chomage<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_homme<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_femme<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_0_9<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_20_39<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_40_59<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_60_64<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_65_69<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_70_74<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_75_79<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_80_plus<=1)

#communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989,  taux_mortalite_total<=1)




communes_dates_1989_2022_temperature_final_1989$taux_mortalite_homme[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_homme>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_femme[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_femme>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_0_9[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_0_9>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_10_19[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_10_19>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_20_39[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_20_39>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_40_59[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_40_59>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_60_64[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_60_64>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_65_69[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_65_69>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_70_74[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_70_74>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_75_79[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_75_79>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_80_plus[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_80_plus>1]<-NA   

communes_dates_1989_2022_temperature_final_1989$taux_mortalite_total[communes_dates_1989_2022_temperature_final_1989$taux_mortalite_total>1]<-NA   





summary(communes_dates_1989_2022_temperature_final_1989$part_agriculteur)

summary(communes_dates_1989_2022_temperature_final_1989$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1989_2022_temperature_final_1989$part_cadre)

summary(communes_dates_1989_2022_temperature_final_1989$part_profession_intermediaire)

summary(communes_dates_1989_2022_temperature_final_1989$part_employe)

summary(communes_dates_1989_2022_temperature_final_1989$part_ouvrier)

summary(communes_dates_1989_2022_temperature_final_1989$part_chomage)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_homme)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_femme)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_0_9)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_10_19)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_20_39)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_40_59)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_60_64)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_65_69)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_70_74)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_75_79)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_80_plus)

summary(communes_dates_1989_2022_temperature_final_1989$taux_mortalite_total)






fwrite(communes_dates_1989_2022_temperature_final_1989,"/données communes années/données mortalité temperature final mois new/communes_dates_1989_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1990_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1990-01-01")
end_date <- as.Date("1990-12-31")

communes_dates_1990_2022_temperature_final_1990 <- communes_dates_1990_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1990_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1990_age_sexe.csv")




communes_dates_1990_2022_temperature_final_1990$date<-as.Date(communes_dates_1990_2022_temperature_final_1990$date)
deces.1990_age_sexe$date<-as.Date(deces.1990_age_sexe$date)





communes_dates_1990_2022_temperature_final_1990<-left_join(communes_dates_1990_2022_temperature_final_1990,deces.1990_age_sexe)

communes_dates_1990_2022_temperature_final_1990[is.na(communes_dates_1990_2022_temperature_final_1990)]<-0


communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 < -20]<-"<-20"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= -20 & communes_dates_1990_2022_temperature_final_1990$value1 < -15]<-"-20_-15"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= -15 & communes_dates_1990_2022_temperature_final_1990$value1 < -10]<-"-15_-10"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= -10 & communes_dates_1990_2022_temperature_final_1990$value1 < -5]<-"-10_-5"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= -5 & communes_dates_1990_2022_temperature_final_1990$value1 < 0]<-"-5_0"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 0 & communes_dates_1990_2022_temperature_final_1990$value1 < 5]<-"0_5"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 5 & communes_dates_1990_2022_temperature_final_1990$value1 < 10]<-"5_10"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 10 & communes_dates_1990_2022_temperature_final_1990$value1 < 15]<-"10_15"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 15 & communes_dates_1990_2022_temperature_final_1990$value1 < 20]<-"15_20"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 20 & communes_dates_1990_2022_temperature_final_1990$value1 < 25]<-"20_25"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 25 & communes_dates_1990_2022_temperature_final_1990$value1 < 28]<-"25_28"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 28 & communes_dates_1990_2022_temperature_final_1990$value1 < 30]<-"28_30"

communes_dates_1990_2022_temperature_final_1990$temperature_bin[communes_dates_1990_2022_temperature_final_1990$value1 >= 30]<-">30"


#test<-filter(communes_dates_1990_2022_temperature_final_1990, is.na(temperature_bin))
#table(communes_dates_1990_2022_temperature_final_1990$temperature_bin)

library(fastDummies)
communes_dates_1990_2022_temperature_final_1990  <- communes_dates_1990_2022_temperature_final_1990  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1990_2022_temperature_final_1990 <- communes_dates_1990_2022_temperature_final_1990 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1990_2022_temperature_final_1990 <- communes_dates_1990_2022_temperature_final_1990 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1990_2022_temperature_final_1990$same_value[is.na(communes_dates_1990_2022_temperature_final_1990$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1990_2022_temperature_final_1990$same_value <- ifelse(communes_dates_1990_2022_temperature_final_1990$temperature_bin != ">30", 0, communes_dates_1990_2022_temperature_final_1990$same_value)



communes_dates_1990_2022_temperature_final_1990$mois<-substring(communes_dates_1990_2022_temperature_final_1990$date,6,7)

communes_dates_1990_2022_temperature_final_1990<-communes_dates_1990_2022_temperature_final_1990[,-c("date","value1","temperature_bin")]

communes_dates_1990_2022_temperature_final_1990<-aggregate(.~COM+mois,communes_dates_1990_2022_temperature_final_1990,sum)



RP_1990_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final_2")

RP_1990_age_sexe_final_2<-RP_1990_age_sexe_final_2[,c(2:15)]

names(RP_1990_age_sexe_final_2)[names(RP_1990_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1990_2022_temperature_final_1990<-left_join(communes_dates_1990_2022_temperature_final_1990,RP_1990_age_sexe_final_2)

#tests<-filter(communes_dates_1990_2022_temperature_final_1990, COM=="01001")
#tests<-filter(communes_dates_1990_2022_temperature_final_1990, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990, !is.na(value_estimated_sum_homme) )

RP_1990_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1990_CSP_final_2")

RP_1990_CSP_final_2<-RP_1990_CSP_final_2[,c(2:12)]
names(RP_1990_CSP_final_2)[names(RP_1990_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1990_CSP_final_2)[names(RP_1990_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1990_2022_temperature_final_1990<-left_join(communes_dates_1990_2022_temperature_final_1990,RP_1990_CSP_final_2)


communes_dates_1990_2022_temperature_final_1990$part_agriculteur<-communes_dates_1990_2022_temperature_final_1990$value_estimated_agriculteur/communes_dates_1990_2022_temperature_final_1990$population_actif_25_54

communes_dates_1990_2022_temperature_final_1990$part_artisan_commercant_chef_entreprise<-communes_dates_1990_2022_temperature_final_1990$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1990_2022_temperature_final_1990$population_actif_25_54

communes_dates_1990_2022_temperature_final_1990$part_cadre<-communes_dates_1990_2022_temperature_final_1990$value_estimated_cadre/communes_dates_1990_2022_temperature_final_1990$population_actif_25_54

communes_dates_1990_2022_temperature_final_1990$part_profession_intermediaire<-communes_dates_1990_2022_temperature_final_1990$value_estimated_profession_intermediaire/communes_dates_1990_2022_temperature_final_1990$population_actif_25_54

communes_dates_1990_2022_temperature_final_1990$part_employe<-communes_dates_1990_2022_temperature_final_1990$value_estimated_employe/communes_dates_1990_2022_temperature_final_1990$population_actif_25_54

communes_dates_1990_2022_temperature_final_1990$part_ouvrier<-communes_dates_1990_2022_temperature_final_1990$value_estimated_ouvrier/communes_dates_1990_2022_temperature_final_1990$population_actif_25_54

communes_dates_1990_2022_temperature_final_1990$part_chomage<-communes_dates_1990_2022_temperature_final_1990$value_estimated_au_chomage/communes_dates_1990_2022_temperature_final_1990$population_actif_25_54






communes_dates_1990_2022_temperature_final_1990$taux_mortalite_homme<-communes_dates_1990_2022_temperature_final_1990$Homme/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_homme

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_femme<-communes_dates_1990_2022_temperature_final_1990$Femme/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_femme


communes_dates_1990_2022_temperature_final_1990$taux_mortalite_0_9<-communes_dates_1990_2022_temperature_final_1990$`0-9`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_0_9_h_f

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_10_19<-communes_dates_1990_2022_temperature_final_1990$`10-19`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_10_19_h_f



communes_dates_1990_2022_temperature_final_1990$taux_mortalite_20_39<-communes_dates_1990_2022_temperature_final_1990$`20-39`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_20_39_h_f




communes_dates_1990_2022_temperature_final_1990$taux_mortalite_40_59<-communes_dates_1990_2022_temperature_final_1990$`40-59`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_40_59_h_f





communes_dates_1990_2022_temperature_final_1990$taux_mortalite_60_64<-communes_dates_1990_2022_temperature_final_1990$`60-64`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_60_64_h_f



communes_dates_1990_2022_temperature_final_1990$taux_mortalite_65_69<-communes_dates_1990_2022_temperature_final_1990$`65-69`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_65_69_h_f


communes_dates_1990_2022_temperature_final_1990$taux_mortalite_70_74<-communes_dates_1990_2022_temperature_final_1990$`70-74`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_70_74_h_f


communes_dates_1990_2022_temperature_final_1990$taux_mortalite_75_79<-communes_dates_1990_2022_temperature_final_1990$`75-79`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_75_79_h_f


communes_dates_1990_2022_temperature_final_1990$taux_mortalite_80_plus<-communes_dates_1990_2022_temperature_final_1990$`80+`/communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_80_plus_h_f


#communes_dates_1990_2022_temperature_final_1990$taux_mortalite_60_70<-(communes_dates_1990_2022_temperature_final_1990$`60-64`+communes_dates_1990_2022_temperature_final_1990$`65-69`)/(communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_60_64_h_f+communes_dates_1990_2022_temperature_final_1990$value_estimated_sum_65_69_h_f)



communes_dates_1990_2022_temperature_final_1990$mort_total<-communes_dates_1990_2022_temperature_final_1990$Femme+communes_dates_1990_2022_temperature_final_1990$Homme


communes_dates_1990_2022_temperature_final_1990$taux_mortalite_total<-communes_dates_1990_2022_temperature_final_1990$mort_total/communes_dates_1990_2022_temperature_final_1990$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990, communes_dates_1990_2022_temperature_final_1990$value_estimated_population>0)
communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990, communes_dates_1990_2022_temperature_final_1990$population_actif_25_54>0)




communes_dates_1990_2022_temperature_final_1990<- communes_dates_1990_2022_temperature_final_1990[ , !names(communes_dates_1990_2022_temperature_final_1990) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_femme))


communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(part_agriculteur))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(part_cadre))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(part_profession_intermediaire))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(part_employe))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(part_ouvrier))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(part_chomage))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_homme))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_0_9))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_10_19))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_20_39))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_40_59))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_60_64))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_65_69))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_70_74))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_75_79))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1990_2022_temperature_final_1990<-communes_dates_1990_2022_temperature_final_1990[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  part_agriculteur<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_10_19<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  part_cadre<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  part_profession_intermediaire<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  part_employe<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  part_ouvrier<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  part_chomage<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_homme<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_femme<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_0_9<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_20_39<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_40_59<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_60_64<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_65_69<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_70_74<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_75_79<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_80_plus<=1)

#communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990,  taux_mortalite_total<=1)




communes_dates_1990_2022_temperature_final_1990$taux_mortalite_homme[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_homme>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_femme[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_femme>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_0_9[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_0_9>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_10_19[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_10_19>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_20_39[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_20_39>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_40_59[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_40_59>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_60_64[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_60_64>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_65_69[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_65_69>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_70_74[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_70_74>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_75_79[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_75_79>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_80_plus[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_80_plus>1]<-NA   

communes_dates_1990_2022_temperature_final_1990$taux_mortalite_total[communes_dates_1990_2022_temperature_final_1990$taux_mortalite_total>1]<-NA   





summary(communes_dates_1990_2022_temperature_final_1990$part_agriculteur)

summary(communes_dates_1990_2022_temperature_final_1990$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1990_2022_temperature_final_1990$part_cadre)

summary(communes_dates_1990_2022_temperature_final_1990$part_profession_intermediaire)

summary(communes_dates_1990_2022_temperature_final_1990$part_employe)

summary(communes_dates_1990_2022_temperature_final_1990$part_ouvrier)

summary(communes_dates_1990_2022_temperature_final_1990$part_chomage)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_homme)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_femme)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_0_9)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_10_19)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_20_39)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_40_59)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_60_64)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_65_69)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_70_74)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_75_79)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_80_plus)

summary(communes_dates_1990_2022_temperature_final_1990$taux_mortalite_total)






fwrite(communes_dates_1990_2022_temperature_final_1990,"/données communes années/données mortalité temperature final mois new/communes_dates_1990_temperature_deces_mois.csv")








################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1991_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1991-01-01")
end_date <- as.Date("1991-12-31")

communes_dates_1991_2022_temperature_final_1991 <- communes_dates_1991_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1991_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1991_age_sexe.csv")




communes_dates_1991_2022_temperature_final_1991$date<-as.Date(communes_dates_1991_2022_temperature_final_1991$date)
deces.1991_age_sexe$date<-as.Date(deces.1991_age_sexe$date)






communes_dates_1991_2022_temperature_final_1991<-left_join(communes_dates_1991_2022_temperature_final_1991,deces.1991_age_sexe)

communes_dates_1991_2022_temperature_final_1991[is.na(communes_dates_1991_2022_temperature_final_1991)]<-0


communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 < -20]<-"<-20"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= -20 & communes_dates_1991_2022_temperature_final_1991$value1 < -15]<-"-20_-15"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= -15 & communes_dates_1991_2022_temperature_final_1991$value1 < -10]<-"-15_-10"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= -10 & communes_dates_1991_2022_temperature_final_1991$value1 < -5]<-"-10_-5"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= -5 & communes_dates_1991_2022_temperature_final_1991$value1 < 0]<-"-5_0"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 0 & communes_dates_1991_2022_temperature_final_1991$value1 < 5]<-"0_5"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 5 & communes_dates_1991_2022_temperature_final_1991$value1 < 10]<-"5_10"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 10 & communes_dates_1991_2022_temperature_final_1991$value1 < 15]<-"10_15"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 15 & communes_dates_1991_2022_temperature_final_1991$value1 < 20]<-"15_20"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 20 & communes_dates_1991_2022_temperature_final_1991$value1 < 25]<-"20_25"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 25 & communes_dates_1991_2022_temperature_final_1991$value1 < 28]<-"25_28"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 28 & communes_dates_1991_2022_temperature_final_1991$value1 < 30]<-"28_30"

communes_dates_1991_2022_temperature_final_1991$temperature_bin[communes_dates_1991_2022_temperature_final_1991$value1 >= 30]<-">30"


#test<-filter(communes_dates_1991_2022_temperature_final_1991, is.na(temperature_bin))
#table(communes_dates_1991_2022_temperature_final_1991$temperature_bin)

library(fastDummies)
communes_dates_1991_2022_temperature_final_1991  <- communes_dates_1991_2022_temperature_final_1991  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1991_2022_temperature_final_1991 <- communes_dates_1991_2022_temperature_final_1991 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1991_2022_temperature_final_1991 <- communes_dates_1991_2022_temperature_final_1991 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1991_2022_temperature_final_1991$same_value[is.na(communes_dates_1991_2022_temperature_final_1991$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1991_2022_temperature_final_1991$same_value <- ifelse(communes_dates_1991_2022_temperature_final_1991$temperature_bin != ">30", 0, communes_dates_1991_2022_temperature_final_1991$same_value)



communes_dates_1991_2022_temperature_final_1991$mois<-substring(communes_dates_1991_2022_temperature_final_1991$date,6,7)

communes_dates_1991_2022_temperature_final_1991<-communes_dates_1991_2022_temperature_final_1991[,-c("date","value1","temperature_bin")]

communes_dates_1991_2022_temperature_final_1991<-aggregate(.~COM+mois,communes_dates_1991_2022_temperature_final_1991,sum)



RP_1991_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1991_age_sexe_final_2")

RP_1991_age_sexe_final_2<-RP_1991_age_sexe_final_2[,c(2:15)]

names(RP_1991_age_sexe_final_2)[names(RP_1991_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1991_2022_temperature_final_1991<-left_join(communes_dates_1991_2022_temperature_final_1991,RP_1991_age_sexe_final_2)

#tests<-filter(communes_dates_1991_2022_temperature_final_1991, COM=="01001")
#tests<-filter(communes_dates_1991_2022_temperature_final_1991, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991, !is.na(value_estimated_sum_homme) )

RP_1991_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1991_CSP_final_2")

RP_1991_CSP_final_2<-RP_1991_CSP_final_2[,c(2:12)]
names(RP_1991_CSP_final_2)[names(RP_1991_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1991_CSP_final_2)[names(RP_1991_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1991_2022_temperature_final_1991<-left_join(communes_dates_1991_2022_temperature_final_1991,RP_1991_CSP_final_2)


communes_dates_1991_2022_temperature_final_1991$part_agriculteur<-communes_dates_1991_2022_temperature_final_1991$value_estimated_agriculteur/communes_dates_1991_2022_temperature_final_1991$population_actif_25_54

communes_dates_1991_2022_temperature_final_1991$part_artisan_commercant_chef_entreprise<-communes_dates_1991_2022_temperature_final_1991$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1991_2022_temperature_final_1991$population_actif_25_54

communes_dates_1991_2022_temperature_final_1991$part_cadre<-communes_dates_1991_2022_temperature_final_1991$value_estimated_cadre/communes_dates_1991_2022_temperature_final_1991$population_actif_25_54

communes_dates_1991_2022_temperature_final_1991$part_profession_intermediaire<-communes_dates_1991_2022_temperature_final_1991$value_estimated_profession_intermediaire/communes_dates_1991_2022_temperature_final_1991$population_actif_25_54

communes_dates_1991_2022_temperature_final_1991$part_employe<-communes_dates_1991_2022_temperature_final_1991$value_estimated_employe/communes_dates_1991_2022_temperature_final_1991$population_actif_25_54

communes_dates_1991_2022_temperature_final_1991$part_ouvrier<-communes_dates_1991_2022_temperature_final_1991$value_estimated_ouvrier/communes_dates_1991_2022_temperature_final_1991$population_actif_25_54

communes_dates_1991_2022_temperature_final_1991$part_chomage<-communes_dates_1991_2022_temperature_final_1991$value_estimated_au_chomage/communes_dates_1991_2022_temperature_final_1991$population_actif_25_54






communes_dates_1991_2022_temperature_final_1991$taux_mortalite_homme<-communes_dates_1991_2022_temperature_final_1991$Homme/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_homme

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_femme<-communes_dates_1991_2022_temperature_final_1991$Femme/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_femme


communes_dates_1991_2022_temperature_final_1991$taux_mortalite_0_9<-communes_dates_1991_2022_temperature_final_1991$`0-9`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_0_9_h_f

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_10_19<-communes_dates_1991_2022_temperature_final_1991$`10-19`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_10_19_h_f



communes_dates_1991_2022_temperature_final_1991$taux_mortalite_20_39<-communes_dates_1991_2022_temperature_final_1991$`20-39`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_20_39_h_f




communes_dates_1991_2022_temperature_final_1991$taux_mortalite_40_59<-communes_dates_1991_2022_temperature_final_1991$`40-59`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_40_59_h_f





communes_dates_1991_2022_temperature_final_1991$taux_mortalite_60_64<-communes_dates_1991_2022_temperature_final_1991$`60-64`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_60_64_h_f



communes_dates_1991_2022_temperature_final_1991$taux_mortalite_65_69<-communes_dates_1991_2022_temperature_final_1991$`65-69`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_65_69_h_f


communes_dates_1991_2022_temperature_final_1991$taux_mortalite_70_74<-communes_dates_1991_2022_temperature_final_1991$`70-74`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_70_74_h_f


communes_dates_1991_2022_temperature_final_1991$taux_mortalite_75_79<-communes_dates_1991_2022_temperature_final_1991$`75-79`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_75_79_h_f


communes_dates_1991_2022_temperature_final_1991$taux_mortalite_80_plus<-communes_dates_1991_2022_temperature_final_1991$`80+`/communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_80_plus_h_f


#communes_dates_1991_2022_temperature_final_1991$taux_mortalite_60_70<-(communes_dates_1991_2022_temperature_final_1991$`60-64`+communes_dates_1991_2022_temperature_final_1991$`65-69`)/(communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_60_64_h_f+communes_dates_1991_2022_temperature_final_1991$value_estimated_sum_65_69_h_f)



communes_dates_1991_2022_temperature_final_1991$mort_total<-communes_dates_1991_2022_temperature_final_1991$Femme+communes_dates_1991_2022_temperature_final_1991$Homme


communes_dates_1991_2022_temperature_final_1991$taux_mortalite_total<-communes_dates_1991_2022_temperature_final_1991$mort_total/communes_dates_1991_2022_temperature_final_1991$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991, communes_dates_1991_2022_temperature_final_1991$value_estimated_population>0)
communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991, communes_dates_1991_2022_temperature_final_1991$population_actif_25_54>0)




communes_dates_1991_2022_temperature_final_1991<- communes_dates_1991_2022_temperature_final_1991[ , !names(communes_dates_1991_2022_temperature_final_1991) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_femme))


communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(part_agriculteur))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(part_cadre))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(part_profession_intermediaire))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(part_employe))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(part_ouvrier))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(part_chomage))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_homme))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_0_9))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_10_19))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_20_39))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_40_59))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_60_64))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_65_69))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_70_74))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_75_79))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1991_2022_temperature_final_1991<-communes_dates_1991_2022_temperature_final_1991[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  part_agriculteur<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_10_19<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  part_cadre<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  part_profession_intermediaire<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  part_employe<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  part_ouvrier<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  part_chomage<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_homme<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_femme<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_0_9<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_20_39<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_40_59<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_60_64<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_65_69<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_70_74<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_75_79<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_80_plus<=1)

#communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991,  taux_mortalite_total<=1)




communes_dates_1991_2022_temperature_final_1991$taux_mortalite_homme[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_homme>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_femme[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_femme>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_0_9[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_0_9>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_10_19[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_10_19>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_20_39[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_20_39>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_40_59[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_40_59>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_60_64[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_60_64>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_65_69[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_65_69>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_70_74[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_70_74>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_75_79[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_75_79>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_80_plus[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_80_plus>1]<-NA   

communes_dates_1991_2022_temperature_final_1991$taux_mortalite_total[communes_dates_1991_2022_temperature_final_1991$taux_mortalite_total>1]<-NA   





summary(communes_dates_1991_2022_temperature_final_1991$part_agriculteur)

summary(communes_dates_1991_2022_temperature_final_1991$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1991_2022_temperature_final_1991$part_cadre)

summary(communes_dates_1991_2022_temperature_final_1991$part_profession_intermediaire)

summary(communes_dates_1991_2022_temperature_final_1991$part_employe)

summary(communes_dates_1991_2022_temperature_final_1991$part_ouvrier)

summary(communes_dates_1991_2022_temperature_final_1991$part_chomage)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_homme)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_femme)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_0_9)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_10_19)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_20_39)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_40_59)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_60_64)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_65_69)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_70_74)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_75_79)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_80_plus)

summary(communes_dates_1991_2022_temperature_final_1991$taux_mortalite_total)






fwrite(communes_dates_1991_2022_temperature_final_1991,"/données communes années/données mortalité temperature final mois new/communes_dates_1991_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1992_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1992-01-01")
end_date <- as.Date("1992-12-31")

communes_dates_1992_2022_temperature_final_1992 <- communes_dates_1992_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1992_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1992_age_sexe.csv")




communes_dates_1992_2022_temperature_final_1992$date<-as.Date(communes_dates_1992_2022_temperature_final_1992$date)
deces.1992_age_sexe$date<-as.Date(deces.1992_age_sexe$date)






communes_dates_1992_2022_temperature_final_1992<-left_join(communes_dates_1992_2022_temperature_final_1992,deces.1992_age_sexe)

communes_dates_1992_2022_temperature_final_1992[is.na(communes_dates_1992_2022_temperature_final_1992)]<-0


communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 < -20]<-"<-20"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= -20 & communes_dates_1992_2022_temperature_final_1992$value1 < -15]<-"-20_-15"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= -15 & communes_dates_1992_2022_temperature_final_1992$value1 < -10]<-"-15_-10"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= -10 & communes_dates_1992_2022_temperature_final_1992$value1 < -5]<-"-10_-5"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= -5 & communes_dates_1992_2022_temperature_final_1992$value1 < 0]<-"-5_0"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 0 & communes_dates_1992_2022_temperature_final_1992$value1 < 5]<-"0_5"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 5 & communes_dates_1992_2022_temperature_final_1992$value1 < 10]<-"5_10"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 10 & communes_dates_1992_2022_temperature_final_1992$value1 < 15]<-"10_15"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 15 & communes_dates_1992_2022_temperature_final_1992$value1 < 20]<-"15_20"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 20 & communes_dates_1992_2022_temperature_final_1992$value1 < 25]<-"20_25"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 25 & communes_dates_1992_2022_temperature_final_1992$value1 < 28]<-"25_28"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 28 & communes_dates_1992_2022_temperature_final_1992$value1 < 30]<-"28_30"

communes_dates_1992_2022_temperature_final_1992$temperature_bin[communes_dates_1992_2022_temperature_final_1992$value1 >= 30]<-">30"


#test<-filter(communes_dates_1992_2022_temperature_final_1992, is.na(temperature_bin))
#table(communes_dates_1992_2022_temperature_final_1992$temperature_bin)

library(fastDummies)
communes_dates_1992_2022_temperature_final_1992  <- communes_dates_1992_2022_temperature_final_1992  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1992_2022_temperature_final_1992 <- communes_dates_1992_2022_temperature_final_1992 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1992_2022_temperature_final_1992 <- communes_dates_1992_2022_temperature_final_1992 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1992_2022_temperature_final_1992$same_value[is.na(communes_dates_1992_2022_temperature_final_1992$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1992_2022_temperature_final_1992$same_value <- ifelse(communes_dates_1992_2022_temperature_final_1992$temperature_bin != ">30", 0, communes_dates_1992_2022_temperature_final_1992$same_value)



communes_dates_1992_2022_temperature_final_1992$mois<-substring(communes_dates_1992_2022_temperature_final_1992$date,6,7)

communes_dates_1992_2022_temperature_final_1992<-communes_dates_1992_2022_temperature_final_1992[,-c("date","value1","temperature_bin")]

communes_dates_1992_2022_temperature_final_1992<-aggregate(.~COM+mois,communes_dates_1992_2022_temperature_final_1992,sum)



RP_1992_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1992_age_sexe_final_2")

RP_1992_age_sexe_final_2<-RP_1992_age_sexe_final_2[,c(2:15)]

names(RP_1992_age_sexe_final_2)[names(RP_1992_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1992_2022_temperature_final_1992<-left_join(communes_dates_1992_2022_temperature_final_1992,RP_1992_age_sexe_final_2)

#tests<-filter(communes_dates_1992_2022_temperature_final_1992, COM=="01001")
#tests<-filter(communes_dates_1992_2022_temperature_final_1992, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992, !is.na(value_estimated_sum_homme) )

RP_1992_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1992_CSP_final_2")

RP_1992_CSP_final_2<-RP_1992_CSP_final_2[,c(2:12)]
names(RP_1992_CSP_final_2)[names(RP_1992_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1992_CSP_final_2)[names(RP_1992_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1992_2022_temperature_final_1992<-left_join(communes_dates_1992_2022_temperature_final_1992,RP_1992_CSP_final_2)


communes_dates_1992_2022_temperature_final_1992$part_agriculteur<-communes_dates_1992_2022_temperature_final_1992$value_estimated_agriculteur/communes_dates_1992_2022_temperature_final_1992$population_actif_25_54

communes_dates_1992_2022_temperature_final_1992$part_artisan_commercant_chef_entreprise<-communes_dates_1992_2022_temperature_final_1992$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1992_2022_temperature_final_1992$population_actif_25_54

communes_dates_1992_2022_temperature_final_1992$part_cadre<-communes_dates_1992_2022_temperature_final_1992$value_estimated_cadre/communes_dates_1992_2022_temperature_final_1992$population_actif_25_54

communes_dates_1992_2022_temperature_final_1992$part_profession_intermediaire<-communes_dates_1992_2022_temperature_final_1992$value_estimated_profession_intermediaire/communes_dates_1992_2022_temperature_final_1992$population_actif_25_54

communes_dates_1992_2022_temperature_final_1992$part_employe<-communes_dates_1992_2022_temperature_final_1992$value_estimated_employe/communes_dates_1992_2022_temperature_final_1992$population_actif_25_54

communes_dates_1992_2022_temperature_final_1992$part_ouvrier<-communes_dates_1992_2022_temperature_final_1992$value_estimated_ouvrier/communes_dates_1992_2022_temperature_final_1992$population_actif_25_54

communes_dates_1992_2022_temperature_final_1992$part_chomage<-communes_dates_1992_2022_temperature_final_1992$value_estimated_au_chomage/communes_dates_1992_2022_temperature_final_1992$population_actif_25_54






communes_dates_1992_2022_temperature_final_1992$taux_mortalite_homme<-communes_dates_1992_2022_temperature_final_1992$Homme/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_homme

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_femme<-communes_dates_1992_2022_temperature_final_1992$Femme/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_femme


communes_dates_1992_2022_temperature_final_1992$taux_mortalite_0_9<-communes_dates_1992_2022_temperature_final_1992$`0-9`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_0_9_h_f

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_10_19<-communes_dates_1992_2022_temperature_final_1992$`10-19`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_10_19_h_f



communes_dates_1992_2022_temperature_final_1992$taux_mortalite_20_39<-communes_dates_1992_2022_temperature_final_1992$`20-39`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_20_39_h_f




communes_dates_1992_2022_temperature_final_1992$taux_mortalite_40_59<-communes_dates_1992_2022_temperature_final_1992$`40-59`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_40_59_h_f





communes_dates_1992_2022_temperature_final_1992$taux_mortalite_60_64<-communes_dates_1992_2022_temperature_final_1992$`60-64`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_60_64_h_f



communes_dates_1992_2022_temperature_final_1992$taux_mortalite_65_69<-communes_dates_1992_2022_temperature_final_1992$`65-69`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_65_69_h_f


communes_dates_1992_2022_temperature_final_1992$taux_mortalite_70_74<-communes_dates_1992_2022_temperature_final_1992$`70-74`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_70_74_h_f


communes_dates_1992_2022_temperature_final_1992$taux_mortalite_75_79<-communes_dates_1992_2022_temperature_final_1992$`75-79`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_75_79_h_f


communes_dates_1992_2022_temperature_final_1992$taux_mortalite_80_plus<-communes_dates_1992_2022_temperature_final_1992$`80+`/communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_80_plus_h_f


#communes_dates_1992_2022_temperature_final_1992$taux_mortalite_60_70<-(communes_dates_1992_2022_temperature_final_1992$`60-64`+communes_dates_1992_2022_temperature_final_1992$`65-69`)/(communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_60_64_h_f+communes_dates_1992_2022_temperature_final_1992$value_estimated_sum_65_69_h_f)



communes_dates_1992_2022_temperature_final_1992$mort_total<-communes_dates_1992_2022_temperature_final_1992$Femme+communes_dates_1992_2022_temperature_final_1992$Homme


communes_dates_1992_2022_temperature_final_1992$taux_mortalite_total<-communes_dates_1992_2022_temperature_final_1992$mort_total/communes_dates_1992_2022_temperature_final_1992$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992, communes_dates_1992_2022_temperature_final_1992$value_estimated_population>0)
communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992, communes_dates_1992_2022_temperature_final_1992$population_actif_25_54>0)




communes_dates_1992_2022_temperature_final_1992<- communes_dates_1992_2022_temperature_final_1992[ , !names(communes_dates_1992_2022_temperature_final_1992) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_femme))


communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(part_agriculteur))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(part_cadre))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(part_profession_intermediaire))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(part_employe))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(part_ouvrier))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(part_chomage))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_homme))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_0_9))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_10_19))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_20_39))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_40_59))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_60_64))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_65_69))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_70_74))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_75_79))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1992_2022_temperature_final_1992<-communes_dates_1992_2022_temperature_final_1992[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  part_agriculteur<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_10_19<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  part_cadre<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  part_profession_intermediaire<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  part_employe<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  part_ouvrier<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  part_chomage<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_homme<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_femme<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_0_9<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_20_39<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_40_59<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_60_64<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_65_69<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_70_74<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_75_79<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_80_plus<=1)

#communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992,  taux_mortalite_total<=1)




communes_dates_1992_2022_temperature_final_1992$taux_mortalite_homme[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_homme>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_femme[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_femme>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_0_9[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_0_9>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_10_19[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_10_19>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_20_39[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_20_39>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_40_59[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_40_59>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_60_64[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_60_64>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_65_69[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_65_69>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_70_74[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_70_74>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_75_79[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_75_79>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_80_plus[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_80_plus>1]<-NA   

communes_dates_1992_2022_temperature_final_1992$taux_mortalite_total[communes_dates_1992_2022_temperature_final_1992$taux_mortalite_total>1]<-NA   





summary(communes_dates_1992_2022_temperature_final_1992$part_agriculteur)

summary(communes_dates_1992_2022_temperature_final_1992$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1992_2022_temperature_final_1992$part_cadre)

summary(communes_dates_1992_2022_temperature_final_1992$part_profession_intermediaire)

summary(communes_dates_1992_2022_temperature_final_1992$part_employe)

summary(communes_dates_1992_2022_temperature_final_1992$part_ouvrier)

summary(communes_dates_1992_2022_temperature_final_1992$part_chomage)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_homme)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_femme)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_0_9)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_10_19)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_20_39)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_40_59)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_60_64)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_65_69)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_70_74)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_75_79)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_80_plus)

summary(communes_dates_1992_2022_temperature_final_1992$taux_mortalite_total)






fwrite(communes_dates_1992_2022_temperature_final_1992,"/données communes années/données mortalité temperature final mois new/communes_dates_1992_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1993_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1993-01-01")
end_date <- as.Date("1993-12-31")

communes_dates_1993_2022_temperature_final_1993 <- communes_dates_1993_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1993_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1993_age_sexe.csv")






communes_dates_1993_2022_temperature_final_1993$date<-as.Date(communes_dates_1993_2022_temperature_final_1993$date)
deces.1993_age_sexe$date<-as.Date(deces.1993_age_sexe$date)







communes_dates_1993_2022_temperature_final_1993<-left_join(communes_dates_1993_2022_temperature_final_1993,deces.1993_age_sexe)

communes_dates_1993_2022_temperature_final_1993[is.na(communes_dates_1993_2022_temperature_final_1993)]<-0


communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 < -20]<-"<-20"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= -20 & communes_dates_1993_2022_temperature_final_1993$value1 < -15]<-"-20_-15"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= -15 & communes_dates_1993_2022_temperature_final_1993$value1 < -10]<-"-15_-10"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= -10 & communes_dates_1993_2022_temperature_final_1993$value1 < -5]<-"-10_-5"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= -5 & communes_dates_1993_2022_temperature_final_1993$value1 < 0]<-"-5_0"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 0 & communes_dates_1993_2022_temperature_final_1993$value1 < 5]<-"0_5"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 5 & communes_dates_1993_2022_temperature_final_1993$value1 < 10]<-"5_10"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 10 & communes_dates_1993_2022_temperature_final_1993$value1 < 15]<-"10_15"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 15 & communes_dates_1993_2022_temperature_final_1993$value1 < 20]<-"15_20"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 20 & communes_dates_1993_2022_temperature_final_1993$value1 < 25]<-"20_25"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 25 & communes_dates_1993_2022_temperature_final_1993$value1 < 28]<-"25_28"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 28 & communes_dates_1993_2022_temperature_final_1993$value1 < 30]<-"28_30"

communes_dates_1993_2022_temperature_final_1993$temperature_bin[communes_dates_1993_2022_temperature_final_1993$value1 >= 30]<-">30"


#test<-filter(communes_dates_1993_2022_temperature_final_1993, is.na(temperature_bin))
#table(communes_dates_1993_2022_temperature_final_1993$temperature_bin)

library(fastDummies)
communes_dates_1993_2022_temperature_final_1993  <- communes_dates_1993_2022_temperature_final_1993  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1993_2022_temperature_final_1993 <- communes_dates_1993_2022_temperature_final_1993 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1993_2022_temperature_final_1993 <- communes_dates_1993_2022_temperature_final_1993 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1993_2022_temperature_final_1993$same_value[is.na(communes_dates_1993_2022_temperature_final_1993$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1993_2022_temperature_final_1993$same_value <- ifelse(communes_dates_1993_2022_temperature_final_1993$temperature_bin != ">30", 0, communes_dates_1993_2022_temperature_final_1993$same_value)



communes_dates_1993_2022_temperature_final_1993$mois<-substring(communes_dates_1993_2022_temperature_final_1993$date,6,7)

communes_dates_1993_2022_temperature_final_1993<-communes_dates_1993_2022_temperature_final_1993[,-c("date","value1","temperature_bin")]

communes_dates_1993_2022_temperature_final_1993<-aggregate(.~COM+mois,communes_dates_1993_2022_temperature_final_1993,sum)



RP_1993_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1993_age_sexe_final_2")

RP_1993_age_sexe_final_2<-RP_1993_age_sexe_final_2[,c(2:15)]

names(RP_1993_age_sexe_final_2)[names(RP_1993_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1993_2022_temperature_final_1993<-left_join(communes_dates_1993_2022_temperature_final_1993,RP_1993_age_sexe_final_2)

#tests<-filter(communes_dates_1993_2022_temperature_final_1993, COM=="01001")
#tests<-filter(communes_dates_1993_2022_temperature_final_1993, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993, !is.na(value_estimated_sum_homme) )

RP_1993_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1993_CSP_final_2")

RP_1993_CSP_final_2<-RP_1993_CSP_final_2[,c(2:12)]
names(RP_1993_CSP_final_2)[names(RP_1993_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1993_CSP_final_2)[names(RP_1993_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1993_2022_temperature_final_1993<-left_join(communes_dates_1993_2022_temperature_final_1993,RP_1993_CSP_final_2)


communes_dates_1993_2022_temperature_final_1993$part_agriculteur<-communes_dates_1993_2022_temperature_final_1993$value_estimated_agriculteur/communes_dates_1993_2022_temperature_final_1993$population_actif_25_54

communes_dates_1993_2022_temperature_final_1993$part_artisan_commercant_chef_entreprise<-communes_dates_1993_2022_temperature_final_1993$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1993_2022_temperature_final_1993$population_actif_25_54

communes_dates_1993_2022_temperature_final_1993$part_cadre<-communes_dates_1993_2022_temperature_final_1993$value_estimated_cadre/communes_dates_1993_2022_temperature_final_1993$population_actif_25_54

communes_dates_1993_2022_temperature_final_1993$part_profession_intermediaire<-communes_dates_1993_2022_temperature_final_1993$value_estimated_profession_intermediaire/communes_dates_1993_2022_temperature_final_1993$population_actif_25_54

communes_dates_1993_2022_temperature_final_1993$part_employe<-communes_dates_1993_2022_temperature_final_1993$value_estimated_employe/communes_dates_1993_2022_temperature_final_1993$population_actif_25_54

communes_dates_1993_2022_temperature_final_1993$part_ouvrier<-communes_dates_1993_2022_temperature_final_1993$value_estimated_ouvrier/communes_dates_1993_2022_temperature_final_1993$population_actif_25_54

communes_dates_1993_2022_temperature_final_1993$part_chomage<-communes_dates_1993_2022_temperature_final_1993$value_estimated_au_chomage/communes_dates_1993_2022_temperature_final_1993$population_actif_25_54






communes_dates_1993_2022_temperature_final_1993$taux_mortalite_homme<-communes_dates_1993_2022_temperature_final_1993$Homme/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_homme

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_femme<-communes_dates_1993_2022_temperature_final_1993$Femme/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_femme


communes_dates_1993_2022_temperature_final_1993$taux_mortalite_0_9<-communes_dates_1993_2022_temperature_final_1993$`0-9`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_0_9_h_f

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_10_19<-communes_dates_1993_2022_temperature_final_1993$`10-19`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_10_19_h_f



communes_dates_1993_2022_temperature_final_1993$taux_mortalite_20_39<-communes_dates_1993_2022_temperature_final_1993$`20-39`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_20_39_h_f




communes_dates_1993_2022_temperature_final_1993$taux_mortalite_40_59<-communes_dates_1993_2022_temperature_final_1993$`40-59`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_40_59_h_f





communes_dates_1993_2022_temperature_final_1993$taux_mortalite_60_64<-communes_dates_1993_2022_temperature_final_1993$`60-64`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_60_64_h_f



communes_dates_1993_2022_temperature_final_1993$taux_mortalite_65_69<-communes_dates_1993_2022_temperature_final_1993$`65-69`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_65_69_h_f


communes_dates_1993_2022_temperature_final_1993$taux_mortalite_70_74<-communes_dates_1993_2022_temperature_final_1993$`70-74`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_70_74_h_f


communes_dates_1993_2022_temperature_final_1993$taux_mortalite_75_79<-communes_dates_1993_2022_temperature_final_1993$`75-79`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_75_79_h_f


communes_dates_1993_2022_temperature_final_1993$taux_mortalite_80_plus<-communes_dates_1993_2022_temperature_final_1993$`80+`/communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_80_plus_h_f


#communes_dates_1993_2022_temperature_final_1993$taux_mortalite_60_70<-(communes_dates_1993_2022_temperature_final_1993$`60-64`+communes_dates_1993_2022_temperature_final_1993$`65-69`)/(communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_60_64_h_f+communes_dates_1993_2022_temperature_final_1993$value_estimated_sum_65_69_h_f)



communes_dates_1993_2022_temperature_final_1993$mort_total<-communes_dates_1993_2022_temperature_final_1993$Femme+communes_dates_1993_2022_temperature_final_1993$Homme


communes_dates_1993_2022_temperature_final_1993$taux_mortalite_total<-communes_dates_1993_2022_temperature_final_1993$mort_total/communes_dates_1993_2022_temperature_final_1993$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993, communes_dates_1993_2022_temperature_final_1993$value_estimated_population>0)
communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993, communes_dates_1993_2022_temperature_final_1993$population_actif_25_54>0)




communes_dates_1993_2022_temperature_final_1993<- communes_dates_1993_2022_temperature_final_1993[ , !names(communes_dates_1993_2022_temperature_final_1993) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_femme))


communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(part_agriculteur))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(part_cadre))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(part_profession_intermediaire))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(part_employe))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(part_ouvrier))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(part_chomage))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_homme))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_0_9))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_10_19))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_20_39))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_40_59))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_60_64))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_65_69))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_70_74))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_75_79))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1993_2022_temperature_final_1993<-communes_dates_1993_2022_temperature_final_1993[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  part_agriculteur<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_10_19<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  part_cadre<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  part_profession_intermediaire<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  part_employe<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  part_ouvrier<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  part_chomage<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_homme<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_femme<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_0_9<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_20_39<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_40_59<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_60_64<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_65_69<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_70_74<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_75_79<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_80_plus<=1)

#communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993,  taux_mortalite_total<=1)




communes_dates_1993_2022_temperature_final_1993$taux_mortalite_homme[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_homme>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_femme[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_femme>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_0_9[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_0_9>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_10_19[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_10_19>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_20_39[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_20_39>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_40_59[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_40_59>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_60_64[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_60_64>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_65_69[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_65_69>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_70_74[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_70_74>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_75_79[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_75_79>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_80_plus[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_80_plus>1]<-NA   

communes_dates_1993_2022_temperature_final_1993$taux_mortalite_total[communes_dates_1993_2022_temperature_final_1993$taux_mortalite_total>1]<-NA   





summary(communes_dates_1993_2022_temperature_final_1993$part_agriculteur)

summary(communes_dates_1993_2022_temperature_final_1993$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1993_2022_temperature_final_1993$part_cadre)

summary(communes_dates_1993_2022_temperature_final_1993$part_profession_intermediaire)

summary(communes_dates_1993_2022_temperature_final_1993$part_employe)

summary(communes_dates_1993_2022_temperature_final_1993$part_ouvrier)

summary(communes_dates_1993_2022_temperature_final_1993$part_chomage)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_homme)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_femme)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_0_9)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_10_19)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_20_39)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_40_59)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_60_64)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_65_69)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_70_74)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_75_79)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_80_plus)

summary(communes_dates_1993_2022_temperature_final_1993$taux_mortalite_total)






fwrite(communes_dates_1993_2022_temperature_final_1993,"/données communes années/données mortalité temperature final mois new/communes_dates_1993_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1994_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1994-01-01")
end_date <- as.Date("1994-12-31")

communes_dates_1994_2022_temperature_final_1994 <- communes_dates_1994_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1994_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1994_age_sexe.csv")





communes_dates_1994_2022_temperature_final_1994$date<-as.Date(communes_dates_1994_2022_temperature_final_1994$date)
deces.1994_age_sexe$date<-as.Date(deces.1994_age_sexe$date)





communes_dates_1994_2022_temperature_final_1994<-left_join(communes_dates_1994_2022_temperature_final_1994,deces.1994_age_sexe)

communes_dates_1994_2022_temperature_final_1994[is.na(communes_dates_1994_2022_temperature_final_1994)]<-0


communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 < -20]<-"<-20"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= -20 & communes_dates_1994_2022_temperature_final_1994$value1 < -15]<-"-20_-15"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= -15 & communes_dates_1994_2022_temperature_final_1994$value1 < -10]<-"-15_-10"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= -10 & communes_dates_1994_2022_temperature_final_1994$value1 < -5]<-"-10_-5"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= -5 & communes_dates_1994_2022_temperature_final_1994$value1 < 0]<-"-5_0"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 0 & communes_dates_1994_2022_temperature_final_1994$value1 < 5]<-"0_5"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 5 & communes_dates_1994_2022_temperature_final_1994$value1 < 10]<-"5_10"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 10 & communes_dates_1994_2022_temperature_final_1994$value1 < 15]<-"10_15"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 15 & communes_dates_1994_2022_temperature_final_1994$value1 < 20]<-"15_20"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 20 & communes_dates_1994_2022_temperature_final_1994$value1 < 25]<-"20_25"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 25 & communes_dates_1994_2022_temperature_final_1994$value1 < 28]<-"25_28"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 28 & communes_dates_1994_2022_temperature_final_1994$value1 < 30]<-"28_30"

communes_dates_1994_2022_temperature_final_1994$temperature_bin[communes_dates_1994_2022_temperature_final_1994$value1 >= 30]<-">30"


#test<-filter(communes_dates_1994_2022_temperature_final_1994, is.na(temperature_bin))
#table(communes_dates_1994_2022_temperature_final_1994$temperature_bin)

library(fastDummies)
communes_dates_1994_2022_temperature_final_1994  <- communes_dates_1994_2022_temperature_final_1994  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1994_2022_temperature_final_1994 <- communes_dates_1994_2022_temperature_final_1994 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1994_2022_temperature_final_1994 <- communes_dates_1994_2022_temperature_final_1994 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1994_2022_temperature_final_1994$same_value[is.na(communes_dates_1994_2022_temperature_final_1994$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1994_2022_temperature_final_1994$same_value <- ifelse(communes_dates_1994_2022_temperature_final_1994$temperature_bin != ">30", 0, communes_dates_1994_2022_temperature_final_1994$same_value)



communes_dates_1994_2022_temperature_final_1994$mois<-substring(communes_dates_1994_2022_temperature_final_1994$date,6,7)

communes_dates_1994_2022_temperature_final_1994<-communes_dates_1994_2022_temperature_final_1994[,-c("date","value1","temperature_bin")]

communes_dates_1994_2022_temperature_final_1994<-aggregate(.~COM+mois,communes_dates_1994_2022_temperature_final_1994,sum)



RP_1994_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1994_age_sexe_final_2")

RP_1994_age_sexe_final_2<-RP_1994_age_sexe_final_2[,c(2:15)]

names(RP_1994_age_sexe_final_2)[names(RP_1994_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1994_2022_temperature_final_1994<-left_join(communes_dates_1994_2022_temperature_final_1994,RP_1994_age_sexe_final_2)

#tests<-filter(communes_dates_1994_2022_temperature_final_1994, COM=="01001")
#tests<-filter(communes_dates_1994_2022_temperature_final_1994, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994, !is.na(value_estimated_sum_homme) )

RP_1994_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1994_CSP_final_2")

RP_1994_CSP_final_2<-RP_1994_CSP_final_2[,c(2:12)]
names(RP_1994_CSP_final_2)[names(RP_1994_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1994_CSP_final_2)[names(RP_1994_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1994_2022_temperature_final_1994<-left_join(communes_dates_1994_2022_temperature_final_1994,RP_1994_CSP_final_2)


communes_dates_1994_2022_temperature_final_1994$part_agriculteur<-communes_dates_1994_2022_temperature_final_1994$value_estimated_agriculteur/communes_dates_1994_2022_temperature_final_1994$population_actif_25_54

communes_dates_1994_2022_temperature_final_1994$part_artisan_commercant_chef_entreprise<-communes_dates_1994_2022_temperature_final_1994$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1994_2022_temperature_final_1994$population_actif_25_54

communes_dates_1994_2022_temperature_final_1994$part_cadre<-communes_dates_1994_2022_temperature_final_1994$value_estimated_cadre/communes_dates_1994_2022_temperature_final_1994$population_actif_25_54

communes_dates_1994_2022_temperature_final_1994$part_profession_intermediaire<-communes_dates_1994_2022_temperature_final_1994$value_estimated_profession_intermediaire/communes_dates_1994_2022_temperature_final_1994$population_actif_25_54

communes_dates_1994_2022_temperature_final_1994$part_employe<-communes_dates_1994_2022_temperature_final_1994$value_estimated_employe/communes_dates_1994_2022_temperature_final_1994$population_actif_25_54

communes_dates_1994_2022_temperature_final_1994$part_ouvrier<-communes_dates_1994_2022_temperature_final_1994$value_estimated_ouvrier/communes_dates_1994_2022_temperature_final_1994$population_actif_25_54

communes_dates_1994_2022_temperature_final_1994$part_chomage<-communes_dates_1994_2022_temperature_final_1994$value_estimated_au_chomage/communes_dates_1994_2022_temperature_final_1994$population_actif_25_54






communes_dates_1994_2022_temperature_final_1994$taux_mortalite_homme<-communes_dates_1994_2022_temperature_final_1994$Homme/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_homme

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_femme<-communes_dates_1994_2022_temperature_final_1994$Femme/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_femme


communes_dates_1994_2022_temperature_final_1994$taux_mortalite_0_9<-communes_dates_1994_2022_temperature_final_1994$`0-9`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_0_9_h_f

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_10_19<-communes_dates_1994_2022_temperature_final_1994$`10-19`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_10_19_h_f



communes_dates_1994_2022_temperature_final_1994$taux_mortalite_20_39<-communes_dates_1994_2022_temperature_final_1994$`20-39`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_20_39_h_f




communes_dates_1994_2022_temperature_final_1994$taux_mortalite_40_59<-communes_dates_1994_2022_temperature_final_1994$`40-59`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_40_59_h_f





communes_dates_1994_2022_temperature_final_1994$taux_mortalite_60_64<-communes_dates_1994_2022_temperature_final_1994$`60-64`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_60_64_h_f



communes_dates_1994_2022_temperature_final_1994$taux_mortalite_65_69<-communes_dates_1994_2022_temperature_final_1994$`65-69`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_65_69_h_f


communes_dates_1994_2022_temperature_final_1994$taux_mortalite_70_74<-communes_dates_1994_2022_temperature_final_1994$`70-74`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_70_74_h_f


communes_dates_1994_2022_temperature_final_1994$taux_mortalite_75_79<-communes_dates_1994_2022_temperature_final_1994$`75-79`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_75_79_h_f


communes_dates_1994_2022_temperature_final_1994$taux_mortalite_80_plus<-communes_dates_1994_2022_temperature_final_1994$`80+`/communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_80_plus_h_f


#communes_dates_1994_2022_temperature_final_1994$taux_mortalite_60_70<-(communes_dates_1994_2022_temperature_final_1994$`60-64`+communes_dates_1994_2022_temperature_final_1994$`65-69`)/(communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_60_64_h_f+communes_dates_1994_2022_temperature_final_1994$value_estimated_sum_65_69_h_f)



communes_dates_1994_2022_temperature_final_1994$mort_total<-communes_dates_1994_2022_temperature_final_1994$Femme+communes_dates_1994_2022_temperature_final_1994$Homme


communes_dates_1994_2022_temperature_final_1994$taux_mortalite_total<-communes_dates_1994_2022_temperature_final_1994$mort_total/communes_dates_1994_2022_temperature_final_1994$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994, communes_dates_1994_2022_temperature_final_1994$value_estimated_population>0)
communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994, communes_dates_1994_2022_temperature_final_1994$population_actif_25_54>0)




communes_dates_1994_2022_temperature_final_1994<- communes_dates_1994_2022_temperature_final_1994[ , !names(communes_dates_1994_2022_temperature_final_1994) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_femme))


communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(part_agriculteur))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(part_cadre))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(part_profession_intermediaire))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(part_employe))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(part_ouvrier))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(part_chomage))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_homme))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_0_9))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_10_19))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_20_39))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_40_59))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_60_64))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_65_69))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_70_74))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_75_79))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1994_2022_temperature_final_1994<-communes_dates_1994_2022_temperature_final_1994[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  part_agriculteur<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_10_19<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  part_cadre<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  part_profession_intermediaire<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  part_employe<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  part_ouvrier<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  part_chomage<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_homme<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_femme<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_0_9<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_20_39<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_40_59<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_60_64<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_65_69<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_70_74<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_75_79<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_80_plus<=1)

#communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994,  taux_mortalite_total<=1)




communes_dates_1994_2022_temperature_final_1994$taux_mortalite_homme[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_homme>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_femme[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_femme>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_0_9[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_0_9>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_10_19[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_10_19>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_20_39[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_20_39>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_40_59[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_40_59>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_60_64[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_60_64>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_65_69[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_65_69>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_70_74[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_70_74>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_75_79[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_75_79>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_80_plus[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_80_plus>1]<-NA   

communes_dates_1994_2022_temperature_final_1994$taux_mortalite_total[communes_dates_1994_2022_temperature_final_1994$taux_mortalite_total>1]<-NA   





summary(communes_dates_1994_2022_temperature_final_1994$part_agriculteur)

summary(communes_dates_1994_2022_temperature_final_1994$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1994_2022_temperature_final_1994$part_cadre)

summary(communes_dates_1994_2022_temperature_final_1994$part_profession_intermediaire)

summary(communes_dates_1994_2022_temperature_final_1994$part_employe)

summary(communes_dates_1994_2022_temperature_final_1994$part_ouvrier)

summary(communes_dates_1994_2022_temperature_final_1994$part_chomage)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_homme)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_femme)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_0_9)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_10_19)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_20_39)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_40_59)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_60_64)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_65_69)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_70_74)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_75_79)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_80_plus)

summary(communes_dates_1994_2022_temperature_final_1994$taux_mortalite_total)






fwrite(communes_dates_1994_2022_temperature_final_1994,"/données communes années/données mortalité temperature final mois new/communes_dates_1994_temperature_deces_mois.csv")








################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1995_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1995-01-01")
end_date <- as.Date("1995-12-31")

communes_dates_1995_2022_temperature_final_1995 <- communes_dates_1995_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1995_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1995_age_sexe.csv")





communes_dates_1995_2022_temperature_final_1995$date<-as.Date(communes_dates_1995_2022_temperature_final_1995$date)
deces.1995_age_sexe$date<-as.Date(deces.1995_age_sexe$date)






communes_dates_1995_2022_temperature_final_1995<-left_join(communes_dates_1995_2022_temperature_final_1995,deces.1995_age_sexe)

communes_dates_1995_2022_temperature_final_1995[is.na(communes_dates_1995_2022_temperature_final_1995)]<-0


communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 < -20]<-"<-20"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= -20 & communes_dates_1995_2022_temperature_final_1995$value1 < -15]<-"-20_-15"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= -15 & communes_dates_1995_2022_temperature_final_1995$value1 < -10]<-"-15_-10"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= -10 & communes_dates_1995_2022_temperature_final_1995$value1 < -5]<-"-10_-5"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= -5 & communes_dates_1995_2022_temperature_final_1995$value1 < 0]<-"-5_0"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 0 & communes_dates_1995_2022_temperature_final_1995$value1 < 5]<-"0_5"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 5 & communes_dates_1995_2022_temperature_final_1995$value1 < 10]<-"5_10"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 10 & communes_dates_1995_2022_temperature_final_1995$value1 < 15]<-"10_15"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 15 & communes_dates_1995_2022_temperature_final_1995$value1 < 20]<-"15_20"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 20 & communes_dates_1995_2022_temperature_final_1995$value1 < 25]<-"20_25"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 25 & communes_dates_1995_2022_temperature_final_1995$value1 < 28]<-"25_28"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 28 & communes_dates_1995_2022_temperature_final_1995$value1 < 30]<-"28_30"

communes_dates_1995_2022_temperature_final_1995$temperature_bin[communes_dates_1995_2022_temperature_final_1995$value1 >= 30]<-">30"


#test<-filter(communes_dates_1995_2022_temperature_final_1995, is.na(temperature_bin))
#table(communes_dates_1995_2022_temperature_final_1995$temperature_bin)

library(fastDummies)
communes_dates_1995_2022_temperature_final_1995  <- communes_dates_1995_2022_temperature_final_1995  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1995_2022_temperature_final_1995 <- communes_dates_1995_2022_temperature_final_1995 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1995_2022_temperature_final_1995 <- communes_dates_1995_2022_temperature_final_1995 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1995_2022_temperature_final_1995$same_value[is.na(communes_dates_1995_2022_temperature_final_1995$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1995_2022_temperature_final_1995$same_value <- ifelse(communes_dates_1995_2022_temperature_final_1995$temperature_bin != ">30", 0, communes_dates_1995_2022_temperature_final_1995$same_value)



communes_dates_1995_2022_temperature_final_1995$mois<-substring(communes_dates_1995_2022_temperature_final_1995$date,6,7)

communes_dates_1995_2022_temperature_final_1995<-communes_dates_1995_2022_temperature_final_1995[,-c("date","value1","temperature_bin")]

communes_dates_1995_2022_temperature_final_1995<-aggregate(.~COM+mois,communes_dates_1995_2022_temperature_final_1995,sum)



RP_1995_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1995_age_sexe_final_2")

RP_1995_age_sexe_final_2<-RP_1995_age_sexe_final_2[,c(2:15)]

names(RP_1995_age_sexe_final_2)[names(RP_1995_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1995_2022_temperature_final_1995<-left_join(communes_dates_1995_2022_temperature_final_1995,RP_1995_age_sexe_final_2)

#tests<-filter(communes_dates_1995_2022_temperature_final_1995, COM=="01001")
#tests<-filter(communes_dates_1995_2022_temperature_final_1995, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995, !is.na(value_estimated_sum_homme) )

RP_1995_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1995_CSP_final_2")

RP_1995_CSP_final_2<-RP_1995_CSP_final_2[,c(2:12)]
names(RP_1995_CSP_final_2)[names(RP_1995_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1995_CSP_final_2)[names(RP_1995_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1995_2022_temperature_final_1995<-left_join(communes_dates_1995_2022_temperature_final_1995,RP_1995_CSP_final_2)


communes_dates_1995_2022_temperature_final_1995$part_agriculteur<-communes_dates_1995_2022_temperature_final_1995$value_estimated_agriculteur/communes_dates_1995_2022_temperature_final_1995$population_actif_25_54

communes_dates_1995_2022_temperature_final_1995$part_artisan_commercant_chef_entreprise<-communes_dates_1995_2022_temperature_final_1995$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1995_2022_temperature_final_1995$population_actif_25_54

communes_dates_1995_2022_temperature_final_1995$part_cadre<-communes_dates_1995_2022_temperature_final_1995$value_estimated_cadre/communes_dates_1995_2022_temperature_final_1995$population_actif_25_54

communes_dates_1995_2022_temperature_final_1995$part_profession_intermediaire<-communes_dates_1995_2022_temperature_final_1995$value_estimated_profession_intermediaire/communes_dates_1995_2022_temperature_final_1995$population_actif_25_54

communes_dates_1995_2022_temperature_final_1995$part_employe<-communes_dates_1995_2022_temperature_final_1995$value_estimated_employe/communes_dates_1995_2022_temperature_final_1995$population_actif_25_54

communes_dates_1995_2022_temperature_final_1995$part_ouvrier<-communes_dates_1995_2022_temperature_final_1995$value_estimated_ouvrier/communes_dates_1995_2022_temperature_final_1995$population_actif_25_54

communes_dates_1995_2022_temperature_final_1995$part_chomage<-communes_dates_1995_2022_temperature_final_1995$value_estimated_au_chomage/communes_dates_1995_2022_temperature_final_1995$population_actif_25_54






communes_dates_1995_2022_temperature_final_1995$taux_mortalite_homme<-communes_dates_1995_2022_temperature_final_1995$Homme/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_homme

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_femme<-communes_dates_1995_2022_temperature_final_1995$Femme/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_femme


communes_dates_1995_2022_temperature_final_1995$taux_mortalite_0_9<-communes_dates_1995_2022_temperature_final_1995$`0-9`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_0_9_h_f

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_10_19<-communes_dates_1995_2022_temperature_final_1995$`10-19`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_10_19_h_f



communes_dates_1995_2022_temperature_final_1995$taux_mortalite_20_39<-communes_dates_1995_2022_temperature_final_1995$`20-39`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_20_39_h_f




communes_dates_1995_2022_temperature_final_1995$taux_mortalite_40_59<-communes_dates_1995_2022_temperature_final_1995$`40-59`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_40_59_h_f





communes_dates_1995_2022_temperature_final_1995$taux_mortalite_60_64<-communes_dates_1995_2022_temperature_final_1995$`60-64`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_60_64_h_f



communes_dates_1995_2022_temperature_final_1995$taux_mortalite_65_69<-communes_dates_1995_2022_temperature_final_1995$`65-69`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_65_69_h_f


communes_dates_1995_2022_temperature_final_1995$taux_mortalite_70_74<-communes_dates_1995_2022_temperature_final_1995$`70-74`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_70_74_h_f


communes_dates_1995_2022_temperature_final_1995$taux_mortalite_75_79<-communes_dates_1995_2022_temperature_final_1995$`75-79`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_75_79_h_f


communes_dates_1995_2022_temperature_final_1995$taux_mortalite_80_plus<-communes_dates_1995_2022_temperature_final_1995$`80+`/communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_80_plus_h_f


#communes_dates_1995_2022_temperature_final_1995$taux_mortalite_60_70<-(communes_dates_1995_2022_temperature_final_1995$`60-64`+communes_dates_1995_2022_temperature_final_1995$`65-69`)/(communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_60_64_h_f+communes_dates_1995_2022_temperature_final_1995$value_estimated_sum_65_69_h_f)



communes_dates_1995_2022_temperature_final_1995$mort_total<-communes_dates_1995_2022_temperature_final_1995$Femme+communes_dates_1995_2022_temperature_final_1995$Homme


communes_dates_1995_2022_temperature_final_1995$taux_mortalite_total<-communes_dates_1995_2022_temperature_final_1995$mort_total/communes_dates_1995_2022_temperature_final_1995$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995, communes_dates_1995_2022_temperature_final_1995$value_estimated_population>0)
communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995, communes_dates_1995_2022_temperature_final_1995$population_actif_25_54>0)




communes_dates_1995_2022_temperature_final_1995<- communes_dates_1995_2022_temperature_final_1995[ , !names(communes_dates_1995_2022_temperature_final_1995) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_femme))


communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(part_agriculteur))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(part_cadre))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(part_profession_intermediaire))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(part_employe))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(part_ouvrier))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(part_chomage))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_homme))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_0_9))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_10_19))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_20_39))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_40_59))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_60_64))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_65_69))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_70_74))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_75_79))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1995_2022_temperature_final_1995<-communes_dates_1995_2022_temperature_final_1995[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  part_agriculteur<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_10_19<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  part_cadre<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  part_profession_intermediaire<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  part_employe<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  part_ouvrier<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  part_chomage<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_homme<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_femme<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_0_9<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_20_39<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_40_59<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_60_64<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_65_69<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_70_74<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_75_79<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_80_plus<=1)

#communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995,  taux_mortalite_total<=1)




communes_dates_1995_2022_temperature_final_1995$taux_mortalite_homme[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_homme>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_femme[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_femme>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_0_9[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_0_9>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_10_19[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_10_19>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_20_39[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_20_39>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_40_59[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_40_59>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_60_64[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_60_64>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_65_69[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_65_69>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_70_74[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_70_74>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_75_79[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_75_79>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_80_plus[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_80_plus>1]<-NA   

communes_dates_1995_2022_temperature_final_1995$taux_mortalite_total[communes_dates_1995_2022_temperature_final_1995$taux_mortalite_total>1]<-NA   





summary(communes_dates_1995_2022_temperature_final_1995$part_agriculteur)

summary(communes_dates_1995_2022_temperature_final_1995$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1995_2022_temperature_final_1995$part_cadre)

summary(communes_dates_1995_2022_temperature_final_1995$part_profession_intermediaire)

summary(communes_dates_1995_2022_temperature_final_1995$part_employe)

summary(communes_dates_1995_2022_temperature_final_1995$part_ouvrier)

summary(communes_dates_1995_2022_temperature_final_1995$part_chomage)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_homme)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_femme)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_0_9)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_10_19)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_20_39)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_40_59)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_60_64)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_65_69)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_70_74)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_75_79)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_80_plus)

summary(communes_dates_1995_2022_temperature_final_1995$taux_mortalite_total)






fwrite(communes_dates_1995_2022_temperature_final_1995,"/données communes années/données mortalité temperature final mois new/communes_dates_1995_temperature_deces_mois.csv")








################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1996_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1996-01-01")
end_date <- as.Date("1996-12-31")

communes_dates_1996_2022_temperature_final_1996 <- communes_dates_1996_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1996_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1996_age_sexe.csv")





communes_dates_1996_2022_temperature_final_1996$date<-as.Date(communes_dates_1996_2022_temperature_final_1996$date)
deces.1996_age_sexe$date<-as.Date(deces.1996_age_sexe$date)






communes_dates_1996_2022_temperature_final_1996<-left_join(communes_dates_1996_2022_temperature_final_1996,deces.1996_age_sexe)

communes_dates_1996_2022_temperature_final_1996[is.na(communes_dates_1996_2022_temperature_final_1996)]<-0


communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 < -20]<-"<-20"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= -20 & communes_dates_1996_2022_temperature_final_1996$value1 < -15]<-"-20_-15"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= -15 & communes_dates_1996_2022_temperature_final_1996$value1 < -10]<-"-15_-10"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= -10 & communes_dates_1996_2022_temperature_final_1996$value1 < -5]<-"-10_-5"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= -5 & communes_dates_1996_2022_temperature_final_1996$value1 < 0]<-"-5_0"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 0 & communes_dates_1996_2022_temperature_final_1996$value1 < 5]<-"0_5"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 5 & communes_dates_1996_2022_temperature_final_1996$value1 < 10]<-"5_10"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 10 & communes_dates_1996_2022_temperature_final_1996$value1 < 15]<-"10_15"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 15 & communes_dates_1996_2022_temperature_final_1996$value1 < 20]<-"15_20"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 20 & communes_dates_1996_2022_temperature_final_1996$value1 < 25]<-"20_25"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 25 & communes_dates_1996_2022_temperature_final_1996$value1 < 28]<-"25_28"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 28 & communes_dates_1996_2022_temperature_final_1996$value1 < 30]<-"28_30"

communes_dates_1996_2022_temperature_final_1996$temperature_bin[communes_dates_1996_2022_temperature_final_1996$value1 >= 30]<-">30"


#test<-filter(communes_dates_1996_2022_temperature_final_1996, is.na(temperature_bin))
#table(communes_dates_1996_2022_temperature_final_1996$temperature_bin)

library(fastDummies)
communes_dates_1996_2022_temperature_final_1996  <- communes_dates_1996_2022_temperature_final_1996  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1996_2022_temperature_final_1996 <- communes_dates_1996_2022_temperature_final_1996 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1996_2022_temperature_final_1996 <- communes_dates_1996_2022_temperature_final_1996 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1996_2022_temperature_final_1996$same_value[is.na(communes_dates_1996_2022_temperature_final_1996$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1996_2022_temperature_final_1996$same_value <- ifelse(communes_dates_1996_2022_temperature_final_1996$temperature_bin != ">30", 0, communes_dates_1996_2022_temperature_final_1996$same_value)



communes_dates_1996_2022_temperature_final_1996$mois<-substring(communes_dates_1996_2022_temperature_final_1996$date,6,7)

communes_dates_1996_2022_temperature_final_1996<-communes_dates_1996_2022_temperature_final_1996[,-c("date","value1","temperature_bin")]

communes_dates_1996_2022_temperature_final_1996<-aggregate(.~COM+mois,communes_dates_1996_2022_temperature_final_1996,sum)



RP_1996_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1996_age_sexe_final_2")

RP_1996_age_sexe_final_2<-RP_1996_age_sexe_final_2[,c(2:15)]

names(RP_1996_age_sexe_final_2)[names(RP_1996_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1996_2022_temperature_final_1996<-left_join(communes_dates_1996_2022_temperature_final_1996,RP_1996_age_sexe_final_2)

#tests<-filter(communes_dates_1996_2022_temperature_final_1996, COM=="01001")
#tests<-filter(communes_dates_1996_2022_temperature_final_1996, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996, !is.na(value_estimated_sum_homme) )

RP_1996_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1996_CSP_final_2")

RP_1996_CSP_final_2<-RP_1996_CSP_final_2[,c(2:12)]
names(RP_1996_CSP_final_2)[names(RP_1996_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1996_CSP_final_2)[names(RP_1996_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1996_2022_temperature_final_1996<-left_join(communes_dates_1996_2022_temperature_final_1996,RP_1996_CSP_final_2)


communes_dates_1996_2022_temperature_final_1996$part_agriculteur<-communes_dates_1996_2022_temperature_final_1996$value_estimated_agriculteur/communes_dates_1996_2022_temperature_final_1996$population_actif_25_54

communes_dates_1996_2022_temperature_final_1996$part_artisan_commercant_chef_entreprise<-communes_dates_1996_2022_temperature_final_1996$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1996_2022_temperature_final_1996$population_actif_25_54

communes_dates_1996_2022_temperature_final_1996$part_cadre<-communes_dates_1996_2022_temperature_final_1996$value_estimated_cadre/communes_dates_1996_2022_temperature_final_1996$population_actif_25_54

communes_dates_1996_2022_temperature_final_1996$part_profession_intermediaire<-communes_dates_1996_2022_temperature_final_1996$value_estimated_profession_intermediaire/communes_dates_1996_2022_temperature_final_1996$population_actif_25_54

communes_dates_1996_2022_temperature_final_1996$part_employe<-communes_dates_1996_2022_temperature_final_1996$value_estimated_employe/communes_dates_1996_2022_temperature_final_1996$population_actif_25_54

communes_dates_1996_2022_temperature_final_1996$part_ouvrier<-communes_dates_1996_2022_temperature_final_1996$value_estimated_ouvrier/communes_dates_1996_2022_temperature_final_1996$population_actif_25_54

communes_dates_1996_2022_temperature_final_1996$part_chomage<-communes_dates_1996_2022_temperature_final_1996$value_estimated_au_chomage/communes_dates_1996_2022_temperature_final_1996$population_actif_25_54






communes_dates_1996_2022_temperature_final_1996$taux_mortalite_homme<-communes_dates_1996_2022_temperature_final_1996$Homme/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_homme

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_femme<-communes_dates_1996_2022_temperature_final_1996$Femme/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_femme


communes_dates_1996_2022_temperature_final_1996$taux_mortalite_0_9<-communes_dates_1996_2022_temperature_final_1996$`0-9`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_0_9_h_f

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_10_19<-communes_dates_1996_2022_temperature_final_1996$`10-19`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_10_19_h_f



communes_dates_1996_2022_temperature_final_1996$taux_mortalite_20_39<-communes_dates_1996_2022_temperature_final_1996$`20-39`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_20_39_h_f




communes_dates_1996_2022_temperature_final_1996$taux_mortalite_40_59<-communes_dates_1996_2022_temperature_final_1996$`40-59`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_40_59_h_f





communes_dates_1996_2022_temperature_final_1996$taux_mortalite_60_64<-communes_dates_1996_2022_temperature_final_1996$`60-64`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_60_64_h_f



communes_dates_1996_2022_temperature_final_1996$taux_mortalite_65_69<-communes_dates_1996_2022_temperature_final_1996$`65-69`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_65_69_h_f


communes_dates_1996_2022_temperature_final_1996$taux_mortalite_70_74<-communes_dates_1996_2022_temperature_final_1996$`70-74`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_70_74_h_f


communes_dates_1996_2022_temperature_final_1996$taux_mortalite_75_79<-communes_dates_1996_2022_temperature_final_1996$`75-79`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_75_79_h_f


communes_dates_1996_2022_temperature_final_1996$taux_mortalite_80_plus<-communes_dates_1996_2022_temperature_final_1996$`80+`/communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_80_plus_h_f


#communes_dates_1996_2022_temperature_final_1996$taux_mortalite_60_70<-(communes_dates_1996_2022_temperature_final_1996$`60-64`+communes_dates_1996_2022_temperature_final_1996$`65-69`)/(communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_60_64_h_f+communes_dates_1996_2022_temperature_final_1996$value_estimated_sum_65_69_h_f)



communes_dates_1996_2022_temperature_final_1996$mort_total<-communes_dates_1996_2022_temperature_final_1996$Femme+communes_dates_1996_2022_temperature_final_1996$Homme


communes_dates_1996_2022_temperature_final_1996$taux_mortalite_total<-communes_dates_1996_2022_temperature_final_1996$mort_total/communes_dates_1996_2022_temperature_final_1996$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996, communes_dates_1996_2022_temperature_final_1996$value_estimated_population>0)
communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996, communes_dates_1996_2022_temperature_final_1996$population_actif_25_54>0)




communes_dates_1996_2022_temperature_final_1996<- communes_dates_1996_2022_temperature_final_1996[ , !names(communes_dates_1996_2022_temperature_final_1996) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_femme))


communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(part_agriculteur))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(part_cadre))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(part_profession_intermediaire))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(part_employe))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(part_ouvrier))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(part_chomage))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_homme))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_0_9))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_10_19))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_20_39))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_40_59))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_60_64))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_65_69))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_70_74))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_75_79))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1996_2022_temperature_final_1996<-communes_dates_1996_2022_temperature_final_1996[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  part_agriculteur<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_10_19<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  part_cadre<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  part_profession_intermediaire<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  part_employe<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  part_ouvrier<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  part_chomage<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_homme<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_femme<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_0_9<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_20_39<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_40_59<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_60_64<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_65_69<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_70_74<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_75_79<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_80_plus<=1)

#communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996,  taux_mortalite_total<=1)




communes_dates_1996_2022_temperature_final_1996$taux_mortalite_homme[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_homme>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_femme[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_femme>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_0_9[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_0_9>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_10_19[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_10_19>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_20_39[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_20_39>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_40_59[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_40_59>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_60_64[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_60_64>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_65_69[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_65_69>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_70_74[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_70_74>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_75_79[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_75_79>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_80_plus[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_80_plus>1]<-NA   

communes_dates_1996_2022_temperature_final_1996$taux_mortalite_total[communes_dates_1996_2022_temperature_final_1996$taux_mortalite_total>1]<-NA   





summary(communes_dates_1996_2022_temperature_final_1996$part_agriculteur)

summary(communes_dates_1996_2022_temperature_final_1996$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1996_2022_temperature_final_1996$part_cadre)

summary(communes_dates_1996_2022_temperature_final_1996$part_profession_intermediaire)

summary(communes_dates_1996_2022_temperature_final_1996$part_employe)

summary(communes_dates_1996_2022_temperature_final_1996$part_ouvrier)

summary(communes_dates_1996_2022_temperature_final_1996$part_chomage)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_homme)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_femme)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_0_9)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_10_19)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_20_39)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_40_59)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_60_64)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_65_69)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_70_74)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_75_79)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_80_plus)

summary(communes_dates_1996_2022_temperature_final_1996$taux_mortalite_total)






fwrite(communes_dates_1996_2022_temperature_final_1996,"/données communes années/données mortalité temperature final mois new/communes_dates_1996_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1997_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1997-01-01")
end_date <- as.Date("1997-12-31")

communes_dates_1997_2022_temperature_final_1997 <- communes_dates_1997_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1997_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1997_age_sexe.csv")



communes_dates_1997_2022_temperature_final_1997$date<-as.Date(communes_dates_1997_2022_temperature_final_1997$date)
deces.1997_age_sexe$date<-as.Date(deces.1997_age_sexe$date)





communes_dates_1997_2022_temperature_final_1997<-left_join(communes_dates_1997_2022_temperature_final_1997,deces.1997_age_sexe)

communes_dates_1997_2022_temperature_final_1997[is.na(communes_dates_1997_2022_temperature_final_1997)]<-0


communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 < -20]<-"<-20"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= -20 & communes_dates_1997_2022_temperature_final_1997$value1 < -15]<-"-20_-15"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= -15 & communes_dates_1997_2022_temperature_final_1997$value1 < -10]<-"-15_-10"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= -10 & communes_dates_1997_2022_temperature_final_1997$value1 < -5]<-"-10_-5"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= -5 & communes_dates_1997_2022_temperature_final_1997$value1 < 0]<-"-5_0"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 0 & communes_dates_1997_2022_temperature_final_1997$value1 < 5]<-"0_5"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 5 & communes_dates_1997_2022_temperature_final_1997$value1 < 10]<-"5_10"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 10 & communes_dates_1997_2022_temperature_final_1997$value1 < 15]<-"10_15"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 15 & communes_dates_1997_2022_temperature_final_1997$value1 < 20]<-"15_20"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 20 & communes_dates_1997_2022_temperature_final_1997$value1 < 25]<-"20_25"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 25 & communes_dates_1997_2022_temperature_final_1997$value1 < 28]<-"25_28"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 28 & communes_dates_1997_2022_temperature_final_1997$value1 < 30]<-"28_30"

communes_dates_1997_2022_temperature_final_1997$temperature_bin[communes_dates_1997_2022_temperature_final_1997$value1 >= 30]<-">30"


#test<-filter(communes_dates_1997_2022_temperature_final_1997, is.na(temperature_bin))
#table(communes_dates_1997_2022_temperature_final_1997$temperature_bin)

library(fastDummies)
communes_dates_1997_2022_temperature_final_1997  <- communes_dates_1997_2022_temperature_final_1997  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1997_2022_temperature_final_1997 <- communes_dates_1997_2022_temperature_final_1997 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1997_2022_temperature_final_1997 <- communes_dates_1997_2022_temperature_final_1997 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1997_2022_temperature_final_1997$same_value[is.na(communes_dates_1997_2022_temperature_final_1997$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1997_2022_temperature_final_1997$same_value <- ifelse(communes_dates_1997_2022_temperature_final_1997$temperature_bin != ">30", 0, communes_dates_1997_2022_temperature_final_1997$same_value)



communes_dates_1997_2022_temperature_final_1997$mois<-substring(communes_dates_1997_2022_temperature_final_1997$date,6,7)

communes_dates_1997_2022_temperature_final_1997<-communes_dates_1997_2022_temperature_final_1997[,-c("date","value1","temperature_bin")]

communes_dates_1997_2022_temperature_final_1997<-aggregate(.~COM+mois,communes_dates_1997_2022_temperature_final_1997,sum)



RP_1997_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1997_age_sexe_final_2")

RP_1997_age_sexe_final_2<-RP_1997_age_sexe_final_2[,c(2:15)]

names(RP_1997_age_sexe_final_2)[names(RP_1997_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1997_2022_temperature_final_1997<-left_join(communes_dates_1997_2022_temperature_final_1997,RP_1997_age_sexe_final_2)

#tests<-filter(communes_dates_1997_2022_temperature_final_1997, COM=="01001")
#tests<-filter(communes_dates_1997_2022_temperature_final_1997, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997, !is.na(value_estimated_sum_homme) )

RP_1997_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1997_CSP_final_2")

RP_1997_CSP_final_2<-RP_1997_CSP_final_2[,c(2:12)]
names(RP_1997_CSP_final_2)[names(RP_1997_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1997_CSP_final_2)[names(RP_1997_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1997_2022_temperature_final_1997<-left_join(communes_dates_1997_2022_temperature_final_1997,RP_1997_CSP_final_2)


communes_dates_1997_2022_temperature_final_1997$part_agriculteur<-communes_dates_1997_2022_temperature_final_1997$value_estimated_agriculteur/communes_dates_1997_2022_temperature_final_1997$population_actif_25_54

communes_dates_1997_2022_temperature_final_1997$part_artisan_commercant_chef_entreprise<-communes_dates_1997_2022_temperature_final_1997$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1997_2022_temperature_final_1997$population_actif_25_54

communes_dates_1997_2022_temperature_final_1997$part_cadre<-communes_dates_1997_2022_temperature_final_1997$value_estimated_cadre/communes_dates_1997_2022_temperature_final_1997$population_actif_25_54

communes_dates_1997_2022_temperature_final_1997$part_profession_intermediaire<-communes_dates_1997_2022_temperature_final_1997$value_estimated_profession_intermediaire/communes_dates_1997_2022_temperature_final_1997$population_actif_25_54

communes_dates_1997_2022_temperature_final_1997$part_employe<-communes_dates_1997_2022_temperature_final_1997$value_estimated_employe/communes_dates_1997_2022_temperature_final_1997$population_actif_25_54

communes_dates_1997_2022_temperature_final_1997$part_ouvrier<-communes_dates_1997_2022_temperature_final_1997$value_estimated_ouvrier/communes_dates_1997_2022_temperature_final_1997$population_actif_25_54

communes_dates_1997_2022_temperature_final_1997$part_chomage<-communes_dates_1997_2022_temperature_final_1997$value_estimated_au_chomage/communes_dates_1997_2022_temperature_final_1997$population_actif_25_54






communes_dates_1997_2022_temperature_final_1997$taux_mortalite_homme<-communes_dates_1997_2022_temperature_final_1997$Homme/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_homme

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_femme<-communes_dates_1997_2022_temperature_final_1997$Femme/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_femme


communes_dates_1997_2022_temperature_final_1997$taux_mortalite_0_9<-communes_dates_1997_2022_temperature_final_1997$`0-9`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_0_9_h_f

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_10_19<-communes_dates_1997_2022_temperature_final_1997$`10-19`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_10_19_h_f



communes_dates_1997_2022_temperature_final_1997$taux_mortalite_20_39<-communes_dates_1997_2022_temperature_final_1997$`20-39`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_20_39_h_f




communes_dates_1997_2022_temperature_final_1997$taux_mortalite_40_59<-communes_dates_1997_2022_temperature_final_1997$`40-59`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_40_59_h_f





communes_dates_1997_2022_temperature_final_1997$taux_mortalite_60_64<-communes_dates_1997_2022_temperature_final_1997$`60-64`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_60_64_h_f



communes_dates_1997_2022_temperature_final_1997$taux_mortalite_65_69<-communes_dates_1997_2022_temperature_final_1997$`65-69`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_65_69_h_f


communes_dates_1997_2022_temperature_final_1997$taux_mortalite_70_74<-communes_dates_1997_2022_temperature_final_1997$`70-74`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_70_74_h_f


communes_dates_1997_2022_temperature_final_1997$taux_mortalite_75_79<-communes_dates_1997_2022_temperature_final_1997$`75-79`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_75_79_h_f


communes_dates_1997_2022_temperature_final_1997$taux_mortalite_80_plus<-communes_dates_1997_2022_temperature_final_1997$`80+`/communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_80_plus_h_f


#communes_dates_1997_2022_temperature_final_1997$taux_mortalite_60_70<-(communes_dates_1997_2022_temperature_final_1997$`60-64`+communes_dates_1997_2022_temperature_final_1997$`65-69`)/(communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_60_64_h_f+communes_dates_1997_2022_temperature_final_1997$value_estimated_sum_65_69_h_f)



communes_dates_1997_2022_temperature_final_1997$mort_total<-communes_dates_1997_2022_temperature_final_1997$Femme+communes_dates_1997_2022_temperature_final_1997$Homme


communes_dates_1997_2022_temperature_final_1997$taux_mortalite_total<-communes_dates_1997_2022_temperature_final_1997$mort_total/communes_dates_1997_2022_temperature_final_1997$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997, communes_dates_1997_2022_temperature_final_1997$value_estimated_population>0)
communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997, communes_dates_1997_2022_temperature_final_1997$population_actif_25_54>0)




communes_dates_1997_2022_temperature_final_1997<- communes_dates_1997_2022_temperature_final_1997[ , !names(communes_dates_1997_2022_temperature_final_1997) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_femme))


communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(part_agriculteur))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(part_cadre))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(part_profession_intermediaire))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(part_employe))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(part_ouvrier))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(part_chomage))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_homme))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_0_9))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_10_19))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_20_39))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_40_59))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_60_64))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_65_69))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_70_74))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_75_79))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1997_2022_temperature_final_1997<-communes_dates_1997_2022_temperature_final_1997[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  part_agriculteur<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_10_19<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  part_cadre<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  part_profession_intermediaire<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  part_employe<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  part_ouvrier<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  part_chomage<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_homme<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_femme<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_0_9<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_20_39<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_40_59<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_60_64<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_65_69<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_70_74<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_75_79<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_80_plus<=1)

#communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997,  taux_mortalite_total<=1)




communes_dates_1997_2022_temperature_final_1997$taux_mortalite_homme[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_homme>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_femme[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_femme>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_0_9[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_0_9>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_10_19[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_10_19>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_20_39[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_20_39>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_40_59[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_40_59>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_60_64[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_60_64>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_65_69[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_65_69>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_70_74[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_70_74>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_75_79[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_75_79>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_80_plus[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_80_plus>1]<-NA   

communes_dates_1997_2022_temperature_final_1997$taux_mortalite_total[communes_dates_1997_2022_temperature_final_1997$taux_mortalite_total>1]<-NA   





summary(communes_dates_1997_2022_temperature_final_1997$part_agriculteur)

summary(communes_dates_1997_2022_temperature_final_1997$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1997_2022_temperature_final_1997$part_cadre)

summary(communes_dates_1997_2022_temperature_final_1997$part_profession_intermediaire)

summary(communes_dates_1997_2022_temperature_final_1997$part_employe)

summary(communes_dates_1997_2022_temperature_final_1997$part_ouvrier)

summary(communes_dates_1997_2022_temperature_final_1997$part_chomage)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_homme)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_femme)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_0_9)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_10_19)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_20_39)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_40_59)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_60_64)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_65_69)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_70_74)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_75_79)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_80_plus)

summary(communes_dates_1997_2022_temperature_final_1997$taux_mortalite_total)






fwrite(communes_dates_1997_2022_temperature_final_1997,"/données communes années/données mortalité temperature final mois new/communes_dates_1997_temperature_deces_mois.csv")







################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1998_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1998-01-01")
end_date <- as.Date("1998-12-31")

communes_dates_1998_2022_temperature_final_1998 <- communes_dates_1998_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1998_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1998_age_sexe.csv")




communes_dates_1998_2022_temperature_final_1998$date<-as.Date(communes_dates_1998_2022_temperature_final_1998$date)
deces.1998_age_sexe$date<-as.Date(deces.1998_age_sexe$date)






communes_dates_1998_2022_temperature_final_1998<-left_join(communes_dates_1998_2022_temperature_final_1998,deces.1998_age_sexe)

communes_dates_1998_2022_temperature_final_1998[is.na(communes_dates_1998_2022_temperature_final_1998)]<-0


communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 < -20]<-"<-20"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= -20 & communes_dates_1998_2022_temperature_final_1998$value1 < -15]<-"-20_-15"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= -15 & communes_dates_1998_2022_temperature_final_1998$value1 < -10]<-"-15_-10"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= -10 & communes_dates_1998_2022_temperature_final_1998$value1 < -5]<-"-10_-5"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= -5 & communes_dates_1998_2022_temperature_final_1998$value1 < 0]<-"-5_0"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 0 & communes_dates_1998_2022_temperature_final_1998$value1 < 5]<-"0_5"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 5 & communes_dates_1998_2022_temperature_final_1998$value1 < 10]<-"5_10"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 10 & communes_dates_1998_2022_temperature_final_1998$value1 < 15]<-"10_15"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 15 & communes_dates_1998_2022_temperature_final_1998$value1 < 20]<-"15_20"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 20 & communes_dates_1998_2022_temperature_final_1998$value1 < 25]<-"20_25"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 25 & communes_dates_1998_2022_temperature_final_1998$value1 < 28]<-"25_28"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 28 & communes_dates_1998_2022_temperature_final_1998$value1 < 30]<-"28_30"

communes_dates_1998_2022_temperature_final_1998$temperature_bin[communes_dates_1998_2022_temperature_final_1998$value1 >= 30]<-">30"


#test<-filter(communes_dates_1998_2022_temperature_final_1998, is.na(temperature_bin))
#table(communes_dates_1998_2022_temperature_final_1998$temperature_bin)

library(fastDummies)
communes_dates_1998_2022_temperature_final_1998  <- communes_dates_1998_2022_temperature_final_1998  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1998_2022_temperature_final_1998 <- communes_dates_1998_2022_temperature_final_1998 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1998_2022_temperature_final_1998 <- communes_dates_1998_2022_temperature_final_1998 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1998_2022_temperature_final_1998$same_value[is.na(communes_dates_1998_2022_temperature_final_1998$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1998_2022_temperature_final_1998$same_value <- ifelse(communes_dates_1998_2022_temperature_final_1998$temperature_bin != ">30", 0, communes_dates_1998_2022_temperature_final_1998$same_value)



communes_dates_1998_2022_temperature_final_1998$mois<-substring(communes_dates_1998_2022_temperature_final_1998$date,6,7)

communes_dates_1998_2022_temperature_final_1998<-communes_dates_1998_2022_temperature_final_1998[,-c("date","value1","temperature_bin")]

communes_dates_1998_2022_temperature_final_1998<-aggregate(.~COM+mois,communes_dates_1998_2022_temperature_final_1998,sum)



RP_1998_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1998_age_sexe_final_2")

RP_1998_age_sexe_final_2<-RP_1998_age_sexe_final_2[,c(2:15)]

names(RP_1998_age_sexe_final_2)[names(RP_1998_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1998_2022_temperature_final_1998<-left_join(communes_dates_1998_2022_temperature_final_1998,RP_1998_age_sexe_final_2)

#tests<-filter(communes_dates_1998_2022_temperature_final_1998, COM=="01001")
#tests<-filter(communes_dates_1998_2022_temperature_final_1998, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998, !is.na(value_estimated_sum_homme) )

RP_1998_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1998_CSP_final_2")

RP_1998_CSP_final_2<-RP_1998_CSP_final_2[,c(2:12)]
names(RP_1998_CSP_final_2)[names(RP_1998_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1998_CSP_final_2)[names(RP_1998_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1998_2022_temperature_final_1998<-left_join(communes_dates_1998_2022_temperature_final_1998,RP_1998_CSP_final_2)


communes_dates_1998_2022_temperature_final_1998$part_agriculteur<-communes_dates_1998_2022_temperature_final_1998$value_estimated_agriculteur/communes_dates_1998_2022_temperature_final_1998$population_actif_25_54

communes_dates_1998_2022_temperature_final_1998$part_artisan_commercant_chef_entreprise<-communes_dates_1998_2022_temperature_final_1998$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1998_2022_temperature_final_1998$population_actif_25_54

communes_dates_1998_2022_temperature_final_1998$part_cadre<-communes_dates_1998_2022_temperature_final_1998$value_estimated_cadre/communes_dates_1998_2022_temperature_final_1998$population_actif_25_54

communes_dates_1998_2022_temperature_final_1998$part_profession_intermediaire<-communes_dates_1998_2022_temperature_final_1998$value_estimated_profession_intermediaire/communes_dates_1998_2022_temperature_final_1998$population_actif_25_54

communes_dates_1998_2022_temperature_final_1998$part_employe<-communes_dates_1998_2022_temperature_final_1998$value_estimated_employe/communes_dates_1998_2022_temperature_final_1998$population_actif_25_54

communes_dates_1998_2022_temperature_final_1998$part_ouvrier<-communes_dates_1998_2022_temperature_final_1998$value_estimated_ouvrier/communes_dates_1998_2022_temperature_final_1998$population_actif_25_54

communes_dates_1998_2022_temperature_final_1998$part_chomage<-communes_dates_1998_2022_temperature_final_1998$value_estimated_au_chomage/communes_dates_1998_2022_temperature_final_1998$population_actif_25_54






communes_dates_1998_2022_temperature_final_1998$taux_mortalite_homme<-communes_dates_1998_2022_temperature_final_1998$Homme/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_homme

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_femme<-communes_dates_1998_2022_temperature_final_1998$Femme/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_femme


communes_dates_1998_2022_temperature_final_1998$taux_mortalite_0_9<-communes_dates_1998_2022_temperature_final_1998$`0-9`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_0_9_h_f

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_10_19<-communes_dates_1998_2022_temperature_final_1998$`10-19`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_10_19_h_f



communes_dates_1998_2022_temperature_final_1998$taux_mortalite_20_39<-communes_dates_1998_2022_temperature_final_1998$`20-39`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_20_39_h_f




communes_dates_1998_2022_temperature_final_1998$taux_mortalite_40_59<-communes_dates_1998_2022_temperature_final_1998$`40-59`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_40_59_h_f





communes_dates_1998_2022_temperature_final_1998$taux_mortalite_60_64<-communes_dates_1998_2022_temperature_final_1998$`60-64`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_60_64_h_f



communes_dates_1998_2022_temperature_final_1998$taux_mortalite_65_69<-communes_dates_1998_2022_temperature_final_1998$`65-69`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_65_69_h_f


communes_dates_1998_2022_temperature_final_1998$taux_mortalite_70_74<-communes_dates_1998_2022_temperature_final_1998$`70-74`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_70_74_h_f


communes_dates_1998_2022_temperature_final_1998$taux_mortalite_75_79<-communes_dates_1998_2022_temperature_final_1998$`75-79`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_75_79_h_f


communes_dates_1998_2022_temperature_final_1998$taux_mortalite_80_plus<-communes_dates_1998_2022_temperature_final_1998$`80+`/communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_80_plus_h_f


#communes_dates_1998_2022_temperature_final_1998$taux_mortalite_60_70<-(communes_dates_1998_2022_temperature_final_1998$`60-64`+communes_dates_1998_2022_temperature_final_1998$`65-69`)/(communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_60_64_h_f+communes_dates_1998_2022_temperature_final_1998$value_estimated_sum_65_69_h_f)



communes_dates_1998_2022_temperature_final_1998$mort_total<-communes_dates_1998_2022_temperature_final_1998$Femme+communes_dates_1998_2022_temperature_final_1998$Homme


communes_dates_1998_2022_temperature_final_1998$taux_mortalite_total<-communes_dates_1998_2022_temperature_final_1998$mort_total/communes_dates_1998_2022_temperature_final_1998$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998, communes_dates_1998_2022_temperature_final_1998$value_estimated_population>0)
communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998, communes_dates_1998_2022_temperature_final_1998$population_actif_25_54>0)




communes_dates_1998_2022_temperature_final_1998<- communes_dates_1998_2022_temperature_final_1998[ , !names(communes_dates_1998_2022_temperature_final_1998) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_femme))


communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(part_agriculteur))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(part_cadre))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(part_profession_intermediaire))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(part_employe))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(part_ouvrier))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(part_chomage))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_homme))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_0_9))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_10_19))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_20_39))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_40_59))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_60_64))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_65_69))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_70_74))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_75_79))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1998_2022_temperature_final_1998<-communes_dates_1998_2022_temperature_final_1998[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  part_agriculteur<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_10_19<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  part_cadre<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  part_profession_intermediaire<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  part_employe<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  part_ouvrier<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  part_chomage<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_homme<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_femme<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_0_9<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_20_39<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_40_59<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_60_64<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_65_69<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_70_74<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_75_79<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_80_plus<=1)

#communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998,  taux_mortalite_total<=1)




communes_dates_1998_2022_temperature_final_1998$taux_mortalite_homme[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_homme>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_femme[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_femme>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_0_9[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_0_9>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_10_19[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_10_19>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_20_39[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_20_39>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_40_59[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_40_59>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_60_64[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_60_64>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_65_69[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_65_69>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_70_74[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_70_74>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_75_79[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_75_79>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_80_plus[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_80_plus>1]<-NA   

communes_dates_1998_2022_temperature_final_1998$taux_mortalite_total[communes_dates_1998_2022_temperature_final_1998$taux_mortalite_total>1]<-NA   





summary(communes_dates_1998_2022_temperature_final_1998$part_agriculteur)

summary(communes_dates_1998_2022_temperature_final_1998$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1998_2022_temperature_final_1998$part_cadre)

summary(communes_dates_1998_2022_temperature_final_1998$part_profession_intermediaire)

summary(communes_dates_1998_2022_temperature_final_1998$part_employe)

summary(communes_dates_1998_2022_temperature_final_1998$part_ouvrier)

summary(communes_dates_1998_2022_temperature_final_1998$part_chomage)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_homme)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_femme)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_0_9)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_10_19)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_20_39)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_40_59)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_60_64)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_65_69)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_70_74)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_75_79)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_80_plus)

summary(communes_dates_1998_2022_temperature_final_1998$taux_mortalite_total)






fwrite(communes_dates_1998_2022_temperature_final_1998,"/données communes années/données mortalité temperature final mois new/communes_dates_1998_temperature_deces_mois.csv")








################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_1999_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1999-01-01")
end_date <- as.Date("1999-12-31")

communes_dates_1999_2022_temperature_final_1999 <- communes_dates_1999_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1999_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1999_age_sexe.csv")




communes_dates_1999_2022_temperature_final_1999$date<-as.Date(communes_dates_1999_2022_temperature_final_1999$date)
deces.1999_age_sexe$date<-as.Date(deces.1999_age_sexe$date)







communes_dates_1999_2022_temperature_final_1999<-left_join(communes_dates_1999_2022_temperature_final_1999,deces.1999_age_sexe)

communes_dates_1999_2022_temperature_final_1999[is.na(communes_dates_1999_2022_temperature_final_1999)]<-0


communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 < -20]<-"<-20"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= -20 & communes_dates_1999_2022_temperature_final_1999$value1 < -15]<-"-20_-15"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= -15 & communes_dates_1999_2022_temperature_final_1999$value1 < -10]<-"-15_-10"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= -10 & communes_dates_1999_2022_temperature_final_1999$value1 < -5]<-"-10_-5"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= -5 & communes_dates_1999_2022_temperature_final_1999$value1 < 0]<-"-5_0"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 0 & communes_dates_1999_2022_temperature_final_1999$value1 < 5]<-"0_5"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 5 & communes_dates_1999_2022_temperature_final_1999$value1 < 10]<-"5_10"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 10 & communes_dates_1999_2022_temperature_final_1999$value1 < 15]<-"10_15"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 15 & communes_dates_1999_2022_temperature_final_1999$value1 < 20]<-"15_20"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 20 & communes_dates_1999_2022_temperature_final_1999$value1 < 25]<-"20_25"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 25 & communes_dates_1999_2022_temperature_final_1999$value1 < 28]<-"25_28"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 28 & communes_dates_1999_2022_temperature_final_1999$value1 < 30]<-"28_30"

communes_dates_1999_2022_temperature_final_1999$temperature_bin[communes_dates_1999_2022_temperature_final_1999$value1 >= 30]<-">30"


#test<-filter(communes_dates_1999_2022_temperature_final_1999, is.na(temperature_bin))
#table(communes_dates_1999_2022_temperature_final_1999$temperature_bin)

library(fastDummies)
communes_dates_1999_2022_temperature_final_1999  <- communes_dates_1999_2022_temperature_final_1999  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_1999_2022_temperature_final_1999 <- communes_dates_1999_2022_temperature_final_1999 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_1999_2022_temperature_final_1999 <- communes_dates_1999_2022_temperature_final_1999 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_1999_2022_temperature_final_1999$same_value[is.na(communes_dates_1999_2022_temperature_final_1999$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_1999_2022_temperature_final_1999$same_value <- ifelse(communes_dates_1999_2022_temperature_final_1999$temperature_bin != ">30", 0, communes_dates_1999_2022_temperature_final_1999$same_value)



communes_dates_1999_2022_temperature_final_1999$mois<-substring(communes_dates_1999_2022_temperature_final_1999$date,6,7)

communes_dates_1999_2022_temperature_final_1999<-communes_dates_1999_2022_temperature_final_1999[,-c("date","value1","temperature_bin")]

communes_dates_1999_2022_temperature_final_1999<-aggregate(.~COM+mois,communes_dates_1999_2022_temperature_final_1999,sum)



RP_1999_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final_2")

RP_1999_age_sexe_final_2<-RP_1999_age_sexe_final_2[,c(2:15)]

names(RP_1999_age_sexe_final_2)[names(RP_1999_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1999_2022_temperature_final_1999<-left_join(communes_dates_1999_2022_temperature_final_1999,RP_1999_age_sexe_final_2)

#tests<-filter(communes_dates_1999_2022_temperature_final_1999, COM=="01001")
#tests<-filter(communes_dates_1999_2022_temperature_final_1999, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999, !is.na(value_estimated_sum_homme) )

RP_1999_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1999_CSP_final_2")

RP_1999_CSP_final_2<-RP_1999_CSP_final_2[,c(2:12)]
names(RP_1999_CSP_final_2)[names(RP_1999_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_1999_CSP_final_2)[names(RP_1999_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_1999_2022_temperature_final_1999<-left_join(communes_dates_1999_2022_temperature_final_1999,RP_1999_CSP_final_2)


communes_dates_1999_2022_temperature_final_1999$part_agriculteur<-communes_dates_1999_2022_temperature_final_1999$value_estimated_agriculteur/communes_dates_1999_2022_temperature_final_1999$population_actif_25_54

communes_dates_1999_2022_temperature_final_1999$part_artisan_commercant_chef_entreprise<-communes_dates_1999_2022_temperature_final_1999$value_estimated_artisan_commercant_chef_entreprise/communes_dates_1999_2022_temperature_final_1999$population_actif_25_54

communes_dates_1999_2022_temperature_final_1999$part_cadre<-communes_dates_1999_2022_temperature_final_1999$value_estimated_cadre/communes_dates_1999_2022_temperature_final_1999$population_actif_25_54

communes_dates_1999_2022_temperature_final_1999$part_profession_intermediaire<-communes_dates_1999_2022_temperature_final_1999$value_estimated_profession_intermediaire/communes_dates_1999_2022_temperature_final_1999$population_actif_25_54

communes_dates_1999_2022_temperature_final_1999$part_employe<-communes_dates_1999_2022_temperature_final_1999$value_estimated_employe/communes_dates_1999_2022_temperature_final_1999$population_actif_25_54

communes_dates_1999_2022_temperature_final_1999$part_ouvrier<-communes_dates_1999_2022_temperature_final_1999$value_estimated_ouvrier/communes_dates_1999_2022_temperature_final_1999$population_actif_25_54

communes_dates_1999_2022_temperature_final_1999$part_chomage<-communes_dates_1999_2022_temperature_final_1999$value_estimated_au_chomage/communes_dates_1999_2022_temperature_final_1999$population_actif_25_54






communes_dates_1999_2022_temperature_final_1999$taux_mortalite_homme<-communes_dates_1999_2022_temperature_final_1999$Homme/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_homme

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_femme<-communes_dates_1999_2022_temperature_final_1999$Femme/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_femme


communes_dates_1999_2022_temperature_final_1999$taux_mortalite_0_9<-communes_dates_1999_2022_temperature_final_1999$`0-9`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_0_9_h_f

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_10_19<-communes_dates_1999_2022_temperature_final_1999$`10-19`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_10_19_h_f



communes_dates_1999_2022_temperature_final_1999$taux_mortalite_20_39<-communes_dates_1999_2022_temperature_final_1999$`20-39`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_20_39_h_f




communes_dates_1999_2022_temperature_final_1999$taux_mortalite_40_59<-communes_dates_1999_2022_temperature_final_1999$`40-59`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_40_59_h_f





communes_dates_1999_2022_temperature_final_1999$taux_mortalite_60_64<-communes_dates_1999_2022_temperature_final_1999$`60-64`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_60_64_h_f



communes_dates_1999_2022_temperature_final_1999$taux_mortalite_65_69<-communes_dates_1999_2022_temperature_final_1999$`65-69`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_65_69_h_f


communes_dates_1999_2022_temperature_final_1999$taux_mortalite_70_74<-communes_dates_1999_2022_temperature_final_1999$`70-74`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_70_74_h_f


communes_dates_1999_2022_temperature_final_1999$taux_mortalite_75_79<-communes_dates_1999_2022_temperature_final_1999$`75-79`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_75_79_h_f


communes_dates_1999_2022_temperature_final_1999$taux_mortalite_80_plus<-communes_dates_1999_2022_temperature_final_1999$`80+`/communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_80_plus_h_f


#communes_dates_1999_2022_temperature_final_1999$taux_mortalite_60_70<-(communes_dates_1999_2022_temperature_final_1999$`60-64`+communes_dates_1999_2022_temperature_final_1999$`65-69`)/(communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_60_64_h_f+communes_dates_1999_2022_temperature_final_1999$value_estimated_sum_65_69_h_f)



communes_dates_1999_2022_temperature_final_1999$mort_total<-communes_dates_1999_2022_temperature_final_1999$Femme+communes_dates_1999_2022_temperature_final_1999$Homme


communes_dates_1999_2022_temperature_final_1999$taux_mortalite_total<-communes_dates_1999_2022_temperature_final_1999$mort_total/communes_dates_1999_2022_temperature_final_1999$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999, communes_dates_1999_2022_temperature_final_1999$value_estimated_population>0)
communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999, communes_dates_1999_2022_temperature_final_1999$population_actif_25_54>0)




communes_dates_1999_2022_temperature_final_1999<- communes_dates_1999_2022_temperature_final_1999[ , !names(communes_dates_1999_2022_temperature_final_1999) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_femme))


communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(part_agriculteur))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(part_cadre))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(part_profession_intermediaire))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(part_employe))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(part_ouvrier))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(part_chomage))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_homme))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_0_9))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_10_19))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_20_39))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_40_59))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_60_64))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_65_69))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_70_74))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_75_79))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_80_plus))

communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  !is.infinite(taux_mortalite_total))


#761 valeurs inf

#

#communes_dates_1999_2022_temperature_final_1999<-communes_dates_1999_2022_temperature_final_1999[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  part_agriculteur<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_10_19<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  part_cadre<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  part_profession_intermediaire<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  part_employe<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  part_ouvrier<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  part_chomage<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_homme<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_femme<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_0_9<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_20_39<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_40_59<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_60_64<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_65_69<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_70_74<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_75_79<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_80_plus<=1)

#communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999,  taux_mortalite_total<=1)




communes_dates_1999_2022_temperature_final_1999$taux_mortalite_homme[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_homme>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_femme[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_femme>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_0_9[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_0_9>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_10_19[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_10_19>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_20_39[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_20_39>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_40_59[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_40_59>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_60_64[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_60_64>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_65_69[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_65_69>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_70_74[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_70_74>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_75_79[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_75_79>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_80_plus[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_80_plus>1]<-NA   

communes_dates_1999_2022_temperature_final_1999$taux_mortalite_total[communes_dates_1999_2022_temperature_final_1999$taux_mortalite_total>1]<-NA   





summary(communes_dates_1999_2022_temperature_final_1999$part_agriculteur)

summary(communes_dates_1999_2022_temperature_final_1999$part_artisan_commercant_chef_entreprise)

summary(communes_dates_1999_2022_temperature_final_1999$part_cadre)

summary(communes_dates_1999_2022_temperature_final_1999$part_profession_intermediaire)

summary(communes_dates_1999_2022_temperature_final_1999$part_employe)

summary(communes_dates_1999_2022_temperature_final_1999$part_ouvrier)

summary(communes_dates_1999_2022_temperature_final_1999$part_chomage)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_homme)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_femme)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_0_9)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_10_19)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_20_39)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_40_59)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_60_64)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_65_69)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_70_74)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_75_79)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_80_plus)

summary(communes_dates_1999_2022_temperature_final_1999$taux_mortalite_total)






fwrite(communes_dates_1999_2022_temperature_final_1999,"/données communes années/données mortalité temperature final mois new/communes_dates_1999_temperature_deces_mois.csv")










################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2000_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2000-01-01")
end_date <- as.Date("2000-12-31")

communes_dates_2000_2022_temperature_final_2000 <- communes_dates_2000_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2000_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2000_age_sexe.csv")

communes_dates_2000_2022_temperature_final_2000$date<-as.Date(communes_dates_2000_2022_temperature_final_2000$date)
deces.2000_age_sexe$date<-as.Date(deces.2000_age_sexe$date)


communes_dates_2000_2022_temperature_final_2000<-left_join(communes_dates_2000_2022_temperature_final_2000,deces.2000_age_sexe)

communes_dates_2000_2022_temperature_final_2000[is.na(communes_dates_2000_2022_temperature_final_2000)]<-0


communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 < -20]<-"<-20"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= -20 & communes_dates_2000_2022_temperature_final_2000$value1 < -15]<-"-20_-15"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= -15 & communes_dates_2000_2022_temperature_final_2000$value1 < -10]<-"-15_-10"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= -10 & communes_dates_2000_2022_temperature_final_2000$value1 < -5]<-"-10_-5"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= -5 & communes_dates_2000_2022_temperature_final_2000$value1 < 0]<-"-5_0"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 0 & communes_dates_2000_2022_temperature_final_2000$value1 < 5]<-"0_5"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 5 & communes_dates_2000_2022_temperature_final_2000$value1 < 10]<-"5_10"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 10 & communes_dates_2000_2022_temperature_final_2000$value1 < 15]<-"10_15"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 15 & communes_dates_2000_2022_temperature_final_2000$value1 < 20]<-"15_20"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 20 & communes_dates_2000_2022_temperature_final_2000$value1 < 25]<-"20_25"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 25 & communes_dates_2000_2022_temperature_final_2000$value1 < 28]<-"25_28"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 28 & communes_dates_2000_2022_temperature_final_2000$value1 < 30]<-"28_30"

communes_dates_2000_2022_temperature_final_2000$temperature_bin[communes_dates_2000_2022_temperature_final_2000$value1 >= 30]<-">30"


#test<-filter(communes_dates_2000_2022_temperature_final_2000, is.na(temperature_bin))
#table(communes_dates_2000_2022_temperature_final_2000$temperature_bin)

library(fastDummies)
communes_dates_2000_2022_temperature_final_2000  <- communes_dates_2000_2022_temperature_final_2000  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2000_2022_temperature_final_2000 <- communes_dates_2000_2022_temperature_final_2000 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2000_2022_temperature_final_2000 <- communes_dates_2000_2022_temperature_final_2000 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2000_2022_temperature_final_2000$same_value[is.na(communes_dates_2000_2022_temperature_final_2000$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2000_2022_temperature_final_2000$same_value <- ifelse(communes_dates_2000_2022_temperature_final_2000$temperature_bin != ">30", 0, communes_dates_2000_2022_temperature_final_2000$same_value)



communes_dates_2000_2022_temperature_final_2000$mois<-substring(communes_dates_2000_2022_temperature_final_2000$date,6,7)

communes_dates_2000_2022_temperature_final_2000<-communes_dates_2000_2022_temperature_final_2000[,-c("date","value1","temperature_bin")]

communes_dates_2000_2022_temperature_final_2000<-aggregate(.~COM+mois,communes_dates_2000_2022_temperature_final_2000,sum)



RP_2000_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2000_age_sexe_final_2")

RP_2000_age_sexe_final_2<-RP_2000_age_sexe_final_2[,c(2:15)]

names(RP_2000_age_sexe_final_2)[names(RP_2000_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2000_2022_temperature_final_2000<-left_join(communes_dates_2000_2022_temperature_final_2000,RP_2000_age_sexe_final_2)

#tests<-filter(communes_dates_2000_2022_temperature_final_2000, COM=="01001")
#tests<-filter(communes_dates_2000_2022_temperature_final_2000, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000, !is.na(value_estimated_sum_homme) )

RP_2000_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2000_CSP_final_2")

RP_2000_CSP_final_2<-RP_2000_CSP_final_2[,c(2:12)]
names(RP_2000_CSP_final_2)[names(RP_2000_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2000_CSP_final_2)[names(RP_2000_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2000_2022_temperature_final_2000<-left_join(communes_dates_2000_2022_temperature_final_2000,RP_2000_CSP_final_2)


communes_dates_2000_2022_temperature_final_2000$part_agriculteur<-communes_dates_2000_2022_temperature_final_2000$value_estimated_agriculteur/communes_dates_2000_2022_temperature_final_2000$population_actif_25_54

communes_dates_2000_2022_temperature_final_2000$part_artisan_commercant_chef_entreprise<-communes_dates_2000_2022_temperature_final_2000$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2000_2022_temperature_final_2000$population_actif_25_54

communes_dates_2000_2022_temperature_final_2000$part_cadre<-communes_dates_2000_2022_temperature_final_2000$value_estimated_cadre/communes_dates_2000_2022_temperature_final_2000$population_actif_25_54

communes_dates_2000_2022_temperature_final_2000$part_profession_intermediaire<-communes_dates_2000_2022_temperature_final_2000$value_estimated_profession_intermediaire/communes_dates_2000_2022_temperature_final_2000$population_actif_25_54

communes_dates_2000_2022_temperature_final_2000$part_employe<-communes_dates_2000_2022_temperature_final_2000$value_estimated_employe/communes_dates_2000_2022_temperature_final_2000$population_actif_25_54

communes_dates_2000_2022_temperature_final_2000$part_ouvrier<-communes_dates_2000_2022_temperature_final_2000$value_estimated_ouvrier/communes_dates_2000_2022_temperature_final_2000$population_actif_25_54

communes_dates_2000_2022_temperature_final_2000$part_chomage<-communes_dates_2000_2022_temperature_final_2000$value_estimated_au_chomage/communes_dates_2000_2022_temperature_final_2000$population_actif_25_54






communes_dates_2000_2022_temperature_final_2000$taux_mortalite_homme<-communes_dates_2000_2022_temperature_final_2000$Homme/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_homme

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_femme<-communes_dates_2000_2022_temperature_final_2000$Femme/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_femme


communes_dates_2000_2022_temperature_final_2000$taux_mortalite_0_9<-communes_dates_2000_2022_temperature_final_2000$`0-9`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_0_9_h_f

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_10_19<-communes_dates_2000_2022_temperature_final_2000$`10-19`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_10_19_h_f



communes_dates_2000_2022_temperature_final_2000$taux_mortalite_20_39<-communes_dates_2000_2022_temperature_final_2000$`20-39`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_20_39_h_f




communes_dates_2000_2022_temperature_final_2000$taux_mortalite_40_59<-communes_dates_2000_2022_temperature_final_2000$`40-59`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_40_59_h_f





communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_64<-communes_dates_2000_2022_temperature_final_2000$`60-64`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_60_64_h_f



communes_dates_2000_2022_temperature_final_2000$taux_mortalite_65_69<-communes_dates_2000_2022_temperature_final_2000$`65-69`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_65_69_h_f


communes_dates_2000_2022_temperature_final_2000$taux_mortalite_70_74<-communes_dates_2000_2022_temperature_final_2000$`70-74`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_70_74_h_f


communes_dates_2000_2022_temperature_final_2000$taux_mortalite_75_79<-communes_dates_2000_2022_temperature_final_2000$`75-79`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_75_79_h_f


communes_dates_2000_2022_temperature_final_2000$taux_mortalite_80_plus<-communes_dates_2000_2022_temperature_final_2000$`80+`/communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_80_plus_h_f


#communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_70<-(communes_dates_2000_2022_temperature_final_2000$`60-64`+communes_dates_2000_2022_temperature_final_2000$`65-69`)/(communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_60_64_h_f+communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_65_69_h_f)



communes_dates_2000_2022_temperature_final_2000$mort_total<-communes_dates_2000_2022_temperature_final_2000$Femme+communes_dates_2000_2022_temperature_final_2000$Homme


communes_dates_2000_2022_temperature_final_2000$taux_mortalite_total<-communes_dates_2000_2022_temperature_final_2000$mort_total/communes_dates_2000_2022_temperature_final_2000$value_estimated_population








communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_74<-(communes_dates_2000_2022_temperature_final_2000$`60-64`+communes_dates_2000_2022_temperature_final_2000$`65-69`+communes_dates_2000_2022_temperature_final_2000$`70-74`)/(communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_60_64_h_f+communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_65_69_h_f+communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_70_74_h_f)


communes_dates_2000_2022_temperature_final_2000$taux_mortalite_75_plus<-(communes_dates_2000_2022_temperature_final_2000$`75-79`+communes_dates_2000_2022_temperature_final_2000$`80+`)/(communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_75_79_h_f+communes_dates_2000_2022_temperature_final_2000$value_estimated_sum_80_plus_h_f)










#on enleve les communes avec des population de 0
communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000, communes_dates_2000_2022_temperature_final_2000$value_estimated_population>0)
communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000, communes_dates_2000_2022_temperature_final_2000$population_actif_25_54>0)




communes_dates_2000_2022_temperature_final_2000<- communes_dates_2000_2022_temperature_final_2000[ , !names(communes_dates_2000_2022_temperature_final_2000) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_femme))


communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(part_agriculteur))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(part_cadre))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(part_profession_intermediaire))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(part_employe))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(part_ouvrier))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(part_chomage))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_homme))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_0_9))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_10_19))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_20_39))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_40_59))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_60_64))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_65_69))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_70_74))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_75_79))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_total))



communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_60_74))
communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  !is.infinite(taux_mortalite_75_plus))



#761 valeurs inf

#

#communes_dates_2000_2022_temperature_final_2000<-communes_dates_2000_2022_temperature_final_2000[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  part_agriculteur<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_10_19<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  part_cadre<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  part_profession_intermediaire<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  part_employe<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  part_ouvrier<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  part_chomage<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_homme<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_femme<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_0_9<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_20_39<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_40_59<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_60_64<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_65_69<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_70_74<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_75_79<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_80_plus<=1)

#communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000,  taux_mortalite_total<=1)




communes_dates_2000_2022_temperature_final_2000$taux_mortalite_homme[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_homme>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_femme[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_femme>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_0_9[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_0_9>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_10_19[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_10_19>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_20_39[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_20_39>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_40_59[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_40_59>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_64[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_64>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_65_69[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_65_69>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_70_74[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_70_74>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_75_79[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_75_79>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_80_plus[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_80_plus>1]<-NA   

communes_dates_2000_2022_temperature_final_2000$taux_mortalite_total[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_total>1]<-NA   




communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_74[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_74>1]<-NA   
communes_dates_2000_2022_temperature_final_2000$taux_mortalite_75_plus[communes_dates_2000_2022_temperature_final_2000$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2000_2022_temperature_final_2000$part_agriculteur)

summary(communes_dates_2000_2022_temperature_final_2000$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2000_2022_temperature_final_2000$part_cadre)

summary(communes_dates_2000_2022_temperature_final_2000$part_profession_intermediaire)

summary(communes_dates_2000_2022_temperature_final_2000$part_employe)

summary(communes_dates_2000_2022_temperature_final_2000$part_ouvrier)

summary(communes_dates_2000_2022_temperature_final_2000$part_chomage)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_homme)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_femme)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_0_9)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_10_19)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_20_39)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_40_59)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_60_64)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_65_69)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_70_74)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_75_79)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_80_plus)

summary(communes_dates_2000_2022_temperature_final_2000$taux_mortalite_total)






fwrite(communes_dates_2000_2022_temperature_final_2000,"/données communes années/données mortalité temperature final mois new/communes_dates_2000_temperature_deces_mois.csv")


#tester format date






################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2001_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2001-01-01")
end_date <- as.Date("2001-12-31")

communes_dates_2001_2022_temperature_final_2001 <- communes_dates_2001_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2001_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2001_age_sexe.csv")




communes_dates_2001_2022_temperature_final_2001$date<-as.Date(communes_dates_2001_2022_temperature_final_2001$date)
deces.2001_age_sexe$date<-as.Date(deces.2001_age_sexe$date)






communes_dates_2001_2022_temperature_final_2001<-left_join(communes_dates_2001_2022_temperature_final_2001,deces.2001_age_sexe)

communes_dates_2001_2022_temperature_final_2001[is.na(communes_dates_2001_2022_temperature_final_2001)]<-0


communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 < -20]<-"<-20"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= -20 & communes_dates_2001_2022_temperature_final_2001$value1 < -15]<-"-20_-15"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= -15 & communes_dates_2001_2022_temperature_final_2001$value1 < -10]<-"-15_-10"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= -10 & communes_dates_2001_2022_temperature_final_2001$value1 < -5]<-"-10_-5"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= -5 & communes_dates_2001_2022_temperature_final_2001$value1 < 0]<-"-5_0"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 0 & communes_dates_2001_2022_temperature_final_2001$value1 < 5]<-"0_5"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 5 & communes_dates_2001_2022_temperature_final_2001$value1 < 10]<-"5_10"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 10 & communes_dates_2001_2022_temperature_final_2001$value1 < 15]<-"10_15"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 15 & communes_dates_2001_2022_temperature_final_2001$value1 < 20]<-"15_20"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 20 & communes_dates_2001_2022_temperature_final_2001$value1 < 25]<-"20_25"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 25 & communes_dates_2001_2022_temperature_final_2001$value1 < 28]<-"25_28"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 28 & communes_dates_2001_2022_temperature_final_2001$value1 < 30]<-"28_30"

communes_dates_2001_2022_temperature_final_2001$temperature_bin[communes_dates_2001_2022_temperature_final_2001$value1 >= 30]<-">30"


#test<-filter(communes_dates_2001_2022_temperature_final_2001, is.na(temperature_bin))
#table(communes_dates_2001_2022_temperature_final_2001$temperature_bin)

library(fastDummies)
communes_dates_2001_2022_temperature_final_2001  <- communes_dates_2001_2022_temperature_final_2001  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2001_2022_temperature_final_2001 <- communes_dates_2001_2022_temperature_final_2001 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2001_2022_temperature_final_2001 <- communes_dates_2001_2022_temperature_final_2001 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2001_2022_temperature_final_2001$same_value[is.na(communes_dates_2001_2022_temperature_final_2001$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2001_2022_temperature_final_2001$same_value <- ifelse(communes_dates_2001_2022_temperature_final_2001$temperature_bin != ">30", 0, communes_dates_2001_2022_temperature_final_2001$same_value)



communes_dates_2001_2022_temperature_final_2001$mois<-substring(communes_dates_2001_2022_temperature_final_2001$date,6,7)

communes_dates_2001_2022_temperature_final_2001<-communes_dates_2001_2022_temperature_final_2001[,-c("date","value1","temperature_bin")]

communes_dates_2001_2022_temperature_final_2001<-aggregate(.~COM+mois,communes_dates_2001_2022_temperature_final_2001,sum)



RP_2001_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2001_age_sexe_final_2")

RP_2001_age_sexe_final_2<-RP_2001_age_sexe_final_2[,c(2:15)]

names(RP_2001_age_sexe_final_2)[names(RP_2001_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2001_2022_temperature_final_2001<-left_join(communes_dates_2001_2022_temperature_final_2001,RP_2001_age_sexe_final_2)

#tests<-filter(communes_dates_2001_2022_temperature_final_2001, COM=="01001")
#tests<-filter(communes_dates_2001_2022_temperature_final_2001, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001, !is.na(value_estimated_sum_homme) )

RP_2001_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2001_CSP_final_2")

RP_2001_CSP_final_2<-RP_2001_CSP_final_2[,c(2:12)]
names(RP_2001_CSP_final_2)[names(RP_2001_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2001_CSP_final_2)[names(RP_2001_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2001_2022_temperature_final_2001<-left_join(communes_dates_2001_2022_temperature_final_2001,RP_2001_CSP_final_2)


communes_dates_2001_2022_temperature_final_2001$part_agriculteur<-communes_dates_2001_2022_temperature_final_2001$value_estimated_agriculteur/communes_dates_2001_2022_temperature_final_2001$population_actif_25_54

communes_dates_2001_2022_temperature_final_2001$part_artisan_commercant_chef_entreprise<-communes_dates_2001_2022_temperature_final_2001$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2001_2022_temperature_final_2001$population_actif_25_54

communes_dates_2001_2022_temperature_final_2001$part_cadre<-communes_dates_2001_2022_temperature_final_2001$value_estimated_cadre/communes_dates_2001_2022_temperature_final_2001$population_actif_25_54

communes_dates_2001_2022_temperature_final_2001$part_profession_intermediaire<-communes_dates_2001_2022_temperature_final_2001$value_estimated_profession_intermediaire/communes_dates_2001_2022_temperature_final_2001$population_actif_25_54

communes_dates_2001_2022_temperature_final_2001$part_employe<-communes_dates_2001_2022_temperature_final_2001$value_estimated_employe/communes_dates_2001_2022_temperature_final_2001$population_actif_25_54

communes_dates_2001_2022_temperature_final_2001$part_ouvrier<-communes_dates_2001_2022_temperature_final_2001$value_estimated_ouvrier/communes_dates_2001_2022_temperature_final_2001$population_actif_25_54

communes_dates_2001_2022_temperature_final_2001$part_chomage<-communes_dates_2001_2022_temperature_final_2001$value_estimated_au_chomage/communes_dates_2001_2022_temperature_final_2001$population_actif_25_54






communes_dates_2001_2022_temperature_final_2001$taux_mortalite_homme<-communes_dates_2001_2022_temperature_final_2001$Homme/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_homme

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_femme<-communes_dates_2001_2022_temperature_final_2001$Femme/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_femme


communes_dates_2001_2022_temperature_final_2001$taux_mortalite_0_9<-communes_dates_2001_2022_temperature_final_2001$`0-9`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_0_9_h_f

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_10_19<-communes_dates_2001_2022_temperature_final_2001$`10-19`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_10_19_h_f



communes_dates_2001_2022_temperature_final_2001$taux_mortalite_20_39<-communes_dates_2001_2022_temperature_final_2001$`20-39`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_20_39_h_f




communes_dates_2001_2022_temperature_final_2001$taux_mortalite_40_59<-communes_dates_2001_2022_temperature_final_2001$`40-59`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_40_59_h_f





communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_64<-communes_dates_2001_2022_temperature_final_2001$`60-64`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_60_64_h_f



communes_dates_2001_2022_temperature_final_2001$taux_mortalite_65_69<-communes_dates_2001_2022_temperature_final_2001$`65-69`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_65_69_h_f


communes_dates_2001_2022_temperature_final_2001$taux_mortalite_70_74<-communes_dates_2001_2022_temperature_final_2001$`70-74`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_70_74_h_f


communes_dates_2001_2022_temperature_final_2001$taux_mortalite_75_79<-communes_dates_2001_2022_temperature_final_2001$`75-79`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_75_79_h_f


communes_dates_2001_2022_temperature_final_2001$taux_mortalite_80_plus<-communes_dates_2001_2022_temperature_final_2001$`80+`/communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_80_plus_h_f


#communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_70<-(communes_dates_2001_2022_temperature_final_2001$`60-64`+communes_dates_2001_2022_temperature_final_2001$`65-69`)/(communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_60_64_h_f+communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_65_69_h_f)



communes_dates_2001_2022_temperature_final_2001$mort_total<-communes_dates_2001_2022_temperature_final_2001$Femme+communes_dates_2001_2022_temperature_final_2001$Homme


communes_dates_2001_2022_temperature_final_2001$taux_mortalite_total<-communes_dates_2001_2022_temperature_final_2001$mort_total/communes_dates_2001_2022_temperature_final_2001$value_estimated_population






communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_74<-(communes_dates_2001_2022_temperature_final_2001$`60-64`+communes_dates_2001_2022_temperature_final_2001$`65-69`+communes_dates_2001_2022_temperature_final_2001$`70-74`)/(communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_60_64_h_f+communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_65_69_h_f+communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_70_74_h_f)


communes_dates_2001_2022_temperature_final_2001$taux_mortalite_75_plus<-(communes_dates_2001_2022_temperature_final_2001$`75-79`+communes_dates_2001_2022_temperature_final_2001$`80+`)/(communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_75_79_h_f+communes_dates_2001_2022_temperature_final_2001$value_estimated_sum_80_plus_h_f)








#on enleve les communes avec des population de 0
communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001, communes_dates_2001_2022_temperature_final_2001$value_estimated_population>0)
communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001, communes_dates_2001_2022_temperature_final_2001$population_actif_25_54>0)




communes_dates_2001_2022_temperature_final_2001<- communes_dates_2001_2022_temperature_final_2001[ , !names(communes_dates_2001_2022_temperature_final_2001) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_femme))


communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(part_agriculteur))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(part_cadre))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(part_profession_intermediaire))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(part_employe))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(part_ouvrier))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(part_chomage))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_homme))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_0_9))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_10_19))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_20_39))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_40_59))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_60_64))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_65_69))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_70_74))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_75_79))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_total))




communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_60_74))
communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  !is.infinite(taux_mortalite_75_plus))









#761 valeurs inf

#

#communes_dates_2001_2022_temperature_final_2001<-communes_dates_2001_2022_temperature_final_2001[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  part_agriculteur<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_10_19<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  part_cadre<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  part_profession_intermediaire<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  part_employe<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  part_ouvrier<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  part_chomage<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_homme<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_femme<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_0_9<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_20_39<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_40_59<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_60_64<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_65_69<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_70_74<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_75_79<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_80_plus<=1)

#communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001,  taux_mortalite_total<=1)




communes_dates_2001_2022_temperature_final_2001$taux_mortalite_homme[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_homme>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_femme[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_femme>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_0_9[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_0_9>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_10_19[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_10_19>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_20_39[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_20_39>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_40_59[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_40_59>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_64[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_64>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_65_69[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_65_69>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_70_74[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_70_74>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_75_79[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_75_79>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_80_plus[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_80_plus>1]<-NA   

communes_dates_2001_2022_temperature_final_2001$taux_mortalite_total[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_total>1]<-NA   






communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_74[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_74>1]<-NA   
communes_dates_2001_2022_temperature_final_2001$taux_mortalite_75_plus[communes_dates_2001_2022_temperature_final_2001$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2001_2022_temperature_final_2001$part_agriculteur)

summary(communes_dates_2001_2022_temperature_final_2001$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2001_2022_temperature_final_2001$part_cadre)

summary(communes_dates_2001_2022_temperature_final_2001$part_profession_intermediaire)

summary(communes_dates_2001_2022_temperature_final_2001$part_employe)

summary(communes_dates_2001_2022_temperature_final_2001$part_ouvrier)

summary(communes_dates_2001_2022_temperature_final_2001$part_chomage)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_homme)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_femme)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_0_9)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_10_19)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_20_39)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_40_59)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_60_64)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_65_69)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_70_74)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_75_79)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_80_plus)

summary(communes_dates_2001_2022_temperature_final_2001$taux_mortalite_total)






fwrite(communes_dates_2001_2022_temperature_final_2001,"/données communes années/données mortalité temperature final mois new/communes_dates_2001_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2002_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2002-01-01")
end_date <- as.Date("2002-12-31")

communes_dates_2002_2022_temperature_final_2002 <- communes_dates_2002_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2002_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2002_age_sexe.csv")





communes_dates_2002_2022_temperature_final_2002$date<-as.Date(communes_dates_2002_2022_temperature_final_2002$date)
deces.2002_age_sexe$date<-as.Date(deces.2002_age_sexe$date)






communes_dates_2002_2022_temperature_final_2002<-left_join(communes_dates_2002_2022_temperature_final_2002,deces.2002_age_sexe)

communes_dates_2002_2022_temperature_final_2002[is.na(communes_dates_2002_2022_temperature_final_2002)]<-0


communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 < -20]<-"<-20"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= -20 & communes_dates_2002_2022_temperature_final_2002$value1 < -15]<-"-20_-15"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= -15 & communes_dates_2002_2022_temperature_final_2002$value1 < -10]<-"-15_-10"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= -10 & communes_dates_2002_2022_temperature_final_2002$value1 < -5]<-"-10_-5"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= -5 & communes_dates_2002_2022_temperature_final_2002$value1 < 0]<-"-5_0"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 0 & communes_dates_2002_2022_temperature_final_2002$value1 < 5]<-"0_5"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 5 & communes_dates_2002_2022_temperature_final_2002$value1 < 10]<-"5_10"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 10 & communes_dates_2002_2022_temperature_final_2002$value1 < 15]<-"10_15"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 15 & communes_dates_2002_2022_temperature_final_2002$value1 < 20]<-"15_20"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 20 & communes_dates_2002_2022_temperature_final_2002$value1 < 25]<-"20_25"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 25 & communes_dates_2002_2022_temperature_final_2002$value1 < 28]<-"25_28"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 28 & communes_dates_2002_2022_temperature_final_2002$value1 < 30]<-"28_30"

communes_dates_2002_2022_temperature_final_2002$temperature_bin[communes_dates_2002_2022_temperature_final_2002$value1 >= 30]<-">30"


#test<-filter(communes_dates_2002_2022_temperature_final_2002, is.na(temperature_bin))
#table(communes_dates_2002_2022_temperature_final_2002$temperature_bin)

library(fastDummies)
communes_dates_2002_2022_temperature_final_2002  <- communes_dates_2002_2022_temperature_final_2002  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2002_2022_temperature_final_2002 <- communes_dates_2002_2022_temperature_final_2002 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2002_2022_temperature_final_2002 <- communes_dates_2002_2022_temperature_final_2002 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2002_2022_temperature_final_2002$same_value[is.na(communes_dates_2002_2022_temperature_final_2002$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2002_2022_temperature_final_2002$same_value <- ifelse(communes_dates_2002_2022_temperature_final_2002$temperature_bin != ">30", 0, communes_dates_2002_2022_temperature_final_2002$same_value)



communes_dates_2002_2022_temperature_final_2002$mois<-substring(communes_dates_2002_2022_temperature_final_2002$date,6,7)

communes_dates_2002_2022_temperature_final_2002<-communes_dates_2002_2022_temperature_final_2002[,-c("date","value1","temperature_bin")]

communes_dates_2002_2022_temperature_final_2002<-aggregate(.~COM+mois,communes_dates_2002_2022_temperature_final_2002,sum)



RP_2002_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2002_age_sexe_final_2")

RP_2002_age_sexe_final_2<-RP_2002_age_sexe_final_2[,c(2:15)]

names(RP_2002_age_sexe_final_2)[names(RP_2002_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2002_2022_temperature_final_2002<-left_join(communes_dates_2002_2022_temperature_final_2002,RP_2002_age_sexe_final_2)

#tests<-filter(communes_dates_2002_2022_temperature_final_2002, COM=="01001")
#tests<-filter(communes_dates_2002_2022_temperature_final_2002, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002, !is.na(value_estimated_sum_homme) )

RP_2002_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2002_CSP_final_2")

RP_2002_CSP_final_2<-RP_2002_CSP_final_2[,c(2:12)]
names(RP_2002_CSP_final_2)[names(RP_2002_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2002_CSP_final_2)[names(RP_2002_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2002_2022_temperature_final_2002<-left_join(communes_dates_2002_2022_temperature_final_2002,RP_2002_CSP_final_2)


communes_dates_2002_2022_temperature_final_2002$part_agriculteur<-communes_dates_2002_2022_temperature_final_2002$value_estimated_agriculteur/communes_dates_2002_2022_temperature_final_2002$population_actif_25_54

communes_dates_2002_2022_temperature_final_2002$part_artisan_commercant_chef_entreprise<-communes_dates_2002_2022_temperature_final_2002$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2002_2022_temperature_final_2002$population_actif_25_54

communes_dates_2002_2022_temperature_final_2002$part_cadre<-communes_dates_2002_2022_temperature_final_2002$value_estimated_cadre/communes_dates_2002_2022_temperature_final_2002$population_actif_25_54

communes_dates_2002_2022_temperature_final_2002$part_profession_intermediaire<-communes_dates_2002_2022_temperature_final_2002$value_estimated_profession_intermediaire/communes_dates_2002_2022_temperature_final_2002$population_actif_25_54

communes_dates_2002_2022_temperature_final_2002$part_employe<-communes_dates_2002_2022_temperature_final_2002$value_estimated_employe/communes_dates_2002_2022_temperature_final_2002$population_actif_25_54

communes_dates_2002_2022_temperature_final_2002$part_ouvrier<-communes_dates_2002_2022_temperature_final_2002$value_estimated_ouvrier/communes_dates_2002_2022_temperature_final_2002$population_actif_25_54

communes_dates_2002_2022_temperature_final_2002$part_chomage<-communes_dates_2002_2022_temperature_final_2002$value_estimated_au_chomage/communes_dates_2002_2022_temperature_final_2002$population_actif_25_54






communes_dates_2002_2022_temperature_final_2002$taux_mortalite_homme<-communes_dates_2002_2022_temperature_final_2002$Homme/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_homme

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_femme<-communes_dates_2002_2022_temperature_final_2002$Femme/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_femme


communes_dates_2002_2022_temperature_final_2002$taux_mortalite_0_9<-communes_dates_2002_2022_temperature_final_2002$`0-9`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_0_9_h_f

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_10_19<-communes_dates_2002_2022_temperature_final_2002$`10-19`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_10_19_h_f



communes_dates_2002_2022_temperature_final_2002$taux_mortalite_20_39<-communes_dates_2002_2022_temperature_final_2002$`20-39`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_20_39_h_f




communes_dates_2002_2022_temperature_final_2002$taux_mortalite_40_59<-communes_dates_2002_2022_temperature_final_2002$`40-59`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_40_59_h_f





communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_64<-communes_dates_2002_2022_temperature_final_2002$`60-64`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_60_64_h_f



communes_dates_2002_2022_temperature_final_2002$taux_mortalite_65_69<-communes_dates_2002_2022_temperature_final_2002$`65-69`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_65_69_h_f


communes_dates_2002_2022_temperature_final_2002$taux_mortalite_70_74<-communes_dates_2002_2022_temperature_final_2002$`70-74`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_70_74_h_f


communes_dates_2002_2022_temperature_final_2002$taux_mortalite_75_79<-communes_dates_2002_2022_temperature_final_2002$`75-79`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_75_79_h_f


communes_dates_2002_2022_temperature_final_2002$taux_mortalite_80_plus<-communes_dates_2002_2022_temperature_final_2002$`80+`/communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_80_plus_h_f


#communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_70<-(communes_dates_2002_2022_temperature_final_2002$`60-64`+communes_dates_2002_2022_temperature_final_2002$`65-69`)/(communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_60_64_h_f+communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_65_69_h_f)



communes_dates_2002_2022_temperature_final_2002$mort_total<-communes_dates_2002_2022_temperature_final_2002$Femme+communes_dates_2002_2022_temperature_final_2002$Homme


communes_dates_2002_2022_temperature_final_2002$taux_mortalite_total<-communes_dates_2002_2022_temperature_final_2002$mort_total/communes_dates_2002_2022_temperature_final_2002$value_estimated_population





communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_74<-(communes_dates_2002_2022_temperature_final_2002$`60-64`+communes_dates_2002_2022_temperature_final_2002$`65-69`+communes_dates_2002_2022_temperature_final_2002$`70-74`)/(communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_60_64_h_f+communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_65_69_h_f+communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_70_74_h_f)


communes_dates_2002_2022_temperature_final_2002$taux_mortalite_75_plus<-(communes_dates_2002_2022_temperature_final_2002$`75-79`+communes_dates_2002_2022_temperature_final_2002$`80+`)/(communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_75_79_h_f+communes_dates_2002_2022_temperature_final_2002$value_estimated_sum_80_plus_h_f)











#on enleve les communes avec des population de 0
communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002, communes_dates_2002_2022_temperature_final_2002$value_estimated_population>0)
communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002, communes_dates_2002_2022_temperature_final_2002$population_actif_25_54>0)




communes_dates_2002_2022_temperature_final_2002<- communes_dates_2002_2022_temperature_final_2002[ , !names(communes_dates_2002_2022_temperature_final_2002) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_femme))


communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(part_agriculteur))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(part_cadre))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(part_profession_intermediaire))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(part_employe))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(part_ouvrier))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(part_chomage))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_homme))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_0_9))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_10_19))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_20_39))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_40_59))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_60_64))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_65_69))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_70_74))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_75_79))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_total))




communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_60_74))
communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  !is.infinite(taux_mortalite_75_plus))







#761 valeurs inf

#

#communes_dates_2002_2022_temperature_final_2002<-communes_dates_2002_2022_temperature_final_2002[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  part_agriculteur<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_10_19<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  part_cadre<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  part_profession_intermediaire<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  part_employe<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  part_ouvrier<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  part_chomage<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_homme<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_femme<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_0_9<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_20_39<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_40_59<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_60_64<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_65_69<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_70_74<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_75_79<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_80_plus<=1)

#communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002,  taux_mortalite_total<=1)




communes_dates_2002_2022_temperature_final_2002$taux_mortalite_homme[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_homme>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_femme[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_femme>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_0_9[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_0_9>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_10_19[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_10_19>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_20_39[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_20_39>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_40_59[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_40_59>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_64[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_64>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_65_69[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_65_69>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_70_74[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_70_74>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_75_79[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_75_79>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_80_plus[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_80_plus>1]<-NA   

communes_dates_2002_2022_temperature_final_2002$taux_mortalite_total[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_total>1]<-NA   




communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_74[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_74>1]<-NA   
communes_dates_2002_2022_temperature_final_2002$taux_mortalite_75_plus[communes_dates_2002_2022_temperature_final_2002$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2002_2022_temperature_final_2002$part_agriculteur)

summary(communes_dates_2002_2022_temperature_final_2002$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2002_2022_temperature_final_2002$part_cadre)

summary(communes_dates_2002_2022_temperature_final_2002$part_profession_intermediaire)

summary(communes_dates_2002_2022_temperature_final_2002$part_employe)

summary(communes_dates_2002_2022_temperature_final_2002$part_ouvrier)

summary(communes_dates_2002_2022_temperature_final_2002$part_chomage)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_homme)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_femme)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_0_9)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_10_19)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_20_39)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_40_59)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_60_64)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_65_69)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_70_74)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_75_79)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_80_plus)

summary(communes_dates_2002_2022_temperature_final_2002$taux_mortalite_total)






fwrite(communes_dates_2002_2022_temperature_final_2002,"/données communes années/données mortalité temperature final mois new/communes_dates_2002_temperature_deces_mois.csv")


















################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2003_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2003-01-01")
end_date <- as.Date("2003-12-31")

communes_dates_2003_2022_temperature_final_2003 <- communes_dates_2003_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2003_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2003_age_sexe.csv")





communes_dates_2003_2022_temperature_final_2003$date<-as.Date(communes_dates_2003_2022_temperature_final_2003$date)
deces.2003_age_sexe$date<-as.Date(deces.2003_age_sexe$date)





communes_dates_2003_2022_temperature_final_2003<-left_join(communes_dates_2003_2022_temperature_final_2003,deces.2003_age_sexe)

communes_dates_2003_2022_temperature_final_2003[is.na(communes_dates_2003_2022_temperature_final_2003)]<-0


communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 < -20]<-"<-20"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= -20 & communes_dates_2003_2022_temperature_final_2003$value1 < -15]<-"-20_-15"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= -15 & communes_dates_2003_2022_temperature_final_2003$value1 < -10]<-"-15_-10"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= -10 & communes_dates_2003_2022_temperature_final_2003$value1 < -5]<-"-10_-5"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= -5 & communes_dates_2003_2022_temperature_final_2003$value1 < 0]<-"-5_0"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 0 & communes_dates_2003_2022_temperature_final_2003$value1 < 5]<-"0_5"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 5 & communes_dates_2003_2022_temperature_final_2003$value1 < 10]<-"5_10"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 10 & communes_dates_2003_2022_temperature_final_2003$value1 < 15]<-"10_15"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 15 & communes_dates_2003_2022_temperature_final_2003$value1 < 20]<-"15_20"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 20 & communes_dates_2003_2022_temperature_final_2003$value1 < 25]<-"20_25"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 25 & communes_dates_2003_2022_temperature_final_2003$value1 < 28]<-"25_28"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 28 & communes_dates_2003_2022_temperature_final_2003$value1 < 30]<-"28_30"

communes_dates_2003_2022_temperature_final_2003$temperature_bin[communes_dates_2003_2022_temperature_final_2003$value1 >= 30]<-">30"


#test<-filter(communes_dates_2003_2022_temperature_final_2003, is.na(temperature_bin))
#table(communes_dates_2003_2022_temperature_final_2003$temperature_bin)

library(fastDummies)
communes_dates_2003_2022_temperature_final_2003  <- communes_dates_2003_2022_temperature_final_2003  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2003_2022_temperature_final_2003 <- communes_dates_2003_2022_temperature_final_2003 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2003_2022_temperature_final_2003 <- communes_dates_2003_2022_temperature_final_2003 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2003_2022_temperature_final_2003$same_value[is.na(communes_dates_2003_2022_temperature_final_2003$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2003_2022_temperature_final_2003$same_value <- ifelse(communes_dates_2003_2022_temperature_final_2003$temperature_bin != ">30", 0, communes_dates_2003_2022_temperature_final_2003$same_value)



communes_dates_2003_2022_temperature_final_2003$mois<-substring(communes_dates_2003_2022_temperature_final_2003$date,6,7)

communes_dates_2003_2022_temperature_final_2003<-communes_dates_2003_2022_temperature_final_2003[,-c("date","value1","temperature_bin")]

communes_dates_2003_2022_temperature_final_2003<-aggregate(.~COM+mois,communes_dates_2003_2022_temperature_final_2003,sum)



RP_2003_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2003_age_sexe_final_2")

RP_2003_age_sexe_final_2<-RP_2003_age_sexe_final_2[,c(2:15)]

names(RP_2003_age_sexe_final_2)[names(RP_2003_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2003_2022_temperature_final_2003<-left_join(communes_dates_2003_2022_temperature_final_2003,RP_2003_age_sexe_final_2)

#tests<-filter(communes_dates_2003_2022_temperature_final_2003, COM=="01001")
#tests<-filter(communes_dates_2003_2022_temperature_final_2003, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003, !is.na(value_estimated_sum_homme) )

RP_2003_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2003_CSP_final_2")

RP_2003_CSP_final_2<-RP_2003_CSP_final_2[,c(2:12)]
names(RP_2003_CSP_final_2)[names(RP_2003_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2003_CSP_final_2)[names(RP_2003_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2003_2022_temperature_final_2003<-left_join(communes_dates_2003_2022_temperature_final_2003,RP_2003_CSP_final_2)


communes_dates_2003_2022_temperature_final_2003$part_agriculteur<-communes_dates_2003_2022_temperature_final_2003$value_estimated_agriculteur/communes_dates_2003_2022_temperature_final_2003$population_actif_25_54

communes_dates_2003_2022_temperature_final_2003$part_artisan_commercant_chef_entreprise<-communes_dates_2003_2022_temperature_final_2003$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2003_2022_temperature_final_2003$population_actif_25_54

communes_dates_2003_2022_temperature_final_2003$part_cadre<-communes_dates_2003_2022_temperature_final_2003$value_estimated_cadre/communes_dates_2003_2022_temperature_final_2003$population_actif_25_54

communes_dates_2003_2022_temperature_final_2003$part_profession_intermediaire<-communes_dates_2003_2022_temperature_final_2003$value_estimated_profession_intermediaire/communes_dates_2003_2022_temperature_final_2003$population_actif_25_54

communes_dates_2003_2022_temperature_final_2003$part_employe<-communes_dates_2003_2022_temperature_final_2003$value_estimated_employe/communes_dates_2003_2022_temperature_final_2003$population_actif_25_54

communes_dates_2003_2022_temperature_final_2003$part_ouvrier<-communes_dates_2003_2022_temperature_final_2003$value_estimated_ouvrier/communes_dates_2003_2022_temperature_final_2003$population_actif_25_54

communes_dates_2003_2022_temperature_final_2003$part_chomage<-communes_dates_2003_2022_temperature_final_2003$value_estimated_au_chomage/communes_dates_2003_2022_temperature_final_2003$population_actif_25_54






communes_dates_2003_2022_temperature_final_2003$taux_mortalite_homme<-communes_dates_2003_2022_temperature_final_2003$Homme/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_homme

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_femme<-communes_dates_2003_2022_temperature_final_2003$Femme/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_femme


communes_dates_2003_2022_temperature_final_2003$taux_mortalite_0_9<-communes_dates_2003_2022_temperature_final_2003$`0-9`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_0_9_h_f

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_10_19<-communes_dates_2003_2022_temperature_final_2003$`10-19`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_10_19_h_f



communes_dates_2003_2022_temperature_final_2003$taux_mortalite_20_39<-communes_dates_2003_2022_temperature_final_2003$`20-39`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_20_39_h_f




communes_dates_2003_2022_temperature_final_2003$taux_mortalite_40_59<-communes_dates_2003_2022_temperature_final_2003$`40-59`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_40_59_h_f





communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_64<-communes_dates_2003_2022_temperature_final_2003$`60-64`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_60_64_h_f



communes_dates_2003_2022_temperature_final_2003$taux_mortalite_65_69<-communes_dates_2003_2022_temperature_final_2003$`65-69`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_65_69_h_f


communes_dates_2003_2022_temperature_final_2003$taux_mortalite_70_74<-communes_dates_2003_2022_temperature_final_2003$`70-74`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_70_74_h_f


communes_dates_2003_2022_temperature_final_2003$taux_mortalite_75_79<-communes_dates_2003_2022_temperature_final_2003$`75-79`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_75_79_h_f


communes_dates_2003_2022_temperature_final_2003$taux_mortalite_80_plus<-communes_dates_2003_2022_temperature_final_2003$`80+`/communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_80_plus_h_f


#communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_70<-(communes_dates_2003_2022_temperature_final_2003$`60-64`+communes_dates_2003_2022_temperature_final_2003$`65-69`)/(communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_60_64_h_f+communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_65_69_h_f)



communes_dates_2003_2022_temperature_final_2003$mort_total<-communes_dates_2003_2022_temperature_final_2003$Femme+communes_dates_2003_2022_temperature_final_2003$Homme


communes_dates_2003_2022_temperature_final_2003$taux_mortalite_total<-communes_dates_2003_2022_temperature_final_2003$mort_total/communes_dates_2003_2022_temperature_final_2003$value_estimated_population






communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_74<-(communes_dates_2003_2022_temperature_final_2003$`60-64`+communes_dates_2003_2022_temperature_final_2003$`65-69`+communes_dates_2003_2022_temperature_final_2003$`70-74`)/(communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_60_64_h_f+communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_65_69_h_f+communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_70_74_h_f)


communes_dates_2003_2022_temperature_final_2003$taux_mortalite_75_plus<-(communes_dates_2003_2022_temperature_final_2003$`75-79`+communes_dates_2003_2022_temperature_final_2003$`80+`)/(communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_75_79_h_f+communes_dates_2003_2022_temperature_final_2003$value_estimated_sum_80_plus_h_f)






#on enleve les communes avec des population de 0
communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003, communes_dates_2003_2022_temperature_final_2003$value_estimated_population>0)
communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003, communes_dates_2003_2022_temperature_final_2003$population_actif_25_54>0)




communes_dates_2003_2022_temperature_final_2003<- communes_dates_2003_2022_temperature_final_2003[ , !names(communes_dates_2003_2022_temperature_final_2003) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_femme))


communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(part_agriculteur))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(part_cadre))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(part_profession_intermediaire))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(part_employe))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(part_ouvrier))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(part_chomage))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_homme))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_0_9))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_10_19))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_20_39))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_40_59))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_60_64))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_65_69))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_70_74))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_75_79))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_total))





communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_60_74))
communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  !is.infinite(taux_mortalite_75_plus))





#761 valeurs inf

#

#communes_dates_2003_2022_temperature_final_2003<-communes_dates_2003_2022_temperature_final_2003[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  part_agriculteur<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_10_19<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  part_cadre<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  part_profession_intermediaire<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  part_employe<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  part_ouvrier<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  part_chomage<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_homme<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_femme<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_0_9<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_20_39<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_40_59<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_60_64<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_65_69<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_70_74<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_75_79<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_80_plus<=1)

#communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003,  taux_mortalite_total<=1)




communes_dates_2003_2022_temperature_final_2003$taux_mortalite_homme[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_homme>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_femme[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_femme>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_0_9[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_0_9>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_10_19[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_10_19>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_20_39[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_20_39>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_40_59[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_40_59>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_64[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_64>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_65_69[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_65_69>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_70_74[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_70_74>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_75_79[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_75_79>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_80_plus[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_80_plus>1]<-NA   

communes_dates_2003_2022_temperature_final_2003$taux_mortalite_total[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_total>1]<-NA   



communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_74[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_74>1]<-NA   
communes_dates_2003_2022_temperature_final_2003$taux_mortalite_75_plus[communes_dates_2003_2022_temperature_final_2003$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2003_2022_temperature_final_2003$part_agriculteur)

summary(communes_dates_2003_2022_temperature_final_2003$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2003_2022_temperature_final_2003$part_cadre)

summary(communes_dates_2003_2022_temperature_final_2003$part_profession_intermediaire)

summary(communes_dates_2003_2022_temperature_final_2003$part_employe)

summary(communes_dates_2003_2022_temperature_final_2003$part_ouvrier)

summary(communes_dates_2003_2022_temperature_final_2003$part_chomage)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_homme)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_femme)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_0_9)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_10_19)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_20_39)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_40_59)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_60_64)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_65_69)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_70_74)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_75_79)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_80_plus)

summary(communes_dates_2003_2022_temperature_final_2003$taux_mortalite_total)






fwrite(communes_dates_2003_2022_temperature_final_2003,"/données communes années/données mortalité temperature final mois new/communes_dates_2003_temperature_deces_mois.csv")














################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2004_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2004-01-01")
end_date <- as.Date("2004-12-31")

communes_dates_2004_2022_temperature_final_2004 <- communes_dates_2004_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2004_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2004_age_sexe.csv")




communes_dates_2004_2022_temperature_final_2004$date<-as.Date(communes_dates_2004_2022_temperature_final_2004$date)
deces.2004_age_sexe$date<-as.Date(deces.2004_age_sexe$date)






communes_dates_2004_2022_temperature_final_2004<-left_join(communes_dates_2004_2022_temperature_final_2004,deces.2004_age_sexe)

communes_dates_2004_2022_temperature_final_2004[is.na(communes_dates_2004_2022_temperature_final_2004)]<-0


communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 < -20]<-"<-20"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= -20 & communes_dates_2004_2022_temperature_final_2004$value1 < -15]<-"-20_-15"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= -15 & communes_dates_2004_2022_temperature_final_2004$value1 < -10]<-"-15_-10"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= -10 & communes_dates_2004_2022_temperature_final_2004$value1 < -5]<-"-10_-5"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= -5 & communes_dates_2004_2022_temperature_final_2004$value1 < 0]<-"-5_0"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 0 & communes_dates_2004_2022_temperature_final_2004$value1 < 5]<-"0_5"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 5 & communes_dates_2004_2022_temperature_final_2004$value1 < 10]<-"5_10"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 10 & communes_dates_2004_2022_temperature_final_2004$value1 < 15]<-"10_15"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 15 & communes_dates_2004_2022_temperature_final_2004$value1 < 20]<-"15_20"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 20 & communes_dates_2004_2022_temperature_final_2004$value1 < 25]<-"20_25"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 25 & communes_dates_2004_2022_temperature_final_2004$value1 < 28]<-"25_28"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 28 & communes_dates_2004_2022_temperature_final_2004$value1 < 30]<-"28_30"

communes_dates_2004_2022_temperature_final_2004$temperature_bin[communes_dates_2004_2022_temperature_final_2004$value1 >= 30]<-">30"


#test<-filter(communes_dates_2004_2022_temperature_final_2004, is.na(temperature_bin))
#table(communes_dates_2004_2022_temperature_final_2004$temperature_bin)

library(fastDummies)
communes_dates_2004_2022_temperature_final_2004  <- communes_dates_2004_2022_temperature_final_2004  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2004_2022_temperature_final_2004 <- communes_dates_2004_2022_temperature_final_2004 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2004_2022_temperature_final_2004 <- communes_dates_2004_2022_temperature_final_2004 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2004_2022_temperature_final_2004$same_value[is.na(communes_dates_2004_2022_temperature_final_2004$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2004_2022_temperature_final_2004$same_value <- ifelse(communes_dates_2004_2022_temperature_final_2004$temperature_bin != ">30", 0, communes_dates_2004_2022_temperature_final_2004$same_value)



communes_dates_2004_2022_temperature_final_2004$mois<-substring(communes_dates_2004_2022_temperature_final_2004$date,6,7)

communes_dates_2004_2022_temperature_final_2004<-communes_dates_2004_2022_temperature_final_2004[,-c("date","value1","temperature_bin")]

communes_dates_2004_2022_temperature_final_2004<-aggregate(.~COM+mois,communes_dates_2004_2022_temperature_final_2004,sum)



RP_2004_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2004_age_sexe_final_2")

RP_2004_age_sexe_final_2<-RP_2004_age_sexe_final_2[,c(2:15)]

names(RP_2004_age_sexe_final_2)[names(RP_2004_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2004_2022_temperature_final_2004<-left_join(communes_dates_2004_2022_temperature_final_2004,RP_2004_age_sexe_final_2)

#tests<-filter(communes_dates_2004_2022_temperature_final_2004, COM=="01001")
#tests<-filter(communes_dates_2004_2022_temperature_final_2004, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004, !is.na(value_estimated_sum_homme) )

RP_2004_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2004_CSP_final_2")

RP_2004_CSP_final_2<-RP_2004_CSP_final_2[,c(2:12)]
names(RP_2004_CSP_final_2)[names(RP_2004_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2004_CSP_final_2)[names(RP_2004_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2004_2022_temperature_final_2004<-left_join(communes_dates_2004_2022_temperature_final_2004,RP_2004_CSP_final_2)


communes_dates_2004_2022_temperature_final_2004$part_agriculteur<-communes_dates_2004_2022_temperature_final_2004$value_estimated_agriculteur/communes_dates_2004_2022_temperature_final_2004$population_actif_25_54

communes_dates_2004_2022_temperature_final_2004$part_artisan_commercant_chef_entreprise<-communes_dates_2004_2022_temperature_final_2004$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2004_2022_temperature_final_2004$population_actif_25_54

communes_dates_2004_2022_temperature_final_2004$part_cadre<-communes_dates_2004_2022_temperature_final_2004$value_estimated_cadre/communes_dates_2004_2022_temperature_final_2004$population_actif_25_54

communes_dates_2004_2022_temperature_final_2004$part_profession_intermediaire<-communes_dates_2004_2022_temperature_final_2004$value_estimated_profession_intermediaire/communes_dates_2004_2022_temperature_final_2004$population_actif_25_54

communes_dates_2004_2022_temperature_final_2004$part_employe<-communes_dates_2004_2022_temperature_final_2004$value_estimated_employe/communes_dates_2004_2022_temperature_final_2004$population_actif_25_54

communes_dates_2004_2022_temperature_final_2004$part_ouvrier<-communes_dates_2004_2022_temperature_final_2004$value_estimated_ouvrier/communes_dates_2004_2022_temperature_final_2004$population_actif_25_54

communes_dates_2004_2022_temperature_final_2004$part_chomage<-communes_dates_2004_2022_temperature_final_2004$value_estimated_au_chomage/communes_dates_2004_2022_temperature_final_2004$population_actif_25_54






communes_dates_2004_2022_temperature_final_2004$taux_mortalite_homme<-communes_dates_2004_2022_temperature_final_2004$Homme/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_homme

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_femme<-communes_dates_2004_2022_temperature_final_2004$Femme/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_femme


communes_dates_2004_2022_temperature_final_2004$taux_mortalite_0_9<-communes_dates_2004_2022_temperature_final_2004$`0-9`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_0_9_h_f

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_10_19<-communes_dates_2004_2022_temperature_final_2004$`10-19`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_10_19_h_f



communes_dates_2004_2022_temperature_final_2004$taux_mortalite_20_39<-communes_dates_2004_2022_temperature_final_2004$`20-39`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_20_39_h_f




communes_dates_2004_2022_temperature_final_2004$taux_mortalite_40_59<-communes_dates_2004_2022_temperature_final_2004$`40-59`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_40_59_h_f





communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_64<-communes_dates_2004_2022_temperature_final_2004$`60-64`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_60_64_h_f



communes_dates_2004_2022_temperature_final_2004$taux_mortalite_65_69<-communes_dates_2004_2022_temperature_final_2004$`65-69`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_65_69_h_f


communes_dates_2004_2022_temperature_final_2004$taux_mortalite_70_74<-communes_dates_2004_2022_temperature_final_2004$`70-74`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_70_74_h_f


communes_dates_2004_2022_temperature_final_2004$taux_mortalite_75_79<-communes_dates_2004_2022_temperature_final_2004$`75-79`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_75_79_h_f


communes_dates_2004_2022_temperature_final_2004$taux_mortalite_80_plus<-communes_dates_2004_2022_temperature_final_2004$`80+`/communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_80_plus_h_f


#communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_70<-(communes_dates_2004_2022_temperature_final_2004$`60-64`+communes_dates_2004_2022_temperature_final_2004$`65-69`)/(communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_60_64_h_f+communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_65_69_h_f)



communes_dates_2004_2022_temperature_final_2004$mort_total<-communes_dates_2004_2022_temperature_final_2004$Femme+communes_dates_2004_2022_temperature_final_2004$Homme


communes_dates_2004_2022_temperature_final_2004$taux_mortalite_total<-communes_dates_2004_2022_temperature_final_2004$mort_total/communes_dates_2004_2022_temperature_final_2004$value_estimated_population








communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_74<-(communes_dates_2004_2022_temperature_final_2004$`60-64`+communes_dates_2004_2022_temperature_final_2004$`65-69`+communes_dates_2004_2022_temperature_final_2004$`70-74`)/(communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_60_64_h_f+communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_65_69_h_f+communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_70_74_h_f)


communes_dates_2004_2022_temperature_final_2004$taux_mortalite_75_plus<-(communes_dates_2004_2022_temperature_final_2004$`75-79`+communes_dates_2004_2022_temperature_final_2004$`80+`)/(communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_75_79_h_f+communes_dates_2004_2022_temperature_final_2004$value_estimated_sum_80_plus_h_f)







#on enleve les communes avec des population de 0
communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004, communes_dates_2004_2022_temperature_final_2004$value_estimated_population>0)
communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004, communes_dates_2004_2022_temperature_final_2004$population_actif_25_54>0)




communes_dates_2004_2022_temperature_final_2004<- communes_dates_2004_2022_temperature_final_2004[ , !names(communes_dates_2004_2022_temperature_final_2004) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_femme))


communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(part_agriculteur))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(part_cadre))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(part_profession_intermediaire))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(part_employe))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(part_ouvrier))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(part_chomage))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_homme))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_0_9))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_10_19))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_20_39))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_40_59))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_60_64))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_65_69))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_70_74))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_75_79))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_total))





communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_60_74))
communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  !is.infinite(taux_mortalite_75_plus))






#761 valeurs inf

#

#communes_dates_2004_2022_temperature_final_2004<-communes_dates_2004_2022_temperature_final_2004[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  part_agriculteur<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_10_19<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  part_cadre<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  part_profession_intermediaire<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  part_employe<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  part_ouvrier<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  part_chomage<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_homme<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_femme<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_0_9<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_20_39<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_40_59<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_60_64<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_65_69<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_70_74<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_75_79<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_80_plus<=1)

#communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004,  taux_mortalite_total<=1)




communes_dates_2004_2022_temperature_final_2004$taux_mortalite_homme[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_homme>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_femme[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_femme>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_0_9[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_0_9>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_10_19[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_10_19>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_20_39[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_20_39>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_40_59[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_40_59>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_64[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_64>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_65_69[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_65_69>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_70_74[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_70_74>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_75_79[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_75_79>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_80_plus[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_80_plus>1]<-NA   

communes_dates_2004_2022_temperature_final_2004$taux_mortalite_total[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_total>1]<-NA   




communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_74[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_74>1]<-NA   
communes_dates_2004_2022_temperature_final_2004$taux_mortalite_75_plus[communes_dates_2004_2022_temperature_final_2004$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2004_2022_temperature_final_2004$part_agriculteur)

summary(communes_dates_2004_2022_temperature_final_2004$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2004_2022_temperature_final_2004$part_cadre)

summary(communes_dates_2004_2022_temperature_final_2004$part_profession_intermediaire)

summary(communes_dates_2004_2022_temperature_final_2004$part_employe)

summary(communes_dates_2004_2022_temperature_final_2004$part_ouvrier)

summary(communes_dates_2004_2022_temperature_final_2004$part_chomage)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_homme)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_femme)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_0_9)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_10_19)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_20_39)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_40_59)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_60_64)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_65_69)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_70_74)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_75_79)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_80_plus)

summary(communes_dates_2004_2022_temperature_final_2004$taux_mortalite_total)






fwrite(communes_dates_2004_2022_temperature_final_2004,"/données communes années/données mortalité temperature final mois new/communes_dates_2004_temperature_deces_mois.csv")







################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2005_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2005-01-01")
end_date <- as.Date("2005-12-31")

communes_dates_2005_2022_temperature_final_2005 <- communes_dates_2005_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2005_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2005_age_sexe.csv")




communes_dates_2005_2022_temperature_final_2005$date<-as.Date(communes_dates_2005_2022_temperature_final_2005$date)
deces.2005_age_sexe$date<-as.Date(deces.2005_age_sexe$date)






communes_dates_2005_2022_temperature_final_2005<-left_join(communes_dates_2005_2022_temperature_final_2005,deces.2005_age_sexe)

communes_dates_2005_2022_temperature_final_2005[is.na(communes_dates_2005_2022_temperature_final_2005)]<-0


communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 < -20]<-"<-20"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= -20 & communes_dates_2005_2022_temperature_final_2005$value1 < -15]<-"-20_-15"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= -15 & communes_dates_2005_2022_temperature_final_2005$value1 < -10]<-"-15_-10"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= -10 & communes_dates_2005_2022_temperature_final_2005$value1 < -5]<-"-10_-5"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= -5 & communes_dates_2005_2022_temperature_final_2005$value1 < 0]<-"-5_0"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 0 & communes_dates_2005_2022_temperature_final_2005$value1 < 5]<-"0_5"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 5 & communes_dates_2005_2022_temperature_final_2005$value1 < 10]<-"5_10"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 10 & communes_dates_2005_2022_temperature_final_2005$value1 < 15]<-"10_15"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 15 & communes_dates_2005_2022_temperature_final_2005$value1 < 20]<-"15_20"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 20 & communes_dates_2005_2022_temperature_final_2005$value1 < 25]<-"20_25"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 25 & communes_dates_2005_2022_temperature_final_2005$value1 < 28]<-"25_28"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 28 & communes_dates_2005_2022_temperature_final_2005$value1 < 30]<-"28_30"

communes_dates_2005_2022_temperature_final_2005$temperature_bin[communes_dates_2005_2022_temperature_final_2005$value1 >= 30]<-">30"


#test<-filter(communes_dates_2005_2022_temperature_final_2005, is.na(temperature_bin))
#table(communes_dates_2005_2022_temperature_final_2005$temperature_bin)

library(fastDummies)
communes_dates_2005_2022_temperature_final_2005  <- communes_dates_2005_2022_temperature_final_2005  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2005_2022_temperature_final_2005 <- communes_dates_2005_2022_temperature_final_2005 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2005_2022_temperature_final_2005 <- communes_dates_2005_2022_temperature_final_2005 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2005_2022_temperature_final_2005$same_value[is.na(communes_dates_2005_2022_temperature_final_2005$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2005_2022_temperature_final_2005$same_value <- ifelse(communes_dates_2005_2022_temperature_final_2005$temperature_bin != ">30", 0, communes_dates_2005_2022_temperature_final_2005$same_value)



communes_dates_2005_2022_temperature_final_2005$mois<-substring(communes_dates_2005_2022_temperature_final_2005$date,6,7)

communes_dates_2005_2022_temperature_final_2005<-communes_dates_2005_2022_temperature_final_2005[,-c("date","value1","temperature_bin")]

communes_dates_2005_2022_temperature_final_2005<-aggregate(.~COM+mois,communes_dates_2005_2022_temperature_final_2005,sum)



RP_2005_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2005_age_sexe_final_2")

RP_2005_age_sexe_final_2<-RP_2005_age_sexe_final_2[,c(2:15)]

names(RP_2005_age_sexe_final_2)[names(RP_2005_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2005_2022_temperature_final_2005<-left_join(communes_dates_2005_2022_temperature_final_2005,RP_2005_age_sexe_final_2)

#tests<-filter(communes_dates_2005_2022_temperature_final_2005, COM=="01001")
#tests<-filter(communes_dates_2005_2022_temperature_final_2005, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005, !is.na(value_estimated_sum_homme) )

RP_2005_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2005_CSP_final_2")

RP_2005_CSP_final_2<-RP_2005_CSP_final_2[,c(2:12)]
names(RP_2005_CSP_final_2)[names(RP_2005_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2005_CSP_final_2)[names(RP_2005_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2005_2022_temperature_final_2005<-left_join(communes_dates_2005_2022_temperature_final_2005,RP_2005_CSP_final_2)


communes_dates_2005_2022_temperature_final_2005$part_agriculteur<-communes_dates_2005_2022_temperature_final_2005$value_estimated_agriculteur/communes_dates_2005_2022_temperature_final_2005$population_actif_25_54

communes_dates_2005_2022_temperature_final_2005$part_artisan_commercant_chef_entreprise<-communes_dates_2005_2022_temperature_final_2005$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2005_2022_temperature_final_2005$population_actif_25_54

communes_dates_2005_2022_temperature_final_2005$part_cadre<-communes_dates_2005_2022_temperature_final_2005$value_estimated_cadre/communes_dates_2005_2022_temperature_final_2005$population_actif_25_54

communes_dates_2005_2022_temperature_final_2005$part_profession_intermediaire<-communes_dates_2005_2022_temperature_final_2005$value_estimated_profession_intermediaire/communes_dates_2005_2022_temperature_final_2005$population_actif_25_54

communes_dates_2005_2022_temperature_final_2005$part_employe<-communes_dates_2005_2022_temperature_final_2005$value_estimated_employe/communes_dates_2005_2022_temperature_final_2005$population_actif_25_54

communes_dates_2005_2022_temperature_final_2005$part_ouvrier<-communes_dates_2005_2022_temperature_final_2005$value_estimated_ouvrier/communes_dates_2005_2022_temperature_final_2005$population_actif_25_54

communes_dates_2005_2022_temperature_final_2005$part_chomage<-communes_dates_2005_2022_temperature_final_2005$value_estimated_au_chomage/communes_dates_2005_2022_temperature_final_2005$population_actif_25_54






communes_dates_2005_2022_temperature_final_2005$taux_mortalite_homme<-communes_dates_2005_2022_temperature_final_2005$Homme/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_homme

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_femme<-communes_dates_2005_2022_temperature_final_2005$Femme/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_femme


communes_dates_2005_2022_temperature_final_2005$taux_mortalite_0_9<-communes_dates_2005_2022_temperature_final_2005$`0-9`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_0_9_h_f

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_10_19<-communes_dates_2005_2022_temperature_final_2005$`10-19`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_10_19_h_f



communes_dates_2005_2022_temperature_final_2005$taux_mortalite_20_39<-communes_dates_2005_2022_temperature_final_2005$`20-39`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_20_39_h_f




communes_dates_2005_2022_temperature_final_2005$taux_mortalite_40_59<-communes_dates_2005_2022_temperature_final_2005$`40-59`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_40_59_h_f





communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_64<-communes_dates_2005_2022_temperature_final_2005$`60-64`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_60_64_h_f



communes_dates_2005_2022_temperature_final_2005$taux_mortalite_65_69<-communes_dates_2005_2022_temperature_final_2005$`65-69`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_65_69_h_f


communes_dates_2005_2022_temperature_final_2005$taux_mortalite_70_74<-communes_dates_2005_2022_temperature_final_2005$`70-74`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_70_74_h_f


communes_dates_2005_2022_temperature_final_2005$taux_mortalite_75_79<-communes_dates_2005_2022_temperature_final_2005$`75-79`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_75_79_h_f


communes_dates_2005_2022_temperature_final_2005$taux_mortalite_80_plus<-communes_dates_2005_2022_temperature_final_2005$`80+`/communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_80_plus_h_f


#communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_70<-(communes_dates_2005_2022_temperature_final_2005$`60-64`+communes_dates_2005_2022_temperature_final_2005$`65-69`)/(communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_60_64_h_f+communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_65_69_h_f)



communes_dates_2005_2022_temperature_final_2005$mort_total<-communes_dates_2005_2022_temperature_final_2005$Femme+communes_dates_2005_2022_temperature_final_2005$Homme


communes_dates_2005_2022_temperature_final_2005$taux_mortalite_total<-communes_dates_2005_2022_temperature_final_2005$mort_total/communes_dates_2005_2022_temperature_final_2005$value_estimated_population






communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_74<-(communes_dates_2005_2022_temperature_final_2005$`60-64`+communes_dates_2005_2022_temperature_final_2005$`65-69`+communes_dates_2005_2022_temperature_final_2005$`70-74`)/(communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_60_64_h_f+communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_65_69_h_f+communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_70_74_h_f)


communes_dates_2005_2022_temperature_final_2005$taux_mortalite_75_plus<-(communes_dates_2005_2022_temperature_final_2005$`75-79`+communes_dates_2005_2022_temperature_final_2005$`80+`)/(communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_75_79_h_f+communes_dates_2005_2022_temperature_final_2005$value_estimated_sum_80_plus_h_f)










#on enleve les communes avec des population de 0
communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005, communes_dates_2005_2022_temperature_final_2005$value_estimated_population>0)
communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005, communes_dates_2005_2022_temperature_final_2005$population_actif_25_54>0)




communes_dates_2005_2022_temperature_final_2005<- communes_dates_2005_2022_temperature_final_2005[ , !names(communes_dates_2005_2022_temperature_final_2005) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_femme))


communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(part_agriculteur))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(part_cadre))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(part_profession_intermediaire))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(part_employe))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(part_ouvrier))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(part_chomage))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_homme))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_0_9))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_10_19))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_20_39))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_40_59))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_60_64))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_65_69))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_70_74))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_75_79))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_total))



communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_60_74))
communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  !is.infinite(taux_mortalite_75_plus))





#761 valeurs inf

#

#communes_dates_2005_2022_temperature_final_2005<-communes_dates_2005_2022_temperature_final_2005[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  part_agriculteur<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_10_19<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  part_cadre<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  part_profession_intermediaire<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  part_employe<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  part_ouvrier<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  part_chomage<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_homme<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_femme<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_0_9<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_20_39<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_40_59<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_60_64<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_65_69<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_70_74<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_75_79<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_80_plus<=1)

#communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005,  taux_mortalite_total<=1)




communes_dates_2005_2022_temperature_final_2005$taux_mortalite_homme[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_homme>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_femme[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_femme>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_0_9[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_0_9>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_10_19[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_10_19>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_20_39[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_20_39>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_40_59[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_40_59>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_64[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_64>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_65_69[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_65_69>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_70_74[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_70_74>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_75_79[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_75_79>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_80_plus[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_80_plus>1]<-NA   

communes_dates_2005_2022_temperature_final_2005$taux_mortalite_total[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_total>1]<-NA   




communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_74[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_74>1]<-NA   
communes_dates_2005_2022_temperature_final_2005$taux_mortalite_75_plus[communes_dates_2005_2022_temperature_final_2005$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2005_2022_temperature_final_2005$part_agriculteur)

summary(communes_dates_2005_2022_temperature_final_2005$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2005_2022_temperature_final_2005$part_cadre)

summary(communes_dates_2005_2022_temperature_final_2005$part_profession_intermediaire)

summary(communes_dates_2005_2022_temperature_final_2005$part_employe)

summary(communes_dates_2005_2022_temperature_final_2005$part_ouvrier)

summary(communes_dates_2005_2022_temperature_final_2005$part_chomage)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_homme)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_femme)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_0_9)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_10_19)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_20_39)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_40_59)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_60_64)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_65_69)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_70_74)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_75_79)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_80_plus)

summary(communes_dates_2005_2022_temperature_final_2005$taux_mortalite_total)






fwrite(communes_dates_2005_2022_temperature_final_2005,"/données communes années/données mortalité temperature final mois new/communes_dates_2005_temperature_deces_mois.csv")










################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2006_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2006-01-01")
end_date <- as.Date("2006-12-31")

communes_dates_2006_2022_temperature_final_2006 <- communes_dates_2006_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2006_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2006_age_sexe.csv")




communes_dates_2006_2022_temperature_final_2006$date<-as.Date(communes_dates_2006_2022_temperature_final_2006$date)
deces.2006_age_sexe$date<-as.Date(deces.2006_age_sexe$date)






communes_dates_2006_2022_temperature_final_2006<-left_join(communes_dates_2006_2022_temperature_final_2006,deces.2006_age_sexe)

communes_dates_2006_2022_temperature_final_2006[is.na(communes_dates_2006_2022_temperature_final_2006)]<-0


communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 < -20]<-"<-20"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= -20 & communes_dates_2006_2022_temperature_final_2006$value1 < -15]<-"-20_-15"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= -15 & communes_dates_2006_2022_temperature_final_2006$value1 < -10]<-"-15_-10"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= -10 & communes_dates_2006_2022_temperature_final_2006$value1 < -5]<-"-10_-5"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= -5 & communes_dates_2006_2022_temperature_final_2006$value1 < 0]<-"-5_0"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 0 & communes_dates_2006_2022_temperature_final_2006$value1 < 5]<-"0_5"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 5 & communes_dates_2006_2022_temperature_final_2006$value1 < 10]<-"5_10"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 10 & communes_dates_2006_2022_temperature_final_2006$value1 < 15]<-"10_15"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 15 & communes_dates_2006_2022_temperature_final_2006$value1 < 20]<-"15_20"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 20 & communes_dates_2006_2022_temperature_final_2006$value1 < 25]<-"20_25"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 25 & communes_dates_2006_2022_temperature_final_2006$value1 < 28]<-"25_28"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 28 & communes_dates_2006_2022_temperature_final_2006$value1 < 30]<-"28_30"

communes_dates_2006_2022_temperature_final_2006$temperature_bin[communes_dates_2006_2022_temperature_final_2006$value1 >= 30]<-">30"


#test<-filter(communes_dates_2006_2022_temperature_final_2006, is.na(temperature_bin))
#table(communes_dates_2006_2022_temperature_final_2006$temperature_bin)

library(fastDummies)
communes_dates_2006_2022_temperature_final_2006  <- communes_dates_2006_2022_temperature_final_2006  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2006_2022_temperature_final_2006 <- communes_dates_2006_2022_temperature_final_2006 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2006_2022_temperature_final_2006 <- communes_dates_2006_2022_temperature_final_2006 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2006_2022_temperature_final_2006$same_value[is.na(communes_dates_2006_2022_temperature_final_2006$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2006_2022_temperature_final_2006$same_value <- ifelse(communes_dates_2006_2022_temperature_final_2006$temperature_bin != ">30", 0, communes_dates_2006_2022_temperature_final_2006$same_value)



communes_dates_2006_2022_temperature_final_2006$mois<-substring(communes_dates_2006_2022_temperature_final_2006$date,6,7)

communes_dates_2006_2022_temperature_final_2006<-communes_dates_2006_2022_temperature_final_2006[,-c("date","value1","temperature_bin")]

communes_dates_2006_2022_temperature_final_2006<-aggregate(.~COM+mois,communes_dates_2006_2022_temperature_final_2006,sum)



RP_2006_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2006_age_sexe_final_2")

RP_2006_age_sexe_final_2<-RP_2006_age_sexe_final_2[,c(2:15)]

names(RP_2006_age_sexe_final_2)[names(RP_2006_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2006_2022_temperature_final_2006<-left_join(communes_dates_2006_2022_temperature_final_2006,RP_2006_age_sexe_final_2)

#tests<-filter(communes_dates_2006_2022_temperature_final_2006, COM=="01001")
#tests<-filter(communes_dates_2006_2022_temperature_final_2006, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006, !is.na(value_estimated_sum_homme) )

RP_2006_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2006_CSP_final_2")

RP_2006_CSP_final_2<-RP_2006_CSP_final_2[,c(2:12)]
names(RP_2006_CSP_final_2)[names(RP_2006_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2006_CSP_final_2)[names(RP_2006_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2006_2022_temperature_final_2006<-left_join(communes_dates_2006_2022_temperature_final_2006,RP_2006_CSP_final_2)


communes_dates_2006_2022_temperature_final_2006$part_agriculteur<-communes_dates_2006_2022_temperature_final_2006$value_estimated_agriculteur/communes_dates_2006_2022_temperature_final_2006$population_actif_25_54

communes_dates_2006_2022_temperature_final_2006$part_artisan_commercant_chef_entreprise<-communes_dates_2006_2022_temperature_final_2006$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2006_2022_temperature_final_2006$population_actif_25_54

communes_dates_2006_2022_temperature_final_2006$part_cadre<-communes_dates_2006_2022_temperature_final_2006$value_estimated_cadre/communes_dates_2006_2022_temperature_final_2006$population_actif_25_54

communes_dates_2006_2022_temperature_final_2006$part_profession_intermediaire<-communes_dates_2006_2022_temperature_final_2006$value_estimated_profession_intermediaire/communes_dates_2006_2022_temperature_final_2006$population_actif_25_54

communes_dates_2006_2022_temperature_final_2006$part_employe<-communes_dates_2006_2022_temperature_final_2006$value_estimated_employe/communes_dates_2006_2022_temperature_final_2006$population_actif_25_54

communes_dates_2006_2022_temperature_final_2006$part_ouvrier<-communes_dates_2006_2022_temperature_final_2006$value_estimated_ouvrier/communes_dates_2006_2022_temperature_final_2006$population_actif_25_54

communes_dates_2006_2022_temperature_final_2006$part_chomage<-communes_dates_2006_2022_temperature_final_2006$value_estimated_au_chomage/communes_dates_2006_2022_temperature_final_2006$population_actif_25_54






communes_dates_2006_2022_temperature_final_2006$taux_mortalite_homme<-communes_dates_2006_2022_temperature_final_2006$Homme/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_homme

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_femme<-communes_dates_2006_2022_temperature_final_2006$Femme/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_femme


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_0_9<-communes_dates_2006_2022_temperature_final_2006$`0-9`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_0_9_h_f

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_10_19<-communes_dates_2006_2022_temperature_final_2006$`10-19`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_10_19_h_f



communes_dates_2006_2022_temperature_final_2006$taux_mortalite_20_39<-communes_dates_2006_2022_temperature_final_2006$`20-39`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_20_39_h_f




communes_dates_2006_2022_temperature_final_2006$taux_mortalite_40_59<-communes_dates_2006_2022_temperature_final_2006$`40-59`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_40_59_h_f





communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_64<-communes_dates_2006_2022_temperature_final_2006$`60-64`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_60_64_h_f



communes_dates_2006_2022_temperature_final_2006$taux_mortalite_65_69<-communes_dates_2006_2022_temperature_final_2006$`65-69`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_65_69_h_f


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_70_74<-communes_dates_2006_2022_temperature_final_2006$`70-74`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_70_74_h_f


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_75_79<-communes_dates_2006_2022_temperature_final_2006$`75-79`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_75_79_h_f


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_80_plus<-communes_dates_2006_2022_temperature_final_2006$`80+`/communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_80_plus_h_f


#communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_70<-(communes_dates_2006_2022_temperature_final_2006$`60-64`+communes_dates_2006_2022_temperature_final_2006$`65-69`)/(communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_60_64_h_f+communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_65_69_h_f)



communes_dates_2006_2022_temperature_final_2006$mort_total<-communes_dates_2006_2022_temperature_final_2006$Femme+communes_dates_2006_2022_temperature_final_2006$Homme


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_total<-communes_dates_2006_2022_temperature_final_2006$mort_total/communes_dates_2006_2022_temperature_final_2006$value_estimated_population







communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_74<-(communes_dates_2006_2022_temperature_final_2006$`60-64`+communes_dates_2006_2022_temperature_final_2006$`65-69`+communes_dates_2006_2022_temperature_final_2006$`70-74`)/(communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_60_64_h_f+communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_65_69_h_f+communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_70_74_h_f)


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_75_plus<-(communes_dates_2006_2022_temperature_final_2006$`75-79`+communes_dates_2006_2022_temperature_final_2006$`80+`)/(communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_75_79_h_f+communes_dates_2006_2022_temperature_final_2006$value_estimated_sum_80_plus_h_f)









#on enleve les communes avec des population de 0
communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006, communes_dates_2006_2022_temperature_final_2006$value_estimated_population>0)
communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006, communes_dates_2006_2022_temperature_final_2006$population_actif_25_54>0)




communes_dates_2006_2022_temperature_final_2006<- communes_dates_2006_2022_temperature_final_2006[ , !names(communes_dates_2006_2022_temperature_final_2006) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_femme))


communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(part_agriculteur))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(part_cadre))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(part_profession_intermediaire))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(part_employe))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(part_ouvrier))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(part_chomage))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_homme))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_0_9))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_10_19))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_20_39))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_40_59))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_60_64))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_65_69))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_70_74))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_75_79))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_total))




communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_60_74))
communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  !is.infinite(taux_mortalite_75_plus))





#761 valeurs inf

#

#communes_dates_2006_2022_temperature_final_2006<-communes_dates_2006_2022_temperature_final_2006[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  part_agriculteur<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_10_19<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  part_cadre<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  part_profession_intermediaire<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  part_employe<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  part_ouvrier<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  part_chomage<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_homme<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_femme<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_0_9<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_20_39<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_40_59<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_60_64<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_65_69<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_70_74<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_75_79<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_80_plus<=1)

#communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006,  taux_mortalite_total<=1)




communes_dates_2006_2022_temperature_final_2006$taux_mortalite_homme[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_homme>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_femme[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_femme>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_0_9[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_0_9>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_10_19[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_10_19>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_20_39[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_20_39>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_40_59[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_40_59>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_64[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_64>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_65_69[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_65_69>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_70_74[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_70_74>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_75_79[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_75_79>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_80_plus[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_80_plus>1]<-NA   

communes_dates_2006_2022_temperature_final_2006$taux_mortalite_total[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_total>1]<-NA   


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_74[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_74>1]<-NA   
communes_dates_2006_2022_temperature_final_2006$taux_mortalite_75_plus[communes_dates_2006_2022_temperature_final_2006$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2006_2022_temperature_final_2006$part_agriculteur)

summary(communes_dates_2006_2022_temperature_final_2006$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2006_2022_temperature_final_2006$part_cadre)

summary(communes_dates_2006_2022_temperature_final_2006$part_profession_intermediaire)

summary(communes_dates_2006_2022_temperature_final_2006$part_employe)

summary(communes_dates_2006_2022_temperature_final_2006$part_ouvrier)

summary(communes_dates_2006_2022_temperature_final_2006$part_chomage)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_homme)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_femme)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_0_9)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_10_19)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_20_39)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_40_59)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_60_64)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_65_69)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_70_74)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_75_79)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_80_plus)

summary(communes_dates_2006_2022_temperature_final_2006$taux_mortalite_total)






fwrite(communes_dates_2006_2022_temperature_final_2006,"/données communes années/données mortalité temperature final mois new/communes_dates_2006_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2007_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2007-01-01")
end_date <- as.Date("2007-12-31")

communes_dates_2007_2022_temperature_final_2007 <- communes_dates_2007_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2007_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2007_age_sexe.csv")




communes_dates_2007_2022_temperature_final_2007$date<-as.Date(communes_dates_2007_2022_temperature_final_2007$date)
deces.2007_age_sexe$date<-as.Date(deces.2007_age_sexe$date)







communes_dates_2007_2022_temperature_final_2007<-left_join(communes_dates_2007_2022_temperature_final_2007,deces.2007_age_sexe)

communes_dates_2007_2022_temperature_final_2007[is.na(communes_dates_2007_2022_temperature_final_2007)]<-0


communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 < -20]<-"<-20"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= -20 & communes_dates_2007_2022_temperature_final_2007$value1 < -15]<-"-20_-15"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= -15 & communes_dates_2007_2022_temperature_final_2007$value1 < -10]<-"-15_-10"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= -10 & communes_dates_2007_2022_temperature_final_2007$value1 < -5]<-"-10_-5"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= -5 & communes_dates_2007_2022_temperature_final_2007$value1 < 0]<-"-5_0"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 0 & communes_dates_2007_2022_temperature_final_2007$value1 < 5]<-"0_5"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 5 & communes_dates_2007_2022_temperature_final_2007$value1 < 10]<-"5_10"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 10 & communes_dates_2007_2022_temperature_final_2007$value1 < 15]<-"10_15"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 15 & communes_dates_2007_2022_temperature_final_2007$value1 < 20]<-"15_20"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 20 & communes_dates_2007_2022_temperature_final_2007$value1 < 25]<-"20_25"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 25 & communes_dates_2007_2022_temperature_final_2007$value1 < 28]<-"25_28"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 28 & communes_dates_2007_2022_temperature_final_2007$value1 < 30]<-"28_30"

communes_dates_2007_2022_temperature_final_2007$temperature_bin[communes_dates_2007_2022_temperature_final_2007$value1 >= 30]<-">30"


#test<-filter(communes_dates_2007_2022_temperature_final_2007, is.na(temperature_bin))
#table(communes_dates_2007_2022_temperature_final_2007$temperature_bin)

library(fastDummies)
communes_dates_2007_2022_temperature_final_2007  <- communes_dates_2007_2022_temperature_final_2007  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2007_2022_temperature_final_2007 <- communes_dates_2007_2022_temperature_final_2007 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2007_2022_temperature_final_2007 <- communes_dates_2007_2022_temperature_final_2007 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2007_2022_temperature_final_2007$same_value[is.na(communes_dates_2007_2022_temperature_final_2007$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2007_2022_temperature_final_2007$same_value <- ifelse(communes_dates_2007_2022_temperature_final_2007$temperature_bin != ">30", 0, communes_dates_2007_2022_temperature_final_2007$same_value)



communes_dates_2007_2022_temperature_final_2007$mois<-substring(communes_dates_2007_2022_temperature_final_2007$date,6,7)

communes_dates_2007_2022_temperature_final_2007<-communes_dates_2007_2022_temperature_final_2007[,-c("date","value1","temperature_bin")]

communes_dates_2007_2022_temperature_final_2007<-aggregate(.~COM+mois,communes_dates_2007_2022_temperature_final_2007,sum)



RP_2007_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2007_age_sexe_final_2")

RP_2007_age_sexe_final_2<-RP_2007_age_sexe_final_2[,c(2:15)]

names(RP_2007_age_sexe_final_2)[names(RP_2007_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2007_2022_temperature_final_2007<-left_join(communes_dates_2007_2022_temperature_final_2007,RP_2007_age_sexe_final_2)

#tests<-filter(communes_dates_2007_2022_temperature_final_2007, COM=="01001")
#tests<-filter(communes_dates_2007_2022_temperature_final_2007, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007, !is.na(value_estimated_sum_homme) )

RP_2007_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2007_CSP_final_2")

RP_2007_CSP_final_2<-RP_2007_CSP_final_2[,c(2:12)]
names(RP_2007_CSP_final_2)[names(RP_2007_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2007_CSP_final_2)[names(RP_2007_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2007_2022_temperature_final_2007<-left_join(communes_dates_2007_2022_temperature_final_2007,RP_2007_CSP_final_2)


communes_dates_2007_2022_temperature_final_2007$part_agriculteur<-communes_dates_2007_2022_temperature_final_2007$value_estimated_agriculteur/communes_dates_2007_2022_temperature_final_2007$population_actif_25_54

communes_dates_2007_2022_temperature_final_2007$part_artisan_commercant_chef_entreprise<-communes_dates_2007_2022_temperature_final_2007$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2007_2022_temperature_final_2007$population_actif_25_54

communes_dates_2007_2022_temperature_final_2007$part_cadre<-communes_dates_2007_2022_temperature_final_2007$value_estimated_cadre/communes_dates_2007_2022_temperature_final_2007$population_actif_25_54

communes_dates_2007_2022_temperature_final_2007$part_profession_intermediaire<-communes_dates_2007_2022_temperature_final_2007$value_estimated_profession_intermediaire/communes_dates_2007_2022_temperature_final_2007$population_actif_25_54

communes_dates_2007_2022_temperature_final_2007$part_employe<-communes_dates_2007_2022_temperature_final_2007$value_estimated_employe/communes_dates_2007_2022_temperature_final_2007$population_actif_25_54

communes_dates_2007_2022_temperature_final_2007$part_ouvrier<-communes_dates_2007_2022_temperature_final_2007$value_estimated_ouvrier/communes_dates_2007_2022_temperature_final_2007$population_actif_25_54

communes_dates_2007_2022_temperature_final_2007$part_chomage<-communes_dates_2007_2022_temperature_final_2007$value_estimated_au_chomage/communes_dates_2007_2022_temperature_final_2007$population_actif_25_54






communes_dates_2007_2022_temperature_final_2007$taux_mortalite_homme<-communes_dates_2007_2022_temperature_final_2007$Homme/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_homme

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_femme<-communes_dates_2007_2022_temperature_final_2007$Femme/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_femme


communes_dates_2007_2022_temperature_final_2007$taux_mortalite_0_9<-communes_dates_2007_2022_temperature_final_2007$`0-9`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_0_9_h_f

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_10_19<-communes_dates_2007_2022_temperature_final_2007$`10-19`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_10_19_h_f



communes_dates_2007_2022_temperature_final_2007$taux_mortalite_20_39<-communes_dates_2007_2022_temperature_final_2007$`20-39`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_20_39_h_f




communes_dates_2007_2022_temperature_final_2007$taux_mortalite_40_59<-communes_dates_2007_2022_temperature_final_2007$`40-59`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_40_59_h_f





communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_64<-communes_dates_2007_2022_temperature_final_2007$`60-64`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_60_64_h_f



communes_dates_2007_2022_temperature_final_2007$taux_mortalite_65_69<-communes_dates_2007_2022_temperature_final_2007$`65-69`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_65_69_h_f


communes_dates_2007_2022_temperature_final_2007$taux_mortalite_70_74<-communes_dates_2007_2022_temperature_final_2007$`70-74`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_70_74_h_f


communes_dates_2007_2022_temperature_final_2007$taux_mortalite_75_79<-communes_dates_2007_2022_temperature_final_2007$`75-79`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_75_79_h_f


communes_dates_2007_2022_temperature_final_2007$taux_mortalite_80_plus<-communes_dates_2007_2022_temperature_final_2007$`80+`/communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_80_plus_h_f


#communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_70<-(communes_dates_2007_2022_temperature_final_2007$`60-64`+communes_dates_2007_2022_temperature_final_2007$`65-69`)/(communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_60_64_h_f+communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_65_69_h_f)



communes_dates_2007_2022_temperature_final_2007$mort_total<-communes_dates_2007_2022_temperature_final_2007$Femme+communes_dates_2007_2022_temperature_final_2007$Homme


communes_dates_2007_2022_temperature_final_2007$taux_mortalite_total<-communes_dates_2007_2022_temperature_final_2007$mort_total/communes_dates_2007_2022_temperature_final_2007$value_estimated_population







communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_74<-(communes_dates_2007_2022_temperature_final_2007$`60-64`+communes_dates_2007_2022_temperature_final_2007$`65-69`+communes_dates_2007_2022_temperature_final_2007$`70-74`)/(communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_60_64_h_f+communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_65_69_h_f+communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_70_74_h_f)


communes_dates_2007_2022_temperature_final_2007$taux_mortalite_75_plus<-(communes_dates_2007_2022_temperature_final_2007$`75-79`+communes_dates_2007_2022_temperature_final_2007$`80+`)/(communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_75_79_h_f+communes_dates_2007_2022_temperature_final_2007$value_estimated_sum_80_plus_h_f)








#on enleve les communes avec des population de 0
communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007, communes_dates_2007_2022_temperature_final_2007$value_estimated_population>0)
communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007, communes_dates_2007_2022_temperature_final_2007$population_actif_25_54>0)




communes_dates_2007_2022_temperature_final_2007<- communes_dates_2007_2022_temperature_final_2007[ , !names(communes_dates_2007_2022_temperature_final_2007) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_femme))


communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(part_agriculteur))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(part_cadre))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(part_profession_intermediaire))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(part_employe))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(part_ouvrier))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(part_chomage))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_homme))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_0_9))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_10_19))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_20_39))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_40_59))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_60_64))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_65_69))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_70_74))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_75_79))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_total))



communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_60_74))
communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  !is.infinite(taux_mortalite_75_plus))



#761 valeurs inf

#

#communes_dates_2007_2022_temperature_final_2007<-communes_dates_2007_2022_temperature_final_2007[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  part_agriculteur<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_10_19<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  part_cadre<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  part_profession_intermediaire<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  part_employe<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  part_ouvrier<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  part_chomage<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_homme<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_femme<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_0_9<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_20_39<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_40_59<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_60_64<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_65_69<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_70_74<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_75_79<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_80_plus<=1)

#communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007,  taux_mortalite_total<=1)




communes_dates_2007_2022_temperature_final_2007$taux_mortalite_homme[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_homme>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_femme[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_femme>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_0_9[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_0_9>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_10_19[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_10_19>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_20_39[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_20_39>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_40_59[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_40_59>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_64[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_64>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_65_69[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_65_69>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_70_74[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_70_74>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_75_79[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_75_79>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_80_plus[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_80_plus>1]<-NA   

communes_dates_2007_2022_temperature_final_2007$taux_mortalite_total[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_total>1]<-NA   




communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_74[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_74>1]<-NA   
communes_dates_2007_2022_temperature_final_2007$taux_mortalite_75_plus[communes_dates_2007_2022_temperature_final_2007$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2007_2022_temperature_final_2007$part_agriculteur)

summary(communes_dates_2007_2022_temperature_final_2007$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2007_2022_temperature_final_2007$part_cadre)

summary(communes_dates_2007_2022_temperature_final_2007$part_profession_intermediaire)

summary(communes_dates_2007_2022_temperature_final_2007$part_employe)

summary(communes_dates_2007_2022_temperature_final_2007$part_ouvrier)

summary(communes_dates_2007_2022_temperature_final_2007$part_chomage)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_homme)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_femme)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_0_9)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_10_19)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_20_39)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_40_59)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_60_64)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_65_69)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_70_74)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_75_79)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_80_plus)

summary(communes_dates_2007_2022_temperature_final_2007$taux_mortalite_total)






fwrite(communes_dates_2007_2022_temperature_final_2007,"/données communes années/données mortalité temperature final mois new/communes_dates_2007_temperature_deces_mois.csv")










################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2008_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2008-01-01")
end_date <- as.Date("2008-12-31")

communes_dates_2008_2022_temperature_final_2008 <- communes_dates_2008_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2008_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2008_age_sexe.csv")




communes_dates_2008_2022_temperature_final_2008$date<-as.Date(communes_dates_2008_2022_temperature_final_2008$date)
deces.2008_age_sexe$date<-as.Date(deces.2008_age_sexe$date)





communes_dates_2008_2022_temperature_final_2008<-left_join(communes_dates_2008_2022_temperature_final_2008,deces.2008_age_sexe)

communes_dates_2008_2022_temperature_final_2008[is.na(communes_dates_2008_2022_temperature_final_2008)]<-0


communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 < -20]<-"<-20"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= -20 & communes_dates_2008_2022_temperature_final_2008$value1 < -15]<-"-20_-15"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= -15 & communes_dates_2008_2022_temperature_final_2008$value1 < -10]<-"-15_-10"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= -10 & communes_dates_2008_2022_temperature_final_2008$value1 < -5]<-"-10_-5"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= -5 & communes_dates_2008_2022_temperature_final_2008$value1 < 0]<-"-5_0"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 0 & communes_dates_2008_2022_temperature_final_2008$value1 < 5]<-"0_5"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 5 & communes_dates_2008_2022_temperature_final_2008$value1 < 10]<-"5_10"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 10 & communes_dates_2008_2022_temperature_final_2008$value1 < 15]<-"10_15"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 15 & communes_dates_2008_2022_temperature_final_2008$value1 < 20]<-"15_20"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 20 & communes_dates_2008_2022_temperature_final_2008$value1 < 25]<-"20_25"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 25 & communes_dates_2008_2022_temperature_final_2008$value1 < 28]<-"25_28"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 28 & communes_dates_2008_2022_temperature_final_2008$value1 < 30]<-"28_30"

communes_dates_2008_2022_temperature_final_2008$temperature_bin[communes_dates_2008_2022_temperature_final_2008$value1 >= 30]<-">30"


#test<-filter(communes_dates_2008_2022_temperature_final_2008, is.na(temperature_bin))
#table(communes_dates_2008_2022_temperature_final_2008$temperature_bin)

library(fastDummies)
communes_dates_2008_2022_temperature_final_2008  <- communes_dates_2008_2022_temperature_final_2008  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2008_2022_temperature_final_2008 <- communes_dates_2008_2022_temperature_final_2008 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2008_2022_temperature_final_2008 <- communes_dates_2008_2022_temperature_final_2008 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2008_2022_temperature_final_2008$same_value[is.na(communes_dates_2008_2022_temperature_final_2008$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2008_2022_temperature_final_2008$same_value <- ifelse(communes_dates_2008_2022_temperature_final_2008$temperature_bin != ">30", 0, communes_dates_2008_2022_temperature_final_2008$same_value)



communes_dates_2008_2022_temperature_final_2008$mois<-substring(communes_dates_2008_2022_temperature_final_2008$date,6,7)

communes_dates_2008_2022_temperature_final_2008<-communes_dates_2008_2022_temperature_final_2008[,-c("date","value1","temperature_bin")]

communes_dates_2008_2022_temperature_final_2008<-aggregate(.~COM+mois,communes_dates_2008_2022_temperature_final_2008,sum)



RP_2008_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final_2")

RP_2008_age_sexe_final_2<-RP_2008_age_sexe_final_2[,c(2:15)]

names(RP_2008_age_sexe_final_2)[names(RP_2008_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2008_2022_temperature_final_2008<-left_join(communes_dates_2008_2022_temperature_final_2008,RP_2008_age_sexe_final_2)

#tests<-filter(communes_dates_2008_2022_temperature_final_2008, COM=="01001")
#tests<-filter(communes_dates_2008_2022_temperature_final_2008, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008, !is.na(value_estimated_sum_homme) )

RP_2008_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2008_CSP_final_2")

RP_2008_CSP_final_2<-RP_2008_CSP_final_2[,c(2:12)]
names(RP_2008_CSP_final_2)[names(RP_2008_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2008_CSP_final_2)[names(RP_2008_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2008_2022_temperature_final_2008<-left_join(communes_dates_2008_2022_temperature_final_2008,RP_2008_CSP_final_2)


communes_dates_2008_2022_temperature_final_2008$part_agriculteur<-communes_dates_2008_2022_temperature_final_2008$value_estimated_agriculteur/communes_dates_2008_2022_temperature_final_2008$population_actif_25_54

communes_dates_2008_2022_temperature_final_2008$part_artisan_commercant_chef_entreprise<-communes_dates_2008_2022_temperature_final_2008$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2008_2022_temperature_final_2008$population_actif_25_54

communes_dates_2008_2022_temperature_final_2008$part_cadre<-communes_dates_2008_2022_temperature_final_2008$value_estimated_cadre/communes_dates_2008_2022_temperature_final_2008$population_actif_25_54

communes_dates_2008_2022_temperature_final_2008$part_profession_intermediaire<-communes_dates_2008_2022_temperature_final_2008$value_estimated_profession_intermediaire/communes_dates_2008_2022_temperature_final_2008$population_actif_25_54

communes_dates_2008_2022_temperature_final_2008$part_employe<-communes_dates_2008_2022_temperature_final_2008$value_estimated_employe/communes_dates_2008_2022_temperature_final_2008$population_actif_25_54

communes_dates_2008_2022_temperature_final_2008$part_ouvrier<-communes_dates_2008_2022_temperature_final_2008$value_estimated_ouvrier/communes_dates_2008_2022_temperature_final_2008$population_actif_25_54

communes_dates_2008_2022_temperature_final_2008$part_chomage<-communes_dates_2008_2022_temperature_final_2008$value_estimated_au_chomage/communes_dates_2008_2022_temperature_final_2008$population_actif_25_54






communes_dates_2008_2022_temperature_final_2008$taux_mortalite_homme<-communes_dates_2008_2022_temperature_final_2008$Homme/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_homme

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_femme<-communes_dates_2008_2022_temperature_final_2008$Femme/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_femme


communes_dates_2008_2022_temperature_final_2008$taux_mortalite_0_9<-communes_dates_2008_2022_temperature_final_2008$`0-9`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_0_9_h_f

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_10_19<-communes_dates_2008_2022_temperature_final_2008$`10-19`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_10_19_h_f



communes_dates_2008_2022_temperature_final_2008$taux_mortalite_20_39<-communes_dates_2008_2022_temperature_final_2008$`20-39`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_20_39_h_f




communes_dates_2008_2022_temperature_final_2008$taux_mortalite_40_59<-communes_dates_2008_2022_temperature_final_2008$`40-59`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_40_59_h_f





communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_64<-communes_dates_2008_2022_temperature_final_2008$`60-64`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_60_64_h_f



communes_dates_2008_2022_temperature_final_2008$taux_mortalite_65_69<-communes_dates_2008_2022_temperature_final_2008$`65-69`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_65_69_h_f


communes_dates_2008_2022_temperature_final_2008$taux_mortalite_70_74<-communes_dates_2008_2022_temperature_final_2008$`70-74`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_70_74_h_f


communes_dates_2008_2022_temperature_final_2008$taux_mortalite_75_79<-communes_dates_2008_2022_temperature_final_2008$`75-79`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_75_79_h_f


communes_dates_2008_2022_temperature_final_2008$taux_mortalite_80_plus<-communes_dates_2008_2022_temperature_final_2008$`80+`/communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_80_plus_h_f


#communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_70<-(communes_dates_2008_2022_temperature_final_2008$`60-64`+communes_dates_2008_2022_temperature_final_2008$`65-69`)/(communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_60_64_h_f+communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_65_69_h_f)



communes_dates_2008_2022_temperature_final_2008$mort_total<-communes_dates_2008_2022_temperature_final_2008$Femme+communes_dates_2008_2022_temperature_final_2008$Homme


communes_dates_2008_2022_temperature_final_2008$taux_mortalite_total<-communes_dates_2008_2022_temperature_final_2008$mort_total/communes_dates_2008_2022_temperature_final_2008$value_estimated_population








communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_74<-(communes_dates_2008_2022_temperature_final_2008$`60-64`+communes_dates_2008_2022_temperature_final_2008$`65-69`+communes_dates_2008_2022_temperature_final_2008$`70-74`)/(communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_60_64_h_f+communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_65_69_h_f+communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_70_74_h_f)


communes_dates_2008_2022_temperature_final_2008$taux_mortalite_75_plus<-(communes_dates_2008_2022_temperature_final_2008$`75-79`+communes_dates_2008_2022_temperature_final_2008$`80+`)/(communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_75_79_h_f+communes_dates_2008_2022_temperature_final_2008$value_estimated_sum_80_plus_h_f)








#on enleve les communes avec des population de 0
communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008, communes_dates_2008_2022_temperature_final_2008$value_estimated_population>0)
communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008, communes_dates_2008_2022_temperature_final_2008$population_actif_25_54>0)




communes_dates_2008_2022_temperature_final_2008<- communes_dates_2008_2022_temperature_final_2008[ , !names(communes_dates_2008_2022_temperature_final_2008) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_femme))


communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(part_agriculteur))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(part_cadre))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(part_profession_intermediaire))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(part_employe))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(part_ouvrier))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(part_chomage))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_homme))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_0_9))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_10_19))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_20_39))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_40_59))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_60_64))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_65_69))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_70_74))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_75_79))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_total))



communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_60_74))
communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  !is.infinite(taux_mortalite_75_plus))





#761 valeurs inf

#

#communes_dates_2008_2022_temperature_final_2008<-communes_dates_2008_2022_temperature_final_2008[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  part_agriculteur<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_10_19<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  part_cadre<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  part_profession_intermediaire<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  part_employe<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  part_ouvrier<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  part_chomage<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_homme<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_femme<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_0_9<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_20_39<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_40_59<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_60_64<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_65_69<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_70_74<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_75_79<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_80_plus<=1)

#communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008,  taux_mortalite_total<=1)




communes_dates_2008_2022_temperature_final_2008$taux_mortalite_homme[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_homme>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_femme[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_femme>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_0_9[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_0_9>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_10_19[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_10_19>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_20_39[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_20_39>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_40_59[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_40_59>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_64[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_64>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_65_69[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_65_69>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_70_74[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_70_74>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_75_79[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_75_79>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_80_plus[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_80_plus>1]<-NA   

communes_dates_2008_2022_temperature_final_2008$taux_mortalite_total[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_total>1]<-NA   



communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_74[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_74>1]<-NA   
communes_dates_2008_2022_temperature_final_2008$taux_mortalite_75_plus[communes_dates_2008_2022_temperature_final_2008$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2008_2022_temperature_final_2008$part_agriculteur)

summary(communes_dates_2008_2022_temperature_final_2008$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2008_2022_temperature_final_2008$part_cadre)

summary(communes_dates_2008_2022_temperature_final_2008$part_profession_intermediaire)

summary(communes_dates_2008_2022_temperature_final_2008$part_employe)

summary(communes_dates_2008_2022_temperature_final_2008$part_ouvrier)

summary(communes_dates_2008_2022_temperature_final_2008$part_chomage)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_homme)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_femme)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_0_9)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_10_19)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_20_39)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_40_59)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_60_64)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_65_69)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_70_74)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_75_79)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_80_plus)

summary(communes_dates_2008_2022_temperature_final_2008$taux_mortalite_total)






fwrite(communes_dates_2008_2022_temperature_final_2008,"/données communes années/données mortalité temperature final mois new/communes_dates_2008_temperature_deces_mois.csv")










################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2009_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2009-01-01")
end_date <- as.Date("2009-12-31")

communes_dates_2009_2022_temperature_final_2009 <- communes_dates_2009_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2009_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2009_age_sexe.csv")




communes_dates_2009_2022_temperature_final_2009$date<-as.Date(communes_dates_2009_2022_temperature_final_2009$date)
deces.2009_age_sexe$date<-as.Date(deces.2009_age_sexe$date)






communes_dates_2009_2022_temperature_final_2009<-left_join(communes_dates_2009_2022_temperature_final_2009,deces.2009_age_sexe)

communes_dates_2009_2022_temperature_final_2009[is.na(communes_dates_2009_2022_temperature_final_2009)]<-0


communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 < -20]<-"<-20"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= -20 & communes_dates_2009_2022_temperature_final_2009$value1 < -15]<-"-20_-15"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= -15 & communes_dates_2009_2022_temperature_final_2009$value1 < -10]<-"-15_-10"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= -10 & communes_dates_2009_2022_temperature_final_2009$value1 < -5]<-"-10_-5"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= -5 & communes_dates_2009_2022_temperature_final_2009$value1 < 0]<-"-5_0"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 0 & communes_dates_2009_2022_temperature_final_2009$value1 < 5]<-"0_5"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 5 & communes_dates_2009_2022_temperature_final_2009$value1 < 10]<-"5_10"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 10 & communes_dates_2009_2022_temperature_final_2009$value1 < 15]<-"10_15"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 15 & communes_dates_2009_2022_temperature_final_2009$value1 < 20]<-"15_20"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 20 & communes_dates_2009_2022_temperature_final_2009$value1 < 25]<-"20_25"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 25 & communes_dates_2009_2022_temperature_final_2009$value1 < 28]<-"25_28"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 28 & communes_dates_2009_2022_temperature_final_2009$value1 < 30]<-"28_30"

communes_dates_2009_2022_temperature_final_2009$temperature_bin[communes_dates_2009_2022_temperature_final_2009$value1 >= 30]<-">30"


#test<-filter(communes_dates_2009_2022_temperature_final_2009, is.na(temperature_bin))
#table(communes_dates_2009_2022_temperature_final_2009$temperature_bin)

library(fastDummies)
communes_dates_2009_2022_temperature_final_2009  <- communes_dates_2009_2022_temperature_final_2009  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2009_2022_temperature_final_2009 <- communes_dates_2009_2022_temperature_final_2009 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2009_2022_temperature_final_2009 <- communes_dates_2009_2022_temperature_final_2009 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2009_2022_temperature_final_2009$same_value[is.na(communes_dates_2009_2022_temperature_final_2009$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2009_2022_temperature_final_2009$same_value <- ifelse(communes_dates_2009_2022_temperature_final_2009$temperature_bin != ">30", 0, communes_dates_2009_2022_temperature_final_2009$same_value)



communes_dates_2009_2022_temperature_final_2009$mois<-substring(communes_dates_2009_2022_temperature_final_2009$date,6,7)

communes_dates_2009_2022_temperature_final_2009<-communes_dates_2009_2022_temperature_final_2009[,-c("date","value1","temperature_bin")]

communes_dates_2009_2022_temperature_final_2009<-aggregate(.~COM+mois,communes_dates_2009_2022_temperature_final_2009,sum)



RP_2009_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2009_age_sexe_final_2")

RP_2009_age_sexe_final_2<-RP_2009_age_sexe_final_2[,c(2:15)]

names(RP_2009_age_sexe_final_2)[names(RP_2009_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2009_2022_temperature_final_2009<-left_join(communes_dates_2009_2022_temperature_final_2009,RP_2009_age_sexe_final_2)

#tests<-filter(communes_dates_2009_2022_temperature_final_2009, COM=="01001")
#tests<-filter(communes_dates_2009_2022_temperature_final_2009, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009, !is.na(value_estimated_sum_homme) )

RP_2009_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2009_CSP_final_2")

RP_2009_CSP_final_2<-RP_2009_CSP_final_2[,c(2:12)]
names(RP_2009_CSP_final_2)[names(RP_2009_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2009_CSP_final_2)[names(RP_2009_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2009_2022_temperature_final_2009<-left_join(communes_dates_2009_2022_temperature_final_2009,RP_2009_CSP_final_2)


communes_dates_2009_2022_temperature_final_2009$part_agriculteur<-communes_dates_2009_2022_temperature_final_2009$value_estimated_agriculteur/communes_dates_2009_2022_temperature_final_2009$population_actif_25_54

communes_dates_2009_2022_temperature_final_2009$part_artisan_commercant_chef_entreprise<-communes_dates_2009_2022_temperature_final_2009$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2009_2022_temperature_final_2009$population_actif_25_54

communes_dates_2009_2022_temperature_final_2009$part_cadre<-communes_dates_2009_2022_temperature_final_2009$value_estimated_cadre/communes_dates_2009_2022_temperature_final_2009$population_actif_25_54

communes_dates_2009_2022_temperature_final_2009$part_profession_intermediaire<-communes_dates_2009_2022_temperature_final_2009$value_estimated_profession_intermediaire/communes_dates_2009_2022_temperature_final_2009$population_actif_25_54

communes_dates_2009_2022_temperature_final_2009$part_employe<-communes_dates_2009_2022_temperature_final_2009$value_estimated_employe/communes_dates_2009_2022_temperature_final_2009$population_actif_25_54

communes_dates_2009_2022_temperature_final_2009$part_ouvrier<-communes_dates_2009_2022_temperature_final_2009$value_estimated_ouvrier/communes_dates_2009_2022_temperature_final_2009$population_actif_25_54

communes_dates_2009_2022_temperature_final_2009$part_chomage<-communes_dates_2009_2022_temperature_final_2009$value_estimated_au_chomage/communes_dates_2009_2022_temperature_final_2009$population_actif_25_54






communes_dates_2009_2022_temperature_final_2009$taux_mortalite_homme<-communes_dates_2009_2022_temperature_final_2009$Homme/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_homme

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_femme<-communes_dates_2009_2022_temperature_final_2009$Femme/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_femme


communes_dates_2009_2022_temperature_final_2009$taux_mortalite_0_9<-communes_dates_2009_2022_temperature_final_2009$`0-9`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_0_9_h_f

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_10_19<-communes_dates_2009_2022_temperature_final_2009$`10-19`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_10_19_h_f



communes_dates_2009_2022_temperature_final_2009$taux_mortalite_20_39<-communes_dates_2009_2022_temperature_final_2009$`20-39`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_20_39_h_f




communes_dates_2009_2022_temperature_final_2009$taux_mortalite_40_59<-communes_dates_2009_2022_temperature_final_2009$`40-59`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_40_59_h_f





communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_64<-communes_dates_2009_2022_temperature_final_2009$`60-64`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_60_64_h_f



communes_dates_2009_2022_temperature_final_2009$taux_mortalite_65_69<-communes_dates_2009_2022_temperature_final_2009$`65-69`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_65_69_h_f


communes_dates_2009_2022_temperature_final_2009$taux_mortalite_70_74<-communes_dates_2009_2022_temperature_final_2009$`70-74`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_70_74_h_f


communes_dates_2009_2022_temperature_final_2009$taux_mortalite_75_79<-communes_dates_2009_2022_temperature_final_2009$`75-79`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_75_79_h_f


communes_dates_2009_2022_temperature_final_2009$taux_mortalite_80_plus<-communes_dates_2009_2022_temperature_final_2009$`80+`/communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_80_plus_h_f


#communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_70<-(communes_dates_2009_2022_temperature_final_2009$`60-64`+communes_dates_2009_2022_temperature_final_2009$`65-69`)/(communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_60_64_h_f+communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_65_69_h_f)



communes_dates_2009_2022_temperature_final_2009$mort_total<-communes_dates_2009_2022_temperature_final_2009$Femme+communes_dates_2009_2022_temperature_final_2009$Homme


communes_dates_2009_2022_temperature_final_2009$taux_mortalite_total<-communes_dates_2009_2022_temperature_final_2009$mort_total/communes_dates_2009_2022_temperature_final_2009$value_estimated_population





communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_74<-(communes_dates_2009_2022_temperature_final_2009$`60-64`+communes_dates_2009_2022_temperature_final_2009$`65-69`+communes_dates_2009_2022_temperature_final_2009$`70-74`)/(communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_60_64_h_f+communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_65_69_h_f+communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_70_74_h_f)


communes_dates_2009_2022_temperature_final_2009$taux_mortalite_75_plus<-(communes_dates_2009_2022_temperature_final_2009$`75-79`+communes_dates_2009_2022_temperature_final_2009$`80+`)/(communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_75_79_h_f+communes_dates_2009_2022_temperature_final_2009$value_estimated_sum_80_plus_h_f)








#on enleve les communes avec des population de 0
communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009, communes_dates_2009_2022_temperature_final_2009$value_estimated_population>0)
communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009, communes_dates_2009_2022_temperature_final_2009$population_actif_25_54>0)




communes_dates_2009_2022_temperature_final_2009<- communes_dates_2009_2022_temperature_final_2009[ , !names(communes_dates_2009_2022_temperature_final_2009) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_femme))


communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(part_agriculteur))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(part_cadre))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(part_profession_intermediaire))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(part_employe))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(part_ouvrier))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(part_chomage))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_homme))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_0_9))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_10_19))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_20_39))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_40_59))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_60_64))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_65_69))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_70_74))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_75_79))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_total))




communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_60_74))
communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  !is.infinite(taux_mortalite_75_plus))





#761 valeurs inf

#

#communes_dates_2009_2022_temperature_final_2009<-communes_dates_2009_2022_temperature_final_2009[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  part_agriculteur<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_10_19<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  part_cadre<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  part_profession_intermediaire<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  part_employe<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  part_ouvrier<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  part_chomage<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_homme<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_femme<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_0_9<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_20_39<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_40_59<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_60_64<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_65_69<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_70_74<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_75_79<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_80_plus<=1)

#communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009,  taux_mortalite_total<=1)




communes_dates_2009_2022_temperature_final_2009$taux_mortalite_homme[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_homme>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_femme[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_femme>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_0_9[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_0_9>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_10_19[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_10_19>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_20_39[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_20_39>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_40_59[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_40_59>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_64[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_64>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_65_69[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_65_69>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_70_74[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_70_74>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_75_79[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_75_79>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_80_plus[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_80_plus>1]<-NA   

communes_dates_2009_2022_temperature_final_2009$taux_mortalite_total[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_total>1]<-NA   



communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_74[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_74>1]<-NA   
communes_dates_2009_2022_temperature_final_2009$taux_mortalite_75_plus[communes_dates_2009_2022_temperature_final_2009$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2009_2022_temperature_final_2009$part_agriculteur)

summary(communes_dates_2009_2022_temperature_final_2009$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2009_2022_temperature_final_2009$part_cadre)

summary(communes_dates_2009_2022_temperature_final_2009$part_profession_intermediaire)

summary(communes_dates_2009_2022_temperature_final_2009$part_employe)

summary(communes_dates_2009_2022_temperature_final_2009$part_ouvrier)

summary(communes_dates_2009_2022_temperature_final_2009$part_chomage)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_homme)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_femme)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_0_9)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_10_19)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_20_39)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_40_59)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_60_64)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_65_69)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_70_74)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_75_79)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_80_plus)

summary(communes_dates_2009_2022_temperature_final_2009$taux_mortalite_total)






fwrite(communes_dates_2009_2022_temperature_final_2009,"/données communes années/données mortalité temperature final mois new/communes_dates_2009_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2010_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2010-12-31")

communes_dates_2010_2022_temperature_final_2010 <- communes_dates_2010_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2010_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2010_age_sexe.csv")



communes_dates_2010_2022_temperature_final_2010$date<-as.Date(communes_dates_2010_2022_temperature_final_2010$date)
deces.2010_age_sexe$date<-as.Date(deces.2010_age_sexe$date)





communes_dates_2010_2022_temperature_final_2010<-left_join(communes_dates_2010_2022_temperature_final_2010,deces.2010_age_sexe)

communes_dates_2010_2022_temperature_final_2010[is.na(communes_dates_2010_2022_temperature_final_2010)]<-0


communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 < -20]<-"<-20"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= -20 & communes_dates_2010_2022_temperature_final_2010$value1 < -15]<-"-20_-15"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= -15 & communes_dates_2010_2022_temperature_final_2010$value1 < -10]<-"-15_-10"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= -10 & communes_dates_2010_2022_temperature_final_2010$value1 < -5]<-"-10_-5"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= -5 & communes_dates_2010_2022_temperature_final_2010$value1 < 0]<-"-5_0"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 0 & communes_dates_2010_2022_temperature_final_2010$value1 < 5]<-"0_5"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 5 & communes_dates_2010_2022_temperature_final_2010$value1 < 10]<-"5_10"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 10 & communes_dates_2010_2022_temperature_final_2010$value1 < 15]<-"10_15"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 15 & communes_dates_2010_2022_temperature_final_2010$value1 < 20]<-"15_20"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 20 & communes_dates_2010_2022_temperature_final_2010$value1 < 25]<-"20_25"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 25 & communes_dates_2010_2022_temperature_final_2010$value1 < 28]<-"25_28"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 28 & communes_dates_2010_2022_temperature_final_2010$value1 < 30]<-"28_30"

communes_dates_2010_2022_temperature_final_2010$temperature_bin[communes_dates_2010_2022_temperature_final_2010$value1 >= 30]<-">30"


#test<-filter(communes_dates_2010_2022_temperature_final_2010, is.na(temperature_bin))
#table(communes_dates_2010_2022_temperature_final_2010$temperature_bin)

library(fastDummies)
communes_dates_2010_2022_temperature_final_2010  <- communes_dates_2010_2022_temperature_final_2010  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2010_2022_temperature_final_2010 <- communes_dates_2010_2022_temperature_final_2010 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2010_2022_temperature_final_2010 <- communes_dates_2010_2022_temperature_final_2010 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2010_2022_temperature_final_2010$same_value[is.na(communes_dates_2010_2022_temperature_final_2010$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2010_2022_temperature_final_2010$same_value <- ifelse(communes_dates_2010_2022_temperature_final_2010$temperature_bin != ">30", 0, communes_dates_2010_2022_temperature_final_2010$same_value)



communes_dates_2010_2022_temperature_final_2010$mois<-substring(communes_dates_2010_2022_temperature_final_2010$date,6,7)

communes_dates_2010_2022_temperature_final_2010<-communes_dates_2010_2022_temperature_final_2010[,-c("date","value1","temperature_bin")]

communes_dates_2010_2022_temperature_final_2010<-aggregate(.~COM+mois,communes_dates_2010_2022_temperature_final_2010,sum)



RP_2010_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2010_age_sexe_final_2")

RP_2010_age_sexe_final_2<-RP_2010_age_sexe_final_2[,c(2:15)]

names(RP_2010_age_sexe_final_2)[names(RP_2010_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2010_2022_temperature_final_2010<-left_join(communes_dates_2010_2022_temperature_final_2010,RP_2010_age_sexe_final_2)

#tests<-filter(communes_dates_2010_2022_temperature_final_2010, COM=="01001")
#tests<-filter(communes_dates_2010_2022_temperature_final_2010, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010, !is.na(value_estimated_sum_homme) )

RP_2010_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2010_CSP_final_2")

RP_2010_CSP_final_2<-RP_2010_CSP_final_2[,c(2:12)]
names(RP_2010_CSP_final_2)[names(RP_2010_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2010_CSP_final_2)[names(RP_2010_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2010_2022_temperature_final_2010<-left_join(communes_dates_2010_2022_temperature_final_2010,RP_2010_CSP_final_2)


communes_dates_2010_2022_temperature_final_2010$part_agriculteur<-communes_dates_2010_2022_temperature_final_2010$value_estimated_agriculteur/communes_dates_2010_2022_temperature_final_2010$population_actif_25_54

communes_dates_2010_2022_temperature_final_2010$part_artisan_commercant_chef_entreprise<-communes_dates_2010_2022_temperature_final_2010$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2010_2022_temperature_final_2010$population_actif_25_54

communes_dates_2010_2022_temperature_final_2010$part_cadre<-communes_dates_2010_2022_temperature_final_2010$value_estimated_cadre/communes_dates_2010_2022_temperature_final_2010$population_actif_25_54

communes_dates_2010_2022_temperature_final_2010$part_profession_intermediaire<-communes_dates_2010_2022_temperature_final_2010$value_estimated_profession_intermediaire/communes_dates_2010_2022_temperature_final_2010$population_actif_25_54

communes_dates_2010_2022_temperature_final_2010$part_employe<-communes_dates_2010_2022_temperature_final_2010$value_estimated_employe/communes_dates_2010_2022_temperature_final_2010$population_actif_25_54

communes_dates_2010_2022_temperature_final_2010$part_ouvrier<-communes_dates_2010_2022_temperature_final_2010$value_estimated_ouvrier/communes_dates_2010_2022_temperature_final_2010$population_actif_25_54

communes_dates_2010_2022_temperature_final_2010$part_chomage<-communes_dates_2010_2022_temperature_final_2010$value_estimated_au_chomage/communes_dates_2010_2022_temperature_final_2010$population_actif_25_54






communes_dates_2010_2022_temperature_final_2010$taux_mortalite_homme<-communes_dates_2010_2022_temperature_final_2010$Homme/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_homme

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_femme<-communes_dates_2010_2022_temperature_final_2010$Femme/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_femme


communes_dates_2010_2022_temperature_final_2010$taux_mortalite_0_9<-communes_dates_2010_2022_temperature_final_2010$`0-9`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_0_9_h_f

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_10_19<-communes_dates_2010_2022_temperature_final_2010$`10-19`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_10_19_h_f



communes_dates_2010_2022_temperature_final_2010$taux_mortalite_20_39<-communes_dates_2010_2022_temperature_final_2010$`20-39`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_20_39_h_f




communes_dates_2010_2022_temperature_final_2010$taux_mortalite_40_59<-communes_dates_2010_2022_temperature_final_2010$`40-59`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_40_59_h_f





communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_64<-communes_dates_2010_2022_temperature_final_2010$`60-64`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_60_64_h_f



communes_dates_2010_2022_temperature_final_2010$taux_mortalite_65_69<-communes_dates_2010_2022_temperature_final_2010$`65-69`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_65_69_h_f


communes_dates_2010_2022_temperature_final_2010$taux_mortalite_70_74<-communes_dates_2010_2022_temperature_final_2010$`70-74`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_70_74_h_f


communes_dates_2010_2022_temperature_final_2010$taux_mortalite_75_79<-communes_dates_2010_2022_temperature_final_2010$`75-79`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_75_79_h_f


communes_dates_2010_2022_temperature_final_2010$taux_mortalite_80_plus<-communes_dates_2010_2022_temperature_final_2010$`80+`/communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_80_plus_h_f


#communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_70<-(communes_dates_2010_2022_temperature_final_2010$`60-64`+communes_dates_2010_2022_temperature_final_2010$`65-69`)/(communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_60_64_h_f+communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_65_69_h_f)



communes_dates_2010_2022_temperature_final_2010$mort_total<-communes_dates_2010_2022_temperature_final_2010$Femme+communes_dates_2010_2022_temperature_final_2010$Homme


communes_dates_2010_2022_temperature_final_2010$taux_mortalite_total<-communes_dates_2010_2022_temperature_final_2010$mort_total/communes_dates_2010_2022_temperature_final_2010$value_estimated_population







communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_74<-(communes_dates_2010_2022_temperature_final_2010$`60-64`+communes_dates_2010_2022_temperature_final_2010$`65-69`+communes_dates_2010_2022_temperature_final_2010$`70-74`)/(communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_60_64_h_f+communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_65_69_h_f+communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_70_74_h_f)


communes_dates_2010_2022_temperature_final_2010$taux_mortalite_75_plus<-(communes_dates_2010_2022_temperature_final_2010$`75-79`+communes_dates_2010_2022_temperature_final_2010$`80+`)/(communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_75_79_h_f+communes_dates_2010_2022_temperature_final_2010$value_estimated_sum_80_plus_h_f)










#on enleve les communes avec des population de 0
communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010, communes_dates_2010_2022_temperature_final_2010$value_estimated_population>0)
communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010, communes_dates_2010_2022_temperature_final_2010$population_actif_25_54>0)




communes_dates_2010_2022_temperature_final_2010<- communes_dates_2010_2022_temperature_final_2010[ , !names(communes_dates_2010_2022_temperature_final_2010) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_femme))


communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(part_agriculteur))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(part_cadre))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(part_profession_intermediaire))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(part_employe))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(part_ouvrier))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(part_chomage))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_homme))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_0_9))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_10_19))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_20_39))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_40_59))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_60_64))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_65_69))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_70_74))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_75_79))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_total))




communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_60_74))
communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  !is.infinite(taux_mortalite_75_plus))






#761 valeurs inf

#

#communes_dates_2010_2022_temperature_final_2010<-communes_dates_2010_2022_temperature_final_2010[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  part_agriculteur<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_10_19<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  part_cadre<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  part_profession_intermediaire<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  part_employe<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  part_ouvrier<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  part_chomage<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_homme<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_femme<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_0_9<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_20_39<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_40_59<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_60_64<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_65_69<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_70_74<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_75_79<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_80_plus<=1)

#communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010,  taux_mortalite_total<=1)




communes_dates_2010_2022_temperature_final_2010$taux_mortalite_homme[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_homme>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_femme[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_femme>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_0_9[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_0_9>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_10_19[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_10_19>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_20_39[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_20_39>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_40_59[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_40_59>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_64[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_64>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_65_69[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_65_69>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_70_74[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_70_74>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_75_79[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_75_79>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_80_plus[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_80_plus>1]<-NA   

communes_dates_2010_2022_temperature_final_2010$taux_mortalite_total[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_total>1]<-NA   



communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_74[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_74>1]<-NA   
communes_dates_2010_2022_temperature_final_2010$taux_mortalite_75_plus[communes_dates_2010_2022_temperature_final_2010$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2010_2022_temperature_final_2010$part_agriculteur)

summary(communes_dates_2010_2022_temperature_final_2010$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2010_2022_temperature_final_2010$part_cadre)

summary(communes_dates_2010_2022_temperature_final_2010$part_profession_intermediaire)

summary(communes_dates_2010_2022_temperature_final_2010$part_employe)

summary(communes_dates_2010_2022_temperature_final_2010$part_ouvrier)

summary(communes_dates_2010_2022_temperature_final_2010$part_chomage)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_homme)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_femme)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_0_9)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_10_19)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_20_39)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_40_59)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_60_64)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_65_69)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_70_74)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_75_79)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_80_plus)

summary(communes_dates_2010_2022_temperature_final_2010$taux_mortalite_total)






fwrite(communes_dates_2010_2022_temperature_final_2010,"/données communes années/données mortalité temperature final mois new/communes_dates_2010_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2011_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2011-01-01")
end_date <- as.Date("2011-12-31")

communes_dates_2011_2022_temperature_final_2011 <- communes_dates_2011_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2011_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2011_age_sexe.csv")





communes_dates_2011_2022_temperature_final_2011$date<-as.Date(communes_dates_2011_2022_temperature_final_2011$date)
deces.2011_age_sexe$date<-as.Date(deces.2011_age_sexe$date)





communes_dates_2011_2022_temperature_final_2011<-left_join(communes_dates_2011_2022_temperature_final_2011,deces.2011_age_sexe)

communes_dates_2011_2022_temperature_final_2011[is.na(communes_dates_2011_2022_temperature_final_2011)]<-0


communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 < -20]<-"<-20"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= -20 & communes_dates_2011_2022_temperature_final_2011$value1 < -15]<-"-20_-15"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= -15 & communes_dates_2011_2022_temperature_final_2011$value1 < -10]<-"-15_-10"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= -10 & communes_dates_2011_2022_temperature_final_2011$value1 < -5]<-"-10_-5"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= -5 & communes_dates_2011_2022_temperature_final_2011$value1 < 0]<-"-5_0"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 0 & communes_dates_2011_2022_temperature_final_2011$value1 < 5]<-"0_5"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 5 & communes_dates_2011_2022_temperature_final_2011$value1 < 10]<-"5_10"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 10 & communes_dates_2011_2022_temperature_final_2011$value1 < 15]<-"10_15"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 15 & communes_dates_2011_2022_temperature_final_2011$value1 < 20]<-"15_20"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 20 & communes_dates_2011_2022_temperature_final_2011$value1 < 25]<-"20_25"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 25 & communes_dates_2011_2022_temperature_final_2011$value1 < 28]<-"25_28"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 28 & communes_dates_2011_2022_temperature_final_2011$value1 < 30]<-"28_30"

communes_dates_2011_2022_temperature_final_2011$temperature_bin[communes_dates_2011_2022_temperature_final_2011$value1 >= 30]<-">30"


#test<-filter(communes_dates_2011_2022_temperature_final_2011, is.na(temperature_bin))
#table(communes_dates_2011_2022_temperature_final_2011$temperature_bin)

library(fastDummies)
communes_dates_2011_2022_temperature_final_2011  <- communes_dates_2011_2022_temperature_final_2011  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2011_2022_temperature_final_2011 <- communes_dates_2011_2022_temperature_final_2011 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2011_2022_temperature_final_2011 <- communes_dates_2011_2022_temperature_final_2011 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2011_2022_temperature_final_2011$same_value[is.na(communes_dates_2011_2022_temperature_final_2011$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2011_2022_temperature_final_2011$same_value <- ifelse(communes_dates_2011_2022_temperature_final_2011$temperature_bin != ">30", 0, communes_dates_2011_2022_temperature_final_2011$same_value)



communes_dates_2011_2022_temperature_final_2011$mois<-substring(communes_dates_2011_2022_temperature_final_2011$date,6,7)

communes_dates_2011_2022_temperature_final_2011<-communes_dates_2011_2022_temperature_final_2011[,-c("date","value1","temperature_bin")]

communes_dates_2011_2022_temperature_final_2011<-aggregate(.~COM+mois,communes_dates_2011_2022_temperature_final_2011,sum)



RP_2011_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2011_age_sexe_final_2")

RP_2011_age_sexe_final_2<-RP_2011_age_sexe_final_2[,c(2:15)]

names(RP_2011_age_sexe_final_2)[names(RP_2011_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2011_2022_temperature_final_2011<-left_join(communes_dates_2011_2022_temperature_final_2011,RP_2011_age_sexe_final_2)

#tests<-filter(communes_dates_2011_2022_temperature_final_2011, COM=="01001")
#tests<-filter(communes_dates_2011_2022_temperature_final_2011, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011, !is.na(value_estimated_sum_homme) )

RP_2011_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2011_CSP_final_2")

RP_2011_CSP_final_2<-RP_2011_CSP_final_2[,c(2:12)]
names(RP_2011_CSP_final_2)[names(RP_2011_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2011_CSP_final_2)[names(RP_2011_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2011_2022_temperature_final_2011<-left_join(communes_dates_2011_2022_temperature_final_2011,RP_2011_CSP_final_2)


communes_dates_2011_2022_temperature_final_2011$part_agriculteur<-communes_dates_2011_2022_temperature_final_2011$value_estimated_agriculteur/communes_dates_2011_2022_temperature_final_2011$population_actif_25_54

communes_dates_2011_2022_temperature_final_2011$part_artisan_commercant_chef_entreprise<-communes_dates_2011_2022_temperature_final_2011$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2011_2022_temperature_final_2011$population_actif_25_54

communes_dates_2011_2022_temperature_final_2011$part_cadre<-communes_dates_2011_2022_temperature_final_2011$value_estimated_cadre/communes_dates_2011_2022_temperature_final_2011$population_actif_25_54

communes_dates_2011_2022_temperature_final_2011$part_profession_intermediaire<-communes_dates_2011_2022_temperature_final_2011$value_estimated_profession_intermediaire/communes_dates_2011_2022_temperature_final_2011$population_actif_25_54

communes_dates_2011_2022_temperature_final_2011$part_employe<-communes_dates_2011_2022_temperature_final_2011$value_estimated_employe/communes_dates_2011_2022_temperature_final_2011$population_actif_25_54

communes_dates_2011_2022_temperature_final_2011$part_ouvrier<-communes_dates_2011_2022_temperature_final_2011$value_estimated_ouvrier/communes_dates_2011_2022_temperature_final_2011$population_actif_25_54

communes_dates_2011_2022_temperature_final_2011$part_chomage<-communes_dates_2011_2022_temperature_final_2011$value_estimated_au_chomage/communes_dates_2011_2022_temperature_final_2011$population_actif_25_54






communes_dates_2011_2022_temperature_final_2011$taux_mortalite_homme<-communes_dates_2011_2022_temperature_final_2011$Homme/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_homme

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_femme<-communes_dates_2011_2022_temperature_final_2011$Femme/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_femme


communes_dates_2011_2022_temperature_final_2011$taux_mortalite_0_9<-communes_dates_2011_2022_temperature_final_2011$`0-9`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_0_9_h_f

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_10_19<-communes_dates_2011_2022_temperature_final_2011$`10-19`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_10_19_h_f



communes_dates_2011_2022_temperature_final_2011$taux_mortalite_20_39<-communes_dates_2011_2022_temperature_final_2011$`20-39`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_20_39_h_f




communes_dates_2011_2022_temperature_final_2011$taux_mortalite_40_59<-communes_dates_2011_2022_temperature_final_2011$`40-59`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_40_59_h_f





communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_64<-communes_dates_2011_2022_temperature_final_2011$`60-64`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_60_64_h_f



communes_dates_2011_2022_temperature_final_2011$taux_mortalite_65_69<-communes_dates_2011_2022_temperature_final_2011$`65-69`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_65_69_h_f


communes_dates_2011_2022_temperature_final_2011$taux_mortalite_70_74<-communes_dates_2011_2022_temperature_final_2011$`70-74`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_70_74_h_f


communes_dates_2011_2022_temperature_final_2011$taux_mortalite_75_79<-communes_dates_2011_2022_temperature_final_2011$`75-79`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_75_79_h_f


communes_dates_2011_2022_temperature_final_2011$taux_mortalite_80_plus<-communes_dates_2011_2022_temperature_final_2011$`80+`/communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_80_plus_h_f


#communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_70<-(communes_dates_2011_2022_temperature_final_2011$`60-64`+communes_dates_2011_2022_temperature_final_2011$`65-69`)/(communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_60_64_h_f+communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_65_69_h_f)



communes_dates_2011_2022_temperature_final_2011$mort_total<-communes_dates_2011_2022_temperature_final_2011$Femme+communes_dates_2011_2022_temperature_final_2011$Homme


communes_dates_2011_2022_temperature_final_2011$taux_mortalite_total<-communes_dates_2011_2022_temperature_final_2011$mort_total/communes_dates_2011_2022_temperature_final_2011$value_estimated_population





communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_74<-(communes_dates_2011_2022_temperature_final_2011$`60-64`+communes_dates_2011_2022_temperature_final_2011$`65-69`+communes_dates_2011_2022_temperature_final_2011$`70-74`)/(communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_60_64_h_f+communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_65_69_h_f+communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_70_74_h_f)


communes_dates_2011_2022_temperature_final_2011$taux_mortalite_75_plus<-(communes_dates_2011_2022_temperature_final_2011$`75-79`+communes_dates_2011_2022_temperature_final_2011$`80+`)/(communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_75_79_h_f+communes_dates_2011_2022_temperature_final_2011$value_estimated_sum_80_plus_h_f)








#on enleve les communes avec des population de 0
communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011, communes_dates_2011_2022_temperature_final_2011$value_estimated_population>0)
communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011, communes_dates_2011_2022_temperature_final_2011$population_actif_25_54>0)




communes_dates_2011_2022_temperature_final_2011<- communes_dates_2011_2022_temperature_final_2011[ , !names(communes_dates_2011_2022_temperature_final_2011) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_femme))


communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(part_agriculteur))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(part_cadre))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(part_profession_intermediaire))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(part_employe))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(part_ouvrier))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(part_chomage))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_homme))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_0_9))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_10_19))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_20_39))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_40_59))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_60_64))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_65_69))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_70_74))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_75_79))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_total))




communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_60_74))
communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  !is.infinite(taux_mortalite_75_plus))







#761 valeurs inf

#

#communes_dates_2011_2022_temperature_final_2011<-communes_dates_2011_2022_temperature_final_2011[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  part_agriculteur<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_10_19<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  part_cadre<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  part_profession_intermediaire<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  part_employe<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  part_ouvrier<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  part_chomage<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_homme<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_femme<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_0_9<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_20_39<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_40_59<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_60_64<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_65_69<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_70_74<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_75_79<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_80_plus<=1)

#communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011,  taux_mortalite_total<=1)




communes_dates_2011_2022_temperature_final_2011$taux_mortalite_homme[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_homme>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_femme[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_femme>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_0_9[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_0_9>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_10_19[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_10_19>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_20_39[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_20_39>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_40_59[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_40_59>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_64[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_64>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_65_69[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_65_69>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_70_74[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_70_74>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_75_79[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_75_79>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_80_plus[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_80_plus>1]<-NA   

communes_dates_2011_2022_temperature_final_2011$taux_mortalite_total[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_total>1]<-NA   




communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_74[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_74>1]<-NA   
communes_dates_2011_2022_temperature_final_2011$taux_mortalite_75_plus[communes_dates_2011_2022_temperature_final_2011$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2011_2022_temperature_final_2011$part_agriculteur)

summary(communes_dates_2011_2022_temperature_final_2011$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2011_2022_temperature_final_2011$part_cadre)

summary(communes_dates_2011_2022_temperature_final_2011$part_profession_intermediaire)

summary(communes_dates_2011_2022_temperature_final_2011$part_employe)

summary(communes_dates_2011_2022_temperature_final_2011$part_ouvrier)

summary(communes_dates_2011_2022_temperature_final_2011$part_chomage)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_homme)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_femme)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_0_9)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_10_19)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_20_39)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_40_59)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_60_64)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_65_69)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_70_74)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_75_79)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_80_plus)

summary(communes_dates_2011_2022_temperature_final_2011$taux_mortalite_total)






fwrite(communes_dates_2011_2022_temperature_final_2011,"/données communes années/données mortalité temperature final mois new/communes_dates_2011_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2012_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2012-01-01")
end_date <- as.Date("2012-12-31")

communes_dates_2012_2022_temperature_final_2012 <- communes_dates_2012_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2012_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2012_age_sexe.csv")




communes_dates_2012_2022_temperature_final_2012$date<-as.Date(communes_dates_2012_2022_temperature_final_2012$date)
deces.2012_age_sexe$date<-as.Date(deces.2012_age_sexe$date)





communes_dates_2012_2022_temperature_final_2012<-left_join(communes_dates_2012_2022_temperature_final_2012,deces.2012_age_sexe)

communes_dates_2012_2022_temperature_final_2012[is.na(communes_dates_2012_2022_temperature_final_2012)]<-0


communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 < -20]<-"<-20"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= -20 & communes_dates_2012_2022_temperature_final_2012$value1 < -15]<-"-20_-15"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= -15 & communes_dates_2012_2022_temperature_final_2012$value1 < -10]<-"-15_-10"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= -10 & communes_dates_2012_2022_temperature_final_2012$value1 < -5]<-"-10_-5"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= -5 & communes_dates_2012_2022_temperature_final_2012$value1 < 0]<-"-5_0"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 0 & communes_dates_2012_2022_temperature_final_2012$value1 < 5]<-"0_5"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 5 & communes_dates_2012_2022_temperature_final_2012$value1 < 10]<-"5_10"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 10 & communes_dates_2012_2022_temperature_final_2012$value1 < 15]<-"10_15"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 15 & communes_dates_2012_2022_temperature_final_2012$value1 < 20]<-"15_20"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 20 & communes_dates_2012_2022_temperature_final_2012$value1 < 25]<-"20_25"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 25 & communes_dates_2012_2022_temperature_final_2012$value1 < 28]<-"25_28"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 28 & communes_dates_2012_2022_temperature_final_2012$value1 < 30]<-"28_30"

communes_dates_2012_2022_temperature_final_2012$temperature_bin[communes_dates_2012_2022_temperature_final_2012$value1 >= 30]<-">30"


#test<-filter(communes_dates_2012_2022_temperature_final_2012, is.na(temperature_bin))
#table(communes_dates_2012_2022_temperature_final_2012$temperature_bin)

library(fastDummies)
communes_dates_2012_2022_temperature_final_2012  <- communes_dates_2012_2022_temperature_final_2012  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2012_2022_temperature_final_2012 <- communes_dates_2012_2022_temperature_final_2012 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2012_2022_temperature_final_2012 <- communes_dates_2012_2022_temperature_final_2012 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2012_2022_temperature_final_2012$same_value[is.na(communes_dates_2012_2022_temperature_final_2012$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2012_2022_temperature_final_2012$same_value <- ifelse(communes_dates_2012_2022_temperature_final_2012$temperature_bin != ">30", 0, communes_dates_2012_2022_temperature_final_2012$same_value)



communes_dates_2012_2022_temperature_final_2012$mois<-substring(communes_dates_2012_2022_temperature_final_2012$date,6,7)

communes_dates_2012_2022_temperature_final_2012<-communes_dates_2012_2022_temperature_final_2012[,-c("date","value1","temperature_bin")]

communes_dates_2012_2022_temperature_final_2012<-aggregate(.~COM+mois,communes_dates_2012_2022_temperature_final_2012,sum)



RP_2012_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2012_age_sexe_final_2")

RP_2012_age_sexe_final_2<-RP_2012_age_sexe_final_2[,c(2:15)]

names(RP_2012_age_sexe_final_2)[names(RP_2012_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2012_2022_temperature_final_2012<-left_join(communes_dates_2012_2022_temperature_final_2012,RP_2012_age_sexe_final_2)

#tests<-filter(communes_dates_2012_2022_temperature_final_2012, COM=="01001")
#tests<-filter(communes_dates_2012_2022_temperature_final_2012, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012, !is.na(value_estimated_sum_homme) )

RP_2012_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2012_CSP_final_2")

RP_2012_CSP_final_2<-RP_2012_CSP_final_2[,c(2:12)]
names(RP_2012_CSP_final_2)[names(RP_2012_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2012_CSP_final_2)[names(RP_2012_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2012_2022_temperature_final_2012<-left_join(communes_dates_2012_2022_temperature_final_2012,RP_2012_CSP_final_2)


communes_dates_2012_2022_temperature_final_2012$part_agriculteur<-communes_dates_2012_2022_temperature_final_2012$value_estimated_agriculteur/communes_dates_2012_2022_temperature_final_2012$population_actif_25_54

communes_dates_2012_2022_temperature_final_2012$part_artisan_commercant_chef_entreprise<-communes_dates_2012_2022_temperature_final_2012$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2012_2022_temperature_final_2012$population_actif_25_54

communes_dates_2012_2022_temperature_final_2012$part_cadre<-communes_dates_2012_2022_temperature_final_2012$value_estimated_cadre/communes_dates_2012_2022_temperature_final_2012$population_actif_25_54

communes_dates_2012_2022_temperature_final_2012$part_profession_intermediaire<-communes_dates_2012_2022_temperature_final_2012$value_estimated_profession_intermediaire/communes_dates_2012_2022_temperature_final_2012$population_actif_25_54

communes_dates_2012_2022_temperature_final_2012$part_employe<-communes_dates_2012_2022_temperature_final_2012$value_estimated_employe/communes_dates_2012_2022_temperature_final_2012$population_actif_25_54

communes_dates_2012_2022_temperature_final_2012$part_ouvrier<-communes_dates_2012_2022_temperature_final_2012$value_estimated_ouvrier/communes_dates_2012_2022_temperature_final_2012$population_actif_25_54

communes_dates_2012_2022_temperature_final_2012$part_chomage<-communes_dates_2012_2022_temperature_final_2012$value_estimated_au_chomage/communes_dates_2012_2022_temperature_final_2012$population_actif_25_54






communes_dates_2012_2022_temperature_final_2012$taux_mortalite_homme<-communes_dates_2012_2022_temperature_final_2012$Homme/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_homme

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_femme<-communes_dates_2012_2022_temperature_final_2012$Femme/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_femme


communes_dates_2012_2022_temperature_final_2012$taux_mortalite_0_9<-communes_dates_2012_2022_temperature_final_2012$`0-9`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_0_9_h_f

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_10_19<-communes_dates_2012_2022_temperature_final_2012$`10-19`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_10_19_h_f



communes_dates_2012_2022_temperature_final_2012$taux_mortalite_20_39<-communes_dates_2012_2022_temperature_final_2012$`20-39`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_20_39_h_f




communes_dates_2012_2022_temperature_final_2012$taux_mortalite_40_59<-communes_dates_2012_2022_temperature_final_2012$`40-59`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_40_59_h_f





communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_64<-communes_dates_2012_2022_temperature_final_2012$`60-64`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_60_64_h_f



communes_dates_2012_2022_temperature_final_2012$taux_mortalite_65_69<-communes_dates_2012_2022_temperature_final_2012$`65-69`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_65_69_h_f


communes_dates_2012_2022_temperature_final_2012$taux_mortalite_70_74<-communes_dates_2012_2022_temperature_final_2012$`70-74`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_70_74_h_f


communes_dates_2012_2022_temperature_final_2012$taux_mortalite_75_79<-communes_dates_2012_2022_temperature_final_2012$`75-79`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_75_79_h_f


communes_dates_2012_2022_temperature_final_2012$taux_mortalite_80_plus<-communes_dates_2012_2022_temperature_final_2012$`80+`/communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_80_plus_h_f


#communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_70<-(communes_dates_2012_2022_temperature_final_2012$`60-64`+communes_dates_2012_2022_temperature_final_2012$`65-69`)/(communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_60_64_h_f+communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_65_69_h_f)



communes_dates_2012_2022_temperature_final_2012$mort_total<-communes_dates_2012_2022_temperature_final_2012$Femme+communes_dates_2012_2022_temperature_final_2012$Homme


communes_dates_2012_2022_temperature_final_2012$taux_mortalite_total<-communes_dates_2012_2022_temperature_final_2012$mort_total/communes_dates_2012_2022_temperature_final_2012$value_estimated_population







communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_74<-(communes_dates_2012_2022_temperature_final_2012$`60-64`+communes_dates_2012_2022_temperature_final_2012$`65-69`+communes_dates_2012_2022_temperature_final_2012$`70-74`)/(communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_60_64_h_f+communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_65_69_h_f+communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_70_74_h_f)


communes_dates_2012_2022_temperature_final_2012$taux_mortalite_75_plus<-(communes_dates_2012_2022_temperature_final_2012$`75-79`+communes_dates_2012_2022_temperature_final_2012$`80+`)/(communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_75_79_h_f+communes_dates_2012_2022_temperature_final_2012$value_estimated_sum_80_plus_h_f)








#on enleve les communes avec des population de 0
communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012, communes_dates_2012_2022_temperature_final_2012$value_estimated_population>0)
communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012, communes_dates_2012_2022_temperature_final_2012$population_actif_25_54>0)




communes_dates_2012_2022_temperature_final_2012<- communes_dates_2012_2022_temperature_final_2012[ , !names(communes_dates_2012_2022_temperature_final_2012) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_femme))


communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(part_agriculteur))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(part_cadre))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(part_profession_intermediaire))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(part_employe))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(part_ouvrier))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(part_chomage))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_homme))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_0_9))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_10_19))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_20_39))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_40_59))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_60_64))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_65_69))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_70_74))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_75_79))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_total))




communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_60_74))
communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  !is.infinite(taux_mortalite_75_plus))






#761 valeurs inf

#

#communes_dates_2012_2022_temperature_final_2012<-communes_dates_2012_2022_temperature_final_2012[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  part_agriculteur<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_10_19<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  part_cadre<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  part_profession_intermediaire<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  part_employe<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  part_ouvrier<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  part_chomage<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_homme<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_femme<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_0_9<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_20_39<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_40_59<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_60_64<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_65_69<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_70_74<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_75_79<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_80_plus<=1)

#communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012,  taux_mortalite_total<=1)




communes_dates_2012_2022_temperature_final_2012$taux_mortalite_homme[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_homme>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_femme[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_femme>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_0_9[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_0_9>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_10_19[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_10_19>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_20_39[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_20_39>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_40_59[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_40_59>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_64[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_64>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_65_69[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_65_69>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_70_74[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_70_74>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_75_79[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_75_79>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_80_plus[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_80_plus>1]<-NA   

communes_dates_2012_2022_temperature_final_2012$taux_mortalite_total[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_total>1]<-NA   



communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_74[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_74>1]<-NA   
communes_dates_2012_2022_temperature_final_2012$taux_mortalite_75_plus[communes_dates_2012_2022_temperature_final_2012$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2012_2022_temperature_final_2012$part_agriculteur)

summary(communes_dates_2012_2022_temperature_final_2012$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2012_2022_temperature_final_2012$part_cadre)

summary(communes_dates_2012_2022_temperature_final_2012$part_profession_intermediaire)

summary(communes_dates_2012_2022_temperature_final_2012$part_employe)

summary(communes_dates_2012_2022_temperature_final_2012$part_ouvrier)

summary(communes_dates_2012_2022_temperature_final_2012$part_chomage)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_homme)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_femme)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_0_9)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_10_19)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_20_39)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_40_59)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_60_64)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_65_69)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_70_74)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_75_79)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_80_plus)

summary(communes_dates_2012_2022_temperature_final_2012$taux_mortalite_total)






fwrite(communes_dates_2012_2022_temperature_final_2012,"/données communes années/données mortalité temperature final mois new/communes_dates_2012_temperature_deces_mois.csv")












################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2013_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2013-01-01")
end_date <- as.Date("2013-12-31")

communes_dates_2013_2022_temperature_final_2013 <- communes_dates_2013_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2013_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2013_age_sexe.csv")





communes_dates_2013_2022_temperature_final_2013$date<-as.Date(communes_dates_2013_2022_temperature_final_2013$date)
deces.2013_age_sexe$date<-as.Date(deces.2013_age_sexe$date)







communes_dates_2013_2022_temperature_final_2013<-left_join(communes_dates_2013_2022_temperature_final_2013,deces.2013_age_sexe)

communes_dates_2013_2022_temperature_final_2013[is.na(communes_dates_2013_2022_temperature_final_2013)]<-0


communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 < -20]<-"<-20"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= -20 & communes_dates_2013_2022_temperature_final_2013$value1 < -15]<-"-20_-15"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= -15 & communes_dates_2013_2022_temperature_final_2013$value1 < -10]<-"-15_-10"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= -10 & communes_dates_2013_2022_temperature_final_2013$value1 < -5]<-"-10_-5"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= -5 & communes_dates_2013_2022_temperature_final_2013$value1 < 0]<-"-5_0"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 0 & communes_dates_2013_2022_temperature_final_2013$value1 < 5]<-"0_5"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 5 & communes_dates_2013_2022_temperature_final_2013$value1 < 10]<-"5_10"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 10 & communes_dates_2013_2022_temperature_final_2013$value1 < 15]<-"10_15"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 15 & communes_dates_2013_2022_temperature_final_2013$value1 < 20]<-"15_20"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 20 & communes_dates_2013_2022_temperature_final_2013$value1 < 25]<-"20_25"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 25 & communes_dates_2013_2022_temperature_final_2013$value1 < 28]<-"25_28"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 28 & communes_dates_2013_2022_temperature_final_2013$value1 < 30]<-"28_30"

communes_dates_2013_2022_temperature_final_2013$temperature_bin[communes_dates_2013_2022_temperature_final_2013$value1 >= 30]<-">30"


#test<-filter(communes_dates_2013_2022_temperature_final_2013, is.na(temperature_bin))
#table(communes_dates_2013_2022_temperature_final_2013$temperature_bin)

library(fastDummies)
communes_dates_2013_2022_temperature_final_2013  <- communes_dates_2013_2022_temperature_final_2013  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2013_2022_temperature_final_2013 <- communes_dates_2013_2022_temperature_final_2013 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2013_2022_temperature_final_2013 <- communes_dates_2013_2022_temperature_final_2013 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2013_2022_temperature_final_2013$same_value[is.na(communes_dates_2013_2022_temperature_final_2013$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2013_2022_temperature_final_2013$same_value <- ifelse(communes_dates_2013_2022_temperature_final_2013$temperature_bin != ">30", 0, communes_dates_2013_2022_temperature_final_2013$same_value)



communes_dates_2013_2022_temperature_final_2013$mois<-substring(communes_dates_2013_2022_temperature_final_2013$date,6,7)

communes_dates_2013_2022_temperature_final_2013<-communes_dates_2013_2022_temperature_final_2013[,-c("date","value1","temperature_bin")]

communes_dates_2013_2022_temperature_final_2013<-aggregate(.~COM+mois,communes_dates_2013_2022_temperature_final_2013,sum)



RP_2013_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final_2")

RP_2013_age_sexe_final_2<-RP_2013_age_sexe_final_2[,c(2:15)]

names(RP_2013_age_sexe_final_2)[names(RP_2013_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2013_2022_temperature_final_2013<-left_join(communes_dates_2013_2022_temperature_final_2013,RP_2013_age_sexe_final_2)

#tests<-filter(communes_dates_2013_2022_temperature_final_2013, COM=="01001")
#tests<-filter(communes_dates_2013_2022_temperature_final_2013, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013, !is.na(value_estimated_sum_homme) )

RP_2013_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2013_CSP_final_2")

RP_2013_CSP_final_2<-RP_2013_CSP_final_2[,c(2:12)]
names(RP_2013_CSP_final_2)[names(RP_2013_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2013_CSP_final_2)[names(RP_2013_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2013_2022_temperature_final_2013<-left_join(communes_dates_2013_2022_temperature_final_2013,RP_2013_CSP_final_2)


communes_dates_2013_2022_temperature_final_2013$part_agriculteur<-communes_dates_2013_2022_temperature_final_2013$value_estimated_agriculteur/communes_dates_2013_2022_temperature_final_2013$population_actif_25_54

communes_dates_2013_2022_temperature_final_2013$part_artisan_commercant_chef_entreprise<-communes_dates_2013_2022_temperature_final_2013$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2013_2022_temperature_final_2013$population_actif_25_54

communes_dates_2013_2022_temperature_final_2013$part_cadre<-communes_dates_2013_2022_temperature_final_2013$value_estimated_cadre/communes_dates_2013_2022_temperature_final_2013$population_actif_25_54

communes_dates_2013_2022_temperature_final_2013$part_profession_intermediaire<-communes_dates_2013_2022_temperature_final_2013$value_estimated_profession_intermediaire/communes_dates_2013_2022_temperature_final_2013$population_actif_25_54

communes_dates_2013_2022_temperature_final_2013$part_employe<-communes_dates_2013_2022_temperature_final_2013$value_estimated_employe/communes_dates_2013_2022_temperature_final_2013$population_actif_25_54

communes_dates_2013_2022_temperature_final_2013$part_ouvrier<-communes_dates_2013_2022_temperature_final_2013$value_estimated_ouvrier/communes_dates_2013_2022_temperature_final_2013$population_actif_25_54

communes_dates_2013_2022_temperature_final_2013$part_chomage<-communes_dates_2013_2022_temperature_final_2013$value_estimated_au_chomage/communes_dates_2013_2022_temperature_final_2013$population_actif_25_54






communes_dates_2013_2022_temperature_final_2013$taux_mortalite_homme<-communes_dates_2013_2022_temperature_final_2013$Homme/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_homme

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_femme<-communes_dates_2013_2022_temperature_final_2013$Femme/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_femme


communes_dates_2013_2022_temperature_final_2013$taux_mortalite_0_9<-communes_dates_2013_2022_temperature_final_2013$`0-9`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_0_9_h_f

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_10_19<-communes_dates_2013_2022_temperature_final_2013$`10-19`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_10_19_h_f



communes_dates_2013_2022_temperature_final_2013$taux_mortalite_20_39<-communes_dates_2013_2022_temperature_final_2013$`20-39`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_20_39_h_f




communes_dates_2013_2022_temperature_final_2013$taux_mortalite_40_59<-communes_dates_2013_2022_temperature_final_2013$`40-59`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_40_59_h_f





communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_64<-communes_dates_2013_2022_temperature_final_2013$`60-64`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_60_64_h_f



communes_dates_2013_2022_temperature_final_2013$taux_mortalite_65_69<-communes_dates_2013_2022_temperature_final_2013$`65-69`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_65_69_h_f


communes_dates_2013_2022_temperature_final_2013$taux_mortalite_70_74<-communes_dates_2013_2022_temperature_final_2013$`70-74`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_70_74_h_f


communes_dates_2013_2022_temperature_final_2013$taux_mortalite_75_79<-communes_dates_2013_2022_temperature_final_2013$`75-79`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_75_79_h_f


communes_dates_2013_2022_temperature_final_2013$taux_mortalite_80_plus<-communes_dates_2013_2022_temperature_final_2013$`80+`/communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_80_plus_h_f


#communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_70<-(communes_dates_2013_2022_temperature_final_2013$`60-64`+communes_dates_2013_2022_temperature_final_2013$`65-69`)/(communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_60_64_h_f+communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_65_69_h_f)



communes_dates_2013_2022_temperature_final_2013$mort_total<-communes_dates_2013_2022_temperature_final_2013$Femme+communes_dates_2013_2022_temperature_final_2013$Homme


communes_dates_2013_2022_temperature_final_2013$taux_mortalite_total<-communes_dates_2013_2022_temperature_final_2013$mort_total/communes_dates_2013_2022_temperature_final_2013$value_estimated_population




communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_74<-(communes_dates_2013_2022_temperature_final_2013$`60-64`+communes_dates_2013_2022_temperature_final_2013$`65-69`+communes_dates_2013_2022_temperature_final_2013$`70-74`)/(communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_60_64_h_f+communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_65_69_h_f+communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_70_74_h_f)


communes_dates_2013_2022_temperature_final_2013$taux_mortalite_75_plus<-(communes_dates_2013_2022_temperature_final_2013$`75-79`+communes_dates_2013_2022_temperature_final_2013$`80+`)/(communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_75_79_h_f+communes_dates_2013_2022_temperature_final_2013$value_estimated_sum_80_plus_h_f)









#on enleve les communes avec des population de 0
communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013, communes_dates_2013_2022_temperature_final_2013$value_estimated_population>0)
communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013, communes_dates_2013_2022_temperature_final_2013$population_actif_25_54>0)




communes_dates_2013_2022_temperature_final_2013<- communes_dates_2013_2022_temperature_final_2013[ , !names(communes_dates_2013_2022_temperature_final_2013) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_femme))


communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(part_agriculteur))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(part_cadre))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(part_profession_intermediaire))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(part_employe))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(part_ouvrier))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(part_chomage))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_homme))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_0_9))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_10_19))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_20_39))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_40_59))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_60_64))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_65_69))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_70_74))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_75_79))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_total))




communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_60_74))
communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  !is.infinite(taux_mortalite_75_plus))









#761 valeurs inf

#

#communes_dates_2013_2022_temperature_final_2013<-communes_dates_2013_2022_temperature_final_2013[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  part_agriculteur<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_10_19<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  part_cadre<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  part_profession_intermediaire<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  part_employe<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  part_ouvrier<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  part_chomage<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_homme<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_femme<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_0_9<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_20_39<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_40_59<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_60_64<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_65_69<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_70_74<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_75_79<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_80_plus<=1)

#communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013,  taux_mortalite_total<=1)




communes_dates_2013_2022_temperature_final_2013$taux_mortalite_homme[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_homme>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_femme[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_femme>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_0_9[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_0_9>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_10_19[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_10_19>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_20_39[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_20_39>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_40_59[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_40_59>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_64[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_64>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_65_69[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_65_69>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_70_74[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_70_74>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_75_79[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_75_79>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_80_plus[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_80_plus>1]<-NA   

communes_dates_2013_2022_temperature_final_2013$taux_mortalite_total[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_total>1]<-NA   



communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_74[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_74>1]<-NA   
communes_dates_2013_2022_temperature_final_2013$taux_mortalite_75_plus[communes_dates_2013_2022_temperature_final_2013$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2013_2022_temperature_final_2013$part_agriculteur)

summary(communes_dates_2013_2022_temperature_final_2013$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2013_2022_temperature_final_2013$part_cadre)

summary(communes_dates_2013_2022_temperature_final_2013$part_profession_intermediaire)

summary(communes_dates_2013_2022_temperature_final_2013$part_employe)

summary(communes_dates_2013_2022_temperature_final_2013$part_ouvrier)

summary(communes_dates_2013_2022_temperature_final_2013$part_chomage)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_homme)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_femme)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_0_9)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_10_19)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_20_39)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_40_59)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_60_64)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_65_69)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_70_74)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_75_79)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_80_plus)

summary(communes_dates_2013_2022_temperature_final_2013$taux_mortalite_total)






fwrite(communes_dates_2013_2022_temperature_final_2013,"/données communes années/données mortalité temperature final mois new/communes_dates_2013_temperature_deces_mois.csv")














################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2014_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2014-12-31")

communes_dates_2014_2022_temperature_final_2014 <- communes_dates_2014_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2014_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2014_age_sexe.csv")





communes_dates_2014_2022_temperature_final_2014$date<-as.Date(communes_dates_2014_2022_temperature_final_2014$date)
deces.2014_age_sexe$date<-as.Date(deces.2014_age_sexe$date)





communes_dates_2014_2022_temperature_final_2014<-left_join(communes_dates_2014_2022_temperature_final_2014,deces.2014_age_sexe)

communes_dates_2014_2022_temperature_final_2014[is.na(communes_dates_2014_2022_temperature_final_2014)]<-0


communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 < -20]<-"<-20"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= -20 & communes_dates_2014_2022_temperature_final_2014$value1 < -15]<-"-20_-15"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= -15 & communes_dates_2014_2022_temperature_final_2014$value1 < -10]<-"-15_-10"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= -10 & communes_dates_2014_2022_temperature_final_2014$value1 < -5]<-"-10_-5"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= -5 & communes_dates_2014_2022_temperature_final_2014$value1 < 0]<-"-5_0"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 0 & communes_dates_2014_2022_temperature_final_2014$value1 < 5]<-"0_5"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 5 & communes_dates_2014_2022_temperature_final_2014$value1 < 10]<-"5_10"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 10 & communes_dates_2014_2022_temperature_final_2014$value1 < 15]<-"10_15"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 15 & communes_dates_2014_2022_temperature_final_2014$value1 < 20]<-"15_20"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 20 & communes_dates_2014_2022_temperature_final_2014$value1 < 25]<-"20_25"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 25 & communes_dates_2014_2022_temperature_final_2014$value1 < 28]<-"25_28"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 28 & communes_dates_2014_2022_temperature_final_2014$value1 < 30]<-"28_30"

communes_dates_2014_2022_temperature_final_2014$temperature_bin[communes_dates_2014_2022_temperature_final_2014$value1 >= 30]<-">30"


#test<-filter(communes_dates_2014_2022_temperature_final_2014, is.na(temperature_bin))
#table(communes_dates_2014_2022_temperature_final_2014$temperature_bin)

library(fastDummies)
communes_dates_2014_2022_temperature_final_2014  <- communes_dates_2014_2022_temperature_final_2014  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2014_2022_temperature_final_2014 <- communes_dates_2014_2022_temperature_final_2014 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2014_2022_temperature_final_2014 <- communes_dates_2014_2022_temperature_final_2014 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2014_2022_temperature_final_2014$same_value[is.na(communes_dates_2014_2022_temperature_final_2014$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2014_2022_temperature_final_2014$same_value <- ifelse(communes_dates_2014_2022_temperature_final_2014$temperature_bin != ">30", 0, communes_dates_2014_2022_temperature_final_2014$same_value)



communes_dates_2014_2022_temperature_final_2014$mois<-substring(communes_dates_2014_2022_temperature_final_2014$date,6,7)

communes_dates_2014_2022_temperature_final_2014<-communes_dates_2014_2022_temperature_final_2014[,-c("date","value1","temperature_bin")]

communes_dates_2014_2022_temperature_final_2014<-aggregate(.~COM+mois,communes_dates_2014_2022_temperature_final_2014,sum)



RP_2014_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2014_age_sexe_final_2")

RP_2014_age_sexe_final_2<-RP_2014_age_sexe_final_2[,c(2:15)]

names(RP_2014_age_sexe_final_2)[names(RP_2014_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2014_2022_temperature_final_2014<-left_join(communes_dates_2014_2022_temperature_final_2014,RP_2014_age_sexe_final_2)

#tests<-filter(communes_dates_2014_2022_temperature_final_2014, COM=="01001")
#tests<-filter(communes_dates_2014_2022_temperature_final_2014, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014, !is.na(value_estimated_sum_homme) )

RP_2014_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2014_CSP_final_2")

RP_2014_CSP_final_2<-RP_2014_CSP_final_2[,c(2:12)]
names(RP_2014_CSP_final_2)[names(RP_2014_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2014_CSP_final_2)[names(RP_2014_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2014_2022_temperature_final_2014<-left_join(communes_dates_2014_2022_temperature_final_2014,RP_2014_CSP_final_2)


communes_dates_2014_2022_temperature_final_2014$part_agriculteur<-communes_dates_2014_2022_temperature_final_2014$value_estimated_agriculteur/communes_dates_2014_2022_temperature_final_2014$population_actif_25_54

communes_dates_2014_2022_temperature_final_2014$part_artisan_commercant_chef_entreprise<-communes_dates_2014_2022_temperature_final_2014$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2014_2022_temperature_final_2014$population_actif_25_54

communes_dates_2014_2022_temperature_final_2014$part_cadre<-communes_dates_2014_2022_temperature_final_2014$value_estimated_cadre/communes_dates_2014_2022_temperature_final_2014$population_actif_25_54

communes_dates_2014_2022_temperature_final_2014$part_profession_intermediaire<-communes_dates_2014_2022_temperature_final_2014$value_estimated_profession_intermediaire/communes_dates_2014_2022_temperature_final_2014$population_actif_25_54

communes_dates_2014_2022_temperature_final_2014$part_employe<-communes_dates_2014_2022_temperature_final_2014$value_estimated_employe/communes_dates_2014_2022_temperature_final_2014$population_actif_25_54

communes_dates_2014_2022_temperature_final_2014$part_ouvrier<-communes_dates_2014_2022_temperature_final_2014$value_estimated_ouvrier/communes_dates_2014_2022_temperature_final_2014$population_actif_25_54

communes_dates_2014_2022_temperature_final_2014$part_chomage<-communes_dates_2014_2022_temperature_final_2014$value_estimated_au_chomage/communes_dates_2014_2022_temperature_final_2014$population_actif_25_54






communes_dates_2014_2022_temperature_final_2014$taux_mortalite_homme<-communes_dates_2014_2022_temperature_final_2014$Homme/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_homme

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_femme<-communes_dates_2014_2022_temperature_final_2014$Femme/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_femme


communes_dates_2014_2022_temperature_final_2014$taux_mortalite_0_9<-communes_dates_2014_2022_temperature_final_2014$`0-9`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_0_9_h_f

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_10_19<-communes_dates_2014_2022_temperature_final_2014$`10-19`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_10_19_h_f



communes_dates_2014_2022_temperature_final_2014$taux_mortalite_20_39<-communes_dates_2014_2022_temperature_final_2014$`20-39`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_20_39_h_f




communes_dates_2014_2022_temperature_final_2014$taux_mortalite_40_59<-communes_dates_2014_2022_temperature_final_2014$`40-59`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_40_59_h_f





communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_64<-communes_dates_2014_2022_temperature_final_2014$`60-64`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_60_64_h_f



communes_dates_2014_2022_temperature_final_2014$taux_mortalite_65_69<-communes_dates_2014_2022_temperature_final_2014$`65-69`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_65_69_h_f


communes_dates_2014_2022_temperature_final_2014$taux_mortalite_70_74<-communes_dates_2014_2022_temperature_final_2014$`70-74`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_70_74_h_f


communes_dates_2014_2022_temperature_final_2014$taux_mortalite_75_79<-communes_dates_2014_2022_temperature_final_2014$`75-79`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_75_79_h_f


communes_dates_2014_2022_temperature_final_2014$taux_mortalite_80_plus<-communes_dates_2014_2022_temperature_final_2014$`80+`/communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_80_plus_h_f


#communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_70<-(communes_dates_2014_2022_temperature_final_2014$`60-64`+communes_dates_2014_2022_temperature_final_2014$`65-69`)/(communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_60_64_h_f+communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_65_69_h_f)



communes_dates_2014_2022_temperature_final_2014$mort_total<-communes_dates_2014_2022_temperature_final_2014$Femme+communes_dates_2014_2022_temperature_final_2014$Homme


communes_dates_2014_2022_temperature_final_2014$taux_mortalite_total<-communes_dates_2014_2022_temperature_final_2014$mort_total/communes_dates_2014_2022_temperature_final_2014$value_estimated_population







communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_74<-(communes_dates_2014_2022_temperature_final_2014$`60-64`+communes_dates_2014_2022_temperature_final_2014$`65-69`+communes_dates_2014_2022_temperature_final_2014$`70-74`)/(communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_60_64_h_f+communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_65_69_h_f+communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_70_74_h_f)


communes_dates_2014_2022_temperature_final_2014$taux_mortalite_75_plus<-(communes_dates_2014_2022_temperature_final_2014$`75-79`+communes_dates_2014_2022_temperature_final_2014$`80+`)/(communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_75_79_h_f+communes_dates_2014_2022_temperature_final_2014$value_estimated_sum_80_plus_h_f)






#on enleve les communes avec des population de 0
communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014, communes_dates_2014_2022_temperature_final_2014$value_estimated_population>0)
communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014, communes_dates_2014_2022_temperature_final_2014$population_actif_25_54>0)




communes_dates_2014_2022_temperature_final_2014<- communes_dates_2014_2022_temperature_final_2014[ , !names(communes_dates_2014_2022_temperature_final_2014) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_femme))


communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(part_agriculteur))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(part_cadre))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(part_profession_intermediaire))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(part_employe))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(part_ouvrier))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(part_chomage))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_homme))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_0_9))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_10_19))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_20_39))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_40_59))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_60_64))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_65_69))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_70_74))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_75_79))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_total))




communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_60_74))
communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  !is.infinite(taux_mortalite_75_plus))







#761 valeurs inf

#

#communes_dates_2014_2022_temperature_final_2014<-communes_dates_2014_2022_temperature_final_2014[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  part_agriculteur<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_10_19<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  part_cadre<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  part_profession_intermediaire<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  part_employe<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  part_ouvrier<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  part_chomage<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_homme<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_femme<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_0_9<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_20_39<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_40_59<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_60_64<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_65_69<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_70_74<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_75_79<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_80_plus<=1)

#communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014,  taux_mortalite_total<=1)




communes_dates_2014_2022_temperature_final_2014$taux_mortalite_homme[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_homme>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_femme[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_femme>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_0_9[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_0_9>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_10_19[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_10_19>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_20_39[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_20_39>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_40_59[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_40_59>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_64[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_64>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_65_69[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_65_69>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_70_74[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_70_74>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_75_79[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_75_79>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_80_plus[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_80_plus>1]<-NA   

communes_dates_2014_2022_temperature_final_2014$taux_mortalite_total[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_total>1]<-NA   



communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_74[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_74>1]<-NA   
communes_dates_2014_2022_temperature_final_2014$taux_mortalite_75_plus[communes_dates_2014_2022_temperature_final_2014$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2014_2022_temperature_final_2014$part_agriculteur)

summary(communes_dates_2014_2022_temperature_final_2014$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2014_2022_temperature_final_2014$part_cadre)

summary(communes_dates_2014_2022_temperature_final_2014$part_profession_intermediaire)

summary(communes_dates_2014_2022_temperature_final_2014$part_employe)

summary(communes_dates_2014_2022_temperature_final_2014$part_ouvrier)

summary(communes_dates_2014_2022_temperature_final_2014$part_chomage)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_homme)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_femme)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_0_9)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_10_19)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_20_39)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_40_59)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_60_64)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_65_69)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_70_74)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_75_79)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_80_plus)

summary(communes_dates_2014_2022_temperature_final_2014$taux_mortalite_total)






fwrite(communes_dates_2014_2022_temperature_final_2014,"/données communes années/données mortalité temperature final mois new/communes_dates_2014_temperature_deces_mois.csv")











################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2015_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2015-12-31")

communes_dates_2015_2022_temperature_final_2015 <- communes_dates_2015_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2015_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2015_age_sexe.csv")





communes_dates_2015_2022_temperature_final_2015$date<-as.Date(communes_dates_2015_2022_temperature_final_2015$date)
deces.2015_age_sexe$date<-as.Date(deces.2015_age_sexe$date)






communes_dates_2015_2022_temperature_final_2015<-left_join(communes_dates_2015_2022_temperature_final_2015,deces.2015_age_sexe)

communes_dates_2015_2022_temperature_final_2015[is.na(communes_dates_2015_2022_temperature_final_2015)]<-0


communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 < -20]<-"<-20"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= -20 & communes_dates_2015_2022_temperature_final_2015$value1 < -15]<-"-20_-15"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= -15 & communes_dates_2015_2022_temperature_final_2015$value1 < -10]<-"-15_-10"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= -10 & communes_dates_2015_2022_temperature_final_2015$value1 < -5]<-"-10_-5"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= -5 & communes_dates_2015_2022_temperature_final_2015$value1 < 0]<-"-5_0"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 0 & communes_dates_2015_2022_temperature_final_2015$value1 < 5]<-"0_5"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 5 & communes_dates_2015_2022_temperature_final_2015$value1 < 10]<-"5_10"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 10 & communes_dates_2015_2022_temperature_final_2015$value1 < 15]<-"10_15"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 15 & communes_dates_2015_2022_temperature_final_2015$value1 < 20]<-"15_20"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 20 & communes_dates_2015_2022_temperature_final_2015$value1 < 25]<-"20_25"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 25 & communes_dates_2015_2022_temperature_final_2015$value1 < 28]<-"25_28"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 28 & communes_dates_2015_2022_temperature_final_2015$value1 < 30]<-"28_30"

communes_dates_2015_2022_temperature_final_2015$temperature_bin[communes_dates_2015_2022_temperature_final_2015$value1 >= 30]<-">30"


#test<-filter(communes_dates_2015_2022_temperature_final_2015, is.na(temperature_bin))
#table(communes_dates_2015_2022_temperature_final_2015$temperature_bin)

library(fastDummies)
communes_dates_2015_2022_temperature_final_2015  <- communes_dates_2015_2022_temperature_final_2015  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2015_2022_temperature_final_2015 <- communes_dates_2015_2022_temperature_final_2015 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2015_2022_temperature_final_2015 <- communes_dates_2015_2022_temperature_final_2015 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2015_2022_temperature_final_2015$same_value[is.na(communes_dates_2015_2022_temperature_final_2015$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2015_2022_temperature_final_2015$same_value <- ifelse(communes_dates_2015_2022_temperature_final_2015$temperature_bin != ">30", 0, communes_dates_2015_2022_temperature_final_2015$same_value)



communes_dates_2015_2022_temperature_final_2015$mois<-substring(communes_dates_2015_2022_temperature_final_2015$date,6,7)

communes_dates_2015_2022_temperature_final_2015<-communes_dates_2015_2022_temperature_final_2015[,-c("date","value1","temperature_bin")]

communes_dates_2015_2022_temperature_final_2015<-aggregate(.~COM+mois,communes_dates_2015_2022_temperature_final_2015,sum)



RP_2015_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2015_age_sexe_final_2")

RP_2015_age_sexe_final_2<-RP_2015_age_sexe_final_2[,c(2:15)]

names(RP_2015_age_sexe_final_2)[names(RP_2015_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2015_2022_temperature_final_2015<-left_join(communes_dates_2015_2022_temperature_final_2015,RP_2015_age_sexe_final_2)

#tests<-filter(communes_dates_2015_2022_temperature_final_2015, COM=="01001")
#tests<-filter(communes_dates_2015_2022_temperature_final_2015, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015, !is.na(value_estimated_sum_homme) )

RP_2015_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2015_CSP_final_2")

RP_2015_CSP_final_2<-RP_2015_CSP_final_2[,c(2:12)]
names(RP_2015_CSP_final_2)[names(RP_2015_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2015_CSP_final_2)[names(RP_2015_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2015_2022_temperature_final_2015<-left_join(communes_dates_2015_2022_temperature_final_2015,RP_2015_CSP_final_2)


communes_dates_2015_2022_temperature_final_2015$part_agriculteur<-communes_dates_2015_2022_temperature_final_2015$value_estimated_agriculteur/communes_dates_2015_2022_temperature_final_2015$population_actif_25_54

communes_dates_2015_2022_temperature_final_2015$part_artisan_commercant_chef_entreprise<-communes_dates_2015_2022_temperature_final_2015$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2015_2022_temperature_final_2015$population_actif_25_54

communes_dates_2015_2022_temperature_final_2015$part_cadre<-communes_dates_2015_2022_temperature_final_2015$value_estimated_cadre/communes_dates_2015_2022_temperature_final_2015$population_actif_25_54

communes_dates_2015_2022_temperature_final_2015$part_profession_intermediaire<-communes_dates_2015_2022_temperature_final_2015$value_estimated_profession_intermediaire/communes_dates_2015_2022_temperature_final_2015$population_actif_25_54

communes_dates_2015_2022_temperature_final_2015$part_employe<-communes_dates_2015_2022_temperature_final_2015$value_estimated_employe/communes_dates_2015_2022_temperature_final_2015$population_actif_25_54

communes_dates_2015_2022_temperature_final_2015$part_ouvrier<-communes_dates_2015_2022_temperature_final_2015$value_estimated_ouvrier/communes_dates_2015_2022_temperature_final_2015$population_actif_25_54

communes_dates_2015_2022_temperature_final_2015$part_chomage<-communes_dates_2015_2022_temperature_final_2015$value_estimated_au_chomage/communes_dates_2015_2022_temperature_final_2015$population_actif_25_54






communes_dates_2015_2022_temperature_final_2015$taux_mortalite_homme<-communes_dates_2015_2022_temperature_final_2015$Homme/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_homme

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_femme<-communes_dates_2015_2022_temperature_final_2015$Femme/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_femme


communes_dates_2015_2022_temperature_final_2015$taux_mortalite_0_9<-communes_dates_2015_2022_temperature_final_2015$`0-9`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_0_9_h_f

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_10_19<-communes_dates_2015_2022_temperature_final_2015$`10-19`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_10_19_h_f



communes_dates_2015_2022_temperature_final_2015$taux_mortalite_20_39<-communes_dates_2015_2022_temperature_final_2015$`20-39`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_20_39_h_f




communes_dates_2015_2022_temperature_final_2015$taux_mortalite_40_59<-communes_dates_2015_2022_temperature_final_2015$`40-59`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_40_59_h_f





communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_64<-communes_dates_2015_2022_temperature_final_2015$`60-64`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_60_64_h_f



communes_dates_2015_2022_temperature_final_2015$taux_mortalite_65_69<-communes_dates_2015_2022_temperature_final_2015$`65-69`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_65_69_h_f


communes_dates_2015_2022_temperature_final_2015$taux_mortalite_70_74<-communes_dates_2015_2022_temperature_final_2015$`70-74`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_70_74_h_f


communes_dates_2015_2022_temperature_final_2015$taux_mortalite_75_79<-communes_dates_2015_2022_temperature_final_2015$`75-79`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_75_79_h_f


communes_dates_2015_2022_temperature_final_2015$taux_mortalite_80_plus<-communes_dates_2015_2022_temperature_final_2015$`80+`/communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_80_plus_h_f


#communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_70<-(communes_dates_2015_2022_temperature_final_2015$`60-64`+communes_dates_2015_2022_temperature_final_2015$`65-69`)/(communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_60_64_h_f+communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_65_69_h_f)



communes_dates_2015_2022_temperature_final_2015$mort_total<-communes_dates_2015_2022_temperature_final_2015$Femme+communes_dates_2015_2022_temperature_final_2015$Homme


communes_dates_2015_2022_temperature_final_2015$taux_mortalite_total<-communes_dates_2015_2022_temperature_final_2015$mort_total/communes_dates_2015_2022_temperature_final_2015$value_estimated_population







communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_74<-(communes_dates_2015_2022_temperature_final_2015$`60-64`+communes_dates_2015_2022_temperature_final_2015$`65-69`+communes_dates_2015_2022_temperature_final_2015$`70-74`)/(communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_60_64_h_f+communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_65_69_h_f+communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_70_74_h_f)


communes_dates_2015_2022_temperature_final_2015$taux_mortalite_75_plus<-(communes_dates_2015_2022_temperature_final_2015$`75-79`+communes_dates_2015_2022_temperature_final_2015$`80+`)/(communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_75_79_h_f+communes_dates_2015_2022_temperature_final_2015$value_estimated_sum_80_plus_h_f)







#on enleve les communes avec des population de 0
communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015, communes_dates_2015_2022_temperature_final_2015$value_estimated_population>0)
communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015, communes_dates_2015_2022_temperature_final_2015$population_actif_25_54>0)




communes_dates_2015_2022_temperature_final_2015<- communes_dates_2015_2022_temperature_final_2015[ , !names(communes_dates_2015_2022_temperature_final_2015) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_femme))


communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(part_agriculteur))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(part_cadre))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(part_profession_intermediaire))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(part_employe))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(part_ouvrier))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(part_chomage))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_homme))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_0_9))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_10_19))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_20_39))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_40_59))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_60_64))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_65_69))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_70_74))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_75_79))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_total))




communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_60_74))
communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  !is.infinite(taux_mortalite_75_plus))







#761 valeurs inf

#

#communes_dates_2015_2022_temperature_final_2015<-communes_dates_2015_2022_temperature_final_2015[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  part_agriculteur<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_10_19<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  part_cadre<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  part_profession_intermediaire<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  part_employe<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  part_ouvrier<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  part_chomage<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_homme<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_femme<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_0_9<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_20_39<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_40_59<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_60_64<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_65_69<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_70_74<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_75_79<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_80_plus<=1)

#communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015,  taux_mortalite_total<=1)




communes_dates_2015_2022_temperature_final_2015$taux_mortalite_homme[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_homme>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_femme[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_femme>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_0_9[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_0_9>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_10_19[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_10_19>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_20_39[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_20_39>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_40_59[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_40_59>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_64[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_64>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_65_69[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_65_69>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_70_74[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_70_74>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_75_79[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_75_79>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_80_plus[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_80_plus>1]<-NA   

communes_dates_2015_2022_temperature_final_2015$taux_mortalite_total[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_total>1]<-NA   



communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_74[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_74>1]<-NA   
communes_dates_2015_2022_temperature_final_2015$taux_mortalite_75_plus[communes_dates_2015_2022_temperature_final_2015$taux_mortalite_75_plus>1]<-NA   





summary(communes_dates_2015_2022_temperature_final_2015$part_agriculteur)

summary(communes_dates_2015_2022_temperature_final_2015$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2015_2022_temperature_final_2015$part_cadre)

summary(communes_dates_2015_2022_temperature_final_2015$part_profession_intermediaire)

summary(communes_dates_2015_2022_temperature_final_2015$part_employe)

summary(communes_dates_2015_2022_temperature_final_2015$part_ouvrier)

summary(communes_dates_2015_2022_temperature_final_2015$part_chomage)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_homme)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_femme)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_0_9)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_10_19)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_20_39)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_40_59)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_60_64)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_65_69)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_70_74)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_75_79)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_80_plus)

summary(communes_dates_2015_2022_temperature_final_2015$taux_mortalite_total)






fwrite(communes_dates_2015_2022_temperature_final_2015,"/données communes années/données mortalité temperature final mois new/communes_dates_2015_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2016_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2016-01-01")
end_date <- as.Date("2016-12-31")

communes_dates_2016_2022_temperature_final_2016 <- communes_dates_2016_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2016_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2016_age_sexe.csv")





communes_dates_2016_2022_temperature_final_2016$date<-as.Date(communes_dates_2016_2022_temperature_final_2016$date)
deces.2016_age_sexe$date<-as.Date(deces.2016_age_sexe$date)








communes_dates_2016_2022_temperature_final_2016<-left_join(communes_dates_2016_2022_temperature_final_2016,deces.2016_age_sexe)

communes_dates_2016_2022_temperature_final_2016[is.na(communes_dates_2016_2022_temperature_final_2016)]<-0


communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 < -20]<-"<-20"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= -20 & communes_dates_2016_2022_temperature_final_2016$value1 < -15]<-"-20_-15"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= -15 & communes_dates_2016_2022_temperature_final_2016$value1 < -10]<-"-15_-10"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= -10 & communes_dates_2016_2022_temperature_final_2016$value1 < -5]<-"-10_-5"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= -5 & communes_dates_2016_2022_temperature_final_2016$value1 < 0]<-"-5_0"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 0 & communes_dates_2016_2022_temperature_final_2016$value1 < 5]<-"0_5"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 5 & communes_dates_2016_2022_temperature_final_2016$value1 < 10]<-"5_10"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 10 & communes_dates_2016_2022_temperature_final_2016$value1 < 15]<-"10_15"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 15 & communes_dates_2016_2022_temperature_final_2016$value1 < 20]<-"15_20"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 20 & communes_dates_2016_2022_temperature_final_2016$value1 < 25]<-"20_25"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 25 & communes_dates_2016_2022_temperature_final_2016$value1 < 28]<-"25_28"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 28 & communes_dates_2016_2022_temperature_final_2016$value1 < 30]<-"28_30"

communes_dates_2016_2022_temperature_final_2016$temperature_bin[communes_dates_2016_2022_temperature_final_2016$value1 >= 30]<-">30"


#test<-filter(communes_dates_2016_2022_temperature_final_2016, is.na(temperature_bin))
#table(communes_dates_2016_2022_temperature_final_2016$temperature_bin)

library(fastDummies)
communes_dates_2016_2022_temperature_final_2016  <- communes_dates_2016_2022_temperature_final_2016  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2016_2022_temperature_final_2016 <- communes_dates_2016_2022_temperature_final_2016 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2016_2022_temperature_final_2016 <- communes_dates_2016_2022_temperature_final_2016 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2016_2022_temperature_final_2016$same_value[is.na(communes_dates_2016_2022_temperature_final_2016$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2016_2022_temperature_final_2016$same_value <- ifelse(communes_dates_2016_2022_temperature_final_2016$temperature_bin != ">30", 0, communes_dates_2016_2022_temperature_final_2016$same_value)



communes_dates_2016_2022_temperature_final_2016$mois<-substring(communes_dates_2016_2022_temperature_final_2016$date,6,7)

communes_dates_2016_2022_temperature_final_2016<-communes_dates_2016_2022_temperature_final_2016[,-c("date","value1","temperature_bin")]

communes_dates_2016_2022_temperature_final_2016<-aggregate(.~COM+mois,communes_dates_2016_2022_temperature_final_2016,sum)



RP_2016_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2016_age_sexe_final_2")

RP_2016_age_sexe_final_2<-RP_2016_age_sexe_final_2[,c(2:15)]

names(RP_2016_age_sexe_final_2)[names(RP_2016_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2016_2022_temperature_final_2016<-left_join(communes_dates_2016_2022_temperature_final_2016,RP_2016_age_sexe_final_2)

#tests<-filter(communes_dates_2016_2022_temperature_final_2016, COM=="01001")
#tests<-filter(communes_dates_2016_2022_temperature_final_2016, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016, !is.na(value_estimated_sum_homme) )

RP_2016_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2016_CSP_final_2")

RP_2016_CSP_final_2<-RP_2016_CSP_final_2[,c(2:12)]
names(RP_2016_CSP_final_2)[names(RP_2016_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2016_CSP_final_2)[names(RP_2016_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2016_2022_temperature_final_2016<-left_join(communes_dates_2016_2022_temperature_final_2016,RP_2016_CSP_final_2)


communes_dates_2016_2022_temperature_final_2016$part_agriculteur<-communes_dates_2016_2022_temperature_final_2016$value_estimated_agriculteur/communes_dates_2016_2022_temperature_final_2016$population_actif_25_54

communes_dates_2016_2022_temperature_final_2016$part_artisan_commercant_chef_entreprise<-communes_dates_2016_2022_temperature_final_2016$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2016_2022_temperature_final_2016$population_actif_25_54

communes_dates_2016_2022_temperature_final_2016$part_cadre<-communes_dates_2016_2022_temperature_final_2016$value_estimated_cadre/communes_dates_2016_2022_temperature_final_2016$population_actif_25_54

communes_dates_2016_2022_temperature_final_2016$part_profession_intermediaire<-communes_dates_2016_2022_temperature_final_2016$value_estimated_profession_intermediaire/communes_dates_2016_2022_temperature_final_2016$population_actif_25_54

communes_dates_2016_2022_temperature_final_2016$part_employe<-communes_dates_2016_2022_temperature_final_2016$value_estimated_employe/communes_dates_2016_2022_temperature_final_2016$population_actif_25_54

communes_dates_2016_2022_temperature_final_2016$part_ouvrier<-communes_dates_2016_2022_temperature_final_2016$value_estimated_ouvrier/communes_dates_2016_2022_temperature_final_2016$population_actif_25_54

communes_dates_2016_2022_temperature_final_2016$part_chomage<-communes_dates_2016_2022_temperature_final_2016$value_estimated_au_chomage/communes_dates_2016_2022_temperature_final_2016$population_actif_25_54






communes_dates_2016_2022_temperature_final_2016$taux_mortalite_homme<-communes_dates_2016_2022_temperature_final_2016$Homme/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_homme

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_femme<-communes_dates_2016_2022_temperature_final_2016$Femme/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_femme


communes_dates_2016_2022_temperature_final_2016$taux_mortalite_0_9<-communes_dates_2016_2022_temperature_final_2016$`0-9`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_0_9_h_f

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_10_19<-communes_dates_2016_2022_temperature_final_2016$`10-19`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_10_19_h_f



communes_dates_2016_2022_temperature_final_2016$taux_mortalite_20_39<-communes_dates_2016_2022_temperature_final_2016$`20-39`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_20_39_h_f




communes_dates_2016_2022_temperature_final_2016$taux_mortalite_40_59<-communes_dates_2016_2022_temperature_final_2016$`40-59`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_40_59_h_f





communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_64<-communes_dates_2016_2022_temperature_final_2016$`60-64`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_60_64_h_f



communes_dates_2016_2022_temperature_final_2016$taux_mortalite_65_69<-communes_dates_2016_2022_temperature_final_2016$`65-69`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_65_69_h_f


communes_dates_2016_2022_temperature_final_2016$taux_mortalite_70_74<-communes_dates_2016_2022_temperature_final_2016$`70-74`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_70_74_h_f


communes_dates_2016_2022_temperature_final_2016$taux_mortalite_75_79<-communes_dates_2016_2022_temperature_final_2016$`75-79`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_75_79_h_f


communes_dates_2016_2022_temperature_final_2016$taux_mortalite_80_plus<-communes_dates_2016_2022_temperature_final_2016$`80+`/communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_80_plus_h_f


#communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_70<-(communes_dates_2016_2022_temperature_final_2016$`60-64`+communes_dates_2016_2022_temperature_final_2016$`65-69`)/(communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_60_64_h_f+communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_65_69_h_f)



communes_dates_2016_2022_temperature_final_2016$mort_total<-communes_dates_2016_2022_temperature_final_2016$Femme+communes_dates_2016_2022_temperature_final_2016$Homme


communes_dates_2016_2022_temperature_final_2016$taux_mortalite_total<-communes_dates_2016_2022_temperature_final_2016$mort_total/communes_dates_2016_2022_temperature_final_2016$value_estimated_population







communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_74<-(communes_dates_2016_2022_temperature_final_2016$`60-64`+communes_dates_2016_2022_temperature_final_2016$`65-69`+communes_dates_2016_2022_temperature_final_2016$`70-74`)/(communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_60_64_h_f+communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_65_69_h_f+communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_70_74_h_f)


communes_dates_2016_2022_temperature_final_2016$taux_mortalite_75_plus<-(communes_dates_2016_2022_temperature_final_2016$`75-79`+communes_dates_2016_2022_temperature_final_2016$`80+`)/(communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_75_79_h_f+communes_dates_2016_2022_temperature_final_2016$value_estimated_sum_80_plus_h_f)







#on enleve les communes avec des population de 0
communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016, communes_dates_2016_2022_temperature_final_2016$value_estimated_population>0)
communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016, communes_dates_2016_2022_temperature_final_2016$population_actif_25_54>0)




communes_dates_2016_2022_temperature_final_2016<- communes_dates_2016_2022_temperature_final_2016[ , !names(communes_dates_2016_2022_temperature_final_2016) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_femme))


communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(part_agriculteur))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(part_cadre))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(part_profession_intermediaire))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(part_employe))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(part_ouvrier))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(part_chomage))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_homme))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_0_9))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_10_19))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_20_39))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_40_59))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_60_64))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_65_69))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_70_74))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_75_79))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_total))





communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_60_74))
communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  !is.infinite(taux_mortalite_75_plus))







#761 valeurs inf

#

#communes_dates_2016_2022_temperature_final_2016<-communes_dates_2016_2022_temperature_final_2016[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  part_agriculteur<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_10_19<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  part_cadre<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  part_profession_intermediaire<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  part_employe<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  part_ouvrier<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  part_chomage<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_homme<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_femme<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_0_9<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_20_39<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_40_59<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_60_64<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_65_69<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_70_74<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_75_79<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_80_plus<=1)

#communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016,  taux_mortalite_total<=1)




communes_dates_2016_2022_temperature_final_2016$taux_mortalite_homme[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_homme>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_femme[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_femme>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_0_9[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_0_9>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_10_19[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_10_19>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_20_39[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_20_39>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_40_59[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_40_59>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_64[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_64>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_65_69[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_65_69>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_70_74[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_70_74>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_75_79[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_75_79>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_80_plus[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_80_plus>1]<-NA   

communes_dates_2016_2022_temperature_final_2016$taux_mortalite_total[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_total>1]<-NA   




communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_74[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_74>1]<-NA   
communes_dates_2016_2022_temperature_final_2016$taux_mortalite_75_plus[communes_dates_2016_2022_temperature_final_2016$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2016_2022_temperature_final_2016$part_agriculteur)

summary(communes_dates_2016_2022_temperature_final_2016$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2016_2022_temperature_final_2016$part_cadre)

summary(communes_dates_2016_2022_temperature_final_2016$part_profession_intermediaire)

summary(communes_dates_2016_2022_temperature_final_2016$part_employe)

summary(communes_dates_2016_2022_temperature_final_2016$part_ouvrier)

summary(communes_dates_2016_2022_temperature_final_2016$part_chomage)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_homme)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_femme)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_0_9)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_10_19)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_20_39)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_40_59)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_60_64)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_65_69)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_70_74)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_75_79)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_80_plus)

summary(communes_dates_2016_2022_temperature_final_2016$taux_mortalite_total)






fwrite(communes_dates_2016_2022_temperature_final_2016,"/données communes années/données mortalité temperature final mois new/communes_dates_2016_temperature_deces_mois.csv")










################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2017_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2017-01-01")
end_date <- as.Date("2017-12-31")

communes_dates_2017_2022_temperature_final_2017 <- communes_dates_2017_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2017_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2017_age_sexe.csv")




communes_dates_2017_2022_temperature_final_2017$date<-as.Date(communes_dates_2017_2022_temperature_final_2017$date)
deces.2017_age_sexe$date<-as.Date(deces.2017_age_sexe$date)





communes_dates_2017_2022_temperature_final_2017<-left_join(communes_dates_2017_2022_temperature_final_2017,deces.2017_age_sexe)

communes_dates_2017_2022_temperature_final_2017[is.na(communes_dates_2017_2022_temperature_final_2017)]<-0


communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 < -20]<-"<-20"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= -20 & communes_dates_2017_2022_temperature_final_2017$value1 < -15]<-"-20_-15"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= -15 & communes_dates_2017_2022_temperature_final_2017$value1 < -10]<-"-15_-10"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= -10 & communes_dates_2017_2022_temperature_final_2017$value1 < -5]<-"-10_-5"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= -5 & communes_dates_2017_2022_temperature_final_2017$value1 < 0]<-"-5_0"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 0 & communes_dates_2017_2022_temperature_final_2017$value1 < 5]<-"0_5"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 5 & communes_dates_2017_2022_temperature_final_2017$value1 < 10]<-"5_10"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 10 & communes_dates_2017_2022_temperature_final_2017$value1 < 15]<-"10_15"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 15 & communes_dates_2017_2022_temperature_final_2017$value1 < 20]<-"15_20"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 20 & communes_dates_2017_2022_temperature_final_2017$value1 < 25]<-"20_25"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 25 & communes_dates_2017_2022_temperature_final_2017$value1 < 28]<-"25_28"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 28 & communes_dates_2017_2022_temperature_final_2017$value1 < 30]<-"28_30"

communes_dates_2017_2022_temperature_final_2017$temperature_bin[communes_dates_2017_2022_temperature_final_2017$value1 >= 30]<-">30"


#test<-filter(communes_dates_2017_2022_temperature_final_2017, is.na(temperature_bin))
#table(communes_dates_2017_2022_temperature_final_2017$temperature_bin)

library(fastDummies)
communes_dates_2017_2022_temperature_final_2017  <- communes_dates_2017_2022_temperature_final_2017  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2017_2022_temperature_final_2017 <- communes_dates_2017_2022_temperature_final_2017 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2017_2022_temperature_final_2017 <- communes_dates_2017_2022_temperature_final_2017 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2017_2022_temperature_final_2017$same_value[is.na(communes_dates_2017_2022_temperature_final_2017$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2017_2022_temperature_final_2017$same_value <- ifelse(communes_dates_2017_2022_temperature_final_2017$temperature_bin != ">30", 0, communes_dates_2017_2022_temperature_final_2017$same_value)



communes_dates_2017_2022_temperature_final_2017$mois<-substring(communes_dates_2017_2022_temperature_final_2017$date,6,7)

communes_dates_2017_2022_temperature_final_2017<-communes_dates_2017_2022_temperature_final_2017[,-c("date","value1","temperature_bin")]

communes_dates_2017_2022_temperature_final_2017<-aggregate(.~COM+mois,communes_dates_2017_2022_temperature_final_2017,sum)



RP_2017_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2017_age_sexe_final_2")

RP_2017_age_sexe_final_2<-RP_2017_age_sexe_final_2[,c(2:15)]

names(RP_2017_age_sexe_final_2)[names(RP_2017_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2017_2022_temperature_final_2017<-left_join(communes_dates_2017_2022_temperature_final_2017,RP_2017_age_sexe_final_2)

#tests<-filter(communes_dates_2017_2022_temperature_final_2017, COM=="01001")
#tests<-filter(communes_dates_2017_2022_temperature_final_2017, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017, !is.na(value_estimated_sum_homme) )

RP_2017_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2017_CSP_final_2")

RP_2017_CSP_final_2<-RP_2017_CSP_final_2[,c(2:12)]
names(RP_2017_CSP_final_2)[names(RP_2017_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2017_CSP_final_2)[names(RP_2017_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2017_2022_temperature_final_2017<-left_join(communes_dates_2017_2022_temperature_final_2017,RP_2017_CSP_final_2)


communes_dates_2017_2022_temperature_final_2017$part_agriculteur<-communes_dates_2017_2022_temperature_final_2017$value_estimated_agriculteur/communes_dates_2017_2022_temperature_final_2017$population_actif_25_54

communes_dates_2017_2022_temperature_final_2017$part_artisan_commercant_chef_entreprise<-communes_dates_2017_2022_temperature_final_2017$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2017_2022_temperature_final_2017$population_actif_25_54

communes_dates_2017_2022_temperature_final_2017$part_cadre<-communes_dates_2017_2022_temperature_final_2017$value_estimated_cadre/communes_dates_2017_2022_temperature_final_2017$population_actif_25_54

communes_dates_2017_2022_temperature_final_2017$part_profession_intermediaire<-communes_dates_2017_2022_temperature_final_2017$value_estimated_profession_intermediaire/communes_dates_2017_2022_temperature_final_2017$population_actif_25_54

communes_dates_2017_2022_temperature_final_2017$part_employe<-communes_dates_2017_2022_temperature_final_2017$value_estimated_employe/communes_dates_2017_2022_temperature_final_2017$population_actif_25_54

communes_dates_2017_2022_temperature_final_2017$part_ouvrier<-communes_dates_2017_2022_temperature_final_2017$value_estimated_ouvrier/communes_dates_2017_2022_temperature_final_2017$population_actif_25_54

communes_dates_2017_2022_temperature_final_2017$part_chomage<-communes_dates_2017_2022_temperature_final_2017$value_estimated_au_chomage/communes_dates_2017_2022_temperature_final_2017$population_actif_25_54






communes_dates_2017_2022_temperature_final_2017$taux_mortalite_homme<-communes_dates_2017_2022_temperature_final_2017$Homme/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_homme

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_femme<-communes_dates_2017_2022_temperature_final_2017$Femme/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_femme


communes_dates_2017_2022_temperature_final_2017$taux_mortalite_0_9<-communes_dates_2017_2022_temperature_final_2017$`0-9`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_0_9_h_f

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_10_19<-communes_dates_2017_2022_temperature_final_2017$`10-19`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_10_19_h_f



communes_dates_2017_2022_temperature_final_2017$taux_mortalite_20_39<-communes_dates_2017_2022_temperature_final_2017$`20-39`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_20_39_h_f




communes_dates_2017_2022_temperature_final_2017$taux_mortalite_40_59<-communes_dates_2017_2022_temperature_final_2017$`40-59`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_40_59_h_f





communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_64<-communes_dates_2017_2022_temperature_final_2017$`60-64`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_60_64_h_f



communes_dates_2017_2022_temperature_final_2017$taux_mortalite_65_69<-communes_dates_2017_2022_temperature_final_2017$`65-69`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_65_69_h_f


communes_dates_2017_2022_temperature_final_2017$taux_mortalite_70_74<-communes_dates_2017_2022_temperature_final_2017$`70-74`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_70_74_h_f


communes_dates_2017_2022_temperature_final_2017$taux_mortalite_75_79<-communes_dates_2017_2022_temperature_final_2017$`75-79`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_75_79_h_f


communes_dates_2017_2022_temperature_final_2017$taux_mortalite_80_plus<-communes_dates_2017_2022_temperature_final_2017$`80+`/communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_80_plus_h_f


#communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_70<-(communes_dates_2017_2022_temperature_final_2017$`60-64`+communes_dates_2017_2022_temperature_final_2017$`65-69`)/(communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_60_64_h_f+communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_65_69_h_f)



communes_dates_2017_2022_temperature_final_2017$mort_total<-communes_dates_2017_2022_temperature_final_2017$Femme+communes_dates_2017_2022_temperature_final_2017$Homme


communes_dates_2017_2022_temperature_final_2017$taux_mortalite_total<-communes_dates_2017_2022_temperature_final_2017$mort_total/communes_dates_2017_2022_temperature_final_2017$value_estimated_population






communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_74<-(communes_dates_2017_2022_temperature_final_2017$`60-64`+communes_dates_2017_2022_temperature_final_2017$`65-69`+communes_dates_2017_2022_temperature_final_2017$`70-74`)/(communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_60_64_h_f+communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_65_69_h_f+communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_70_74_h_f)


communes_dates_2017_2022_temperature_final_2017$taux_mortalite_75_plus<-(communes_dates_2017_2022_temperature_final_2017$`75-79`+communes_dates_2017_2022_temperature_final_2017$`80+`)/(communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_75_79_h_f+communes_dates_2017_2022_temperature_final_2017$value_estimated_sum_80_plus_h_f)









#on enleve les communes avec des population de 0
communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017, communes_dates_2017_2022_temperature_final_2017$value_estimated_population>0)
communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017, communes_dates_2017_2022_temperature_final_2017$population_actif_25_54>0)




communes_dates_2017_2022_temperature_final_2017<- communes_dates_2017_2022_temperature_final_2017[ , !names(communes_dates_2017_2022_temperature_final_2017) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_femme))


communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(part_agriculteur))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(part_cadre))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(part_profession_intermediaire))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(part_employe))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(part_ouvrier))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(part_chomage))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_homme))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_0_9))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_10_19))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_20_39))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_40_59))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_60_64))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_65_69))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_70_74))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_75_79))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_total))




communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_60_74))
communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  !is.infinite(taux_mortalite_75_plus))








#761 valeurs inf

#

#communes_dates_2017_2022_temperature_final_2017<-communes_dates_2017_2022_temperature_final_2017[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  part_agriculteur<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_10_19<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  part_cadre<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  part_profession_intermediaire<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  part_employe<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  part_ouvrier<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  part_chomage<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_homme<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_femme<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_0_9<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_20_39<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_40_59<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_60_64<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_65_69<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_70_74<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_75_79<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_80_plus<=1)

#communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017,  taux_mortalite_total<=1)




communes_dates_2017_2022_temperature_final_2017$taux_mortalite_homme[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_homme>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_femme[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_femme>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_0_9[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_0_9>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_10_19[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_10_19>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_20_39[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_20_39>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_40_59[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_40_59>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_64[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_64>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_65_69[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_65_69>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_70_74[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_70_74>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_75_79[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_75_79>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_80_plus[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_80_plus>1]<-NA   

communes_dates_2017_2022_temperature_final_2017$taux_mortalite_total[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_total>1]<-NA   




communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_74[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_74>1]<-NA   
communes_dates_2017_2022_temperature_final_2017$taux_mortalite_75_plus[communes_dates_2017_2022_temperature_final_2017$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2017_2022_temperature_final_2017$part_agriculteur)

summary(communes_dates_2017_2022_temperature_final_2017$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2017_2022_temperature_final_2017$part_cadre)

summary(communes_dates_2017_2022_temperature_final_2017$part_profession_intermediaire)

summary(communes_dates_2017_2022_temperature_final_2017$part_employe)

summary(communes_dates_2017_2022_temperature_final_2017$part_ouvrier)

summary(communes_dates_2017_2022_temperature_final_2017$part_chomage)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_homme)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_femme)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_0_9)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_10_19)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_20_39)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_40_59)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_60_64)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_65_69)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_70_74)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_75_79)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_80_plus)

summary(communes_dates_2017_2022_temperature_final_2017$taux_mortalite_total)






fwrite(communes_dates_2017_2022_temperature_final_2017,"/données communes années/données mortalité temperature final mois new/communes_dates_2017_temperature_deces_mois.csv")









################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2018_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2018-12-31")

communes_dates_2018_2022_temperature_final_2018 <- communes_dates_2018_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2018_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2018_age_sexe.csv")





communes_dates_2018_2022_temperature_final_2018$date<-as.Date(communes_dates_2018_2022_temperature_final_2018$date)
deces.2018_age_sexe$date<-as.Date(deces.2018_age_sexe$date)







communes_dates_2018_2022_temperature_final_2018<-left_join(communes_dates_2018_2022_temperature_final_2018,deces.2018_age_sexe)

communes_dates_2018_2022_temperature_final_2018[is.na(communes_dates_2018_2022_temperature_final_2018)]<-0


communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 < -20]<-"<-20"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= -20 & communes_dates_2018_2022_temperature_final_2018$value1 < -15]<-"-20_-15"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= -15 & communes_dates_2018_2022_temperature_final_2018$value1 < -10]<-"-15_-10"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= -10 & communes_dates_2018_2022_temperature_final_2018$value1 < -5]<-"-10_-5"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= -5 & communes_dates_2018_2022_temperature_final_2018$value1 < 0]<-"-5_0"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 0 & communes_dates_2018_2022_temperature_final_2018$value1 < 5]<-"0_5"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 5 & communes_dates_2018_2022_temperature_final_2018$value1 < 10]<-"5_10"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 10 & communes_dates_2018_2022_temperature_final_2018$value1 < 15]<-"10_15"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 15 & communes_dates_2018_2022_temperature_final_2018$value1 < 20]<-"15_20"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 20 & communes_dates_2018_2022_temperature_final_2018$value1 < 25]<-"20_25"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 25 & communes_dates_2018_2022_temperature_final_2018$value1 < 28]<-"25_28"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 28 & communes_dates_2018_2022_temperature_final_2018$value1 < 30]<-"28_30"

communes_dates_2018_2022_temperature_final_2018$temperature_bin[communes_dates_2018_2022_temperature_final_2018$value1 >= 30]<-">30"


#test<-filter(communes_dates_2018_2022_temperature_final_2018, is.na(temperature_bin))
#table(communes_dates_2018_2022_temperature_final_2018$temperature_bin)

library(fastDummies)
communes_dates_2018_2022_temperature_final_2018  <- communes_dates_2018_2022_temperature_final_2018  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2018_2022_temperature_final_2018 <- communes_dates_2018_2022_temperature_final_2018 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2018_2022_temperature_final_2018 <- communes_dates_2018_2022_temperature_final_2018 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2018_2022_temperature_final_2018$same_value[is.na(communes_dates_2018_2022_temperature_final_2018$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2018_2022_temperature_final_2018$same_value <- ifelse(communes_dates_2018_2022_temperature_final_2018$temperature_bin != ">30", 0, communes_dates_2018_2022_temperature_final_2018$same_value)



communes_dates_2018_2022_temperature_final_2018$mois<-substring(communes_dates_2018_2022_temperature_final_2018$date,6,7)

communes_dates_2018_2022_temperature_final_2018<-communes_dates_2018_2022_temperature_final_2018[,-c("date","value1","temperature_bin")]

communes_dates_2018_2022_temperature_final_2018<-aggregate(.~COM+mois,communes_dates_2018_2022_temperature_final_2018,sum)



RP_2018_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2018_age_sexe_final_2")

RP_2018_age_sexe_final_2<-RP_2018_age_sexe_final_2[,c(2:15)]

names(RP_2018_age_sexe_final_2)[names(RP_2018_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2018_2022_temperature_final_2018<-left_join(communes_dates_2018_2022_temperature_final_2018,RP_2018_age_sexe_final_2)

#tests<-filter(communes_dates_2018_2022_temperature_final_2018, COM=="01001")
#tests<-filter(communes_dates_2018_2022_temperature_final_2018, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018, !is.na(value_estimated_sum_homme) )

RP_2018_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2018_CSP_final_2")

RP_2018_CSP_final_2<-RP_2018_CSP_final_2[,c(2:12)]
names(RP_2018_CSP_final_2)[names(RP_2018_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2018_CSP_final_2)[names(RP_2018_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2018_2022_temperature_final_2018<-left_join(communes_dates_2018_2022_temperature_final_2018,RP_2018_CSP_final_2)


communes_dates_2018_2022_temperature_final_2018$part_agriculteur<-communes_dates_2018_2022_temperature_final_2018$value_estimated_agriculteur/communes_dates_2018_2022_temperature_final_2018$population_actif_25_54

communes_dates_2018_2022_temperature_final_2018$part_artisan_commercant_chef_entreprise<-communes_dates_2018_2022_temperature_final_2018$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2018_2022_temperature_final_2018$population_actif_25_54

communes_dates_2018_2022_temperature_final_2018$part_cadre<-communes_dates_2018_2022_temperature_final_2018$value_estimated_cadre/communes_dates_2018_2022_temperature_final_2018$population_actif_25_54

communes_dates_2018_2022_temperature_final_2018$part_profession_intermediaire<-communes_dates_2018_2022_temperature_final_2018$value_estimated_profession_intermediaire/communes_dates_2018_2022_temperature_final_2018$population_actif_25_54

communes_dates_2018_2022_temperature_final_2018$part_employe<-communes_dates_2018_2022_temperature_final_2018$value_estimated_employe/communes_dates_2018_2022_temperature_final_2018$population_actif_25_54

communes_dates_2018_2022_temperature_final_2018$part_ouvrier<-communes_dates_2018_2022_temperature_final_2018$value_estimated_ouvrier/communes_dates_2018_2022_temperature_final_2018$population_actif_25_54

communes_dates_2018_2022_temperature_final_2018$part_chomage<-communes_dates_2018_2022_temperature_final_2018$value_estimated_au_chomage/communes_dates_2018_2022_temperature_final_2018$population_actif_25_54






communes_dates_2018_2022_temperature_final_2018$taux_mortalite_homme<-communes_dates_2018_2022_temperature_final_2018$Homme/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_homme

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_femme<-communes_dates_2018_2022_temperature_final_2018$Femme/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_femme


communes_dates_2018_2022_temperature_final_2018$taux_mortalite_0_9<-communes_dates_2018_2022_temperature_final_2018$`0-9`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_0_9_h_f

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_10_19<-communes_dates_2018_2022_temperature_final_2018$`10-19`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_10_19_h_f



communes_dates_2018_2022_temperature_final_2018$taux_mortalite_20_39<-communes_dates_2018_2022_temperature_final_2018$`20-39`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_20_39_h_f




communes_dates_2018_2022_temperature_final_2018$taux_mortalite_40_59<-communes_dates_2018_2022_temperature_final_2018$`40-59`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_40_59_h_f





communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_64<-communes_dates_2018_2022_temperature_final_2018$`60-64`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_60_64_h_f



communes_dates_2018_2022_temperature_final_2018$taux_mortalite_65_69<-communes_dates_2018_2022_temperature_final_2018$`65-69`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_65_69_h_f


communes_dates_2018_2022_temperature_final_2018$taux_mortalite_70_74<-communes_dates_2018_2022_temperature_final_2018$`70-74`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_70_74_h_f


communes_dates_2018_2022_temperature_final_2018$taux_mortalite_75_79<-communes_dates_2018_2022_temperature_final_2018$`75-79`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_75_79_h_f


communes_dates_2018_2022_temperature_final_2018$taux_mortalite_80_plus<-communes_dates_2018_2022_temperature_final_2018$`80+`/communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_80_plus_h_f


#communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_70<-(communes_dates_2018_2022_temperature_final_2018$`60-64`+communes_dates_2018_2022_temperature_final_2018$`65-69`)/(communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_60_64_h_f+communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_65_69_h_f)



communes_dates_2018_2022_temperature_final_2018$mort_total<-communes_dates_2018_2022_temperature_final_2018$Femme+communes_dates_2018_2022_temperature_final_2018$Homme


communes_dates_2018_2022_temperature_final_2018$taux_mortalite_total<-communes_dates_2018_2022_temperature_final_2018$mort_total/communes_dates_2018_2022_temperature_final_2018$value_estimated_population





communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_74<-(communes_dates_2018_2022_temperature_final_2018$`60-64`+communes_dates_2018_2022_temperature_final_2018$`65-69`+communes_dates_2018_2022_temperature_final_2018$`70-74`)/(communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_60_64_h_f+communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_65_69_h_f+communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_70_74_h_f)


communes_dates_2018_2022_temperature_final_2018$taux_mortalite_75_plus<-(communes_dates_2018_2022_temperature_final_2018$`75-79`+communes_dates_2018_2022_temperature_final_2018$`80+`)/(communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_75_79_h_f+communes_dates_2018_2022_temperature_final_2018$value_estimated_sum_80_plus_h_f)










#on enleve les communes avec des population de 0
communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018, communes_dates_2018_2022_temperature_final_2018$value_estimated_population>0)
communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018, communes_dates_2018_2022_temperature_final_2018$population_actif_25_54>0)




communes_dates_2018_2022_temperature_final_2018<- communes_dates_2018_2022_temperature_final_2018[ , !names(communes_dates_2018_2022_temperature_final_2018) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_femme))


communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(part_agriculteur))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(part_cadre))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(part_profession_intermediaire))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(part_employe))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(part_ouvrier))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(part_chomage))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_homme))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_0_9))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_10_19))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_20_39))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_40_59))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_60_64))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_65_69))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_70_74))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_75_79))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_total))





communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_60_74))
communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  !is.infinite(taux_mortalite_75_plus))






#761 valeurs inf

#

#communes_dates_2018_2022_temperature_final_2018<-communes_dates_2018_2022_temperature_final_2018[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  part_agriculteur<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_10_19<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  part_cadre<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  part_profession_intermediaire<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  part_employe<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  part_ouvrier<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  part_chomage<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_homme<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_femme<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_0_9<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_20_39<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_40_59<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_60_64<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_65_69<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_70_74<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_75_79<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_80_plus<=1)

#communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018,  taux_mortalite_total<=1)




communes_dates_2018_2022_temperature_final_2018$taux_mortalite_homme[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_homme>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_femme[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_femme>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_0_9[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_0_9>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_10_19[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_10_19>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_20_39[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_20_39>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_40_59[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_40_59>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_64[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_64>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_65_69[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_65_69>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_70_74[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_70_74>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_75_79[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_75_79>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_80_plus[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_80_plus>1]<-NA   

communes_dates_2018_2022_temperature_final_2018$taux_mortalite_total[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_total>1]<-NA   




communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_74[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_74>1]<-NA   
communes_dates_2018_2022_temperature_final_2018$taux_mortalite_75_plus[communes_dates_2018_2022_temperature_final_2018$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2018_2022_temperature_final_2018$part_agriculteur)

summary(communes_dates_2018_2022_temperature_final_2018$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2018_2022_temperature_final_2018$part_cadre)

summary(communes_dates_2018_2022_temperature_final_2018$part_profession_intermediaire)

summary(communes_dates_2018_2022_temperature_final_2018$part_employe)

summary(communes_dates_2018_2022_temperature_final_2018$part_ouvrier)

summary(communes_dates_2018_2022_temperature_final_2018$part_chomage)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_homme)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_femme)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_0_9)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_10_19)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_20_39)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_40_59)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_60_64)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_65_69)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_70_74)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_75_79)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_80_plus)

summary(communes_dates_2018_2022_temperature_final_2018$taux_mortalite_total)






fwrite(communes_dates_2018_2022_temperature_final_2018,"/données communes années/données mortalité temperature final mois new/communes_dates_2018_temperature_deces_mois.csv")












################








rm(list = ls())
gc()








library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)




#
#
#rbind le tout

communes_dates_2019_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2019-12-31")

communes_dates_2019_2022_temperature_final_2019 <- communes_dates_2019_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2019_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2019_age_sexe.csv")




communes_dates_2019_2022_temperature_final_2019$date<-as.Date(communes_dates_2019_2022_temperature_final_2019$date)
deces.2019_age_sexe$date<-as.Date(deces.2019_age_sexe$date)





communes_dates_2019_2022_temperature_final_2019<-left_join(communes_dates_2019_2022_temperature_final_2019,deces.2019_age_sexe)

communes_dates_2019_2022_temperature_final_2019[is.na(communes_dates_2019_2022_temperature_final_2019)]<-0


communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 < -20]<-"<-20"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= -20 & communes_dates_2019_2022_temperature_final_2019$value1 < -15]<-"-20_-15"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= -15 & communes_dates_2019_2022_temperature_final_2019$value1 < -10]<-"-15_-10"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= -10 & communes_dates_2019_2022_temperature_final_2019$value1 < -5]<-"-10_-5"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= -5 & communes_dates_2019_2022_temperature_final_2019$value1 < 0]<-"-5_0"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 0 & communes_dates_2019_2022_temperature_final_2019$value1 < 5]<-"0_5"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 5 & communes_dates_2019_2022_temperature_final_2019$value1 < 10]<-"5_10"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 10 & communes_dates_2019_2022_temperature_final_2019$value1 < 15]<-"10_15"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 15 & communes_dates_2019_2022_temperature_final_2019$value1 < 20]<-"15_20"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 20 & communes_dates_2019_2022_temperature_final_2019$value1 < 25]<-"20_25"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 25 & communes_dates_2019_2022_temperature_final_2019$value1 < 28]<-"25_28"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 28 & communes_dates_2019_2022_temperature_final_2019$value1 < 30]<-"28_30"

communes_dates_2019_2022_temperature_final_2019$temperature_bin[communes_dates_2019_2022_temperature_final_2019$value1 >= 30]<-">30"


#test<-filter(communes_dates_2019_2022_temperature_final_2019, is.na(temperature_bin))
#table(communes_dates_2019_2022_temperature_final_2019$temperature_bin)

library(fastDummies)
communes_dates_2019_2022_temperature_final_2019  <- communes_dates_2019_2022_temperature_final_2019  %>%
  dummy_cols(select_columns = "temperature_bin")


communes_dates_2019_2022_temperature_final_2019 <- communes_dates_2019_2022_temperature_final_2019 %>%
  arrange(COM, date)

# Ajouter une colonne pour la nouvelle variable
communes_dates_2019_2022_temperature_final_2019 <- communes_dates_2019_2022_temperature_final_2019 %>%
  mutate(same_value = ifelse(COM == lag(COM) & temperature_bin == lag(temperature_bin), 1, 0))

communes_dates_2019_2022_temperature_final_2019$same_value[is.na(communes_dates_2019_2022_temperature_final_2019$same_value)]<-0
#la première row est NA car pas de row avant

communes_dates_2019_2022_temperature_final_2019$same_value <- ifelse(communes_dates_2019_2022_temperature_final_2019$temperature_bin != ">30", 0, communes_dates_2019_2022_temperature_final_2019$same_value)



communes_dates_2019_2022_temperature_final_2019$mois<-substring(communes_dates_2019_2022_temperature_final_2019$date,6,7)

communes_dates_2019_2022_temperature_final_2019<-communes_dates_2019_2022_temperature_final_2019[,-c("date","value1","temperature_bin")]

communes_dates_2019_2022_temperature_final_2019<-aggregate(.~COM+mois,communes_dates_2019_2022_temperature_final_2019,sum)



RP_2019_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final_2")

RP_2019_age_sexe_final_2<-RP_2019_age_sexe_final_2[,c(2:15)]

names(RP_2019_age_sexe_final_2)[names(RP_2019_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2019_2022_temperature_final_2019<-left_join(communes_dates_2019_2022_temperature_final_2019,RP_2019_age_sexe_final_2)

#tests<-filter(communes_dates_2019_2022_temperature_final_2019, COM=="01001")
#tests<-filter(communes_dates_2019_2022_temperature_final_2019, is.na(value_estimated_sum_homme))
#table(tests$COM) 250 communes NA la plupart tres petite population

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019, !is.na(value_estimated_sum_homme) )

RP_2019_CSP_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2019_CSP_final_2")

RP_2019_CSP_final_2<-RP_2019_CSP_final_2[,c(2:12)]
names(RP_2019_CSP_final_2)[names(RP_2019_CSP_final_2)=="COM_AP"]<-"COM"
names(RP_2019_CSP_final_2)[names(RP_2019_CSP_final_2)=="value_estimated_population"]<-"population_actif_25_54"

communes_dates_2019_2022_temperature_final_2019<-left_join(communes_dates_2019_2022_temperature_final_2019,RP_2019_CSP_final_2)


communes_dates_2019_2022_temperature_final_2019$part_agriculteur<-communes_dates_2019_2022_temperature_final_2019$value_estimated_agriculteur/communes_dates_2019_2022_temperature_final_2019$population_actif_25_54

communes_dates_2019_2022_temperature_final_2019$part_artisan_commercant_chef_entreprise<-communes_dates_2019_2022_temperature_final_2019$value_estimated_artisan_commercant_chef_entreprise/communes_dates_2019_2022_temperature_final_2019$population_actif_25_54

communes_dates_2019_2022_temperature_final_2019$part_cadre<-communes_dates_2019_2022_temperature_final_2019$value_estimated_cadre/communes_dates_2019_2022_temperature_final_2019$population_actif_25_54

communes_dates_2019_2022_temperature_final_2019$part_profession_intermediaire<-communes_dates_2019_2022_temperature_final_2019$value_estimated_profession_intermediaire/communes_dates_2019_2022_temperature_final_2019$population_actif_25_54

communes_dates_2019_2022_temperature_final_2019$part_employe<-communes_dates_2019_2022_temperature_final_2019$value_estimated_employe/communes_dates_2019_2022_temperature_final_2019$population_actif_25_54

communes_dates_2019_2022_temperature_final_2019$part_ouvrier<-communes_dates_2019_2022_temperature_final_2019$value_estimated_ouvrier/communes_dates_2019_2022_temperature_final_2019$population_actif_25_54

communes_dates_2019_2022_temperature_final_2019$part_chomage<-communes_dates_2019_2022_temperature_final_2019$value_estimated_au_chomage/communes_dates_2019_2022_temperature_final_2019$population_actif_25_54






communes_dates_2019_2022_temperature_final_2019$taux_mortalite_homme<-communes_dates_2019_2022_temperature_final_2019$Homme/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_homme

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_femme<-communes_dates_2019_2022_temperature_final_2019$Femme/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_femme


communes_dates_2019_2022_temperature_final_2019$taux_mortalite_0_9<-communes_dates_2019_2022_temperature_final_2019$`0-9`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_0_9_h_f

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_10_19<-communes_dates_2019_2022_temperature_final_2019$`10-19`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_10_19_h_f



communes_dates_2019_2022_temperature_final_2019$taux_mortalite_20_39<-communes_dates_2019_2022_temperature_final_2019$`20-39`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_20_39_h_f




communes_dates_2019_2022_temperature_final_2019$taux_mortalite_40_59<-communes_dates_2019_2022_temperature_final_2019$`40-59`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_40_59_h_f





communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_64<-communes_dates_2019_2022_temperature_final_2019$`60-64`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_60_64_h_f



communes_dates_2019_2022_temperature_final_2019$taux_mortalite_65_69<-communes_dates_2019_2022_temperature_final_2019$`65-69`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_65_69_h_f


communes_dates_2019_2022_temperature_final_2019$taux_mortalite_70_74<-communes_dates_2019_2022_temperature_final_2019$`70-74`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_70_74_h_f


communes_dates_2019_2022_temperature_final_2019$taux_mortalite_75_79<-communes_dates_2019_2022_temperature_final_2019$`75-79`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_75_79_h_f


communes_dates_2019_2022_temperature_final_2019$taux_mortalite_80_plus<-communes_dates_2019_2022_temperature_final_2019$`80+`/communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_80_plus_h_f


#communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_70<-(communes_dates_2019_2022_temperature_final_2019$`60-64`+communes_dates_2019_2022_temperature_final_2019$`65-69`)/(communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_60_64_h_f+communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_65_69_h_f)



communes_dates_2019_2022_temperature_final_2019$mort_total<-communes_dates_2019_2022_temperature_final_2019$Femme+communes_dates_2019_2022_temperature_final_2019$Homme


communes_dates_2019_2022_temperature_final_2019$taux_mortalite_total<-communes_dates_2019_2022_temperature_final_2019$mort_total/communes_dates_2019_2022_temperature_final_2019$value_estimated_population






communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_74<-(communes_dates_2019_2022_temperature_final_2019$`60-64`+communes_dates_2019_2022_temperature_final_2019$`65-69`+communes_dates_2019_2022_temperature_final_2019$`70-74`)/(communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_60_64_h_f+communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_65_69_h_f+communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_70_74_h_f)


communes_dates_2019_2022_temperature_final_2019$taux_mortalite_75_plus<-(communes_dates_2019_2022_temperature_final_2019$`75-79`+communes_dates_2019_2022_temperature_final_2019$`80+`)/(communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_75_79_h_f+communes_dates_2019_2022_temperature_final_2019$value_estimated_sum_80_plus_h_f)








#on enleve les communes avec des population de 0
communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019, communes_dates_2019_2022_temperature_final_2019$value_estimated_population>0)
communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019, communes_dates_2019_2022_temperature_final_2019$population_actif_25_54>0)




communes_dates_2019_2022_temperature_final_2019<- communes_dates_2019_2022_temperature_final_2019[ , !names(communes_dates_2019_2022_temperature_final_2019) %in% c("Femme","Homme","0-9","10-19","20-39" , "40-59" ,"60-64" ,"65-69","70-74" ,"75-79","80+","value_estimated_sum_homme","value_estimated_sum_femme","value_estimated_sum_0_9_h_f","value_estimated_sum_10_19_h_f","value_estimated_sum_20_39_h_f","value_estimated_sum_40_59_h_f", "value_estimated_sum_60_64_h_f","value_estimated_sum_65_69_h_f","value_estimated_sum_70_74_h_f","value_estimated_sum_75_79_h_f","value_estimated_sum_80_plus_h_f",
                                                                                                                                                                    "value_estimated_agriculteur","value_estimated_artisan_commercant_chef_entreprise", "value_estimated_cadre","value_estimated_profession_intermediaire","value_estimated_employe", "value_estimated_ouvrier","value_estimated_en_emploi", "value_estimated_au_chomage","mort_total")]



#
#
#
#

#

#



communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_femme))


communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(part_agriculteur))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(part_artisan_commercant_chef_entreprise))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(part_cadre))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(part_profession_intermediaire))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(part_employe))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(part_ouvrier))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(part_chomage))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_homme))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_0_9))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_10_19))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_20_39))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_40_59))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_60_64))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_65_69))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_70_74))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_75_79))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_80_plus))

communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_total))




communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_60_74))
communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  !is.infinite(taux_mortalite_75_plus))








#761 valeurs inf

#

#communes_dates_2019_2022_temperature_final_2019<-communes_dates_2019_2022_temperature_final_2019[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_10_19 != 1.4, ]

#




#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  part_agriculteur<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_10_19<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  part_artisan_commercant_chef_entreprise<=1)
#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  part_cadre<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  part_profession_intermediaire<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  part_employe<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  part_ouvrier<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  part_chomage<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_homme<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_femme<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_0_9<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_20_39<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_40_59<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_60_64<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_65_69<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_70_74<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_75_79<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_80_plus<=1)

#communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019,  taux_mortalite_total<=1)




communes_dates_2019_2022_temperature_final_2019$taux_mortalite_homme[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_homme>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_femme[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_femme>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_0_9[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_0_9>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_10_19[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_10_19>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_20_39[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_20_39>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_40_59[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_40_59>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_64[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_64>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_65_69[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_65_69>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_70_74[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_70_74>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_75_79[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_75_79>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_80_plus[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_80_plus>1]<-NA   

communes_dates_2019_2022_temperature_final_2019$taux_mortalite_total[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_total>1]<-NA   




communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_74[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_74>1]<-NA   
communes_dates_2019_2022_temperature_final_2019$taux_mortalite_75_plus[communes_dates_2019_2022_temperature_final_2019$taux_mortalite_75_plus>1]<-NA   




summary(communes_dates_2019_2022_temperature_final_2019$part_agriculteur)

summary(communes_dates_2019_2022_temperature_final_2019$part_artisan_commercant_chef_entreprise)

summary(communes_dates_2019_2022_temperature_final_2019$part_cadre)

summary(communes_dates_2019_2022_temperature_final_2019$part_profession_intermediaire)

summary(communes_dates_2019_2022_temperature_final_2019$part_employe)

summary(communes_dates_2019_2022_temperature_final_2019$part_ouvrier)

summary(communes_dates_2019_2022_temperature_final_2019$part_chomage)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_homme)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_femme)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_0_9)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_10_19)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_20_39)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_40_59)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_60_64)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_65_69)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_70_74)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_75_79)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_80_plus)

summary(communes_dates_2019_2022_temperature_final_2019$taux_mortalite_total)






fwrite(communes_dates_2019_2022_temperature_final_2019,"/données communes années/données mortalité temperature final mois new/communes_dates_2019_temperature_deces_mois.csv")
















































#We then combine all the years 


##############




library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)



communes_dates_1980_2022_temperature_final_1980_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1980_temperature_deces_mois.csv")

communes_dates_1981_2022_temperature_final_1981_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1981_temperature_deces_mois.csv")

communes_dates_1982_2022_temperature_final_1982_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1982_temperature_deces_mois.csv")

communes_dates_1983_2022_temperature_final_1983_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1983_temperature_deces_mois.csv")

communes_dates_1984_2022_temperature_final_1984_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1984_temperature_deces_mois.csv")

communes_dates_1985_2022_temperature_final_1985_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1985_temperature_deces_mois.csv")

communes_dates_1986_2022_temperature_final_1986_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1986_temperature_deces_mois.csv")

communes_dates_1987_2022_temperature_final_1987_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1987_temperature_deces_mois.csv")

communes_dates_1988_2022_temperature_final_1988_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1988_temperature_deces_mois.csv")

communes_dates_1989_2022_temperature_final_1989_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1989_temperature_deces_mois.csv")

communes_dates_1990_2022_temperature_final_1990_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1990_temperature_deces_mois.csv")

communes_dates_1991_2022_temperature_final_1991_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1991_temperature_deces_mois.csv")

communes_dates_1992_2022_temperature_final_1992_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1992_temperature_deces_mois.csv")

communes_dates_1993_2022_temperature_final_1993_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1993_temperature_deces_mois.csv")

communes_dates_1994_2022_temperature_final_1994_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1994_temperature_deces_mois.csv")

communes_dates_1995_2022_temperature_final_1995_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1995_temperature_deces_mois.csv")

communes_dates_1996_2022_temperature_final_1996_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1996_temperature_deces_mois.csv")

communes_dates_1997_2022_temperature_final_1997_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1997_temperature_deces_mois.csv")

communes_dates_1998_2022_temperature_final_1998_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1998_temperature_deces_mois.csv")

communes_dates_1999_2022_temperature_final_1999_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1999_temperature_deces_mois.csv")

communes_dates_2000_2022_temperature_final_2000_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2000_temperature_deces_mois.csv")

communes_dates_2001_2022_temperature_final_2001_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2001_temperature_deces_mois.csv")

communes_dates_2002_2022_temperature_final_2002_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2002_temperature_deces_mois.csv")

communes_dates_2003_2022_temperature_final_2003_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2003_temperature_deces_mois.csv")

communes_dates_2004_2022_temperature_final_2004_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2004_temperature_deces_mois.csv")

communes_dates_2005_2022_temperature_final_2005_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2005_temperature_deces_mois.csv")

communes_dates_2006_2022_temperature_final_2006_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2006_temperature_deces_mois.csv")

communes_dates_2007_2022_temperature_final_2007_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2007_temperature_deces_mois.csv")

communes_dates_2008_2022_temperature_final_2008_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2008_temperature_deces_mois.csv")

communes_dates_2009_2022_temperature_final_2009_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2009_temperature_deces_mois.csv")

communes_dates_2010_2022_temperature_final_2010_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2010_temperature_deces_mois.csv")

communes_dates_2011_2022_temperature_final_2011_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2011_temperature_deces_mois.csv")

communes_dates_2012_2022_temperature_final_2012_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2012_temperature_deces_mois.csv")

communes_dates_2013_2022_temperature_final_2013_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2013_temperature_deces_mois.csv")

communes_dates_2014_2022_temperature_final_2014_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2014_temperature_deces_mois.csv")

communes_dates_2015_2022_temperature_final_2015_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2015_temperature_deces_mois.csv")

communes_dates_2016_2022_temperature_final_2016_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2016_temperature_deces_mois.csv")

communes_dates_2017_2022_temperature_final_2017_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2017_temperature_deces_mois.csv")

communes_dates_2018_2022_temperature_final_2018_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2018_temperature_deces_mois.csv")

communes_dates_2019_2022_temperature_final_2019_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_2019_temperature_deces_mois.csv")






#creation of a variable when there is none in the year in question because there are no such temperatures in those years :



communes_dates_1980_2022_temperature_final_1980_mois$'temperature_bin_>30'<-0
communes_dates_1980_2022_temperature_final_1980_mois$'temperature_bin_<-20'<-0

communes_dates_1981_2022_temperature_final_1981_mois$'temperature_bin_<-20'<-0
communes_dates_1982_2022_temperature_final_1982_mois$'temperature_bin_<-20'<-0
communes_dates_1983_2022_temperature_final_1983_mois$'temperature_bin_<-20'<-0

communes_dates_1984_2022_temperature_final_1984_mois$'temperature_bin_>30'<-0
communes_dates_1984_2022_temperature_final_1984_mois$'temperature_bin_<-20'<-0

communes_dates_1985_2022_temperature_final_1985_mois$'temperature_bin_>30'<-0

communes_dates_1986_2022_temperature_final_1986_mois$'temperature_bin_>30'<-0

communes_dates_1988_2022_temperature_final_1988_mois$'temperature_bin_>30'<-0
communes_dates_1988_2022_temperature_final_1988_mois$'temperature_bin_<-20'<-0

communes_dates_1989_2022_temperature_final_1989_mois$'temperature_bin_<-20'<-0
communes_dates_1989_2022_temperature_final_1989_mois$'temperature_bin_-20_-15'<-0


communes_dates_1992_2022_temperature_final_1992_mois$'temperature_bin_<-20'<-0

communes_dates_1994_2022_temperature_final_1994_mois$'temperature_bin_<-20'<-0

communes_dates_1996_2022_temperature_final_1996_mois$'temperature_bin_<-20'<-0

communes_dates_1997_2022_temperature_final_1997_mois$'temperature_bin_>30'<-0
communes_dates_1997_2022_temperature_final_1997_mois$'temperature_bin_<-20'<-0
communes_dates_1997_2022_temperature_final_1997_mois$'temperature_bin_-20_-15'<-0


communes_dates_1998_2022_temperature_final_1998_mois$'temperature_bin_<-20'<-0

communes_dates_2000_2022_temperature_final_2000_mois$'temperature_bin_<-20'<-0

communes_dates_2001_2022_temperature_final_2001_mois$'temperature_bin_<-20'<-0


communes_dates_2002_2022_temperature_final_2002_mois$'temperature_bin_>30'<-0
communes_dates_2002_2022_temperature_final_2002_mois$'temperature_bin_<-20'<-0

communes_dates_2004_2022_temperature_final_2004_mois$'temperature_bin_<-20'<-0


communes_dates_2006_2022_temperature_final_2006_mois$'temperature_bin_<-20'<-0

communes_dates_2007_2022_temperature_final_2007_mois$'temperature_bin_<-20'<-0

communes_dates_2008_2022_temperature_final_2008_mois$'temperature_bin_<-20'<-0
communes_dates_2009_2022_temperature_final_2009_mois$'temperature_bin_<-20'<-0

communes_dates_2011_2022_temperature_final_2011_mois$'temperature_bin_<-20'<-0


communes_dates_2014_2022_temperature_final_2014_mois$'temperature_bin_>30'<-0
communes_dates_2014_2022_temperature_final_2014_mois$'temperature_bin_<-20'<-0


communes_dates_2015_2022_temperature_final_2015_mois$'temperature_bin_<-20'<-0

communes_dates_2016_2022_temperature_final_2016_mois$'temperature_bin_<-20'<-0
communes_dates_2019_2022_temperature_final_2019_mois$'temperature_bin_<-20'<-0



communes_dates_1990_2022_temperature_final_1990_mois$'temperature_bin_<-20'<-0






communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_1980_mois,communes_dates_1981_2022_temperature_final_1981_mois)

communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1982_2022_temperature_final_1982_mois)

communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1983_2022_temperature_final_1983_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1984_2022_temperature_final_1984_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1985_2022_temperature_final_1985_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1986_2022_temperature_final_1986_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1987_2022_temperature_final_1987_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1988_2022_temperature_final_1988_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1989_2022_temperature_final_1989_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1990_2022_temperature_final_1990_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1991_2022_temperature_final_1991_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1992_2022_temperature_final_1992_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1993_2022_temperature_final_1993_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1994_2022_temperature_final_1994_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1995_2022_temperature_final_1995_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1996_2022_temperature_final_1996_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1997_2022_temperature_final_1997_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1998_2022_temperature_final_1998_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_1999_2022_temperature_final_1999_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2000_2022_temperature_final_2000_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2001_2022_temperature_final_2001_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2002_2022_temperature_final_2002_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2003_2022_temperature_final_2003_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2004_2022_temperature_final_2004_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2005_2022_temperature_final_2005_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2006_2022_temperature_final_2006_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2007_2022_temperature_final_2007_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2008_2022_temperature_final_2008_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2009_2022_temperature_final_2009_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2010_2022_temperature_final_2010_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2011_2022_temperature_final_2011_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2012_2022_temperature_final_2012_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2013_2022_temperature_final_2013_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2014_2022_temperature_final_2014_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2015_2022_temperature_final_2015_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2016_2022_temperature_final_2016_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2017_2022_temperature_final_2017_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2018_2022_temperature_final_2018_mois)
communes_dates_1980_2022_temperature_final_mois<-rbind(communes_dates_1980_2022_temperature_final_mois,communes_dates_2019_2022_temperature_final_2019_mois)



summary(communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-10_-5`)

summary(communes_dates_1980_2022_temperature_final_mois$`temperature_bin_>30`)

summary(communes_dates_1980_2022_temperature_final_mois$`temperature_bin_<-20`)

summary(communes_dates_1980_2022_temperature_final_mois$same_value)

summary(communes_dates_1980_2022_temperature_final_mois$value_estimated_population)

summary(communes_dates_1980_2022_temperature_final_mois$population_actif_25_54)

summary(communes_dates_1980_2022_temperature_final_mois$part_ouvrier)


summary(communes_dates_1980_2022_temperature_final_mois$taux_mortalite_homme)

summary(communes_dates_1980_2022_temperature_final_mois$taux_mortalite_40_59)

summary(communes_dates_1980_2022_temperature_final_mois$taux_mortalite_total)

table(communes_dates_1980_2022_temperature_final_mois$year)



communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois<-communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-5_0`+communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-10_-5`+communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-15_-10`+communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-20_-15`+communes_dates_1980_2022_temperature_final_mois$temperature_bin_0_5+communes_dates_1980_2022_temperature_final_mois$temperature_bin_5_10+communes_dates_1980_2022_temperature_final_mois$temperature_bin_10_15+communes_dates_1980_2022_temperature_final_mois$temperature_bin_15_20+communes_dates_1980_2022_temperature_final_mois$temperature_bin_20_25+communes_dates_1980_2022_temperature_final_mois$temperature_bin_25_28+communes_dates_1980_2022_temperature_final_mois$temperature_bin_28_30+communes_dates_1980_2022_temperature_final_mois$`temperature_bin_>30`+communes_dates_1980_2022_temperature_final_mois$`temperature_bin_<-20`


table(communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois)


communes_dates_1980_2022_temperature_final_mois$temperature_bin_moins_5_0_part<-communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-5_0`/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois


communes_dates_1980_2022_temperature_final_mois$temperature_bin_moins_10_moins_5_part<-communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-10_-5`/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois


communes_dates_1980_2022_temperature_final_mois$temperature_bin_moins_15_moins_10_part<-communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-15_-10`/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois



communes_dates_1980_2022_temperature_final_mois$temperature_bin_moins_20_moins_15_part<-communes_dates_1980_2022_temperature_final_mois$`temperature_bin_-20_-15`/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois


communes_dates_1980_2022_temperature_final_mois$temperature_bin_0_5_part<-communes_dates_1980_2022_temperature_final_mois$temperature_bin_0_5/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois



communes_dates_1980_2022_temperature_final_mois$temperature_bin_5_10_part<-communes_dates_1980_2022_temperature_final_mois$temperature_bin_5_10/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois



communes_dates_1980_2022_temperature_final_mois$temperature_bin_10_15_part<-communes_dates_1980_2022_temperature_final_mois$temperature_bin_10_15/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois



communes_dates_1980_2022_temperature_final_mois$temperature_bin_15_20_part<-communes_dates_1980_2022_temperature_final_mois$temperature_bin_15_20/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois



communes_dates_1980_2022_temperature_final_mois$temperature_bin_20_25_part<-communes_dates_1980_2022_temperature_final_mois$temperature_bin_20_25/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois



communes_dates_1980_2022_temperature_final_mois$temperature_bin_25_28_part<-communes_dates_1980_2022_temperature_final_mois$temperature_bin_25_28/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois


communes_dates_1980_2022_temperature_final_mois$temperature_bin_28_30_part<-communes_dates_1980_2022_temperature_final_mois$temperature_bin_28_30/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois



communes_dates_1980_2022_temperature_final_mois$temperature_bin_superieur_30_part<-communes_dates_1980_2022_temperature_final_mois$`temperature_bin_>30`/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois


communes_dates_1980_2022_temperature_final_mois$temperature_bin_inferieu_moins_20_part<-communes_dates_1980_2022_temperature_final_mois$`temperature_bin_<-20`/communes_dates_1980_2022_temperature_final_mois$nbr_jours_mois


















communes_dates_1980_2022_temperature_final_mois<-communes_dates_1980_2022_temperature_final_mois[,-c(3:13,37,38)]










fwrite(communes_dates_1980_2022_temperature_final_mois,"/données communes années/données mortalité temperature final mois/communes_dates_1980_2022_temperature_final_mois.csv")









########################



#gathering meteorological data


library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)


library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)





humidity<-fread("/données temperature/hu")
shape<-fread("/shape_nom_communes_hu")

temp<-names(humidity)
temp<-as.data.frame(temp)

humidity<-humidity[,c(1,10959:26480)]

humidity<-left_join(humidity,shape)
#humidity<-filter(humidity, !(is.na(humidity$X2022.01.01)))

#humidity2<-filter(humidity, (is.na(humidity$X2022.01.01)))

#humidity3<-filter(humidity, (is.na(humidity$X2000.01.01)))


temp<-names(humidity)
temp<-as.data.frame(temp)

humidity<-humidity[,c(2:15524)]

humidity <- humidity %>% gather(variable, value, -insee)
humidity$variable<-substring(humidity$variable,2,11)

humidity$variable <- gsub("\\.", "-", humidity$variable)


com_temp<-table(humidity$insee)
com_temp<-as.data.frame(com_temp)

names(com_temp)[names(com_temp)=="Var1"]<-"COM"

humidity$value <- round(humidity$value, 1)






names(humidity)[names(humidity)=="insee"]<-"COM"
names(humidity)[names(humidity)=="variable"]<-"date"



summary(humidity$value)

#humidity$date <- as.Date(humidity$date)


#start_date <- as.Date("1980-01-01")
#end_date <- as.Date("2022-12-31")
#humidity <- humidity %>% filter(date >= start_date & date <= end_date)




humidity<-filter(humidity, !is.na(humidity$value))



humidity$humidity_bin[humidity$value < 20]<-"moins_20"

humidity$humidity_bin[humidity$value >= 20 & humidity$value < 40]<-"20_40"

humidity$humidity_bin[humidity$value >= 40 & humidity$value < 60]<-"40_60"

humidity$humidity_bin[humidity$value >= 60 & humidity$value < 80]<-"60_80"

humidity$humidity_bin[humidity$value >= 80]<-"plus_80"


humidity$year<-substring(humidity$date,1,4)

humidity1<-filter(humidity, humidity$year<1990)
humidity2<-filter(humidity, humidity$year<2000 & humidity$year>= 1990)
humidity3<-filter(humidity, humidity$year>=2000 & humidity$year< 2010)
humidity4<-filter(humidity, humidity$year>= 2010)

rm(humidity)
gc()

library(fastDummies)
humidity1  <- humidity1  %>%
  dummy_cols(select_columns = "humidity_bin")

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)
#pas besoin 


#humidity1 <- humidity1 %>%
#  arrange(COM, date)



humidity1$mois<-substring(humidity1$date,6,7)

humidity1<-humidity1[,-c(2:4)]

humidity1<-aggregate(.~COM+year+mois,humidity1,sum)


humidity1$nbr_jours_humidity<-humidity1$humidity_bin_moins_20+humidity1$humidity_bin_plus_80+humidity1$humidity_bin_20_40+humidity1$humidity_bin_40_60+humidity1$humidity_bin_60_80
#la plupart des données manquantes c'est année 80

humidity1<-filter(humidity1, humidity1$nbr_jours_humidity>= 26)
#from 4 063 680 obs to 3 998 388, minus 65 000 obs


library(fastDummies)
humidity2  <- humidity2  %>%
  dummy_cols(select_columns = "humidity_bin")




humidity2$mois<-substring(humidity2$date,6,7)

humidity2<-humidity2[,-c(2:4)]

humidity2<-aggregate(.~COM+year+mois,humidity2,sum)


humidity2$nbr_jours_humidity<-humidity2$humidity_bin_moins_20+humidity2$humidity_bin_plus_80+humidity2$humidity_bin_20_40+humidity2$humidity_bin_40_60+humidity2$humidity_bin_60_80


humidity2<-filter(humidity2, humidity2$nbr_jours_humidity>= 26)



library(fastDummies)
humidity3  <- humidity3  %>%
  dummy_cols(select_columns = "humidity_bin")




humidity3$mois<-substring(humidity3$date,6,7)

humidity3<-humidity3[,-c(2:4)]

humidity3<-aggregate(.~COM+year+mois,humidity3,sum)


humidity3$nbr_jours_humidity<-humidity3$humidity_bin_moins_20+humidity3$humidity_bin_plus_80+humidity3$humidity_bin_20_40+humidity3$humidity_bin_40_60+humidity3$humidity_bin_60_80


humidity3<-filter(humidity3, humidity3$nbr_jours_humidity>= 26)





library(fastDummies)
humidity4  <- humidity4  %>%
  dummy_cols(select_columns = "humidity_bin")




humidity4$mois<-substring(humidity4$date,6,7)

humidity4<-humidity4[,-c(2:4)]

humidity4<-aggregate(.~COM+year+mois,humidity4,sum)


humidity4$nbr_jours_humidity<-humidity4$humidity_bin_moins_20+humidity4$humidity_bin_plus_80+humidity4$humidity_bin_20_40+humidity4$humidity_bin_40_60+humidity4$humidity_bin_60_80


humidity4<-filter(humidity4, humidity4$nbr_jours_humidity>= 26)





humidity<-rbind(humidity1,humidity2)
humidity<-rbind(humidity,humidity3)
humidity<-rbind(humidity,humidity4)



table(humidity$nbr_jours_humidity)
summary(humidity$humidity_bin_moins_20)



humidity$humidity_bin_moins_20_part<-humidity$humidity_bin_moins_20/humidity$nbr_jours_humidity

humidity$humidity_bin_20_40_part<-humidity$humidity_bin_20_40/humidity$nbr_jours_humidity

humidity$humidity_bin_40_60_part<-humidity$humidity_bin_40_60/humidity$nbr_jours_humidity

humidity$humidity_bin_60_80_part<-humidity$humidity_bin_60_80/humidity$nbr_jours_humidity

humidity$humidity_bin_plus_80_part<-humidity$humidity_bin_plus_80/humidity$nbr_jours_humidity


humidity$total_part<-humidity$humidity_bin_moins_20_part+humidity$humidity_bin_20_40_part+humidity$humidity_bin_40_60_part+humidity$humidity_bin_60_80_part+humidity$humidity_bin_plus_80_part

table(humidity$total_part)
#1 OK





humidity<-humidity[,-c(4:9,15)]


humidity$mois<-as.numeric(humidity$mois)
humidity$year<-as.numeric(humidity$year)



communes_dates_1980_2022_temperature_final_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1980_2022_temperature_final_mois.csv")

communes_dates_1980_2022_temperature_final_mois<-left_join(communes_dates_1980_2022_temperature_final_mois,humidity)

test_na<-filter(communes_dates_1980_2022_temperature_final_mois, is.na(communes_dates_1980_2022_temperature_final_mois$humidity_bin_60_80_part))
#429 000 missing


#fwrite(communes_dates_1980_2022_temperature_final_mois,"/données communes années/données mortalité temperature final mois/communes_dates_1980_2022_temperature_final_mois_humidity.csv")




######### rain #######









library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)


library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)





rain<-fread("/données temperature/rr")
shape<-fread("/shape_nom_communes_rr")

temp<-names(rain)
temp<-as.data.frame(temp)

rain<-rain[,c(1,10959:26480)]

rain<-left_join(rain,shape)
#rain<-filter(rain, !(is.na(rain$X2022.01.01)))

#rain2<-filter(rain, (is.na(rain$X2022.01.01)))

#rain3<-filter(rain, (is.na(rain$X2000.01.01)))


temp<-names(rain)
temp<-as.data.frame(temp)

rain<-rain[,c(2:15524)]

rain <- rain %>% gather(variable, value, -insee)
rain$variable<-substring(rain$variable,2,11)

rain$variable <- gsub("\\.", "-", rain$variable)


com_temp<-table(rain$insee)
com_temp<-as.data.frame(com_temp)

names(com_temp)[names(com_temp)=="Var1"]<-"COM"

rain$value <- round(rain$value, 1)






names(rain)[names(rain)=="insee"]<-"COM"
names(rain)[names(rain)=="variable"]<-"date"



summary(rain$value)

#rain$date <- as.Date(rain$date)


#start_date <- as.Date("1980-01-01")
#end_date <- as.Date("2022-12-31")
#rain <- rain %>% filter(date >= start_date & date <= end_date)




rain<-filter(rain, !is.na(rain$value))


rain$rain_bin[rain$value == 0]<-"zero"


rain$rain_bin[rain$value >0 & rain$value < 3]<-"0_3"

rain$rain_bin[rain$value >= 3 & rain$value < 10]<-"3_10"

rain$rain_bin[rain$value >= 10 & rain$value < 100]<-"10_100"

rain$rain_bin[rain$value >= 100]<-"plus_100"


rain$year<-substring(rain$date,1,4)

rain1<-filter(rain, rain$year<1990)
rain2<-filter(rain, rain$year<2000 & rain$year>= 1990)
rain3<-filter(rain, rain$year>=2000 & rain$year< 2010)
rain4<-filter(rain, rain$year>= 2010)

rm(rain)
gc()

library(fastDummies)
rain1  <- rain1  %>%
  dummy_cols(select_columns = "rain_bin")

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)
#pas besoin 


#rain1 <- rain1 %>%
#  arrange(COM, date)



rain1$mois<-substring(rain1$date,6,7)

rain1<-rain1[,-c(2:4)]

rain1<-aggregate(.~COM+year+mois,rain1,sum)


rain1$nbr_jours_rain<-rain1$rain_bin_0_3+rain1$rain_bin_3_10+rain1$rain_bin_10_100+rain1$rain_bin_plus_100+rain1$rain_bin_zero

rain1<-filter(rain1, rain1$nbr_jours_rain>= 26)


library(fastDummies)
rain2  <- rain2  %>%
  dummy_cols(select_columns = "rain_bin")




rain2$mois<-substring(rain2$date,6,7)

rain2<-rain2[,-c(2:4)]

rain2<-aggregate(.~COM+year+mois,rain2,sum)


rain2$nbr_jours_rain<-rain2$rain_bin_0_3+rain2$rain_bin_3_10+rain2$rain_bin_10_100+rain2$rain_bin_plus_100+rain2$rain_bin_zero


rain2<-filter(rain2, rain2$nbr_jours_rain>= 26)



library(fastDummies)
rain3  <- rain3  %>%
  dummy_cols(select_columns = "rain_bin")




rain3$mois<-substring(rain3$date,6,7)

rain3<-rain3[,-c(2:4)]

rain3<-aggregate(.~COM+year+mois,rain3,sum)


rain3$nbr_jours_rain<-rain3$rain_bin_0_3+rain3$rain_bin_3_10+rain3$rain_bin_10_100+rain3$rain_bin_plus_100+rain3$rain_bin_zero


rain3<-filter(rain3, rain3$nbr_jours_rain>= 26)





library(fastDummies)
rain4  <- rain4  %>%
  dummy_cols(select_columns = "rain_bin")




rain4$mois<-substring(rain4$date,6,7)

rain4<-rain4[,-c(2:4)]

rain4<-aggregate(.~COM+year+mois,rain4,sum)


rain4$nbr_jours_rain<-rain4$rain_bin_0_3+rain4$rain_bin_3_10+rain4$rain_bin_10_100+rain4$rain_bin_plus_100+rain4$rain_bin_zero



rain4<-filter(rain4, rain4$nbr_jours_rain>= 26)





rain<-rbind(rain1,rain2)
rain<-rbind(rain,rain3)
rain<-rbind(rain,rain4)



table(rain$nbr_jours_rain)
summary(rain$rain_bin_zero)



rain$rain_bin_0_3_part<-rain$rain_bin_0_3/rain$nbr_jours_rain

rain$rain_bin_3_10_part<-rain$rain_bin_3_10/rain$nbr_jours_rain

rain$rain_bin_10_100_part<-rain$rain_bin_10_100/rain$nbr_jours_rain

rain$rain_bin_plus_100_part<-rain$rain_bin_plus_100/rain$nbr_jours_rain

rain$rain_bin_zero_part<-rain$rain_bin_zero/rain$nbr_jours_rain


rain$total_part<-rain$rain_bin_0_3_part+rain$rain_bin_3_10_part+rain$rain_bin_10_100_part+rain$rain_bin_plus_100_part+rain$rain_bin_zero_part

table(rain$total_part)
#1 OK





rain<-rain[,-c(4:9,15)]


rain$mois<-as.numeric(rain$mois)
rain$year<-as.numeric(rain$year)



communes_dates_1980_2022_temperature_final_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1980_2022_temperature_final_mois_humidity.csv")

communes_dates_1980_2022_temperature_final_mois<-left_join(communes_dates_1980_2022_temperature_final_mois,rain)

test_na<-filter(communes_dates_1980_2022_temperature_final_mois, is.na(communes_dates_1980_2022_temperature_final_mois$rain_bin_plus_100_part))
#0 missing


#fwrite(communes_dates_1980_2022_temperature_final_mois,"/données communes années/données mortalité temperature final mois/communes_dates_1980_2022_temperature_final_mois_humidity_rain.csv")







######### wind #######









library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)


library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)
library(data.table)
library(dplyr)
library(readr)
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)





wind<-fread("/données temperature/fg")
shape<-fread("/shape_nom_communes_fg")

temp<-names(wind)
temp<-as.data.frame(temp)

#les données wind commencent pile en 1980 on a de la chance

#wind<-wind[,c(1,10959:26480)]

wind<-left_join(wind,shape)
#wind<-filter(wind, !(is.na(wind$X2022.01.01)))

#wind2<-filter(wind, (is.na(wind$X2022.01.01)))

#wind3<-filter(wind, (is.na(wind$X2000.01.01)))


temp<-names(wind)
temp<-as.data.frame(temp)

wind<-wind[,c(2:15524)]

wind <- wind %>% gather(variable, value, -insee)
wind$variable<-substring(wind$variable,2,11)

wind$variable <- gsub("\\.", "-", wind$variable)


com_temp<-table(wind$insee)
com_temp<-as.data.frame(com_temp)

names(com_temp)[names(com_temp)=="Var1"]<-"COM"


#summary(wind$value)
#wind$value[wind$value<0]<-NA
#summary(wind$value)


wind$value <- round(wind$value, 1)


#test_na<-filter(wind, wind$value<0)
#1 186 245


names(wind)[names(wind)=="insee"]<-"COM"
names(wind)[names(wind)=="variable"]<-"date"



summary(wind$value)
#na : 13 395 486 

#wind$date <- as.Date(wind$date)


#start_date <- as.Date("1980-01-01")
#end_date <- as.Date("2022-12-31")
#wind <- wind %>% filter(date >= start_date & date <= end_date)




wind<-filter(wind, !is.na(wind$value))
#526 832 202
wind<-filter(wind, wind$value>= 0)
#525 645 957
summary(wind$value)




wind$wind_bin[wind$value >= 0 & wind$value < 3]<-"0_3"

wind$wind_bin[wind$value >= 3 & wind$value < 10]<-"3_10"

wind$wind_bin[wind$value >= 10 & wind$value < 20]<-"10_20"

wind$wind_bin[wind$value >= 20]<-"plus_20"


wind$year<-substring(wind$date,1,4)

wind1<-filter(wind, wind$year<1990)
wind2<-filter(wind, wind$year<2000 & wind$year>= 1990)
wind3<-filter(wind, wind$year>=2000 & wind$year< 2010)
wind4<-filter(wind, wind$year>= 2010)

rm(wind)
gc()

library(fastDummies)
wind1  <- wind1  %>%
  dummy_cols(select_columns = "wind_bin")

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)
#pas besoin 


#wind1 <- wind1 %>%
#  arrange(COM, date)



wind1$mois<-substring(wind1$date,6,7)

wind1<-wind1[,-c(2:4)]

wind1<-aggregate(.~COM+year+mois,wind1,sum)


wind1$nbr_jours_wind<-wind1$wind_bin_0_3+wind1$wind_bin_3_10+wind1$wind_bin_10_20+wind1$wind_bin_plus_20
#ici pas de valeurs manquantes apparement

wind1<-filter(wind1, wind1$nbr_jours_wind>= 26)
#


library(fastDummies)
wind2  <- wind2  %>%
  dummy_cols(select_columns = "wind_bin")




wind2$mois<-substring(wind2$date,6,7)

wind2<-wind2[,-c(2:4)]

wind2<-aggregate(.~COM+year+mois,wind2,sum)


wind2$nbr_jours_wind<-wind2$wind_bin_0_3+wind2$wind_bin_3_10+wind2$wind_bin_10_20+wind2$wind_bin_plus_20

wind2<-filter(wind2, wind2$nbr_jours_wind>= 26)



library(fastDummies)
wind3  <- wind3  %>%
  dummy_cols(select_columns = "wind_bin")




wind3$mois<-substring(wind3$date,6,7)

wind3<-wind3[,-c(2:4)]

wind3<-aggregate(.~COM+year+mois,wind3,sum)


wind3$nbr_jours_wind<-wind3$wind_bin_0_3+wind3$wind_bin_3_10+wind3$wind_bin_10_20+wind3$wind_bin_plus_20

wind3<-filter(wind3, wind3$nbr_jours_wind>= 26)





library(fastDummies)
wind4  <- wind4  %>%
  dummy_cols(select_columns = "wind_bin")




wind4$mois<-substring(wind4$date,6,7)

wind4<-wind4[,-c(2:4)]

wind4<-aggregate(.~COM+year+mois,wind4,sum)


wind4$nbr_jours_wind<-wind4$wind_bin_0_3+wind4$wind_bin_3_10+wind4$wind_bin_10_20+wind4$wind_bin_plus_20

wind4<-filter(wind4, wind4$nbr_jours_wind>= 26)





wind<-rbind(wind1,wind2)
wind<-rbind(wind,wind3)
wind<-rbind(wind,wind4)



table(wind$nbr_jours_wind)
summary(wind$)



wind$wind_bin_0_3_part<-wind$wind_bin_0_3/wind$nbr_jours_wind

wind$wind_bin_3_10_part<-wind$wind_bin_3_10/wind$nbr_jours_wind

wind$wind_bin_10_20_part<-wind$wind_bin_10_20/wind$nbr_jours_wind

wind$wind_bin_plus_20_part<-wind$wind_bin_plus_20/wind$nbr_jours_wind



wind$total_part<-wind$wind_bin_0_3_part+wind$wind_bin_3_10_part+wind$wind_bin_10_20_part+wind$wind_bin_plus_20_part

table(wind$total_part)
#1 OK





wind<-wind[,-c(4:8,13)]


wind$mois<-as.numeric(wind$mois)
wind$year<-as.numeric(wind$year)



communes_dates_1980_2022_temperature_final_mois<-fread("/données communes années/données mortalité temperature final mois/communes_dates_1980_2022_temperature_final_mois_humidity_rain.csv")

communes_dates_1980_2022_temperature_final_mois<-left_join(communes_dates_1980_2022_temperature_final_mois,wind)

test_na<-filter(communes_dates_1980_2022_temperature_final_mois, is.na(communes_dates_1980_2022_temperature_final_mois$wind_bin_plus_20_part))
#23 000 missing


#fwrite(communes_dates_1980_2022_temperature_final_mois,"/données communes années/données mortalité temperature final mois/communes_dates_1980_2022_temperature_final_mois_humidity_rain_wind.csv")











