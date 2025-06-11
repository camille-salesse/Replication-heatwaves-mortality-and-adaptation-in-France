

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






table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1980 <- read_delim("fichier deces insee/deces-1980.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1980<-left_join(deces.1980, table_passage_bis)


deces.1980$COM<-ifelse(!is.na(deces.1980$COM_AP),deces.1980$COM_AP,deces.1980$lieudeces)

deces.1980$datedeces <- as.character(deces.1980$datedeces)
deces.1980$datedeces <- as.Date(deces.1980$datedeces, format = "%Y%m%d")


deces.1980$datenaiss <- as.character(deces.1980$datenaiss)
deces.1980$datenaiss <- as.Date(deces.1980$datenaiss, format = "%Y%m%d")


deces.1980$year <- as.numeric(format(deces.1980$datedeces, "%Y"))
deces.1980<-filter(deces.1980, deces.1980$year==1980)
deces.1980<-deces.1980[,-12]

library(lubridate)
deces.1980$age <- as.period(interval(deces.1980$datenaiss , deces.1980$datedeces ))
deces.1980$age_years <- year(deces.1980$age)

library(readr)


deces.1980$sexe<-as.character(deces.1980$sexe)

deces.1980$SEX<-ifelse(deces.1980$sexe=="1","Homme","Femme")

deces.1980_final<-deces.1980[,c("COM","datedeces","age_years","SEX")]

deces.1980_final$nbr_mort<-1


deces.1980_final$month <- as.numeric(format(deces.1980_final$datedeces, "%m"))




deces.1980_final$tranche_age <- cut(deces.1980_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1980_final$month <- as.numeric(format(deces.1980_final$datedeces, "%m"))


deces.1980_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1980_final,sum)

library(tidyr)

deces.1980_spread <- deces.1980_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1980_spread$`0-9`[is.na(deces.1980_spread$`0-9`)]<-0
deces.1980_spread$`10-19`[is.na(deces.1980_spread$`10-19`)]<-0
deces.1980_spread$`20-39`[is.na(deces.1980_spread$`20-39`)]<-0
deces.1980_spread$`40-59`[is.na(deces.1980_spread$`40-59`)]<-0
deces.1980_spread$`60-64`[is.na(deces.1980_spread$`60-64`)]<-0
deces.1980_spread$`65-69`[is.na(deces.1980_spread$`65-69`)]<-0
deces.1980_spread$`70-74`[is.na(deces.1980_spread$`70-74`)]<-0
deces.1980_spread$`75-79`[is.na(deces.1980_spread$`75-79`)]<-0
deces.1980_spread$`80+`[is.na(deces.1980_spread$`80+`)]<-0



#deces.1980_spread<-left_join(deces.1980_spread, deces.1980_spread)



deces.1980_spread$year<-1980


#fwrite(deces.1980_spread,"/fichier deces insee/décès travaillé/deces.1980_age_sexe.csv")




#################### partie 2 ######


RP_1980_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1980_age_sexe_final_2")

RP_1980_age_sexe_final_2<-RP_1980_age_sexe_final_2[,c(2:15)]

names(RP_1980_age_sexe_final_2)[names(RP_1980_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1980_age_sexe_final_2<-filter(RP_1980_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1980_mortalite<-left_join(deces.1980_spread,RP_1980_age_sexe_final_2)


Base_1980_mortalite$`75+`<-Base_1980_mortalite$`75-79`+Base_1980_mortalite$`80+`
Base_1980_mortalite$value_estimated_sum_75_plus_h_f<-Base_1980_mortalite$value_estimated_sum_75_79_h_f+Base_1980_mortalite$value_estimated_sum_80_plus_h_f


#Base_1980_mortalite$taux_mortalite_homme<-Base_1980_mortalite$Homme/Base_1980_mortalite$value_estimated_sum_homme

#Base_1980_mortalite$taux_mortalite_femme<-Base_1980_mortalite$Femme/Base_1980_mortalite$value_estimated_sum_femme

Base_1980_mortalite$taux_mortalite_0_9<-Base_1980_mortalite$`0-9`/Base_1980_mortalite$value_estimated_sum_0_9_h_f

Base_1980_mortalite$taux_mortalite_10_19<-Base_1980_mortalite$`10-19`/Base_1980_mortalite$value_estimated_sum_10_19_h_f

Base_1980_mortalite$taux_mortalite_20_39<-Base_1980_mortalite$`20-39`/Base_1980_mortalite$value_estimated_sum_20_39_h_f

Base_1980_mortalite$taux_mortalite_40_59<-Base_1980_mortalite$`40-59`/Base_1980_mortalite$value_estimated_sum_40_59_h_f

Base_1980_mortalite$taux_mortalite_60_64<-Base_1980_mortalite$`60-64`/Base_1980_mortalite$value_estimated_sum_60_64_h_f

Base_1980_mortalite$taux_mortalite_65_69<-Base_1980_mortalite$`65-69`/Base_1980_mortalite$value_estimated_sum_65_69_h_f

Base_1980_mortalite$taux_mortalite_70_74<-Base_1980_mortalite$`70-74`/Base_1980_mortalite$value_estimated_sum_70_74_h_f

Base_1980_mortalite$taux_mortalite_75_79<-Base_1980_mortalite$`75-79`/Base_1980_mortalite$value_estimated_sum_75_79_h_f

Base_1980_mortalite$taux_mortalite_75_plus<-Base_1980_mortalite$`75+`/Base_1980_mortalite$value_estimated_sum_75_plus_h_f

Base_1980_mortalite$taux_mortalite_80_plus<-Base_1980_mortalite$`80+`/Base_1980_mortalite$value_estimated_sum_80_plus_h_f

#Base_1980_mortalite$mort_total<-Base_1980_mortalite$Femme+Base_1980_mortalite$Homme

Base_1980_mortalite$mort_total<-Base_1980_mortalite$`0-9`+Base_1980_mortalite$`10-19`+Base_1980_mortalite$`20-39` +Base_1980_mortalite$`40-59` +Base_1980_mortalite$`60-64` +Base_1980_mortalite$`65-69` +Base_1980_mortalite$`70-74` +Base_1980_mortalite$`75+`



Base_1980_mortalite$taux_mortalite_total<-Base_1980_mortalite$mort_total/Base_1980_mortalite$value_estimated_population


Base_1980_mortalite<-filter(Base_1980_mortalite, Base_1980_mortalite$value_estimated_population>0)


Base_1980_mortalite<-Base_1980_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1980_mortalite<-filter(Base_1980_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1980_mortalite,"/heatwave and mortality code and data/new data/Base_1980_mortalite.csv")




rm(list = ls())
gc()


#################################### 





table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1981 <- read_delim("fichier deces insee/deces-1981.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1981<-left_join(deces.1981, table_passage_bis)


deces.1981$COM<-ifelse(!is.na(deces.1981$COM_AP),deces.1981$COM_AP,deces.1981$lieudeces)

deces.1981$datedeces <- as.character(deces.1981$datedeces)
deces.1981$datedeces <- as.Date(deces.1981$datedeces, format = "%Y%m%d")


deces.1981$datenaiss <- as.character(deces.1981$datenaiss)
deces.1981$datenaiss <- as.Date(deces.1981$datenaiss, format = "%Y%m%d")


deces.1981$year <- as.numeric(format(deces.1981$datedeces, "%Y"))
deces.1981<-filter(deces.1981, deces.1981$year==1981)
deces.1981<-deces.1981[,-12]

library(lubridate)
deces.1981$age <- as.period(interval(deces.1981$datenaiss , deces.1981$datedeces ))
deces.1981$age_years <- year(deces.1981$age)

library(readr)


deces.1981$sexe<-as.character(deces.1981$sexe)

deces.1981$SEX<-ifelse(deces.1981$sexe=="1","Homme","Femme")

deces.1981_final<-deces.1981[,c("COM","datedeces","age_years","SEX")]

deces.1981_final$nbr_mort<-1


deces.1981_final$month <- as.numeric(format(deces.1981_final$datedeces, "%m"))




deces.1981_final$tranche_age <- cut(deces.1981_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1981_final$month <- as.numeric(format(deces.1981_final$datedeces, "%m"))


deces.1981_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1981_final,sum)

library(tidyr)

deces.1981_spread <- deces.1981_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1981_spread$`0-9`[is.na(deces.1981_spread$`0-9`)]<-0
deces.1981_spread$`10-19`[is.na(deces.1981_spread$`10-19`)]<-0
deces.1981_spread$`20-39`[is.na(deces.1981_spread$`20-39`)]<-0
deces.1981_spread$`40-59`[is.na(deces.1981_spread$`40-59`)]<-0
deces.1981_spread$`60-64`[is.na(deces.1981_spread$`60-64`)]<-0
deces.1981_spread$`65-69`[is.na(deces.1981_spread$`65-69`)]<-0
deces.1981_spread$`70-74`[is.na(deces.1981_spread$`70-74`)]<-0
deces.1981_spread$`75-79`[is.na(deces.1981_spread$`75-79`)]<-0
deces.1981_spread$`80+`[is.na(deces.1981_spread$`80+`)]<-0



#deces.1981_spread<-left_join(deces.1981_spread, deces.1981_spread)



deces.1981_spread$year<-1981


#fwrite(deces.1981_spread,"/fichier deces insee/décès travaillé/deces.1981_age_sexe.csv")




#################### partie 2 ######


RP_1981_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1981_age_sexe_final_2")

RP_1981_age_sexe_final_2<-RP_1981_age_sexe_final_2[,c(2:15)]

names(RP_1981_age_sexe_final_2)[names(RP_1981_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1981_age_sexe_final_2<-filter(RP_1981_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1981_mortalite<-left_join(deces.1981_spread,RP_1981_age_sexe_final_2)


Base_1981_mortalite$`75+`<-Base_1981_mortalite$`75-79`+Base_1981_mortalite$`80+`
Base_1981_mortalite$value_estimated_sum_75_plus_h_f<-Base_1981_mortalite$value_estimated_sum_75_79_h_f+Base_1981_mortalite$value_estimated_sum_80_plus_h_f


#Base_1981_mortalite$taux_mortalite_homme<-Base_1981_mortalite$Homme/Base_1981_mortalite$value_estimated_sum_homme

#Base_1981_mortalite$taux_mortalite_femme<-Base_1981_mortalite$Femme/Base_1981_mortalite$value_estimated_sum_femme

Base_1981_mortalite$taux_mortalite_0_9<-Base_1981_mortalite$`0-9`/Base_1981_mortalite$value_estimated_sum_0_9_h_f

Base_1981_mortalite$taux_mortalite_10_19<-Base_1981_mortalite$`10-19`/Base_1981_mortalite$value_estimated_sum_10_19_h_f

Base_1981_mortalite$taux_mortalite_20_39<-Base_1981_mortalite$`20-39`/Base_1981_mortalite$value_estimated_sum_20_39_h_f

Base_1981_mortalite$taux_mortalite_40_59<-Base_1981_mortalite$`40-59`/Base_1981_mortalite$value_estimated_sum_40_59_h_f

Base_1981_mortalite$taux_mortalite_60_64<-Base_1981_mortalite$`60-64`/Base_1981_mortalite$value_estimated_sum_60_64_h_f

Base_1981_mortalite$taux_mortalite_65_69<-Base_1981_mortalite$`65-69`/Base_1981_mortalite$value_estimated_sum_65_69_h_f

Base_1981_mortalite$taux_mortalite_70_74<-Base_1981_mortalite$`70-74`/Base_1981_mortalite$value_estimated_sum_70_74_h_f

Base_1981_mortalite$taux_mortalite_75_79<-Base_1981_mortalite$`75-79`/Base_1981_mortalite$value_estimated_sum_75_79_h_f

Base_1981_mortalite$taux_mortalite_75_plus<-Base_1981_mortalite$`75+`/Base_1981_mortalite$value_estimated_sum_75_plus_h_f

Base_1981_mortalite$taux_mortalite_80_plus<-Base_1981_mortalite$`80+`/Base_1981_mortalite$value_estimated_sum_80_plus_h_f

#Base_1981_mortalite$mort_total<-Base_1981_mortalite$Femme+Base_1981_mortalite$Homme

Base_1981_mortalite$mort_total<-Base_1981_mortalite$`0-9`+Base_1981_mortalite$`10-19`+Base_1981_mortalite$`20-39` +Base_1981_mortalite$`40-59` +Base_1981_mortalite$`60-64` +Base_1981_mortalite$`65-69` +Base_1981_mortalite$`70-74` +Base_1981_mortalite$`75+`



Base_1981_mortalite$taux_mortalite_total<-Base_1981_mortalite$mort_total/Base_1981_mortalite$value_estimated_population


Base_1981_mortalite<-filter(Base_1981_mortalite, Base_1981_mortalite$value_estimated_population>0)


Base_1981_mortalite<-Base_1981_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1981_mortalite<-filter(Base_1981_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1981_mortalite,"/heatwave and mortality code and data/new data/Base_1981_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1982 <- read_delim("fichier deces insee/deces-1982.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1982<-left_join(deces.1982, table_passage_bis)


deces.1982$COM<-ifelse(!is.na(deces.1982$COM_AP),deces.1982$COM_AP,deces.1982$lieudeces)

deces.1982$datedeces <- as.character(deces.1982$datedeces)
deces.1982$datedeces <- as.Date(deces.1982$datedeces, format = "%Y%m%d")


deces.1982$datenaiss <- as.character(deces.1982$datenaiss)
deces.1982$datenaiss <- as.Date(deces.1982$datenaiss, format = "%Y%m%d")


deces.1982$year <- as.numeric(format(deces.1982$datedeces, "%Y"))
deces.1982<-filter(deces.1982, deces.1982$year==1982)
deces.1982<-deces.1982[,-12]

library(lubridate)
deces.1982$age <- as.period(interval(deces.1982$datenaiss , deces.1982$datedeces ))
deces.1982$age_years <- year(deces.1982$age)

library(readr)


deces.1982$sexe<-as.character(deces.1982$sexe)

deces.1982$SEX<-ifelse(deces.1982$sexe=="1","Homme","Femme")

deces.1982_final<-deces.1982[,c("COM","datedeces","age_years","SEX")]

deces.1982_final$nbr_mort<-1


deces.1982_final$month <- as.numeric(format(deces.1982_final$datedeces, "%m"))




deces.1982_final$tranche_age <- cut(deces.1982_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1982_final$month <- as.numeric(format(deces.1982_final$datedeces, "%m"))


deces.1982_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1982_final,sum)

library(tidyr)

deces.1982_spread <- deces.1982_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1982_spread$`0-9`[is.na(deces.1982_spread$`0-9`)]<-0
deces.1982_spread$`10-19`[is.na(deces.1982_spread$`10-19`)]<-0
deces.1982_spread$`20-39`[is.na(deces.1982_spread$`20-39`)]<-0
deces.1982_spread$`40-59`[is.na(deces.1982_spread$`40-59`)]<-0
deces.1982_spread$`60-64`[is.na(deces.1982_spread$`60-64`)]<-0
deces.1982_spread$`65-69`[is.na(deces.1982_spread$`65-69`)]<-0
deces.1982_spread$`70-74`[is.na(deces.1982_spread$`70-74`)]<-0
deces.1982_spread$`75-79`[is.na(deces.1982_spread$`75-79`)]<-0
deces.1982_spread$`80+`[is.na(deces.1982_spread$`80+`)]<-0



#deces.1982_spread<-left_join(deces.1982_spread, deces.1982_spread)



deces.1982_spread$year<-1982


#fwrite(deces.1982_spread,"/fichier deces insee/décès travaillé/deces.1982_age_sexe.csv")




#################### partie 2 ######


RP_1982_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1982_age_sexe_final_2")

RP_1982_age_sexe_final_2<-RP_1982_age_sexe_final_2[,c(2:15)]

names(RP_1982_age_sexe_final_2)[names(RP_1982_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1982_age_sexe_final_2<-filter(RP_1982_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1982_mortalite<-left_join(deces.1982_spread,RP_1982_age_sexe_final_2)


Base_1982_mortalite$`75+`<-Base_1982_mortalite$`75-79`+Base_1982_mortalite$`80+`
Base_1982_mortalite$value_estimated_sum_75_plus_h_f<-Base_1982_mortalite$value_estimated_sum_75_79_h_f+Base_1982_mortalite$value_estimated_sum_80_plus_h_f


#Base_1982_mortalite$taux_mortalite_homme<-Base_1982_mortalite$Homme/Base_1982_mortalite$value_estimated_sum_homme

#Base_1982_mortalite$taux_mortalite_femme<-Base_1982_mortalite$Femme/Base_1982_mortalite$value_estimated_sum_femme

Base_1982_mortalite$taux_mortalite_0_9<-Base_1982_mortalite$`0-9`/Base_1982_mortalite$value_estimated_sum_0_9_h_f

Base_1982_mortalite$taux_mortalite_10_19<-Base_1982_mortalite$`10-19`/Base_1982_mortalite$value_estimated_sum_10_19_h_f

Base_1982_mortalite$taux_mortalite_20_39<-Base_1982_mortalite$`20-39`/Base_1982_mortalite$value_estimated_sum_20_39_h_f

Base_1982_mortalite$taux_mortalite_40_59<-Base_1982_mortalite$`40-59`/Base_1982_mortalite$value_estimated_sum_40_59_h_f

Base_1982_mortalite$taux_mortalite_60_64<-Base_1982_mortalite$`60-64`/Base_1982_mortalite$value_estimated_sum_60_64_h_f

Base_1982_mortalite$taux_mortalite_65_69<-Base_1982_mortalite$`65-69`/Base_1982_mortalite$value_estimated_sum_65_69_h_f

Base_1982_mortalite$taux_mortalite_70_74<-Base_1982_mortalite$`70-74`/Base_1982_mortalite$value_estimated_sum_70_74_h_f

Base_1982_mortalite$taux_mortalite_75_79<-Base_1982_mortalite$`75-79`/Base_1982_mortalite$value_estimated_sum_75_79_h_f

Base_1982_mortalite$taux_mortalite_75_plus<-Base_1982_mortalite$`75+`/Base_1982_mortalite$value_estimated_sum_75_plus_h_f

Base_1982_mortalite$taux_mortalite_80_plus<-Base_1982_mortalite$`80+`/Base_1982_mortalite$value_estimated_sum_80_plus_h_f

#Base_1982_mortalite$mort_total<-Base_1982_mortalite$Femme+Base_1982_mortalite$Homme

Base_1982_mortalite$mort_total<-Base_1982_mortalite$`0-9`+Base_1982_mortalite$`10-19`+Base_1982_mortalite$`20-39` +Base_1982_mortalite$`40-59` +Base_1982_mortalite$`60-64` +Base_1982_mortalite$`65-69` +Base_1982_mortalite$`70-74` +Base_1982_mortalite$`75+`



Base_1982_mortalite$taux_mortalite_total<-Base_1982_mortalite$mort_total/Base_1982_mortalite$value_estimated_population


Base_1982_mortalite<-filter(Base_1982_mortalite, Base_1982_mortalite$value_estimated_population>0)


Base_1982_mortalite<-Base_1982_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1982_mortalite<-filter(Base_1982_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1982_mortalite,"/heatwave and mortality code and data/new data/Base_1982_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1983 <- read_delim("fichier deces insee/deces-1983.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1983<-left_join(deces.1983, table_passage_bis)


deces.1983$COM<-ifelse(!is.na(deces.1983$COM_AP),deces.1983$COM_AP,deces.1983$lieudeces)

deces.1983$datedeces <- as.character(deces.1983$datedeces)
deces.1983$datedeces <- as.Date(deces.1983$datedeces, format = "%Y%m%d")


deces.1983$datenaiss <- as.character(deces.1983$datenaiss)
deces.1983$datenaiss <- as.Date(deces.1983$datenaiss, format = "%Y%m%d")


deces.1983$year <- as.numeric(format(deces.1983$datedeces, "%Y"))
deces.1983<-filter(deces.1983, deces.1983$year==1983)
deces.1983<-deces.1983[,-12]

library(lubridate)
deces.1983$age <- as.period(interval(deces.1983$datenaiss , deces.1983$datedeces ))
deces.1983$age_years <- year(deces.1983$age)

library(readr)


deces.1983$sexe<-as.character(deces.1983$sexe)

deces.1983$SEX<-ifelse(deces.1983$sexe=="1","Homme","Femme")

deces.1983_final<-deces.1983[,c("COM","datedeces","age_years","SEX")]

deces.1983_final$nbr_mort<-1


deces.1983_final$month <- as.numeric(format(deces.1983_final$datedeces, "%m"))




deces.1983_final$tranche_age <- cut(deces.1983_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1983_final$month <- as.numeric(format(deces.1983_final$datedeces, "%m"))


deces.1983_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1983_final,sum)

library(tidyr)

deces.1983_spread <- deces.1983_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1983_spread$`0-9`[is.na(deces.1983_spread$`0-9`)]<-0
deces.1983_spread$`10-19`[is.na(deces.1983_spread$`10-19`)]<-0
deces.1983_spread$`20-39`[is.na(deces.1983_spread$`20-39`)]<-0
deces.1983_spread$`40-59`[is.na(deces.1983_spread$`40-59`)]<-0
deces.1983_spread$`60-64`[is.na(deces.1983_spread$`60-64`)]<-0
deces.1983_spread$`65-69`[is.na(deces.1983_spread$`65-69`)]<-0
deces.1983_spread$`70-74`[is.na(deces.1983_spread$`70-74`)]<-0
deces.1983_spread$`75-79`[is.na(deces.1983_spread$`75-79`)]<-0
deces.1983_spread$`80+`[is.na(deces.1983_spread$`80+`)]<-0



#deces.1983_spread<-left_join(deces.1983_spread, deces.1983_spread)



deces.1983_spread$year<-1983


#fwrite(deces.1983_spread,"/fichier deces insee/décès travaillé/deces.1983_age_sexe.csv")




#################### partie 2 ######


RP_1983_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1983_age_sexe_final_2")

RP_1983_age_sexe_final_2<-RP_1983_age_sexe_final_2[,c(2:15)]

names(RP_1983_age_sexe_final_2)[names(RP_1983_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1983_age_sexe_final_2<-filter(RP_1983_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1983_mortalite<-left_join(deces.1983_spread,RP_1983_age_sexe_final_2)


Base_1983_mortalite$`75+`<-Base_1983_mortalite$`75-79`+Base_1983_mortalite$`80+`
Base_1983_mortalite$value_estimated_sum_75_plus_h_f<-Base_1983_mortalite$value_estimated_sum_75_79_h_f+Base_1983_mortalite$value_estimated_sum_80_plus_h_f


#Base_1983_mortalite$taux_mortalite_homme<-Base_1983_mortalite$Homme/Base_1983_mortalite$value_estimated_sum_homme

#Base_1983_mortalite$taux_mortalite_femme<-Base_1983_mortalite$Femme/Base_1983_mortalite$value_estimated_sum_femme

Base_1983_mortalite$taux_mortalite_0_9<-Base_1983_mortalite$`0-9`/Base_1983_mortalite$value_estimated_sum_0_9_h_f

Base_1983_mortalite$taux_mortalite_10_19<-Base_1983_mortalite$`10-19`/Base_1983_mortalite$value_estimated_sum_10_19_h_f

Base_1983_mortalite$taux_mortalite_20_39<-Base_1983_mortalite$`20-39`/Base_1983_mortalite$value_estimated_sum_20_39_h_f

Base_1983_mortalite$taux_mortalite_40_59<-Base_1983_mortalite$`40-59`/Base_1983_mortalite$value_estimated_sum_40_59_h_f

Base_1983_mortalite$taux_mortalite_60_64<-Base_1983_mortalite$`60-64`/Base_1983_mortalite$value_estimated_sum_60_64_h_f

Base_1983_mortalite$taux_mortalite_65_69<-Base_1983_mortalite$`65-69`/Base_1983_mortalite$value_estimated_sum_65_69_h_f

Base_1983_mortalite$taux_mortalite_70_74<-Base_1983_mortalite$`70-74`/Base_1983_mortalite$value_estimated_sum_70_74_h_f

Base_1983_mortalite$taux_mortalite_75_79<-Base_1983_mortalite$`75-79`/Base_1983_mortalite$value_estimated_sum_75_79_h_f

Base_1983_mortalite$taux_mortalite_75_plus<-Base_1983_mortalite$`75+`/Base_1983_mortalite$value_estimated_sum_75_plus_h_f

Base_1983_mortalite$taux_mortalite_80_plus<-Base_1983_mortalite$`80+`/Base_1983_mortalite$value_estimated_sum_80_plus_h_f

#Base_1983_mortalite$mort_total<-Base_1983_mortalite$Femme+Base_1983_mortalite$Homme

Base_1983_mortalite$mort_total<-Base_1983_mortalite$`0-9`+Base_1983_mortalite$`10-19`+Base_1983_mortalite$`20-39` +Base_1983_mortalite$`40-59` +Base_1983_mortalite$`60-64` +Base_1983_mortalite$`65-69` +Base_1983_mortalite$`70-74` +Base_1983_mortalite$`75+`



Base_1983_mortalite$taux_mortalite_total<-Base_1983_mortalite$mort_total/Base_1983_mortalite$value_estimated_population


Base_1983_mortalite<-filter(Base_1983_mortalite, Base_1983_mortalite$value_estimated_population>0)


Base_1983_mortalite<-Base_1983_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1983_mortalite<-filter(Base_1983_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1983_mortalite,"/heatwave and mortality code and data/new data/Base_1983_mortalite.csv")




rm(list = ls())
gc()


#################################### 



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1984 <- read_delim("fichier deces insee/deces-1984.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1984<-left_join(deces.1984, table_passage_bis)


deces.1984$COM<-ifelse(!is.na(deces.1984$COM_AP),deces.1984$COM_AP,deces.1984$lieudeces)

deces.1984$datedeces <- as.character(deces.1984$datedeces)
deces.1984$datedeces <- as.Date(deces.1984$datedeces, format = "%Y%m%d")


deces.1984$datenaiss <- as.character(deces.1984$datenaiss)
deces.1984$datenaiss <- as.Date(deces.1984$datenaiss, format = "%Y%m%d")


deces.1984$year <- as.numeric(format(deces.1984$datedeces, "%Y"))
deces.1984<-filter(deces.1984, deces.1984$year==1984)
deces.1984<-deces.1984[,-12]

library(lubridate)
deces.1984$age <- as.period(interval(deces.1984$datenaiss , deces.1984$datedeces ))
deces.1984$age_years <- year(deces.1984$age)

library(readr)


deces.1984$sexe<-as.character(deces.1984$sexe)

deces.1984$SEX<-ifelse(deces.1984$sexe=="1","Homme","Femme")

deces.1984_final<-deces.1984[,c("COM","datedeces","age_years","SEX")]

deces.1984_final$nbr_mort<-1


deces.1984_final$month <- as.numeric(format(deces.1984_final$datedeces, "%m"))




deces.1984_final$tranche_age <- cut(deces.1984_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1984_final$month <- as.numeric(format(deces.1984_final$datedeces, "%m"))


deces.1984_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1984_final,sum)

library(tidyr)

deces.1984_spread <- deces.1984_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1984_spread$`0-9`[is.na(deces.1984_spread$`0-9`)]<-0
deces.1984_spread$`10-19`[is.na(deces.1984_spread$`10-19`)]<-0
deces.1984_spread$`20-39`[is.na(deces.1984_spread$`20-39`)]<-0
deces.1984_spread$`40-59`[is.na(deces.1984_spread$`40-59`)]<-0
deces.1984_spread$`60-64`[is.na(deces.1984_spread$`60-64`)]<-0
deces.1984_spread$`65-69`[is.na(deces.1984_spread$`65-69`)]<-0
deces.1984_spread$`70-74`[is.na(deces.1984_spread$`70-74`)]<-0
deces.1984_spread$`75-79`[is.na(deces.1984_spread$`75-79`)]<-0
deces.1984_spread$`80+`[is.na(deces.1984_spread$`80+`)]<-0



#deces.1984_spread<-left_join(deces.1984_spread, deces.1984_spread)



deces.1984_spread$year<-1984


#fwrite(deces.1984_spread,"/fichier deces insee/décès travaillé/deces.1984_age_sexe.csv")




#################### partie 2 ######


RP_1984_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1984_age_sexe_final_2")

RP_1984_age_sexe_final_2<-RP_1984_age_sexe_final_2[,c(2:15)]

names(RP_1984_age_sexe_final_2)[names(RP_1984_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1984_age_sexe_final_2<-filter(RP_1984_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1984_mortalite<-left_join(deces.1984_spread,RP_1984_age_sexe_final_2)


Base_1984_mortalite$`75+`<-Base_1984_mortalite$`75-79`+Base_1984_mortalite$`80+`
Base_1984_mortalite$value_estimated_sum_75_plus_h_f<-Base_1984_mortalite$value_estimated_sum_75_79_h_f+Base_1984_mortalite$value_estimated_sum_80_plus_h_f


#Base_1984_mortalite$taux_mortalite_homme<-Base_1984_mortalite$Homme/Base_1984_mortalite$value_estimated_sum_homme

#Base_1984_mortalite$taux_mortalite_femme<-Base_1984_mortalite$Femme/Base_1984_mortalite$value_estimated_sum_femme

Base_1984_mortalite$taux_mortalite_0_9<-Base_1984_mortalite$`0-9`/Base_1984_mortalite$value_estimated_sum_0_9_h_f

Base_1984_mortalite$taux_mortalite_10_19<-Base_1984_mortalite$`10-19`/Base_1984_mortalite$value_estimated_sum_10_19_h_f

Base_1984_mortalite$taux_mortalite_20_39<-Base_1984_mortalite$`20-39`/Base_1984_mortalite$value_estimated_sum_20_39_h_f

Base_1984_mortalite$taux_mortalite_40_59<-Base_1984_mortalite$`40-59`/Base_1984_mortalite$value_estimated_sum_40_59_h_f

Base_1984_mortalite$taux_mortalite_60_64<-Base_1984_mortalite$`60-64`/Base_1984_mortalite$value_estimated_sum_60_64_h_f

Base_1984_mortalite$taux_mortalite_65_69<-Base_1984_mortalite$`65-69`/Base_1984_mortalite$value_estimated_sum_65_69_h_f

Base_1984_mortalite$taux_mortalite_70_74<-Base_1984_mortalite$`70-74`/Base_1984_mortalite$value_estimated_sum_70_74_h_f

Base_1984_mortalite$taux_mortalite_75_79<-Base_1984_mortalite$`75-79`/Base_1984_mortalite$value_estimated_sum_75_79_h_f

Base_1984_mortalite$taux_mortalite_75_plus<-Base_1984_mortalite$`75+`/Base_1984_mortalite$value_estimated_sum_75_plus_h_f

Base_1984_mortalite$taux_mortalite_80_plus<-Base_1984_mortalite$`80+`/Base_1984_mortalite$value_estimated_sum_80_plus_h_f

#Base_1984_mortalite$mort_total<-Base_1984_mortalite$Femme+Base_1984_mortalite$Homme

Base_1984_mortalite$mort_total<-Base_1984_mortalite$`0-9`+Base_1984_mortalite$`10-19`+Base_1984_mortalite$`20-39` +Base_1984_mortalite$`40-59` +Base_1984_mortalite$`60-64` +Base_1984_mortalite$`65-69` +Base_1984_mortalite$`70-74` +Base_1984_mortalite$`75+`



Base_1984_mortalite$taux_mortalite_total<-Base_1984_mortalite$mort_total/Base_1984_mortalite$value_estimated_population


Base_1984_mortalite<-filter(Base_1984_mortalite, Base_1984_mortalite$value_estimated_population>0)


Base_1984_mortalite<-Base_1984_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1984_mortalite<-filter(Base_1984_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1984_mortalite,"/heatwave and mortality code and data/new data/Base_1984_mortalite.csv")




rm(list = ls())
gc()


#################################### 



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1985 <- read_delim("fichier deces insee/deces-1985.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1985<-left_join(deces.1985, table_passage_bis)


deces.1985$COM<-ifelse(!is.na(deces.1985$COM_AP),deces.1985$COM_AP,deces.1985$lieudeces)

deces.1985$datedeces <- as.character(deces.1985$datedeces)
deces.1985$datedeces <- as.Date(deces.1985$datedeces, format = "%Y%m%d")


deces.1985$datenaiss <- as.character(deces.1985$datenaiss)
deces.1985$datenaiss <- as.Date(deces.1985$datenaiss, format = "%Y%m%d")


deces.1985$year <- as.numeric(format(deces.1985$datedeces, "%Y"))
deces.1985<-filter(deces.1985, deces.1985$year==1985)
deces.1985<-deces.1985[,-12]

library(lubridate)
deces.1985$age <- as.period(interval(deces.1985$datenaiss , deces.1985$datedeces ))
deces.1985$age_years <- year(deces.1985$age)

library(readr)


deces.1985$sexe<-as.character(deces.1985$sexe)

deces.1985$SEX<-ifelse(deces.1985$sexe=="1","Homme","Femme")

deces.1985_final<-deces.1985[,c("COM","datedeces","age_years","SEX")]

deces.1985_final$nbr_mort<-1


deces.1985_final$month <- as.numeric(format(deces.1985_final$datedeces, "%m"))




deces.1985_final$tranche_age <- cut(deces.1985_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1985_final$month <- as.numeric(format(deces.1985_final$datedeces, "%m"))


deces.1985_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1985_final,sum)

library(tidyr)

deces.1985_spread <- deces.1985_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1985_spread$`0-9`[is.na(deces.1985_spread$`0-9`)]<-0
deces.1985_spread$`10-19`[is.na(deces.1985_spread$`10-19`)]<-0
deces.1985_spread$`20-39`[is.na(deces.1985_spread$`20-39`)]<-0
deces.1985_spread$`40-59`[is.na(deces.1985_spread$`40-59`)]<-0
deces.1985_spread$`60-64`[is.na(deces.1985_spread$`60-64`)]<-0
deces.1985_spread$`65-69`[is.na(deces.1985_spread$`65-69`)]<-0
deces.1985_spread$`70-74`[is.na(deces.1985_spread$`70-74`)]<-0
deces.1985_spread$`75-79`[is.na(deces.1985_spread$`75-79`)]<-0
deces.1985_spread$`80+`[is.na(deces.1985_spread$`80+`)]<-0



#deces.1985_spread<-left_join(deces.1985_spread, deces.1985_spread)



deces.1985_spread$year<-1985


#fwrite(deces.1985_spread,"/fichier deces insee/décès travaillé/deces.1985_age_sexe.csv")




#################### partie 2 ######


RP_1985_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1985_age_sexe_final_2")

RP_1985_age_sexe_final_2<-RP_1985_age_sexe_final_2[,c(2:15)]

names(RP_1985_age_sexe_final_2)[names(RP_1985_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1985_age_sexe_final_2<-filter(RP_1985_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1985_mortalite<-left_join(deces.1985_spread,RP_1985_age_sexe_final_2)


Base_1985_mortalite$`75+`<-Base_1985_mortalite$`75-79`+Base_1985_mortalite$`80+`
Base_1985_mortalite$value_estimated_sum_75_plus_h_f<-Base_1985_mortalite$value_estimated_sum_75_79_h_f+Base_1985_mortalite$value_estimated_sum_80_plus_h_f


#Base_1985_mortalite$taux_mortalite_homme<-Base_1985_mortalite$Homme/Base_1985_mortalite$value_estimated_sum_homme

#Base_1985_mortalite$taux_mortalite_femme<-Base_1985_mortalite$Femme/Base_1985_mortalite$value_estimated_sum_femme

Base_1985_mortalite$taux_mortalite_0_9<-Base_1985_mortalite$`0-9`/Base_1985_mortalite$value_estimated_sum_0_9_h_f

Base_1985_mortalite$taux_mortalite_10_19<-Base_1985_mortalite$`10-19`/Base_1985_mortalite$value_estimated_sum_10_19_h_f

Base_1985_mortalite$taux_mortalite_20_39<-Base_1985_mortalite$`20-39`/Base_1985_mortalite$value_estimated_sum_20_39_h_f

Base_1985_mortalite$taux_mortalite_40_59<-Base_1985_mortalite$`40-59`/Base_1985_mortalite$value_estimated_sum_40_59_h_f

Base_1985_mortalite$taux_mortalite_60_64<-Base_1985_mortalite$`60-64`/Base_1985_mortalite$value_estimated_sum_60_64_h_f

Base_1985_mortalite$taux_mortalite_65_69<-Base_1985_mortalite$`65-69`/Base_1985_mortalite$value_estimated_sum_65_69_h_f

Base_1985_mortalite$taux_mortalite_70_74<-Base_1985_mortalite$`70-74`/Base_1985_mortalite$value_estimated_sum_70_74_h_f

Base_1985_mortalite$taux_mortalite_75_79<-Base_1985_mortalite$`75-79`/Base_1985_mortalite$value_estimated_sum_75_79_h_f

Base_1985_mortalite$taux_mortalite_75_plus<-Base_1985_mortalite$`75+`/Base_1985_mortalite$value_estimated_sum_75_plus_h_f

Base_1985_mortalite$taux_mortalite_80_plus<-Base_1985_mortalite$`80+`/Base_1985_mortalite$value_estimated_sum_80_plus_h_f

#Base_1985_mortalite$mort_total<-Base_1985_mortalite$Femme+Base_1985_mortalite$Homme

Base_1985_mortalite$mort_total<-Base_1985_mortalite$`0-9`+Base_1985_mortalite$`10-19`+Base_1985_mortalite$`20-39` +Base_1985_mortalite$`40-59` +Base_1985_mortalite$`60-64` +Base_1985_mortalite$`65-69` +Base_1985_mortalite$`70-74` +Base_1985_mortalite$`75+`



Base_1985_mortalite$taux_mortalite_total<-Base_1985_mortalite$mort_total/Base_1985_mortalite$value_estimated_population


Base_1985_mortalite<-filter(Base_1985_mortalite, Base_1985_mortalite$value_estimated_population>0)


Base_1985_mortalite<-Base_1985_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1985_mortalite<-filter(Base_1985_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1985_mortalite,"/heatwave and mortality code and data/new data/Base_1985_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1986 <- read_delim("fichier deces insee/deces-1986.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1986<-left_join(deces.1986, table_passage_bis)


deces.1986$COM<-ifelse(!is.na(deces.1986$COM_AP),deces.1986$COM_AP,deces.1986$lieudeces)

deces.1986$datedeces <- as.character(deces.1986$datedeces)
deces.1986$datedeces <- as.Date(deces.1986$datedeces, format = "%Y%m%d")


deces.1986$datenaiss <- as.character(deces.1986$datenaiss)
deces.1986$datenaiss <- as.Date(deces.1986$datenaiss, format = "%Y%m%d")


deces.1986$year <- as.numeric(format(deces.1986$datedeces, "%Y"))
deces.1986<-filter(deces.1986, deces.1986$year==1986)
deces.1986<-deces.1986[,-12]

library(lubridate)
deces.1986$age <- as.period(interval(deces.1986$datenaiss , deces.1986$datedeces ))
deces.1986$age_years <- year(deces.1986$age)

library(readr)


deces.1986$sexe<-as.character(deces.1986$sexe)

deces.1986$SEX<-ifelse(deces.1986$sexe=="1","Homme","Femme")

deces.1986_final<-deces.1986[,c("COM","datedeces","age_years","SEX")]

deces.1986_final$nbr_mort<-1


deces.1986_final$month <- as.numeric(format(deces.1986_final$datedeces, "%m"))




deces.1986_final$tranche_age <- cut(deces.1986_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1986_final$month <- as.numeric(format(deces.1986_final$datedeces, "%m"))


deces.1986_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1986_final,sum)

library(tidyr)

deces.1986_spread <- deces.1986_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1986_spread$`0-9`[is.na(deces.1986_spread$`0-9`)]<-0
deces.1986_spread$`10-19`[is.na(deces.1986_spread$`10-19`)]<-0
deces.1986_spread$`20-39`[is.na(deces.1986_spread$`20-39`)]<-0
deces.1986_spread$`40-59`[is.na(deces.1986_spread$`40-59`)]<-0
deces.1986_spread$`60-64`[is.na(deces.1986_spread$`60-64`)]<-0
deces.1986_spread$`65-69`[is.na(deces.1986_spread$`65-69`)]<-0
deces.1986_spread$`70-74`[is.na(deces.1986_spread$`70-74`)]<-0
deces.1986_spread$`75-79`[is.na(deces.1986_spread$`75-79`)]<-0
deces.1986_spread$`80+`[is.na(deces.1986_spread$`80+`)]<-0



#deces.1986_spread<-left_join(deces.1986_spread, deces.1986_spread)



deces.1986_spread$year<-1986


#fwrite(deces.1986_spread,"/fichier deces insee/décès travaillé/deces.1986_age_sexe.csv")




#################### partie 2 ######


RP_1986_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1986_age_sexe_final_2")

RP_1986_age_sexe_final_2<-RP_1986_age_sexe_final_2[,c(2:15)]

names(RP_1986_age_sexe_final_2)[names(RP_1986_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1986_age_sexe_final_2<-filter(RP_1986_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1986_mortalite<-left_join(deces.1986_spread,RP_1986_age_sexe_final_2)


Base_1986_mortalite$`75+`<-Base_1986_mortalite$`75-79`+Base_1986_mortalite$`80+`
Base_1986_mortalite$value_estimated_sum_75_plus_h_f<-Base_1986_mortalite$value_estimated_sum_75_79_h_f+Base_1986_mortalite$value_estimated_sum_80_plus_h_f


#Base_1986_mortalite$taux_mortalite_homme<-Base_1986_mortalite$Homme/Base_1986_mortalite$value_estimated_sum_homme

#Base_1986_mortalite$taux_mortalite_femme<-Base_1986_mortalite$Femme/Base_1986_mortalite$value_estimated_sum_femme

Base_1986_mortalite$taux_mortalite_0_9<-Base_1986_mortalite$`0-9`/Base_1986_mortalite$value_estimated_sum_0_9_h_f

Base_1986_mortalite$taux_mortalite_10_19<-Base_1986_mortalite$`10-19`/Base_1986_mortalite$value_estimated_sum_10_19_h_f

Base_1986_mortalite$taux_mortalite_20_39<-Base_1986_mortalite$`20-39`/Base_1986_mortalite$value_estimated_sum_20_39_h_f

Base_1986_mortalite$taux_mortalite_40_59<-Base_1986_mortalite$`40-59`/Base_1986_mortalite$value_estimated_sum_40_59_h_f

Base_1986_mortalite$taux_mortalite_60_64<-Base_1986_mortalite$`60-64`/Base_1986_mortalite$value_estimated_sum_60_64_h_f

Base_1986_mortalite$taux_mortalite_65_69<-Base_1986_mortalite$`65-69`/Base_1986_mortalite$value_estimated_sum_65_69_h_f

Base_1986_mortalite$taux_mortalite_70_74<-Base_1986_mortalite$`70-74`/Base_1986_mortalite$value_estimated_sum_70_74_h_f

Base_1986_mortalite$taux_mortalite_75_79<-Base_1986_mortalite$`75-79`/Base_1986_mortalite$value_estimated_sum_75_79_h_f

Base_1986_mortalite$taux_mortalite_75_plus<-Base_1986_mortalite$`75+`/Base_1986_mortalite$value_estimated_sum_75_plus_h_f

Base_1986_mortalite$taux_mortalite_80_plus<-Base_1986_mortalite$`80+`/Base_1986_mortalite$value_estimated_sum_80_plus_h_f

#Base_1986_mortalite$mort_total<-Base_1986_mortalite$Femme+Base_1986_mortalite$Homme

Base_1986_mortalite$mort_total<-Base_1986_mortalite$`0-9`+Base_1986_mortalite$`10-19`+Base_1986_mortalite$`20-39` +Base_1986_mortalite$`40-59` +Base_1986_mortalite$`60-64` +Base_1986_mortalite$`65-69` +Base_1986_mortalite$`70-74` +Base_1986_mortalite$`75+`



Base_1986_mortalite$taux_mortalite_total<-Base_1986_mortalite$mort_total/Base_1986_mortalite$value_estimated_population


Base_1986_mortalite<-filter(Base_1986_mortalite, Base_1986_mortalite$value_estimated_population>0)


Base_1986_mortalite<-Base_1986_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1986_mortalite<-filter(Base_1986_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1986_mortalite,"/heatwave and mortality code and data/new data/Base_1986_mortalite.csv")




rm(list = ls())
gc()


#################################### 



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1987 <- read_delim("fichier deces insee/deces-1987.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1987<-left_join(deces.1987, table_passage_bis)


deces.1987$COM<-ifelse(!is.na(deces.1987$COM_AP),deces.1987$COM_AP,deces.1987$lieudeces)

deces.1987$datedeces <- as.character(deces.1987$datedeces)
deces.1987$datedeces <- as.Date(deces.1987$datedeces, format = "%Y%m%d")


deces.1987$datenaiss <- as.character(deces.1987$datenaiss)
deces.1987$datenaiss <- as.Date(deces.1987$datenaiss, format = "%Y%m%d")


deces.1987$year <- as.numeric(format(deces.1987$datedeces, "%Y"))
deces.1987<-filter(deces.1987, deces.1987$year==1987)
deces.1987<-deces.1987[,-12]

library(lubridate)
deces.1987$age <- as.period(interval(deces.1987$datenaiss , deces.1987$datedeces ))
deces.1987$age_years <- year(deces.1987$age)

library(readr)


deces.1987$sexe<-as.character(deces.1987$sexe)

deces.1987$SEX<-ifelse(deces.1987$sexe=="1","Homme","Femme")

deces.1987_final<-deces.1987[,c("COM","datedeces","age_years","SEX")]

deces.1987_final$nbr_mort<-1


deces.1987_final$month <- as.numeric(format(deces.1987_final$datedeces, "%m"))




deces.1987_final$tranche_age <- cut(deces.1987_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1987_final$month <- as.numeric(format(deces.1987_final$datedeces, "%m"))


deces.1987_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1987_final,sum)

library(tidyr)

deces.1987_spread <- deces.1987_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1987_spread$`0-9`[is.na(deces.1987_spread$`0-9`)]<-0
deces.1987_spread$`10-19`[is.na(deces.1987_spread$`10-19`)]<-0
deces.1987_spread$`20-39`[is.na(deces.1987_spread$`20-39`)]<-0
deces.1987_spread$`40-59`[is.na(deces.1987_spread$`40-59`)]<-0
deces.1987_spread$`60-64`[is.na(deces.1987_spread$`60-64`)]<-0
deces.1987_spread$`65-69`[is.na(deces.1987_spread$`65-69`)]<-0
deces.1987_spread$`70-74`[is.na(deces.1987_spread$`70-74`)]<-0
deces.1987_spread$`75-79`[is.na(deces.1987_spread$`75-79`)]<-0
deces.1987_spread$`80+`[is.na(deces.1987_spread$`80+`)]<-0



#deces.1987_spread<-left_join(deces.1987_spread, deces.1987_spread)



deces.1987_spread$year<-1987


#fwrite(deces.1987_spread,"/fichier deces insee/décès travaillé/deces.1987_age_sexe.csv")




#################### partie 2 ######


RP_1987_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1987_age_sexe_final_2")

RP_1987_age_sexe_final_2<-RP_1987_age_sexe_final_2[,c(2:15)]

names(RP_1987_age_sexe_final_2)[names(RP_1987_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1987_age_sexe_final_2<-filter(RP_1987_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1987_mortalite<-left_join(deces.1987_spread,RP_1987_age_sexe_final_2)


Base_1987_mortalite$`75+`<-Base_1987_mortalite$`75-79`+Base_1987_mortalite$`80+`
Base_1987_mortalite$value_estimated_sum_75_plus_h_f<-Base_1987_mortalite$value_estimated_sum_75_79_h_f+Base_1987_mortalite$value_estimated_sum_80_plus_h_f


#Base_1987_mortalite$taux_mortalite_homme<-Base_1987_mortalite$Homme/Base_1987_mortalite$value_estimated_sum_homme

#Base_1987_mortalite$taux_mortalite_femme<-Base_1987_mortalite$Femme/Base_1987_mortalite$value_estimated_sum_femme

Base_1987_mortalite$taux_mortalite_0_9<-Base_1987_mortalite$`0-9`/Base_1987_mortalite$value_estimated_sum_0_9_h_f

Base_1987_mortalite$taux_mortalite_10_19<-Base_1987_mortalite$`10-19`/Base_1987_mortalite$value_estimated_sum_10_19_h_f

Base_1987_mortalite$taux_mortalite_20_39<-Base_1987_mortalite$`20-39`/Base_1987_mortalite$value_estimated_sum_20_39_h_f

Base_1987_mortalite$taux_mortalite_40_59<-Base_1987_mortalite$`40-59`/Base_1987_mortalite$value_estimated_sum_40_59_h_f

Base_1987_mortalite$taux_mortalite_60_64<-Base_1987_mortalite$`60-64`/Base_1987_mortalite$value_estimated_sum_60_64_h_f

Base_1987_mortalite$taux_mortalite_65_69<-Base_1987_mortalite$`65-69`/Base_1987_mortalite$value_estimated_sum_65_69_h_f

Base_1987_mortalite$taux_mortalite_70_74<-Base_1987_mortalite$`70-74`/Base_1987_mortalite$value_estimated_sum_70_74_h_f

Base_1987_mortalite$taux_mortalite_75_79<-Base_1987_mortalite$`75-79`/Base_1987_mortalite$value_estimated_sum_75_79_h_f

Base_1987_mortalite$taux_mortalite_75_plus<-Base_1987_mortalite$`75+`/Base_1987_mortalite$value_estimated_sum_75_plus_h_f

Base_1987_mortalite$taux_mortalite_80_plus<-Base_1987_mortalite$`80+`/Base_1987_mortalite$value_estimated_sum_80_plus_h_f

#Base_1987_mortalite$mort_total<-Base_1987_mortalite$Femme+Base_1987_mortalite$Homme

Base_1987_mortalite$mort_total<-Base_1987_mortalite$`0-9`+Base_1987_mortalite$`10-19`+Base_1987_mortalite$`20-39` +Base_1987_mortalite$`40-59` +Base_1987_mortalite$`60-64` +Base_1987_mortalite$`65-69` +Base_1987_mortalite$`70-74` +Base_1987_mortalite$`75+`



Base_1987_mortalite$taux_mortalite_total<-Base_1987_mortalite$mort_total/Base_1987_mortalite$value_estimated_population


Base_1987_mortalite<-filter(Base_1987_mortalite, Base_1987_mortalite$value_estimated_population>0)


Base_1987_mortalite<-Base_1987_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1987_mortalite<-filter(Base_1987_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1987_mortalite,"/heatwave and mortality code and data/new data/Base_1987_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1988 <- read_delim("fichier deces insee/deces-1988.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1988<-left_join(deces.1988, table_passage_bis)


deces.1988$COM<-ifelse(!is.na(deces.1988$COM_AP),deces.1988$COM_AP,deces.1988$lieudeces)

deces.1988$datedeces <- as.character(deces.1988$datedeces)
deces.1988$datedeces <- as.Date(deces.1988$datedeces, format = "%Y%m%d")


deces.1988$datenaiss <- as.character(deces.1988$datenaiss)
deces.1988$datenaiss <- as.Date(deces.1988$datenaiss, format = "%Y%m%d")


deces.1988$year <- as.numeric(format(deces.1988$datedeces, "%Y"))
deces.1988<-filter(deces.1988, deces.1988$year==1988)
deces.1988<-deces.1988[,-12]

library(lubridate)
deces.1988$age <- as.period(interval(deces.1988$datenaiss , deces.1988$datedeces ))
deces.1988$age_years <- year(deces.1988$age)

library(readr)


deces.1988$sexe<-as.character(deces.1988$sexe)

deces.1988$SEX<-ifelse(deces.1988$sexe=="1","Homme","Femme")

deces.1988_final<-deces.1988[,c("COM","datedeces","age_years","SEX")]

deces.1988_final$nbr_mort<-1


deces.1988_final$month <- as.numeric(format(deces.1988_final$datedeces, "%m"))




deces.1988_final$tranche_age <- cut(deces.1988_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1988_final$month <- as.numeric(format(deces.1988_final$datedeces, "%m"))


deces.1988_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1988_final,sum)

library(tidyr)

deces.1988_spread <- deces.1988_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1988_spread$`0-9`[is.na(deces.1988_spread$`0-9`)]<-0
deces.1988_spread$`10-19`[is.na(deces.1988_spread$`10-19`)]<-0
deces.1988_spread$`20-39`[is.na(deces.1988_spread$`20-39`)]<-0
deces.1988_spread$`40-59`[is.na(deces.1988_spread$`40-59`)]<-0
deces.1988_spread$`60-64`[is.na(deces.1988_spread$`60-64`)]<-0
deces.1988_spread$`65-69`[is.na(deces.1988_spread$`65-69`)]<-0
deces.1988_spread$`70-74`[is.na(deces.1988_spread$`70-74`)]<-0
deces.1988_spread$`75-79`[is.na(deces.1988_spread$`75-79`)]<-0
deces.1988_spread$`80+`[is.na(deces.1988_spread$`80+`)]<-0



#deces.1988_spread<-left_join(deces.1988_spread, deces.1988_spread)



deces.1988_spread$year<-1988


#fwrite(deces.1988_spread,"/fichier deces insee/décès travaillé/deces.1988_age_sexe.csv")




#################### partie 2 ######


RP_1988_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1988_age_sexe_final_2")

RP_1988_age_sexe_final_2<-RP_1988_age_sexe_final_2[,c(2:15)]

names(RP_1988_age_sexe_final_2)[names(RP_1988_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1988_age_sexe_final_2<-filter(RP_1988_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1988_mortalite<-left_join(deces.1988_spread,RP_1988_age_sexe_final_2)


Base_1988_mortalite$`75+`<-Base_1988_mortalite$`75-79`+Base_1988_mortalite$`80+`
Base_1988_mortalite$value_estimated_sum_75_plus_h_f<-Base_1988_mortalite$value_estimated_sum_75_79_h_f+Base_1988_mortalite$value_estimated_sum_80_plus_h_f


#Base_1988_mortalite$taux_mortalite_homme<-Base_1988_mortalite$Homme/Base_1988_mortalite$value_estimated_sum_homme

#Base_1988_mortalite$taux_mortalite_femme<-Base_1988_mortalite$Femme/Base_1988_mortalite$value_estimated_sum_femme

Base_1988_mortalite$taux_mortalite_0_9<-Base_1988_mortalite$`0-9`/Base_1988_mortalite$value_estimated_sum_0_9_h_f

Base_1988_mortalite$taux_mortalite_10_19<-Base_1988_mortalite$`10-19`/Base_1988_mortalite$value_estimated_sum_10_19_h_f

Base_1988_mortalite$taux_mortalite_20_39<-Base_1988_mortalite$`20-39`/Base_1988_mortalite$value_estimated_sum_20_39_h_f

Base_1988_mortalite$taux_mortalite_40_59<-Base_1988_mortalite$`40-59`/Base_1988_mortalite$value_estimated_sum_40_59_h_f

Base_1988_mortalite$taux_mortalite_60_64<-Base_1988_mortalite$`60-64`/Base_1988_mortalite$value_estimated_sum_60_64_h_f

Base_1988_mortalite$taux_mortalite_65_69<-Base_1988_mortalite$`65-69`/Base_1988_mortalite$value_estimated_sum_65_69_h_f

Base_1988_mortalite$taux_mortalite_70_74<-Base_1988_mortalite$`70-74`/Base_1988_mortalite$value_estimated_sum_70_74_h_f

Base_1988_mortalite$taux_mortalite_75_79<-Base_1988_mortalite$`75-79`/Base_1988_mortalite$value_estimated_sum_75_79_h_f

Base_1988_mortalite$taux_mortalite_75_plus<-Base_1988_mortalite$`75+`/Base_1988_mortalite$value_estimated_sum_75_plus_h_f

Base_1988_mortalite$taux_mortalite_80_plus<-Base_1988_mortalite$`80+`/Base_1988_mortalite$value_estimated_sum_80_plus_h_f

#Base_1988_mortalite$mort_total<-Base_1988_mortalite$Femme+Base_1988_mortalite$Homme

Base_1988_mortalite$mort_total<-Base_1988_mortalite$`0-9`+Base_1988_mortalite$`10-19`+Base_1988_mortalite$`20-39` +Base_1988_mortalite$`40-59` +Base_1988_mortalite$`60-64` +Base_1988_mortalite$`65-69` +Base_1988_mortalite$`70-74` +Base_1988_mortalite$`75+`



Base_1988_mortalite$taux_mortalite_total<-Base_1988_mortalite$mort_total/Base_1988_mortalite$value_estimated_population


Base_1988_mortalite<-filter(Base_1988_mortalite, Base_1988_mortalite$value_estimated_population>0)


Base_1988_mortalite<-Base_1988_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1988_mortalite<-filter(Base_1988_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1988_mortalite,"/heatwave and mortality code and data/new data/Base_1988_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1989 <- read_delim("fichier deces insee/deces-1989.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1989<-left_join(deces.1989, table_passage_bis)


deces.1989$COM<-ifelse(!is.na(deces.1989$COM_AP),deces.1989$COM_AP,deces.1989$lieudeces)

deces.1989$datedeces <- as.character(deces.1989$datedeces)
deces.1989$datedeces <- as.Date(deces.1989$datedeces, format = "%Y%m%d")


deces.1989$datenaiss <- as.character(deces.1989$datenaiss)
deces.1989$datenaiss <- as.Date(deces.1989$datenaiss, format = "%Y%m%d")


deces.1989$year <- as.numeric(format(deces.1989$datedeces, "%Y"))
deces.1989<-filter(deces.1989, deces.1989$year==1989)
deces.1989<-deces.1989[,-12]

library(lubridate)
deces.1989$age <- as.period(interval(deces.1989$datenaiss , deces.1989$datedeces ))
deces.1989$age_years <- year(deces.1989$age)

library(readr)


deces.1989$sexe<-as.character(deces.1989$sexe)

deces.1989$SEX<-ifelse(deces.1989$sexe=="1","Homme","Femme")

deces.1989_final<-deces.1989[,c("COM","datedeces","age_years","SEX")]

deces.1989_final$nbr_mort<-1


deces.1989_final$month <- as.numeric(format(deces.1989_final$datedeces, "%m"))




deces.1989_final$tranche_age <- cut(deces.1989_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1989_final$month <- as.numeric(format(deces.1989_final$datedeces, "%m"))


deces.1989_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1989_final,sum)

library(tidyr)

deces.1989_spread <- deces.1989_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1989_spread$`0-9`[is.na(deces.1989_spread$`0-9`)]<-0
deces.1989_spread$`10-19`[is.na(deces.1989_spread$`10-19`)]<-0
deces.1989_spread$`20-39`[is.na(deces.1989_spread$`20-39`)]<-0
deces.1989_spread$`40-59`[is.na(deces.1989_spread$`40-59`)]<-0
deces.1989_spread$`60-64`[is.na(deces.1989_spread$`60-64`)]<-0
deces.1989_spread$`65-69`[is.na(deces.1989_spread$`65-69`)]<-0
deces.1989_spread$`70-74`[is.na(deces.1989_spread$`70-74`)]<-0
deces.1989_spread$`75-79`[is.na(deces.1989_spread$`75-79`)]<-0
deces.1989_spread$`80+`[is.na(deces.1989_spread$`80+`)]<-0



#deces.1989_spread<-left_join(deces.1989_spread, deces.1989_spread)



deces.1989_spread$year<-1989


#fwrite(deces.1989_spread,"/fichier deces insee/décès travaillé/deces.1989_age_sexe.csv")




#################### partie 2 ######


RP_1989_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1989_age_sexe_final_2")

RP_1989_age_sexe_final_2<-RP_1989_age_sexe_final_2[,c(2:15)]

names(RP_1989_age_sexe_final_2)[names(RP_1989_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1989_age_sexe_final_2<-filter(RP_1989_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1989_mortalite<-left_join(deces.1989_spread,RP_1989_age_sexe_final_2)


Base_1989_mortalite$`75+`<-Base_1989_mortalite$`75-79`+Base_1989_mortalite$`80+`
Base_1989_mortalite$value_estimated_sum_75_plus_h_f<-Base_1989_mortalite$value_estimated_sum_75_79_h_f+Base_1989_mortalite$value_estimated_sum_80_plus_h_f


#Base_1989_mortalite$taux_mortalite_homme<-Base_1989_mortalite$Homme/Base_1989_mortalite$value_estimated_sum_homme

#Base_1989_mortalite$taux_mortalite_femme<-Base_1989_mortalite$Femme/Base_1989_mortalite$value_estimated_sum_femme

Base_1989_mortalite$taux_mortalite_0_9<-Base_1989_mortalite$`0-9`/Base_1989_mortalite$value_estimated_sum_0_9_h_f

Base_1989_mortalite$taux_mortalite_10_19<-Base_1989_mortalite$`10-19`/Base_1989_mortalite$value_estimated_sum_10_19_h_f

Base_1989_mortalite$taux_mortalite_20_39<-Base_1989_mortalite$`20-39`/Base_1989_mortalite$value_estimated_sum_20_39_h_f

Base_1989_mortalite$taux_mortalite_40_59<-Base_1989_mortalite$`40-59`/Base_1989_mortalite$value_estimated_sum_40_59_h_f

Base_1989_mortalite$taux_mortalite_60_64<-Base_1989_mortalite$`60-64`/Base_1989_mortalite$value_estimated_sum_60_64_h_f

Base_1989_mortalite$taux_mortalite_65_69<-Base_1989_mortalite$`65-69`/Base_1989_mortalite$value_estimated_sum_65_69_h_f

Base_1989_mortalite$taux_mortalite_70_74<-Base_1989_mortalite$`70-74`/Base_1989_mortalite$value_estimated_sum_70_74_h_f

Base_1989_mortalite$taux_mortalite_75_79<-Base_1989_mortalite$`75-79`/Base_1989_mortalite$value_estimated_sum_75_79_h_f

Base_1989_mortalite$taux_mortalite_75_plus<-Base_1989_mortalite$`75+`/Base_1989_mortalite$value_estimated_sum_75_plus_h_f

Base_1989_mortalite$taux_mortalite_80_plus<-Base_1989_mortalite$`80+`/Base_1989_mortalite$value_estimated_sum_80_plus_h_f

#Base_1989_mortalite$mort_total<-Base_1989_mortalite$Femme+Base_1989_mortalite$Homme

Base_1989_mortalite$mort_total<-Base_1989_mortalite$`0-9`+Base_1989_mortalite$`10-19`+Base_1989_mortalite$`20-39` +Base_1989_mortalite$`40-59` +Base_1989_mortalite$`60-64` +Base_1989_mortalite$`65-69` +Base_1989_mortalite$`70-74` +Base_1989_mortalite$`75+`



Base_1989_mortalite$taux_mortalite_total<-Base_1989_mortalite$mort_total/Base_1989_mortalite$value_estimated_population


Base_1989_mortalite<-filter(Base_1989_mortalite, Base_1989_mortalite$value_estimated_population>0)


Base_1989_mortalite<-Base_1989_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1989_mortalite<-filter(Base_1989_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1989_mortalite,"/heatwave and mortality code and data/new data/Base_1989_mortalite.csv")




rm(list = ls())
gc()


#################################### 



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1990 <- read_delim("fichier deces insee/deces-1990.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1990<-left_join(deces.1990, table_passage_bis)


deces.1990$COM<-ifelse(!is.na(deces.1990$COM_AP),deces.1990$COM_AP,deces.1990$lieudeces)

deces.1990$datedeces <- as.character(deces.1990$datedeces)
deces.1990$datedeces <- as.Date(deces.1990$datedeces, format = "%Y%m%d")


deces.1990$datenaiss <- as.character(deces.1990$datenaiss)
deces.1990$datenaiss <- as.Date(deces.1990$datenaiss, format = "%Y%m%d")


deces.1990$year <- as.numeric(format(deces.1990$datedeces, "%Y"))
deces.1990<-filter(deces.1990, deces.1990$year==1990)
deces.1990<-deces.1990[,-12]

library(lubridate)
deces.1990$age <- as.period(interval(deces.1990$datenaiss , deces.1990$datedeces ))
deces.1990$age_years <- year(deces.1990$age)

library(readr)


deces.1990$sexe<-as.character(deces.1990$sexe)

deces.1990$SEX<-ifelse(deces.1990$sexe=="1","Homme","Femme")

deces.1990_final<-deces.1990[,c("COM","datedeces","age_years","SEX")]

deces.1990_final$nbr_mort<-1


deces.1990_final$month <- as.numeric(format(deces.1990_final$datedeces, "%m"))




deces.1990_final$tranche_age <- cut(deces.1990_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1990_final$month <- as.numeric(format(deces.1990_final$datedeces, "%m"))


deces.1990_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1990_final,sum)

library(tidyr)

deces.1990_spread <- deces.1990_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1990_spread$`0-9`[is.na(deces.1990_spread$`0-9`)]<-0
deces.1990_spread$`10-19`[is.na(deces.1990_spread$`10-19`)]<-0
deces.1990_spread$`20-39`[is.na(deces.1990_spread$`20-39`)]<-0
deces.1990_spread$`40-59`[is.na(deces.1990_spread$`40-59`)]<-0
deces.1990_spread$`60-64`[is.na(deces.1990_spread$`60-64`)]<-0
deces.1990_spread$`65-69`[is.na(deces.1990_spread$`65-69`)]<-0
deces.1990_spread$`70-74`[is.na(deces.1990_spread$`70-74`)]<-0
deces.1990_spread$`75-79`[is.na(deces.1990_spread$`75-79`)]<-0
deces.1990_spread$`80+`[is.na(deces.1990_spread$`80+`)]<-0



#deces.1990_spread<-left_join(deces.1990_spread, deces.1990_spread)



deces.1990_spread$year<-1990


#fwrite(deces.1990_spread,"/fichier deces insee/décès travaillé/deces.1990_age_sexe.csv")




#################### partie 2 ######


RP_1990_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1990_age_sexe_final_2")

RP_1990_age_sexe_final_2<-RP_1990_age_sexe_final_2[,c(2:15)]

names(RP_1990_age_sexe_final_2)[names(RP_1990_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1990_age_sexe_final_2<-filter(RP_1990_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1990_mortalite<-left_join(deces.1990_spread,RP_1990_age_sexe_final_2)


Base_1990_mortalite$`75+`<-Base_1990_mortalite$`75-79`+Base_1990_mortalite$`80+`
Base_1990_mortalite$value_estimated_sum_75_plus_h_f<-Base_1990_mortalite$value_estimated_sum_75_79_h_f+Base_1990_mortalite$value_estimated_sum_80_plus_h_f


#Base_1990_mortalite$taux_mortalite_homme<-Base_1990_mortalite$Homme/Base_1990_mortalite$value_estimated_sum_homme

#Base_1990_mortalite$taux_mortalite_femme<-Base_1990_mortalite$Femme/Base_1990_mortalite$value_estimated_sum_femme

Base_1990_mortalite$taux_mortalite_0_9<-Base_1990_mortalite$`0-9`/Base_1990_mortalite$value_estimated_sum_0_9_h_f

Base_1990_mortalite$taux_mortalite_10_19<-Base_1990_mortalite$`10-19`/Base_1990_mortalite$value_estimated_sum_10_19_h_f

Base_1990_mortalite$taux_mortalite_20_39<-Base_1990_mortalite$`20-39`/Base_1990_mortalite$value_estimated_sum_20_39_h_f

Base_1990_mortalite$taux_mortalite_40_59<-Base_1990_mortalite$`40-59`/Base_1990_mortalite$value_estimated_sum_40_59_h_f

Base_1990_mortalite$taux_mortalite_60_64<-Base_1990_mortalite$`60-64`/Base_1990_mortalite$value_estimated_sum_60_64_h_f

Base_1990_mortalite$taux_mortalite_65_69<-Base_1990_mortalite$`65-69`/Base_1990_mortalite$value_estimated_sum_65_69_h_f

Base_1990_mortalite$taux_mortalite_70_74<-Base_1990_mortalite$`70-74`/Base_1990_mortalite$value_estimated_sum_70_74_h_f

Base_1990_mortalite$taux_mortalite_75_79<-Base_1990_mortalite$`75-79`/Base_1990_mortalite$value_estimated_sum_75_79_h_f

Base_1990_mortalite$taux_mortalite_75_plus<-Base_1990_mortalite$`75+`/Base_1990_mortalite$value_estimated_sum_75_plus_h_f

Base_1990_mortalite$taux_mortalite_80_plus<-Base_1990_mortalite$`80+`/Base_1990_mortalite$value_estimated_sum_80_plus_h_f

#Base_1990_mortalite$mort_total<-Base_1990_mortalite$Femme+Base_1990_mortalite$Homme

Base_1990_mortalite$mort_total<-Base_1990_mortalite$`0-9`+Base_1990_mortalite$`10-19`+Base_1990_mortalite$`20-39` +Base_1990_mortalite$`40-59` +Base_1990_mortalite$`60-64` +Base_1990_mortalite$`65-69` +Base_1990_mortalite$`70-74` +Base_1990_mortalite$`75+`



Base_1990_mortalite$taux_mortalite_total<-Base_1990_mortalite$mort_total/Base_1990_mortalite$value_estimated_population


Base_1990_mortalite<-filter(Base_1990_mortalite, Base_1990_mortalite$value_estimated_population>0)


Base_1990_mortalite<-Base_1990_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1990_mortalite<-filter(Base_1990_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1990_mortalite,"/heatwave and mortality code and data/new data/Base_1990_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1991 <- read_delim("fichier deces insee/deces-1991.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1991<-left_join(deces.1991, table_passage_bis)


deces.1991$COM<-ifelse(!is.na(deces.1991$COM_AP),deces.1991$COM_AP,deces.1991$lieudeces)

deces.1991$datedeces <- as.character(deces.1991$datedeces)
deces.1991$datedeces <- as.Date(deces.1991$datedeces, format = "%Y%m%d")


deces.1991$datenaiss <- as.character(deces.1991$datenaiss)
deces.1991$datenaiss <- as.Date(deces.1991$datenaiss, format = "%Y%m%d")


deces.1991$year <- as.numeric(format(deces.1991$datedeces, "%Y"))
deces.1991<-filter(deces.1991, deces.1991$year==1991)
deces.1991<-deces.1991[,-12]

library(lubridate)
deces.1991$age <- as.period(interval(deces.1991$datenaiss , deces.1991$datedeces ))
deces.1991$age_years <- year(deces.1991$age)

library(readr)


deces.1991$sexe<-as.character(deces.1991$sexe)

deces.1991$SEX<-ifelse(deces.1991$sexe=="1","Homme","Femme")

deces.1991_final<-deces.1991[,c("COM","datedeces","age_years","SEX")]

deces.1991_final$nbr_mort<-1


deces.1991_final$month <- as.numeric(format(deces.1991_final$datedeces, "%m"))




deces.1991_final$tranche_age <- cut(deces.1991_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1991_final$month <- as.numeric(format(deces.1991_final$datedeces, "%m"))


deces.1991_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1991_final,sum)

library(tidyr)

deces.1991_spread <- deces.1991_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1991_spread$`0-9`[is.na(deces.1991_spread$`0-9`)]<-0
deces.1991_spread$`10-19`[is.na(deces.1991_spread$`10-19`)]<-0
deces.1991_spread$`20-39`[is.na(deces.1991_spread$`20-39`)]<-0
deces.1991_spread$`40-59`[is.na(deces.1991_spread$`40-59`)]<-0
deces.1991_spread$`60-64`[is.na(deces.1991_spread$`60-64`)]<-0
deces.1991_spread$`65-69`[is.na(deces.1991_spread$`65-69`)]<-0
deces.1991_spread$`70-74`[is.na(deces.1991_spread$`70-74`)]<-0
deces.1991_spread$`75-79`[is.na(deces.1991_spread$`75-79`)]<-0
deces.1991_spread$`80+`[is.na(deces.1991_spread$`80+`)]<-0



#deces.1991_spread<-left_join(deces.1991_spread, deces.1991_spread)



deces.1991_spread$year<-1991


#fwrite(deces.1991_spread,"/fichier deces insee/décès travaillé/deces.1991_age_sexe.csv")




#################### partie 2 ######


RP_1991_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1991_age_sexe_final_2")

RP_1991_age_sexe_final_2<-RP_1991_age_sexe_final_2[,c(2:15)]

names(RP_1991_age_sexe_final_2)[names(RP_1991_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1991_age_sexe_final_2<-filter(RP_1991_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1991_mortalite<-left_join(deces.1991_spread,RP_1991_age_sexe_final_2)


Base_1991_mortalite$`75+`<-Base_1991_mortalite$`75-79`+Base_1991_mortalite$`80+`
Base_1991_mortalite$value_estimated_sum_75_plus_h_f<-Base_1991_mortalite$value_estimated_sum_75_79_h_f+Base_1991_mortalite$value_estimated_sum_80_plus_h_f


#Base_1991_mortalite$taux_mortalite_homme<-Base_1991_mortalite$Homme/Base_1991_mortalite$value_estimated_sum_homme

#Base_1991_mortalite$taux_mortalite_femme<-Base_1991_mortalite$Femme/Base_1991_mortalite$value_estimated_sum_femme

Base_1991_mortalite$taux_mortalite_0_9<-Base_1991_mortalite$`0-9`/Base_1991_mortalite$value_estimated_sum_0_9_h_f

Base_1991_mortalite$taux_mortalite_10_19<-Base_1991_mortalite$`10-19`/Base_1991_mortalite$value_estimated_sum_10_19_h_f

Base_1991_mortalite$taux_mortalite_20_39<-Base_1991_mortalite$`20-39`/Base_1991_mortalite$value_estimated_sum_20_39_h_f

Base_1991_mortalite$taux_mortalite_40_59<-Base_1991_mortalite$`40-59`/Base_1991_mortalite$value_estimated_sum_40_59_h_f

Base_1991_mortalite$taux_mortalite_60_64<-Base_1991_mortalite$`60-64`/Base_1991_mortalite$value_estimated_sum_60_64_h_f

Base_1991_mortalite$taux_mortalite_65_69<-Base_1991_mortalite$`65-69`/Base_1991_mortalite$value_estimated_sum_65_69_h_f

Base_1991_mortalite$taux_mortalite_70_74<-Base_1991_mortalite$`70-74`/Base_1991_mortalite$value_estimated_sum_70_74_h_f

Base_1991_mortalite$taux_mortalite_75_79<-Base_1991_mortalite$`75-79`/Base_1991_mortalite$value_estimated_sum_75_79_h_f

Base_1991_mortalite$taux_mortalite_75_plus<-Base_1991_mortalite$`75+`/Base_1991_mortalite$value_estimated_sum_75_plus_h_f

Base_1991_mortalite$taux_mortalite_80_plus<-Base_1991_mortalite$`80+`/Base_1991_mortalite$value_estimated_sum_80_plus_h_f

#Base_1991_mortalite$mort_total<-Base_1991_mortalite$Femme+Base_1991_mortalite$Homme

Base_1991_mortalite$mort_total<-Base_1991_mortalite$`0-9`+Base_1991_mortalite$`10-19`+Base_1991_mortalite$`20-39` +Base_1991_mortalite$`40-59` +Base_1991_mortalite$`60-64` +Base_1991_mortalite$`65-69` +Base_1991_mortalite$`70-74` +Base_1991_mortalite$`75+`



Base_1991_mortalite$taux_mortalite_total<-Base_1991_mortalite$mort_total/Base_1991_mortalite$value_estimated_population


Base_1991_mortalite<-filter(Base_1991_mortalite, Base_1991_mortalite$value_estimated_population>0)


Base_1991_mortalite<-Base_1991_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1991_mortalite<-filter(Base_1991_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1991_mortalite,"/heatwave and mortality code and data/new data/Base_1991_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1992 <- read_delim("fichier deces insee/deces-1992.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1992<-left_join(deces.1992, table_passage_bis)


deces.1992$COM<-ifelse(!is.na(deces.1992$COM_AP),deces.1992$COM_AP,deces.1992$lieudeces)

deces.1992$datedeces <- as.character(deces.1992$datedeces)
deces.1992$datedeces <- as.Date(deces.1992$datedeces, format = "%Y%m%d")


deces.1992$datenaiss <- as.character(deces.1992$datenaiss)
deces.1992$datenaiss <- as.Date(deces.1992$datenaiss, format = "%Y%m%d")


deces.1992$year <- as.numeric(format(deces.1992$datedeces, "%Y"))
deces.1992<-filter(deces.1992, deces.1992$year==1992)
deces.1992<-deces.1992[,-12]

library(lubridate)
deces.1992$age <- as.period(interval(deces.1992$datenaiss , deces.1992$datedeces ))
deces.1992$age_years <- year(deces.1992$age)

library(readr)


deces.1992$sexe<-as.character(deces.1992$sexe)

deces.1992$SEX<-ifelse(deces.1992$sexe=="1","Homme","Femme")

deces.1992_final<-deces.1992[,c("COM","datedeces","age_years","SEX")]

deces.1992_final$nbr_mort<-1


deces.1992_final$month <- as.numeric(format(deces.1992_final$datedeces, "%m"))




deces.1992_final$tranche_age <- cut(deces.1992_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1992_final$month <- as.numeric(format(deces.1992_final$datedeces, "%m"))


deces.1992_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1992_final,sum)

library(tidyr)

deces.1992_spread <- deces.1992_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1992_spread$`0-9`[is.na(deces.1992_spread$`0-9`)]<-0
deces.1992_spread$`10-19`[is.na(deces.1992_spread$`10-19`)]<-0
deces.1992_spread$`20-39`[is.na(deces.1992_spread$`20-39`)]<-0
deces.1992_spread$`40-59`[is.na(deces.1992_spread$`40-59`)]<-0
deces.1992_spread$`60-64`[is.na(deces.1992_spread$`60-64`)]<-0
deces.1992_spread$`65-69`[is.na(deces.1992_spread$`65-69`)]<-0
deces.1992_spread$`70-74`[is.na(deces.1992_spread$`70-74`)]<-0
deces.1992_spread$`75-79`[is.na(deces.1992_spread$`75-79`)]<-0
deces.1992_spread$`80+`[is.na(deces.1992_spread$`80+`)]<-0



#deces.1992_spread<-left_join(deces.1992_spread, deces.1992_spread)



deces.1992_spread$year<-1992


#fwrite(deces.1992_spread,"/fichier deces insee/décès travaillé/deces.1992_age_sexe.csv")




#################### partie 2 ######


RP_1992_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1992_age_sexe_final_2")

RP_1992_age_sexe_final_2<-RP_1992_age_sexe_final_2[,c(2:15)]

names(RP_1992_age_sexe_final_2)[names(RP_1992_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1992_age_sexe_final_2<-filter(RP_1992_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1992_mortalite<-left_join(deces.1992_spread,RP_1992_age_sexe_final_2)


Base_1992_mortalite$`75+`<-Base_1992_mortalite$`75-79`+Base_1992_mortalite$`80+`
Base_1992_mortalite$value_estimated_sum_75_plus_h_f<-Base_1992_mortalite$value_estimated_sum_75_79_h_f+Base_1992_mortalite$value_estimated_sum_80_plus_h_f


#Base_1992_mortalite$taux_mortalite_homme<-Base_1992_mortalite$Homme/Base_1992_mortalite$value_estimated_sum_homme

#Base_1992_mortalite$taux_mortalite_femme<-Base_1992_mortalite$Femme/Base_1992_mortalite$value_estimated_sum_femme

Base_1992_mortalite$taux_mortalite_0_9<-Base_1992_mortalite$`0-9`/Base_1992_mortalite$value_estimated_sum_0_9_h_f

Base_1992_mortalite$taux_mortalite_10_19<-Base_1992_mortalite$`10-19`/Base_1992_mortalite$value_estimated_sum_10_19_h_f

Base_1992_mortalite$taux_mortalite_20_39<-Base_1992_mortalite$`20-39`/Base_1992_mortalite$value_estimated_sum_20_39_h_f

Base_1992_mortalite$taux_mortalite_40_59<-Base_1992_mortalite$`40-59`/Base_1992_mortalite$value_estimated_sum_40_59_h_f

Base_1992_mortalite$taux_mortalite_60_64<-Base_1992_mortalite$`60-64`/Base_1992_mortalite$value_estimated_sum_60_64_h_f

Base_1992_mortalite$taux_mortalite_65_69<-Base_1992_mortalite$`65-69`/Base_1992_mortalite$value_estimated_sum_65_69_h_f

Base_1992_mortalite$taux_mortalite_70_74<-Base_1992_mortalite$`70-74`/Base_1992_mortalite$value_estimated_sum_70_74_h_f

Base_1992_mortalite$taux_mortalite_75_79<-Base_1992_mortalite$`75-79`/Base_1992_mortalite$value_estimated_sum_75_79_h_f

Base_1992_mortalite$taux_mortalite_75_plus<-Base_1992_mortalite$`75+`/Base_1992_mortalite$value_estimated_sum_75_plus_h_f

Base_1992_mortalite$taux_mortalite_80_plus<-Base_1992_mortalite$`80+`/Base_1992_mortalite$value_estimated_sum_80_plus_h_f

#Base_1992_mortalite$mort_total<-Base_1992_mortalite$Femme+Base_1992_mortalite$Homme

Base_1992_mortalite$mort_total<-Base_1992_mortalite$`0-9`+Base_1992_mortalite$`10-19`+Base_1992_mortalite$`20-39` +Base_1992_mortalite$`40-59` +Base_1992_mortalite$`60-64` +Base_1992_mortalite$`65-69` +Base_1992_mortalite$`70-74` +Base_1992_mortalite$`75+`



Base_1992_mortalite$taux_mortalite_total<-Base_1992_mortalite$mort_total/Base_1992_mortalite$value_estimated_population


Base_1992_mortalite<-filter(Base_1992_mortalite, Base_1992_mortalite$value_estimated_population>0)


Base_1992_mortalite<-Base_1992_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1992_mortalite<-filter(Base_1992_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1992_mortalite,"/heatwave and mortality code and data/new data/Base_1992_mortalite.csv")




rm(list = ls())
gc()


#################################### 



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1993 <- read_delim("fichier deces insee/deces-1993.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1993<-left_join(deces.1993, table_passage_bis)


deces.1993$COM<-ifelse(!is.na(deces.1993$COM_AP),deces.1993$COM_AP,deces.1993$lieudeces)

deces.1993$datedeces <- as.character(deces.1993$datedeces)
deces.1993$datedeces <- as.Date(deces.1993$datedeces, format = "%Y%m%d")


deces.1993$datenaiss <- as.character(deces.1993$datenaiss)
deces.1993$datenaiss <- as.Date(deces.1993$datenaiss, format = "%Y%m%d")


deces.1993$year <- as.numeric(format(deces.1993$datedeces, "%Y"))
deces.1993<-filter(deces.1993, deces.1993$year==1993)
deces.1993<-deces.1993[,-12]

library(lubridate)
deces.1993$age <- as.period(interval(deces.1993$datenaiss , deces.1993$datedeces ))
deces.1993$age_years <- year(deces.1993$age)

library(readr)


deces.1993$sexe<-as.character(deces.1993$sexe)

deces.1993$SEX<-ifelse(deces.1993$sexe=="1","Homme","Femme")

deces.1993_final<-deces.1993[,c("COM","datedeces","age_years","SEX")]

deces.1993_final$nbr_mort<-1


deces.1993_final$month <- as.numeric(format(deces.1993_final$datedeces, "%m"))




deces.1993_final$tranche_age <- cut(deces.1993_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1993_final$month <- as.numeric(format(deces.1993_final$datedeces, "%m"))


deces.1993_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1993_final,sum)

library(tidyr)

deces.1993_spread <- deces.1993_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1993_spread$`0-9`[is.na(deces.1993_spread$`0-9`)]<-0
deces.1993_spread$`10-19`[is.na(deces.1993_spread$`10-19`)]<-0
deces.1993_spread$`20-39`[is.na(deces.1993_spread$`20-39`)]<-0
deces.1993_spread$`40-59`[is.na(deces.1993_spread$`40-59`)]<-0
deces.1993_spread$`60-64`[is.na(deces.1993_spread$`60-64`)]<-0
deces.1993_spread$`65-69`[is.na(deces.1993_spread$`65-69`)]<-0
deces.1993_spread$`70-74`[is.na(deces.1993_spread$`70-74`)]<-0
deces.1993_spread$`75-79`[is.na(deces.1993_spread$`75-79`)]<-0
deces.1993_spread$`80+`[is.na(deces.1993_spread$`80+`)]<-0



#deces.1993_spread<-left_join(deces.1993_spread, deces.1993_spread)



deces.1993_spread$year<-1993


#fwrite(deces.1993_spread,"/fichier deces insee/décès travaillé/deces.1993_age_sexe.csv")




#################### partie 2 ######


RP_1993_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1993_age_sexe_final_2")

RP_1993_age_sexe_final_2<-RP_1993_age_sexe_final_2[,c(2:15)]

names(RP_1993_age_sexe_final_2)[names(RP_1993_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1993_age_sexe_final_2<-filter(RP_1993_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1993_mortalite<-left_join(deces.1993_spread,RP_1993_age_sexe_final_2)


Base_1993_mortalite$`75+`<-Base_1993_mortalite$`75-79`+Base_1993_mortalite$`80+`
Base_1993_mortalite$value_estimated_sum_75_plus_h_f<-Base_1993_mortalite$value_estimated_sum_75_79_h_f+Base_1993_mortalite$value_estimated_sum_80_plus_h_f


#Base_1993_mortalite$taux_mortalite_homme<-Base_1993_mortalite$Homme/Base_1993_mortalite$value_estimated_sum_homme

#Base_1993_mortalite$taux_mortalite_femme<-Base_1993_mortalite$Femme/Base_1993_mortalite$value_estimated_sum_femme

Base_1993_mortalite$taux_mortalite_0_9<-Base_1993_mortalite$`0-9`/Base_1993_mortalite$value_estimated_sum_0_9_h_f

Base_1993_mortalite$taux_mortalite_10_19<-Base_1993_mortalite$`10-19`/Base_1993_mortalite$value_estimated_sum_10_19_h_f

Base_1993_mortalite$taux_mortalite_20_39<-Base_1993_mortalite$`20-39`/Base_1993_mortalite$value_estimated_sum_20_39_h_f

Base_1993_mortalite$taux_mortalite_40_59<-Base_1993_mortalite$`40-59`/Base_1993_mortalite$value_estimated_sum_40_59_h_f

Base_1993_mortalite$taux_mortalite_60_64<-Base_1993_mortalite$`60-64`/Base_1993_mortalite$value_estimated_sum_60_64_h_f

Base_1993_mortalite$taux_mortalite_65_69<-Base_1993_mortalite$`65-69`/Base_1993_mortalite$value_estimated_sum_65_69_h_f

Base_1993_mortalite$taux_mortalite_70_74<-Base_1993_mortalite$`70-74`/Base_1993_mortalite$value_estimated_sum_70_74_h_f

Base_1993_mortalite$taux_mortalite_75_79<-Base_1993_mortalite$`75-79`/Base_1993_mortalite$value_estimated_sum_75_79_h_f

Base_1993_mortalite$taux_mortalite_75_plus<-Base_1993_mortalite$`75+`/Base_1993_mortalite$value_estimated_sum_75_plus_h_f

Base_1993_mortalite$taux_mortalite_80_plus<-Base_1993_mortalite$`80+`/Base_1993_mortalite$value_estimated_sum_80_plus_h_f

#Base_1993_mortalite$mort_total<-Base_1993_mortalite$Femme+Base_1993_mortalite$Homme

Base_1993_mortalite$mort_total<-Base_1993_mortalite$`0-9`+Base_1993_mortalite$`10-19`+Base_1993_mortalite$`20-39` +Base_1993_mortalite$`40-59` +Base_1993_mortalite$`60-64` +Base_1993_mortalite$`65-69` +Base_1993_mortalite$`70-74` +Base_1993_mortalite$`75+`



Base_1993_mortalite$taux_mortalite_total<-Base_1993_mortalite$mort_total/Base_1993_mortalite$value_estimated_population


Base_1993_mortalite<-filter(Base_1993_mortalite, Base_1993_mortalite$value_estimated_population>0)


Base_1993_mortalite<-Base_1993_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1993_mortalite<-filter(Base_1993_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1993_mortalite,"/heatwave and mortality code and data/new data/Base_1993_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1994 <- read_delim("fichier deces insee/deces-1994.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1994<-left_join(deces.1994, table_passage_bis)


deces.1994$COM<-ifelse(!is.na(deces.1994$COM_AP),deces.1994$COM_AP,deces.1994$lieudeces)

deces.1994$datedeces <- as.character(deces.1994$datedeces)
deces.1994$datedeces <- as.Date(deces.1994$datedeces, format = "%Y%m%d")


deces.1994$datenaiss <- as.character(deces.1994$datenaiss)
deces.1994$datenaiss <- as.Date(deces.1994$datenaiss, format = "%Y%m%d")


deces.1994$year <- as.numeric(format(deces.1994$datedeces, "%Y"))
deces.1994<-filter(deces.1994, deces.1994$year==1994)
deces.1994<-deces.1994[,-12]

library(lubridate)
deces.1994$age <- as.period(interval(deces.1994$datenaiss , deces.1994$datedeces ))
deces.1994$age_years <- year(deces.1994$age)

library(readr)


deces.1994$sexe<-as.character(deces.1994$sexe)

deces.1994$SEX<-ifelse(deces.1994$sexe=="1","Homme","Femme")

deces.1994_final<-deces.1994[,c("COM","datedeces","age_years","SEX")]

deces.1994_final$nbr_mort<-1


deces.1994_final$month <- as.numeric(format(deces.1994_final$datedeces, "%m"))




deces.1994_final$tranche_age <- cut(deces.1994_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1994_final$month <- as.numeric(format(deces.1994_final$datedeces, "%m"))


deces.1994_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1994_final,sum)

library(tidyr)

deces.1994_spread <- deces.1994_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1994_spread$`0-9`[is.na(deces.1994_spread$`0-9`)]<-0
deces.1994_spread$`10-19`[is.na(deces.1994_spread$`10-19`)]<-0
deces.1994_spread$`20-39`[is.na(deces.1994_spread$`20-39`)]<-0
deces.1994_spread$`40-59`[is.na(deces.1994_spread$`40-59`)]<-0
deces.1994_spread$`60-64`[is.na(deces.1994_spread$`60-64`)]<-0
deces.1994_spread$`65-69`[is.na(deces.1994_spread$`65-69`)]<-0
deces.1994_spread$`70-74`[is.na(deces.1994_spread$`70-74`)]<-0
deces.1994_spread$`75-79`[is.na(deces.1994_spread$`75-79`)]<-0
deces.1994_spread$`80+`[is.na(deces.1994_spread$`80+`)]<-0



#deces.1994_spread<-left_join(deces.1994_spread, deces.1994_spread)



deces.1994_spread$year<-1994


#fwrite(deces.1994_spread,"/fichier deces insee/décès travaillé/deces.1994_age_sexe.csv")




#################### partie 2 ######


RP_1994_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1994_age_sexe_final_2")

RP_1994_age_sexe_final_2<-RP_1994_age_sexe_final_2[,c(2:15)]

names(RP_1994_age_sexe_final_2)[names(RP_1994_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1994_age_sexe_final_2<-filter(RP_1994_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1994_mortalite<-left_join(deces.1994_spread,RP_1994_age_sexe_final_2)


Base_1994_mortalite$`75+`<-Base_1994_mortalite$`75-79`+Base_1994_mortalite$`80+`
Base_1994_mortalite$value_estimated_sum_75_plus_h_f<-Base_1994_mortalite$value_estimated_sum_75_79_h_f+Base_1994_mortalite$value_estimated_sum_80_plus_h_f


#Base_1994_mortalite$taux_mortalite_homme<-Base_1994_mortalite$Homme/Base_1994_mortalite$value_estimated_sum_homme

#Base_1994_mortalite$taux_mortalite_femme<-Base_1994_mortalite$Femme/Base_1994_mortalite$value_estimated_sum_femme

Base_1994_mortalite$taux_mortalite_0_9<-Base_1994_mortalite$`0-9`/Base_1994_mortalite$value_estimated_sum_0_9_h_f

Base_1994_mortalite$taux_mortalite_10_19<-Base_1994_mortalite$`10-19`/Base_1994_mortalite$value_estimated_sum_10_19_h_f

Base_1994_mortalite$taux_mortalite_20_39<-Base_1994_mortalite$`20-39`/Base_1994_mortalite$value_estimated_sum_20_39_h_f

Base_1994_mortalite$taux_mortalite_40_59<-Base_1994_mortalite$`40-59`/Base_1994_mortalite$value_estimated_sum_40_59_h_f

Base_1994_mortalite$taux_mortalite_60_64<-Base_1994_mortalite$`60-64`/Base_1994_mortalite$value_estimated_sum_60_64_h_f

Base_1994_mortalite$taux_mortalite_65_69<-Base_1994_mortalite$`65-69`/Base_1994_mortalite$value_estimated_sum_65_69_h_f

Base_1994_mortalite$taux_mortalite_70_74<-Base_1994_mortalite$`70-74`/Base_1994_mortalite$value_estimated_sum_70_74_h_f

Base_1994_mortalite$taux_mortalite_75_79<-Base_1994_mortalite$`75-79`/Base_1994_mortalite$value_estimated_sum_75_79_h_f

Base_1994_mortalite$taux_mortalite_75_plus<-Base_1994_mortalite$`75+`/Base_1994_mortalite$value_estimated_sum_75_plus_h_f

Base_1994_mortalite$taux_mortalite_80_plus<-Base_1994_mortalite$`80+`/Base_1994_mortalite$value_estimated_sum_80_plus_h_f

#Base_1994_mortalite$mort_total<-Base_1994_mortalite$Femme+Base_1994_mortalite$Homme

Base_1994_mortalite$mort_total<-Base_1994_mortalite$`0-9`+Base_1994_mortalite$`10-19`+Base_1994_mortalite$`20-39` +Base_1994_mortalite$`40-59` +Base_1994_mortalite$`60-64` +Base_1994_mortalite$`65-69` +Base_1994_mortalite$`70-74` +Base_1994_mortalite$`75+`



Base_1994_mortalite$taux_mortalite_total<-Base_1994_mortalite$mort_total/Base_1994_mortalite$value_estimated_population


Base_1994_mortalite<-filter(Base_1994_mortalite, Base_1994_mortalite$value_estimated_population>0)


Base_1994_mortalite<-Base_1994_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1994_mortalite<-filter(Base_1994_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1994_mortalite,"/heatwave and mortality code and data/new data/Base_1994_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1995 <- read_delim("fichier deces insee/deces-1995.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1995<-left_join(deces.1995, table_passage_bis)


deces.1995$COM<-ifelse(!is.na(deces.1995$COM_AP),deces.1995$COM_AP,deces.1995$lieudeces)

deces.1995$datedeces <- as.character(deces.1995$datedeces)
deces.1995$datedeces <- as.Date(deces.1995$datedeces, format = "%Y%m%d")


deces.1995$datenaiss <- as.character(deces.1995$datenaiss)
deces.1995$datenaiss <- as.Date(deces.1995$datenaiss, format = "%Y%m%d")


deces.1995$year <- as.numeric(format(deces.1995$datedeces, "%Y"))
deces.1995<-filter(deces.1995, deces.1995$year==1995)
deces.1995<-deces.1995[,-12]

library(lubridate)
deces.1995$age <- as.period(interval(deces.1995$datenaiss , deces.1995$datedeces ))
deces.1995$age_years <- year(deces.1995$age)

library(readr)


deces.1995$sexe<-as.character(deces.1995$sexe)

deces.1995$SEX<-ifelse(deces.1995$sexe=="1","Homme","Femme")

deces.1995_final<-deces.1995[,c("COM","datedeces","age_years","SEX")]

deces.1995_final$nbr_mort<-1


deces.1995_final$month <- as.numeric(format(deces.1995_final$datedeces, "%m"))




deces.1995_final$tranche_age <- cut(deces.1995_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1995_final$month <- as.numeric(format(deces.1995_final$datedeces, "%m"))


deces.1995_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1995_final,sum)

library(tidyr)

deces.1995_spread <- deces.1995_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1995_spread$`0-9`[is.na(deces.1995_spread$`0-9`)]<-0
deces.1995_spread$`10-19`[is.na(deces.1995_spread$`10-19`)]<-0
deces.1995_spread$`20-39`[is.na(deces.1995_spread$`20-39`)]<-0
deces.1995_spread$`40-59`[is.na(deces.1995_spread$`40-59`)]<-0
deces.1995_spread$`60-64`[is.na(deces.1995_spread$`60-64`)]<-0
deces.1995_spread$`65-69`[is.na(deces.1995_spread$`65-69`)]<-0
deces.1995_spread$`70-74`[is.na(deces.1995_spread$`70-74`)]<-0
deces.1995_spread$`75-79`[is.na(deces.1995_spread$`75-79`)]<-0
deces.1995_spread$`80+`[is.na(deces.1995_spread$`80+`)]<-0



#deces.1995_spread<-left_join(deces.1995_spread, deces.1995_spread)



deces.1995_spread$year<-1995


#fwrite(deces.1995_spread,"/fichier deces insee/décès travaillé/deces.1995_age_sexe.csv")




#################### partie 2 ######


RP_1995_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1995_age_sexe_final_2")

RP_1995_age_sexe_final_2<-RP_1995_age_sexe_final_2[,c(2:15)]

names(RP_1995_age_sexe_final_2)[names(RP_1995_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1995_age_sexe_final_2<-filter(RP_1995_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1995_mortalite<-left_join(deces.1995_spread,RP_1995_age_sexe_final_2)


Base_1995_mortalite$`75+`<-Base_1995_mortalite$`75-79`+Base_1995_mortalite$`80+`
Base_1995_mortalite$value_estimated_sum_75_plus_h_f<-Base_1995_mortalite$value_estimated_sum_75_79_h_f+Base_1995_mortalite$value_estimated_sum_80_plus_h_f


#Base_1995_mortalite$taux_mortalite_homme<-Base_1995_mortalite$Homme/Base_1995_mortalite$value_estimated_sum_homme

#Base_1995_mortalite$taux_mortalite_femme<-Base_1995_mortalite$Femme/Base_1995_mortalite$value_estimated_sum_femme

Base_1995_mortalite$taux_mortalite_0_9<-Base_1995_mortalite$`0-9`/Base_1995_mortalite$value_estimated_sum_0_9_h_f

Base_1995_mortalite$taux_mortalite_10_19<-Base_1995_mortalite$`10-19`/Base_1995_mortalite$value_estimated_sum_10_19_h_f

Base_1995_mortalite$taux_mortalite_20_39<-Base_1995_mortalite$`20-39`/Base_1995_mortalite$value_estimated_sum_20_39_h_f

Base_1995_mortalite$taux_mortalite_40_59<-Base_1995_mortalite$`40-59`/Base_1995_mortalite$value_estimated_sum_40_59_h_f

Base_1995_mortalite$taux_mortalite_60_64<-Base_1995_mortalite$`60-64`/Base_1995_mortalite$value_estimated_sum_60_64_h_f

Base_1995_mortalite$taux_mortalite_65_69<-Base_1995_mortalite$`65-69`/Base_1995_mortalite$value_estimated_sum_65_69_h_f

Base_1995_mortalite$taux_mortalite_70_74<-Base_1995_mortalite$`70-74`/Base_1995_mortalite$value_estimated_sum_70_74_h_f

Base_1995_mortalite$taux_mortalite_75_79<-Base_1995_mortalite$`75-79`/Base_1995_mortalite$value_estimated_sum_75_79_h_f

Base_1995_mortalite$taux_mortalite_75_plus<-Base_1995_mortalite$`75+`/Base_1995_mortalite$value_estimated_sum_75_plus_h_f

Base_1995_mortalite$taux_mortalite_80_plus<-Base_1995_mortalite$`80+`/Base_1995_mortalite$value_estimated_sum_80_plus_h_f

#Base_1995_mortalite$mort_total<-Base_1995_mortalite$Femme+Base_1995_mortalite$Homme

Base_1995_mortalite$mort_total<-Base_1995_mortalite$`0-9`+Base_1995_mortalite$`10-19`+Base_1995_mortalite$`20-39` +Base_1995_mortalite$`40-59` +Base_1995_mortalite$`60-64` +Base_1995_mortalite$`65-69` +Base_1995_mortalite$`70-74` +Base_1995_mortalite$`75+`



Base_1995_mortalite$taux_mortalite_total<-Base_1995_mortalite$mort_total/Base_1995_mortalite$value_estimated_population


Base_1995_mortalite<-filter(Base_1995_mortalite, Base_1995_mortalite$value_estimated_population>0)


Base_1995_mortalite<-Base_1995_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1995_mortalite<-filter(Base_1995_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1995_mortalite,"/heatwave and mortality code and data/new data/Base_1995_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1996 <- read_delim("fichier deces insee/deces-1996.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1996<-left_join(deces.1996, table_passage_bis)


deces.1996$COM<-ifelse(!is.na(deces.1996$COM_AP),deces.1996$COM_AP,deces.1996$lieudeces)

deces.1996$datedeces <- as.character(deces.1996$datedeces)
deces.1996$datedeces <- as.Date(deces.1996$datedeces, format = "%Y%m%d")


deces.1996$datenaiss <- as.character(deces.1996$datenaiss)
deces.1996$datenaiss <- as.Date(deces.1996$datenaiss, format = "%Y%m%d")


deces.1996$year <- as.numeric(format(deces.1996$datedeces, "%Y"))
deces.1996<-filter(deces.1996, deces.1996$year==1996)
deces.1996<-deces.1996[,-12]

library(lubridate)
deces.1996$age <- as.period(interval(deces.1996$datenaiss , deces.1996$datedeces ))
deces.1996$age_years <- year(deces.1996$age)

library(readr)


deces.1996$sexe<-as.character(deces.1996$sexe)

deces.1996$SEX<-ifelse(deces.1996$sexe=="1","Homme","Femme")

deces.1996_final<-deces.1996[,c("COM","datedeces","age_years","SEX")]

deces.1996_final$nbr_mort<-1


deces.1996_final$month <- as.numeric(format(deces.1996_final$datedeces, "%m"))




deces.1996_final$tranche_age <- cut(deces.1996_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1996_final$month <- as.numeric(format(deces.1996_final$datedeces, "%m"))


deces.1996_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1996_final,sum)

library(tidyr)

deces.1996_spread <- deces.1996_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1996_spread$`0-9`[is.na(deces.1996_spread$`0-9`)]<-0
deces.1996_spread$`10-19`[is.na(deces.1996_spread$`10-19`)]<-0
deces.1996_spread$`20-39`[is.na(deces.1996_spread$`20-39`)]<-0
deces.1996_spread$`40-59`[is.na(deces.1996_spread$`40-59`)]<-0
deces.1996_spread$`60-64`[is.na(deces.1996_spread$`60-64`)]<-0
deces.1996_spread$`65-69`[is.na(deces.1996_spread$`65-69`)]<-0
deces.1996_spread$`70-74`[is.na(deces.1996_spread$`70-74`)]<-0
deces.1996_spread$`75-79`[is.na(deces.1996_spread$`75-79`)]<-0
deces.1996_spread$`80+`[is.na(deces.1996_spread$`80+`)]<-0



#deces.1996_spread<-left_join(deces.1996_spread, deces.1996_spread)



deces.1996_spread$year<-1996


#fwrite(deces.1996_spread,"/fichier deces insee/décès travaillé/deces.1996_age_sexe.csv")




#################### partie 2 ######


RP_1996_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1996_age_sexe_final_2")

RP_1996_age_sexe_final_2<-RP_1996_age_sexe_final_2[,c(2:15)]

names(RP_1996_age_sexe_final_2)[names(RP_1996_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1996_age_sexe_final_2<-filter(RP_1996_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1996_mortalite<-left_join(deces.1996_spread,RP_1996_age_sexe_final_2)


Base_1996_mortalite$`75+`<-Base_1996_mortalite$`75-79`+Base_1996_mortalite$`80+`
Base_1996_mortalite$value_estimated_sum_75_plus_h_f<-Base_1996_mortalite$value_estimated_sum_75_79_h_f+Base_1996_mortalite$value_estimated_sum_80_plus_h_f


#Base_1996_mortalite$taux_mortalite_homme<-Base_1996_mortalite$Homme/Base_1996_mortalite$value_estimated_sum_homme

#Base_1996_mortalite$taux_mortalite_femme<-Base_1996_mortalite$Femme/Base_1996_mortalite$value_estimated_sum_femme

Base_1996_mortalite$taux_mortalite_0_9<-Base_1996_mortalite$`0-9`/Base_1996_mortalite$value_estimated_sum_0_9_h_f

Base_1996_mortalite$taux_mortalite_10_19<-Base_1996_mortalite$`10-19`/Base_1996_mortalite$value_estimated_sum_10_19_h_f

Base_1996_mortalite$taux_mortalite_20_39<-Base_1996_mortalite$`20-39`/Base_1996_mortalite$value_estimated_sum_20_39_h_f

Base_1996_mortalite$taux_mortalite_40_59<-Base_1996_mortalite$`40-59`/Base_1996_mortalite$value_estimated_sum_40_59_h_f

Base_1996_mortalite$taux_mortalite_60_64<-Base_1996_mortalite$`60-64`/Base_1996_mortalite$value_estimated_sum_60_64_h_f

Base_1996_mortalite$taux_mortalite_65_69<-Base_1996_mortalite$`65-69`/Base_1996_mortalite$value_estimated_sum_65_69_h_f

Base_1996_mortalite$taux_mortalite_70_74<-Base_1996_mortalite$`70-74`/Base_1996_mortalite$value_estimated_sum_70_74_h_f

Base_1996_mortalite$taux_mortalite_75_79<-Base_1996_mortalite$`75-79`/Base_1996_mortalite$value_estimated_sum_75_79_h_f

Base_1996_mortalite$taux_mortalite_75_plus<-Base_1996_mortalite$`75+`/Base_1996_mortalite$value_estimated_sum_75_plus_h_f

Base_1996_mortalite$taux_mortalite_80_plus<-Base_1996_mortalite$`80+`/Base_1996_mortalite$value_estimated_sum_80_plus_h_f

#Base_1996_mortalite$mort_total<-Base_1996_mortalite$Femme+Base_1996_mortalite$Homme

Base_1996_mortalite$mort_total<-Base_1996_mortalite$`0-9`+Base_1996_mortalite$`10-19`+Base_1996_mortalite$`20-39` +Base_1996_mortalite$`40-59` +Base_1996_mortalite$`60-64` +Base_1996_mortalite$`65-69` +Base_1996_mortalite$`70-74` +Base_1996_mortalite$`75+`



Base_1996_mortalite$taux_mortalite_total<-Base_1996_mortalite$mort_total/Base_1996_mortalite$value_estimated_population


Base_1996_mortalite<-filter(Base_1996_mortalite, Base_1996_mortalite$value_estimated_population>0)


Base_1996_mortalite<-Base_1996_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1996_mortalite<-filter(Base_1996_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1996_mortalite,"/heatwave and mortality code and data/new data/Base_1996_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1997 <- read_delim("fichier deces insee/deces-1997.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1997<-left_join(deces.1997, table_passage_bis)


deces.1997$COM<-ifelse(!is.na(deces.1997$COM_AP),deces.1997$COM_AP,deces.1997$lieudeces)

deces.1997$datedeces <- as.character(deces.1997$datedeces)
deces.1997$datedeces <- as.Date(deces.1997$datedeces, format = "%Y%m%d")


deces.1997$datenaiss <- as.character(deces.1997$datenaiss)
deces.1997$datenaiss <- as.Date(deces.1997$datenaiss, format = "%Y%m%d")


deces.1997$year <- as.numeric(format(deces.1997$datedeces, "%Y"))
deces.1997<-filter(deces.1997, deces.1997$year==1997)
deces.1997<-deces.1997[,-12]

library(lubridate)
deces.1997$age <- as.period(interval(deces.1997$datenaiss , deces.1997$datedeces ))
deces.1997$age_years <- year(deces.1997$age)

library(readr)


deces.1997$sexe<-as.character(deces.1997$sexe)

deces.1997$SEX<-ifelse(deces.1997$sexe=="1","Homme","Femme")

deces.1997_final<-deces.1997[,c("COM","datedeces","age_years","SEX")]

deces.1997_final$nbr_mort<-1


deces.1997_final$month <- as.numeric(format(deces.1997_final$datedeces, "%m"))




deces.1997_final$tranche_age <- cut(deces.1997_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1997_final$month <- as.numeric(format(deces.1997_final$datedeces, "%m"))


deces.1997_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1997_final,sum)

library(tidyr)

deces.1997_spread <- deces.1997_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1997_spread$`0-9`[is.na(deces.1997_spread$`0-9`)]<-0
deces.1997_spread$`10-19`[is.na(deces.1997_spread$`10-19`)]<-0
deces.1997_spread$`20-39`[is.na(deces.1997_spread$`20-39`)]<-0
deces.1997_spread$`40-59`[is.na(deces.1997_spread$`40-59`)]<-0
deces.1997_spread$`60-64`[is.na(deces.1997_spread$`60-64`)]<-0
deces.1997_spread$`65-69`[is.na(deces.1997_spread$`65-69`)]<-0
deces.1997_spread$`70-74`[is.na(deces.1997_spread$`70-74`)]<-0
deces.1997_spread$`75-79`[is.na(deces.1997_spread$`75-79`)]<-0
deces.1997_spread$`80+`[is.na(deces.1997_spread$`80+`)]<-0



#deces.1997_spread<-left_join(deces.1997_spread, deces.1997_spread)



deces.1997_spread$year<-1997


#fwrite(deces.1997_spread,"/fichier deces insee/décès travaillé/deces.1997_age_sexe.csv")




#################### partie 2 ######


RP_1997_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1997_age_sexe_final_2")

RP_1997_age_sexe_final_2<-RP_1997_age_sexe_final_2[,c(2:15)]

names(RP_1997_age_sexe_final_2)[names(RP_1997_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1997_age_sexe_final_2<-filter(RP_1997_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1997_mortalite<-left_join(deces.1997_spread,RP_1997_age_sexe_final_2)


Base_1997_mortalite$`75+`<-Base_1997_mortalite$`75-79`+Base_1997_mortalite$`80+`
Base_1997_mortalite$value_estimated_sum_75_plus_h_f<-Base_1997_mortalite$value_estimated_sum_75_79_h_f+Base_1997_mortalite$value_estimated_sum_80_plus_h_f


#Base_1997_mortalite$taux_mortalite_homme<-Base_1997_mortalite$Homme/Base_1997_mortalite$value_estimated_sum_homme

#Base_1997_mortalite$taux_mortalite_femme<-Base_1997_mortalite$Femme/Base_1997_mortalite$value_estimated_sum_femme

Base_1997_mortalite$taux_mortalite_0_9<-Base_1997_mortalite$`0-9`/Base_1997_mortalite$value_estimated_sum_0_9_h_f

Base_1997_mortalite$taux_mortalite_10_19<-Base_1997_mortalite$`10-19`/Base_1997_mortalite$value_estimated_sum_10_19_h_f

Base_1997_mortalite$taux_mortalite_20_39<-Base_1997_mortalite$`20-39`/Base_1997_mortalite$value_estimated_sum_20_39_h_f

Base_1997_mortalite$taux_mortalite_40_59<-Base_1997_mortalite$`40-59`/Base_1997_mortalite$value_estimated_sum_40_59_h_f

Base_1997_mortalite$taux_mortalite_60_64<-Base_1997_mortalite$`60-64`/Base_1997_mortalite$value_estimated_sum_60_64_h_f

Base_1997_mortalite$taux_mortalite_65_69<-Base_1997_mortalite$`65-69`/Base_1997_mortalite$value_estimated_sum_65_69_h_f

Base_1997_mortalite$taux_mortalite_70_74<-Base_1997_mortalite$`70-74`/Base_1997_mortalite$value_estimated_sum_70_74_h_f

Base_1997_mortalite$taux_mortalite_75_79<-Base_1997_mortalite$`75-79`/Base_1997_mortalite$value_estimated_sum_75_79_h_f

Base_1997_mortalite$taux_mortalite_75_plus<-Base_1997_mortalite$`75+`/Base_1997_mortalite$value_estimated_sum_75_plus_h_f

Base_1997_mortalite$taux_mortalite_80_plus<-Base_1997_mortalite$`80+`/Base_1997_mortalite$value_estimated_sum_80_plus_h_f

#Base_1997_mortalite$mort_total<-Base_1997_mortalite$Femme+Base_1997_mortalite$Homme

Base_1997_mortalite$mort_total<-Base_1997_mortalite$`0-9`+Base_1997_mortalite$`10-19`+Base_1997_mortalite$`20-39` +Base_1997_mortalite$`40-59` +Base_1997_mortalite$`60-64` +Base_1997_mortalite$`65-69` +Base_1997_mortalite$`70-74` +Base_1997_mortalite$`75+`



Base_1997_mortalite$taux_mortalite_total<-Base_1997_mortalite$mort_total/Base_1997_mortalite$value_estimated_population


Base_1997_mortalite<-filter(Base_1997_mortalite, Base_1997_mortalite$value_estimated_population>0)


Base_1997_mortalite<-Base_1997_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1997_mortalite<-filter(Base_1997_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1997_mortalite,"/heatwave and mortality code and data/new data/Base_1997_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1998 <- read_delim("fichier deces insee/deces-1998.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1998<-left_join(deces.1998, table_passage_bis)


deces.1998$COM<-ifelse(!is.na(deces.1998$COM_AP),deces.1998$COM_AP,deces.1998$lieudeces)

deces.1998$datedeces <- as.character(deces.1998$datedeces)
deces.1998$datedeces <- as.Date(deces.1998$datedeces, format = "%Y%m%d")


deces.1998$datenaiss <- as.character(deces.1998$datenaiss)
deces.1998$datenaiss <- as.Date(deces.1998$datenaiss, format = "%Y%m%d")


deces.1998$year <- as.numeric(format(deces.1998$datedeces, "%Y"))
deces.1998<-filter(deces.1998, deces.1998$year==1998)
deces.1998<-deces.1998[,-12]

library(lubridate)
deces.1998$age <- as.period(interval(deces.1998$datenaiss , deces.1998$datedeces ))
deces.1998$age_years <- year(deces.1998$age)

library(readr)


deces.1998$sexe<-as.character(deces.1998$sexe)

deces.1998$SEX<-ifelse(deces.1998$sexe=="1","Homme","Femme")

deces.1998_final<-deces.1998[,c("COM","datedeces","age_years","SEX")]

deces.1998_final$nbr_mort<-1


deces.1998_final$month <- as.numeric(format(deces.1998_final$datedeces, "%m"))




deces.1998_final$tranche_age <- cut(deces.1998_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1998_final$month <- as.numeric(format(deces.1998_final$datedeces, "%m"))


deces.1998_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1998_final,sum)

library(tidyr)

deces.1998_spread <- deces.1998_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1998_spread$`0-9`[is.na(deces.1998_spread$`0-9`)]<-0
deces.1998_spread$`10-19`[is.na(deces.1998_spread$`10-19`)]<-0
deces.1998_spread$`20-39`[is.na(deces.1998_spread$`20-39`)]<-0
deces.1998_spread$`40-59`[is.na(deces.1998_spread$`40-59`)]<-0
deces.1998_spread$`60-64`[is.na(deces.1998_spread$`60-64`)]<-0
deces.1998_spread$`65-69`[is.na(deces.1998_spread$`65-69`)]<-0
deces.1998_spread$`70-74`[is.na(deces.1998_spread$`70-74`)]<-0
deces.1998_spread$`75-79`[is.na(deces.1998_spread$`75-79`)]<-0
deces.1998_spread$`80+`[is.na(deces.1998_spread$`80+`)]<-0



#deces.1998_spread<-left_join(deces.1998_spread, deces.1998_spread)



deces.1998_spread$year<-1998


#fwrite(deces.1998_spread,"/fichier deces insee/décès travaillé/deces.1998_age_sexe.csv")




#################### partie 2 ######


RP_1998_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1998_age_sexe_final_2")

RP_1998_age_sexe_final_2<-RP_1998_age_sexe_final_2[,c(2:15)]

names(RP_1998_age_sexe_final_2)[names(RP_1998_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1998_age_sexe_final_2<-filter(RP_1998_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1998_mortalite<-left_join(deces.1998_spread,RP_1998_age_sexe_final_2)


Base_1998_mortalite$`75+`<-Base_1998_mortalite$`75-79`+Base_1998_mortalite$`80+`
Base_1998_mortalite$value_estimated_sum_75_plus_h_f<-Base_1998_mortalite$value_estimated_sum_75_79_h_f+Base_1998_mortalite$value_estimated_sum_80_plus_h_f


#Base_1998_mortalite$taux_mortalite_homme<-Base_1998_mortalite$Homme/Base_1998_mortalite$value_estimated_sum_homme

#Base_1998_mortalite$taux_mortalite_femme<-Base_1998_mortalite$Femme/Base_1998_mortalite$value_estimated_sum_femme

Base_1998_mortalite$taux_mortalite_0_9<-Base_1998_mortalite$`0-9`/Base_1998_mortalite$value_estimated_sum_0_9_h_f

Base_1998_mortalite$taux_mortalite_10_19<-Base_1998_mortalite$`10-19`/Base_1998_mortalite$value_estimated_sum_10_19_h_f

Base_1998_mortalite$taux_mortalite_20_39<-Base_1998_mortalite$`20-39`/Base_1998_mortalite$value_estimated_sum_20_39_h_f

Base_1998_mortalite$taux_mortalite_40_59<-Base_1998_mortalite$`40-59`/Base_1998_mortalite$value_estimated_sum_40_59_h_f

Base_1998_mortalite$taux_mortalite_60_64<-Base_1998_mortalite$`60-64`/Base_1998_mortalite$value_estimated_sum_60_64_h_f

Base_1998_mortalite$taux_mortalite_65_69<-Base_1998_mortalite$`65-69`/Base_1998_mortalite$value_estimated_sum_65_69_h_f

Base_1998_mortalite$taux_mortalite_70_74<-Base_1998_mortalite$`70-74`/Base_1998_mortalite$value_estimated_sum_70_74_h_f

Base_1998_mortalite$taux_mortalite_75_79<-Base_1998_mortalite$`75-79`/Base_1998_mortalite$value_estimated_sum_75_79_h_f

Base_1998_mortalite$taux_mortalite_75_plus<-Base_1998_mortalite$`75+`/Base_1998_mortalite$value_estimated_sum_75_plus_h_f

Base_1998_mortalite$taux_mortalite_80_plus<-Base_1998_mortalite$`80+`/Base_1998_mortalite$value_estimated_sum_80_plus_h_f

#Base_1998_mortalite$mort_total<-Base_1998_mortalite$Femme+Base_1998_mortalite$Homme

Base_1998_mortalite$mort_total<-Base_1998_mortalite$`0-9`+Base_1998_mortalite$`10-19`+Base_1998_mortalite$`20-39` +Base_1998_mortalite$`40-59` +Base_1998_mortalite$`60-64` +Base_1998_mortalite$`65-69` +Base_1998_mortalite$`70-74` +Base_1998_mortalite$`75+`



Base_1998_mortalite$taux_mortalite_total<-Base_1998_mortalite$mort_total/Base_1998_mortalite$value_estimated_population


Base_1998_mortalite<-filter(Base_1998_mortalite, Base_1998_mortalite$value_estimated_population>0)


Base_1998_mortalite<-Base_1998_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1998_mortalite<-filter(Base_1998_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1998_mortalite,"/heatwave and mortality code and data/new data/Base_1998_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1999 <- read_delim("fichier deces insee/deces-1999.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1999<-left_join(deces.1999, table_passage_bis)


deces.1999$COM<-ifelse(!is.na(deces.1999$COM_AP),deces.1999$COM_AP,deces.1999$lieudeces)

deces.1999$datedeces <- as.character(deces.1999$datedeces)
deces.1999$datedeces <- as.Date(deces.1999$datedeces, format = "%Y%m%d")


deces.1999$datenaiss <- as.character(deces.1999$datenaiss)
deces.1999$datenaiss <- as.Date(deces.1999$datenaiss, format = "%Y%m%d")


deces.1999$year <- as.numeric(format(deces.1999$datedeces, "%Y"))
deces.1999<-filter(deces.1999, deces.1999$year==1999)
deces.1999<-deces.1999[,-12]

library(lubridate)
deces.1999$age <- as.period(interval(deces.1999$datenaiss , deces.1999$datedeces ))
deces.1999$age_years <- year(deces.1999$age)

library(readr)


deces.1999$sexe<-as.character(deces.1999$sexe)

deces.1999$SEX<-ifelse(deces.1999$sexe=="1","Homme","Femme")

deces.1999_final<-deces.1999[,c("COM","datedeces","age_years","SEX")]

deces.1999_final$nbr_mort<-1


deces.1999_final$month <- as.numeric(format(deces.1999_final$datedeces, "%m"))




deces.1999_final$tranche_age <- cut(deces.1999_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1999_final$month <- as.numeric(format(deces.1999_final$datedeces, "%m"))


deces.1999_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.1999_final,sum)

library(tidyr)

deces.1999_spread <- deces.1999_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1999_spread$`0-9`[is.na(deces.1999_spread$`0-9`)]<-0
deces.1999_spread$`10-19`[is.na(deces.1999_spread$`10-19`)]<-0
deces.1999_spread$`20-39`[is.na(deces.1999_spread$`20-39`)]<-0
deces.1999_spread$`40-59`[is.na(deces.1999_spread$`40-59`)]<-0
deces.1999_spread$`60-64`[is.na(deces.1999_spread$`60-64`)]<-0
deces.1999_spread$`65-69`[is.na(deces.1999_spread$`65-69`)]<-0
deces.1999_spread$`70-74`[is.na(deces.1999_spread$`70-74`)]<-0
deces.1999_spread$`75-79`[is.na(deces.1999_spread$`75-79`)]<-0
deces.1999_spread$`80+`[is.na(deces.1999_spread$`80+`)]<-0



#deces.1999_spread<-left_join(deces.1999_spread, deces.1999_spread)



deces.1999_spread$year<-1999


#fwrite(deces.1999_spread,"/fichier deces insee/décès travaillé/deces.1999_age_sexe.csv")




#################### partie 2 ######


RP_1999_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1999_age_sexe_final_2")

RP_1999_age_sexe_final_2<-RP_1999_age_sexe_final_2[,c(2:15)]

names(RP_1999_age_sexe_final_2)[names(RP_1999_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_1999_age_sexe_final_2<-filter(RP_1999_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_1999_mortalite<-left_join(deces.1999_spread,RP_1999_age_sexe_final_2)


Base_1999_mortalite$`75+`<-Base_1999_mortalite$`75-79`+Base_1999_mortalite$`80+`
Base_1999_mortalite$value_estimated_sum_75_plus_h_f<-Base_1999_mortalite$value_estimated_sum_75_79_h_f+Base_1999_mortalite$value_estimated_sum_80_plus_h_f


#Base_1999_mortalite$taux_mortalite_homme<-Base_1999_mortalite$Homme/Base_1999_mortalite$value_estimated_sum_homme

#Base_1999_mortalite$taux_mortalite_femme<-Base_1999_mortalite$Femme/Base_1999_mortalite$value_estimated_sum_femme

Base_1999_mortalite$taux_mortalite_0_9<-Base_1999_mortalite$`0-9`/Base_1999_mortalite$value_estimated_sum_0_9_h_f

Base_1999_mortalite$taux_mortalite_10_19<-Base_1999_mortalite$`10-19`/Base_1999_mortalite$value_estimated_sum_10_19_h_f

Base_1999_mortalite$taux_mortalite_20_39<-Base_1999_mortalite$`20-39`/Base_1999_mortalite$value_estimated_sum_20_39_h_f

Base_1999_mortalite$taux_mortalite_40_59<-Base_1999_mortalite$`40-59`/Base_1999_mortalite$value_estimated_sum_40_59_h_f

Base_1999_mortalite$taux_mortalite_60_64<-Base_1999_mortalite$`60-64`/Base_1999_mortalite$value_estimated_sum_60_64_h_f

Base_1999_mortalite$taux_mortalite_65_69<-Base_1999_mortalite$`65-69`/Base_1999_mortalite$value_estimated_sum_65_69_h_f

Base_1999_mortalite$taux_mortalite_70_74<-Base_1999_mortalite$`70-74`/Base_1999_mortalite$value_estimated_sum_70_74_h_f

Base_1999_mortalite$taux_mortalite_75_79<-Base_1999_mortalite$`75-79`/Base_1999_mortalite$value_estimated_sum_75_79_h_f

Base_1999_mortalite$taux_mortalite_75_plus<-Base_1999_mortalite$`75+`/Base_1999_mortalite$value_estimated_sum_75_plus_h_f

Base_1999_mortalite$taux_mortalite_80_plus<-Base_1999_mortalite$`80+`/Base_1999_mortalite$value_estimated_sum_80_plus_h_f

#Base_1999_mortalite$mort_total<-Base_1999_mortalite$Femme+Base_1999_mortalite$Homme

Base_1999_mortalite$mort_total<-Base_1999_mortalite$`0-9`+Base_1999_mortalite$`10-19`+Base_1999_mortalite$`20-39` +Base_1999_mortalite$`40-59` +Base_1999_mortalite$`60-64` +Base_1999_mortalite$`65-69` +Base_1999_mortalite$`70-74` +Base_1999_mortalite$`75+`



Base_1999_mortalite$taux_mortalite_total<-Base_1999_mortalite$mort_total/Base_1999_mortalite$value_estimated_population


Base_1999_mortalite<-filter(Base_1999_mortalite, Base_1999_mortalite$value_estimated_population>0)


Base_1999_mortalite<-Base_1999_mortalite[,c(1:2,12,24,27:36,38)]


#Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_homme))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_1999_mortalite<-filter(Base_1999_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_1999_mortalite,"/heatwave and mortality code and data/new data/Base_1999_mortalite.csv")




rm(list = ls())
gc()


#################################### 



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2000 <- read_delim("fichier deces insee/deces-2000.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2000<-left_join(deces.2000, table_passage_bis)


deces.2000$COM<-ifelse(!is.na(deces.2000$COM_AP),deces.2000$COM_AP,deces.2000$lieudeces)

deces.2000$datedeces <- as.character(deces.2000$datedeces)
deces.2000$datedeces <- as.Date(deces.2000$datedeces, format = "%Y%m%d")


deces.2000$datenaiss <- as.character(deces.2000$datenaiss)
deces.2000$datenaiss <- as.Date(deces.2000$datenaiss, format = "%Y%m%d")


deces.2000$year <- as.numeric(format(deces.2000$datedeces, "%Y"))
deces.2000<-filter(deces.2000, deces.2000$year==2000)
deces.2000<-deces.2000[,-12]

library(lubridate)
deces.2000$age <- as.period(interval(deces.2000$datenaiss , deces.2000$datedeces ))
deces.2000$age_years <- year(deces.2000$age)

library(readr)


deces.2000$sexe<-as.character(deces.2000$sexe)

deces.2000$SEX<-ifelse(deces.2000$sexe=="1","Homme","Femme")

deces.2000_final<-deces.2000[,c("COM","datedeces","age_years","SEX")]

deces.2000_final$nbr_mort<-1


deces.2000_final$month <- as.numeric(format(deces.2000_final$datedeces, "%m"))




deces.2000_final$tranche_age <- cut(deces.2000_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2000_final$month <- as.numeric(format(deces.2000_final$datedeces, "%m"))


deces.2000_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2000_final,sum)

library(tidyr)

deces.2000_spread <- deces.2000_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2000_spread$`0-9`[is.na(deces.2000_spread$`0-9`)]<-0
deces.2000_spread$`10-19`[is.na(deces.2000_spread$`10-19`)]<-0
deces.2000_spread$`20-39`[is.na(deces.2000_spread$`20-39`)]<-0
deces.2000_spread$`40-59`[is.na(deces.2000_spread$`40-59`)]<-0
deces.2000_spread$`60-64`[is.na(deces.2000_spread$`60-64`)]<-0
deces.2000_spread$`65-69`[is.na(deces.2000_spread$`65-69`)]<-0
deces.2000_spread$`70-74`[is.na(deces.2000_spread$`70-74`)]<-0
deces.2000_spread$`75-79`[is.na(deces.2000_spread$`75-79`)]<-0
deces.2000_spread$`80+`[is.na(deces.2000_spread$`80+`)]<-0



#deces.2000_spread<-left_join(deces.2000_spread, deces.2000_spread)



deces.2000_spread$year<-2000


#fwrite(deces.2000_spread,"/fichier deces insee/décès travaillé/deces.2000_age_sexe.csv")




#################### partie 2 ######


RP_2000_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2000_age_sexe_final_2")

RP_2000_age_sexe_final_2<-RP_2000_age_sexe_final_2[,c(2:15)]

names(RP_2000_age_sexe_final_2)[names(RP_2000_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2000_age_sexe_final_2<-filter(RP_2000_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2000_mortalite<-left_join(deces.2000_spread,RP_2000_age_sexe_final_2)


Base_2000_mortalite$`75+`<-Base_2000_mortalite$`75-79`+Base_2000_mortalite$`80+`
Base_2000_mortalite$value_estimated_sum_75_plus_h_f<-Base_2000_mortalite$value_estimated_sum_75_79_h_f+Base_2000_mortalite$value_estimated_sum_80_plus_h_f


#Base_2000_mortalite$taux_mortalite_homme<-Base_2000_mortalite$Homme/Base_2000_mortalite$value_estimated_sum_homme

#Base_2000_mortalite$taux_mortalite_femme<-Base_2000_mortalite$Femme/Base_2000_mortalite$value_estimated_sum_femme

Base_2000_mortalite$taux_mortalite_0_9<-Base_2000_mortalite$`0-9`/Base_2000_mortalite$value_estimated_sum_0_9_h_f

Base_2000_mortalite$taux_mortalite_10_19<-Base_2000_mortalite$`10-19`/Base_2000_mortalite$value_estimated_sum_10_19_h_f

Base_2000_mortalite$taux_mortalite_20_39<-Base_2000_mortalite$`20-39`/Base_2000_mortalite$value_estimated_sum_20_39_h_f

Base_2000_mortalite$taux_mortalite_40_59<-Base_2000_mortalite$`40-59`/Base_2000_mortalite$value_estimated_sum_40_59_h_f

Base_2000_mortalite$taux_mortalite_60_64<-Base_2000_mortalite$`60-64`/Base_2000_mortalite$value_estimated_sum_60_64_h_f

Base_2000_mortalite$taux_mortalite_65_69<-Base_2000_mortalite$`65-69`/Base_2000_mortalite$value_estimated_sum_65_69_h_f

Base_2000_mortalite$taux_mortalite_70_74<-Base_2000_mortalite$`70-74`/Base_2000_mortalite$value_estimated_sum_70_74_h_f

Base_2000_mortalite$taux_mortalite_75_79<-Base_2000_mortalite$`75-79`/Base_2000_mortalite$value_estimated_sum_75_79_h_f

Base_2000_mortalite$taux_mortalite_75_plus<-Base_2000_mortalite$`75+`/Base_2000_mortalite$value_estimated_sum_75_plus_h_f

Base_2000_mortalite$taux_mortalite_80_plus<-Base_2000_mortalite$`80+`/Base_2000_mortalite$value_estimated_sum_80_plus_h_f

#Base_2000_mortalite$mort_total<-Base_2000_mortalite$Femme+Base_2000_mortalite$Homme

Base_2000_mortalite$mort_total<-Base_2000_mortalite$`0-9`+Base_2000_mortalite$`10-19`+Base_2000_mortalite$`20-39` +Base_2000_mortalite$`40-59` +Base_2000_mortalite$`60-64` +Base_2000_mortalite$`65-69` +Base_2000_mortalite$`70-74` +Base_2000_mortalite$`75+`



Base_2000_mortalite$taux_mortalite_total<-Base_2000_mortalite$mort_total/Base_2000_mortalite$value_estimated_population


Base_2000_mortalite<-filter(Base_2000_mortalite, Base_2000_mortalite$value_estimated_population>0)


Base_2000_mortalite<-Base_2000_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2000_mortalite<-filter(Base_2000_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2000_mortalite,"/heatwave and mortality code and data/new data/Base_2000_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2001 <- read_delim("fichier deces insee/deces-2001.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2001<-left_join(deces.2001, table_passage_bis)


deces.2001$COM<-ifelse(!is.na(deces.2001$COM_AP),deces.2001$COM_AP,deces.2001$lieudeces)

deces.2001$datedeces <- as.character(deces.2001$datedeces)
deces.2001$datedeces <- as.Date(deces.2001$datedeces, format = "%Y%m%d")


deces.2001$datenaiss <- as.character(deces.2001$datenaiss)
deces.2001$datenaiss <- as.Date(deces.2001$datenaiss, format = "%Y%m%d")


deces.2001$year <- as.numeric(format(deces.2001$datedeces, "%Y"))
deces.2001<-filter(deces.2001, deces.2001$year==2001)
deces.2001<-deces.2001[,-12]

library(lubridate)
deces.2001$age <- as.period(interval(deces.2001$datenaiss , deces.2001$datedeces ))
deces.2001$age_years <- year(deces.2001$age)

library(readr)


deces.2001$sexe<-as.character(deces.2001$sexe)

deces.2001$SEX<-ifelse(deces.2001$sexe=="1","Homme","Femme")

deces.2001_final<-deces.2001[,c("COM","datedeces","age_years","SEX")]

deces.2001_final$nbr_mort<-1


deces.2001_final$month <- as.numeric(format(deces.2001_final$datedeces, "%m"))




deces.2001_final$tranche_age <- cut(deces.2001_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2001_final$month <- as.numeric(format(deces.2001_final$datedeces, "%m"))


deces.2001_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2001_final,sum)

library(tidyr)

deces.2001_spread <- deces.2001_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2001_spread$`0-9`[is.na(deces.2001_spread$`0-9`)]<-0
deces.2001_spread$`10-19`[is.na(deces.2001_spread$`10-19`)]<-0
deces.2001_spread$`20-39`[is.na(deces.2001_spread$`20-39`)]<-0
deces.2001_spread$`40-59`[is.na(deces.2001_spread$`40-59`)]<-0
deces.2001_spread$`60-64`[is.na(deces.2001_spread$`60-64`)]<-0
deces.2001_spread$`65-69`[is.na(deces.2001_spread$`65-69`)]<-0
deces.2001_spread$`70-74`[is.na(deces.2001_spread$`70-74`)]<-0
deces.2001_spread$`75-79`[is.na(deces.2001_spread$`75-79`)]<-0
deces.2001_spread$`80+`[is.na(deces.2001_spread$`80+`)]<-0



#deces.2001_spread<-left_join(deces.2001_spread, deces.2001_spread)



deces.2001_spread$year<-2001


#fwrite(deces.2001_spread,"/fichier deces insee/décès travaillé/deces.2001_age_sexe.csv")




#################### partie 2 ######


RP_2001_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2001_age_sexe_final_2")

RP_2001_age_sexe_final_2<-RP_2001_age_sexe_final_2[,c(2:15)]

names(RP_2001_age_sexe_final_2)[names(RP_2001_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2001_age_sexe_final_2<-filter(RP_2001_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2001_mortalite<-left_join(deces.2001_spread,RP_2001_age_sexe_final_2)


Base_2001_mortalite$`75+`<-Base_2001_mortalite$`75-79`+Base_2001_mortalite$`80+`
Base_2001_mortalite$value_estimated_sum_75_plus_h_f<-Base_2001_mortalite$value_estimated_sum_75_79_h_f+Base_2001_mortalite$value_estimated_sum_80_plus_h_f


#Base_2001_mortalite$taux_mortalite_homme<-Base_2001_mortalite$Homme/Base_2001_mortalite$value_estimated_sum_homme

#Base_2001_mortalite$taux_mortalite_femme<-Base_2001_mortalite$Femme/Base_2001_mortalite$value_estimated_sum_femme

Base_2001_mortalite$taux_mortalite_0_9<-Base_2001_mortalite$`0-9`/Base_2001_mortalite$value_estimated_sum_0_9_h_f

Base_2001_mortalite$taux_mortalite_10_19<-Base_2001_mortalite$`10-19`/Base_2001_mortalite$value_estimated_sum_10_19_h_f

Base_2001_mortalite$taux_mortalite_20_39<-Base_2001_mortalite$`20-39`/Base_2001_mortalite$value_estimated_sum_20_39_h_f

Base_2001_mortalite$taux_mortalite_40_59<-Base_2001_mortalite$`40-59`/Base_2001_mortalite$value_estimated_sum_40_59_h_f

Base_2001_mortalite$taux_mortalite_60_64<-Base_2001_mortalite$`60-64`/Base_2001_mortalite$value_estimated_sum_60_64_h_f

Base_2001_mortalite$taux_mortalite_65_69<-Base_2001_mortalite$`65-69`/Base_2001_mortalite$value_estimated_sum_65_69_h_f

Base_2001_mortalite$taux_mortalite_70_74<-Base_2001_mortalite$`70-74`/Base_2001_mortalite$value_estimated_sum_70_74_h_f

Base_2001_mortalite$taux_mortalite_75_79<-Base_2001_mortalite$`75-79`/Base_2001_mortalite$value_estimated_sum_75_79_h_f

Base_2001_mortalite$taux_mortalite_75_plus<-Base_2001_mortalite$`75+`/Base_2001_mortalite$value_estimated_sum_75_plus_h_f

Base_2001_mortalite$taux_mortalite_80_plus<-Base_2001_mortalite$`80+`/Base_2001_mortalite$value_estimated_sum_80_plus_h_f

#Base_2001_mortalite$mort_total<-Base_2001_mortalite$Femme+Base_2001_mortalite$Homme

Base_2001_mortalite$mort_total<-Base_2001_mortalite$`0-9`+Base_2001_mortalite$`10-19`+Base_2001_mortalite$`20-39` +Base_2001_mortalite$`40-59` +Base_2001_mortalite$`60-64` +Base_2001_mortalite$`65-69` +Base_2001_mortalite$`70-74` +Base_2001_mortalite$`75+`



Base_2001_mortalite$taux_mortalite_total<-Base_2001_mortalite$mort_total/Base_2001_mortalite$value_estimated_population


Base_2001_mortalite<-filter(Base_2001_mortalite, Base_2001_mortalite$value_estimated_population>0)


Base_2001_mortalite<-Base_2001_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2001_mortalite<-filter(Base_2001_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2001_mortalite,"/heatwave and mortality code and data/new data/Base_2001_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2002 <- read_delim("fichier deces insee/deces-2002.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2002<-left_join(deces.2002, table_passage_bis)


deces.2002$COM<-ifelse(!is.na(deces.2002$COM_AP),deces.2002$COM_AP,deces.2002$lieudeces)

deces.2002$datedeces <- as.character(deces.2002$datedeces)
deces.2002$datedeces <- as.Date(deces.2002$datedeces, format = "%Y%m%d")


deces.2002$datenaiss <- as.character(deces.2002$datenaiss)
deces.2002$datenaiss <- as.Date(deces.2002$datenaiss, format = "%Y%m%d")


deces.2002$year <- as.numeric(format(deces.2002$datedeces, "%Y"))
deces.2002<-filter(deces.2002, deces.2002$year==2002)
deces.2002<-deces.2002[,-12]

library(lubridate)
deces.2002$age <- as.period(interval(deces.2002$datenaiss , deces.2002$datedeces ))
deces.2002$age_years <- year(deces.2002$age)

library(readr)


deces.2002$sexe<-as.character(deces.2002$sexe)

deces.2002$SEX<-ifelse(deces.2002$sexe=="1","Homme","Femme")

deces.2002_final<-deces.2002[,c("COM","datedeces","age_years","SEX")]

deces.2002_final$nbr_mort<-1


deces.2002_final$month <- as.numeric(format(deces.2002_final$datedeces, "%m"))




deces.2002_final$tranche_age <- cut(deces.2002_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2002_final$month <- as.numeric(format(deces.2002_final$datedeces, "%m"))


deces.2002_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2002_final,sum)

library(tidyr)

deces.2002_spread <- deces.2002_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2002_spread$`0-9`[is.na(deces.2002_spread$`0-9`)]<-0
deces.2002_spread$`10-19`[is.na(deces.2002_spread$`10-19`)]<-0
deces.2002_spread$`20-39`[is.na(deces.2002_spread$`20-39`)]<-0
deces.2002_spread$`40-59`[is.na(deces.2002_spread$`40-59`)]<-0
deces.2002_spread$`60-64`[is.na(deces.2002_spread$`60-64`)]<-0
deces.2002_spread$`65-69`[is.na(deces.2002_spread$`65-69`)]<-0
deces.2002_spread$`70-74`[is.na(deces.2002_spread$`70-74`)]<-0
deces.2002_spread$`75-79`[is.na(deces.2002_spread$`75-79`)]<-0
deces.2002_spread$`80+`[is.na(deces.2002_spread$`80+`)]<-0



#deces.2002_spread<-left_join(deces.2002_spread, deces.2002_spread)



deces.2002_spread$year<-2002


#fwrite(deces.2002_spread,"/fichier deces insee/décès travaillé/deces.2002_age_sexe.csv")




#################### partie 2 ######


RP_2002_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2002_age_sexe_final_2")

RP_2002_age_sexe_final_2<-RP_2002_age_sexe_final_2[,c(2:15)]

names(RP_2002_age_sexe_final_2)[names(RP_2002_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2002_age_sexe_final_2<-filter(RP_2002_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2002_mortalite<-left_join(deces.2002_spread,RP_2002_age_sexe_final_2)


Base_2002_mortalite$`75+`<-Base_2002_mortalite$`75-79`+Base_2002_mortalite$`80+`
Base_2002_mortalite$value_estimated_sum_75_plus_h_f<-Base_2002_mortalite$value_estimated_sum_75_79_h_f+Base_2002_mortalite$value_estimated_sum_80_plus_h_f


#Base_2002_mortalite$taux_mortalite_homme<-Base_2002_mortalite$Homme/Base_2002_mortalite$value_estimated_sum_homme

#Base_2002_mortalite$taux_mortalite_femme<-Base_2002_mortalite$Femme/Base_2002_mortalite$value_estimated_sum_femme

Base_2002_mortalite$taux_mortalite_0_9<-Base_2002_mortalite$`0-9`/Base_2002_mortalite$value_estimated_sum_0_9_h_f

Base_2002_mortalite$taux_mortalite_10_19<-Base_2002_mortalite$`10-19`/Base_2002_mortalite$value_estimated_sum_10_19_h_f

Base_2002_mortalite$taux_mortalite_20_39<-Base_2002_mortalite$`20-39`/Base_2002_mortalite$value_estimated_sum_20_39_h_f

Base_2002_mortalite$taux_mortalite_40_59<-Base_2002_mortalite$`40-59`/Base_2002_mortalite$value_estimated_sum_40_59_h_f

Base_2002_mortalite$taux_mortalite_60_64<-Base_2002_mortalite$`60-64`/Base_2002_mortalite$value_estimated_sum_60_64_h_f

Base_2002_mortalite$taux_mortalite_65_69<-Base_2002_mortalite$`65-69`/Base_2002_mortalite$value_estimated_sum_65_69_h_f

Base_2002_mortalite$taux_mortalite_70_74<-Base_2002_mortalite$`70-74`/Base_2002_mortalite$value_estimated_sum_70_74_h_f

Base_2002_mortalite$taux_mortalite_75_79<-Base_2002_mortalite$`75-79`/Base_2002_mortalite$value_estimated_sum_75_79_h_f

Base_2002_mortalite$taux_mortalite_75_plus<-Base_2002_mortalite$`75+`/Base_2002_mortalite$value_estimated_sum_75_plus_h_f

Base_2002_mortalite$taux_mortalite_80_plus<-Base_2002_mortalite$`80+`/Base_2002_mortalite$value_estimated_sum_80_plus_h_f

#Base_2002_mortalite$mort_total<-Base_2002_mortalite$Femme+Base_2002_mortalite$Homme

Base_2002_mortalite$mort_total<-Base_2002_mortalite$`0-9`+Base_2002_mortalite$`10-19`+Base_2002_mortalite$`20-39` +Base_2002_mortalite$`40-59` +Base_2002_mortalite$`60-64` +Base_2002_mortalite$`65-69` +Base_2002_mortalite$`70-74` +Base_2002_mortalite$`75+`



Base_2002_mortalite$taux_mortalite_total<-Base_2002_mortalite$mort_total/Base_2002_mortalite$value_estimated_population


Base_2002_mortalite<-filter(Base_2002_mortalite, Base_2002_mortalite$value_estimated_population>0)


Base_2002_mortalite<-Base_2002_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2002_mortalite<-filter(Base_2002_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2002_mortalite,"/heatwave and mortality code and data/new data/Base_2002_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2003 <- read_delim("fichier deces insee/deces-2003.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2003<-left_join(deces.2003, table_passage_bis)


deces.2003$COM<-ifelse(!is.na(deces.2003$COM_AP),deces.2003$COM_AP,deces.2003$lieudeces)

deces.2003$datedeces <- as.character(deces.2003$datedeces)
deces.2003$datedeces <- as.Date(deces.2003$datedeces, format = "%Y%m%d")


deces.2003$datenaiss <- as.character(deces.2003$datenaiss)
deces.2003$datenaiss <- as.Date(deces.2003$datenaiss, format = "%Y%m%d")


deces.2003$year <- as.numeric(format(deces.2003$datedeces, "%Y"))
deces.2003<-filter(deces.2003, deces.2003$year==2003)
deces.2003<-deces.2003[,-12]

library(lubridate)
deces.2003$age <- as.period(interval(deces.2003$datenaiss , deces.2003$datedeces ))
deces.2003$age_years <- year(deces.2003$age)

library(readr)


deces.2003$sexe<-as.character(deces.2003$sexe)

deces.2003$SEX<-ifelse(deces.2003$sexe=="1","Homme","Femme")

deces.2003_final<-deces.2003[,c("COM","datedeces","age_years","SEX")]

deces.2003_final$nbr_mort<-1


deces.2003_final$month <- as.numeric(format(deces.2003_final$datedeces, "%m"))




deces.2003_final$tranche_age <- cut(deces.2003_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2003_final$month <- as.numeric(format(deces.2003_final$datedeces, "%m"))


deces.2003_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2003_final,sum)

library(tidyr)

deces.2003_spread <- deces.2003_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2003_spread$`0-9`[is.na(deces.2003_spread$`0-9`)]<-0
deces.2003_spread$`10-19`[is.na(deces.2003_spread$`10-19`)]<-0
deces.2003_spread$`20-39`[is.na(deces.2003_spread$`20-39`)]<-0
deces.2003_spread$`40-59`[is.na(deces.2003_spread$`40-59`)]<-0
deces.2003_spread$`60-64`[is.na(deces.2003_spread$`60-64`)]<-0
deces.2003_spread$`65-69`[is.na(deces.2003_spread$`65-69`)]<-0
deces.2003_spread$`70-74`[is.na(deces.2003_spread$`70-74`)]<-0
deces.2003_spread$`75-79`[is.na(deces.2003_spread$`75-79`)]<-0
deces.2003_spread$`80+`[is.na(deces.2003_spread$`80+`)]<-0



#deces.2003_spread<-left_join(deces.2003_spread, deces.2003_spread)



deces.2003_spread$year<-2003


#fwrite(deces.2003_spread,"/fichier deces insee/décès travaillé/deces.2003_age_sexe.csv")




#################### partie 2 ######


RP_2003_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2003_age_sexe_final_2")

RP_2003_age_sexe_final_2<-RP_2003_age_sexe_final_2[,c(2:15)]

names(RP_2003_age_sexe_final_2)[names(RP_2003_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2003_age_sexe_final_2<-filter(RP_2003_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2003_mortalite<-left_join(deces.2003_spread,RP_2003_age_sexe_final_2)


Base_2003_mortalite$`75+`<-Base_2003_mortalite$`75-79`+Base_2003_mortalite$`80+`
Base_2003_mortalite$value_estimated_sum_75_plus_h_f<-Base_2003_mortalite$value_estimated_sum_75_79_h_f+Base_2003_mortalite$value_estimated_sum_80_plus_h_f


#Base_2003_mortalite$taux_mortalite_homme<-Base_2003_mortalite$Homme/Base_2003_mortalite$value_estimated_sum_homme

#Base_2003_mortalite$taux_mortalite_femme<-Base_2003_mortalite$Femme/Base_2003_mortalite$value_estimated_sum_femme

Base_2003_mortalite$taux_mortalite_0_9<-Base_2003_mortalite$`0-9`/Base_2003_mortalite$value_estimated_sum_0_9_h_f

Base_2003_mortalite$taux_mortalite_10_19<-Base_2003_mortalite$`10-19`/Base_2003_mortalite$value_estimated_sum_10_19_h_f

Base_2003_mortalite$taux_mortalite_20_39<-Base_2003_mortalite$`20-39`/Base_2003_mortalite$value_estimated_sum_20_39_h_f

Base_2003_mortalite$taux_mortalite_40_59<-Base_2003_mortalite$`40-59`/Base_2003_mortalite$value_estimated_sum_40_59_h_f

Base_2003_mortalite$taux_mortalite_60_64<-Base_2003_mortalite$`60-64`/Base_2003_mortalite$value_estimated_sum_60_64_h_f

Base_2003_mortalite$taux_mortalite_65_69<-Base_2003_mortalite$`65-69`/Base_2003_mortalite$value_estimated_sum_65_69_h_f

Base_2003_mortalite$taux_mortalite_70_74<-Base_2003_mortalite$`70-74`/Base_2003_mortalite$value_estimated_sum_70_74_h_f

Base_2003_mortalite$taux_mortalite_75_79<-Base_2003_mortalite$`75-79`/Base_2003_mortalite$value_estimated_sum_75_79_h_f

Base_2003_mortalite$taux_mortalite_75_plus<-Base_2003_mortalite$`75+`/Base_2003_mortalite$value_estimated_sum_75_plus_h_f

Base_2003_mortalite$taux_mortalite_80_plus<-Base_2003_mortalite$`80+`/Base_2003_mortalite$value_estimated_sum_80_plus_h_f

#Base_2003_mortalite$mort_total<-Base_2003_mortalite$Femme+Base_2003_mortalite$Homme

Base_2003_mortalite$mort_total<-Base_2003_mortalite$`0-9`+Base_2003_mortalite$`10-19`+Base_2003_mortalite$`20-39` +Base_2003_mortalite$`40-59` +Base_2003_mortalite$`60-64` +Base_2003_mortalite$`65-69` +Base_2003_mortalite$`70-74` +Base_2003_mortalite$`75+`



Base_2003_mortalite$taux_mortalite_total<-Base_2003_mortalite$mort_total/Base_2003_mortalite$value_estimated_population


Base_2003_mortalite<-filter(Base_2003_mortalite, Base_2003_mortalite$value_estimated_population>0)


Base_2003_mortalite<-Base_2003_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2003_mortalite<-filter(Base_2003_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2003_mortalite,"/heatwave and mortality code and data/new data/Base_2003_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2004 <- read_delim("fichier deces insee/deces-2004.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2004<-left_join(deces.2004, table_passage_bis)


deces.2004$COM<-ifelse(!is.na(deces.2004$COM_AP),deces.2004$COM_AP,deces.2004$lieudeces)

deces.2004$datedeces <- as.character(deces.2004$datedeces)
deces.2004$datedeces <- as.Date(deces.2004$datedeces, format = "%Y%m%d")


deces.2004$datenaiss <- as.character(deces.2004$datenaiss)
deces.2004$datenaiss <- as.Date(deces.2004$datenaiss, format = "%Y%m%d")


deces.2004$year <- as.numeric(format(deces.2004$datedeces, "%Y"))
deces.2004<-filter(deces.2004, deces.2004$year==2004)
deces.2004<-deces.2004[,-12]

library(lubridate)
deces.2004$age <- as.period(interval(deces.2004$datenaiss , deces.2004$datedeces ))
deces.2004$age_years <- year(deces.2004$age)

library(readr)


deces.2004$sexe<-as.character(deces.2004$sexe)

deces.2004$SEX<-ifelse(deces.2004$sexe=="1","Homme","Femme")

deces.2004_final<-deces.2004[,c("COM","datedeces","age_years","SEX")]

deces.2004_final$nbr_mort<-1


deces.2004_final$month <- as.numeric(format(deces.2004_final$datedeces, "%m"))




deces.2004_final$tranche_age <- cut(deces.2004_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2004_final$month <- as.numeric(format(deces.2004_final$datedeces, "%m"))


deces.2004_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2004_final,sum)

library(tidyr)

deces.2004_spread <- deces.2004_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2004_spread$`0-9`[is.na(deces.2004_spread$`0-9`)]<-0
deces.2004_spread$`10-19`[is.na(deces.2004_spread$`10-19`)]<-0
deces.2004_spread$`20-39`[is.na(deces.2004_spread$`20-39`)]<-0
deces.2004_spread$`40-59`[is.na(deces.2004_spread$`40-59`)]<-0
deces.2004_spread$`60-64`[is.na(deces.2004_spread$`60-64`)]<-0
deces.2004_spread$`65-69`[is.na(deces.2004_spread$`65-69`)]<-0
deces.2004_spread$`70-74`[is.na(deces.2004_spread$`70-74`)]<-0
deces.2004_spread$`75-79`[is.na(deces.2004_spread$`75-79`)]<-0
deces.2004_spread$`80+`[is.na(deces.2004_spread$`80+`)]<-0



#deces.2004_spread<-left_join(deces.2004_spread, deces.2004_spread)



deces.2004_spread$year<-2004


#fwrite(deces.2004_spread,"/fichier deces insee/décès travaillé/deces.2004_age_sexe.csv")




#################### partie 2 ######


RP_2004_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2004_age_sexe_final_2")

RP_2004_age_sexe_final_2<-RP_2004_age_sexe_final_2[,c(2:15)]

names(RP_2004_age_sexe_final_2)[names(RP_2004_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2004_age_sexe_final_2<-filter(RP_2004_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2004_mortalite<-left_join(deces.2004_spread,RP_2004_age_sexe_final_2)


Base_2004_mortalite$`75+`<-Base_2004_mortalite$`75-79`+Base_2004_mortalite$`80+`
Base_2004_mortalite$value_estimated_sum_75_plus_h_f<-Base_2004_mortalite$value_estimated_sum_75_79_h_f+Base_2004_mortalite$value_estimated_sum_80_plus_h_f


#Base_2004_mortalite$taux_mortalite_homme<-Base_2004_mortalite$Homme/Base_2004_mortalite$value_estimated_sum_homme

#Base_2004_mortalite$taux_mortalite_femme<-Base_2004_mortalite$Femme/Base_2004_mortalite$value_estimated_sum_femme

Base_2004_mortalite$taux_mortalite_0_9<-Base_2004_mortalite$`0-9`/Base_2004_mortalite$value_estimated_sum_0_9_h_f

Base_2004_mortalite$taux_mortalite_10_19<-Base_2004_mortalite$`10-19`/Base_2004_mortalite$value_estimated_sum_10_19_h_f

Base_2004_mortalite$taux_mortalite_20_39<-Base_2004_mortalite$`20-39`/Base_2004_mortalite$value_estimated_sum_20_39_h_f

Base_2004_mortalite$taux_mortalite_40_59<-Base_2004_mortalite$`40-59`/Base_2004_mortalite$value_estimated_sum_40_59_h_f

Base_2004_mortalite$taux_mortalite_60_64<-Base_2004_mortalite$`60-64`/Base_2004_mortalite$value_estimated_sum_60_64_h_f

Base_2004_mortalite$taux_mortalite_65_69<-Base_2004_mortalite$`65-69`/Base_2004_mortalite$value_estimated_sum_65_69_h_f

Base_2004_mortalite$taux_mortalite_70_74<-Base_2004_mortalite$`70-74`/Base_2004_mortalite$value_estimated_sum_70_74_h_f

Base_2004_mortalite$taux_mortalite_75_79<-Base_2004_mortalite$`75-79`/Base_2004_mortalite$value_estimated_sum_75_79_h_f

Base_2004_mortalite$taux_mortalite_75_plus<-Base_2004_mortalite$`75+`/Base_2004_mortalite$value_estimated_sum_75_plus_h_f

Base_2004_mortalite$taux_mortalite_80_plus<-Base_2004_mortalite$`80+`/Base_2004_mortalite$value_estimated_sum_80_plus_h_f

#Base_2004_mortalite$mort_total<-Base_2004_mortalite$Femme+Base_2004_mortalite$Homme

Base_2004_mortalite$mort_total<-Base_2004_mortalite$`0-9`+Base_2004_mortalite$`10-19`+Base_2004_mortalite$`20-39` +Base_2004_mortalite$`40-59` +Base_2004_mortalite$`60-64` +Base_2004_mortalite$`65-69` +Base_2004_mortalite$`70-74` +Base_2004_mortalite$`75+`



Base_2004_mortalite$taux_mortalite_total<-Base_2004_mortalite$mort_total/Base_2004_mortalite$value_estimated_population


Base_2004_mortalite<-filter(Base_2004_mortalite, Base_2004_mortalite$value_estimated_population>0)


Base_2004_mortalite<-Base_2004_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2004_mortalite<-filter(Base_2004_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2004_mortalite,"/heatwave and mortality code and data/new data/Base_2004_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2005 <- read_delim("fichier deces insee/deces-2005.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2005<-left_join(deces.2005, table_passage_bis)


deces.2005$COM<-ifelse(!is.na(deces.2005$COM_AP),deces.2005$COM_AP,deces.2005$lieudeces)

deces.2005$datedeces <- as.character(deces.2005$datedeces)
deces.2005$datedeces <- as.Date(deces.2005$datedeces, format = "%Y%m%d")


deces.2005$datenaiss <- as.character(deces.2005$datenaiss)
deces.2005$datenaiss <- as.Date(deces.2005$datenaiss, format = "%Y%m%d")


deces.2005$year <- as.numeric(format(deces.2005$datedeces, "%Y"))
deces.2005<-filter(deces.2005, deces.2005$year==2005)
deces.2005<-deces.2005[,-12]

library(lubridate)
deces.2005$age <- as.period(interval(deces.2005$datenaiss , deces.2005$datedeces ))
deces.2005$age_years <- year(deces.2005$age)

library(readr)


deces.2005$sexe<-as.character(deces.2005$sexe)

deces.2005$SEX<-ifelse(deces.2005$sexe=="1","Homme","Femme")

deces.2005_final<-deces.2005[,c("COM","datedeces","age_years","SEX")]

deces.2005_final$nbr_mort<-1


deces.2005_final$month <- as.numeric(format(deces.2005_final$datedeces, "%m"))




deces.2005_final$tranche_age <- cut(deces.2005_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2005_final$month <- as.numeric(format(deces.2005_final$datedeces, "%m"))


deces.2005_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2005_final,sum)

library(tidyr)

deces.2005_spread <- deces.2005_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2005_spread$`0-9`[is.na(deces.2005_spread$`0-9`)]<-0
deces.2005_spread$`10-19`[is.na(deces.2005_spread$`10-19`)]<-0
deces.2005_spread$`20-39`[is.na(deces.2005_spread$`20-39`)]<-0
deces.2005_spread$`40-59`[is.na(deces.2005_spread$`40-59`)]<-0
deces.2005_spread$`60-64`[is.na(deces.2005_spread$`60-64`)]<-0
deces.2005_spread$`65-69`[is.na(deces.2005_spread$`65-69`)]<-0
deces.2005_spread$`70-74`[is.na(deces.2005_spread$`70-74`)]<-0
deces.2005_spread$`75-79`[is.na(deces.2005_spread$`75-79`)]<-0
deces.2005_spread$`80+`[is.na(deces.2005_spread$`80+`)]<-0



#deces.2005_spread<-left_join(deces.2005_spread, deces.2005_spread)



deces.2005_spread$year<-2005


#fwrite(deces.2005_spread,"/fichier deces insee/décès travaillé/deces.2005_age_sexe.csv")




#################### partie 2 ######


RP_2005_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2005_age_sexe_final_2")

RP_2005_age_sexe_final_2<-RP_2005_age_sexe_final_2[,c(2:15)]

names(RP_2005_age_sexe_final_2)[names(RP_2005_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2005_age_sexe_final_2<-filter(RP_2005_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2005_mortalite<-left_join(deces.2005_spread,RP_2005_age_sexe_final_2)


Base_2005_mortalite$`75+`<-Base_2005_mortalite$`75-79`+Base_2005_mortalite$`80+`
Base_2005_mortalite$value_estimated_sum_75_plus_h_f<-Base_2005_mortalite$value_estimated_sum_75_79_h_f+Base_2005_mortalite$value_estimated_sum_80_plus_h_f


#Base_2005_mortalite$taux_mortalite_homme<-Base_2005_mortalite$Homme/Base_2005_mortalite$value_estimated_sum_homme

#Base_2005_mortalite$taux_mortalite_femme<-Base_2005_mortalite$Femme/Base_2005_mortalite$value_estimated_sum_femme

Base_2005_mortalite$taux_mortalite_0_9<-Base_2005_mortalite$`0-9`/Base_2005_mortalite$value_estimated_sum_0_9_h_f

Base_2005_mortalite$taux_mortalite_10_19<-Base_2005_mortalite$`10-19`/Base_2005_mortalite$value_estimated_sum_10_19_h_f

Base_2005_mortalite$taux_mortalite_20_39<-Base_2005_mortalite$`20-39`/Base_2005_mortalite$value_estimated_sum_20_39_h_f

Base_2005_mortalite$taux_mortalite_40_59<-Base_2005_mortalite$`40-59`/Base_2005_mortalite$value_estimated_sum_40_59_h_f

Base_2005_mortalite$taux_mortalite_60_64<-Base_2005_mortalite$`60-64`/Base_2005_mortalite$value_estimated_sum_60_64_h_f

Base_2005_mortalite$taux_mortalite_65_69<-Base_2005_mortalite$`65-69`/Base_2005_mortalite$value_estimated_sum_65_69_h_f

Base_2005_mortalite$taux_mortalite_70_74<-Base_2005_mortalite$`70-74`/Base_2005_mortalite$value_estimated_sum_70_74_h_f

Base_2005_mortalite$taux_mortalite_75_79<-Base_2005_mortalite$`75-79`/Base_2005_mortalite$value_estimated_sum_75_79_h_f

Base_2005_mortalite$taux_mortalite_75_plus<-Base_2005_mortalite$`75+`/Base_2005_mortalite$value_estimated_sum_75_plus_h_f

Base_2005_mortalite$taux_mortalite_80_plus<-Base_2005_mortalite$`80+`/Base_2005_mortalite$value_estimated_sum_80_plus_h_f

#Base_2005_mortalite$mort_total<-Base_2005_mortalite$Femme+Base_2005_mortalite$Homme

Base_2005_mortalite$mort_total<-Base_2005_mortalite$`0-9`+Base_2005_mortalite$`10-19`+Base_2005_mortalite$`20-39` +Base_2005_mortalite$`40-59` +Base_2005_mortalite$`60-64` +Base_2005_mortalite$`65-69` +Base_2005_mortalite$`70-74` +Base_2005_mortalite$`75+`



Base_2005_mortalite$taux_mortalite_total<-Base_2005_mortalite$mort_total/Base_2005_mortalite$value_estimated_population


Base_2005_mortalite<-filter(Base_2005_mortalite, Base_2005_mortalite$value_estimated_population>0)


Base_2005_mortalite<-Base_2005_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2005_mortalite<-filter(Base_2005_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2005_mortalite,"/heatwave and mortality code and data/new data/Base_2005_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2006 <- read_delim("fichier deces insee/deces-2006.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2006<-left_join(deces.2006, table_passage_bis)


deces.2006$COM<-ifelse(!is.na(deces.2006$COM_AP),deces.2006$COM_AP,deces.2006$lieudeces)

deces.2006$datedeces <- as.character(deces.2006$datedeces)
deces.2006$datedeces <- as.Date(deces.2006$datedeces, format = "%Y%m%d")


deces.2006$datenaiss <- as.character(deces.2006$datenaiss)
deces.2006$datenaiss <- as.Date(deces.2006$datenaiss, format = "%Y%m%d")


deces.2006$year <- as.numeric(format(deces.2006$datedeces, "%Y"))
deces.2006<-filter(deces.2006, deces.2006$year==2006)
deces.2006<-deces.2006[,-12]

library(lubridate)
deces.2006$age <- as.period(interval(deces.2006$datenaiss , deces.2006$datedeces ))
deces.2006$age_years <- year(deces.2006$age)

library(readr)


deces.2006$sexe<-as.character(deces.2006$sexe)

deces.2006$SEX<-ifelse(deces.2006$sexe=="1","Homme","Femme")

deces.2006_final<-deces.2006[,c("COM","datedeces","age_years","SEX")]

deces.2006_final$nbr_mort<-1


deces.2006_final$month <- as.numeric(format(deces.2006_final$datedeces, "%m"))




deces.2006_final$tranche_age <- cut(deces.2006_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2006_final$month <- as.numeric(format(deces.2006_final$datedeces, "%m"))


deces.2006_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2006_final,sum)

library(tidyr)

deces.2006_spread <- deces.2006_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2006_spread$`0-9`[is.na(deces.2006_spread$`0-9`)]<-0
deces.2006_spread$`10-19`[is.na(deces.2006_spread$`10-19`)]<-0
deces.2006_spread$`20-39`[is.na(deces.2006_spread$`20-39`)]<-0
deces.2006_spread$`40-59`[is.na(deces.2006_spread$`40-59`)]<-0
deces.2006_spread$`60-64`[is.na(deces.2006_spread$`60-64`)]<-0
deces.2006_spread$`65-69`[is.na(deces.2006_spread$`65-69`)]<-0
deces.2006_spread$`70-74`[is.na(deces.2006_spread$`70-74`)]<-0
deces.2006_spread$`75-79`[is.na(deces.2006_spread$`75-79`)]<-0
deces.2006_spread$`80+`[is.na(deces.2006_spread$`80+`)]<-0



#deces.2006_spread<-left_join(deces.2006_spread, deces.2006_spread)



deces.2006_spread$year<-2006


#fwrite(deces.2006_spread,"/fichier deces insee/décès travaillé/deces.2006_age_sexe.csv")




#################### partie 2 ######


RP_2006_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2006_age_sexe_final_2")

RP_2006_age_sexe_final_2<-RP_2006_age_sexe_final_2[,c(2:15)]

names(RP_2006_age_sexe_final_2)[names(RP_2006_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2006_age_sexe_final_2<-filter(RP_2006_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2006_mortalite<-left_join(deces.2006_spread,RP_2006_age_sexe_final_2)


Base_2006_mortalite$`75+`<-Base_2006_mortalite$`75-79`+Base_2006_mortalite$`80+`
Base_2006_mortalite$value_estimated_sum_75_plus_h_f<-Base_2006_mortalite$value_estimated_sum_75_79_h_f+Base_2006_mortalite$value_estimated_sum_80_plus_h_f


#Base_2006_mortalite$taux_mortalite_homme<-Base_2006_mortalite$Homme/Base_2006_mortalite$value_estimated_sum_homme

#Base_2006_mortalite$taux_mortalite_femme<-Base_2006_mortalite$Femme/Base_2006_mortalite$value_estimated_sum_femme

Base_2006_mortalite$taux_mortalite_0_9<-Base_2006_mortalite$`0-9`/Base_2006_mortalite$value_estimated_sum_0_9_h_f

Base_2006_mortalite$taux_mortalite_10_19<-Base_2006_mortalite$`10-19`/Base_2006_mortalite$value_estimated_sum_10_19_h_f

Base_2006_mortalite$taux_mortalite_20_39<-Base_2006_mortalite$`20-39`/Base_2006_mortalite$value_estimated_sum_20_39_h_f

Base_2006_mortalite$taux_mortalite_40_59<-Base_2006_mortalite$`40-59`/Base_2006_mortalite$value_estimated_sum_40_59_h_f

Base_2006_mortalite$taux_mortalite_60_64<-Base_2006_mortalite$`60-64`/Base_2006_mortalite$value_estimated_sum_60_64_h_f

Base_2006_mortalite$taux_mortalite_65_69<-Base_2006_mortalite$`65-69`/Base_2006_mortalite$value_estimated_sum_65_69_h_f

Base_2006_mortalite$taux_mortalite_70_74<-Base_2006_mortalite$`70-74`/Base_2006_mortalite$value_estimated_sum_70_74_h_f

Base_2006_mortalite$taux_mortalite_75_79<-Base_2006_mortalite$`75-79`/Base_2006_mortalite$value_estimated_sum_75_79_h_f

Base_2006_mortalite$taux_mortalite_75_plus<-Base_2006_mortalite$`75+`/Base_2006_mortalite$value_estimated_sum_75_plus_h_f

Base_2006_mortalite$taux_mortalite_80_plus<-Base_2006_mortalite$`80+`/Base_2006_mortalite$value_estimated_sum_80_plus_h_f

#Base_2006_mortalite$mort_total<-Base_2006_mortalite$Femme+Base_2006_mortalite$Homme

Base_2006_mortalite$mort_total<-Base_2006_mortalite$`0-9`+Base_2006_mortalite$`10-19`+Base_2006_mortalite$`20-39` +Base_2006_mortalite$`40-59` +Base_2006_mortalite$`60-64` +Base_2006_mortalite$`65-69` +Base_2006_mortalite$`70-74` +Base_2006_mortalite$`75+`



Base_2006_mortalite$taux_mortalite_total<-Base_2006_mortalite$mort_total/Base_2006_mortalite$value_estimated_population


Base_2006_mortalite<-filter(Base_2006_mortalite, Base_2006_mortalite$value_estimated_population>0)


Base_2006_mortalite<-Base_2006_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2006_mortalite<-filter(Base_2006_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2006_mortalite,"/heatwave and mortality code and data/new data/Base_2006_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2007 <- read_delim("fichier deces insee/deces-2007.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2007<-left_join(deces.2007, table_passage_bis)


deces.2007$COM<-ifelse(!is.na(deces.2007$COM_AP),deces.2007$COM_AP,deces.2007$lieudeces)

deces.2007$datedeces <- as.character(deces.2007$datedeces)
deces.2007$datedeces <- as.Date(deces.2007$datedeces, format = "%Y%m%d")


deces.2007$datenaiss <- as.character(deces.2007$datenaiss)
deces.2007$datenaiss <- as.Date(deces.2007$datenaiss, format = "%Y%m%d")


deces.2007$year <- as.numeric(format(deces.2007$datedeces, "%Y"))
deces.2007<-filter(deces.2007, deces.2007$year==2007)
deces.2007<-deces.2007[,-12]

library(lubridate)
deces.2007$age <- as.period(interval(deces.2007$datenaiss , deces.2007$datedeces ))
deces.2007$age_years <- year(deces.2007$age)

library(readr)


deces.2007$sexe<-as.character(deces.2007$sexe)

deces.2007$SEX<-ifelse(deces.2007$sexe=="1","Homme","Femme")

deces.2007_final<-deces.2007[,c("COM","datedeces","age_years","SEX")]

deces.2007_final$nbr_mort<-1


deces.2007_final$month <- as.numeric(format(deces.2007_final$datedeces, "%m"))




deces.2007_final$tranche_age <- cut(deces.2007_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2007_final$month <- as.numeric(format(deces.2007_final$datedeces, "%m"))


deces.2007_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2007_final,sum)

library(tidyr)

deces.2007_spread <- deces.2007_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2007_spread$`0-9`[is.na(deces.2007_spread$`0-9`)]<-0
deces.2007_spread$`10-19`[is.na(deces.2007_spread$`10-19`)]<-0
deces.2007_spread$`20-39`[is.na(deces.2007_spread$`20-39`)]<-0
deces.2007_spread$`40-59`[is.na(deces.2007_spread$`40-59`)]<-0
deces.2007_spread$`60-64`[is.na(deces.2007_spread$`60-64`)]<-0
deces.2007_spread$`65-69`[is.na(deces.2007_spread$`65-69`)]<-0
deces.2007_spread$`70-74`[is.na(deces.2007_spread$`70-74`)]<-0
deces.2007_spread$`75-79`[is.na(deces.2007_spread$`75-79`)]<-0
deces.2007_spread$`80+`[is.na(deces.2007_spread$`80+`)]<-0



#deces.2007_spread<-left_join(deces.2007_spread, deces.2007_spread)



deces.2007_spread$year<-2007


#fwrite(deces.2007_spread,"/fichier deces insee/décès travaillé/deces.2007_age_sexe.csv")




#################### partie 2 ######


RP_2007_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2007_age_sexe_final_2")

RP_2007_age_sexe_final_2<-RP_2007_age_sexe_final_2[,c(2:15)]

names(RP_2007_age_sexe_final_2)[names(RP_2007_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2007_age_sexe_final_2<-filter(RP_2007_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2007_mortalite<-left_join(deces.2007_spread,RP_2007_age_sexe_final_2)


Base_2007_mortalite$`75+`<-Base_2007_mortalite$`75-79`+Base_2007_mortalite$`80+`
Base_2007_mortalite$value_estimated_sum_75_plus_h_f<-Base_2007_mortalite$value_estimated_sum_75_79_h_f+Base_2007_mortalite$value_estimated_sum_80_plus_h_f


#Base_2007_mortalite$taux_mortalite_homme<-Base_2007_mortalite$Homme/Base_2007_mortalite$value_estimated_sum_homme

#Base_2007_mortalite$taux_mortalite_femme<-Base_2007_mortalite$Femme/Base_2007_mortalite$value_estimated_sum_femme

Base_2007_mortalite$taux_mortalite_0_9<-Base_2007_mortalite$`0-9`/Base_2007_mortalite$value_estimated_sum_0_9_h_f

Base_2007_mortalite$taux_mortalite_10_19<-Base_2007_mortalite$`10-19`/Base_2007_mortalite$value_estimated_sum_10_19_h_f

Base_2007_mortalite$taux_mortalite_20_39<-Base_2007_mortalite$`20-39`/Base_2007_mortalite$value_estimated_sum_20_39_h_f

Base_2007_mortalite$taux_mortalite_40_59<-Base_2007_mortalite$`40-59`/Base_2007_mortalite$value_estimated_sum_40_59_h_f

Base_2007_mortalite$taux_mortalite_60_64<-Base_2007_mortalite$`60-64`/Base_2007_mortalite$value_estimated_sum_60_64_h_f

Base_2007_mortalite$taux_mortalite_65_69<-Base_2007_mortalite$`65-69`/Base_2007_mortalite$value_estimated_sum_65_69_h_f

Base_2007_mortalite$taux_mortalite_70_74<-Base_2007_mortalite$`70-74`/Base_2007_mortalite$value_estimated_sum_70_74_h_f

Base_2007_mortalite$taux_mortalite_75_79<-Base_2007_mortalite$`75-79`/Base_2007_mortalite$value_estimated_sum_75_79_h_f

Base_2007_mortalite$taux_mortalite_75_plus<-Base_2007_mortalite$`75+`/Base_2007_mortalite$value_estimated_sum_75_plus_h_f

Base_2007_mortalite$taux_mortalite_80_plus<-Base_2007_mortalite$`80+`/Base_2007_mortalite$value_estimated_sum_80_plus_h_f

#Base_2007_mortalite$mort_total<-Base_2007_mortalite$Femme+Base_2007_mortalite$Homme

Base_2007_mortalite$mort_total<-Base_2007_mortalite$`0-9`+Base_2007_mortalite$`10-19`+Base_2007_mortalite$`20-39` +Base_2007_mortalite$`40-59` +Base_2007_mortalite$`60-64` +Base_2007_mortalite$`65-69` +Base_2007_mortalite$`70-74` +Base_2007_mortalite$`75+`



Base_2007_mortalite$taux_mortalite_total<-Base_2007_mortalite$mort_total/Base_2007_mortalite$value_estimated_population


Base_2007_mortalite<-filter(Base_2007_mortalite, Base_2007_mortalite$value_estimated_population>0)


Base_2007_mortalite<-Base_2007_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2007_mortalite<-filter(Base_2007_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2007_mortalite,"/heatwave and mortality code and data/new data/Base_2007_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2008 <- read_delim("fichier deces insee/deces-2008.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2008<-left_join(deces.2008, table_passage_bis)


deces.2008$COM<-ifelse(!is.na(deces.2008$COM_AP),deces.2008$COM_AP,deces.2008$lieudeces)

deces.2008$datedeces <- as.character(deces.2008$datedeces)
deces.2008$datedeces <- as.Date(deces.2008$datedeces, format = "%Y%m%d")


deces.2008$datenaiss <- as.character(deces.2008$datenaiss)
deces.2008$datenaiss <- as.Date(deces.2008$datenaiss, format = "%Y%m%d")


deces.2008$year <- as.numeric(format(deces.2008$datedeces, "%Y"))
deces.2008<-filter(deces.2008, deces.2008$year==2008)
deces.2008<-deces.2008[,-12]

library(lubridate)
deces.2008$age <- as.period(interval(deces.2008$datenaiss , deces.2008$datedeces ))
deces.2008$age_years <- year(deces.2008$age)

library(readr)


deces.2008$sexe<-as.character(deces.2008$sexe)

deces.2008$SEX<-ifelse(deces.2008$sexe=="1","Homme","Femme")

deces.2008_final<-deces.2008[,c("COM","datedeces","age_years","SEX")]

deces.2008_final$nbr_mort<-1


deces.2008_final$month <- as.numeric(format(deces.2008_final$datedeces, "%m"))




deces.2008_final$tranche_age <- cut(deces.2008_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2008_final$month <- as.numeric(format(deces.2008_final$datedeces, "%m"))


deces.2008_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2008_final,sum)

library(tidyr)

deces.2008_spread <- deces.2008_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2008_spread$`0-9`[is.na(deces.2008_spread$`0-9`)]<-0
deces.2008_spread$`10-19`[is.na(deces.2008_spread$`10-19`)]<-0
deces.2008_spread$`20-39`[is.na(deces.2008_spread$`20-39`)]<-0
deces.2008_spread$`40-59`[is.na(deces.2008_spread$`40-59`)]<-0
deces.2008_spread$`60-64`[is.na(deces.2008_spread$`60-64`)]<-0
deces.2008_spread$`65-69`[is.na(deces.2008_spread$`65-69`)]<-0
deces.2008_spread$`70-74`[is.na(deces.2008_spread$`70-74`)]<-0
deces.2008_spread$`75-79`[is.na(deces.2008_spread$`75-79`)]<-0
deces.2008_spread$`80+`[is.na(deces.2008_spread$`80+`)]<-0



#deces.2008_spread<-left_join(deces.2008_spread, deces.2008_spread)



deces.2008_spread$year<-2008


#fwrite(deces.2008_spread,"/fichier deces insee/décès travaillé/deces.2008_age_sexe.csv")




#################### partie 2 ######


RP_2008_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2008_age_sexe_final_2")

RP_2008_age_sexe_final_2<-RP_2008_age_sexe_final_2[,c(2:15)]

names(RP_2008_age_sexe_final_2)[names(RP_2008_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2008_age_sexe_final_2<-filter(RP_2008_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2008_mortalite<-left_join(deces.2008_spread,RP_2008_age_sexe_final_2)


Base_2008_mortalite$`75+`<-Base_2008_mortalite$`75-79`+Base_2008_mortalite$`80+`
Base_2008_mortalite$value_estimated_sum_75_plus_h_f<-Base_2008_mortalite$value_estimated_sum_75_79_h_f+Base_2008_mortalite$value_estimated_sum_80_plus_h_f


#Base_2008_mortalite$taux_mortalite_homme<-Base_2008_mortalite$Homme/Base_2008_mortalite$value_estimated_sum_homme

#Base_2008_mortalite$taux_mortalite_femme<-Base_2008_mortalite$Femme/Base_2008_mortalite$value_estimated_sum_femme

Base_2008_mortalite$taux_mortalite_0_9<-Base_2008_mortalite$`0-9`/Base_2008_mortalite$value_estimated_sum_0_9_h_f

Base_2008_mortalite$taux_mortalite_10_19<-Base_2008_mortalite$`10-19`/Base_2008_mortalite$value_estimated_sum_10_19_h_f

Base_2008_mortalite$taux_mortalite_20_39<-Base_2008_mortalite$`20-39`/Base_2008_mortalite$value_estimated_sum_20_39_h_f

Base_2008_mortalite$taux_mortalite_40_59<-Base_2008_mortalite$`40-59`/Base_2008_mortalite$value_estimated_sum_40_59_h_f

Base_2008_mortalite$taux_mortalite_60_64<-Base_2008_mortalite$`60-64`/Base_2008_mortalite$value_estimated_sum_60_64_h_f

Base_2008_mortalite$taux_mortalite_65_69<-Base_2008_mortalite$`65-69`/Base_2008_mortalite$value_estimated_sum_65_69_h_f

Base_2008_mortalite$taux_mortalite_70_74<-Base_2008_mortalite$`70-74`/Base_2008_mortalite$value_estimated_sum_70_74_h_f

Base_2008_mortalite$taux_mortalite_75_79<-Base_2008_mortalite$`75-79`/Base_2008_mortalite$value_estimated_sum_75_79_h_f

Base_2008_mortalite$taux_mortalite_75_plus<-Base_2008_mortalite$`75+`/Base_2008_mortalite$value_estimated_sum_75_plus_h_f

Base_2008_mortalite$taux_mortalite_80_plus<-Base_2008_mortalite$`80+`/Base_2008_mortalite$value_estimated_sum_80_plus_h_f

#Base_2008_mortalite$mort_total<-Base_2008_mortalite$Femme+Base_2008_mortalite$Homme

Base_2008_mortalite$mort_total<-Base_2008_mortalite$`0-9`+Base_2008_mortalite$`10-19`+Base_2008_mortalite$`20-39` +Base_2008_mortalite$`40-59` +Base_2008_mortalite$`60-64` +Base_2008_mortalite$`65-69` +Base_2008_mortalite$`70-74` +Base_2008_mortalite$`75+`



Base_2008_mortalite$taux_mortalite_total<-Base_2008_mortalite$mort_total/Base_2008_mortalite$value_estimated_population


Base_2008_mortalite<-filter(Base_2008_mortalite, Base_2008_mortalite$value_estimated_population>0)


Base_2008_mortalite<-Base_2008_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2008_mortalite<-filter(Base_2008_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2008_mortalite,"/heatwave and mortality code and data/new data/Base_2008_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2009 <- read_delim("fichier deces insee/deces-2009.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2009<-left_join(deces.2009, table_passage_bis)


deces.2009$COM<-ifelse(!is.na(deces.2009$COM_AP),deces.2009$COM_AP,deces.2009$lieudeces)

deces.2009$datedeces <- as.character(deces.2009$datedeces)
deces.2009$datedeces <- as.Date(deces.2009$datedeces, format = "%Y%m%d")


deces.2009$datenaiss <- as.character(deces.2009$datenaiss)
deces.2009$datenaiss <- as.Date(deces.2009$datenaiss, format = "%Y%m%d")


deces.2009$year <- as.numeric(format(deces.2009$datedeces, "%Y"))
deces.2009<-filter(deces.2009, deces.2009$year==2009)
deces.2009<-deces.2009[,-12]

library(lubridate)
deces.2009$age <- as.period(interval(deces.2009$datenaiss , deces.2009$datedeces ))
deces.2009$age_years <- year(deces.2009$age)

library(readr)


deces.2009$sexe<-as.character(deces.2009$sexe)

deces.2009$SEX<-ifelse(deces.2009$sexe=="1","Homme","Femme")

deces.2009_final<-deces.2009[,c("COM","datedeces","age_years","SEX")]

deces.2009_final$nbr_mort<-1


deces.2009_final$month <- as.numeric(format(deces.2009_final$datedeces, "%m"))




deces.2009_final$tranche_age <- cut(deces.2009_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2009_final$month <- as.numeric(format(deces.2009_final$datedeces, "%m"))


deces.2009_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2009_final,sum)

library(tidyr)

deces.2009_spread <- deces.2009_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2009_spread$`0-9`[is.na(deces.2009_spread$`0-9`)]<-0
deces.2009_spread$`10-19`[is.na(deces.2009_spread$`10-19`)]<-0
deces.2009_spread$`20-39`[is.na(deces.2009_spread$`20-39`)]<-0
deces.2009_spread$`40-59`[is.na(deces.2009_spread$`40-59`)]<-0
deces.2009_spread$`60-64`[is.na(deces.2009_spread$`60-64`)]<-0
deces.2009_spread$`65-69`[is.na(deces.2009_spread$`65-69`)]<-0
deces.2009_spread$`70-74`[is.na(deces.2009_spread$`70-74`)]<-0
deces.2009_spread$`75-79`[is.na(deces.2009_spread$`75-79`)]<-0
deces.2009_spread$`80+`[is.na(deces.2009_spread$`80+`)]<-0



#deces.2009_spread<-left_join(deces.2009_spread, deces.2009_spread)



deces.2009_spread$year<-2009


#fwrite(deces.2009_spread,"/fichier deces insee/décès travaillé/deces.2009_age_sexe.csv")




#################### partie 2 ######


RP_2009_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2009_age_sexe_final_2")

RP_2009_age_sexe_final_2<-RP_2009_age_sexe_final_2[,c(2:15)]

names(RP_2009_age_sexe_final_2)[names(RP_2009_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2009_age_sexe_final_2<-filter(RP_2009_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2009_mortalite<-left_join(deces.2009_spread,RP_2009_age_sexe_final_2)


Base_2009_mortalite$`75+`<-Base_2009_mortalite$`75-79`+Base_2009_mortalite$`80+`
Base_2009_mortalite$value_estimated_sum_75_plus_h_f<-Base_2009_mortalite$value_estimated_sum_75_79_h_f+Base_2009_mortalite$value_estimated_sum_80_plus_h_f


#Base_2009_mortalite$taux_mortalite_homme<-Base_2009_mortalite$Homme/Base_2009_mortalite$value_estimated_sum_homme

#Base_2009_mortalite$taux_mortalite_femme<-Base_2009_mortalite$Femme/Base_2009_mortalite$value_estimated_sum_femme

Base_2009_mortalite$taux_mortalite_0_9<-Base_2009_mortalite$`0-9`/Base_2009_mortalite$value_estimated_sum_0_9_h_f

Base_2009_mortalite$taux_mortalite_10_19<-Base_2009_mortalite$`10-19`/Base_2009_mortalite$value_estimated_sum_10_19_h_f

Base_2009_mortalite$taux_mortalite_20_39<-Base_2009_mortalite$`20-39`/Base_2009_mortalite$value_estimated_sum_20_39_h_f

Base_2009_mortalite$taux_mortalite_40_59<-Base_2009_mortalite$`40-59`/Base_2009_mortalite$value_estimated_sum_40_59_h_f

Base_2009_mortalite$taux_mortalite_60_64<-Base_2009_mortalite$`60-64`/Base_2009_mortalite$value_estimated_sum_60_64_h_f

Base_2009_mortalite$taux_mortalite_65_69<-Base_2009_mortalite$`65-69`/Base_2009_mortalite$value_estimated_sum_65_69_h_f

Base_2009_mortalite$taux_mortalite_70_74<-Base_2009_mortalite$`70-74`/Base_2009_mortalite$value_estimated_sum_70_74_h_f

Base_2009_mortalite$taux_mortalite_75_79<-Base_2009_mortalite$`75-79`/Base_2009_mortalite$value_estimated_sum_75_79_h_f

Base_2009_mortalite$taux_mortalite_75_plus<-Base_2009_mortalite$`75+`/Base_2009_mortalite$value_estimated_sum_75_plus_h_f

Base_2009_mortalite$taux_mortalite_80_plus<-Base_2009_mortalite$`80+`/Base_2009_mortalite$value_estimated_sum_80_plus_h_f

#Base_2009_mortalite$mort_total<-Base_2009_mortalite$Femme+Base_2009_mortalite$Homme

Base_2009_mortalite$mort_total<-Base_2009_mortalite$`0-9`+Base_2009_mortalite$`10-19`+Base_2009_mortalite$`20-39` +Base_2009_mortalite$`40-59` +Base_2009_mortalite$`60-64` +Base_2009_mortalite$`65-69` +Base_2009_mortalite$`70-74` +Base_2009_mortalite$`75+`



Base_2009_mortalite$taux_mortalite_total<-Base_2009_mortalite$mort_total/Base_2009_mortalite$value_estimated_population


Base_2009_mortalite<-filter(Base_2009_mortalite, Base_2009_mortalite$value_estimated_population>0)


Base_2009_mortalite<-Base_2009_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2009_mortalite<-filter(Base_2009_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2009_mortalite,"/heatwave and mortality code and data/new data/Base_2009_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2010 <- read_delim("fichier deces insee/deces-2010.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2010<-left_join(deces.2010, table_passage_bis)


deces.2010$COM<-ifelse(!is.na(deces.2010$COM_AP),deces.2010$COM_AP,deces.2010$lieudeces)

deces.2010$datedeces <- as.character(deces.2010$datedeces)
deces.2010$datedeces <- as.Date(deces.2010$datedeces, format = "%Y%m%d")


deces.2010$datenaiss <- as.character(deces.2010$datenaiss)
deces.2010$datenaiss <- as.Date(deces.2010$datenaiss, format = "%Y%m%d")


deces.2010$year <- as.numeric(format(deces.2010$datedeces, "%Y"))
deces.2010<-filter(deces.2010, deces.2010$year==2010)
deces.2010<-deces.2010[,-12]

library(lubridate)
deces.2010$age <- as.period(interval(deces.2010$datenaiss , deces.2010$datedeces ))
deces.2010$age_years <- year(deces.2010$age)

library(readr)


deces.2010$sexe<-as.character(deces.2010$sexe)

deces.2010$SEX<-ifelse(deces.2010$sexe=="1","Homme","Femme")

deces.2010_final<-deces.2010[,c("COM","datedeces","age_years","SEX")]

deces.2010_final$nbr_mort<-1


deces.2010_final$month <- as.numeric(format(deces.2010_final$datedeces, "%m"))




deces.2010_final$tranche_age <- cut(deces.2010_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2010_final$month <- as.numeric(format(deces.2010_final$datedeces, "%m"))


deces.2010_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2010_final,sum)

library(tidyr)

deces.2010_spread <- deces.2010_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2010_spread$`0-9`[is.na(deces.2010_spread$`0-9`)]<-0
deces.2010_spread$`10-19`[is.na(deces.2010_spread$`10-19`)]<-0
deces.2010_spread$`20-39`[is.na(deces.2010_spread$`20-39`)]<-0
deces.2010_spread$`40-59`[is.na(deces.2010_spread$`40-59`)]<-0
deces.2010_spread$`60-64`[is.na(deces.2010_spread$`60-64`)]<-0
deces.2010_spread$`65-69`[is.na(deces.2010_spread$`65-69`)]<-0
deces.2010_spread$`70-74`[is.na(deces.2010_spread$`70-74`)]<-0
deces.2010_spread$`75-79`[is.na(deces.2010_spread$`75-79`)]<-0
deces.2010_spread$`80+`[is.na(deces.2010_spread$`80+`)]<-0



#deces.2010_spread<-left_join(deces.2010_spread, deces.2010_spread)



deces.2010_spread$year<-2010


#fwrite(deces.2010_spread,"/fichier deces insee/décès travaillé/deces.2010_age_sexe.csv")




#################### partie 2 ######


RP_2010_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2010_age_sexe_final_2")

RP_2010_age_sexe_final_2<-RP_2010_age_sexe_final_2[,c(2:15)]

names(RP_2010_age_sexe_final_2)[names(RP_2010_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2010_age_sexe_final_2<-filter(RP_2010_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2010_mortalite<-left_join(deces.2010_spread,RP_2010_age_sexe_final_2)


Base_2010_mortalite$`75+`<-Base_2010_mortalite$`75-79`+Base_2010_mortalite$`80+`
Base_2010_mortalite$value_estimated_sum_75_plus_h_f<-Base_2010_mortalite$value_estimated_sum_75_79_h_f+Base_2010_mortalite$value_estimated_sum_80_plus_h_f


#Base_2010_mortalite$taux_mortalite_homme<-Base_2010_mortalite$Homme/Base_2010_mortalite$value_estimated_sum_homme

#Base_2010_mortalite$taux_mortalite_femme<-Base_2010_mortalite$Femme/Base_2010_mortalite$value_estimated_sum_femme

Base_2010_mortalite$taux_mortalite_0_9<-Base_2010_mortalite$`0-9`/Base_2010_mortalite$value_estimated_sum_0_9_h_f

Base_2010_mortalite$taux_mortalite_10_19<-Base_2010_mortalite$`10-19`/Base_2010_mortalite$value_estimated_sum_10_19_h_f

Base_2010_mortalite$taux_mortalite_20_39<-Base_2010_mortalite$`20-39`/Base_2010_mortalite$value_estimated_sum_20_39_h_f

Base_2010_mortalite$taux_mortalite_40_59<-Base_2010_mortalite$`40-59`/Base_2010_mortalite$value_estimated_sum_40_59_h_f

Base_2010_mortalite$taux_mortalite_60_64<-Base_2010_mortalite$`60-64`/Base_2010_mortalite$value_estimated_sum_60_64_h_f

Base_2010_mortalite$taux_mortalite_65_69<-Base_2010_mortalite$`65-69`/Base_2010_mortalite$value_estimated_sum_65_69_h_f

Base_2010_mortalite$taux_mortalite_70_74<-Base_2010_mortalite$`70-74`/Base_2010_mortalite$value_estimated_sum_70_74_h_f

Base_2010_mortalite$taux_mortalite_75_79<-Base_2010_mortalite$`75-79`/Base_2010_mortalite$value_estimated_sum_75_79_h_f

Base_2010_mortalite$taux_mortalite_75_plus<-Base_2010_mortalite$`75+`/Base_2010_mortalite$value_estimated_sum_75_plus_h_f

Base_2010_mortalite$taux_mortalite_80_plus<-Base_2010_mortalite$`80+`/Base_2010_mortalite$value_estimated_sum_80_plus_h_f

#Base_2010_mortalite$mort_total<-Base_2010_mortalite$Femme+Base_2010_mortalite$Homme

Base_2010_mortalite$mort_total<-Base_2010_mortalite$`0-9`+Base_2010_mortalite$`10-19`+Base_2010_mortalite$`20-39` +Base_2010_mortalite$`40-59` +Base_2010_mortalite$`60-64` +Base_2010_mortalite$`65-69` +Base_2010_mortalite$`70-74` +Base_2010_mortalite$`75+`



Base_2010_mortalite$taux_mortalite_total<-Base_2010_mortalite$mort_total/Base_2010_mortalite$value_estimated_population


Base_2010_mortalite<-filter(Base_2010_mortalite, Base_2010_mortalite$value_estimated_population>0)


Base_2010_mortalite<-Base_2010_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2010_mortalite<-filter(Base_2010_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2010_mortalite,"/heatwave and mortality code and data/new data/Base_2010_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2011 <- read_delim("fichier deces insee/deces-2011.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2011<-left_join(deces.2011, table_passage_bis)


deces.2011$COM<-ifelse(!is.na(deces.2011$COM_AP),deces.2011$COM_AP,deces.2011$lieudeces)

deces.2011$datedeces <- as.character(deces.2011$datedeces)
deces.2011$datedeces <- as.Date(deces.2011$datedeces, format = "%Y%m%d")


deces.2011$datenaiss <- as.character(deces.2011$datenaiss)
deces.2011$datenaiss <- as.Date(deces.2011$datenaiss, format = "%Y%m%d")


deces.2011$year <- as.numeric(format(deces.2011$datedeces, "%Y"))
deces.2011<-filter(deces.2011, deces.2011$year==2011)
deces.2011<-deces.2011[,-12]

library(lubridate)
deces.2011$age <- as.period(interval(deces.2011$datenaiss , deces.2011$datedeces ))
deces.2011$age_years <- year(deces.2011$age)

library(readr)


deces.2011$sexe<-as.character(deces.2011$sexe)

deces.2011$SEX<-ifelse(deces.2011$sexe=="1","Homme","Femme")

deces.2011_final<-deces.2011[,c("COM","datedeces","age_years","SEX")]

deces.2011_final$nbr_mort<-1


deces.2011_final$month <- as.numeric(format(deces.2011_final$datedeces, "%m"))




deces.2011_final$tranche_age <- cut(deces.2011_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2011_final$month <- as.numeric(format(deces.2011_final$datedeces, "%m"))


deces.2011_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2011_final,sum)

library(tidyr)

deces.2011_spread <- deces.2011_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2011_spread$`0-9`[is.na(deces.2011_spread$`0-9`)]<-0
deces.2011_spread$`10-19`[is.na(deces.2011_spread$`10-19`)]<-0
deces.2011_spread$`20-39`[is.na(deces.2011_spread$`20-39`)]<-0
deces.2011_spread$`40-59`[is.na(deces.2011_spread$`40-59`)]<-0
deces.2011_spread$`60-64`[is.na(deces.2011_spread$`60-64`)]<-0
deces.2011_spread$`65-69`[is.na(deces.2011_spread$`65-69`)]<-0
deces.2011_spread$`70-74`[is.na(deces.2011_spread$`70-74`)]<-0
deces.2011_spread$`75-79`[is.na(deces.2011_spread$`75-79`)]<-0
deces.2011_spread$`80+`[is.na(deces.2011_spread$`80+`)]<-0



#deces.2011_spread<-left_join(deces.2011_spread, deces.2011_spread)



deces.2011_spread$year<-2011


#fwrite(deces.2011_spread,"/fichier deces insee/décès travaillé/deces.2011_age_sexe.csv")




#################### partie 2 ######


RP_2011_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2011_age_sexe_final_2")

RP_2011_age_sexe_final_2<-RP_2011_age_sexe_final_2[,c(2:15)]

names(RP_2011_age_sexe_final_2)[names(RP_2011_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2011_age_sexe_final_2<-filter(RP_2011_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2011_mortalite<-left_join(deces.2011_spread,RP_2011_age_sexe_final_2)


Base_2011_mortalite$`75+`<-Base_2011_mortalite$`75-79`+Base_2011_mortalite$`80+`
Base_2011_mortalite$value_estimated_sum_75_plus_h_f<-Base_2011_mortalite$value_estimated_sum_75_79_h_f+Base_2011_mortalite$value_estimated_sum_80_plus_h_f


#Base_2011_mortalite$taux_mortalite_homme<-Base_2011_mortalite$Homme/Base_2011_mortalite$value_estimated_sum_homme

#Base_2011_mortalite$taux_mortalite_femme<-Base_2011_mortalite$Femme/Base_2011_mortalite$value_estimated_sum_femme

Base_2011_mortalite$taux_mortalite_0_9<-Base_2011_mortalite$`0-9`/Base_2011_mortalite$value_estimated_sum_0_9_h_f

Base_2011_mortalite$taux_mortalite_10_19<-Base_2011_mortalite$`10-19`/Base_2011_mortalite$value_estimated_sum_10_19_h_f

Base_2011_mortalite$taux_mortalite_20_39<-Base_2011_mortalite$`20-39`/Base_2011_mortalite$value_estimated_sum_20_39_h_f

Base_2011_mortalite$taux_mortalite_40_59<-Base_2011_mortalite$`40-59`/Base_2011_mortalite$value_estimated_sum_40_59_h_f

Base_2011_mortalite$taux_mortalite_60_64<-Base_2011_mortalite$`60-64`/Base_2011_mortalite$value_estimated_sum_60_64_h_f

Base_2011_mortalite$taux_mortalite_65_69<-Base_2011_mortalite$`65-69`/Base_2011_mortalite$value_estimated_sum_65_69_h_f

Base_2011_mortalite$taux_mortalite_70_74<-Base_2011_mortalite$`70-74`/Base_2011_mortalite$value_estimated_sum_70_74_h_f

Base_2011_mortalite$taux_mortalite_75_79<-Base_2011_mortalite$`75-79`/Base_2011_mortalite$value_estimated_sum_75_79_h_f

Base_2011_mortalite$taux_mortalite_75_plus<-Base_2011_mortalite$`75+`/Base_2011_mortalite$value_estimated_sum_75_plus_h_f

Base_2011_mortalite$taux_mortalite_80_plus<-Base_2011_mortalite$`80+`/Base_2011_mortalite$value_estimated_sum_80_plus_h_f

#Base_2011_mortalite$mort_total<-Base_2011_mortalite$Femme+Base_2011_mortalite$Homme

Base_2011_mortalite$mort_total<-Base_2011_mortalite$`0-9`+Base_2011_mortalite$`10-19`+Base_2011_mortalite$`20-39` +Base_2011_mortalite$`40-59` +Base_2011_mortalite$`60-64` +Base_2011_mortalite$`65-69` +Base_2011_mortalite$`70-74` +Base_2011_mortalite$`75+`



Base_2011_mortalite$taux_mortalite_total<-Base_2011_mortalite$mort_total/Base_2011_mortalite$value_estimated_population


Base_2011_mortalite<-filter(Base_2011_mortalite, Base_2011_mortalite$value_estimated_population>0)


Base_2011_mortalite<-Base_2011_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2011_mortalite<-filter(Base_2011_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2011_mortalite,"/heatwave and mortality code and data/new data/Base_2011_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2012 <- read_delim("fichier deces insee/deces-2012.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2012<-left_join(deces.2012, table_passage_bis)


deces.2012$COM<-ifelse(!is.na(deces.2012$COM_AP),deces.2012$COM_AP,deces.2012$lieudeces)

deces.2012$datedeces <- as.character(deces.2012$datedeces)
deces.2012$datedeces <- as.Date(deces.2012$datedeces, format = "%Y%m%d")


deces.2012$datenaiss <- as.character(deces.2012$datenaiss)
deces.2012$datenaiss <- as.Date(deces.2012$datenaiss, format = "%Y%m%d")


deces.2012$year <- as.numeric(format(deces.2012$datedeces, "%Y"))
deces.2012<-filter(deces.2012, deces.2012$year==2012)
deces.2012<-deces.2012[,-12]

library(lubridate)
deces.2012$age <- as.period(interval(deces.2012$datenaiss , deces.2012$datedeces ))
deces.2012$age_years <- year(deces.2012$age)

library(readr)


deces.2012$sexe<-as.character(deces.2012$sexe)

deces.2012$SEX<-ifelse(deces.2012$sexe=="1","Homme","Femme")

deces.2012_final<-deces.2012[,c("COM","datedeces","age_years","SEX")]

deces.2012_final$nbr_mort<-1


deces.2012_final$month <- as.numeric(format(deces.2012_final$datedeces, "%m"))




deces.2012_final$tranche_age <- cut(deces.2012_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2012_final$month <- as.numeric(format(deces.2012_final$datedeces, "%m"))


deces.2012_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2012_final,sum)

library(tidyr)

deces.2012_spread <- deces.2012_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2012_spread$`0-9`[is.na(deces.2012_spread$`0-9`)]<-0
deces.2012_spread$`10-19`[is.na(deces.2012_spread$`10-19`)]<-0
deces.2012_spread$`20-39`[is.na(deces.2012_spread$`20-39`)]<-0
deces.2012_spread$`40-59`[is.na(deces.2012_spread$`40-59`)]<-0
deces.2012_spread$`60-64`[is.na(deces.2012_spread$`60-64`)]<-0
deces.2012_spread$`65-69`[is.na(deces.2012_spread$`65-69`)]<-0
deces.2012_spread$`70-74`[is.na(deces.2012_spread$`70-74`)]<-0
deces.2012_spread$`75-79`[is.na(deces.2012_spread$`75-79`)]<-0
deces.2012_spread$`80+`[is.na(deces.2012_spread$`80+`)]<-0



#deces.2012_spread<-left_join(deces.2012_spread, deces.2012_spread)



deces.2012_spread$year<-2012


#fwrite(deces.2012_spread,"/fichier deces insee/décès travaillé/deces.2012_age_sexe.csv")




#################### partie 2 ######


RP_2012_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2012_age_sexe_final_2")

RP_2012_age_sexe_final_2<-RP_2012_age_sexe_final_2[,c(2:15)]

names(RP_2012_age_sexe_final_2)[names(RP_2012_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2012_age_sexe_final_2<-filter(RP_2012_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2012_mortalite<-left_join(deces.2012_spread,RP_2012_age_sexe_final_2)


Base_2012_mortalite$`75+`<-Base_2012_mortalite$`75-79`+Base_2012_mortalite$`80+`
Base_2012_mortalite$value_estimated_sum_75_plus_h_f<-Base_2012_mortalite$value_estimated_sum_75_79_h_f+Base_2012_mortalite$value_estimated_sum_80_plus_h_f


#Base_2012_mortalite$taux_mortalite_homme<-Base_2012_mortalite$Homme/Base_2012_mortalite$value_estimated_sum_homme

#Base_2012_mortalite$taux_mortalite_femme<-Base_2012_mortalite$Femme/Base_2012_mortalite$value_estimated_sum_femme

Base_2012_mortalite$taux_mortalite_0_9<-Base_2012_mortalite$`0-9`/Base_2012_mortalite$value_estimated_sum_0_9_h_f

Base_2012_mortalite$taux_mortalite_10_19<-Base_2012_mortalite$`10-19`/Base_2012_mortalite$value_estimated_sum_10_19_h_f

Base_2012_mortalite$taux_mortalite_20_39<-Base_2012_mortalite$`20-39`/Base_2012_mortalite$value_estimated_sum_20_39_h_f

Base_2012_mortalite$taux_mortalite_40_59<-Base_2012_mortalite$`40-59`/Base_2012_mortalite$value_estimated_sum_40_59_h_f

Base_2012_mortalite$taux_mortalite_60_64<-Base_2012_mortalite$`60-64`/Base_2012_mortalite$value_estimated_sum_60_64_h_f

Base_2012_mortalite$taux_mortalite_65_69<-Base_2012_mortalite$`65-69`/Base_2012_mortalite$value_estimated_sum_65_69_h_f

Base_2012_mortalite$taux_mortalite_70_74<-Base_2012_mortalite$`70-74`/Base_2012_mortalite$value_estimated_sum_70_74_h_f

Base_2012_mortalite$taux_mortalite_75_79<-Base_2012_mortalite$`75-79`/Base_2012_mortalite$value_estimated_sum_75_79_h_f

Base_2012_mortalite$taux_mortalite_75_plus<-Base_2012_mortalite$`75+`/Base_2012_mortalite$value_estimated_sum_75_plus_h_f

Base_2012_mortalite$taux_mortalite_80_plus<-Base_2012_mortalite$`80+`/Base_2012_mortalite$value_estimated_sum_80_plus_h_f

#Base_2012_mortalite$mort_total<-Base_2012_mortalite$Femme+Base_2012_mortalite$Homme

Base_2012_mortalite$mort_total<-Base_2012_mortalite$`0-9`+Base_2012_mortalite$`10-19`+Base_2012_mortalite$`20-39` +Base_2012_mortalite$`40-59` +Base_2012_mortalite$`60-64` +Base_2012_mortalite$`65-69` +Base_2012_mortalite$`70-74` +Base_2012_mortalite$`75+`



Base_2012_mortalite$taux_mortalite_total<-Base_2012_mortalite$mort_total/Base_2012_mortalite$value_estimated_population


Base_2012_mortalite<-filter(Base_2012_mortalite, Base_2012_mortalite$value_estimated_population>0)


Base_2012_mortalite<-Base_2012_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2012_mortalite<-filter(Base_2012_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2012_mortalite,"/heatwave and mortality code and data/new data/Base_2012_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2013 <- read_delim("fichier deces insee/deces-2013.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2013<-left_join(deces.2013, table_passage_bis)


deces.2013$COM<-ifelse(!is.na(deces.2013$COM_AP),deces.2013$COM_AP,deces.2013$lieudeces)

deces.2013$datedeces <- as.character(deces.2013$datedeces)
deces.2013$datedeces <- as.Date(deces.2013$datedeces, format = "%Y%m%d")


deces.2013$datenaiss <- as.character(deces.2013$datenaiss)
deces.2013$datenaiss <- as.Date(deces.2013$datenaiss, format = "%Y%m%d")


deces.2013$year <- as.numeric(format(deces.2013$datedeces, "%Y"))
deces.2013<-filter(deces.2013, deces.2013$year==2013)
deces.2013<-deces.2013[,-12]

library(lubridate)
deces.2013$age <- as.period(interval(deces.2013$datenaiss , deces.2013$datedeces ))
deces.2013$age_years <- year(deces.2013$age)

library(readr)


deces.2013$sexe<-as.character(deces.2013$sexe)

deces.2013$SEX<-ifelse(deces.2013$sexe=="1","Homme","Femme")

deces.2013_final<-deces.2013[,c("COM","datedeces","age_years","SEX")]

deces.2013_final$nbr_mort<-1


deces.2013_final$month <- as.numeric(format(deces.2013_final$datedeces, "%m"))




deces.2013_final$tranche_age <- cut(deces.2013_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2013_final$month <- as.numeric(format(deces.2013_final$datedeces, "%m"))


deces.2013_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2013_final,sum)

library(tidyr)

deces.2013_spread <- deces.2013_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2013_spread$`0-9`[is.na(deces.2013_spread$`0-9`)]<-0
deces.2013_spread$`10-19`[is.na(deces.2013_spread$`10-19`)]<-0
deces.2013_spread$`20-39`[is.na(deces.2013_spread$`20-39`)]<-0
deces.2013_spread$`40-59`[is.na(deces.2013_spread$`40-59`)]<-0
deces.2013_spread$`60-64`[is.na(deces.2013_spread$`60-64`)]<-0
deces.2013_spread$`65-69`[is.na(deces.2013_spread$`65-69`)]<-0
deces.2013_spread$`70-74`[is.na(deces.2013_spread$`70-74`)]<-0
deces.2013_spread$`75-79`[is.na(deces.2013_spread$`75-79`)]<-0
deces.2013_spread$`80+`[is.na(deces.2013_spread$`80+`)]<-0



#deces.2013_spread<-left_join(deces.2013_spread, deces.2013_spread)



deces.2013_spread$year<-2013


#fwrite(deces.2013_spread,"/fichier deces insee/décès travaillé/deces.2013_age_sexe.csv")




#################### partie 2 ######


RP_2013_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2013_age_sexe_final_2")

RP_2013_age_sexe_final_2<-RP_2013_age_sexe_final_2[,c(2:15)]

names(RP_2013_age_sexe_final_2)[names(RP_2013_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2013_age_sexe_final_2<-filter(RP_2013_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2013_mortalite<-left_join(deces.2013_spread,RP_2013_age_sexe_final_2)


Base_2013_mortalite$`75+`<-Base_2013_mortalite$`75-79`+Base_2013_mortalite$`80+`
Base_2013_mortalite$value_estimated_sum_75_plus_h_f<-Base_2013_mortalite$value_estimated_sum_75_79_h_f+Base_2013_mortalite$value_estimated_sum_80_plus_h_f


#Base_2013_mortalite$taux_mortalite_homme<-Base_2013_mortalite$Homme/Base_2013_mortalite$value_estimated_sum_homme

#Base_2013_mortalite$taux_mortalite_femme<-Base_2013_mortalite$Femme/Base_2013_mortalite$value_estimated_sum_femme

Base_2013_mortalite$taux_mortalite_0_9<-Base_2013_mortalite$`0-9`/Base_2013_mortalite$value_estimated_sum_0_9_h_f

Base_2013_mortalite$taux_mortalite_10_19<-Base_2013_mortalite$`10-19`/Base_2013_mortalite$value_estimated_sum_10_19_h_f

Base_2013_mortalite$taux_mortalite_20_39<-Base_2013_mortalite$`20-39`/Base_2013_mortalite$value_estimated_sum_20_39_h_f

Base_2013_mortalite$taux_mortalite_40_59<-Base_2013_mortalite$`40-59`/Base_2013_mortalite$value_estimated_sum_40_59_h_f

Base_2013_mortalite$taux_mortalite_60_64<-Base_2013_mortalite$`60-64`/Base_2013_mortalite$value_estimated_sum_60_64_h_f

Base_2013_mortalite$taux_mortalite_65_69<-Base_2013_mortalite$`65-69`/Base_2013_mortalite$value_estimated_sum_65_69_h_f

Base_2013_mortalite$taux_mortalite_70_74<-Base_2013_mortalite$`70-74`/Base_2013_mortalite$value_estimated_sum_70_74_h_f

Base_2013_mortalite$taux_mortalite_75_79<-Base_2013_mortalite$`75-79`/Base_2013_mortalite$value_estimated_sum_75_79_h_f

Base_2013_mortalite$taux_mortalite_75_plus<-Base_2013_mortalite$`75+`/Base_2013_mortalite$value_estimated_sum_75_plus_h_f

Base_2013_mortalite$taux_mortalite_80_plus<-Base_2013_mortalite$`80+`/Base_2013_mortalite$value_estimated_sum_80_plus_h_f

#Base_2013_mortalite$mort_total<-Base_2013_mortalite$Femme+Base_2013_mortalite$Homme

Base_2013_mortalite$mort_total<-Base_2013_mortalite$`0-9`+Base_2013_mortalite$`10-19`+Base_2013_mortalite$`20-39` +Base_2013_mortalite$`40-59` +Base_2013_mortalite$`60-64` +Base_2013_mortalite$`65-69` +Base_2013_mortalite$`70-74` +Base_2013_mortalite$`75+`



Base_2013_mortalite$taux_mortalite_total<-Base_2013_mortalite$mort_total/Base_2013_mortalite$value_estimated_population


Base_2013_mortalite<-filter(Base_2013_mortalite, Base_2013_mortalite$value_estimated_population>0)


Base_2013_mortalite<-Base_2013_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2013_mortalite<-filter(Base_2013_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2013_mortalite,"/heatwave and mortality code and data/new data/Base_2013_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2014 <- read_delim("fichier deces insee/deces-2014.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2014<-left_join(deces.2014, table_passage_bis)


deces.2014$COM<-ifelse(!is.na(deces.2014$COM_AP),deces.2014$COM_AP,deces.2014$lieudeces)

deces.2014$datedeces <- as.character(deces.2014$datedeces)
deces.2014$datedeces <- as.Date(deces.2014$datedeces, format = "%Y%m%d")


deces.2014$datenaiss <- as.character(deces.2014$datenaiss)
deces.2014$datenaiss <- as.Date(deces.2014$datenaiss, format = "%Y%m%d")


deces.2014$year <- as.numeric(format(deces.2014$datedeces, "%Y"))
deces.2014<-filter(deces.2014, deces.2014$year==2014)
deces.2014<-deces.2014[,-12]

library(lubridate)
deces.2014$age <- as.period(interval(deces.2014$datenaiss , deces.2014$datedeces ))
deces.2014$age_years <- year(deces.2014$age)

library(readr)


deces.2014$sexe<-as.character(deces.2014$sexe)

deces.2014$SEX<-ifelse(deces.2014$sexe=="1","Homme","Femme")

deces.2014_final<-deces.2014[,c("COM","datedeces","age_years","SEX")]

deces.2014_final$nbr_mort<-1


deces.2014_final$month <- as.numeric(format(deces.2014_final$datedeces, "%m"))




deces.2014_final$tranche_age <- cut(deces.2014_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2014_final$month <- as.numeric(format(deces.2014_final$datedeces, "%m"))


deces.2014_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2014_final,sum)

library(tidyr)

deces.2014_spread <- deces.2014_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2014_spread$`0-9`[is.na(deces.2014_spread$`0-9`)]<-0
deces.2014_spread$`10-19`[is.na(deces.2014_spread$`10-19`)]<-0
deces.2014_spread$`20-39`[is.na(deces.2014_spread$`20-39`)]<-0
deces.2014_spread$`40-59`[is.na(deces.2014_spread$`40-59`)]<-0
deces.2014_spread$`60-64`[is.na(deces.2014_spread$`60-64`)]<-0
deces.2014_spread$`65-69`[is.na(deces.2014_spread$`65-69`)]<-0
deces.2014_spread$`70-74`[is.na(deces.2014_spread$`70-74`)]<-0
deces.2014_spread$`75-79`[is.na(deces.2014_spread$`75-79`)]<-0
deces.2014_spread$`80+`[is.na(deces.2014_spread$`80+`)]<-0



#deces.2014_spread<-left_join(deces.2014_spread, deces.2014_spread)



deces.2014_spread$year<-2014


#fwrite(deces.2014_spread,"/fichier deces insee/décès travaillé/deces.2014_age_sexe.csv")




#################### partie 2 ######


RP_2014_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2014_age_sexe_final_2")

RP_2014_age_sexe_final_2<-RP_2014_age_sexe_final_2[,c(2:15)]

names(RP_2014_age_sexe_final_2)[names(RP_2014_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2014_age_sexe_final_2<-filter(RP_2014_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2014_mortalite<-left_join(deces.2014_spread,RP_2014_age_sexe_final_2)


Base_2014_mortalite$`75+`<-Base_2014_mortalite$`75-79`+Base_2014_mortalite$`80+`
Base_2014_mortalite$value_estimated_sum_75_plus_h_f<-Base_2014_mortalite$value_estimated_sum_75_79_h_f+Base_2014_mortalite$value_estimated_sum_80_plus_h_f


#Base_2014_mortalite$taux_mortalite_homme<-Base_2014_mortalite$Homme/Base_2014_mortalite$value_estimated_sum_homme

#Base_2014_mortalite$taux_mortalite_femme<-Base_2014_mortalite$Femme/Base_2014_mortalite$value_estimated_sum_femme

Base_2014_mortalite$taux_mortalite_0_9<-Base_2014_mortalite$`0-9`/Base_2014_mortalite$value_estimated_sum_0_9_h_f

Base_2014_mortalite$taux_mortalite_10_19<-Base_2014_mortalite$`10-19`/Base_2014_mortalite$value_estimated_sum_10_19_h_f

Base_2014_mortalite$taux_mortalite_20_39<-Base_2014_mortalite$`20-39`/Base_2014_mortalite$value_estimated_sum_20_39_h_f

Base_2014_mortalite$taux_mortalite_40_59<-Base_2014_mortalite$`40-59`/Base_2014_mortalite$value_estimated_sum_40_59_h_f

Base_2014_mortalite$taux_mortalite_60_64<-Base_2014_mortalite$`60-64`/Base_2014_mortalite$value_estimated_sum_60_64_h_f

Base_2014_mortalite$taux_mortalite_65_69<-Base_2014_mortalite$`65-69`/Base_2014_mortalite$value_estimated_sum_65_69_h_f

Base_2014_mortalite$taux_mortalite_70_74<-Base_2014_mortalite$`70-74`/Base_2014_mortalite$value_estimated_sum_70_74_h_f

Base_2014_mortalite$taux_mortalite_75_79<-Base_2014_mortalite$`75-79`/Base_2014_mortalite$value_estimated_sum_75_79_h_f

Base_2014_mortalite$taux_mortalite_75_plus<-Base_2014_mortalite$`75+`/Base_2014_mortalite$value_estimated_sum_75_plus_h_f

Base_2014_mortalite$taux_mortalite_80_plus<-Base_2014_mortalite$`80+`/Base_2014_mortalite$value_estimated_sum_80_plus_h_f

#Base_2014_mortalite$mort_total<-Base_2014_mortalite$Femme+Base_2014_mortalite$Homme

Base_2014_mortalite$mort_total<-Base_2014_mortalite$`0-9`+Base_2014_mortalite$`10-19`+Base_2014_mortalite$`20-39` +Base_2014_mortalite$`40-59` +Base_2014_mortalite$`60-64` +Base_2014_mortalite$`65-69` +Base_2014_mortalite$`70-74` +Base_2014_mortalite$`75+`



Base_2014_mortalite$taux_mortalite_total<-Base_2014_mortalite$mort_total/Base_2014_mortalite$value_estimated_population


Base_2014_mortalite<-filter(Base_2014_mortalite, Base_2014_mortalite$value_estimated_population>0)


Base_2014_mortalite<-Base_2014_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2014_mortalite<-filter(Base_2014_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2014_mortalite,"/heatwave and mortality code and data/new data/Base_2014_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2015 <- read_delim("fichier deces insee/deces-2015.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2015<-left_join(deces.2015, table_passage_bis)


deces.2015$COM<-ifelse(!is.na(deces.2015$COM_AP),deces.2015$COM_AP,deces.2015$lieudeces)

deces.2015$datedeces <- as.character(deces.2015$datedeces)
deces.2015$datedeces <- as.Date(deces.2015$datedeces, format = "%Y%m%d")


deces.2015$datenaiss <- as.character(deces.2015$datenaiss)
deces.2015$datenaiss <- as.Date(deces.2015$datenaiss, format = "%Y%m%d")


deces.2015$year <- as.numeric(format(deces.2015$datedeces, "%Y"))
deces.2015<-filter(deces.2015, deces.2015$year==2015)
deces.2015<-deces.2015[,-12]

library(lubridate)
deces.2015$age <- as.period(interval(deces.2015$datenaiss , deces.2015$datedeces ))
deces.2015$age_years <- year(deces.2015$age)

library(readr)


deces.2015$sexe<-as.character(deces.2015$sexe)

deces.2015$SEX<-ifelse(deces.2015$sexe=="1","Homme","Femme")

deces.2015_final<-deces.2015[,c("COM","datedeces","age_years","SEX")]

deces.2015_final$nbr_mort<-1


deces.2015_final$month <- as.numeric(format(deces.2015_final$datedeces, "%m"))




deces.2015_final$tranche_age <- cut(deces.2015_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2015_final$month <- as.numeric(format(deces.2015_final$datedeces, "%m"))


deces.2015_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2015_final,sum)

library(tidyr)

deces.2015_spread <- deces.2015_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2015_spread$`0-9`[is.na(deces.2015_spread$`0-9`)]<-0
deces.2015_spread$`10-19`[is.na(deces.2015_spread$`10-19`)]<-0
deces.2015_spread$`20-39`[is.na(deces.2015_spread$`20-39`)]<-0
deces.2015_spread$`40-59`[is.na(deces.2015_spread$`40-59`)]<-0
deces.2015_spread$`60-64`[is.na(deces.2015_spread$`60-64`)]<-0
deces.2015_spread$`65-69`[is.na(deces.2015_spread$`65-69`)]<-0
deces.2015_spread$`70-74`[is.na(deces.2015_spread$`70-74`)]<-0
deces.2015_spread$`75-79`[is.na(deces.2015_spread$`75-79`)]<-0
deces.2015_spread$`80+`[is.na(deces.2015_spread$`80+`)]<-0



#deces.2015_spread<-left_join(deces.2015_spread, deces.2015_spread)



deces.2015_spread$year<-2015


#fwrite(deces.2015_spread,"/fichier deces insee/décès travaillé/deces.2015_age_sexe.csv")




#################### partie 2 ######


RP_2015_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2015_age_sexe_final_2")

RP_2015_age_sexe_final_2<-RP_2015_age_sexe_final_2[,c(2:15)]

names(RP_2015_age_sexe_final_2)[names(RP_2015_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2015_age_sexe_final_2<-filter(RP_2015_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2015_mortalite<-left_join(deces.2015_spread,RP_2015_age_sexe_final_2)


Base_2015_mortalite$`75+`<-Base_2015_mortalite$`75-79`+Base_2015_mortalite$`80+`
Base_2015_mortalite$value_estimated_sum_75_plus_h_f<-Base_2015_mortalite$value_estimated_sum_75_79_h_f+Base_2015_mortalite$value_estimated_sum_80_plus_h_f


#Base_2015_mortalite$taux_mortalite_homme<-Base_2015_mortalite$Homme/Base_2015_mortalite$value_estimated_sum_homme

#Base_2015_mortalite$taux_mortalite_femme<-Base_2015_mortalite$Femme/Base_2015_mortalite$value_estimated_sum_femme

Base_2015_mortalite$taux_mortalite_0_9<-Base_2015_mortalite$`0-9`/Base_2015_mortalite$value_estimated_sum_0_9_h_f

Base_2015_mortalite$taux_mortalite_10_19<-Base_2015_mortalite$`10-19`/Base_2015_mortalite$value_estimated_sum_10_19_h_f

Base_2015_mortalite$taux_mortalite_20_39<-Base_2015_mortalite$`20-39`/Base_2015_mortalite$value_estimated_sum_20_39_h_f

Base_2015_mortalite$taux_mortalite_40_59<-Base_2015_mortalite$`40-59`/Base_2015_mortalite$value_estimated_sum_40_59_h_f

Base_2015_mortalite$taux_mortalite_60_64<-Base_2015_mortalite$`60-64`/Base_2015_mortalite$value_estimated_sum_60_64_h_f

Base_2015_mortalite$taux_mortalite_65_69<-Base_2015_mortalite$`65-69`/Base_2015_mortalite$value_estimated_sum_65_69_h_f

Base_2015_mortalite$taux_mortalite_70_74<-Base_2015_mortalite$`70-74`/Base_2015_mortalite$value_estimated_sum_70_74_h_f

Base_2015_mortalite$taux_mortalite_75_79<-Base_2015_mortalite$`75-79`/Base_2015_mortalite$value_estimated_sum_75_79_h_f

Base_2015_mortalite$taux_mortalite_75_plus<-Base_2015_mortalite$`75+`/Base_2015_mortalite$value_estimated_sum_75_plus_h_f

Base_2015_mortalite$taux_mortalite_80_plus<-Base_2015_mortalite$`80+`/Base_2015_mortalite$value_estimated_sum_80_plus_h_f

#Base_2015_mortalite$mort_total<-Base_2015_mortalite$Femme+Base_2015_mortalite$Homme

Base_2015_mortalite$mort_total<-Base_2015_mortalite$`0-9`+Base_2015_mortalite$`10-19`+Base_2015_mortalite$`20-39` +Base_2015_mortalite$`40-59` +Base_2015_mortalite$`60-64` +Base_2015_mortalite$`65-69` +Base_2015_mortalite$`70-74` +Base_2015_mortalite$`75+`



Base_2015_mortalite$taux_mortalite_total<-Base_2015_mortalite$mort_total/Base_2015_mortalite$value_estimated_population


Base_2015_mortalite<-filter(Base_2015_mortalite, Base_2015_mortalite$value_estimated_population>0)


Base_2015_mortalite<-Base_2015_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2015_mortalite<-filter(Base_2015_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2015_mortalite,"/heatwave and mortality code and data/new data/Base_2015_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2016 <- read_delim("fichier deces insee/deces-2016.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2016<-left_join(deces.2016, table_passage_bis)


deces.2016$COM<-ifelse(!is.na(deces.2016$COM_AP),deces.2016$COM_AP,deces.2016$lieudeces)

deces.2016$datedeces <- as.character(deces.2016$datedeces)
deces.2016$datedeces <- as.Date(deces.2016$datedeces, format = "%Y%m%d")


deces.2016$datenaiss <- as.character(deces.2016$datenaiss)
deces.2016$datenaiss <- as.Date(deces.2016$datenaiss, format = "%Y%m%d")


deces.2016$year <- as.numeric(format(deces.2016$datedeces, "%Y"))
deces.2016<-filter(deces.2016, deces.2016$year==2016)
deces.2016<-deces.2016[,-12]

library(lubridate)
deces.2016$age <- as.period(interval(deces.2016$datenaiss , deces.2016$datedeces ))
deces.2016$age_years <- year(deces.2016$age)

library(readr)


deces.2016$sexe<-as.character(deces.2016$sexe)

deces.2016$SEX<-ifelse(deces.2016$sexe=="1","Homme","Femme")

deces.2016_final<-deces.2016[,c("COM","datedeces","age_years","SEX")]

deces.2016_final$nbr_mort<-1


deces.2016_final$month <- as.numeric(format(deces.2016_final$datedeces, "%m"))




deces.2016_final$tranche_age <- cut(deces.2016_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2016_final$month <- as.numeric(format(deces.2016_final$datedeces, "%m"))


deces.2016_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2016_final,sum)

library(tidyr)

deces.2016_spread <- deces.2016_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2016_spread$`0-9`[is.na(deces.2016_spread$`0-9`)]<-0
deces.2016_spread$`10-19`[is.na(deces.2016_spread$`10-19`)]<-0
deces.2016_spread$`20-39`[is.na(deces.2016_spread$`20-39`)]<-0
deces.2016_spread$`40-59`[is.na(deces.2016_spread$`40-59`)]<-0
deces.2016_spread$`60-64`[is.na(deces.2016_spread$`60-64`)]<-0
deces.2016_spread$`65-69`[is.na(deces.2016_spread$`65-69`)]<-0
deces.2016_spread$`70-74`[is.na(deces.2016_spread$`70-74`)]<-0
deces.2016_spread$`75-79`[is.na(deces.2016_spread$`75-79`)]<-0
deces.2016_spread$`80+`[is.na(deces.2016_spread$`80+`)]<-0



#deces.2016_spread<-left_join(deces.2016_spread, deces.2016_spread)



deces.2016_spread$year<-2016


#fwrite(deces.2016_spread,"/fichier deces insee/décès travaillé/deces.2016_age_sexe.csv")




#################### partie 2 ######


RP_2016_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2016_age_sexe_final_2")

RP_2016_age_sexe_final_2<-RP_2016_age_sexe_final_2[,c(2:15)]

names(RP_2016_age_sexe_final_2)[names(RP_2016_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2016_age_sexe_final_2<-filter(RP_2016_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2016_mortalite<-left_join(deces.2016_spread,RP_2016_age_sexe_final_2)


Base_2016_mortalite$`75+`<-Base_2016_mortalite$`75-79`+Base_2016_mortalite$`80+`
Base_2016_mortalite$value_estimated_sum_75_plus_h_f<-Base_2016_mortalite$value_estimated_sum_75_79_h_f+Base_2016_mortalite$value_estimated_sum_80_plus_h_f


#Base_2016_mortalite$taux_mortalite_homme<-Base_2016_mortalite$Homme/Base_2016_mortalite$value_estimated_sum_homme

#Base_2016_mortalite$taux_mortalite_femme<-Base_2016_mortalite$Femme/Base_2016_mortalite$value_estimated_sum_femme

Base_2016_mortalite$taux_mortalite_0_9<-Base_2016_mortalite$`0-9`/Base_2016_mortalite$value_estimated_sum_0_9_h_f

Base_2016_mortalite$taux_mortalite_10_19<-Base_2016_mortalite$`10-19`/Base_2016_mortalite$value_estimated_sum_10_19_h_f

Base_2016_mortalite$taux_mortalite_20_39<-Base_2016_mortalite$`20-39`/Base_2016_mortalite$value_estimated_sum_20_39_h_f

Base_2016_mortalite$taux_mortalite_40_59<-Base_2016_mortalite$`40-59`/Base_2016_mortalite$value_estimated_sum_40_59_h_f

Base_2016_mortalite$taux_mortalite_60_64<-Base_2016_mortalite$`60-64`/Base_2016_mortalite$value_estimated_sum_60_64_h_f

Base_2016_mortalite$taux_mortalite_65_69<-Base_2016_mortalite$`65-69`/Base_2016_mortalite$value_estimated_sum_65_69_h_f

Base_2016_mortalite$taux_mortalite_70_74<-Base_2016_mortalite$`70-74`/Base_2016_mortalite$value_estimated_sum_70_74_h_f

Base_2016_mortalite$taux_mortalite_75_79<-Base_2016_mortalite$`75-79`/Base_2016_mortalite$value_estimated_sum_75_79_h_f

Base_2016_mortalite$taux_mortalite_75_plus<-Base_2016_mortalite$`75+`/Base_2016_mortalite$value_estimated_sum_75_plus_h_f

Base_2016_mortalite$taux_mortalite_80_plus<-Base_2016_mortalite$`80+`/Base_2016_mortalite$value_estimated_sum_80_plus_h_f

#Base_2016_mortalite$mort_total<-Base_2016_mortalite$Femme+Base_2016_mortalite$Homme

Base_2016_mortalite$mort_total<-Base_2016_mortalite$`0-9`+Base_2016_mortalite$`10-19`+Base_2016_mortalite$`20-39` +Base_2016_mortalite$`40-59` +Base_2016_mortalite$`60-64` +Base_2016_mortalite$`65-69` +Base_2016_mortalite$`70-74` +Base_2016_mortalite$`75+`



Base_2016_mortalite$taux_mortalite_total<-Base_2016_mortalite$mort_total/Base_2016_mortalite$value_estimated_population


Base_2016_mortalite<-filter(Base_2016_mortalite, Base_2016_mortalite$value_estimated_population>0)


Base_2016_mortalite<-Base_2016_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2016_mortalite<-filter(Base_2016_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2016_mortalite,"/heatwave and mortality code and data/new data/Base_2016_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2017 <- read_delim("fichier deces insee/deces-2017.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2017<-left_join(deces.2017, table_passage_bis)


deces.2017$COM<-ifelse(!is.na(deces.2017$COM_AP),deces.2017$COM_AP,deces.2017$lieudeces)

deces.2017$datedeces <- as.character(deces.2017$datedeces)
deces.2017$datedeces <- as.Date(deces.2017$datedeces, format = "%Y%m%d")


deces.2017$datenaiss <- as.character(deces.2017$datenaiss)
deces.2017$datenaiss <- as.Date(deces.2017$datenaiss, format = "%Y%m%d")


deces.2017$year <- as.numeric(format(deces.2017$datedeces, "%Y"))
deces.2017<-filter(deces.2017, deces.2017$year==2017)
deces.2017<-deces.2017[,-12]

library(lubridate)
deces.2017$age <- as.period(interval(deces.2017$datenaiss , deces.2017$datedeces ))
deces.2017$age_years <- year(deces.2017$age)

library(readr)


deces.2017$sexe<-as.character(deces.2017$sexe)

deces.2017$SEX<-ifelse(deces.2017$sexe=="1","Homme","Femme")

deces.2017_final<-deces.2017[,c("COM","datedeces","age_years","SEX")]

deces.2017_final$nbr_mort<-1


deces.2017_final$month <- as.numeric(format(deces.2017_final$datedeces, "%m"))




deces.2017_final$tranche_age <- cut(deces.2017_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2017_final$month <- as.numeric(format(deces.2017_final$datedeces, "%m"))


deces.2017_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2017_final,sum)

library(tidyr)

deces.2017_spread <- deces.2017_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2017_spread$`0-9`[is.na(deces.2017_spread$`0-9`)]<-0
deces.2017_spread$`10-19`[is.na(deces.2017_spread$`10-19`)]<-0
deces.2017_spread$`20-39`[is.na(deces.2017_spread$`20-39`)]<-0
deces.2017_spread$`40-59`[is.na(deces.2017_spread$`40-59`)]<-0
deces.2017_spread$`60-64`[is.na(deces.2017_spread$`60-64`)]<-0
deces.2017_spread$`65-69`[is.na(deces.2017_spread$`65-69`)]<-0
deces.2017_spread$`70-74`[is.na(deces.2017_spread$`70-74`)]<-0
deces.2017_spread$`75-79`[is.na(deces.2017_spread$`75-79`)]<-0
deces.2017_spread$`80+`[is.na(deces.2017_spread$`80+`)]<-0



#deces.2017_spread<-left_join(deces.2017_spread, deces.2017_spread)



deces.2017_spread$year<-2017


#fwrite(deces.2017_spread,"/fichier deces insee/décès travaillé/deces.2017_age_sexe.csv")




#################### partie 2 ######


RP_2017_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2017_age_sexe_final_2")

RP_2017_age_sexe_final_2<-RP_2017_age_sexe_final_2[,c(2:15)]

names(RP_2017_age_sexe_final_2)[names(RP_2017_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2017_age_sexe_final_2<-filter(RP_2017_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2017_mortalite<-left_join(deces.2017_spread,RP_2017_age_sexe_final_2)


Base_2017_mortalite$`75+`<-Base_2017_mortalite$`75-79`+Base_2017_mortalite$`80+`
Base_2017_mortalite$value_estimated_sum_75_plus_h_f<-Base_2017_mortalite$value_estimated_sum_75_79_h_f+Base_2017_mortalite$value_estimated_sum_80_plus_h_f


#Base_2017_mortalite$taux_mortalite_homme<-Base_2017_mortalite$Homme/Base_2017_mortalite$value_estimated_sum_homme

#Base_2017_mortalite$taux_mortalite_femme<-Base_2017_mortalite$Femme/Base_2017_mortalite$value_estimated_sum_femme

Base_2017_mortalite$taux_mortalite_0_9<-Base_2017_mortalite$`0-9`/Base_2017_mortalite$value_estimated_sum_0_9_h_f

Base_2017_mortalite$taux_mortalite_10_19<-Base_2017_mortalite$`10-19`/Base_2017_mortalite$value_estimated_sum_10_19_h_f

Base_2017_mortalite$taux_mortalite_20_39<-Base_2017_mortalite$`20-39`/Base_2017_mortalite$value_estimated_sum_20_39_h_f

Base_2017_mortalite$taux_mortalite_40_59<-Base_2017_mortalite$`40-59`/Base_2017_mortalite$value_estimated_sum_40_59_h_f

Base_2017_mortalite$taux_mortalite_60_64<-Base_2017_mortalite$`60-64`/Base_2017_mortalite$value_estimated_sum_60_64_h_f

Base_2017_mortalite$taux_mortalite_65_69<-Base_2017_mortalite$`65-69`/Base_2017_mortalite$value_estimated_sum_65_69_h_f

Base_2017_mortalite$taux_mortalite_70_74<-Base_2017_mortalite$`70-74`/Base_2017_mortalite$value_estimated_sum_70_74_h_f

Base_2017_mortalite$taux_mortalite_75_79<-Base_2017_mortalite$`75-79`/Base_2017_mortalite$value_estimated_sum_75_79_h_f

Base_2017_mortalite$taux_mortalite_75_plus<-Base_2017_mortalite$`75+`/Base_2017_mortalite$value_estimated_sum_75_plus_h_f

Base_2017_mortalite$taux_mortalite_80_plus<-Base_2017_mortalite$`80+`/Base_2017_mortalite$value_estimated_sum_80_plus_h_f

#Base_2017_mortalite$mort_total<-Base_2017_mortalite$Femme+Base_2017_mortalite$Homme

Base_2017_mortalite$mort_total<-Base_2017_mortalite$`0-9`+Base_2017_mortalite$`10-19`+Base_2017_mortalite$`20-39` +Base_2017_mortalite$`40-59` +Base_2017_mortalite$`60-64` +Base_2017_mortalite$`65-69` +Base_2017_mortalite$`70-74` +Base_2017_mortalite$`75+`



Base_2017_mortalite$taux_mortalite_total<-Base_2017_mortalite$mort_total/Base_2017_mortalite$value_estimated_population


Base_2017_mortalite<-filter(Base_2017_mortalite, Base_2017_mortalite$value_estimated_population>0)


Base_2017_mortalite<-Base_2017_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2017_mortalite<-filter(Base_2017_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2017_mortalite,"/heatwave and mortality code and data/new data/Base_2017_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2018 <- read_delim("fichier deces insee/deces-2018.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2018<-left_join(deces.2018, table_passage_bis)


deces.2018$COM<-ifelse(!is.na(deces.2018$COM_AP),deces.2018$COM_AP,deces.2018$lieudeces)

deces.2018$datedeces <- as.character(deces.2018$datedeces)
deces.2018$datedeces <- as.Date(deces.2018$datedeces, format = "%Y%m%d")


deces.2018$datenaiss <- as.character(deces.2018$datenaiss)
deces.2018$datenaiss <- as.Date(deces.2018$datenaiss, format = "%Y%m%d")


deces.2018$year <- as.numeric(format(deces.2018$datedeces, "%Y"))
deces.2018<-filter(deces.2018, deces.2018$year==2018)
deces.2018<-deces.2018[,-12]

library(lubridate)
deces.2018$age <- as.period(interval(deces.2018$datenaiss , deces.2018$datedeces ))
deces.2018$age_years <- year(deces.2018$age)

library(readr)


deces.2018$sexe<-as.character(deces.2018$sexe)

deces.2018$SEX<-ifelse(deces.2018$sexe=="1","Homme","Femme")

deces.2018_final<-deces.2018[,c("COM","datedeces","age_years","SEX")]

deces.2018_final$nbr_mort<-1


deces.2018_final$month <- as.numeric(format(deces.2018_final$datedeces, "%m"))




deces.2018_final$tranche_age <- cut(deces.2018_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2018_final$month <- as.numeric(format(deces.2018_final$datedeces, "%m"))


deces.2018_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2018_final,sum)

library(tidyr)

deces.2018_spread <- deces.2018_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2018_spread$`0-9`[is.na(deces.2018_spread$`0-9`)]<-0
deces.2018_spread$`10-19`[is.na(deces.2018_spread$`10-19`)]<-0
deces.2018_spread$`20-39`[is.na(deces.2018_spread$`20-39`)]<-0
deces.2018_spread$`40-59`[is.na(deces.2018_spread$`40-59`)]<-0
deces.2018_spread$`60-64`[is.na(deces.2018_spread$`60-64`)]<-0
deces.2018_spread$`65-69`[is.na(deces.2018_spread$`65-69`)]<-0
deces.2018_spread$`70-74`[is.na(deces.2018_spread$`70-74`)]<-0
deces.2018_spread$`75-79`[is.na(deces.2018_spread$`75-79`)]<-0
deces.2018_spread$`80+`[is.na(deces.2018_spread$`80+`)]<-0



#deces.2018_spread<-left_join(deces.2018_spread, deces.2018_spread)



deces.2018_spread$year<-2018


#fwrite(deces.2018_spread,"/fichier deces insee/décès travaillé/deces.2018_age_sexe.csv")




#################### partie 2 ######


RP_2018_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2018_age_sexe_final_2")

RP_2018_age_sexe_final_2<-RP_2018_age_sexe_final_2[,c(2:15)]

names(RP_2018_age_sexe_final_2)[names(RP_2018_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2018_age_sexe_final_2<-filter(RP_2018_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2018_mortalite<-left_join(deces.2018_spread,RP_2018_age_sexe_final_2)


Base_2018_mortalite$`75+`<-Base_2018_mortalite$`75-79`+Base_2018_mortalite$`80+`
Base_2018_mortalite$value_estimated_sum_75_plus_h_f<-Base_2018_mortalite$value_estimated_sum_75_79_h_f+Base_2018_mortalite$value_estimated_sum_80_plus_h_f


#Base_2018_mortalite$taux_mortalite_homme<-Base_2018_mortalite$Homme/Base_2018_mortalite$value_estimated_sum_homme

#Base_2018_mortalite$taux_mortalite_femme<-Base_2018_mortalite$Femme/Base_2018_mortalite$value_estimated_sum_femme

Base_2018_mortalite$taux_mortalite_0_9<-Base_2018_mortalite$`0-9`/Base_2018_mortalite$value_estimated_sum_0_9_h_f

Base_2018_mortalite$taux_mortalite_10_19<-Base_2018_mortalite$`10-19`/Base_2018_mortalite$value_estimated_sum_10_19_h_f

Base_2018_mortalite$taux_mortalite_20_39<-Base_2018_mortalite$`20-39`/Base_2018_mortalite$value_estimated_sum_20_39_h_f

Base_2018_mortalite$taux_mortalite_40_59<-Base_2018_mortalite$`40-59`/Base_2018_mortalite$value_estimated_sum_40_59_h_f

Base_2018_mortalite$taux_mortalite_60_64<-Base_2018_mortalite$`60-64`/Base_2018_mortalite$value_estimated_sum_60_64_h_f

Base_2018_mortalite$taux_mortalite_65_69<-Base_2018_mortalite$`65-69`/Base_2018_mortalite$value_estimated_sum_65_69_h_f

Base_2018_mortalite$taux_mortalite_70_74<-Base_2018_mortalite$`70-74`/Base_2018_mortalite$value_estimated_sum_70_74_h_f

Base_2018_mortalite$taux_mortalite_75_79<-Base_2018_mortalite$`75-79`/Base_2018_mortalite$value_estimated_sum_75_79_h_f

Base_2018_mortalite$taux_mortalite_75_plus<-Base_2018_mortalite$`75+`/Base_2018_mortalite$value_estimated_sum_75_plus_h_f

Base_2018_mortalite$taux_mortalite_80_plus<-Base_2018_mortalite$`80+`/Base_2018_mortalite$value_estimated_sum_80_plus_h_f

#Base_2018_mortalite$mort_total<-Base_2018_mortalite$Femme+Base_2018_mortalite$Homme

Base_2018_mortalite$mort_total<-Base_2018_mortalite$`0-9`+Base_2018_mortalite$`10-19`+Base_2018_mortalite$`20-39` +Base_2018_mortalite$`40-59` +Base_2018_mortalite$`60-64` +Base_2018_mortalite$`65-69` +Base_2018_mortalite$`70-74` +Base_2018_mortalite$`75+`



Base_2018_mortalite$taux_mortalite_total<-Base_2018_mortalite$mort_total/Base_2018_mortalite$value_estimated_population


Base_2018_mortalite<-filter(Base_2018_mortalite, Base_2018_mortalite$value_estimated_population>0)


Base_2018_mortalite<-Base_2018_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2018_mortalite<-filter(Base_2018_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2018_mortalite,"/heatwave and mortality code and data/new data/Base_2018_mortalite.csv")




rm(list = ls())
gc()


#################################### 




table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2019 <- read_delim("fichier deces insee/deces-2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2019<-left_join(deces.2019, table_passage_bis)


deces.2019$COM<-ifelse(!is.na(deces.2019$COM_AP),deces.2019$COM_AP,deces.2019$lieudeces)

deces.2019$datedeces <- as.character(deces.2019$datedeces)
deces.2019$datedeces <- as.Date(deces.2019$datedeces, format = "%Y%m%d")


deces.2019$datenaiss <- as.character(deces.2019$datenaiss)
deces.2019$datenaiss <- as.Date(deces.2019$datenaiss, format = "%Y%m%d")


deces.2019$year <- as.numeric(format(deces.2019$datedeces, "%Y"))
deces.2019<-filter(deces.2019, deces.2019$year==2019)
deces.2019<-deces.2019[,-12]

library(lubridate)
deces.2019$age <- as.period(interval(deces.2019$datenaiss , deces.2019$datedeces ))
deces.2019$age_years <- year(deces.2019$age)

library(readr)


deces.2019$sexe<-as.character(deces.2019$sexe)

deces.2019$SEX<-ifelse(deces.2019$sexe=="1","Homme","Femme")

deces.2019_final<-deces.2019[,c("COM","datedeces","age_years","SEX")]

deces.2019_final$nbr_mort<-1


deces.2019_final$month <- as.numeric(format(deces.2019_final$datedeces, "%m"))




deces.2019_final$tranche_age <- cut(deces.2019_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.2019_final$month <- as.numeric(format(deces.2019_final$datedeces, "%m"))


deces.2019_final_ag2<-aggregate(nbr_mort~COM+month+tranche_age,deces.2019_final,sum)

library(tidyr)

deces.2019_spread <- deces.2019_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2019_spread$`0-9`[is.na(deces.2019_spread$`0-9`)]<-0
deces.2019_spread$`10-19`[is.na(deces.2019_spread$`10-19`)]<-0
deces.2019_spread$`20-39`[is.na(deces.2019_spread$`20-39`)]<-0
deces.2019_spread$`40-59`[is.na(deces.2019_spread$`40-59`)]<-0
deces.2019_spread$`60-64`[is.na(deces.2019_spread$`60-64`)]<-0
deces.2019_spread$`65-69`[is.na(deces.2019_spread$`65-69`)]<-0
deces.2019_spread$`70-74`[is.na(deces.2019_spread$`70-74`)]<-0
deces.2019_spread$`75-79`[is.na(deces.2019_spread$`75-79`)]<-0
deces.2019_spread$`80+`[is.na(deces.2019_spread$`80+`)]<-0



#deces.2019_spread<-left_join(deces.2019_spread, deces.2019_spread)



deces.2019_spread$year<-2019


#fwrite(deces.2019_spread,"/fichier deces insee/décès travaillé/deces.2019_age_sexe.csv")




#################### partie 2 ######


RP_2019_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2019_age_sexe_final_2")

RP_2019_age_sexe_final_2<-RP_2019_age_sexe_final_2[,c(2:15)]

names(RP_2019_age_sexe_final_2)[names(RP_2019_age_sexe_final_2)=="COM_AP"]<-"COM"


RP_2019_age_sexe_final_2<-filter(RP_2019_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

Base_2019_mortalite<-left_join(deces.2019_spread,RP_2019_age_sexe_final_2)


Base_2019_mortalite$`75+`<-Base_2019_mortalite$`75-79`+Base_2019_mortalite$`80+`
Base_2019_mortalite$value_estimated_sum_75_plus_h_f<-Base_2019_mortalite$value_estimated_sum_75_79_h_f+Base_2019_mortalite$value_estimated_sum_80_plus_h_f


#Base_2019_mortalite$taux_mortalite_homme<-Base_2019_mortalite$Homme/Base_2019_mortalite$value_estimated_sum_homme

#Base_2019_mortalite$taux_mortalite_femme<-Base_2019_mortalite$Femme/Base_2019_mortalite$value_estimated_sum_femme

Base_2019_mortalite$taux_mortalite_0_9<-Base_2019_mortalite$`0-9`/Base_2019_mortalite$value_estimated_sum_0_9_h_f

Base_2019_mortalite$taux_mortalite_10_19<-Base_2019_mortalite$`10-19`/Base_2019_mortalite$value_estimated_sum_10_19_h_f

Base_2019_mortalite$taux_mortalite_20_39<-Base_2019_mortalite$`20-39`/Base_2019_mortalite$value_estimated_sum_20_39_h_f

Base_2019_mortalite$taux_mortalite_40_59<-Base_2019_mortalite$`40-59`/Base_2019_mortalite$value_estimated_sum_40_59_h_f

Base_2019_mortalite$taux_mortalite_60_64<-Base_2019_mortalite$`60-64`/Base_2019_mortalite$value_estimated_sum_60_64_h_f

Base_2019_mortalite$taux_mortalite_65_69<-Base_2019_mortalite$`65-69`/Base_2019_mortalite$value_estimated_sum_65_69_h_f

Base_2019_mortalite$taux_mortalite_70_74<-Base_2019_mortalite$`70-74`/Base_2019_mortalite$value_estimated_sum_70_74_h_f

Base_2019_mortalite$taux_mortalite_75_79<-Base_2019_mortalite$`75-79`/Base_2019_mortalite$value_estimated_sum_75_79_h_f

Base_2019_mortalite$taux_mortalite_75_plus<-Base_2019_mortalite$`75+`/Base_2019_mortalite$value_estimated_sum_75_plus_h_f

Base_2019_mortalite$taux_mortalite_80_plus<-Base_2019_mortalite$`80+`/Base_2019_mortalite$value_estimated_sum_80_plus_h_f

#Base_2019_mortalite$mort_total<-Base_2019_mortalite$Femme+Base_2019_mortalite$Homme

Base_2019_mortalite$mort_total<-Base_2019_mortalite$`0-9`+Base_2019_mortalite$`10-19`+Base_2019_mortalite$`20-39` +Base_2019_mortalite$`40-59` +Base_2019_mortalite$`60-64` +Base_2019_mortalite$`65-69` +Base_2019_mortalite$`70-74` +Base_2019_mortalite$`75+`



Base_2019_mortalite$taux_mortalite_total<-Base_2019_mortalite$mort_total/Base_2019_mortalite$value_estimated_population


Base_2019_mortalite<-filter(Base_2019_mortalite, Base_2019_mortalite$value_estimated_population>0)


Base_2019_mortalite<-Base_2019_mortalite[,c(1:2,12,24,27:36,38)]


#Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_femme))

#Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_homme))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_0_9))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_10_19))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_20_39))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_40_59))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_60_64))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_65_69))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_70_74))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_75_79))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_80_plus))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_75_plus))

Base_2019_mortalite<-filter(Base_2019_mortalite,  !is.infinite(taux_mortalite_total))




fwrite(Base_2019_mortalite,"/heatwave and mortality code and data/new data/Base_2019_mortalite.csv")




rm(list = ls())
gc()


#################################### 








communes_dates_1980_2022_temperature_final_mois<-fread("/heatwave and mortality code and data/base_donnees_final_heatwave.csv")

communes_date<-as.data.frame(table(communes_dates_1980_2022_temperature_final_mois$COM))
rm(communes_dates_1980_2022_temperature_final_mois)

names(communes_date)[names(communes_date)=="Var1"]<-"COM"

# Créer un vecteur des années et des mois
years <- 1980:2021
months <- 1:12

# Générer toutes les combinaisons possibles des communes, années et mois
combinations <- expand.grid(COM = communes_date$COM, year = years, month = months)

# Convertir en data.table pour plus de rapidité si besoin
setDT(combinations)


#fwrite(combinations,"/heatwave and mortality code and data/municipality_month_year.csv")


communes_month_year<-fread("/heatwave and mortality code and data/municipality_month_year.csv")


Base_1980_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1980_mortalite.csv")
Base_1981_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1981_mortalite.csv")
Base_1982_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1982_mortalite.csv")
Base_1983_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1983_mortalite.csv")
Base_1984_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1984_mortalite.csv")
Base_1985_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1985_mortalite.csv")
Base_1986_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1986_mortalite.csv")
Base_1987_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1987_mortalite.csv")
Base_1988_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1988_mortalite.csv")
Base_1989_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1989_mortalite.csv")
Base_1990_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1990_mortalite.csv")
Base_1991_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1991_mortalite.csv")
Base_1992_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1992_mortalite.csv")
Base_1993_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1993_mortalite.csv")
Base_1994_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1994_mortalite.csv")
Base_1995_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1995_mortalite.csv")
Base_1996_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1996_mortalite.csv")
Base_1997_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1997_mortalite.csv")
Base_1998_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1998_mortalite.csv")
Base_1999_mortalite<-fread("/heatwave and mortality code and data/new data/Base_1999_mortalite.csv")
Base_2000_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2000_mortalite.csv")
Base_2001_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2001_mortalite.csv")
Base_2002_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2002_mortalite.csv")
Base_2003_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2003_mortalite.csv")
Base_2004_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2004_mortalite.csv")
Base_2005_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2005_mortalite.csv")
Base_2006_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2006_mortalite.csv")
Base_2007_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2007_mortalite.csv")
Base_2008_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2008_mortalite.csv")
Base_2009_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2009_mortalite.csv")
Base_2010_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2010_mortalite.csv")
Base_2011_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2011_mortalite.csv")
Base_2012_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2012_mortalite.csv")
Base_2013_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2013_mortalite.csv")
Base_2014_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2014_mortalite.csv")
Base_2015_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2015_mortalite.csv")
Base_2016_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2016_mortalite.csv")
Base_2017_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2017_mortalite.csv")
Base_2018_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2018_mortalite.csv")
Base_2019_mortalite<-fread("/heatwave and mortality code and data/new data/Base_2019_mortalite.csv")

Base_1980_2019_mortalite<-rbind(Base_1980_mortalite,Base_1981_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1982_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1983_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1984_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1985_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1986_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1987_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1988_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1989_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1990_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1991_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1992_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1993_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1994_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1995_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1996_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1997_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1998_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_1999_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2000_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2001_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2002_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2003_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2004_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2005_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2006_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2007_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2008_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2009_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2010_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2011_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2012_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2013_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2014_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2015_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2016_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2017_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2018_mortalite)
Base_1980_2019_mortalite<-rbind(Base_1980_2019_mortalite,Base_2019_mortalite)




RP_1980_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1980_age_sexe_final_2")

RP_1980_age_sexe_final_2<-RP_1980_age_sexe_final_2[,c(2:15)]

names(RP_1980_age_sexe_final_2)[names(RP_1980_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1980_age_sexe_final_2<-filter(RP_1980_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1980_age_sexe_final_2<-RP_1980_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1981_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1981_age_sexe_final_2")

RP_1981_age_sexe_final_2<-RP_1981_age_sexe_final_2[,c(2:15)]

names(RP_1981_age_sexe_final_2)[names(RP_1981_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1981_age_sexe_final_2<-filter(RP_1981_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1981_age_sexe_final_2<-RP_1981_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1982_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1982_age_sexe_final_2")

RP_1982_age_sexe_final_2<-RP_1982_age_sexe_final_2[,c(2:15)]

names(RP_1982_age_sexe_final_2)[names(RP_1982_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1982_age_sexe_final_2<-filter(RP_1982_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1982_age_sexe_final_2<-RP_1982_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1983_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1983_age_sexe_final_2")

RP_1983_age_sexe_final_2<-RP_1983_age_sexe_final_2[,c(2:15)]

names(RP_1983_age_sexe_final_2)[names(RP_1983_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1983_age_sexe_final_2<-filter(RP_1983_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1983_age_sexe_final_2<-RP_1983_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1984_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1984_age_sexe_final_2")

RP_1984_age_sexe_final_2<-RP_1984_age_sexe_final_2[,c(2:15)]

names(RP_1984_age_sexe_final_2)[names(RP_1984_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1984_age_sexe_final_2<-filter(RP_1984_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1984_age_sexe_final_2<-RP_1984_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1985_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1985_age_sexe_final_2")

RP_1985_age_sexe_final_2<-RP_1985_age_sexe_final_2[,c(2:15)]

names(RP_1985_age_sexe_final_2)[names(RP_1985_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1985_age_sexe_final_2<-filter(RP_1985_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1985_age_sexe_final_2<-RP_1985_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1986_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1986_age_sexe_final_2")

RP_1986_age_sexe_final_2<-RP_1986_age_sexe_final_2[,c(2:15)]

names(RP_1986_age_sexe_final_2)[names(RP_1986_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1986_age_sexe_final_2<-filter(RP_1986_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1986_age_sexe_final_2<-RP_1986_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1987_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1987_age_sexe_final_2")

RP_1987_age_sexe_final_2<-RP_1987_age_sexe_final_2[,c(2:15)]

names(RP_1987_age_sexe_final_2)[names(RP_1987_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1987_age_sexe_final_2<-filter(RP_1987_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1987_age_sexe_final_2<-RP_1987_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1988_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1988_age_sexe_final_2")

RP_1988_age_sexe_final_2<-RP_1988_age_sexe_final_2[,c(2:15)]

names(RP_1988_age_sexe_final_2)[names(RP_1988_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1988_age_sexe_final_2<-filter(RP_1988_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1988_age_sexe_final_2<-RP_1988_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1989_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1989_age_sexe_final_2")

RP_1989_age_sexe_final_2<-RP_1989_age_sexe_final_2[,c(2:15)]

names(RP_1989_age_sexe_final_2)[names(RP_1989_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1989_age_sexe_final_2<-filter(RP_1989_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1989_age_sexe_final_2<-RP_1989_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1990_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1990_age_sexe_final_2")

RP_1990_age_sexe_final_2<-RP_1990_age_sexe_final_2[,c(2:15)]

names(RP_1990_age_sexe_final_2)[names(RP_1990_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1990_age_sexe_final_2<-filter(RP_1990_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1990_age_sexe_final_2<-RP_1990_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1991_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1991_age_sexe_final_2")

RP_1991_age_sexe_final_2<-RP_1991_age_sexe_final_2[,c(2:15)]

names(RP_1991_age_sexe_final_2)[names(RP_1991_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1991_age_sexe_final_2<-filter(RP_1991_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1991_age_sexe_final_2<-RP_1991_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1992_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1992_age_sexe_final_2")

RP_1992_age_sexe_final_2<-RP_1992_age_sexe_final_2[,c(2:15)]

names(RP_1992_age_sexe_final_2)[names(RP_1992_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1992_age_sexe_final_2<-filter(RP_1992_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1992_age_sexe_final_2<-RP_1992_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1993_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1993_age_sexe_final_2")

RP_1993_age_sexe_final_2<-RP_1993_age_sexe_final_2[,c(2:15)]

names(RP_1993_age_sexe_final_2)[names(RP_1993_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1993_age_sexe_final_2<-filter(RP_1993_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1993_age_sexe_final_2<-RP_1993_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1994_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1994_age_sexe_final_2")

RP_1994_age_sexe_final_2<-RP_1994_age_sexe_final_2[,c(2:15)]

names(RP_1994_age_sexe_final_2)[names(RP_1994_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1994_age_sexe_final_2<-filter(RP_1994_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1994_age_sexe_final_2<-RP_1994_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1995_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1995_age_sexe_final_2")

RP_1995_age_sexe_final_2<-RP_1995_age_sexe_final_2[,c(2:15)]

names(RP_1995_age_sexe_final_2)[names(RP_1995_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1995_age_sexe_final_2<-filter(RP_1995_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1995_age_sexe_final_2<-RP_1995_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1996_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1996_age_sexe_final_2")

RP_1996_age_sexe_final_2<-RP_1996_age_sexe_final_2[,c(2:15)]

names(RP_1996_age_sexe_final_2)[names(RP_1996_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1996_age_sexe_final_2<-filter(RP_1996_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1996_age_sexe_final_2<-RP_1996_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1997_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1997_age_sexe_final_2")

RP_1997_age_sexe_final_2<-RP_1997_age_sexe_final_2[,c(2:15)]

names(RP_1997_age_sexe_final_2)[names(RP_1997_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1997_age_sexe_final_2<-filter(RP_1997_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1997_age_sexe_final_2<-RP_1997_age_sexe_final_2[,c("COM","year","value_estimated_population")]


RP_1998_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1998_age_sexe_final_2")

RP_1998_age_sexe_final_2<-RP_1998_age_sexe_final_2[,c(2:15)]

names(RP_1998_age_sexe_final_2)[names(RP_1998_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1998_age_sexe_final_2<-filter(RP_1998_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1998_age_sexe_final_2<-RP_1998_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_1999_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_1999_age_sexe_final_2")

RP_1999_age_sexe_final_2<-RP_1999_age_sexe_final_2[,c(2:15)]

names(RP_1999_age_sexe_final_2)[names(RP_1999_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_1999_age_sexe_final_2<-filter(RP_1999_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_1999_age_sexe_final_2<-RP_1999_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2000_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2000_age_sexe_final_2")

RP_2000_age_sexe_final_2<-RP_2000_age_sexe_final_2[,c(2:15)]

names(RP_2000_age_sexe_final_2)[names(RP_2000_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2000_age_sexe_final_2<-filter(RP_2000_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2000_age_sexe_final_2<-RP_2000_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2001_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2001_age_sexe_final_2")

RP_2001_age_sexe_final_2<-RP_2001_age_sexe_final_2[,c(2:15)]

names(RP_2001_age_sexe_final_2)[names(RP_2001_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2001_age_sexe_final_2<-filter(RP_2001_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2001_age_sexe_final_2<-RP_2001_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2002_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2002_age_sexe_final_2")

RP_2002_age_sexe_final_2<-RP_2002_age_sexe_final_2[,c(2:15)]

names(RP_2002_age_sexe_final_2)[names(RP_2002_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2002_age_sexe_final_2<-filter(RP_2002_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2002_age_sexe_final_2<-RP_2002_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2003_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2003_age_sexe_final_2")

RP_2003_age_sexe_final_2<-RP_2003_age_sexe_final_2[,c(2:15)]

names(RP_2003_age_sexe_final_2)[names(RP_2003_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2003_age_sexe_final_2<-filter(RP_2003_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2003_age_sexe_final_2<-RP_2003_age_sexe_final_2[,c("COM","year","value_estimated_population")]


RP_2004_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2004_age_sexe_final_2")

RP_2004_age_sexe_final_2<-RP_2004_age_sexe_final_2[,c(2:15)]

names(RP_2004_age_sexe_final_2)[names(RP_2004_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2004_age_sexe_final_2<-filter(RP_2004_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2004_age_sexe_final_2<-RP_2004_age_sexe_final_2[,c("COM","year","value_estimated_population")]


RP_2005_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2005_age_sexe_final_2")

RP_2005_age_sexe_final_2<-RP_2005_age_sexe_final_2[,c(2:15)]

names(RP_2005_age_sexe_final_2)[names(RP_2005_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2005_age_sexe_final_2<-filter(RP_2005_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2005_age_sexe_final_2<-RP_2005_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2006_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2006_age_sexe_final_2")

RP_2006_age_sexe_final_2<-RP_2006_age_sexe_final_2[,c(2:15)]

names(RP_2006_age_sexe_final_2)[names(RP_2006_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2006_age_sexe_final_2<-filter(RP_2006_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2006_age_sexe_final_2<-RP_2006_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2007_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2007_age_sexe_final_2")

RP_2007_age_sexe_final_2<-RP_2007_age_sexe_final_2[,c(2:15)]

names(RP_2007_age_sexe_final_2)[names(RP_2007_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2007_age_sexe_final_2<-filter(RP_2007_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2007_age_sexe_final_2<-RP_2007_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2008_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2008_age_sexe_final_2")

RP_2008_age_sexe_final_2<-RP_2008_age_sexe_final_2[,c(2:15)]

names(RP_2008_age_sexe_final_2)[names(RP_2008_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2008_age_sexe_final_2<-filter(RP_2008_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2008_age_sexe_final_2<-RP_2008_age_sexe_final_2[,c("COM","year","value_estimated_population")]


RP_2009_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2009_age_sexe_final_2")

RP_2009_age_sexe_final_2<-RP_2009_age_sexe_final_2[,c(2:15)]

names(RP_2009_age_sexe_final_2)[names(RP_2009_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2009_age_sexe_final_2<-filter(RP_2009_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2009_age_sexe_final_2<-RP_2009_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2010_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2010_age_sexe_final_2")

RP_2010_age_sexe_final_2<-RP_2010_age_sexe_final_2[,c(2:15)]

names(RP_2010_age_sexe_final_2)[names(RP_2010_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2010_age_sexe_final_2<-filter(RP_2010_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2010_age_sexe_final_2<-RP_2010_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2011_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2011_age_sexe_final_2")

RP_2011_age_sexe_final_2<-RP_2011_age_sexe_final_2[,c(2:15)]

names(RP_2011_age_sexe_final_2)[names(RP_2011_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2011_age_sexe_final_2<-filter(RP_2011_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2011_age_sexe_final_2<-RP_2011_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2012_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2012_age_sexe_final_2")

RP_2012_age_sexe_final_2<-RP_2012_age_sexe_final_2[,c(2:15)]

names(RP_2012_age_sexe_final_2)[names(RP_2012_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2012_age_sexe_final_2<-filter(RP_2012_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2012_age_sexe_final_2<-RP_2012_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2013_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2013_age_sexe_final_2")

RP_2013_age_sexe_final_2<-RP_2013_age_sexe_final_2[,c(2:15)]

names(RP_2013_age_sexe_final_2)[names(RP_2013_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2013_age_sexe_final_2<-filter(RP_2013_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2013_age_sexe_final_2<-RP_2013_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2014_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2014_age_sexe_final_2")

RP_2014_age_sexe_final_2<-RP_2014_age_sexe_final_2[,c(2:15)]

names(RP_2014_age_sexe_final_2)[names(RP_2014_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2014_age_sexe_final_2<-filter(RP_2014_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2014_age_sexe_final_2<-RP_2014_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2015_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2015_age_sexe_final_2")

RP_2015_age_sexe_final_2<-RP_2015_age_sexe_final_2[,c(2:15)]

names(RP_2015_age_sexe_final_2)[names(RP_2015_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2015_age_sexe_final_2<-filter(RP_2015_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2015_age_sexe_final_2<-RP_2015_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2016_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2016_age_sexe_final_2")

RP_2016_age_sexe_final_2<-RP_2016_age_sexe_final_2[,c(2:15)]

names(RP_2016_age_sexe_final_2)[names(RP_2016_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2016_age_sexe_final_2<-filter(RP_2016_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2016_age_sexe_final_2<-RP_2016_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2017_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2017_age_sexe_final_2")

RP_2017_age_sexe_final_2<-RP_2017_age_sexe_final_2[,c(2:15)]

names(RP_2017_age_sexe_final_2)[names(RP_2017_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2017_age_sexe_final_2<-filter(RP_2017_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2017_age_sexe_final_2<-RP_2017_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2018_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2018_age_sexe_final_2")

RP_2018_age_sexe_final_2<-RP_2018_age_sexe_final_2[,c(2:15)]

names(RP_2018_age_sexe_final_2)[names(RP_2018_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2018_age_sexe_final_2<-filter(RP_2018_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2018_age_sexe_final_2<-RP_2018_age_sexe_final_2[,c("COM","year","value_estimated_population")]



RP_2019_age_sexe_final_2 <- read_csv("recensement_heatwaves/rp travaillé/RP_2019_age_sexe_final_2")

RP_2019_age_sexe_final_2<-RP_2019_age_sexe_final_2[,c(2:15)]

names(RP_2019_age_sexe_final_2)[names(RP_2019_age_sexe_final_2)=="COM_AP"]<-"COM"

RP_2019_age_sexe_final_2<-filter(RP_2019_age_sexe_final_2, !is.na(value_estimated_sum_homme) )

RP_2019_age_sexe_final_2<-RP_2019_age_sexe_final_2[,c("COM","year","value_estimated_population")]






RP_population_total<-rbind(RP_1980_age_sexe_final_2,RP_1981_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1982_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1983_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1984_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1985_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1986_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1987_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1988_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1989_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1990_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1991_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1992_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1993_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1994_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1995_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1996_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1997_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1998_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_1999_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2000_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2001_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2002_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2003_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2004_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2005_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2006_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2007_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2008_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2009_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2010_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2011_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2012_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2013_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2014_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2015_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2016_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2017_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2018_age_sexe_final_2)
RP_population_total<-rbind(RP_population_total,RP_2019_age_sexe_final_2)





#Base_1980_2019_population_year<-Base_1980_2019_mortalite[,c("COM","year","value_estimated_population")]
#Base_1980_2019_population_year_unique <- Base_1980_2019_population_year[!duplicated(Base_1980_2019_population_year), ]

Base_1980_2019_mortalite<-Base_1980_2019_mortalite[,c(-4)]


base_finale_mortalite_NEW<- left_join(communes_month_year,Base_1980_2019_mortalite)


base_finale_mortalite_NEW[is.na(base_finale_mortalite_NEW)] <- 0


base_finale_mortalite_NEW<-left_join(base_finale_mortalite_NEW,RP_population_total)





fwrite(base_finale_mortalite_NEW,"/heatwave and mortality code and data/new data/base_finale_mortalite_NEW.csv")


