


#code for creating age and gender demographic data


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
library(readxl)



RP_1975_age_sexe <- read_excel("recensement 1980-2022/RP_1975_age_sexe.xlsx")

names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1975_age_sexe$COM <- paste(RP_1975_age_sexe$DEP_geo_courante, RP_1975_age_sexe$COM_geo_courante, sep = "")


names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 0 à 4 ans\r\nHommes\r\nRP1975"]<-"0_4_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 5 à 9 ans\r\nHommes\r\nRP1975"]<-"5_9_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 10 à 14 ans\r\nHommes\r\nRP1975"]<-"10_14_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 15 à 19 ans\r\nHommes\r\nRP1975"]<-"15_19_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 20 à 24 ans\r\nHommes\r\nRP1975"]<-"20_24_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 25 à 29 ans\r\nHommes\r\nRP1975"]<-"25_29_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 30 à 34 ans\r\nHommes\r\nRP1975"]<-"30_34_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 35 à 39 ans\r\nHommes\r\nRP1975"]<-"35_39_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 40 à 44 ans\r\nHommes\r\nRP1975"]<-"40_44_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 45 à 49 ans\r\nHommes\r\nRP1975"]<-"45_49_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 50 à 54 ans\r\nHommes\r\nRP1975"]<-"50_54_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 55 à 59 ans\r\nHommes\r\nRP1975"]<-"55_59_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 60 à 64 ans\r\nHommes\r\nRP1975"]<-"60_64_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 65 à 69 ans\r\nHommes\r\nRP1975"]<-"65_69_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 70 à 74 ans\r\nHommes\r\nRP1975"]<-"70_74_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 75 à 79 ans\r\nHommes\r\nRP1975"]<-"75_79_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 80 à 84 ans\r\nHommes\r\nRP1975"]<-"80_84_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 85 à 89 ans\r\nHommes\r\nRP1975"]<-"85_89_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 90 à 94 ans\r\nHommes\r\nRP1975"]<-"90_94_H"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="95 ans et plus\r\nHommes\r\nRP1975"]<-"90_plus_H"





names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 0 à 4 ans\r\nFemmes\r\nRP1975"]<-"0_4_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 5 à 9 ans\r\nFemmes\r\nRP1975"]<-"5_9_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 10 à 14 ans\r\nFemmes\r\nRP1975"]<-"10_14_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 15 à 19 ans\r\nFemmes\r\nRP1975"]<-"15_19_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 20 à 24 ans\r\nFemmes\r\nRP1975"]<-"20_24_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 25 à 29 ans\r\nFemmes\r\nRP1975"]<-"25_29_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 30 à 34 ans\r\nFemmes\r\nRP1975"]<-"30_34_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 35 à 39 ans\r\nFemmes\r\nRP1975"]<-"35_39_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 40 à 44 ans\r\nFemmes\r\nRP1975"]<-"40_44_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 45 à 49 ans\r\nFemmes\r\nRP1975"]<-"45_49_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 50 à 54 ans\r\nFemmes\r\nRP1975"]<-"50_54_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 55 à 59 ans\r\nFemmes\r\nRP1975"]<-"55_59_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 60 à 64 ans\r\nFemmes\r\nRP1975"]<-"60_64_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 65 à 69 ans\r\nFemmes\r\nRP1975"]<-"65_69_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 70 à 74 ans\r\nFemmes\r\nRP1975"]<-"70_74_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 75 à 79 ans\r\nFemmes\r\nRP1975"]<-"75_79_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 80 à 84 ans\r\nFemmes\r\nRP1975"]<-"80_84_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 85 à 89 ans\r\nFemmes\r\nRP1975"]<-"85_89_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="De 90 à 94 ans\r\nFemmes\r\nRP1975"]<-"90_94_F"
names(RP_1975_age_sexe)[names(RP_1975_age_sexe)=="95 ans et plus\r\nFemmes\r\nRP1975"]<-"90_plus_F"




RP_1975_age_sexe$sum_0_9_h_f<-RP_1975_age_sexe$`0_4_H` + RP_1975_age_sexe$`0_4_F` + RP_1975_age_sexe$`5_9_H` + RP_1975_age_sexe$`5_9_F`


RP_1975_age_sexe$sum_10_19_h_f<-RP_1975_age_sexe$`10_14_H` + RP_1975_age_sexe$`10_14_F` + RP_1975_age_sexe$`15_19_H` + RP_1975_age_sexe$`15_19_F`


RP_1975_age_sexe$sum_20_39_h_f<-RP_1975_age_sexe$`20_24_H` + RP_1975_age_sexe$`20_24_F` + RP_1975_age_sexe$`25_29_H` + RP_1975_age_sexe$`25_29_F` + RP_1975_age_sexe$`30_34_H` + RP_1975_age_sexe$`30_34_F` + RP_1975_age_sexe$`35_39_H` + RP_1975_age_sexe$`35_39_F`


RP_1975_age_sexe$sum_40_59_h_f<-RP_1975_age_sexe$`40_44_H` + RP_1975_age_sexe$`40_44_F` + RP_1975_age_sexe$`45_49_H` + RP_1975_age_sexe$`45_49_F` + RP_1975_age_sexe$`50_54_H` + RP_1975_age_sexe$`50_54_F` + RP_1975_age_sexe$`55_59_H` + RP_1975_age_sexe$`55_59_F`


RP_1975_age_sexe$sum_60_64_h_f<-RP_1975_age_sexe$`60_64_H` + RP_1975_age_sexe$`60_64_F` 


RP_1975_age_sexe$sum_65_69_h_f<-RP_1975_age_sexe$`65_69_H` + RP_1975_age_sexe$`65_69_F` 

RP_1975_age_sexe$sum_70_74_h_f<-RP_1975_age_sexe$`70_74_H` + RP_1975_age_sexe$`70_74_F` 

RP_1975_age_sexe$sum_75_79_h_f<-RP_1975_age_sexe$`75_79_H` + RP_1975_age_sexe$`75_79_F` 

RP_1975_age_sexe$sum_80_plus_h_f<-RP_1975_age_sexe$`80_84_H` + RP_1975_age_sexe$`80_84_F` + RP_1975_age_sexe$`85_89_H` + RP_1975_age_sexe$`85_89_F` + RP_1975_age_sexe$`90_94_H` + RP_1975_age_sexe$`90_94_F` + RP_1975_age_sexe$`90_plus_H` + RP_1975_age_sexe$`90_plus_F`


RP_1975_age_sexe$population<- rowSums(RP_1975_age_sexe[,7:46])



RP_1975_age_sexe$sum_homme<-RP_1975_age_sexe$`0_4_H` + RP_1975_age_sexe$`5_9_H` + RP_1975_age_sexe$`10_14_H` + RP_1975_age_sexe$`15_19_H` + RP_1975_age_sexe$`20_24_H` + RP_1975_age_sexe$`25_29_H` + RP_1975_age_sexe$`30_34_H` + RP_1975_age_sexe$`35_39_H`+ RP_1975_age_sexe$`40_44_H`+ RP_1975_age_sexe$`45_49_H`+ RP_1975_age_sexe$`50_54_H`+ RP_1975_age_sexe$`55_59_H`+ RP_1975_age_sexe$`60_64_H`+ RP_1975_age_sexe$`65_69_H`+ RP_1975_age_sexe$`70_74_H`+ RP_1975_age_sexe$`75_79_H`+ RP_1975_age_sexe$`80_84_H`+ RP_1975_age_sexe$`85_89_H`+ RP_1975_age_sexe$`90_94_H`+ RP_1975_age_sexe$`90_plus_H`

RP_1975_age_sexe$sum_femme<-RP_1975_age_sexe$`0_4_F` + RP_1975_age_sexe$`5_9_F` + RP_1975_age_sexe$`10_14_F` + RP_1975_age_sexe$`15_19_F` + RP_1975_age_sexe$`20_24_F` + RP_1975_age_sexe$`25_29_F` + RP_1975_age_sexe$`30_34_F` + RP_1975_age_sexe$`35_39_F`+ RP_1975_age_sexe$`40_44_F`+ RP_1975_age_sexe$`45_49_F`+ RP_1975_age_sexe$`50_54_F`+ RP_1975_age_sexe$`55_59_F`+ RP_1975_age_sexe$`60_64_F`+ RP_1975_age_sexe$`65_69_F`+ RP_1975_age_sexe$`70_74_F`+ RP_1975_age_sexe$`75_79_F`+ RP_1975_age_sexe$`80_84_F`+ RP_1975_age_sexe$`85_89_F`+ RP_1975_age_sexe$`90_94_F`+ RP_1975_age_sexe$`90_plus_F`



RP_1975_age_sexe<-RP_1975_age_sexe[,47:59]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1975_age_sexe<-left_join(RP_1975_age_sexe,table_passage_bis)

RP_1975_age_sexe$COM_AP<-ifelse(!is.na(RP_1975_age_sexe$COM_AP),RP_1975_age_sexe$COM_AP,RP_1975_age_sexe$COM)


ag_1<-aggregate(sum_homme~COM_AP,RP_1975_age_sexe,sum)
ag_2<-aggregate(sum_femme~COM_AP,RP_1975_age_sexe,sum)
ag_3<-aggregate(sum_0_9_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_4<-aggregate(sum_10_19_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_5<-aggregate(sum_20_39_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_6<-aggregate(sum_40_59_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_7<-aggregate(sum_60_64_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_8<-aggregate(sum_65_69_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_9<-aggregate(sum_70_74_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_10<-aggregate(sum_75_79_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_11<-aggregate(sum_80_plus_h_f~COM_AP,RP_1975_age_sexe,sum)
ag_12<-aggregate(population~COM_AP,RP_1975_age_sexe,sum)

RP_1975_age_sexe_final<-left_join(ag_1,ag_2)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_3)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_4)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_5)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_6)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_7)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_8)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_9)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_10)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_11)
RP_1975_age_sexe_final<-left_join(RP_1975_age_sexe_final,ag_12)



write.csv2(RP_1975_age_sexe_final, "recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final")

#library(readr)
#RP_1975_age_sexe_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)



##############


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
library(readxl)



RP_1982_age_sexe <- read_excel("recensement 1980-2022/RP_1982_age_sexe.xlsx")

names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1982_age_sexe$COM <- paste(RP_1982_age_sexe$DEP_geo_courante, RP_1982_age_sexe$COM_geo_courante, sep = "")


names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 0 à 4 ans\r\nHommes\r\nRP1982"]<-"0_4_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 5 à 9 ans\r\nHommes\r\nRP1982"]<-"5_9_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 10 à 14 ans\r\nHommes\r\nRP1982"]<-"10_14_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 15 à 19 ans\r\nHommes\r\nRP1982"]<-"15_19_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 20 à 24 ans\r\nHommes\r\nRP1982"]<-"20_24_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 25 à 29 ans\r\nHommes\r\nRP1982"]<-"25_29_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 30 à 34 ans\r\nHommes\r\nRP1982"]<-"30_34_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 35 à 39 ans\r\nHommes\r\nRP1982"]<-"35_39_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 40 à 44 ans\r\nHommes\r\nRP1982"]<-"40_44_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 45 à 49 ans\r\nHommes\r\nRP1982"]<-"45_49_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 50 à 54 ans\r\nHommes\r\nRP1982"]<-"50_54_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 55 à 59 ans\r\nHommes\r\nRP1982"]<-"55_59_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 60 à 64 ans\r\nHommes\r\nRP1982"]<-"60_64_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 65 à 69 ans\r\nHommes\r\nRP1982"]<-"65_69_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 70 à 74 ans\r\nHommes\r\nRP1982"]<-"70_74_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 75 à 79 ans\r\nHommes\r\nRP1982"]<-"75_79_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 80 à 84 ans\r\nHommes\r\nRP1982"]<-"80_84_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 85 à 89 ans\r\nHommes\r\nRP1982"]<-"85_89_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 90 à 94 ans\r\nHommes\r\nRP1982"]<-"90_94_H"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="95 ans et plus\r\nHommes\r\nRP1982"]<-"90_plus_H"





names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 0 à 4 ans\r\nFemmes\r\nRP1982"]<-"0_4_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 5 à 9 ans\r\nFemmes\r\nRP1982"]<-"5_9_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 10 à 14 ans\r\nFemmes\r\nRP1982"]<-"10_14_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 15 à 19 ans\r\nFemmes\r\nRP1982"]<-"15_19_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 20 à 24 ans\r\nFemmes\r\nRP1982"]<-"20_24_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 25 à 29 ans\r\nFemmes\r\nRP1982"]<-"25_29_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 30 à 34 ans\r\nFemmes\r\nRP1982"]<-"30_34_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 35 à 39 ans\r\nFemmes\r\nRP1982"]<-"35_39_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 40 à 44 ans\r\nFemmes\r\nRP1982"]<-"40_44_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 45 à 49 ans\r\nFemmes\r\nRP1982"]<-"45_49_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 50 à 54 ans\r\nFemmes\r\nRP1982"]<-"50_54_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 55 à 59 ans\r\nFemmes\r\nRP1982"]<-"55_59_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 60 à 64 ans\r\nFemmes\r\nRP1982"]<-"60_64_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 65 à 69 ans\r\nFemmes\r\nRP1982"]<-"65_69_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 70 à 74 ans\r\nFemmes\r\nRP1982"]<-"70_74_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 75 à 79 ans\r\nFemmes\r\nRP1982"]<-"75_79_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 80 à 84 ans\r\nFemmes\r\nRP1982"]<-"80_84_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 85 à 89 ans\r\nFemmes\r\nRP1982"]<-"85_89_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="De 90 à 94 ans\r\nFemmes\r\nRP1982"]<-"90_94_F"
names(RP_1982_age_sexe)[names(RP_1982_age_sexe)=="95 ans et plus\r\nFemmes\r\nRP1982"]<-"90_plus_F"




RP_1982_age_sexe$sum_0_9_h_f<-RP_1982_age_sexe$`0_4_H` + RP_1982_age_sexe$`0_4_F` + RP_1982_age_sexe$`5_9_H` + RP_1982_age_sexe$`5_9_F`


RP_1982_age_sexe$sum_10_19_h_f<-RP_1982_age_sexe$`10_14_H` + RP_1982_age_sexe$`10_14_F` + RP_1982_age_sexe$`15_19_H` + RP_1982_age_sexe$`15_19_F`


RP_1982_age_sexe$sum_20_39_h_f<-RP_1982_age_sexe$`20_24_H` + RP_1982_age_sexe$`20_24_F` + RP_1982_age_sexe$`25_29_H` + RP_1982_age_sexe$`25_29_F` + RP_1982_age_sexe$`30_34_H` + RP_1982_age_sexe$`30_34_F` + RP_1982_age_sexe$`35_39_H` + RP_1982_age_sexe$`35_39_F`


RP_1982_age_sexe$sum_40_59_h_f<-RP_1982_age_sexe$`40_44_H` + RP_1982_age_sexe$`40_44_F` + RP_1982_age_sexe$`45_49_H` + RP_1982_age_sexe$`45_49_F` + RP_1982_age_sexe$`50_54_H` + RP_1982_age_sexe$`50_54_F` + RP_1982_age_sexe$`55_59_H` + RP_1982_age_sexe$`55_59_F`


RP_1982_age_sexe$sum_60_64_h_f<-RP_1982_age_sexe$`60_64_H` + RP_1982_age_sexe$`60_64_F` 


RP_1982_age_sexe$sum_65_69_h_f<-RP_1982_age_sexe$`65_69_H` + RP_1982_age_sexe$`65_69_F` 

RP_1982_age_sexe$sum_70_74_h_f<-RP_1982_age_sexe$`70_74_H` + RP_1982_age_sexe$`70_74_F` 

RP_1982_age_sexe$sum_75_79_h_f<-RP_1982_age_sexe$`75_79_H` + RP_1982_age_sexe$`75_79_F` 

RP_1982_age_sexe$sum_80_plus_h_f<-RP_1982_age_sexe$`80_84_H` + RP_1982_age_sexe$`80_84_F` + RP_1982_age_sexe$`85_89_H` + RP_1982_age_sexe$`85_89_F` + RP_1982_age_sexe$`90_94_H` + RP_1982_age_sexe$`90_94_F` + RP_1982_age_sexe$`90_plus_H` + RP_1982_age_sexe$`90_plus_F`


RP_1982_age_sexe$population<- rowSums(RP_1982_age_sexe[,7:46])



RP_1982_age_sexe$sum_homme<-RP_1982_age_sexe$`0_4_H` + RP_1982_age_sexe$`5_9_H` + RP_1982_age_sexe$`10_14_H` + RP_1982_age_sexe$`15_19_H` + RP_1982_age_sexe$`20_24_H` + RP_1982_age_sexe$`25_29_H` + RP_1982_age_sexe$`30_34_H` + RP_1982_age_sexe$`35_39_H`+ RP_1982_age_sexe$`40_44_H`+ RP_1982_age_sexe$`45_49_H`+ RP_1982_age_sexe$`50_54_H`+ RP_1982_age_sexe$`55_59_H`+ RP_1982_age_sexe$`60_64_H`+ RP_1982_age_sexe$`65_69_H`+ RP_1982_age_sexe$`70_74_H`+ RP_1982_age_sexe$`75_79_H`+ RP_1982_age_sexe$`80_84_H`+ RP_1982_age_sexe$`85_89_H`+ RP_1982_age_sexe$`90_94_H`+ RP_1982_age_sexe$`90_plus_H`

RP_1982_age_sexe$sum_femme<-RP_1982_age_sexe$`0_4_F` + RP_1982_age_sexe$`5_9_F` + RP_1982_age_sexe$`10_14_F` + RP_1982_age_sexe$`15_19_F` + RP_1982_age_sexe$`20_24_F` + RP_1982_age_sexe$`25_29_F` + RP_1982_age_sexe$`30_34_F` + RP_1982_age_sexe$`35_39_F`+ RP_1982_age_sexe$`40_44_F`+ RP_1982_age_sexe$`45_49_F`+ RP_1982_age_sexe$`50_54_F`+ RP_1982_age_sexe$`55_59_F`+ RP_1982_age_sexe$`60_64_F`+ RP_1982_age_sexe$`65_69_F`+ RP_1982_age_sexe$`70_74_F`+ RP_1982_age_sexe$`75_79_F`+ RP_1982_age_sexe$`80_84_F`+ RP_1982_age_sexe$`85_89_F`+ RP_1982_age_sexe$`90_94_F`+ RP_1982_age_sexe$`90_plus_F`



RP_1982_age_sexe<-RP_1982_age_sexe[,47:59]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1982_age_sexe<-left_join(RP_1982_age_sexe,table_passage_bis)

RP_1982_age_sexe$COM_AP<-ifelse(!is.na(RP_1982_age_sexe$COM_AP),RP_1982_age_sexe$COM_AP,RP_1982_age_sexe$COM)


ag_1<-aggregate(sum_homme~COM_AP,RP_1982_age_sexe,sum)
ag_2<-aggregate(sum_femme~COM_AP,RP_1982_age_sexe,sum)
ag_3<-aggregate(sum_0_9_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_4<-aggregate(sum_10_19_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_5<-aggregate(sum_20_39_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_6<-aggregate(sum_40_59_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_7<-aggregate(sum_60_64_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_8<-aggregate(sum_65_69_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_9<-aggregate(sum_70_74_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_10<-aggregate(sum_75_79_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_11<-aggregate(sum_80_plus_h_f~COM_AP,RP_1982_age_sexe,sum)
ag_12<-aggregate(population~COM_AP,RP_1982_age_sexe,sum)

RP_1982_age_sexe_final<-left_join(ag_1,ag_2)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_3)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_4)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_5)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_6)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_7)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_8)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_9)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_10)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_11)
RP_1982_age_sexe_final<-left_join(RP_1982_age_sexe_final,ag_12)



write.csv2(RP_1982_age_sexe_final, "recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final")

#library(readr)
#RP_1982_age_sexe_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




##############


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
library(readxl)



RP_1990_age_sexe <- read_excel("recensement 1980-2022/RP_1990_age_sexe.xlsx")

names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1990_age_sexe$COM <- paste(RP_1990_age_sexe$DEP_geo_courante, RP_1990_age_sexe$COM_geo_courante, sep = "")


names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 0 à 4 ans\r\nHommes\r\nRP1990"]<-"0_4_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 5 à 9 ans\r\nHommes\r\nRP1990"]<-"5_9_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 10 à 14 ans\r\nHommes\r\nRP1990"]<-"10_14_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 15 à 19 ans\r\nHommes\r\nRP1990"]<-"15_19_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 20 à 24 ans\r\nHommes\r\nRP1990"]<-"20_24_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 25 à 29 ans\r\nHommes\r\nRP1990"]<-"25_29_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 30 à 34 ans\r\nHommes\r\nRP1990"]<-"30_34_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 35 à 39 ans\r\nHommes\r\nRP1990"]<-"35_39_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 40 à 44 ans\r\nHommes\r\nRP1990"]<-"40_44_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 45 à 49 ans\r\nHommes\r\nRP1990"]<-"45_49_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 50 à 54 ans\r\nHommes\r\nRP1990"]<-"50_54_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 55 à 59 ans\r\nHommes\r\nRP1990"]<-"55_59_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 60 à 64 ans\r\nHommes\r\nRP1990"]<-"60_64_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 65 à 69 ans\r\nHommes\r\nRP1990"]<-"65_69_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 70 à 74 ans\r\nHommes\r\nRP1990"]<-"70_74_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 75 à 79 ans\r\nHommes\r\nRP1990"]<-"75_79_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 80 à 84 ans\r\nHommes\r\nRP1990"]<-"80_84_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 85 à 89 ans\r\nHommes\r\nRP1990"]<-"85_89_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 90 à 94 ans\r\nHommes\r\nRP1990"]<-"90_94_H"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="95 ans et plus\r\nHommes\r\nRP1990"]<-"90_plus_H"





names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 0 à 4 ans\r\nFemmes\r\nRP1990"]<-"0_4_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 5 à 9 ans\r\nFemmes\r\nRP1990"]<-"5_9_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 10 à 14 ans\r\nFemmes\r\nRP1990"]<-"10_14_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 15 à 19 ans\r\nFemmes\r\nRP1990"]<-"15_19_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 20 à 24 ans\r\nFemmes\r\nRP1990"]<-"20_24_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 25 à 29 ans\r\nFemmes\r\nRP1990"]<-"25_29_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 30 à 34 ans\r\nFemmes\r\nRP1990"]<-"30_34_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 35 à 39 ans\r\nFemmes\r\nRP1990"]<-"35_39_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 40 à 44 ans\r\nFemmes\r\nRP1990"]<-"40_44_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 45 à 49 ans\r\nFemmes\r\nRP1990"]<-"45_49_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 50 à 54 ans\r\nFemmes\r\nRP1990"]<-"50_54_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 55 à 59 ans\r\nFemmes\r\nRP1990"]<-"55_59_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 60 à 64 ans\r\nFemmes\r\nRP1990"]<-"60_64_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 65 à 69 ans\r\nFemmes\r\nRP1990"]<-"65_69_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 70 à 74 ans\r\nFemmes\r\nRP1990"]<-"70_74_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 75 à 79 ans\r\nFemmes\r\nRP1990"]<-"75_79_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 80 à 84 ans\r\nFemmes\r\nRP1990"]<-"80_84_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 85 à 89 ans\r\nFemmes\r\nRP1990"]<-"85_89_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="De 90 à 94 ans\r\nFemmes\r\nRP1990"]<-"90_94_F"
names(RP_1990_age_sexe)[names(RP_1990_age_sexe)=="95 ans et plus\r\nFemmes\r\nRP1990"]<-"90_plus_F"




RP_1990_age_sexe$sum_0_9_h_f<-RP_1990_age_sexe$`0_4_H` + RP_1990_age_sexe$`0_4_F` + RP_1990_age_sexe$`5_9_H` + RP_1990_age_sexe$`5_9_F`


RP_1990_age_sexe$sum_10_19_h_f<-RP_1990_age_sexe$`10_14_H` + RP_1990_age_sexe$`10_14_F` + RP_1990_age_sexe$`15_19_H` + RP_1990_age_sexe$`15_19_F`


RP_1990_age_sexe$sum_20_39_h_f<-RP_1990_age_sexe$`20_24_H` + RP_1990_age_sexe$`20_24_F` + RP_1990_age_sexe$`25_29_H` + RP_1990_age_sexe$`25_29_F` + RP_1990_age_sexe$`30_34_H` + RP_1990_age_sexe$`30_34_F` + RP_1990_age_sexe$`35_39_H` + RP_1990_age_sexe$`35_39_F`


RP_1990_age_sexe$sum_40_59_h_f<-RP_1990_age_sexe$`40_44_H` + RP_1990_age_sexe$`40_44_F` + RP_1990_age_sexe$`45_49_H` + RP_1990_age_sexe$`45_49_F` + RP_1990_age_sexe$`50_54_H` + RP_1990_age_sexe$`50_54_F` + RP_1990_age_sexe$`55_59_H` + RP_1990_age_sexe$`55_59_F`


RP_1990_age_sexe$sum_60_64_h_f<-RP_1990_age_sexe$`60_64_H` + RP_1990_age_sexe$`60_64_F` 


RP_1990_age_sexe$sum_65_69_h_f<-RP_1990_age_sexe$`65_69_H` + RP_1990_age_sexe$`65_69_F` 

RP_1990_age_sexe$sum_70_74_h_f<-RP_1990_age_sexe$`70_74_H` + RP_1990_age_sexe$`70_74_F` 

RP_1990_age_sexe$sum_75_79_h_f<-RP_1990_age_sexe$`75_79_H` + RP_1990_age_sexe$`75_79_F` 

RP_1990_age_sexe$sum_80_plus_h_f<-RP_1990_age_sexe$`80_84_H` + RP_1990_age_sexe$`80_84_F` + RP_1990_age_sexe$`85_89_H` + RP_1990_age_sexe$`85_89_F` + RP_1990_age_sexe$`90_94_H` + RP_1990_age_sexe$`90_94_F` + RP_1990_age_sexe$`90_plus_H` + RP_1990_age_sexe$`90_plus_F`


RP_1990_age_sexe$population<- rowSums(RP_1990_age_sexe[,7:46])



RP_1990_age_sexe$sum_homme<-RP_1990_age_sexe$`0_4_H` + RP_1990_age_sexe$`5_9_H` + RP_1990_age_sexe$`10_14_H` + RP_1990_age_sexe$`15_19_H` + RP_1990_age_sexe$`20_24_H` + RP_1990_age_sexe$`25_29_H` + RP_1990_age_sexe$`30_34_H` + RP_1990_age_sexe$`35_39_H`+ RP_1990_age_sexe$`40_44_H`+ RP_1990_age_sexe$`45_49_H`+ RP_1990_age_sexe$`50_54_H`+ RP_1990_age_sexe$`55_59_H`+ RP_1990_age_sexe$`60_64_H`+ RP_1990_age_sexe$`65_69_H`+ RP_1990_age_sexe$`70_74_H`+ RP_1990_age_sexe$`75_79_H`+ RP_1990_age_sexe$`80_84_H`+ RP_1990_age_sexe$`85_89_H`+ RP_1990_age_sexe$`90_94_H`+ RP_1990_age_sexe$`90_plus_H`

RP_1990_age_sexe$sum_femme<-RP_1990_age_sexe$`0_4_F` + RP_1990_age_sexe$`5_9_F` + RP_1990_age_sexe$`10_14_F` + RP_1990_age_sexe$`15_19_F` + RP_1990_age_sexe$`20_24_F` + RP_1990_age_sexe$`25_29_F` + RP_1990_age_sexe$`30_34_F` + RP_1990_age_sexe$`35_39_F`+ RP_1990_age_sexe$`40_44_F`+ RP_1990_age_sexe$`45_49_F`+ RP_1990_age_sexe$`50_54_F`+ RP_1990_age_sexe$`55_59_F`+ RP_1990_age_sexe$`60_64_F`+ RP_1990_age_sexe$`65_69_F`+ RP_1990_age_sexe$`70_74_F`+ RP_1990_age_sexe$`75_79_F`+ RP_1990_age_sexe$`80_84_F`+ RP_1990_age_sexe$`85_89_F`+ RP_1990_age_sexe$`90_94_F`+ RP_1990_age_sexe$`90_plus_F`



RP_1990_age_sexe<-RP_1990_age_sexe[,47:59]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1990_age_sexe<-left_join(RP_1990_age_sexe,table_passage_bis)

RP_1990_age_sexe$COM_AP<-ifelse(!is.na(RP_1990_age_sexe$COM_AP),RP_1990_age_sexe$COM_AP,RP_1990_age_sexe$COM)


ag_1<-aggregate(sum_homme~COM_AP,RP_1990_age_sexe,sum)
ag_2<-aggregate(sum_femme~COM_AP,RP_1990_age_sexe,sum)
ag_3<-aggregate(sum_0_9_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_4<-aggregate(sum_10_19_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_5<-aggregate(sum_20_39_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_6<-aggregate(sum_40_59_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_7<-aggregate(sum_60_64_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_8<-aggregate(sum_65_69_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_9<-aggregate(sum_70_74_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_10<-aggregate(sum_75_79_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_11<-aggregate(sum_80_plus_h_f~COM_AP,RP_1990_age_sexe,sum)
ag_12<-aggregate(population~COM_AP,RP_1990_age_sexe,sum)

RP_1990_age_sexe_final<-left_join(ag_1,ag_2)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_3)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_4)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_5)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_6)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_7)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_8)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_9)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_10)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_11)
RP_1990_age_sexe_final<-left_join(RP_1990_age_sexe_final,ag_12)



write.csv2(RP_1990_age_sexe_final, "recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final")

#library(readr)
#RP_1990_age_sexe_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




##############


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
library(readxl)



RP_1999_age_sexe <- read_excel("recensement 1980-2022/RP_1999_age_sexe.xlsx")

names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1999_age_sexe$COM <- paste(RP_1999_age_sexe$DEP_geo_courante, RP_1999_age_sexe$COM_geo_courante, sep = "")


names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 0 à 4 ans\r\nHommes\r\nRP1999"]<-"0_4_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 5 à 9 ans\r\nHommes\r\nRP1999"]<-"5_9_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 10 à 14 ans\r\nHommes\r\nRP1999"]<-"10_14_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 15 à 19 ans\r\nHommes\r\nRP1999"]<-"15_19_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 20 à 24 ans\r\nHommes\r\nRP1999"]<-"20_24_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 25 à 29 ans\r\nHommes\r\nRP1999"]<-"25_29_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 30 à 34 ans\r\nHommes\r\nRP1999"]<-"30_34_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 35 à 39 ans\r\nHommes\r\nRP1999"]<-"35_39_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 40 à 44 ans\r\nHommes\r\nRP1999"]<-"40_44_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 45 à 49 ans\r\nHommes\r\nRP1999"]<-"45_49_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 50 à 54 ans\r\nHommes\r\nRP1999"]<-"50_54_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 55 à 59 ans\r\nHommes\r\nRP1999"]<-"55_59_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 60 à 64 ans\r\nHommes\r\nRP1999"]<-"60_64_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 65 à 69 ans\r\nHommes\r\nRP1999"]<-"65_69_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 70 à 74 ans\r\nHommes\r\nRP1999"]<-"70_74_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 75 à 79 ans\r\nHommes\r\nRP1999"]<-"75_79_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 80 à 84 ans\r\nHommes\r\nRP1999"]<-"80_84_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 85 à 89 ans\r\nHommes\r\nRP1999"]<-"85_89_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 90 à 94 ans\r\nHommes\r\nRP1999"]<-"90_94_H"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="95 ans et plus\r\nHommes\r\nRP1999"]<-"90_plus_H"





names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 0 à 4 ans\r\nFemmes\r\nRP1999"]<-"0_4_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 5 à 9 ans\r\nFemmes\r\nRP1999"]<-"5_9_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 10 à 14 ans\r\nFemmes\r\nRP1999"]<-"10_14_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 15 à 19 ans\r\nFemmes\r\nRP1999"]<-"15_19_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 20 à 24 ans\r\nFemmes\r\nRP1999"]<-"20_24_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 25 à 29 ans\r\nFemmes\r\nRP1999"]<-"25_29_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 30 à 34 ans\r\nFemmes\r\nRP1999"]<-"30_34_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 35 à 39 ans\r\nFemmes\r\nRP1999"]<-"35_39_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 40 à 44 ans\r\nFemmes\r\nRP1999"]<-"40_44_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 45 à 49 ans\r\nFemmes\r\nRP1999"]<-"45_49_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 50 à 54 ans\r\nFemmes\r\nRP1999"]<-"50_54_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 55 à 59 ans\r\nFemmes\r\nRP1999"]<-"55_59_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 60 à 64 ans\r\nFemmes\r\nRP1999"]<-"60_64_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 65 à 69 ans\r\nFemmes\r\nRP1999"]<-"65_69_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 70 à 74 ans\r\nFemmes\r\nRP1999"]<-"70_74_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 75 à 79 ans\r\nFemmes\r\nRP1999"]<-"75_79_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 80 à 84 ans\r\nFemmes\r\nRP1999"]<-"80_84_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 85 à 89 ans\r\nFemmes\r\nRP1999"]<-"85_89_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="De 90 à 94 ans\r\nFemmes\r\nRP1999"]<-"90_94_F"
names(RP_1999_age_sexe)[names(RP_1999_age_sexe)=="95 ans et plus\r\nFemmes\r\nRP1999"]<-"90_plus_F"




RP_1999_age_sexe$sum_0_9_h_f<-RP_1999_age_sexe$`0_4_H` + RP_1999_age_sexe$`0_4_F` + RP_1999_age_sexe$`5_9_H` + RP_1999_age_sexe$`5_9_F`


RP_1999_age_sexe$sum_10_19_h_f<-RP_1999_age_sexe$`10_14_H` + RP_1999_age_sexe$`10_14_F` + RP_1999_age_sexe$`15_19_H` + RP_1999_age_sexe$`15_19_F`


RP_1999_age_sexe$sum_20_39_h_f<-RP_1999_age_sexe$`20_24_H` + RP_1999_age_sexe$`20_24_F` + RP_1999_age_sexe$`25_29_H` + RP_1999_age_sexe$`25_29_F` + RP_1999_age_sexe$`30_34_H` + RP_1999_age_sexe$`30_34_F` + RP_1999_age_sexe$`35_39_H` + RP_1999_age_sexe$`35_39_F`


RP_1999_age_sexe$sum_40_59_h_f<-RP_1999_age_sexe$`40_44_H` + RP_1999_age_sexe$`40_44_F` + RP_1999_age_sexe$`45_49_H` + RP_1999_age_sexe$`45_49_F` + RP_1999_age_sexe$`50_54_H` + RP_1999_age_sexe$`50_54_F` + RP_1999_age_sexe$`55_59_H` + RP_1999_age_sexe$`55_59_F`


RP_1999_age_sexe$sum_60_64_h_f<-RP_1999_age_sexe$`60_64_H` + RP_1999_age_sexe$`60_64_F` 


RP_1999_age_sexe$sum_65_69_h_f<-RP_1999_age_sexe$`65_69_H` + RP_1999_age_sexe$`65_69_F` 

RP_1999_age_sexe$sum_70_74_h_f<-RP_1999_age_sexe$`70_74_H` + RP_1999_age_sexe$`70_74_F` 

RP_1999_age_sexe$sum_75_79_h_f<-RP_1999_age_sexe$`75_79_H` + RP_1999_age_sexe$`75_79_F` 

RP_1999_age_sexe$sum_80_plus_h_f<-RP_1999_age_sexe$`80_84_H` + RP_1999_age_sexe$`80_84_F` + RP_1999_age_sexe$`85_89_H` + RP_1999_age_sexe$`85_89_F` + RP_1999_age_sexe$`90_94_H` + RP_1999_age_sexe$`90_94_F` + RP_1999_age_sexe$`90_plus_H` + RP_1999_age_sexe$`90_plus_F`


RP_1999_age_sexe$population<- rowSums(RP_1999_age_sexe[,7:46])



RP_1999_age_sexe$sum_homme<-RP_1999_age_sexe$`0_4_H` + RP_1999_age_sexe$`5_9_H` + RP_1999_age_sexe$`10_14_H` + RP_1999_age_sexe$`15_19_H` + RP_1999_age_sexe$`20_24_H` + RP_1999_age_sexe$`25_29_H` + RP_1999_age_sexe$`30_34_H` + RP_1999_age_sexe$`35_39_H`+ RP_1999_age_sexe$`40_44_H`+ RP_1999_age_sexe$`45_49_H`+ RP_1999_age_sexe$`50_54_H`+ RP_1999_age_sexe$`55_59_H`+ RP_1999_age_sexe$`60_64_H`+ RP_1999_age_sexe$`65_69_H`+ RP_1999_age_sexe$`70_74_H`+ RP_1999_age_sexe$`75_79_H`+ RP_1999_age_sexe$`80_84_H`+ RP_1999_age_sexe$`85_89_H`+ RP_1999_age_sexe$`90_94_H`+ RP_1999_age_sexe$`90_plus_H`

RP_1999_age_sexe$sum_femme<-RP_1999_age_sexe$`0_4_F` + RP_1999_age_sexe$`5_9_F` + RP_1999_age_sexe$`10_14_F` + RP_1999_age_sexe$`15_19_F` + RP_1999_age_sexe$`20_24_F` + RP_1999_age_sexe$`25_29_F` + RP_1999_age_sexe$`30_34_F` + RP_1999_age_sexe$`35_39_F`+ RP_1999_age_sexe$`40_44_F`+ RP_1999_age_sexe$`45_49_F`+ RP_1999_age_sexe$`50_54_F`+ RP_1999_age_sexe$`55_59_F`+ RP_1999_age_sexe$`60_64_F`+ RP_1999_age_sexe$`65_69_F`+ RP_1999_age_sexe$`70_74_F`+ RP_1999_age_sexe$`75_79_F`+ RP_1999_age_sexe$`80_84_F`+ RP_1999_age_sexe$`85_89_F`+ RP_1999_age_sexe$`90_94_F`+ RP_1999_age_sexe$`90_plus_F`



RP_1999_age_sexe<-RP_1999_age_sexe[,47:59]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1999_age_sexe<-left_join(RP_1999_age_sexe,table_passage_bis)

RP_1999_age_sexe$COM_AP<-ifelse(!is.na(RP_1999_age_sexe$COM_AP),RP_1999_age_sexe$COM_AP,RP_1999_age_sexe$COM)


ag_1<-aggregate(sum_homme~COM_AP,RP_1999_age_sexe,sum)
ag_2<-aggregate(sum_femme~COM_AP,RP_1999_age_sexe,sum)
ag_3<-aggregate(sum_0_9_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_4<-aggregate(sum_10_19_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_5<-aggregate(sum_20_39_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_6<-aggregate(sum_40_59_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_7<-aggregate(sum_60_64_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_8<-aggregate(sum_65_69_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_9<-aggregate(sum_70_74_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_10<-aggregate(sum_75_79_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_11<-aggregate(sum_80_plus_h_f~COM_AP,RP_1999_age_sexe,sum)
ag_12<-aggregate(population~COM_AP,RP_1999_age_sexe,sum)

RP_1999_age_sexe_final<-left_join(ag_1,ag_2)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_3)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_4)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_5)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_6)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_7)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_8)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_9)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_10)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_11)
RP_1999_age_sexe_final<-left_join(RP_1999_age_sexe_final,ag_12)



write.csv2(RP_1999_age_sexe_final, "recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final")

#library(readr)
#RP_1999_age_sexe_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)





##############


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
library(readxl)



RP_2008_age_sexe <- read_excel("recensement 1980-2022/RP_2008_age_sexe.xlsx")

names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_2008_age_sexe$COM <- paste(RP_2008_age_sexe$DEP_geo_courante, RP_2008_age_sexe$COM_geo_courante, sep = "")


names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 0 à 4 ans\r\nHommes\r\nRP2008"]<-"0_4_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 5 à 9 ans\r\nHommes\r\nRP2008"]<-"5_9_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 10 à 14 ans\r\nHommes\r\nRP2008"]<-"10_14_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 15 à 19 ans\r\nHommes\r\nRP2008"]<-"15_19_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 20 à 24 ans\r\nHommes\r\nRP2008"]<-"20_24_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 25 à 29 ans\r\nHommes\r\nRP2008"]<-"25_29_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 30 à 34 ans\r\nHommes\r\nRP2008"]<-"30_34_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 35 à 39 ans\r\nHommes\r\nRP2008"]<-"35_39_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 40 à 44 ans\r\nHommes\r\nRP2008"]<-"40_44_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 45 à 49 ans\r\nHommes\r\nRP2008"]<-"45_49_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 50 à 54 ans\r\nHommes\r\nRP2008"]<-"50_54_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 55 à 59 ans\r\nHommes\r\nRP2008"]<-"55_59_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 60 à 64 ans\r\nHommes\r\nRP2008"]<-"60_64_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 65 à 69 ans\r\nHommes\r\nRP2008"]<-"65_69_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 70 à 74 ans\r\nHommes\r\nRP2008"]<-"70_74_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 75 à 79 ans\r\nHommes\r\nRP2008"]<-"75_79_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 80 à 84 ans\r\nHommes\r\nRP2008"]<-"80_84_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 85 à 89 ans\r\nHommes\r\nRP2008"]<-"85_89_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 90 à 94 ans\r\nHommes\r\nRP2008"]<-"90_94_H"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="95 ans et plus\r\nHommes\r\nRP2008"]<-"90_plus_H"





names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 0 à 4 ans\r\nFemmes\r\nRP2008"]<-"0_4_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 5 à 9 ans\r\nFemmes\r\nRP2008"]<-"5_9_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 10 à 14 ans\r\nFemmes\r\nRP2008"]<-"10_14_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 15 à 19 ans\r\nFemmes\r\nRP2008"]<-"15_19_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 20 à 24 ans\r\nFemmes\r\nRP2008"]<-"20_24_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 25 à 29 ans\r\nFemmes\r\nRP2008"]<-"25_29_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 30 à 34 ans\r\nFemmes\r\nRP2008"]<-"30_34_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 35 à 39 ans\r\nFemmes\r\nRP2008"]<-"35_39_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 40 à 44 ans\r\nFemmes\r\nRP2008"]<-"40_44_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 45 à 49 ans\r\nFemmes\r\nRP2008"]<-"45_49_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 50 à 54 ans\r\nFemmes\r\nRP2008"]<-"50_54_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 55 à 59 ans\r\nFemmes\r\nRP2008"]<-"55_59_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 60 à 64 ans\r\nFemmes\r\nRP2008"]<-"60_64_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 65 à 69 ans\r\nFemmes\r\nRP2008"]<-"65_69_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 70 à 74 ans\r\nFemmes\r\nRP2008"]<-"70_74_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 75 à 79 ans\r\nFemmes\r\nRP2008"]<-"75_79_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 80 à 84 ans\r\nFemmes\r\nRP2008"]<-"80_84_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 85 à 89 ans\r\nFemmes\r\nRP2008"]<-"85_89_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="De 90 à 94 ans\r\nFemmes\r\nRP2008"]<-"90_94_F"
names(RP_2008_age_sexe)[names(RP_2008_age_sexe)=="95 ans et plus\r\nFemmes\r\nRP2008"]<-"90_plus_F"




RP_2008_age_sexe$sum_0_9_h_f<-RP_2008_age_sexe$`0_4_H` + RP_2008_age_sexe$`0_4_F` + RP_2008_age_sexe$`5_9_H` + RP_2008_age_sexe$`5_9_F`


RP_2008_age_sexe$sum_10_19_h_f<-RP_2008_age_sexe$`10_14_H` + RP_2008_age_sexe$`10_14_F` + RP_2008_age_sexe$`15_19_H` + RP_2008_age_sexe$`15_19_F`


RP_2008_age_sexe$sum_20_39_h_f<-RP_2008_age_sexe$`20_24_H` + RP_2008_age_sexe$`20_24_F` + RP_2008_age_sexe$`25_29_H` + RP_2008_age_sexe$`25_29_F` + RP_2008_age_sexe$`30_34_H` + RP_2008_age_sexe$`30_34_F` + RP_2008_age_sexe$`35_39_H` + RP_2008_age_sexe$`35_39_F`


RP_2008_age_sexe$sum_40_59_h_f<-RP_2008_age_sexe$`40_44_H` + RP_2008_age_sexe$`40_44_F` + RP_2008_age_sexe$`45_49_H` + RP_2008_age_sexe$`45_49_F` + RP_2008_age_sexe$`50_54_H` + RP_2008_age_sexe$`50_54_F` + RP_2008_age_sexe$`55_59_H` + RP_2008_age_sexe$`55_59_F`


RP_2008_age_sexe$sum_60_64_h_f<-RP_2008_age_sexe$`60_64_H` + RP_2008_age_sexe$`60_64_F` 


RP_2008_age_sexe$sum_65_69_h_f<-RP_2008_age_sexe$`65_69_H` + RP_2008_age_sexe$`65_69_F` 

RP_2008_age_sexe$sum_70_74_h_f<-RP_2008_age_sexe$`70_74_H` + RP_2008_age_sexe$`70_74_F` 

RP_2008_age_sexe$sum_75_79_h_f<-RP_2008_age_sexe$`75_79_H` + RP_2008_age_sexe$`75_79_F` 

RP_2008_age_sexe$sum_80_plus_h_f<-RP_2008_age_sexe$`80_84_H` + RP_2008_age_sexe$`80_84_F` + RP_2008_age_sexe$`85_89_H` + RP_2008_age_sexe$`85_89_F` + RP_2008_age_sexe$`90_94_H` + RP_2008_age_sexe$`90_94_F` + RP_2008_age_sexe$`90_plus_H` + RP_2008_age_sexe$`90_plus_F`


RP_2008_age_sexe$population<- rowSums(RP_2008_age_sexe[,7:46])



RP_2008_age_sexe$sum_homme<-RP_2008_age_sexe$`0_4_H` + RP_2008_age_sexe$`5_9_H` + RP_2008_age_sexe$`10_14_H` + RP_2008_age_sexe$`15_19_H` + RP_2008_age_sexe$`20_24_H` + RP_2008_age_sexe$`25_29_H` + RP_2008_age_sexe$`30_34_H` + RP_2008_age_sexe$`35_39_H`+ RP_2008_age_sexe$`40_44_H`+ RP_2008_age_sexe$`45_49_H`+ RP_2008_age_sexe$`50_54_H`+ RP_2008_age_sexe$`55_59_H`+ RP_2008_age_sexe$`60_64_H`+ RP_2008_age_sexe$`65_69_H`+ RP_2008_age_sexe$`70_74_H`+ RP_2008_age_sexe$`75_79_H`+ RP_2008_age_sexe$`80_84_H`+ RP_2008_age_sexe$`85_89_H`+ RP_2008_age_sexe$`90_94_H`+ RP_2008_age_sexe$`90_plus_H`

RP_2008_age_sexe$sum_femme<-RP_2008_age_sexe$`0_4_F` + RP_2008_age_sexe$`5_9_F` + RP_2008_age_sexe$`10_14_F` + RP_2008_age_sexe$`15_19_F` + RP_2008_age_sexe$`20_24_F` + RP_2008_age_sexe$`25_29_F` + RP_2008_age_sexe$`30_34_F` + RP_2008_age_sexe$`35_39_F`+ RP_2008_age_sexe$`40_44_F`+ RP_2008_age_sexe$`45_49_F`+ RP_2008_age_sexe$`50_54_F`+ RP_2008_age_sexe$`55_59_F`+ RP_2008_age_sexe$`60_64_F`+ RP_2008_age_sexe$`65_69_F`+ RP_2008_age_sexe$`70_74_F`+ RP_2008_age_sexe$`75_79_F`+ RP_2008_age_sexe$`80_84_F`+ RP_2008_age_sexe$`85_89_F`+ RP_2008_age_sexe$`90_94_F`+ RP_2008_age_sexe$`90_plus_F`



RP_2008_age_sexe<-RP_2008_age_sexe[,47:59]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_2008_age_sexe<-left_join(RP_2008_age_sexe,table_passage_bis)

RP_2008_age_sexe$COM_AP<-ifelse(!is.na(RP_2008_age_sexe$COM_AP),RP_2008_age_sexe$COM_AP,RP_2008_age_sexe$COM)


ag_1<-aggregate(sum_homme~COM_AP,RP_2008_age_sexe,sum)
ag_2<-aggregate(sum_femme~COM_AP,RP_2008_age_sexe,sum)
ag_3<-aggregate(sum_0_9_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_4<-aggregate(sum_10_19_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_5<-aggregate(sum_20_39_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_6<-aggregate(sum_40_59_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_7<-aggregate(sum_60_64_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_8<-aggregate(sum_65_69_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_9<-aggregate(sum_70_74_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_10<-aggregate(sum_75_79_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_11<-aggregate(sum_80_plus_h_f~COM_AP,RP_2008_age_sexe,sum)
ag_12<-aggregate(population~COM_AP,RP_2008_age_sexe,sum)

RP_2008_age_sexe_final<-left_join(ag_1,ag_2)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_3)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_4)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_5)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_6)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_7)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_8)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_9)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_10)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_11)
RP_2008_age_sexe_final<-left_join(RP_2008_age_sexe_final,ag_12)

RP_2008_age_sexe_final$sum_10_19_h_f <- round(RP_2008_age_sexe_final$sum_10_19_h_f, 3)


write.csv(RP_2008_age_sexe_final, "recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final")

#library(readr)
#RP_2008_age_sexe_final <- read_delim("recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




##############


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
library(readxl)



RP_2013_age_sexe <- read_excel("recensement 1980-2022/RP_2013_age_sexe.xlsx")

names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_2013_age_sexe$COM <- paste(RP_2013_age_sexe$DEP_geo_courante, RP_2013_age_sexe$COM_geo_courante, sep = "")


names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 0 à 4 ans\r\nHommes\r\nRP2013"]<-"0_4_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 5 à 9 ans\r\nHommes\r\nRP2013"]<-"5_9_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 10 à 14 ans\r\nHommes\r\nRP2013"]<-"10_14_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 15 à 19 ans\r\nHommes\r\nRP2013"]<-"15_19_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 20 à 24 ans\r\nHommes\r\nRP2013"]<-"20_24_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 25 à 29 ans\r\nHommes\r\nRP2013"]<-"25_29_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 30 à 34 ans\r\nHommes\r\nRP2013"]<-"30_34_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 35 à 39 ans\r\nHommes\r\nRP2013"]<-"35_39_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 40 à 44 ans\r\nHommes\r\nRP2013"]<-"40_44_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 45 à 49 ans\r\nHommes\r\nRP2013"]<-"45_49_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 50 à 54 ans\r\nHommes\r\nRP2013"]<-"50_54_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 55 à 59 ans\r\nHommes\r\nRP2013"]<-"55_59_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 60 à 64 ans\r\nHommes\r\nRP2013"]<-"60_64_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 65 à 69 ans\r\nHommes\r\nRP2013"]<-"65_69_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 70 à 74 ans\r\nHommes\r\nRP2013"]<-"70_74_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 75 à 79 ans\r\nHommes\r\nRP2013"]<-"75_79_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 80 à 84 ans\r\nHommes\r\nRP2013"]<-"80_84_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 85 à 89 ans\r\nHommes\r\nRP2013"]<-"85_89_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 90 à 94 ans\r\nHommes\r\nRP2013"]<-"90_94_H"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="95 ans et plus\r\nHommes\r\nRP2013"]<-"90_plus_H"





names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 0 à 4 ans\r\nFemmes\r\nRP2013"]<-"0_4_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 5 à 9 ans\r\nFemmes\r\nRP2013"]<-"5_9_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 10 à 14 ans\r\nFemmes\r\nRP2013"]<-"10_14_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 15 à 19 ans\r\nFemmes\r\nRP2013"]<-"15_19_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 20 à 24 ans\r\nFemmes\r\nRP2013"]<-"20_24_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 25 à 29 ans\r\nFemmes\r\nRP2013"]<-"25_29_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 30 à 34 ans\r\nFemmes\r\nRP2013"]<-"30_34_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 35 à 39 ans\r\nFemmes\r\nRP2013"]<-"35_39_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 40 à 44 ans\r\nFemmes\r\nRP2013"]<-"40_44_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 45 à 49 ans\r\nFemmes\r\nRP2013"]<-"45_49_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 50 à 54 ans\r\nFemmes\r\nRP2013"]<-"50_54_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 55 à 59 ans\r\nFemmes\r\nRP2013"]<-"55_59_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 60 à 64 ans\r\nFemmes\r\nRP2013"]<-"60_64_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 65 à 69 ans\r\nFemmes\r\nRP2013"]<-"65_69_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 70 à 74 ans\r\nFemmes\r\nRP2013"]<-"70_74_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 75 à 79 ans\r\nFemmes\r\nRP2013"]<-"75_79_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 80 à 84 ans\r\nFemmes\r\nRP2013"]<-"80_84_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 85 à 89 ans\r\nFemmes\r\nRP2013"]<-"85_89_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="De 90 à 94 ans\r\nFemmes\r\nRP2013"]<-"90_94_F"
names(RP_2013_age_sexe)[names(RP_2013_age_sexe)=="95 ans et plus\r\nFemmes\r\nRP2013"]<-"90_plus_F"




RP_2013_age_sexe$sum_0_9_h_f<-RP_2013_age_sexe$`0_4_H` + RP_2013_age_sexe$`0_4_F` + RP_2013_age_sexe$`5_9_H` + RP_2013_age_sexe$`5_9_F`


RP_2013_age_sexe$sum_10_19_h_f<-RP_2013_age_sexe$`10_14_H` + RP_2013_age_sexe$`10_14_F` + RP_2013_age_sexe$`15_19_H` + RP_2013_age_sexe$`15_19_F`


RP_2013_age_sexe$sum_20_39_h_f<-RP_2013_age_sexe$`20_24_H` + RP_2013_age_sexe$`20_24_F` + RP_2013_age_sexe$`25_29_H` + RP_2013_age_sexe$`25_29_F` + RP_2013_age_sexe$`30_34_H` + RP_2013_age_sexe$`30_34_F` + RP_2013_age_sexe$`35_39_H` + RP_2013_age_sexe$`35_39_F`


RP_2013_age_sexe$sum_40_59_h_f<-RP_2013_age_sexe$`40_44_H` + RP_2013_age_sexe$`40_44_F` + RP_2013_age_sexe$`45_49_H` + RP_2013_age_sexe$`45_49_F` + RP_2013_age_sexe$`50_54_H` + RP_2013_age_sexe$`50_54_F` + RP_2013_age_sexe$`55_59_H` + RP_2013_age_sexe$`55_59_F`


RP_2013_age_sexe$sum_60_64_h_f<-RP_2013_age_sexe$`60_64_H` + RP_2013_age_sexe$`60_64_F` 


RP_2013_age_sexe$sum_65_69_h_f<-RP_2013_age_sexe$`65_69_H` + RP_2013_age_sexe$`65_69_F` 

RP_2013_age_sexe$sum_70_74_h_f<-RP_2013_age_sexe$`70_74_H` + RP_2013_age_sexe$`70_74_F` 

RP_2013_age_sexe$sum_75_79_h_f<-RP_2013_age_sexe$`75_79_H` + RP_2013_age_sexe$`75_79_F` 

RP_2013_age_sexe$sum_80_plus_h_f<-RP_2013_age_sexe$`80_84_H` + RP_2013_age_sexe$`80_84_F` + RP_2013_age_sexe$`85_89_H` + RP_2013_age_sexe$`85_89_F` + RP_2013_age_sexe$`90_94_H` + RP_2013_age_sexe$`90_94_F` + RP_2013_age_sexe$`90_plus_H` + RP_2013_age_sexe$`90_plus_F`


RP_2013_age_sexe$population<- rowSums(RP_2013_age_sexe[,7:46])



RP_2013_age_sexe$sum_homme<-RP_2013_age_sexe$`0_4_H` + RP_2013_age_sexe$`5_9_H` + RP_2013_age_sexe$`10_14_H` + RP_2013_age_sexe$`15_19_H` + RP_2013_age_sexe$`20_24_H` + RP_2013_age_sexe$`25_29_H` + RP_2013_age_sexe$`30_34_H` + RP_2013_age_sexe$`35_39_H`+ RP_2013_age_sexe$`40_44_H`+ RP_2013_age_sexe$`45_49_H`+ RP_2013_age_sexe$`50_54_H`+ RP_2013_age_sexe$`55_59_H`+ RP_2013_age_sexe$`60_64_H`+ RP_2013_age_sexe$`65_69_H`+ RP_2013_age_sexe$`70_74_H`+ RP_2013_age_sexe$`75_79_H`+ RP_2013_age_sexe$`80_84_H`+ RP_2013_age_sexe$`85_89_H`+ RP_2013_age_sexe$`90_94_H`+ RP_2013_age_sexe$`90_plus_H`

RP_2013_age_sexe$sum_femme<-RP_2013_age_sexe$`0_4_F` + RP_2013_age_sexe$`5_9_F` + RP_2013_age_sexe$`10_14_F` + RP_2013_age_sexe$`15_19_F` + RP_2013_age_sexe$`20_24_F` + RP_2013_age_sexe$`25_29_F` + RP_2013_age_sexe$`30_34_F` + RP_2013_age_sexe$`35_39_F`+ RP_2013_age_sexe$`40_44_F`+ RP_2013_age_sexe$`45_49_F`+ RP_2013_age_sexe$`50_54_F`+ RP_2013_age_sexe$`55_59_F`+ RP_2013_age_sexe$`60_64_F`+ RP_2013_age_sexe$`65_69_F`+ RP_2013_age_sexe$`70_74_F`+ RP_2013_age_sexe$`75_79_F`+ RP_2013_age_sexe$`80_84_F`+ RP_2013_age_sexe$`85_89_F`+ RP_2013_age_sexe$`90_94_F`+ RP_2013_age_sexe$`90_plus_F`



RP_2013_age_sexe<-RP_2013_age_sexe[,47:59]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_2013_age_sexe<-left_join(RP_2013_age_sexe,table_passage_bis)

RP_2013_age_sexe$COM_AP<-ifelse(!is.na(RP_2013_age_sexe$COM_AP),RP_2013_age_sexe$COM_AP,RP_2013_age_sexe$COM)


ag_1<-aggregate(sum_homme~COM_AP,RP_2013_age_sexe,sum)
ag_2<-aggregate(sum_femme~COM_AP,RP_2013_age_sexe,sum)
ag_3<-aggregate(sum_0_9_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_4<-aggregate(sum_10_19_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_5<-aggregate(sum_20_39_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_6<-aggregate(sum_40_59_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_7<-aggregate(sum_60_64_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_8<-aggregate(sum_65_69_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_9<-aggregate(sum_70_74_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_10<-aggregate(sum_75_79_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_11<-aggregate(sum_80_plus_h_f~COM_AP,RP_2013_age_sexe,sum)
ag_12<-aggregate(population~COM_AP,RP_2013_age_sexe,sum)

RP_2013_age_sexe_final<-left_join(ag_1,ag_2)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_3)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_4)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_5)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_6)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_7)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_8)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_9)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_10)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_11)
RP_2013_age_sexe_final<-left_join(RP_2013_age_sexe_final,ag_12)



write.csv(RP_2013_age_sexe_final, "recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final")

#library(readr)
#RP_2013_age_sexe_final <- read_delim("recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




##############


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
library(readxl)



RP_2019_age_sexe <- read_excel("recensement 1980-2022/RP_2019_age_sexe.xlsx")

names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_2019_age_sexe$COM <- paste(RP_2019_age_sexe$DEP_geo_courante, RP_2019_age_sexe$COM_geo_courante, sep = "")


names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 0 à 4 ans\r\nHommes\r\nRP2019"]<-"0_4_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 5 à 9 ans\r\nHommes\r\nRP2019"]<-"5_9_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 10 à 14 ans\r\nHommes\r\nRP2019"]<-"10_14_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 15 à 19 ans\r\nHommes\r\nRP2019"]<-"15_19_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 20 à 24 ans\r\nHommes\r\nRP2019"]<-"20_24_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 25 à 29 ans\r\nHommes\r\nRP2019"]<-"25_29_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 30 à 34 ans\r\nHommes\r\nRP2019"]<-"30_34_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 35 à 39 ans\r\nHommes\r\nRP2019"]<-"35_39_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 40 à 44 ans\r\nHommes\r\nRP2019"]<-"40_44_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 45 à 49 ans\r\nHommes\r\nRP2019"]<-"45_49_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 50 à 54 ans\r\nHommes\r\nRP2019"]<-"50_54_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 55 à 59 ans\r\nHommes\r\nRP2019"]<-"55_59_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 60 à 64 ans\r\nHommes\r\nRP2019"]<-"60_64_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 65 à 69 ans\r\nHommes\r\nRP2019"]<-"65_69_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 70 à 74 ans\r\nHommes\r\nRP2019"]<-"70_74_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 75 à 79 ans\r\nHommes\r\nRP2019"]<-"75_79_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 80 à 84 ans\r\nHommes\r\nRP2019"]<-"80_84_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 85 à 89 ans\r\nHommes\r\nRP2019"]<-"85_89_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 90 à 94 ans\r\nHommes\r\nRP2019"]<-"90_94_H"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="95 ans et plus\r\nHommes\r\nRP2019"]<-"90_plus_H"





names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 0 à 4 ans\r\nFemmes\r\nRP2019"]<-"0_4_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 5 à 9 ans\r\nFemmes\r\nRP2019"]<-"5_9_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 10 à 14 ans\r\nFemmes\r\nRP2019"]<-"10_14_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 15 à 19 ans\r\nFemmes\r\nRP2019"]<-"15_19_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 20 à 24 ans\r\nFemmes\r\nRP2019"]<-"20_24_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 25 à 29 ans\r\nFemmes\r\nRP2019"]<-"25_29_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 30 à 34 ans\r\nFemmes\r\nRP2019"]<-"30_34_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 35 à 39 ans\r\nFemmes\r\nRP2019"]<-"35_39_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 40 à 44 ans\r\nFemmes\r\nRP2019"]<-"40_44_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 45 à 49 ans\r\nFemmes\r\nRP2019"]<-"45_49_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 50 à 54 ans\r\nFemmes\r\nRP2019"]<-"50_54_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 55 à 59 ans\r\nFemmes\r\nRP2019"]<-"55_59_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 60 à 64 ans\r\nFemmes\r\nRP2019"]<-"60_64_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 65 à 69 ans\r\nFemmes\r\nRP2019"]<-"65_69_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 70 à 74 ans\r\nFemmes\r\nRP2019"]<-"70_74_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 75 à 79 ans\r\nFemmes\r\nRP2019"]<-"75_79_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 80 à 84 ans\r\nFemmes\r\nRP2019"]<-"80_84_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 85 à 89 ans\r\nFemmes\r\nRP2019"]<-"85_89_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="De 90 à 94 ans\r\nFemmes\r\nRP2019"]<-"90_94_F"
names(RP_2019_age_sexe)[names(RP_2019_age_sexe)=="95 ans et plus\r\nFemmes\r\nRP2019"]<-"90_plus_F"




RP_2019_age_sexe$sum_0_9_h_f<-RP_2019_age_sexe$`0_4_H` + RP_2019_age_sexe$`0_4_F` + RP_2019_age_sexe$`5_9_H` + RP_2019_age_sexe$`5_9_F`


RP_2019_age_sexe$sum_10_19_h_f<-RP_2019_age_sexe$`10_14_H` + RP_2019_age_sexe$`10_14_F` + RP_2019_age_sexe$`15_19_H` + RP_2019_age_sexe$`15_19_F`


RP_2019_age_sexe$sum_20_39_h_f<-RP_2019_age_sexe$`20_24_H` + RP_2019_age_sexe$`20_24_F` + RP_2019_age_sexe$`25_29_H` + RP_2019_age_sexe$`25_29_F` + RP_2019_age_sexe$`30_34_H` + RP_2019_age_sexe$`30_34_F` + RP_2019_age_sexe$`35_39_H` + RP_2019_age_sexe$`35_39_F`


RP_2019_age_sexe$sum_40_59_h_f<-RP_2019_age_sexe$`40_44_H` + RP_2019_age_sexe$`40_44_F` + RP_2019_age_sexe$`45_49_H` + RP_2019_age_sexe$`45_49_F` + RP_2019_age_sexe$`50_54_H` + RP_2019_age_sexe$`50_54_F` + RP_2019_age_sexe$`55_59_H` + RP_2019_age_sexe$`55_59_F`


RP_2019_age_sexe$sum_60_64_h_f<-RP_2019_age_sexe$`60_64_H` + RP_2019_age_sexe$`60_64_F` 


RP_2019_age_sexe$sum_65_69_h_f<-RP_2019_age_sexe$`65_69_H` + RP_2019_age_sexe$`65_69_F` 

RP_2019_age_sexe$sum_70_74_h_f<-RP_2019_age_sexe$`70_74_H` + RP_2019_age_sexe$`70_74_F` 

RP_2019_age_sexe$sum_75_79_h_f<-RP_2019_age_sexe$`75_79_H` + RP_2019_age_sexe$`75_79_F` 

RP_2019_age_sexe$sum_80_plus_h_f<-RP_2019_age_sexe$`80_84_H` + RP_2019_age_sexe$`80_84_F` + RP_2019_age_sexe$`85_89_H` + RP_2019_age_sexe$`85_89_F` + RP_2019_age_sexe$`90_94_H` + RP_2019_age_sexe$`90_94_F` + RP_2019_age_sexe$`90_plus_H` + RP_2019_age_sexe$`90_plus_F`


RP_2019_age_sexe$population<- rowSums(RP_2019_age_sexe[,7:46])



RP_2019_age_sexe$sum_homme<-RP_2019_age_sexe$`0_4_H` + RP_2019_age_sexe$`5_9_H` + RP_2019_age_sexe$`10_14_H` + RP_2019_age_sexe$`15_19_H` + RP_2019_age_sexe$`20_24_H` + RP_2019_age_sexe$`25_29_H` + RP_2019_age_sexe$`30_34_H` + RP_2019_age_sexe$`35_39_H`+ RP_2019_age_sexe$`40_44_H`+ RP_2019_age_sexe$`45_49_H`+ RP_2019_age_sexe$`50_54_H`+ RP_2019_age_sexe$`55_59_H`+ RP_2019_age_sexe$`60_64_H`+ RP_2019_age_sexe$`65_69_H`+ RP_2019_age_sexe$`70_74_H`+ RP_2019_age_sexe$`75_79_H`+ RP_2019_age_sexe$`80_84_H`+ RP_2019_age_sexe$`85_89_H`+ RP_2019_age_sexe$`90_94_H`+ RP_2019_age_sexe$`90_plus_H`

RP_2019_age_sexe$sum_femme<-RP_2019_age_sexe$`0_4_F` + RP_2019_age_sexe$`5_9_F` + RP_2019_age_sexe$`10_14_F` + RP_2019_age_sexe$`15_19_F` + RP_2019_age_sexe$`20_24_F` + RP_2019_age_sexe$`25_29_F` + RP_2019_age_sexe$`30_34_F` + RP_2019_age_sexe$`35_39_F`+ RP_2019_age_sexe$`40_44_F`+ RP_2019_age_sexe$`45_49_F`+ RP_2019_age_sexe$`50_54_F`+ RP_2019_age_sexe$`55_59_F`+ RP_2019_age_sexe$`60_64_F`+ RP_2019_age_sexe$`65_69_F`+ RP_2019_age_sexe$`70_74_F`+ RP_2019_age_sexe$`75_79_F`+ RP_2019_age_sexe$`80_84_F`+ RP_2019_age_sexe$`85_89_F`+ RP_2019_age_sexe$`90_94_F`+ RP_2019_age_sexe$`90_plus_F`



RP_2019_age_sexe<-RP_2019_age_sexe[,47:59]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_2019_age_sexe<-left_join(RP_2019_age_sexe,table_passage_bis)

RP_2019_age_sexe$COM_AP<-ifelse(!is.na(RP_2019_age_sexe$COM_AP),RP_2019_age_sexe$COM_AP,RP_2019_age_sexe$COM)


ag_1<-aggregate(sum_homme~COM_AP,RP_2019_age_sexe,sum)
ag_2<-aggregate(sum_femme~COM_AP,RP_2019_age_sexe,sum)
ag_3<-aggregate(sum_0_9_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_4<-aggregate(sum_10_19_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_5<-aggregate(sum_20_39_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_6<-aggregate(sum_40_59_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_7<-aggregate(sum_60_64_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_8<-aggregate(sum_65_69_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_9<-aggregate(sum_70_74_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_10<-aggregate(sum_75_79_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_11<-aggregate(sum_80_plus_h_f~COM_AP,RP_2019_age_sexe,sum)
ag_12<-aggregate(population~COM_AP,RP_2019_age_sexe,sum)

RP_2019_age_sexe_final<-left_join(ag_1,ag_2)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_3)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_4)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_5)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_6)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_7)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_8)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_9)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_10)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_11)
RP_2019_age_sexe_final<-left_join(RP_2019_age_sexe_final,ag_12)



write.csv(RP_2019_age_sexe_final, "recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final")

#library(readr)
#RP_2019_age_sexe_final <- read_delim("recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)














#########################



#linear interpolation of data between years 







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
library(readxl)
library(readr)

RP_2019_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_1975_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1982_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1990_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1999_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_2008_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_2013_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




df_merged <- merge(RP_1975_age_sexe_final, RP_1982_age_sexe_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1975 divisée par le nombre d'années pour chaque variable
df_merged$difference_sum_homme <- (df_merged$sum_homme.y - df_merged$sum_homme.x) / 7
df_merged$difference_sum_femme <- (df_merged$sum_femme.y - df_merged$sum_femme.x) / 7
df_merged$difference_sum_0_9_h_f <- (df_merged$sum_0_9_h_f.y - df_merged$sum_0_9_h_f.x) / 7
df_merged$difference_sum_10_19_h_f <- (df_merged$sum_10_19_h_f.y - df_merged$sum_10_19_h_f.x) / 7
df_merged$difference_sum_20_39_h_f <- (df_merged$sum_20_39_h_f.y - df_merged$sum_20_39_h_f.x) / 7
df_merged$difference_sum_40_59_h_f <- (df_merged$sum_40_59_h_f.y - df_merged$sum_40_59_h_f.x) / 7
df_merged$difference_sum_60_64_h_f <- (df_merged$sum_60_64_h_f.y - df_merged$sum_60_64_h_f.x) / 7
df_merged$difference_sum_65_69_h_f <- (df_merged$sum_65_69_h_f.y - df_merged$sum_65_69_h_f.x) / 7
df_merged$difference_sum_70_74_h_f <- (df_merged$sum_70_74_h_f.y - df_merged$sum_70_74_h_f.x) / 7
df_merged$difference_sum_75_79_h_f <- (df_merged$sum_75_79_h_f.y - df_merged$sum_75_79_h_f.x) / 7
df_merged$difference_sum_80_plus_h_f <- (df_merged$sum_80_plus_h_f.y - df_merged$sum_80_plus_h_f.x) / 7
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 7

# Créer une liste avec les années entre 1975 et 1980
years <- 1975:1982

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1975
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_sum_homme<- df_temp$sum_homme.x + (x - 1975) * df_temp$difference_sum_homme
  df_temp$value_estimated_sum_femme <- df_temp$sum_femme.x + (x - 1975) * df_temp$difference_sum_femme
  df_temp$value_estimated_sum_0_9_h_f <- df_temp$sum_0_9_h_f.x + (x - 1975) * df_temp$difference_sum_0_9_h_f
  df_temp$value_estimated_sum_10_19_h_f <- df_temp$sum_10_19_h_f.x + (x - 1975) * df_temp$difference_sum_10_19_h_f
  df_temp$value_estimated_sum_20_39_h_f <- df_temp$sum_20_39_h_f.x + (x - 1975) * df_temp$difference_sum_20_39_h_f
  df_temp$value_estimated_sum_40_59_h_f <- df_temp$sum_40_59_h_f.x + (x - 1975) * df_temp$difference_sum_40_59_h_f
  df_temp$value_estimated_sum_60_64_h_f <- df_temp$sum_60_64_h_f.x + (x - 1975) * df_temp$difference_sum_60_64_h_f
  df_temp$value_estimated_sum_65_69_h_f <- df_temp$sum_65_69_h_f.x + (x - 1975) * df_temp$difference_sum_65_69_h_f
  df_temp$value_estimated_sum_70_74_h_f <- df_temp$sum_70_74_h_f.x + (x - 1975) * df_temp$difference_sum_70_74_h_f
  df_temp$value_estimated_sum_75_79_h_f <- df_temp$sum_75_79_h_f.x + (x - 1975) * df_temp$difference_sum_75_79_h_f
  df_temp$value_estimated_sum_80_plus_h_f <- df_temp$sum_80_plus_h_f.x + (x - 1975) * df_temp$difference_sum_80_plus_h_f
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1975) * df_temp$difference_population
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

RP_1975_age_sexe_final_2<-as.data.frame(list_df$df_1975)
RP_1975_age_sexe_final_2<-RP_1975_age_sexe_final_2[,c(1,40:52)]

RP_1976_age_sexe_final_2<-as.data.frame(list_df$df_1976)
RP_1976_age_sexe_final_2<-RP_1976_age_sexe_final_2[,c(1,40:52)]


RP_1977_age_sexe_final_2<-as.data.frame(list_df$df_1977)
RP_1977_age_sexe_final_2<-RP_1977_age_sexe_final_2[,c(1,40:52)]

RP_1978_age_sexe_final_2<-as.data.frame(list_df$df_1978)
RP_1978_age_sexe_final_2<-RP_1978_age_sexe_final_2[,c(1,40:52)]

RP_1979_age_sexe_final_2<-as.data.frame(list_df$df_1979)
RP_1979_age_sexe_final_2<-RP_1979_age_sexe_final_2[,c(1,40:52)]

RP_1980_age_sexe_final_2<-as.data.frame(list_df$df_1980)
RP_1980_age_sexe_final_2<-RP_1980_age_sexe_final_2[,c(1,40:52)]

RP_1981_age_sexe_final_2<-as.data.frame(list_df$df_1981)
RP_1981_age_sexe_final_2<-RP_1981_age_sexe_final_2[,c(1,40:52)]

RP_1982_age_sexe_final_2<-as.data.frame(list_df$df_1982)
RP_1982_age_sexe_final_2<-RP_1982_age_sexe_final_2[,c(1,40:52)]

write.csv(RP_1975_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final_2")

#library(readr)
#RP_1975_age_sexe_final_2 <- read_delim("/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final_2", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

write.csv(RP_1976_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1976_age_sexe_final_2")
write.csv(RP_1977_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1977_age_sexe_final_2")
write.csv(RP_1978_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1978_age_sexe_final_2")
write.csv(RP_1979_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1979_age_sexe_final_2")
write.csv(RP_1980_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1980_age_sexe_final_2")
write.csv(RP_1981_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1981_age_sexe_final_2")
write.csv(RP_1982_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final_2")

#############



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
library(readxl)
library(readr)

RP_2019_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_1975_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1982_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1990_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1999_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_2008_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_2013_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




df_merged <- merge(RP_1982_age_sexe_final, RP_1990_age_sexe_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1982 divisée par le nombre d'années pour chaque variable
df_merged$difference_sum_homme <- (df_merged$sum_homme.y - df_merged$sum_homme.x) / 8
df_merged$difference_sum_femme <- (df_merged$sum_femme.y - df_merged$sum_femme.x) / 8
df_merged$difference_sum_0_9_h_f <- (df_merged$sum_0_9_h_f.y - df_merged$sum_0_9_h_f.x) / 8
df_merged$difference_sum_10_19_h_f <- (df_merged$sum_10_19_h_f.y - df_merged$sum_10_19_h_f.x) / 8
df_merged$difference_sum_20_39_h_f <- (df_merged$sum_20_39_h_f.y - df_merged$sum_20_39_h_f.x) / 8
df_merged$difference_sum_40_59_h_f <- (df_merged$sum_40_59_h_f.y - df_merged$sum_40_59_h_f.x) / 8
df_merged$difference_sum_60_64_h_f <- (df_merged$sum_60_64_h_f.y - df_merged$sum_60_64_h_f.x) / 8
df_merged$difference_sum_65_69_h_f <- (df_merged$sum_65_69_h_f.y - df_merged$sum_65_69_h_f.x) / 8
df_merged$difference_sum_70_74_h_f <- (df_merged$sum_70_74_h_f.y - df_merged$sum_70_74_h_f.x) / 8
df_merged$difference_sum_75_79_h_f <- (df_merged$sum_75_79_h_f.y - df_merged$sum_75_79_h_f.x) / 8
df_merged$difference_sum_80_plus_h_f <- (df_merged$sum_80_plus_h_f.y - df_merged$sum_80_plus_h_f.x) / 8
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 8

# Créer une liste avec les années entre 1982 et 1980
years <- 1982:1990

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1982
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_sum_homme<- df_temp$sum_homme.x + (x - 1982) * df_temp$difference_sum_homme
  df_temp$value_estimated_sum_femme <- df_temp$sum_femme.x + (x - 1982) * df_temp$difference_sum_femme
  df_temp$value_estimated_sum_0_9_h_f <- df_temp$sum_0_9_h_f.x + (x - 1982) * df_temp$difference_sum_0_9_h_f
  df_temp$value_estimated_sum_10_19_h_f <- df_temp$sum_10_19_h_f.x + (x - 1982) * df_temp$difference_sum_10_19_h_f
  df_temp$value_estimated_sum_20_39_h_f <- df_temp$sum_20_39_h_f.x + (x - 1982) * df_temp$difference_sum_20_39_h_f
  df_temp$value_estimated_sum_40_59_h_f <- df_temp$sum_40_59_h_f.x + (x - 1982) * df_temp$difference_sum_40_59_h_f
  df_temp$value_estimated_sum_60_64_h_f <- df_temp$sum_60_64_h_f.x + (x - 1982) * df_temp$difference_sum_60_64_h_f
  df_temp$value_estimated_sum_65_69_h_f <- df_temp$sum_65_69_h_f.x + (x - 1982) * df_temp$difference_sum_65_69_h_f
  df_temp$value_estimated_sum_70_74_h_f <- df_temp$sum_70_74_h_f.x + (x - 1982) * df_temp$difference_sum_70_74_h_f
  df_temp$value_estimated_sum_75_79_h_f <- df_temp$sum_75_79_h_f.x + (x - 1982) * df_temp$difference_sum_75_79_h_f
  df_temp$value_estimated_sum_80_plus_h_f <- df_temp$sum_80_plus_h_f.x + (x - 1982) * df_temp$difference_sum_80_plus_h_f
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1982) * df_temp$difference_population
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

#RP_1982_age_sexe_final_2<-as.data.frame(list_df$df_1982)
#RP_1982_age_sexe_final_2<-RP_1982_age_sexe_final_2[,c(1,40:52)]

RP_1983_age_sexe_final_2<-as.data.frame(list_df$df_1983)
RP_1983_age_sexe_final_2<-RP_1983_age_sexe_final_2[,c(1,40:52)]

RP_1984_age_sexe_final_2<-as.data.frame(list_df$df_1984)
RP_1984_age_sexe_final_2<-RP_1984_age_sexe_final_2[,c(1,40:52)]

RP_1985_age_sexe_final_2<-as.data.frame(list_df$df_1985)
RP_1985_age_sexe_final_2<-RP_1985_age_sexe_final_2[,c(1,40:52)]

RP_1986_age_sexe_final_2<-as.data.frame(list_df$df_1986)
RP_1986_age_sexe_final_2<-RP_1986_age_sexe_final_2[,c(1,40:52)]

RP_1987_age_sexe_final_2<-as.data.frame(list_df$df_1987)
RP_1987_age_sexe_final_2<-RP_1987_age_sexe_final_2[,c(1,40:52)]

RP_1988_age_sexe_final_2<-as.data.frame(list_df$df_1988)
RP_1988_age_sexe_final_2<-RP_1988_age_sexe_final_2[,c(1,40:52)]

RP_1989_age_sexe_final_2<-as.data.frame(list_df$df_1989)
RP_1989_age_sexe_final_2<-RP_1989_age_sexe_final_2[,c(1,40:52)]

RP_1990_age_sexe_final_2<-as.data.frame(list_df$df_1990)
RP_1990_age_sexe_final_2<-RP_1990_age_sexe_final_2[,c(1,40:52)]

write.csv(RP_1983_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1983_age_sexe_final_2")


write.csv(RP_1984_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1984_age_sexe_final_2")
write.csv(RP_1985_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1985_age_sexe_final_2")
write.csv(RP_1986_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1986_age_sexe_final_2")
write.csv(RP_1987_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1987_age_sexe_final_2")
write.csv(RP_1988_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1988_age_sexe_final_2")
write.csv(RP_1989_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1989_age_sexe_final_2")
write.csv(RP_1990_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final_2")





#############



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
library(readxl)
library(readr)

RP_2019_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_1975_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1982_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1990_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1999_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_2008_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_2013_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




df_merged <- merge(RP_1990_age_sexe_final, RP_1999_age_sexe_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1990 divisée par le nombre d'années pour chaque variable
df_merged$difference_sum_homme <- (df_merged$sum_homme.y - df_merged$sum_homme.x) / 9
df_merged$difference_sum_femme <- (df_merged$sum_femme.y - df_merged$sum_femme.x) / 9
df_merged$difference_sum_0_9_h_f <- (df_merged$sum_0_9_h_f.y - df_merged$sum_0_9_h_f.x) / 9
df_merged$difference_sum_10_19_h_f <- (df_merged$sum_10_19_h_f.y - df_merged$sum_10_19_h_f.x) / 9
df_merged$difference_sum_20_39_h_f <- (df_merged$sum_20_39_h_f.y - df_merged$sum_20_39_h_f.x) / 9
df_merged$difference_sum_40_59_h_f <- (df_merged$sum_40_59_h_f.y - df_merged$sum_40_59_h_f.x) / 9
df_merged$difference_sum_60_64_h_f <- (df_merged$sum_60_64_h_f.y - df_merged$sum_60_64_h_f.x) / 9
df_merged$difference_sum_65_69_h_f <- (df_merged$sum_65_69_h_f.y - df_merged$sum_65_69_h_f.x) / 9
df_merged$difference_sum_70_74_h_f <- (df_merged$sum_70_74_h_f.y - df_merged$sum_70_74_h_f.x) / 9
df_merged$difference_sum_75_79_h_f <- (df_merged$sum_75_79_h_f.y - df_merged$sum_75_79_h_f.x) / 9
df_merged$difference_sum_80_plus_h_f <- (df_merged$sum_80_plus_h_f.y - df_merged$sum_80_plus_h_f.x) / 9
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 9

# Créer une liste avec les années entre 1990 et 1980
years <- 1990:1999

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1990
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_sum_homme<- df_temp$sum_homme.x + (x - 1990) * df_temp$difference_sum_homme
  df_temp$value_estimated_sum_femme <- df_temp$sum_femme.x + (x - 1990) * df_temp$difference_sum_femme
  df_temp$value_estimated_sum_0_9_h_f <- df_temp$sum_0_9_h_f.x + (x - 1990) * df_temp$difference_sum_0_9_h_f
  df_temp$value_estimated_sum_10_19_h_f <- df_temp$sum_10_19_h_f.x + (x - 1990) * df_temp$difference_sum_10_19_h_f
  df_temp$value_estimated_sum_20_39_h_f <- df_temp$sum_20_39_h_f.x + (x - 1990) * df_temp$difference_sum_20_39_h_f
  df_temp$value_estimated_sum_40_59_h_f <- df_temp$sum_40_59_h_f.x + (x - 1990) * df_temp$difference_sum_40_59_h_f
  df_temp$value_estimated_sum_60_64_h_f <- df_temp$sum_60_64_h_f.x + (x - 1990) * df_temp$difference_sum_60_64_h_f
  df_temp$value_estimated_sum_65_69_h_f <- df_temp$sum_65_69_h_f.x + (x - 1990) * df_temp$difference_sum_65_69_h_f
  df_temp$value_estimated_sum_70_74_h_f <- df_temp$sum_70_74_h_f.x + (x - 1990) * df_temp$difference_sum_70_74_h_f
  df_temp$value_estimated_sum_75_79_h_f <- df_temp$sum_75_79_h_f.x + (x - 1990) * df_temp$difference_sum_75_79_h_f
  df_temp$value_estimated_sum_80_plus_h_f <- df_temp$sum_80_plus_h_f.x + (x - 1990) * df_temp$difference_sum_80_plus_h_f
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1990) * df_temp$difference_population
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

#RP_1990_age_sexe_final_2<-as.data.frame(list_df$df_1990)
#RP_1990_age_sexe_final_2<-RP_1990_age_sexe_final_2[,c(1,40:52)]

RP_1991_age_sexe_final_2<-as.data.frame(list_df$df_1991)
RP_1991_age_sexe_final_2<-RP_1991_age_sexe_final_2[,c(1,40:52)]

RP_1992_age_sexe_final_2<-as.data.frame(list_df$df_1992)
RP_1992_age_sexe_final_2<-RP_1992_age_sexe_final_2[,c(1,40:52)]

RP_1993_age_sexe_final_2<-as.data.frame(list_df$df_1993)
RP_1993_age_sexe_final_2<-RP_1993_age_sexe_final_2[,c(1,40:52)]

RP_1994_age_sexe_final_2<-as.data.frame(list_df$df_1994)
RP_1994_age_sexe_final_2<-RP_1994_age_sexe_final_2[,c(1,40:52)]

RP_1995_age_sexe_final_2<-as.data.frame(list_df$df_1995)
RP_1995_age_sexe_final_2<-RP_1995_age_sexe_final_2[,c(1,40:52)]

RP_1996_age_sexe_final_2<-as.data.frame(list_df$df_1996)
RP_1996_age_sexe_final_2<-RP_1996_age_sexe_final_2[,c(1,40:52)]

RP_1997_age_sexe_final_2<-as.data.frame(list_df$df_1997)
RP_1997_age_sexe_final_2<-RP_1997_age_sexe_final_2[,c(1,40:52)]

RP_1998_age_sexe_final_2<-as.data.frame(list_df$df_1998)
RP_1998_age_sexe_final_2<-RP_1998_age_sexe_final_2[,c(1,40:52)]

RP_1999_age_sexe_final_2<-as.data.frame(list_df$df_1999)
RP_1999_age_sexe_final_2<-RP_1999_age_sexe_final_2[,c(1,40:52)]







write.csv(RP_1991_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1991_age_sexe_final_2")

write.csv(RP_1992_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1992_age_sexe_final_2")
write.csv(RP_1993_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1993_age_sexe_final_2")
write.csv(RP_1994_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1994_age_sexe_final_2")
write.csv(RP_1995_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1995_age_sexe_final_2")
write.csv(RP_1996_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1996_age_sexe_final_2")
write.csv(RP_1997_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1997_age_sexe_final_2")
write.csv(RP_1998_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1998_age_sexe_final_2")
write.csv(RP_1999_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final_2")





############



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
library(readxl)
library(readr)


RP_1975_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1982_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1990_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1999_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_2008_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final")

RP_2013_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final")

RP_2019_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final")


df_merged <- merge(RP_1999_age_sexe_final, RP_2008_age_sexe_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1999 divisée par le nombre d'années pour chaque variable
df_merged$difference_sum_homme <- (df_merged$sum_homme.y - df_merged$sum_homme.x) / 9
df_merged$difference_sum_femme <- (df_merged$sum_femme.y - df_merged$sum_femme.x) / 9
df_merged$difference_sum_0_9_h_f <- (df_merged$sum_0_9_h_f.y - df_merged$sum_0_9_h_f.x) / 9
df_merged$difference_sum_10_19_h_f <- (df_merged$sum_10_19_h_f.y - df_merged$sum_10_19_h_f.x) / 9
df_merged$difference_sum_20_39_h_f <- (df_merged$sum_20_39_h_f.y - df_merged$sum_20_39_h_f.x) / 9
df_merged$difference_sum_40_59_h_f <- (df_merged$sum_40_59_h_f.y - df_merged$sum_40_59_h_f.x) / 9
df_merged$difference_sum_60_64_h_f <- (df_merged$sum_60_64_h_f.y - df_merged$sum_60_64_h_f.x) / 9
df_merged$difference_sum_65_69_h_f <- (df_merged$sum_65_69_h_f.y - df_merged$sum_65_69_h_f.x) / 9
df_merged$difference_sum_70_74_h_f <- (df_merged$sum_70_74_h_f.y - df_merged$sum_70_74_h_f.x) / 9
df_merged$difference_sum_75_79_h_f <- (df_merged$sum_75_79_h_f.y - df_merged$sum_75_79_h_f.x) / 9
df_merged$difference_sum_80_plus_h_f <- (df_merged$sum_80_plus_h_f.y - df_merged$sum_80_plus_h_f.x) / 9
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 9

# Créer une liste avec les années entre 1999 et 1980
years <- 1999:2008

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1999
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_sum_homme<- df_temp$sum_homme.x + (x - 1999) * df_temp$difference_sum_homme
  df_temp$value_estimated_sum_femme <- df_temp$sum_femme.x + (x - 1999) * df_temp$difference_sum_femme
  df_temp$value_estimated_sum_0_9_h_f <- df_temp$sum_0_9_h_f.x + (x - 1999) * df_temp$difference_sum_0_9_h_f
  df_temp$value_estimated_sum_10_19_h_f <- df_temp$sum_10_19_h_f.x + (x - 1999) * df_temp$difference_sum_10_19_h_f
  df_temp$value_estimated_sum_20_39_h_f <- df_temp$sum_20_39_h_f.x + (x - 1999) * df_temp$difference_sum_20_39_h_f
  df_temp$value_estimated_sum_40_59_h_f <- df_temp$sum_40_59_h_f.x + (x - 1999) * df_temp$difference_sum_40_59_h_f
  df_temp$value_estimated_sum_60_64_h_f <- df_temp$sum_60_64_h_f.x + (x - 1999) * df_temp$difference_sum_60_64_h_f
  df_temp$value_estimated_sum_65_69_h_f <- df_temp$sum_65_69_h_f.x + (x - 1999) * df_temp$difference_sum_65_69_h_f
  df_temp$value_estimated_sum_70_74_h_f <- df_temp$sum_70_74_h_f.x + (x - 1999) * df_temp$difference_sum_70_74_h_f
  df_temp$value_estimated_sum_75_79_h_f <- df_temp$sum_75_79_h_f.x + (x - 1999) * df_temp$difference_sum_75_79_h_f
  df_temp$value_estimated_sum_80_plus_h_f <- df_temp$sum_80_plus_h_f.x + (x - 1999) * df_temp$difference_sum_80_plus_h_f
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1999) * df_temp$difference_population
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

#RP_1999_age_sexe_final_2<-as.data.frame(list_df$df_1999)
#RP_1999_age_sexe_final_2<-RP_1999_age_sexe_final_2[,c(1,40:52)]

RP_2000_age_sexe_final_2<-as.data.frame(list_df$df_2000)
RP_2000_age_sexe_final_2<-RP_2000_age_sexe_final_2[,c(1,40:52)]

RP_2001_age_sexe_final_2<-as.data.frame(list_df$df_2001)
RP_2001_age_sexe_final_2<-RP_2001_age_sexe_final_2[,c(1,40:52)]

RP_2002_age_sexe_final_2<-as.data.frame(list_df$df_2002)
RP_2002_age_sexe_final_2<-RP_2002_age_sexe_final_2[,c(1,40:52)]

RP_2003_age_sexe_final_2<-as.data.frame(list_df$df_2003)
RP_2003_age_sexe_final_2<-RP_2003_age_sexe_final_2[,c(1,40:52)]

RP_2004_age_sexe_final_2<-as.data.frame(list_df$df_2004)
RP_2004_age_sexe_final_2<-RP_2004_age_sexe_final_2[,c(1,40:52)]

RP_2005_age_sexe_final_2<-as.data.frame(list_df$df_2005)
RP_2005_age_sexe_final_2<-RP_2005_age_sexe_final_2[,c(1,40:52)]

RP_2006_age_sexe_final_2<-as.data.frame(list_df$df_2006)
RP_2006_age_sexe_final_2<-RP_2006_age_sexe_final_2[,c(1,40:52)]

RP_2007_age_sexe_final_2<-as.data.frame(list_df$df_2007)
RP_2007_age_sexe_final_2<-RP_2007_age_sexe_final_2[,c(1,40:52)]



RP_2008_age_sexe_final_2<-as.data.frame(list_df$df_2008)
RP_2008_age_sexe_final_2<-RP_2008_age_sexe_final_2[,c(1,40:52)]







write.csv(RP_2000_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2000_age_sexe_final_2")
write.csv(RP_2001_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2001_age_sexe_final_2")
write.csv(RP_2002_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2002_age_sexe_final_2")
write.csv(RP_2003_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2003_age_sexe_final_2")
write.csv(RP_2004_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2004_age_sexe_final_2")
write.csv(RP_2005_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2005_age_sexe_final_2")
write.csv(RP_2006_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2006_age_sexe_final_2")
write.csv(RP_2007_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2007_age_sexe_final_2")
write.csv(RP_2008_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final_2")










############



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
library(readxl)
library(readr)


RP_1975_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1982_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1990_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1999_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_2008_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final")

RP_2013_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final")

RP_2019_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final")


df_merged <- merge(RP_2008_age_sexe_final, RP_2013_age_sexe_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 2008 divisée par le nombre d'années pour chaque variable
df_merged$difference_sum_homme <- (df_merged$sum_homme.y - df_merged$sum_homme.x) / 5
df_merged$difference_sum_femme <- (df_merged$sum_femme.y - df_merged$sum_femme.x) / 5
df_merged$difference_sum_0_9_h_f <- (df_merged$sum_0_9_h_f.y - df_merged$sum_0_9_h_f.x) / 5
df_merged$difference_sum_10_19_h_f <- (df_merged$sum_10_19_h_f.y - df_merged$sum_10_19_h_f.x) / 5
df_merged$difference_sum_20_39_h_f <- (df_merged$sum_20_39_h_f.y - df_merged$sum_20_39_h_f.x) / 5
df_merged$difference_sum_40_59_h_f <- (df_merged$sum_40_59_h_f.y - df_merged$sum_40_59_h_f.x) / 5
df_merged$difference_sum_60_64_h_f <- (df_merged$sum_60_64_h_f.y - df_merged$sum_60_64_h_f.x) / 5
df_merged$difference_sum_65_69_h_f <- (df_merged$sum_65_69_h_f.y - df_merged$sum_65_69_h_f.x) / 5
df_merged$difference_sum_70_74_h_f <- (df_merged$sum_70_74_h_f.y - df_merged$sum_70_74_h_f.x) / 5
df_merged$difference_sum_75_79_h_f <- (df_merged$sum_75_79_h_f.y - df_merged$sum_75_79_h_f.x) / 5
df_merged$difference_sum_80_plus_h_f <- (df_merged$sum_80_plus_h_f.y - df_merged$sum_80_plus_h_f.x) / 5
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 5

# Créer une liste avec les années entre 2008 et 1980
years <- 2008:2013

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 2008
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_sum_homme<- df_temp$sum_homme.x + (x - 2008) * df_temp$difference_sum_homme
  df_temp$value_estimated_sum_femme <- df_temp$sum_femme.x + (x - 2008) * df_temp$difference_sum_femme
  df_temp$value_estimated_sum_0_9_h_f <- df_temp$sum_0_9_h_f.x + (x - 2008) * df_temp$difference_sum_0_9_h_f
  df_temp$value_estimated_sum_10_19_h_f <- df_temp$sum_10_19_h_f.x + (x - 2008) * df_temp$difference_sum_10_19_h_f
  df_temp$value_estimated_sum_20_39_h_f <- df_temp$sum_20_39_h_f.x + (x - 2008) * df_temp$difference_sum_20_39_h_f
  df_temp$value_estimated_sum_40_59_h_f <- df_temp$sum_40_59_h_f.x + (x - 2008) * df_temp$difference_sum_40_59_h_f
  df_temp$value_estimated_sum_60_64_h_f <- df_temp$sum_60_64_h_f.x + (x - 2008) * df_temp$difference_sum_60_64_h_f
  df_temp$value_estimated_sum_65_69_h_f <- df_temp$sum_65_69_h_f.x + (x - 2008) * df_temp$difference_sum_65_69_h_f
  df_temp$value_estimated_sum_70_74_h_f <- df_temp$sum_70_74_h_f.x + (x - 2008) * df_temp$difference_sum_70_74_h_f
  df_temp$value_estimated_sum_75_79_h_f <- df_temp$sum_75_79_h_f.x + (x - 2008) * df_temp$difference_sum_75_79_h_f
  df_temp$value_estimated_sum_80_plus_h_f <- df_temp$sum_80_plus_h_f.x + (x - 2008) * df_temp$difference_sum_80_plus_h_f
  df_temp$value_estimated_population <- df_temp$population.x + (x - 2008) * df_temp$difference_population
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

#RP_2008_age_sexe_final_2<-as.data.frame(list_df$df_2008)
#RP_2008_age_sexe_final_2<-RP_2008_age_sexe_final_2[,c(1,40:52)]

RP_2009_age_sexe_final_2<-as.data.frame(list_df$df_2009)
RP_2009_age_sexe_final_2<-RP_2009_age_sexe_final_2[,c(1,40:52)]

RP_2010_age_sexe_final_2<-as.data.frame(list_df$df_2010)
RP_2010_age_sexe_final_2<-RP_2010_age_sexe_final_2[,c(1,40:52)]

RP_2011_age_sexe_final_2<-as.data.frame(list_df$df_2011)
RP_2011_age_sexe_final_2<-RP_2011_age_sexe_final_2[,c(1,40:52)]

RP_2012_age_sexe_final_2<-as.data.frame(list_df$df_2012)
RP_2012_age_sexe_final_2<-RP_2012_age_sexe_final_2[,c(1,40:52)]


RP_2013_age_sexe_final_2<-as.data.frame(list_df$df_2013)
RP_2013_age_sexe_final_2<-RP_2013_age_sexe_final_2[,c(1,40:52)]







write.csv(RP_2009_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2009_age_sexe_final_2")
write.csv(RP_2010_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2010_age_sexe_final_2")
write.csv(RP_2011_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2011_age_sexe_final_2")
write.csv(RP_2012_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2012_age_sexe_final_2")
write.csv(RP_2013_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final_2")






names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_homme"]<-"value_estimated_sum_homme"

names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_femme"]<-"value_estimated_sum_femme"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_0_9_h_f"]<-"value_estimated_sum_0_9_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_10_19_h_f"]<-"value_estimated_sum_10_19_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_20_39_h_f"]<-"value_estimated_sum_20_39_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_40_59_h_f"]<-"value_estimated_sum_40_59_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_60_64_h_f"]<-"value_estimated_sum_60_64_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_65_69_h_f"]<-"value_estimated_sum_65_69_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_70_74_h_f"]<-"value_estimated_sum_70_74_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_75_79_h_f"]<-"value_estimated_sum_75_79_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="sum_80_plus_h_f"]<-"value_estimated_sum_80_plus_h_f"
names(RP_2013_age_sexe_final)[names(RP_2013_age_sexe_final)=="population"]<-"value_estimated_population"


RP_2013_age_sexe_final<-RP_2013_age_sexe_final[,-1]

RP_2013_age_sexe_final$year<-2013


write.csv(RP_2013_age_sexe_final, "/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final_2")










############



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
library(readxl)
library(readr)


RP_1975_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1975_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1982_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1990_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
RP_1999_age_sexe_final <- read_delim("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final", 
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

RP_2008_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final")

RP_2013_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final")

RP_2019_age_sexe_final <- read_csv("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final")


df_merged <- merge(RP_2013_age_sexe_final, RP_2019_age_sexe_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 2013 divisée par le nombre d'années pour chaque variable
df_merged$difference_sum_homme <- (df_merged$sum_homme.y - df_merged$sum_homme.x) / 6
df_merged$difference_sum_femme <- (df_merged$sum_femme.y - df_merged$sum_femme.x) / 6
df_merged$difference_sum_0_9_h_f <- (df_merged$sum_0_9_h_f.y - df_merged$sum_0_9_h_f.x) / 6
df_merged$difference_sum_10_19_h_f <- (df_merged$sum_10_19_h_f.y - df_merged$sum_10_19_h_f.x) / 6
df_merged$difference_sum_20_39_h_f <- (df_merged$sum_20_39_h_f.y - df_merged$sum_20_39_h_f.x) / 6
df_merged$difference_sum_40_59_h_f <- (df_merged$sum_40_59_h_f.y - df_merged$sum_40_59_h_f.x) / 6
df_merged$difference_sum_60_64_h_f <- (df_merged$sum_60_64_h_f.y - df_merged$sum_60_64_h_f.x) / 6
df_merged$difference_sum_65_69_h_f <- (df_merged$sum_65_69_h_f.y - df_merged$sum_65_69_h_f.x) / 6
df_merged$difference_sum_70_74_h_f <- (df_merged$sum_70_74_h_f.y - df_merged$sum_70_74_h_f.x) / 6
df_merged$difference_sum_75_79_h_f <- (df_merged$sum_75_79_h_f.y - df_merged$sum_75_79_h_f.x) / 6
df_merged$difference_sum_80_plus_h_f <- (df_merged$sum_80_plus_h_f.y - df_merged$sum_80_plus_h_f.x) / 6
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 6

# Créer une liste avec les années entre 2013 et 1980
years <- 2013:2019

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 2013
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_sum_homme<- df_temp$sum_homme.x + (x - 2013) * df_temp$difference_sum_homme
  df_temp$value_estimated_sum_femme <- df_temp$sum_femme.x + (x - 2013) * df_temp$difference_sum_femme
  df_temp$value_estimated_sum_0_9_h_f <- df_temp$sum_0_9_h_f.x + (x - 2013) * df_temp$difference_sum_0_9_h_f
  df_temp$value_estimated_sum_10_19_h_f <- df_temp$sum_10_19_h_f.x + (x - 2013) * df_temp$difference_sum_10_19_h_f
  df_temp$value_estimated_sum_20_39_h_f <- df_temp$sum_20_39_h_f.x + (x - 2013) * df_temp$difference_sum_20_39_h_f
  df_temp$value_estimated_sum_40_59_h_f <- df_temp$sum_40_59_h_f.x + (x - 2013) * df_temp$difference_sum_40_59_h_f
  df_temp$value_estimated_sum_60_64_h_f <- df_temp$sum_60_64_h_f.x + (x - 2013) * df_temp$difference_sum_60_64_h_f
  df_temp$value_estimated_sum_65_69_h_f <- df_temp$sum_65_69_h_f.x + (x - 2013) * df_temp$difference_sum_65_69_h_f
  df_temp$value_estimated_sum_70_74_h_f <- df_temp$sum_70_74_h_f.x + (x - 2013) * df_temp$difference_sum_70_74_h_f
  df_temp$value_estimated_sum_75_79_h_f <- df_temp$sum_75_79_h_f.x + (x - 2013) * df_temp$difference_sum_75_79_h_f
  df_temp$value_estimated_sum_80_plus_h_f <- df_temp$sum_80_plus_h_f.x + (x - 2013) * df_temp$difference_sum_80_plus_h_f
  df_temp$value_estimated_population <- df_temp$population.x + (x - 2013) * df_temp$difference_population
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

#RP_2013_age_sexe_final_2<-as.data.frame(list_df$df_2013)
#RP_2013_age_sexe_final_2<-RP_2013_age_sexe_final_2[,c(1,40:52)]

RP_2014_age_sexe_final_2<-as.data.frame(list_df$df_2014)
RP_2014_age_sexe_final_2<-RP_2014_age_sexe_final_2[,c(1,40:52)]

RP_2015_age_sexe_final_2<-as.data.frame(list_df$df_2015)
RP_2015_age_sexe_final_2<-RP_2015_age_sexe_final_2[,c(1,40:52)]

RP_2016_age_sexe_final_2<-as.data.frame(list_df$df_2016)
RP_2016_age_sexe_final_2<-RP_2016_age_sexe_final_2[,c(1,40:52)]

RP_2017_age_sexe_final_2<-as.data.frame(list_df$df_2017)
RP_2017_age_sexe_final_2<-RP_2017_age_sexe_final_2[,c(1,40:52)]

RP_2018_age_sexe_final_2<-as.data.frame(list_df$df_2018)
RP_2018_age_sexe_final_2<-RP_2018_age_sexe_final_2[,c(1,40:52)]



RP_2019_age_sexe_final_2<-as.data.frame(list_df$df_2019)
RP_2019_age_sexe_final_2<-RP_2019_age_sexe_final_2[,c(1,40:52)]







write.csv(RP_2014_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2014_age_sexe_final_2")
write.csv(RP_2015_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2015_age_sexe_final_2")
write.csv(RP_2016_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2016_age_sexe_final_2")
write.csv(RP_2017_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2017_age_sexe_final_2")
write.csv(RP_2018_age_sexe_final_2, "/recensement 1980-2022/rp travaillé/RP_2018_age_sexe_final_2")





names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_homme"]<-"value_estimated_sum_homme"

names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_femme"]<-"value_estimated_sum_femme"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_0_9_h_f"]<-"value_estimated_sum_0_9_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_10_19_h_f"]<-"value_estimated_sum_10_19_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_20_39_h_f"]<-"value_estimated_sum_20_39_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_40_59_h_f"]<-"value_estimated_sum_40_59_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_60_64_h_f"]<-"value_estimated_sum_60_64_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_65_69_h_f"]<-"value_estimated_sum_65_69_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_70_74_h_f"]<-"value_estimated_sum_70_74_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_75_79_h_f"]<-"value_estimated_sum_75_79_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="sum_80_plus_h_f"]<-"value_estimated_sum_80_plus_h_f"
names(RP_2019_age_sexe_final)[names(RP_2019_age_sexe_final)=="population"]<-"value_estimated_population"


RP_2019_age_sexe_final<-RP_2019_age_sexe_final[,-1]

RP_2019_age_sexe_final$year<-2019


write.csv(RP_2019_age_sexe_final, "/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final_2")






