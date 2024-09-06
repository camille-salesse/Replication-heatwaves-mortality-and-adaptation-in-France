


#work on socio-demographic data


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


RP_1975_CSP <- read_excel("recensement 1980-2022/RP_1975_CSP.xlsx")

names(RP_1975_CSP)[names(RP_1975_CSP)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1975_CSP$COM <- paste(RP_1975_CSP$DEP_geo_courante, RP_1975_CSP$COM_geo_courante, sep = "")


names(RP_1975_CSP)[names(RP_1975_CSP)=="Agriculteurs\r\nActifs ayant un emploi\r\nRP1975"]<-"agriculteur_en_emploi"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Agriculteurs\r\nChômeurs\r\nRP1975"]<-"agriculteur_au_chomage"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nActifs ayant un emploi\r\nRP1975"]<-"artisan_commercant_chef_entreprise_en_emploi"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nChômeurs\r\nRP1975"]<-"artisan_commercant_chef_entreprise_au_chomage"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Cadres et professions intellectuelles supérieures\r\nActifs ayant un emploi\r\nRP1975"]<-"cadre_en_emploi"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Cadres et professions intellectuelles supérieures\r\nChômeurs\r\nRP1975"]<-"cadre_au_chomage"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Professions intermédiaires\r\nActifs ayant un emploi\r\nRP1975"]<-"profession_intermediaire_en_emploi"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Professions intermédiaires\r\nChômeurs\r\nRP1975"]<-"profession_intermediaire_au_chomage"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Employés\r\nActifs ayant un emploi\r\nRP1975"]<-"employe_en_emploi"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Employés\r\nChômeurs\r\nRP1975"]<-"employe_au_chomage"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Ouvriers\r\nActifs ayant un emploi\r\nRP1975"]<-"ouvrier_en_emploi"
names(RP_1975_CSP)[names(RP_1975_CSP)=="Ouvriers\r\nChômeurs\r\nRP1975"]<-"ouvrier_au_chomage"



RP_1975_CSP$agriculteur<-RP_1975_CSP$agriculteur_en_emploi+RP_1975_CSP$agriculteur_au_chomage

RP_1975_CSP$artisan_commercant_chef_entreprise<-RP_1975_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1975_CSP$artisan_commercant_chef_entreprise_au_chomage

RP_1975_CSP$cadre<-RP_1975_CSP$cadre_en_emploi+RP_1975_CSP$cadre_au_chomage

RP_1975_CSP$profession_intermediaire<-RP_1975_CSP$profession_intermediaire_en_emploi+RP_1975_CSP$profession_intermediaire_au_chomage

RP_1975_CSP$employe<-RP_1975_CSP$employe_en_emploi+RP_1975_CSP$employe_au_chomage

RP_1975_CSP$ouvrier<-RP_1975_CSP$ouvrier_en_emploi+RP_1975_CSP$ouvrier_au_chomage


RP_1975_CSP$population<- rowSums(RP_1975_CSP[,20:25])


RP_1975_CSP$en_emploi<-RP_1975_CSP$agriculteur_en_emploi+RP_1975_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1975_CSP$cadre_en_emploi+RP_1975_CSP$profession_intermediaire_en_emploi+RP_1975_CSP$employe_en_emploi+RP_1975_CSP$ouvrier_en_emploi


RP_1975_CSP$au_chomage<-RP_1975_CSP$agriculteur_au_chomage+RP_1975_CSP$artisan_commercant_chef_entreprise_au_chomage+RP_1975_CSP$cadre_au_chomage+RP_1975_CSP$profession_intermediaire_au_chomage+RP_1975_CSP$employe_au_chomage+RP_1975_CSP$ouvrier_au_chomage

#RP_1975_CSP$part_chomage<- RP_1975_CSP$au_chomage/RP_1975_CSP$population



RP_1975_CSP<-RP_1975_CSP[,19:28]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1975_CSP<-left_join(RP_1975_CSP,table_passage_bis)

RP_1975_CSP$COM_AP<-ifelse(!is.na(RP_1975_CSP$COM_AP),RP_1975_CSP$COM_AP,RP_1975_CSP$COM)


ag_1<-aggregate(agriculteur~COM_AP,RP_1975_CSP,sum)
ag_2<-aggregate(artisan_commercant_chef_entreprise~COM_AP,RP_1975_CSP,sum)
ag_3<-aggregate(cadre~COM_AP,RP_1975_CSP,sum)
ag_4<-aggregate(profession_intermediaire~COM_AP,RP_1975_CSP,sum)
ag_5<-aggregate(employe~COM_AP,RP_1975_CSP,sum)
ag_6<-aggregate(ouvrier~COM_AP,RP_1975_CSP,sum)
ag_7<-aggregate(population~COM_AP,RP_1975_CSP,sum)
ag_8<-aggregate(en_emploi~COM_AP,RP_1975_CSP,sum)
ag_9<-aggregate(au_chomage~COM_AP,RP_1975_CSP,sum)
#ag_10<-aggregate(part_chomage~COM_AP,RP_1975_CSP,sum)

RP_1975_CSP_final<-left_join(ag_1,ag_2)
RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_3)
RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_4)
RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_5)
RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_6)
RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_7)
RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_8)
RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_9)
#RP_1975_CSP_final<-left_join(RP_1975_CSP_final,ag_10)



write.csv(RP_1975_CSP_final, "recensement 1980-2022/rp travaillé/RP_1975_CSP_final")

#library(readr)
#RP_1975_CSP_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1975_CSP_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




#################



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


RP_1982_CSP <- read_excel("recensement 1980-2022/RP_1982_CSP.xlsx")

names(RP_1982_CSP)[names(RP_1982_CSP)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1982_CSP$COM <- paste(RP_1982_CSP$DEP_geo_courante, RP_1982_CSP$COM_geo_courante, sep = "")


names(RP_1982_CSP)[names(RP_1982_CSP)=="Agriculteurs\r\nActifs ayant un emploi\r\nRP1982"]<-"agriculteur_en_emploi"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Agriculteurs\r\nChômeurs\r\nRP1982"]<-"agriculteur_au_chomage"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nActifs ayant un emploi\r\nRP1982"]<-"artisan_commercant_chef_entreprise_en_emploi"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nChômeurs\r\nRP1982"]<-"artisan_commercant_chef_entreprise_au_chomage"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Cadres et professions intellectuelles supérieures\r\nActifs ayant un emploi\r\nRP1982"]<-"cadre_en_emploi"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Cadres et professions intellectuelles supérieures\r\nChômeurs\r\nRP1982"]<-"cadre_au_chomage"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Professions intermédiaires\r\nActifs ayant un emploi\r\nRP1982"]<-"profession_intermediaire_en_emploi"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Professions intermédiaires\r\nChômeurs\r\nRP1982"]<-"profession_intermediaire_au_chomage"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Employés\r\nActifs ayant un emploi\r\nRP1982"]<-"employe_en_emploi"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Employés\r\nChômeurs\r\nRP1982"]<-"employe_au_chomage"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Ouvriers\r\nActifs ayant un emploi\r\nRP1982"]<-"ouvrier_en_emploi"
names(RP_1982_CSP)[names(RP_1982_CSP)=="Ouvriers\r\nChômeurs\r\nRP1982"]<-"ouvrier_au_chomage"



RP_1982_CSP$agriculteur<-RP_1982_CSP$agriculteur_en_emploi+RP_1982_CSP$agriculteur_au_chomage

RP_1982_CSP$artisan_commercant_chef_entreprise<-RP_1982_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1982_CSP$artisan_commercant_chef_entreprise_au_chomage

RP_1982_CSP$cadre<-RP_1982_CSP$cadre_en_emploi+RP_1982_CSP$cadre_au_chomage

RP_1982_CSP$profession_intermediaire<-RP_1982_CSP$profession_intermediaire_en_emploi+RP_1982_CSP$profession_intermediaire_au_chomage

RP_1982_CSP$employe<-RP_1982_CSP$employe_en_emploi+RP_1982_CSP$employe_au_chomage

RP_1982_CSP$ouvrier<-RP_1982_CSP$ouvrier_en_emploi+RP_1982_CSP$ouvrier_au_chomage


RP_1982_CSP$population<- rowSums(RP_1982_CSP[,20:25])


RP_1982_CSP$en_emploi<-RP_1982_CSP$agriculteur_en_emploi+RP_1982_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1982_CSP$cadre_en_emploi+RP_1982_CSP$profession_intermediaire_en_emploi+RP_1982_CSP$employe_en_emploi+RP_1982_CSP$ouvrier_en_emploi


RP_1982_CSP$au_chomage<-RP_1982_CSP$agriculteur_au_chomage+RP_1982_CSP$artisan_commercant_chef_entreprise_au_chomage+RP_1982_CSP$cadre_au_chomage+RP_1982_CSP$profession_intermediaire_au_chomage+RP_1982_CSP$employe_au_chomage+RP_1982_CSP$ouvrier_au_chomage

#RP_1982_CSP$part_chomage<- RP_1982_CSP$au_chomage/RP_1982_CSP$population



RP_1982_CSP<-RP_1982_CSP[,19:28]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1982_CSP<-left_join(RP_1982_CSP,table_passage_bis)

RP_1982_CSP$COM_AP<-ifelse(!is.na(RP_1982_CSP$COM_AP),RP_1982_CSP$COM_AP,RP_1982_CSP$COM)


ag_1<-aggregate(agriculteur~COM_AP,RP_1982_CSP,sum)
ag_2<-aggregate(artisan_commercant_chef_entreprise~COM_AP,RP_1982_CSP,sum)
ag_3<-aggregate(cadre~COM_AP,RP_1982_CSP,sum)
ag_4<-aggregate(profession_intermediaire~COM_AP,RP_1982_CSP,sum)
ag_5<-aggregate(employe~COM_AP,RP_1982_CSP,sum)
ag_6<-aggregate(ouvrier~COM_AP,RP_1982_CSP,sum)
ag_7<-aggregate(population~COM_AP,RP_1982_CSP,sum)
ag_8<-aggregate(en_emploi~COM_AP,RP_1982_CSP,sum)
ag_9<-aggregate(au_chomage~COM_AP,RP_1982_CSP,sum)
#ag_10<-aggregate(part_chomage~COM_AP,RP_1982_CSP,sum)

RP_1982_CSP_final<-left_join(ag_1,ag_2)
RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_3)
RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_4)
RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_5)
RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_6)
RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_7)
RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_8)
RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_9)
#RP_1982_CSP_final<-left_join(RP_1982_CSP_final,ag_10)



write.csv(RP_1982_CSP_final, "recensement 1980-2022/rp travaillé/RP_1982_CSP_final")

#library(readr)
#RP_1982_CSP_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1982_CSP_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




###########


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


RP_1990_CSP <- read_excel("recensement 1980-2022/RP_1990_CSP.xlsx")

names(RP_1990_CSP)[names(RP_1990_CSP)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1990_CSP$COM <- paste(RP_1990_CSP$DEP_geo_courante, RP_1990_CSP$COM_geo_courante, sep = "")


names(RP_1990_CSP)[names(RP_1990_CSP)=="Agriculteurs\r\nActifs ayant un emploi\r\nRP1990"]<-"agriculteur_en_emploi"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Agriculteurs\r\nChômeurs\r\nRP1990"]<-"agriculteur_au_chomage"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nActifs ayant un emploi\r\nRP1990"]<-"artisan_commercant_chef_entreprise_en_emploi"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nChômeurs\r\nRP1990"]<-"artisan_commercant_chef_entreprise_au_chomage"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Cadres et professions intellectuelles supérieures\r\nActifs ayant un emploi\r\nRP1990"]<-"cadre_en_emploi"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Cadres et professions intellectuelles supérieures\r\nChômeurs\r\nRP1990"]<-"cadre_au_chomage"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Professions intermédiaires\r\nActifs ayant un emploi\r\nRP1990"]<-"profession_intermediaire_en_emploi"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Professions intermédiaires\r\nChômeurs\r\nRP1990"]<-"profession_intermediaire_au_chomage"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Employés\r\nActifs ayant un emploi\r\nRP1990"]<-"employe_en_emploi"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Employés\r\nChômeurs\r\nRP1990"]<-"employe_au_chomage"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Ouvriers\r\nActifs ayant un emploi\r\nRP1990"]<-"ouvrier_en_emploi"
names(RP_1990_CSP)[names(RP_1990_CSP)=="Ouvriers\r\nChômeurs\r\nRP1990"]<-"ouvrier_au_chomage"



RP_1990_CSP$agriculteur<-RP_1990_CSP$agriculteur_en_emploi+RP_1990_CSP$agriculteur_au_chomage

RP_1990_CSP$artisan_commercant_chef_entreprise<-RP_1990_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1990_CSP$artisan_commercant_chef_entreprise_au_chomage

RP_1990_CSP$cadre<-RP_1990_CSP$cadre_en_emploi+RP_1990_CSP$cadre_au_chomage

RP_1990_CSP$profession_intermediaire<-RP_1990_CSP$profession_intermediaire_en_emploi+RP_1990_CSP$profession_intermediaire_au_chomage

RP_1990_CSP$employe<-RP_1990_CSP$employe_en_emploi+RP_1990_CSP$employe_au_chomage

RP_1990_CSP$ouvrier<-RP_1990_CSP$ouvrier_en_emploi+RP_1990_CSP$ouvrier_au_chomage


RP_1990_CSP$population<- rowSums(RP_1990_CSP[,20:25])


RP_1990_CSP$en_emploi<-RP_1990_CSP$agriculteur_en_emploi+RP_1990_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1990_CSP$cadre_en_emploi+RP_1990_CSP$profession_intermediaire_en_emploi+RP_1990_CSP$employe_en_emploi+RP_1990_CSP$ouvrier_en_emploi


RP_1990_CSP$au_chomage<-RP_1990_CSP$agriculteur_au_chomage+RP_1990_CSP$artisan_commercant_chef_entreprise_au_chomage+RP_1990_CSP$cadre_au_chomage+RP_1990_CSP$profession_intermediaire_au_chomage+RP_1990_CSP$employe_au_chomage+RP_1990_CSP$ouvrier_au_chomage

#RP_1990_CSP$part_chomage<- RP_1990_CSP$au_chomage/RP_1990_CSP$population



RP_1990_CSP<-RP_1990_CSP[,19:28]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1990_CSP<-left_join(RP_1990_CSP,table_passage_bis)

RP_1990_CSP$COM_AP<-ifelse(!is.na(RP_1990_CSP$COM_AP),RP_1990_CSP$COM_AP,RP_1990_CSP$COM)


ag_1<-aggregate(agriculteur~COM_AP,RP_1990_CSP,sum)
ag_2<-aggregate(artisan_commercant_chef_entreprise~COM_AP,RP_1990_CSP,sum)
ag_3<-aggregate(cadre~COM_AP,RP_1990_CSP,sum)
ag_4<-aggregate(profession_intermediaire~COM_AP,RP_1990_CSP,sum)
ag_5<-aggregate(employe~COM_AP,RP_1990_CSP,sum)
ag_6<-aggregate(ouvrier~COM_AP,RP_1990_CSP,sum)
ag_7<-aggregate(population~COM_AP,RP_1990_CSP,sum)
ag_8<-aggregate(en_emploi~COM_AP,RP_1990_CSP,sum)
ag_9<-aggregate(au_chomage~COM_AP,RP_1990_CSP,sum)
#ag_10<-aggregate(part_chomage~COM_AP,RP_1990_CSP,sum)

RP_1990_CSP_final<-left_join(ag_1,ag_2)
RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_3)
RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_4)
RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_5)
RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_6)
RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_7)
RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_8)
RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_9)
#RP_1990_CSP_final<-left_join(RP_1990_CSP_final,ag_10)



write.csv(RP_1990_CSP_final, "recensement 1980-2022/rp travaillé/RP_1990_CSP_final")

#library(readr)
#RP_1990_CSP_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1990_CSP_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)





###########



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


RP_1999_CSP <- read_excel("recensement 1980-2022/RP_1999_CSP.xlsx")

names(RP_1999_CSP)[names(RP_1999_CSP)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_1999_CSP$COM <- paste(RP_1999_CSP$DEP_geo_courante, RP_1999_CSP$COM_geo_courante, sep = "")


names(RP_1999_CSP)[names(RP_1999_CSP)=="Agriculteurs\r\nActifs ayant un emploi\r\nRP1999"]<-"agriculteur_en_emploi"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Agriculteurs\r\nChômeurs\r\nRP1999"]<-"agriculteur_au_chomage"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nActifs ayant un emploi\r\nRP1999"]<-"artisan_commercant_chef_entreprise_en_emploi"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nChômeurs\r\nRP1999"]<-"artisan_commercant_chef_entreprise_au_chomage"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Cadres et professions intellectuelles supérieures\r\nActifs ayant un emploi\r\nRP1999"]<-"cadre_en_emploi"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Cadres et professions intellectuelles supérieures\r\nChômeurs\r\nRP1999"]<-"cadre_au_chomage"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Professions intermédiaires\r\nActifs ayant un emploi\r\nRP1999"]<-"profession_intermediaire_en_emploi"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Professions intermédiaires\r\nChômeurs\r\nRP1999"]<-"profession_intermediaire_au_chomage"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Employés\r\nActifs ayant un emploi\r\nRP1999"]<-"employe_en_emploi"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Employés\r\nChômeurs\r\nRP1999"]<-"employe_au_chomage"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Ouvriers\r\nActifs ayant un emploi\r\nRP1999"]<-"ouvrier_en_emploi"
names(RP_1999_CSP)[names(RP_1999_CSP)=="Ouvriers\r\nChômeurs\r\nRP1999"]<-"ouvrier_au_chomage"



RP_1999_CSP$agriculteur<-RP_1999_CSP$agriculteur_en_emploi+RP_1999_CSP$agriculteur_au_chomage

RP_1999_CSP$artisan_commercant_chef_entreprise<-RP_1999_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1999_CSP$artisan_commercant_chef_entreprise_au_chomage

RP_1999_CSP$cadre<-RP_1999_CSP$cadre_en_emploi+RP_1999_CSP$cadre_au_chomage

RP_1999_CSP$profession_intermediaire<-RP_1999_CSP$profession_intermediaire_en_emploi+RP_1999_CSP$profession_intermediaire_au_chomage

RP_1999_CSP$employe<-RP_1999_CSP$employe_en_emploi+RP_1999_CSP$employe_au_chomage

RP_1999_CSP$ouvrier<-RP_1999_CSP$ouvrier_en_emploi+RP_1999_CSP$ouvrier_au_chomage


RP_1999_CSP$population<- rowSums(RP_1999_CSP[,20:25])


RP_1999_CSP$en_emploi<-RP_1999_CSP$agriculteur_en_emploi+RP_1999_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_1999_CSP$cadre_en_emploi+RP_1999_CSP$profession_intermediaire_en_emploi+RP_1999_CSP$employe_en_emploi+RP_1999_CSP$ouvrier_en_emploi


RP_1999_CSP$au_chomage<-RP_1999_CSP$agriculteur_au_chomage+RP_1999_CSP$artisan_commercant_chef_entreprise_au_chomage+RP_1999_CSP$cadre_au_chomage+RP_1999_CSP$profession_intermediaire_au_chomage+RP_1999_CSP$employe_au_chomage+RP_1999_CSP$ouvrier_au_chomage

#RP_1999_CSP$part_chomage<- RP_1999_CSP$au_chomage/RP_1999_CSP$population



RP_1999_CSP<-RP_1999_CSP[,19:28]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_1999_CSP<-left_join(RP_1999_CSP,table_passage_bis)

RP_1999_CSP$COM_AP<-ifelse(!is.na(RP_1999_CSP$COM_AP),RP_1999_CSP$COM_AP,RP_1999_CSP$COM)


ag_1<-aggregate(agriculteur~COM_AP,RP_1999_CSP,sum)
ag_2<-aggregate(artisan_commercant_chef_entreprise~COM_AP,RP_1999_CSP,sum)
ag_3<-aggregate(cadre~COM_AP,RP_1999_CSP,sum)
ag_4<-aggregate(profession_intermediaire~COM_AP,RP_1999_CSP,sum)
ag_5<-aggregate(employe~COM_AP,RP_1999_CSP,sum)
ag_6<-aggregate(ouvrier~COM_AP,RP_1999_CSP,sum)
ag_7<-aggregate(population~COM_AP,RP_1999_CSP,sum)
ag_8<-aggregate(en_emploi~COM_AP,RP_1999_CSP,sum)
ag_9<-aggregate(au_chomage~COM_AP,RP_1999_CSP,sum)
#ag_10<-aggregate(part_chomage~COM_AP,RP_1999_CSP,sum)

RP_1999_CSP_final<-left_join(ag_1,ag_2)
RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_3)
RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_4)
RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_5)
RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_6)
RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_7)
RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_8)
RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_9)
#RP_1999_CSP_final<-left_join(RP_1999_CSP_final,ag_10)



write.csv(RP_1999_CSP_final, "recensement 1980-2022/rp travaillé/RP_1999_CSP_final")

#library(readr)
#RP_1999_CSP_final <- read_delim("recensement 1980-2022/rp travaillé/RP_1999_CSP_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




###########


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


RP_2008_CSP <- read_excel("recensement 1980-2022/RP_2008_CSP.xlsx")

names(RP_2008_CSP)[names(RP_2008_CSP)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_2008_CSP$COM <- paste(RP_2008_CSP$DEP_geo_courante, RP_2008_CSP$COM_geo_courante, sep = "")


names(RP_2008_CSP)[names(RP_2008_CSP)=="Agriculteurs\r\nActifs ayant un emploi\r\nRP2008"]<-"agriculteur_en_emploi"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Agriculteurs\r\nChômeurs\r\nRP2008"]<-"agriculteur_au_chomage"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nActifs ayant un emploi\r\nRP2008"]<-"artisan_commercant_chef_entreprise_en_emploi"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nChômeurs\r\nRP2008"]<-"artisan_commercant_chef_entreprise_au_chomage"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Cadres et professions intellectuelles supérieures\r\nActifs ayant un emploi\r\nRP2008"]<-"cadre_en_emploi"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Cadres et professions intellectuelles supérieures\r\nChômeurs\r\nRP2008"]<-"cadre_au_chomage"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Professions intermédiaires\r\nActifs ayant un emploi\r\nRP2008"]<-"profession_intermediaire_en_emploi"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Professions intermédiaires\r\nChômeurs\r\nRP2008"]<-"profession_intermediaire_au_chomage"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Employés\r\nActifs ayant un emploi\r\nRP2008"]<-"employe_en_emploi"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Employés\r\nChômeurs\r\nRP2008"]<-"employe_au_chomage"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Ouvriers\r\nActifs ayant un emploi\r\nRP2008"]<-"ouvrier_en_emploi"
names(RP_2008_CSP)[names(RP_2008_CSP)=="Ouvriers\r\nChômeurs\r\nRP2008"]<-"ouvrier_au_chomage"



RP_2008_CSP$agriculteur<-RP_2008_CSP$agriculteur_en_emploi+RP_2008_CSP$agriculteur_au_chomage

RP_2008_CSP$artisan_commercant_chef_entreprise<-RP_2008_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_2008_CSP$artisan_commercant_chef_entreprise_au_chomage

RP_2008_CSP$cadre<-RP_2008_CSP$cadre_en_emploi+RP_2008_CSP$cadre_au_chomage

RP_2008_CSP$profession_intermediaire<-RP_2008_CSP$profession_intermediaire_en_emploi+RP_2008_CSP$profession_intermediaire_au_chomage

RP_2008_CSP$employe<-RP_2008_CSP$employe_en_emploi+RP_2008_CSP$employe_au_chomage

RP_2008_CSP$ouvrier<-RP_2008_CSP$ouvrier_en_emploi+RP_2008_CSP$ouvrier_au_chomage


RP_2008_CSP$population<- rowSums(RP_2008_CSP[,20:25])


RP_2008_CSP$en_emploi<-RP_2008_CSP$agriculteur_en_emploi+RP_2008_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_2008_CSP$cadre_en_emploi+RP_2008_CSP$profession_intermediaire_en_emploi+RP_2008_CSP$employe_en_emploi+RP_2008_CSP$ouvrier_en_emploi


RP_2008_CSP$au_chomage<-RP_2008_CSP$agriculteur_au_chomage+RP_2008_CSP$artisan_commercant_chef_entreprise_au_chomage+RP_2008_CSP$cadre_au_chomage+RP_2008_CSP$profession_intermediaire_au_chomage+RP_2008_CSP$employe_au_chomage+RP_2008_CSP$ouvrier_au_chomage

#RP_2008_CSP$part_chomage<- RP_2008_CSP$au_chomage/RP_2008_CSP$population



RP_2008_CSP<-RP_2008_CSP[,19:28]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_2008_CSP<-left_join(RP_2008_CSP,table_passage_bis)

RP_2008_CSP$COM_AP<-ifelse(!is.na(RP_2008_CSP$COM_AP),RP_2008_CSP$COM_AP,RP_2008_CSP$COM)


ag_1<-aggregate(agriculteur~COM_AP,RP_2008_CSP,sum)
ag_2<-aggregate(artisan_commercant_chef_entreprise~COM_AP,RP_2008_CSP,sum)
ag_3<-aggregate(cadre~COM_AP,RP_2008_CSP,sum)
ag_4<-aggregate(profession_intermediaire~COM_AP,RP_2008_CSP,sum)
ag_5<-aggregate(employe~COM_AP,RP_2008_CSP,sum)
ag_6<-aggregate(ouvrier~COM_AP,RP_2008_CSP,sum)
ag_7<-aggregate(population~COM_AP,RP_2008_CSP,sum)
ag_8<-aggregate(en_emploi~COM_AP,RP_2008_CSP,sum)
ag_9<-aggregate(au_chomage~COM_AP,RP_2008_CSP,sum)
#ag_10<-aggregate(part_chomage~COM_AP,RP_2008_CSP,sum)

RP_2008_CSP_final<-left_join(ag_1,ag_2)
RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_3)
RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_4)
RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_5)
RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_6)
RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_7)
RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_8)
RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_9)
#RP_2008_CSP_final<-left_join(RP_2008_CSP_final,ag_10)



write.csv(RP_2008_CSP_final, "recensement 1980-2022/rp travaillé/RP_2008_CSP_final")

#library(readr)
#RP_2008_CSP_final <- read_delim("recensement 1980-2022/rp travaillé/RP_2008_CSP_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




###########


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


RP_2013_CSP <- read_excel("recensement 1980-2022/RP_2013_CSP.xlsx")

names(RP_2013_CSP)[names(RP_2013_CSP)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_2013_CSP$COM <- paste(RP_2013_CSP$DEP_geo_courante, RP_2013_CSP$COM_geo_courante, sep = "")


names(RP_2013_CSP)[names(RP_2013_CSP)=="Agriculteurs\r\nActifs ayant un emploi\r\nRP2013"]<-"agriculteur_en_emploi"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Agriculteurs\r\nChômeurs\r\nRP2013"]<-"agriculteur_au_chomage"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nActifs ayant un emploi\r\nRP2013"]<-"artisan_commercant_chef_entreprise_en_emploi"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nChômeurs\r\nRP2013"]<-"artisan_commercant_chef_entreprise_au_chomage"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Cadres et professions intellectuelles supérieures\r\nActifs ayant un emploi\r\nRP2013"]<-"cadre_en_emploi"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Cadres et professions intellectuelles supérieures\r\nChômeurs\r\nRP2013"]<-"cadre_au_chomage"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Professions intermédiaires\r\nActifs ayant un emploi\r\nRP2013"]<-"profession_intermediaire_en_emploi"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Professions intermédiaires\r\nChômeurs\r\nRP2013"]<-"profession_intermediaire_au_chomage"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Employés\r\nActifs ayant un emploi\r\nRP2013"]<-"employe_en_emploi"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Employés\r\nChômeurs\r\nRP2013"]<-"employe_au_chomage"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Ouvriers\r\nActifs ayant un emploi\r\nRP2013"]<-"ouvrier_en_emploi"
names(RP_2013_CSP)[names(RP_2013_CSP)=="Ouvriers\r\nChômeurs\r\nRP2013"]<-"ouvrier_au_chomage"



RP_2013_CSP$agriculteur<-RP_2013_CSP$agriculteur_en_emploi+RP_2013_CSP$agriculteur_au_chomage

RP_2013_CSP$artisan_commercant_chef_entreprise<-RP_2013_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_2013_CSP$artisan_commercant_chef_entreprise_au_chomage

RP_2013_CSP$cadre<-RP_2013_CSP$cadre_en_emploi+RP_2013_CSP$cadre_au_chomage

RP_2013_CSP$profession_intermediaire<-RP_2013_CSP$profession_intermediaire_en_emploi+RP_2013_CSP$profession_intermediaire_au_chomage

RP_2013_CSP$employe<-RP_2013_CSP$employe_en_emploi+RP_2013_CSP$employe_au_chomage

RP_2013_CSP$ouvrier<-RP_2013_CSP$ouvrier_en_emploi+RP_2013_CSP$ouvrier_au_chomage


RP_2013_CSP$population<- rowSums(RP_2013_CSP[,20:25])


RP_2013_CSP$en_emploi<-RP_2013_CSP$agriculteur_en_emploi+RP_2013_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_2013_CSP$cadre_en_emploi+RP_2013_CSP$profession_intermediaire_en_emploi+RP_2013_CSP$employe_en_emploi+RP_2013_CSP$ouvrier_en_emploi


RP_2013_CSP$au_chomage<-RP_2013_CSP$agriculteur_au_chomage+RP_2013_CSP$artisan_commercant_chef_entreprise_au_chomage+RP_2013_CSP$cadre_au_chomage+RP_2013_CSP$profession_intermediaire_au_chomage+RP_2013_CSP$employe_au_chomage+RP_2013_CSP$ouvrier_au_chomage

#RP_2013_CSP$part_chomage<- RP_2013_CSP$au_chomage/RP_2013_CSP$population



RP_2013_CSP<-RP_2013_CSP[,19:28]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_2013_CSP<-left_join(RP_2013_CSP,table_passage_bis)

RP_2013_CSP$COM_AP<-ifelse(!is.na(RP_2013_CSP$COM_AP),RP_2013_CSP$COM_AP,RP_2013_CSP$COM)


ag_1<-aggregate(agriculteur~COM_AP,RP_2013_CSP,sum)
ag_2<-aggregate(artisan_commercant_chef_entreprise~COM_AP,RP_2013_CSP,sum)
ag_3<-aggregate(cadre~COM_AP,RP_2013_CSP,sum)
ag_4<-aggregate(profession_intermediaire~COM_AP,RP_2013_CSP,sum)
ag_5<-aggregate(employe~COM_AP,RP_2013_CSP,sum)
ag_6<-aggregate(ouvrier~COM_AP,RP_2013_CSP,sum)
ag_7<-aggregate(population~COM_AP,RP_2013_CSP,sum)
ag_8<-aggregate(en_emploi~COM_AP,RP_2013_CSP,sum)
ag_9<-aggregate(au_chomage~COM_AP,RP_2013_CSP,sum)
#ag_10<-aggregate(part_chomage~COM_AP,RP_2013_CSP,sum)

RP_2013_CSP_final<-left_join(ag_1,ag_2)
RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_3)
RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_4)
RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_5)
RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_6)
RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_7)
RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_8)
RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_9)
#RP_2013_CSP_final<-left_join(RP_2013_CSP_final,ag_10)



write.csv(RP_2013_CSP_final, "recensement 1980-2022/rp travaillé/RP_2013_CSP_final")

#library(readr)
#RP_2013_CSP_final <- read_delim("recensement 1980-2022/rp travaillé/RP_2013_CSP_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)





###########



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


RP_2019_CSP <- read_excel("recensement 1980-2022/RP_2019_CSP.xlsx")

names(RP_2019_CSP)[names(RP_2019_CSP)=="Département\r\nen géographie courante"]<-"DEP_geo_courante"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Commune\r\nen géographie courante"]<-"COM_geo_courante"


RP_2019_CSP$COM <- paste(RP_2019_CSP$DEP_geo_courante, RP_2019_CSP$COM_geo_courante, sep = "")


names(RP_2019_CSP)[names(RP_2019_CSP)=="Agriculteurs\r\nActifs ayant un emploi\r\nRP2019"]<-"agriculteur_en_emploi"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Agriculteurs\r\nChômeurs\r\nRP2019"]<-"agriculteur_au_chomage"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nActifs ayant un emploi\r\nRP2019"]<-"artisan_commercant_chef_entreprise_en_emploi"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Artisans, commerçants, chefs d'entreprise\r\nChômeurs\r\nRP2019"]<-"artisan_commercant_chef_entreprise_au_chomage"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Cadres et professions intellectuelles supérieures\r\nActifs ayant un emploi\r\nRP2019"]<-"cadre_en_emploi"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Cadres et professions intellectuelles supérieures\r\nChômeurs\r\nRP2019"]<-"cadre_au_chomage"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Professions intermédiaires\r\nActifs ayant un emploi\r\nRP2019"]<-"profession_intermediaire_en_emploi"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Professions intermédiaires\r\nChômeurs\r\nRP2019"]<-"profession_intermediaire_au_chomage"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Employés\r\nActifs ayant un emploi\r\nRP2019"]<-"employe_en_emploi"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Employés\r\nChômeurs\r\nRP2019"]<-"employe_au_chomage"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Ouvriers\r\nActifs ayant un emploi\r\nRP2019"]<-"ouvrier_en_emploi"
names(RP_2019_CSP)[names(RP_2019_CSP)=="Ouvriers\r\nChômeurs\r\nRP2019"]<-"ouvrier_au_chomage"



RP_2019_CSP$agriculteur<-RP_2019_CSP$agriculteur_en_emploi+RP_2019_CSP$agriculteur_au_chomage

RP_2019_CSP$artisan_commercant_chef_entreprise<-RP_2019_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_2019_CSP$artisan_commercant_chef_entreprise_au_chomage

RP_2019_CSP$cadre<-RP_2019_CSP$cadre_en_emploi+RP_2019_CSP$cadre_au_chomage

RP_2019_CSP$profession_intermediaire<-RP_2019_CSP$profession_intermediaire_en_emploi+RP_2019_CSP$profession_intermediaire_au_chomage

RP_2019_CSP$employe<-RP_2019_CSP$employe_en_emploi+RP_2019_CSP$employe_au_chomage

RP_2019_CSP$ouvrier<-RP_2019_CSP$ouvrier_en_emploi+RP_2019_CSP$ouvrier_au_chomage


RP_2019_CSP$population<- rowSums(RP_2019_CSP[,20:25])


RP_2019_CSP$en_emploi<-RP_2019_CSP$agriculteur_en_emploi+RP_2019_CSP$artisan_commercant_chef_entreprise_en_emploi+RP_2019_CSP$cadre_en_emploi+RP_2019_CSP$profession_intermediaire_en_emploi+RP_2019_CSP$employe_en_emploi+RP_2019_CSP$ouvrier_en_emploi


RP_2019_CSP$au_chomage<-RP_2019_CSP$agriculteur_au_chomage+RP_2019_CSP$artisan_commercant_chef_entreprise_au_chomage+RP_2019_CSP$cadre_au_chomage+RP_2019_CSP$profession_intermediaire_au_chomage+RP_2019_CSP$employe_au_chomage+RP_2019_CSP$ouvrier_au_chomage

#RP_2019_CSP$part_chomage<- RP_2019_CSP$au_chomage/RP_2019_CSP$population



RP_2019_CSP<-RP_2019_CSP[,19:28]

#adapter au code commune



table_passage_1970_2022 <- read_csv("table passage 1970_2022/table_passage_1970_2022")


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]

names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"COM"
RP_2019_CSP<-left_join(RP_2019_CSP,table_passage_bis)

RP_2019_CSP$COM_AP<-ifelse(!is.na(RP_2019_CSP$COM_AP),RP_2019_CSP$COM_AP,RP_2019_CSP$COM)


ag_1<-aggregate(agriculteur~COM_AP,RP_2019_CSP,sum)
ag_2<-aggregate(artisan_commercant_chef_entreprise~COM_AP,RP_2019_CSP,sum)
ag_3<-aggregate(cadre~COM_AP,RP_2019_CSP,sum)
ag_4<-aggregate(profession_intermediaire~COM_AP,RP_2019_CSP,sum)
ag_5<-aggregate(employe~COM_AP,RP_2019_CSP,sum)
ag_6<-aggregate(ouvrier~COM_AP,RP_2019_CSP,sum)
ag_7<-aggregate(population~COM_AP,RP_2019_CSP,sum)
ag_8<-aggregate(en_emploi~COM_AP,RP_2019_CSP,sum)
ag_9<-aggregate(au_chomage~COM_AP,RP_2019_CSP,sum)
#ag_10<-aggregate(part_chomage~COM_AP,RP_2019_CSP,sum)

RP_2019_CSP_final<-left_join(ag_1,ag_2)
RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_3)
RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_4)
RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_5)
RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_6)
RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_7)
RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_8)
RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_9)
#RP_2019_CSP_final<-left_join(RP_2019_CSP_final,ag_10)



write.csv(RP_2019_CSP_final, "recensement 1980-2022/rp travaillé/RP_2019_CSP_final")

#library(readr)
#RP_2019_CSP_final <- read_delim("recensement 1980-2022/rp travaillé/RP_2019_CSP_final", 
#                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)




###################



#linear interpolation between years





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

RP_1975_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1975_CSP_final")

RP_1982_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1982_CSP_final")

RP_1990_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1990_CSP_final")

RP_1999_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1999_CSP_final")

RP_2008_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2008_CSP_final")

RP_2013_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2013_CSP_final")

RP_2019_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2019_CSP_final")





df_merged <- merge(RP_1975_CSP_final, RP_1982_CSP_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1975 divisée par le nombre d'années pour chaque variable
df_merged$difference_agriculteur <- (df_merged$agriculteur.y - df_merged$agriculteur.x) / 7
df_merged$difference_artisan_commercant_chef_entreprise <- (df_merged$artisan_commercant_chef_entreprise.y - df_merged$artisan_commercant_chef_entreprise.x) / 7
df_merged$difference_cadre <- (df_merged$cadre.y - df_merged$cadre.x) / 7
df_merged$difference_profession_intermediaire <- (df_merged$profession_intermediaire.y - df_merged$profession_intermediaire.x) / 7
df_merged$difference_employe <- (df_merged$employe.y - df_merged$employe.x) / 7
df_merged$difference_ouvrier <- (df_merged$ouvrier.y - df_merged$ouvrier.x) / 7
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 7
df_merged$difference_en_emploi <- (df_merged$en_emploi.y - df_merged$en_emploi.x) / 7
df_merged$difference_au_chomage <- (df_merged$au_chomage.y - df_merged$au_chomage.x) / 7

# Créer une liste avec les années entre 1975 et 1980
years <- 1975:1982

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1975
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_agriculteur<- df_temp$agriculteur.x + (x - 1975) * df_temp$difference_agriculteur
  df_temp$value_estimated_artisan_commercant_chef_entreprise <- df_temp$artisan_commercant_chef_entreprise.x + (x - 1975) * df_temp$difference_artisan_commercant_chef_entreprise
  df_temp$value_estimated_cadre <- df_temp$cadre.x + (x - 1975) * df_temp$difference_cadre
  df_temp$value_estimated_profession_intermediaire <- df_temp$profession_intermediaire.x + (x - 1975) * df_temp$difference_profession_intermediaire
  df_temp$value_estimated_employe <- df_temp$employe.x + (x - 1975) * df_temp$difference_employe
  df_temp$value_estimated_ouvrier <- df_temp$ouvrier.x + (x - 1975) * df_temp$difference_ouvrier
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1975) * df_temp$difference_population
  df_temp$value_estimated_en_emploi <- df_temp$en_emploi.x + (x - 1975) * df_temp$difference_en_emploi
  df_temp$value_estimated_au_chomage <- df_temp$au_chomage.x + (x - 1975) * df_temp$difference_au_chomage
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

RP_1976_CSP_final_2<-as.data.frame(list_df$df_1976)
RP_1976_CSP_final_2<-RP_1976_CSP_final_2[,c(1,31:40)]

RP_1977_CSP_final_2<-as.data.frame(list_df$df_1977)
RP_1977_CSP_final_2<-RP_1977_CSP_final_2[,c(1,31:40)]

RP_1978_CSP_final_2<-as.data.frame(list_df$df_1978)
RP_1978_CSP_final_2<-RP_1978_CSP_final_2[,c(1,31:40)]

RP_1979_CSP_final_2<-as.data.frame(list_df$df_1979)
RP_1979_CSP_final_2<-RP_1979_CSP_final_2[,c(1,31:40)]

RP_1980_CSP_final_2<-as.data.frame(list_df$df_1980)
RP_1980_CSP_final_2<-RP_1980_CSP_final_2[,c(1,31:40)]

RP_1981_CSP_final_2<-as.data.frame(list_df$df_1981)
RP_1981_CSP_final_2<-RP_1981_CSP_final_2[,c(1,31:40)]

RP_1982_CSP_final_2<-as.data.frame(list_df$df_1982)
RP_1982_CSP_final_2<-RP_1982_CSP_final_2[,c(1,31:40)]




write.csv(RP_1976_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1976_CSP_final_2")
write.csv(RP_1977_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1977_CSP_final_2")
write.csv(RP_1978_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1978_CSP_final_2")
write.csv(RP_1979_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1979_CSP_final_2")
write.csv(RP_1980_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1980_CSP_final_2")
write.csv(RP_1981_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1981_CSP_final_2")
write.csv(RP_1982_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1982_CSP_final_2")

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

RP_1975_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1975_CSP_final")

RP_1982_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1982_CSP_final")

RP_1990_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1990_CSP_final")

RP_1999_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1999_CSP_final")

RP_2008_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2008_CSP_final")

RP_2013_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2013_CSP_final")

RP_2019_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2019_CSP_final")





df_merged <- merge(RP_1982_CSP_final, RP_1990_CSP_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1982 divisée par le nombre d'années pour chaque variable
df_merged$difference_agriculteur <- (df_merged$agriculteur.y - df_merged$agriculteur.x) / 8
df_merged$difference_artisan_commercant_chef_entreprise <- (df_merged$artisan_commercant_chef_entreprise.y - df_merged$artisan_commercant_chef_entreprise.x) / 8
df_merged$difference_cadre <- (df_merged$cadre.y - df_merged$cadre.x) / 8
df_merged$difference_profession_intermediaire <- (df_merged$profession_intermediaire.y - df_merged$profession_intermediaire.x) / 8
df_merged$difference_employe <- (df_merged$employe.y - df_merged$employe.x) / 8
df_merged$difference_ouvrier <- (df_merged$ouvrier.y - df_merged$ouvrier.x) / 8
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 8
df_merged$difference_en_emploi <- (df_merged$en_emploi.y - df_merged$en_emploi.x) / 8
df_merged$difference_au_chomage <- (df_merged$au_chomage.y - df_merged$au_chomage.x) / 8

# Créer une liste avec les années entre 1982 et 1980
years <- 1982:1990

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1982
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_agriculteur<- df_temp$agriculteur.x + (x - 1982) * df_temp$difference_agriculteur
  df_temp$value_estimated_artisan_commercant_chef_entreprise <- df_temp$artisan_commercant_chef_entreprise.x + (x - 1982) * df_temp$difference_artisan_commercant_chef_entreprise
  df_temp$value_estimated_cadre <- df_temp$cadre.x + (x - 1982) * df_temp$difference_cadre
  df_temp$value_estimated_profession_intermediaire <- df_temp$profession_intermediaire.x + (x - 1982) * df_temp$difference_profession_intermediaire
  df_temp$value_estimated_employe <- df_temp$employe.x + (x - 1982) * df_temp$difference_employe
  df_temp$value_estimated_ouvrier <- df_temp$ouvrier.x + (x - 1982) * df_temp$difference_ouvrier
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1982) * df_temp$difference_population
  df_temp$value_estimated_en_emploi <- df_temp$en_emploi.x + (x - 1982) * df_temp$difference_en_emploi
  df_temp$value_estimated_au_chomage <- df_temp$au_chomage.x + (x - 1982) * df_temp$difference_au_chomage
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

RP_1983_CSP_final_2<-as.data.frame(list_df$df_1983)
RP_1983_CSP_final_2<-RP_1983_CSP_final_2[,c(1,31:40)]

RP_1984_CSP_final_2<-as.data.frame(list_df$df_1984)
RP_1984_CSP_final_2<-RP_1984_CSP_final_2[,c(1,31:40)]

RP_1985_CSP_final_2<-as.data.frame(list_df$df_1985)
RP_1985_CSP_final_2<-RP_1985_CSP_final_2[,c(1,31:40)]

RP_1986_CSP_final_2<-as.data.frame(list_df$df_1986)
RP_1986_CSP_final_2<-RP_1986_CSP_final_2[,c(1,31:40)]

RP_1987_CSP_final_2<-as.data.frame(list_df$df_1987)
RP_1987_CSP_final_2<-RP_1987_CSP_final_2[,c(1,31:40)]

RP_1988_CSP_final_2<-as.data.frame(list_df$df_1988)
RP_1988_CSP_final_2<-RP_1988_CSP_final_2[,c(1,31:40)]

RP_1989_CSP_final_2<-as.data.frame(list_df$df_1989)
RP_1989_CSP_final_2<-RP_1989_CSP_final_2[,c(1,31:40)]


RP_1990_CSP_final_2<-as.data.frame(list_df$df_1990)
RP_1990_CSP_final_2<-RP_1990_CSP_final_2[,c(1,31:40)]




write.csv(RP_1983_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1983_CSP_final_2")


write.csv(RP_1984_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1984_CSP_final_2")
write.csv(RP_1985_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1985_CSP_final_2")
write.csv(RP_1986_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1986_CSP_final_2")
write.csv(RP_1987_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1987_CSP_final_2")
write.csv(RP_1988_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1988_CSP_final_2")
write.csv(RP_1989_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1989_CSP_final_2")
write.csv(RP_1990_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1990_CSP_final_2")





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

RP_1975_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1975_CSP_final")

RP_1982_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1982_CSP_final")

RP_1990_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1990_CSP_final")

RP_1999_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1999_CSP_final")

RP_2008_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2008_CSP_final")

RP_2013_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2013_CSP_final")

RP_2019_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2019_CSP_final")





df_merged <- merge(RP_1990_CSP_final, RP_1999_CSP_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1990 divisée par le nombre d'années pour chaque variable
df_merged$difference_agriculteur <- (df_merged$agriculteur.y - df_merged$agriculteur.x) / 9
df_merged$difference_artisan_commercant_chef_entreprise <- (df_merged$artisan_commercant_chef_entreprise.y - df_merged$artisan_commercant_chef_entreprise.x) / 9
df_merged$difference_cadre <- (df_merged$cadre.y - df_merged$cadre.x) / 9
df_merged$difference_profession_intermediaire <- (df_merged$profession_intermediaire.y - df_merged$profession_intermediaire.x) / 9
df_merged$difference_employe <- (df_merged$employe.y - df_merged$employe.x) / 9
df_merged$difference_ouvrier <- (df_merged$ouvrier.y - df_merged$ouvrier.x) / 9
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 9
df_merged$difference_en_emploi <- (df_merged$en_emploi.y - df_merged$en_emploi.x) / 9
df_merged$difference_au_chomage <- (df_merged$au_chomage.y - df_merged$au_chomage.x) / 9

# Créer une liste avec les années entre 1990 et 1980
years <- 1990:1999

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1990
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_agriculteur<- df_temp$agriculteur.x + (x - 1990) * df_temp$difference_agriculteur
  df_temp$value_estimated_artisan_commercant_chef_entreprise <- df_temp$artisan_commercant_chef_entreprise.x + (x - 1990) * df_temp$difference_artisan_commercant_chef_entreprise
  df_temp$value_estimated_cadre <- df_temp$cadre.x + (x - 1990) * df_temp$difference_cadre
  df_temp$value_estimated_profession_intermediaire <- df_temp$profession_intermediaire.x + (x - 1990) * df_temp$difference_profession_intermediaire
  df_temp$value_estimated_employe <- df_temp$employe.x + (x - 1990) * df_temp$difference_employe
  df_temp$value_estimated_ouvrier <- df_temp$ouvrier.x + (x - 1990) * df_temp$difference_ouvrier
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1990) * df_temp$difference_population
  df_temp$value_estimated_en_emploi <- df_temp$en_emploi.x + (x - 1990) * df_temp$difference_en_emploi
  df_temp$value_estimated_au_chomage <- df_temp$au_chomage.x + (x - 1990) * df_temp$difference_au_chomage
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

RP_1991_CSP_final_2<-as.data.frame(list_df$df_1991)
RP_1991_CSP_final_2<-RP_1991_CSP_final_2[,c(1,31:40)]

RP_1992_CSP_final_2<-as.data.frame(list_df$df_1992)
RP_1992_CSP_final_2<-RP_1992_CSP_final_2[,c(1,31:40)]

RP_1993_CSP_final_2<-as.data.frame(list_df$df_1993)
RP_1993_CSP_final_2<-RP_1993_CSP_final_2[,c(1,31:40)]

RP_1994_CSP_final_2<-as.data.frame(list_df$df_1994)
RP_1994_CSP_final_2<-RP_1994_CSP_final_2[,c(1,31:40)]

RP_1995_CSP_final_2<-as.data.frame(list_df$df_1995)
RP_1995_CSP_final_2<-RP_1995_CSP_final_2[,c(1,31:40)]

RP_1996_CSP_final_2<-as.data.frame(list_df$df_1996)
RP_1996_CSP_final_2<-RP_1996_CSP_final_2[,c(1,31:40)]

RP_1997_CSP_final_2<-as.data.frame(list_df$df_1997)
RP_1997_CSP_final_2<-RP_1997_CSP_final_2[,c(1,31:40)]

RP_1998_CSP_final_2<-as.data.frame(list_df$df_1998)
RP_1998_CSP_final_2<-RP_1998_CSP_final_2[,c(1,31:40)]



RP_1999_CSP_final_2<-as.data.frame(list_df$df_1999)
RP_1999_CSP_final_2<-RP_1999_CSP_final_2[,c(1,31:40)]




write.csv(RP_1991_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1991_CSP_final_2")

write.csv(RP_1992_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1992_CSP_final_2")
write.csv(RP_1993_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1993_CSP_final_2")
write.csv(RP_1994_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1994_CSP_final_2")
write.csv(RP_1995_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1995_CSP_final_2")
write.csv(RP_1996_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1996_CSP_final_2")
write.csv(RP_1997_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1997_CSP_final_2")
write.csv(RP_1998_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1998_CSP_final_2")
write.csv(RP_1999_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_1999_CSP_final_2")








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

RP_1975_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1975_CSP_final")

RP_1982_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1982_CSP_final")

RP_1990_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1990_CSP_final")

RP_1999_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1999_CSP_final")

RP_2008_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2008_CSP_final")

RP_2013_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2013_CSP_final")

RP_2019_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2019_CSP_final")





df_merged <- merge(RP_1999_CSP_final, RP_2008_CSP_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 1999 divisée par le nombre d'années pour chaque variable
df_merged$difference_agriculteur <- (df_merged$agriculteur.y - df_merged$agriculteur.x) / 9
df_merged$difference_artisan_commercant_chef_entreprise <- (df_merged$artisan_commercant_chef_entreprise.y - df_merged$artisan_commercant_chef_entreprise.x) / 9
df_merged$difference_cadre <- (df_merged$cadre.y - df_merged$cadre.x) / 9
df_merged$difference_profession_intermediaire <- (df_merged$profession_intermediaire.y - df_merged$profession_intermediaire.x) / 9
df_merged$difference_employe <- (df_merged$employe.y - df_merged$employe.x) / 9
df_merged$difference_ouvrier <- (df_merged$ouvrier.y - df_merged$ouvrier.x) / 9
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 9
df_merged$difference_en_emploi <- (df_merged$en_emploi.y - df_merged$en_emploi.x) / 9
df_merged$difference_au_chomage <- (df_merged$au_chomage.y - df_merged$au_chomage.x) / 9

# Créer une liste avec les années entre 1999 et 1980
years <- 1999:2008

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 1999
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_agriculteur<- df_temp$agriculteur.x + (x - 1999) * df_temp$difference_agriculteur
  df_temp$value_estimated_artisan_commercant_chef_entreprise <- df_temp$artisan_commercant_chef_entreprise.x + (x - 1999) * df_temp$difference_artisan_commercant_chef_entreprise
  df_temp$value_estimated_cadre <- df_temp$cadre.x + (x - 1999) * df_temp$difference_cadre
  df_temp$value_estimated_profession_intermediaire <- df_temp$profession_intermediaire.x + (x - 1999) * df_temp$difference_profession_intermediaire
  df_temp$value_estimated_employe <- df_temp$employe.x + (x - 1999) * df_temp$difference_employe
  df_temp$value_estimated_ouvrier <- df_temp$ouvrier.x + (x - 1999) * df_temp$difference_ouvrier
  df_temp$value_estimated_population <- df_temp$population.x + (x - 1999) * df_temp$difference_population
  df_temp$value_estimated_en_emploi <- df_temp$en_emploi.x + (x - 1999) * df_temp$difference_en_emploi
  df_temp$value_estimated_au_chomage <- df_temp$au_chomage.x + (x - 1999) * df_temp$difference_au_chomage
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

RP_2000_CSP_final_2<-as.data.frame(list_df$df_2000)
RP_2000_CSP_final_2<-RP_2000_CSP_final_2[,c(1,31:40)]

RP_2001_CSP_final_2<-as.data.frame(list_df$df_2001)
RP_2001_CSP_final_2<-RP_2001_CSP_final_2[,c(1,31:40)]

RP_2002_CSP_final_2<-as.data.frame(list_df$df_2002)
RP_2002_CSP_final_2<-RP_2002_CSP_final_2[,c(1,31:40)]

RP_2003_CSP_final_2<-as.data.frame(list_df$df_2003)
RP_2003_CSP_final_2<-RP_2003_CSP_final_2[,c(1,31:40)]

RP_2004_CSP_final_2<-as.data.frame(list_df$df_2004)
RP_2004_CSP_final_2<-RP_2004_CSP_final_2[,c(1,31:40)]

RP_2005_CSP_final_2<-as.data.frame(list_df$df_2005)
RP_2005_CSP_final_2<-RP_2005_CSP_final_2[,c(1,31:40)]

RP_2006_CSP_final_2<-as.data.frame(list_df$df_2006)
RP_2006_CSP_final_2<-RP_2006_CSP_final_2[,c(1,31:40)]

RP_2007_CSP_final_2<-as.data.frame(list_df$df_2007)
RP_2007_CSP_final_2<-RP_2007_CSP_final_2[,c(1,31:40)]


RP_2008_CSP_final_2<-as.data.frame(list_df$df_2008)
RP_2008_CSP_final_2<-RP_2008_CSP_final_2[,c(1,31:40)]




write.csv(RP_2000_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2000_CSP_final_2")


write.csv(RP_2001_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2001_CSP_final_2")
write.csv(RP_2002_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2002_CSP_final_2")
write.csv(RP_2003_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2003_CSP_final_2")
write.csv(RP_2004_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2004_CSP_final_2")
write.csv(RP_2005_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2005_CSP_final_2")
write.csv(RP_2006_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2006_CSP_final_2")
write.csv(RP_2007_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2007_CSP_final_2")
write.csv(RP_2008_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2008_CSP_final_2")











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

RP_1975_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1975_CSP_final")

RP_1982_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1982_CSP_final")

RP_1990_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1990_CSP_final")

RP_1999_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1999_CSP_final")

RP_2008_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2008_CSP_final")

RP_2013_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2013_CSP_final")

RP_2019_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2019_CSP_final")





df_merged <- merge(RP_2008_CSP_final, RP_2013_CSP_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 2008 divisée par le nombre d'années pour chaque variable
df_merged$difference_agriculteur <- (df_merged$agriculteur.y - df_merged$agriculteur.x) / 5
df_merged$difference_artisan_commercant_chef_entreprise <- (df_merged$artisan_commercant_chef_entreprise.y - df_merged$artisan_commercant_chef_entreprise.x) / 5
df_merged$difference_cadre <- (df_merged$cadre.y - df_merged$cadre.x) / 5
df_merged$difference_profession_intermediaire <- (df_merged$profession_intermediaire.y - df_merged$profession_intermediaire.x) / 5
df_merged$difference_employe <- (df_merged$employe.y - df_merged$employe.x) / 5
df_merged$difference_ouvrier <- (df_merged$ouvrier.y - df_merged$ouvrier.x) / 5
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 5
df_merged$difference_en_emploi <- (df_merged$en_emploi.y - df_merged$en_emploi.x) / 5
df_merged$difference_au_chomage <- (df_merged$au_chomage.y - df_merged$au_chomage.x) / 5

# Créer une liste avec les années entre 2008 et 1980
years <- 2008:2013

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 2008
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_agriculteur<- df_temp$agriculteur.x + (x - 2008) * df_temp$difference_agriculteur
  df_temp$value_estimated_artisan_commercant_chef_entreprise <- df_temp$artisan_commercant_chef_entreprise.x + (x - 2008) * df_temp$difference_artisan_commercant_chef_entreprise
  df_temp$value_estimated_cadre <- df_temp$cadre.x + (x - 2008) * df_temp$difference_cadre
  df_temp$value_estimated_profession_intermediaire <- df_temp$profession_intermediaire.x + (x - 2008) * df_temp$difference_profession_intermediaire
  df_temp$value_estimated_employe <- df_temp$employe.x + (x - 2008) * df_temp$difference_employe
  df_temp$value_estimated_ouvrier <- df_temp$ouvrier.x + (x - 2008) * df_temp$difference_ouvrier
  df_temp$value_estimated_population <- df_temp$population.x + (x - 2008) * df_temp$difference_population
  df_temp$value_estimated_en_emploi <- df_temp$en_emploi.x + (x - 2008) * df_temp$difference_en_emploi
  df_temp$value_estimated_au_chomage <- df_temp$au_chomage.x + (x - 2008) * df_temp$difference_au_chomage
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

RP_2009_CSP_final_2<-as.data.frame(list_df$df_2009)
RP_2009_CSP_final_2<-RP_2009_CSP_final_2[,c(1,31:40)]

RP_2010_CSP_final_2<-as.data.frame(list_df$df_2010)
RP_2010_CSP_final_2<-RP_2010_CSP_final_2[,c(1,31:40)]

RP_2011_CSP_final_2<-as.data.frame(list_df$df_2011)
RP_2011_CSP_final_2<-RP_2011_CSP_final_2[,c(1,31:40)]

RP_2012_CSP_final_2<-as.data.frame(list_df$df_2012)
RP_2012_CSP_final_2<-RP_2012_CSP_final_2[,c(1,31:40)]


RP_2013_CSP_final_2<-as.data.frame(list_df$df_2013)
RP_2013_CSP_final_2<-RP_2013_CSP_final_2[,c(1,31:40)]




write.csv(RP_2009_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2009_CSP_final_2")

write.csv(RP_2010_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2010_CSP_final_2")

write.csv(RP_2011_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2011_CSP_final_2")

write.csv(RP_2012_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2012_CSP_final_2")

write.csv(RP_2013_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2013_CSP_final_2")











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

RP_1975_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1975_CSP_final")

RP_1982_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1982_CSP_final")

RP_1990_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1990_CSP_final")

RP_1999_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_1999_CSP_final")

RP_2008_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2008_CSP_final")

RP_2013_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2013_CSP_final")

RP_2019_CSP_final <- read_csv("recensement 1980-2022/rp travaillé/RP_2019_CSP_final")





df_merged <- merge(RP_2013_CSP_final, RP_2019_CSP_final, by = "COM_AP")

# Calculer la différence entre les valeurs de 1980 et 2013 divisée par le nombre d'années pour chaque variable
df_merged$difference_agriculteur <- (df_merged$agriculteur.y - df_merged$agriculteur.x) / 6
df_merged$difference_artisan_commercant_chef_entreprise <- (df_merged$artisan_commercant_chef_entreprise.y - df_merged$artisan_commercant_chef_entreprise.x) / 6
df_merged$difference_cadre <- (df_merged$cadre.y - df_merged$cadre.x) / 6
df_merged$difference_profession_intermediaire <- (df_merged$profession_intermediaire.y - df_merged$profession_intermediaire.x) / 6
df_merged$difference_employe <- (df_merged$employe.y - df_merged$employe.x) / 6
df_merged$difference_ouvrier <- (df_merged$ouvrier.y - df_merged$ouvrier.x) / 6
df_merged$difference_population <- (df_merged$population.y - df_merged$population.x) / 6
df_merged$difference_en_emploi <- (df_merged$en_emploi.y - df_merged$en_emploi.x) / 6
df_merged$difference_au_chomage <- (df_merged$au_chomage.y - df_merged$au_chomage.x) / 6

# Créer une liste avec les années entre 2013 et 1980
years <- 2013:2019

# Pour chaque année, estimer la valeur de chaque variable en utilisant la différence calculée
# et le nombre d'années écoulées depuis 2013
list_df <- lapply(years, function(x) {
  df_temp <- df_merged
  df_temp$value_estimated_agriculteur<- df_temp$agriculteur.x + (x - 2013) * df_temp$difference_agriculteur
  df_temp$value_estimated_artisan_commercant_chef_entreprise <- df_temp$artisan_commercant_chef_entreprise.x + (x - 2013) * df_temp$difference_artisan_commercant_chef_entreprise
  df_temp$value_estimated_cadre <- df_temp$cadre.x + (x - 2013) * df_temp$difference_cadre
  df_temp$value_estimated_profession_intermediaire <- df_temp$profession_intermediaire.x + (x - 2013) * df_temp$difference_profession_intermediaire
  df_temp$value_estimated_employe <- df_temp$employe.x + (x - 2013) * df_temp$difference_employe
  df_temp$value_estimated_ouvrier <- df_temp$ouvrier.x + (x - 2013) * df_temp$difference_ouvrier
  df_temp$value_estimated_population <- df_temp$population.x + (x - 2013) * df_temp$difference_population
  df_temp$value_estimated_en_emploi <- df_temp$en_emploi.x + (x - 2013) * df_temp$difference_en_emploi
  df_temp$value_estimated_au_chomage <- df_temp$au_chomage.x + (x - 2013) * df_temp$difference_au_chomage
  df_temp$year <- x
  df_temp
})

# Stocker les résultats dans une liste nommée
names(list_df) <- paste("df_", years, sep = "")

RP_2014_CSP_final_2<-as.data.frame(list_df$df_2014)
RP_2014_CSP_final_2<-RP_2014_CSP_final_2[,c(1,31:40)]

RP_2015_CSP_final_2<-as.data.frame(list_df$df_2015)
RP_2015_CSP_final_2<-RP_2015_CSP_final_2[,c(1,31:40)]

RP_2016_CSP_final_2<-as.data.frame(list_df$df_2016)
RP_2016_CSP_final_2<-RP_2016_CSP_final_2[,c(1,31:40)]

RP_2017_CSP_final_2<-as.data.frame(list_df$df_2017)
RP_2017_CSP_final_2<-RP_2017_CSP_final_2[,c(1,31:40)]

RP_2018_CSP_final_2<-as.data.frame(list_df$df_2018)
RP_2018_CSP_final_2<-RP_2018_CSP_final_2[,c(1,31:40)]



RP_2019_CSP_final_2<-as.data.frame(list_df$df_2019)
RP_2019_CSP_final_2<-RP_2019_CSP_final_2[,c(1,31:40)]




write.csv(RP_2014_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2014_CSP_final_2")

write.csv(RP_2015_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2015_CSP_final_2")
write.csv(RP_2016_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2016_CSP_final_2")
write.csv(RP_2017_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2017_CSP_final_2")
write.csv(RP_2018_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2018_CSP_final_2")
write.csv(RP_2019_CSP_final_2, "recensement 1980-2022/rp travaillé/RP_2019_CSP_final_2")












