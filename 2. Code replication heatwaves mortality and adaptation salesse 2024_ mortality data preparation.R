
#this code is used to work with mortality data








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






table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1980 <- read_delim("/fichier deces insee/deces-1980.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1980<-left_join(deces.1980, table_passage_bis)


deces.1980$COM<-ifelse(!is.na(deces.1980$COM_AP),deces.1980$COM_AP,deces.1980$lieudeces)

deces.1980$datedeces <- as.character(deces.1980$datedeces)
deces.1980$datedeces <- as.Date(deces.1980$datedeces, format = "%Y%m%d")


deces.1980$datenaiss <- as.character(deces.1980$datenaiss)
deces.1980$datenaiss <- as.Date(deces.1980$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1980$age <- as.period(interval(deces.1980$datenaiss , deces.1980$datedeces ))
deces.1980$age_years <- year(deces.1980$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1980$sexe<-as.character(deces.1980$sexe)

deces.1980$SEX<-ifelse(deces.1980$sexe=="1","Homme","Femme")

deces.1980_final<-deces.1980[,c("COM","datedeces","age_years","SEX")]

deces.1980_final$nbr_mort<-1


deces.1980_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1980_final,sum)

library(tidyr)

deces.1980_spread <- deces.1980_final_ag %>%
  spread(SEX, nbr_mort)

deces.1980_spread$Femme[is.na(deces.1980_spread$Femme)]<-0
deces.1980_spread$Homme[is.na(deces.1980_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme



deces.1980_final$tranche_age <- cut(deces.1980_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))


deces.1980_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1980_final,sum)

library(tidyr)

deces.1980_spread2 <- deces.1980_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1980_spread2$`0-9`[is.na(deces.1980_spread2$`0-9`)]<-0
deces.1980_spread2$`10-19`[is.na(deces.1980_spread2$`10-19`)]<-0
deces.1980_spread2$`20-39`[is.na(deces.1980_spread2$`20-39`)]<-0
deces.1980_spread2$`40-59`[is.na(deces.1980_spread2$`40-59`)]<-0
deces.1980_spread2$`60-64`[is.na(deces.1980_spread2$`60-64`)]<-0
deces.1980_spread2$`65-69`[is.na(deces.1980_spread2$`65-69`)]<-0
deces.1980_spread2$`70-74`[is.na(deces.1980_spread2$`70-74`)]<-0
deces.1980_spread2$`75-79`[is.na(deces.1980_spread2$`75-79`)]<-0
deces.1980_spread2$`80+`[is.na(deces.1980_spread2$`80+`)]<-0


deces.1980_spread<-left_join(deces.1980_spread, deces.1980_spread2)


names(deces.1980_spread)[names(deces.1980_spread)=="datedeces"]<-"date"



fwrite(deces.1980_spread,"/fichier deces insee/décès travaillé/deces.1980_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1981 <- read_delim("/fichier deces insee/deces-1981.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1981<-left_join(deces.1981, table_passage_bis)


deces.1981$COM<-ifelse(!is.na(deces.1981$COM_AP),deces.1981$COM_AP,deces.1981$lieudeces)

deces.1981$datedeces <- as.character(deces.1981$datedeces)
deces.1981$datedeces <- as.Date(deces.1981$datedeces, format = "%Y%m%d")


deces.1981$datenaiss <- as.character(deces.1981$datenaiss)
deces.1981$datenaiss <- as.Date(deces.1981$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1981$age <- as.period(interval(deces.1981$datenaiss , deces.1981$datedeces ))
deces.1981$age_years <- year(deces.1981$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1981$sexe<-as.character(deces.1981$sexe)

deces.1981$SEX<-ifelse(deces.1981$sexe=="1","Homme","Femme")

deces.1981_final<-deces.1981[,c("COM","datedeces","age_years","SEX")]

deces.1981_final$nbr_mort<-1


deces.1981_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1981_final,sum)

library(tidyr)

deces.1981_spread <- deces.1981_final_ag %>%
  spread(SEX, nbr_mort)

deces.1981_spread$Femme[is.na(deces.1981_spread$Femme)]<-0
deces.1981_spread$Homme[is.na(deces.1981_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1981_final$tranche_age <- cut(deces.1981_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1981_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1981_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1981_final,sum)

library(tidyr)

deces.1981_spread2 <- deces.1981_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1981_spread2$`0-9`[is.na(deces.1981_spread2$`0-9`)]<-0
deces.1981_spread2$`10-19`[is.na(deces.1981_spread2$`10-19`)]<-0
deces.1981_spread2$`20-39`[is.na(deces.1981_spread2$`20-39`)]<-0
deces.1981_spread2$`40-59`[is.na(deces.1981_spread2$`40-59`)]<-0
deces.1981_spread2$`60-64`[is.na(deces.1981_spread2$`60-64`)]<-0
deces.1981_spread2$`65-69`[is.na(deces.1981_spread2$`65-69`)]<-0
deces.1981_spread2$`70-74`[is.na(deces.1981_spread2$`70-74`)]<-0
deces.1981_spread2$`75-79`[is.na(deces.1981_spread2$`75-79`)]<-0
deces.1981_spread2$`80+`[is.na(deces.1981_spread2$`80+`)]<-0


deces.1981_spread<-left_join(deces.1981_spread, deces.1981_spread2)


names(deces.1981_spread)[names(deces.1981_spread)=="datedeces"]<-"date"



fwrite(deces.1981_spread,"/fichier deces insee/décès travaillé/deces.1981_age_sexe.csv")






##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1982 <- read_delim("/fichier deces insee/deces-1982.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1982<-left_join(deces.1982, table_passage_bis)


deces.1982$COM<-ifelse(!is.na(deces.1982$COM_AP),deces.1982$COM_AP,deces.1982$lieudeces)

deces.1982$datedeces <- as.character(deces.1982$datedeces)
deces.1982$datedeces <- as.Date(deces.1982$datedeces, format = "%Y%m%d")


deces.1982$datenaiss <- as.character(deces.1982$datenaiss)
deces.1982$datenaiss <- as.Date(deces.1982$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1982$age <- as.period(interval(deces.1982$datenaiss , deces.1982$datedeces ))
deces.1982$age_years <- year(deces.1982$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1982$sexe<-as.character(deces.1982$sexe)

deces.1982$SEX<-ifelse(deces.1982$sexe=="1","Homme","Femme")

deces.1982_final<-deces.1982[,c("COM","datedeces","age_years","SEX")]

deces.1982_final$nbr_mort<-1


deces.1982_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1982_final,sum)

library(tidyr)

deces.1982_spread <- deces.1982_final_ag %>%
  spread(SEX, nbr_mort)

deces.1982_spread$Femme[is.na(deces.1982_spread$Femme)]<-0
deces.1982_spread$Homme[is.na(deces.1982_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1982_final$tranche_age <- cut(deces.1982_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1982_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1982_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1982_final,sum)

library(tidyr)

deces.1982_spread2 <- deces.1982_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1982_spread2$`0-9`[is.na(deces.1982_spread2$`0-9`)]<-0
deces.1982_spread2$`10-19`[is.na(deces.1982_spread2$`10-19`)]<-0
deces.1982_spread2$`20-39`[is.na(deces.1982_spread2$`20-39`)]<-0
deces.1982_spread2$`40-59`[is.na(deces.1982_spread2$`40-59`)]<-0
deces.1982_spread2$`60-64`[is.na(deces.1982_spread2$`60-64`)]<-0
deces.1982_spread2$`65-69`[is.na(deces.1982_spread2$`65-69`)]<-0
deces.1982_spread2$`70-74`[is.na(deces.1982_spread2$`70-74`)]<-0
deces.1982_spread2$`75-79`[is.na(deces.1982_spread2$`75-79`)]<-0
deces.1982_spread2$`80+`[is.na(deces.1982_spread2$`80+`)]<-0


deces.1982_spread<-left_join(deces.1982_spread, deces.1982_spread2)


names(deces.1982_spread)[names(deces.1982_spread)=="datedeces"]<-"date"



fwrite(deces.1982_spread,"/fichier deces insee/décès travaillé/deces.1982_age_sexe.csv")





##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1983 <- read_delim("/fichier deces insee/deces-1983.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1983<-left_join(deces.1983, table_passage_bis)


deces.1983$COM<-ifelse(!is.na(deces.1983$COM_AP),deces.1983$COM_AP,deces.1983$lieudeces)

deces.1983$datedeces <- as.character(deces.1983$datedeces)
deces.1983$datedeces <- as.Date(deces.1983$datedeces, format = "%Y%m%d")


deces.1983$datenaiss <- as.character(deces.1983$datenaiss)
deces.1983$datenaiss <- as.Date(deces.1983$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1983$age <- as.period(interval(deces.1983$datenaiss , deces.1983$datedeces ))
deces.1983$age_years <- year(deces.1983$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1983$sexe<-as.character(deces.1983$sexe)

deces.1983$SEX<-ifelse(deces.1983$sexe=="1","Homme","Femme")

deces.1983_final<-deces.1983[,c("COM","datedeces","age_years","SEX")]

deces.1983_final$nbr_mort<-1


deces.1983_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1983_final,sum)

library(tidyr)

deces.1983_spread <- deces.1983_final_ag %>%
  spread(SEX, nbr_mort)

deces.1983_spread$Femme[is.na(deces.1983_spread$Femme)]<-0
deces.1983_spread$Homme[is.na(deces.1983_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1983_final$tranche_age <- cut(deces.1983_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1983_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1983_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1983_final,sum)

library(tidyr)

deces.1983_spread2 <- deces.1983_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1983_spread2$`0-9`[is.na(deces.1983_spread2$`0-9`)]<-0
deces.1983_spread2$`10-19`[is.na(deces.1983_spread2$`10-19`)]<-0
deces.1983_spread2$`20-39`[is.na(deces.1983_spread2$`20-39`)]<-0
deces.1983_spread2$`40-59`[is.na(deces.1983_spread2$`40-59`)]<-0
deces.1983_spread2$`60-64`[is.na(deces.1983_spread2$`60-64`)]<-0
deces.1983_spread2$`65-69`[is.na(deces.1983_spread2$`65-69`)]<-0
deces.1983_spread2$`70-74`[is.na(deces.1983_spread2$`70-74`)]<-0
deces.1983_spread2$`75-79`[is.na(deces.1983_spread2$`75-79`)]<-0
deces.1983_spread2$`80+`[is.na(deces.1983_spread2$`80+`)]<-0


deces.1983_spread<-left_join(deces.1983_spread, deces.1983_spread2)


names(deces.1983_spread)[names(deces.1983_spread)=="datedeces"]<-"date"



fwrite(deces.1983_spread,"/fichier deces insee/décès travaillé/deces.1983_age_sexe.csv")





##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1984 <- read_delim("/fichier deces insee/deces-1984.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1984<-left_join(deces.1984, table_passage_bis)


deces.1984$COM<-ifelse(!is.na(deces.1984$COM_AP),deces.1984$COM_AP,deces.1984$lieudeces)

deces.1984$datedeces <- as.character(deces.1984$datedeces)
deces.1984$datedeces <- as.Date(deces.1984$datedeces, format = "%Y%m%d")


deces.1984$datenaiss <- as.character(deces.1984$datenaiss)
deces.1984$datenaiss <- as.Date(deces.1984$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1984$age <- as.period(interval(deces.1984$datenaiss , deces.1984$datedeces ))
deces.1984$age_years <- year(deces.1984$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1984$sexe<-as.character(deces.1984$sexe)

deces.1984$SEX<-ifelse(deces.1984$sexe=="1","Homme","Femme")

deces.1984_final<-deces.1984[,c("COM","datedeces","age_years","SEX")]

deces.1984_final$nbr_mort<-1


deces.1984_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1984_final,sum)

library(tidyr)

deces.1984_spread <- deces.1984_final_ag %>%
  spread(SEX, nbr_mort)

deces.1984_spread$Femme[is.na(deces.1984_spread$Femme)]<-0
deces.1984_spread$Homme[is.na(deces.1984_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1984_final$tranche_age <- cut(deces.1984_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1984_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1984_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1984_final,sum)

library(tidyr)

deces.1984_spread2 <- deces.1984_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1984_spread2$`0-9`[is.na(deces.1984_spread2$`0-9`)]<-0
deces.1984_spread2$`10-19`[is.na(deces.1984_spread2$`10-19`)]<-0
deces.1984_spread2$`20-39`[is.na(deces.1984_spread2$`20-39`)]<-0
deces.1984_spread2$`40-59`[is.na(deces.1984_spread2$`40-59`)]<-0
deces.1984_spread2$`60-64`[is.na(deces.1984_spread2$`60-64`)]<-0
deces.1984_spread2$`65-69`[is.na(deces.1984_spread2$`65-69`)]<-0
deces.1984_spread2$`70-74`[is.na(deces.1984_spread2$`70-74`)]<-0
deces.1984_spread2$`75-79`[is.na(deces.1984_spread2$`75-79`)]<-0
deces.1984_spread2$`80+`[is.na(deces.1984_spread2$`80+`)]<-0


deces.1984_spread<-left_join(deces.1984_spread, deces.1984_spread2)


names(deces.1984_spread)[names(deces.1984_spread)=="datedeces"]<-"date"



fwrite(deces.1984_spread,"/fichier deces insee/décès travaillé/deces.1984_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1985 <- read_delim("/fichier deces insee/deces-1985.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1985<-left_join(deces.1985, table_passage_bis)


deces.1985$COM<-ifelse(!is.na(deces.1985$COM_AP),deces.1985$COM_AP,deces.1985$lieudeces)

deces.1985$datedeces <- as.character(deces.1985$datedeces)
deces.1985$datedeces <- as.Date(deces.1985$datedeces, format = "%Y%m%d")


deces.1985$datenaiss <- as.character(deces.1985$datenaiss)
deces.1985$datenaiss <- as.Date(deces.1985$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1985$age <- as.period(interval(deces.1985$datenaiss , deces.1985$datedeces ))
deces.1985$age_years <- year(deces.1985$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1985$sexe<-as.character(deces.1985$sexe)

deces.1985$SEX<-ifelse(deces.1985$sexe=="1","Homme","Femme")

deces.1985_final<-deces.1985[,c("COM","datedeces","age_years","SEX")]

deces.1985_final$nbr_mort<-1


deces.1985_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1985_final,sum)

library(tidyr)

deces.1985_spread <- deces.1985_final_ag %>%
  spread(SEX, nbr_mort)

deces.1985_spread$Femme[is.na(deces.1985_spread$Femme)]<-0
deces.1985_spread$Homme[is.na(deces.1985_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1985_final$tranche_age <- cut(deces.1985_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1985_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1985_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1985_final,sum)

library(tidyr)

deces.1985_spread2 <- deces.1985_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1985_spread2$`0-9`[is.na(deces.1985_spread2$`0-9`)]<-0
deces.1985_spread2$`10-19`[is.na(deces.1985_spread2$`10-19`)]<-0
deces.1985_spread2$`20-39`[is.na(deces.1985_spread2$`20-39`)]<-0
deces.1985_spread2$`40-59`[is.na(deces.1985_spread2$`40-59`)]<-0
deces.1985_spread2$`60-64`[is.na(deces.1985_spread2$`60-64`)]<-0
deces.1985_spread2$`65-69`[is.na(deces.1985_spread2$`65-69`)]<-0
deces.1985_spread2$`70-74`[is.na(deces.1985_spread2$`70-74`)]<-0
deces.1985_spread2$`75-79`[is.na(deces.1985_spread2$`75-79`)]<-0
deces.1985_spread2$`80+`[is.na(deces.1985_spread2$`80+`)]<-0


deces.1985_spread<-left_join(deces.1985_spread, deces.1985_spread2)


names(deces.1985_spread)[names(deces.1985_spread)=="datedeces"]<-"date"



fwrite(deces.1985_spread,"/fichier deces insee/décès travaillé/deces.1985_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1986 <- read_delim("/fichier deces insee/deces-1986.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1986<-left_join(deces.1986, table_passage_bis)


deces.1986$COM<-ifelse(!is.na(deces.1986$COM_AP),deces.1986$COM_AP,deces.1986$lieudeces)

deces.1986$datedeces <- as.character(deces.1986$datedeces)
deces.1986$datedeces <- as.Date(deces.1986$datedeces, format = "%Y%m%d")


deces.1986$datenaiss <- as.character(deces.1986$datenaiss)
deces.1986$datenaiss <- as.Date(deces.1986$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1986$age <- as.period(interval(deces.1986$datenaiss , deces.1986$datedeces ))
deces.1986$age_years <- year(deces.1986$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1986$sexe<-as.character(deces.1986$sexe)

deces.1986$SEX<-ifelse(deces.1986$sexe=="1","Homme","Femme")

deces.1986_final<-deces.1986[,c("COM","datedeces","age_years","SEX")]

deces.1986_final$nbr_mort<-1


deces.1986_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1986_final,sum)

library(tidyr)

deces.1986_spread <- deces.1986_final_ag %>%
  spread(SEX, nbr_mort)

deces.1986_spread$Femme[is.na(deces.1986_spread$Femme)]<-0
deces.1986_spread$Homme[is.na(deces.1986_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1986_final$tranche_age <- cut(deces.1986_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1986_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1986_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1986_final,sum)

library(tidyr)

deces.1986_spread2 <- deces.1986_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1986_spread2$`0-9`[is.na(deces.1986_spread2$`0-9`)]<-0
deces.1986_spread2$`10-19`[is.na(deces.1986_spread2$`10-19`)]<-0
deces.1986_spread2$`20-39`[is.na(deces.1986_spread2$`20-39`)]<-0
deces.1986_spread2$`40-59`[is.na(deces.1986_spread2$`40-59`)]<-0
deces.1986_spread2$`60-64`[is.na(deces.1986_spread2$`60-64`)]<-0
deces.1986_spread2$`65-69`[is.na(deces.1986_spread2$`65-69`)]<-0
deces.1986_spread2$`70-74`[is.na(deces.1986_spread2$`70-74`)]<-0
deces.1986_spread2$`75-79`[is.na(deces.1986_spread2$`75-79`)]<-0
deces.1986_spread2$`80+`[is.na(deces.1986_spread2$`80+`)]<-0


deces.1986_spread<-left_join(deces.1986_spread, deces.1986_spread2)


names(deces.1986_spread)[names(deces.1986_spread)=="datedeces"]<-"date"



fwrite(deces.1986_spread,"/fichier deces insee/décès travaillé/deces.1986_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1987 <- read_delim("/fichier deces insee/deces-1987.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1987<-left_join(deces.1987, table_passage_bis)


deces.1987$COM<-ifelse(!is.na(deces.1987$COM_AP),deces.1987$COM_AP,deces.1987$lieudeces)

deces.1987$datedeces <- as.character(deces.1987$datedeces)
deces.1987$datedeces <- as.Date(deces.1987$datedeces, format = "%Y%m%d")


deces.1987$datenaiss <- as.character(deces.1987$datenaiss)
deces.1987$datenaiss <- as.Date(deces.1987$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1987$age <- as.period(interval(deces.1987$datenaiss , deces.1987$datedeces ))
deces.1987$age_years <- year(deces.1987$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1987$sexe<-as.character(deces.1987$sexe)

deces.1987$SEX<-ifelse(deces.1987$sexe=="1","Homme","Femme")

deces.1987_final<-deces.1987[,c("COM","datedeces","age_years","SEX")]

deces.1987_final$nbr_mort<-1


deces.1987_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1987_final,sum)

library(tidyr)

deces.1987_spread <- deces.1987_final_ag %>%
  spread(SEX, nbr_mort)

deces.1987_spread$Femme[is.na(deces.1987_spread$Femme)]<-0
deces.1987_spread$Homme[is.na(deces.1987_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1987_final$tranche_age <- cut(deces.1987_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1987_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1987_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1987_final,sum)

library(tidyr)

deces.1987_spread2 <- deces.1987_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1987_spread2$`0-9`[is.na(deces.1987_spread2$`0-9`)]<-0
deces.1987_spread2$`10-19`[is.na(deces.1987_spread2$`10-19`)]<-0
deces.1987_spread2$`20-39`[is.na(deces.1987_spread2$`20-39`)]<-0
deces.1987_spread2$`40-59`[is.na(deces.1987_spread2$`40-59`)]<-0
deces.1987_spread2$`60-64`[is.na(deces.1987_spread2$`60-64`)]<-0
deces.1987_spread2$`65-69`[is.na(deces.1987_spread2$`65-69`)]<-0
deces.1987_spread2$`70-74`[is.na(deces.1987_spread2$`70-74`)]<-0
deces.1987_spread2$`75-79`[is.na(deces.1987_spread2$`75-79`)]<-0
deces.1987_spread2$`80+`[is.na(deces.1987_spread2$`80+`)]<-0


deces.1987_spread<-left_join(deces.1987_spread, deces.1987_spread2)


names(deces.1987_spread)[names(deces.1987_spread)=="datedeces"]<-"date"



fwrite(deces.1987_spread,"/fichier deces insee/décès travaillé/deces.1987_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1988 <- read_delim("/fichier deces insee/deces-1988.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1988<-left_join(deces.1988, table_passage_bis)


deces.1988$COM<-ifelse(!is.na(deces.1988$COM_AP),deces.1988$COM_AP,deces.1988$lieudeces)

deces.1988$datedeces <- as.character(deces.1988$datedeces)
deces.1988$datedeces <- as.Date(deces.1988$datedeces, format = "%Y%m%d")


deces.1988$datenaiss <- as.character(deces.1988$datenaiss)
deces.1988$datenaiss <- as.Date(deces.1988$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1988$age <- as.period(interval(deces.1988$datenaiss , deces.1988$datedeces ))
deces.1988$age_years <- year(deces.1988$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1988$sexe<-as.character(deces.1988$sexe)

deces.1988$SEX<-ifelse(deces.1988$sexe=="1","Homme","Femme")

deces.1988_final<-deces.1988[,c("COM","datedeces","age_years","SEX")]

deces.1988_final$nbr_mort<-1


deces.1988_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1988_final,sum)

library(tidyr)

deces.1988_spread <- deces.1988_final_ag %>%
  spread(SEX, nbr_mort)

deces.1988_spread$Femme[is.na(deces.1988_spread$Femme)]<-0
deces.1988_spread$Homme[is.na(deces.1988_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1988_final$tranche_age <- cut(deces.1988_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1988_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1988_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1988_final,sum)

library(tidyr)

deces.1988_spread2 <- deces.1988_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1988_spread2$`0-9`[is.na(deces.1988_spread2$`0-9`)]<-0
deces.1988_spread2$`10-19`[is.na(deces.1988_spread2$`10-19`)]<-0
deces.1988_spread2$`20-39`[is.na(deces.1988_spread2$`20-39`)]<-0
deces.1988_spread2$`40-59`[is.na(deces.1988_spread2$`40-59`)]<-0
deces.1988_spread2$`60-64`[is.na(deces.1988_spread2$`60-64`)]<-0
deces.1988_spread2$`65-69`[is.na(deces.1988_spread2$`65-69`)]<-0
deces.1988_spread2$`70-74`[is.na(deces.1988_spread2$`70-74`)]<-0
deces.1988_spread2$`75-79`[is.na(deces.1988_spread2$`75-79`)]<-0
deces.1988_spread2$`80+`[is.na(deces.1988_spread2$`80+`)]<-0


deces.1988_spread<-left_join(deces.1988_spread, deces.1988_spread2)


names(deces.1988_spread)[names(deces.1988_spread)=="datedeces"]<-"date"



fwrite(deces.1988_spread,"/fichier deces insee/décès travaillé/deces.1988_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1989 <- read_delim("/fichier deces insee/deces-1989.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1989<-left_join(deces.1989, table_passage_bis)


deces.1989$COM<-ifelse(!is.na(deces.1989$COM_AP),deces.1989$COM_AP,deces.1989$lieudeces)

deces.1989$datedeces <- as.character(deces.1989$datedeces)
deces.1989$datedeces <- as.Date(deces.1989$datedeces, format = "%Y%m%d")


deces.1989$datenaiss <- as.character(deces.1989$datenaiss)
deces.1989$datenaiss <- as.Date(deces.1989$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1989$age <- as.period(interval(deces.1989$datenaiss , deces.1989$datedeces ))
deces.1989$age_years <- year(deces.1989$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1989$sexe<-as.character(deces.1989$sexe)

deces.1989$SEX<-ifelse(deces.1989$sexe=="1","Homme","Femme")

deces.1989_final<-deces.1989[,c("COM","datedeces","age_years","SEX")]

deces.1989_final$nbr_mort<-1


deces.1989_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1989_final,sum)

library(tidyr)

deces.1989_spread <- deces.1989_final_ag %>%
  spread(SEX, nbr_mort)

deces.1989_spread$Femme[is.na(deces.1989_spread$Femme)]<-0
deces.1989_spread$Homme[is.na(deces.1989_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1989_final$tranche_age <- cut(deces.1989_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1989_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1989_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1989_final,sum)

library(tidyr)

deces.1989_spread2 <- deces.1989_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1989_spread2$`0-9`[is.na(deces.1989_spread2$`0-9`)]<-0
deces.1989_spread2$`10-19`[is.na(deces.1989_spread2$`10-19`)]<-0
deces.1989_spread2$`20-39`[is.na(deces.1989_spread2$`20-39`)]<-0
deces.1989_spread2$`40-59`[is.na(deces.1989_spread2$`40-59`)]<-0
deces.1989_spread2$`60-64`[is.na(deces.1989_spread2$`60-64`)]<-0
deces.1989_spread2$`65-69`[is.na(deces.1989_spread2$`65-69`)]<-0
deces.1989_spread2$`70-74`[is.na(deces.1989_spread2$`70-74`)]<-0
deces.1989_spread2$`75-79`[is.na(deces.1989_spread2$`75-79`)]<-0
deces.1989_spread2$`80+`[is.na(deces.1989_spread2$`80+`)]<-0


deces.1989_spread<-left_join(deces.1989_spread, deces.1989_spread2)


names(deces.1989_spread)[names(deces.1989_spread)=="datedeces"]<-"date"



fwrite(deces.1989_spread,"/fichier deces insee/décès travaillé/deces.1989_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1990 <- read_delim("/fichier deces insee/deces-1990.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1990<-left_join(deces.1990, table_passage_bis)


deces.1990$COM<-ifelse(!is.na(deces.1990$COM_AP),deces.1990$COM_AP,deces.1990$lieudeces)

deces.1990$datedeces <- as.character(deces.1990$datedeces)
deces.1990$datedeces <- as.Date(deces.1990$datedeces, format = "%Y%m%d")


deces.1990$datenaiss <- as.character(deces.1990$datenaiss)
deces.1990$datenaiss <- as.Date(deces.1990$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1990$age <- as.period(interval(deces.1990$datenaiss , deces.1990$datedeces ))
deces.1990$age_years <- year(deces.1990$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1990$sexe<-as.character(deces.1990$sexe)

deces.1990$SEX<-ifelse(deces.1990$sexe=="1","Homme","Femme")

deces.1990_final<-deces.1990[,c("COM","datedeces","age_years","SEX")]

deces.1990_final$nbr_mort<-1


deces.1990_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1990_final,sum)

library(tidyr)

deces.1990_spread <- deces.1990_final_ag %>%
  spread(SEX, nbr_mort)

deces.1990_spread$Femme[is.na(deces.1990_spread$Femme)]<-0
deces.1990_spread$Homme[is.na(deces.1990_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1990_final$tranche_age <- cut(deces.1990_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1990_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1990_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1990_final,sum)

library(tidyr)

deces.1990_spread2 <- deces.1990_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1990_spread2$`0-9`[is.na(deces.1990_spread2$`0-9`)]<-0
deces.1990_spread2$`10-19`[is.na(deces.1990_spread2$`10-19`)]<-0
deces.1990_spread2$`20-39`[is.na(deces.1990_spread2$`20-39`)]<-0
deces.1990_spread2$`40-59`[is.na(deces.1990_spread2$`40-59`)]<-0
deces.1990_spread2$`60-64`[is.na(deces.1990_spread2$`60-64`)]<-0
deces.1990_spread2$`65-69`[is.na(deces.1990_spread2$`65-69`)]<-0
deces.1990_spread2$`70-74`[is.na(deces.1990_spread2$`70-74`)]<-0
deces.1990_spread2$`75-79`[is.na(deces.1990_spread2$`75-79`)]<-0
deces.1990_spread2$`80+`[is.na(deces.1990_spread2$`80+`)]<-0


deces.1990_spread<-left_join(deces.1990_spread, deces.1990_spread2)


names(deces.1990_spread)[names(deces.1990_spread)=="datedeces"]<-"date"



fwrite(deces.1990_spread,"/fichier deces insee/décès travaillé/deces.1990_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1991 <- read_delim("/fichier deces insee/deces-1991.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1991<-left_join(deces.1991, table_passage_bis)


deces.1991$COM<-ifelse(!is.na(deces.1991$COM_AP),deces.1991$COM_AP,deces.1991$lieudeces)

deces.1991$datedeces <- as.character(deces.1991$datedeces)
deces.1991$datedeces <- as.Date(deces.1991$datedeces, format = "%Y%m%d")


deces.1991$datenaiss <- as.character(deces.1991$datenaiss)
deces.1991$datenaiss <- as.Date(deces.1991$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1991$age <- as.period(interval(deces.1991$datenaiss , deces.1991$datedeces ))
deces.1991$age_years <- year(deces.1991$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1991$sexe<-as.character(deces.1991$sexe)

deces.1991$SEX<-ifelse(deces.1991$sexe=="1","Homme","Femme")

deces.1991_final<-deces.1991[,c("COM","datedeces","age_years","SEX")]

deces.1991_final$nbr_mort<-1


deces.1991_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1991_final,sum)

library(tidyr)

deces.1991_spread <- deces.1991_final_ag %>%
  spread(SEX, nbr_mort)

deces.1991_spread$Femme[is.na(deces.1991_spread$Femme)]<-0
deces.1991_spread$Homme[is.na(deces.1991_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1991_final$tranche_age <- cut(deces.1991_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1991_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1991_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1991_final,sum)

library(tidyr)

deces.1991_spread2 <- deces.1991_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1991_spread2$`0-9`[is.na(deces.1991_spread2$`0-9`)]<-0
deces.1991_spread2$`10-19`[is.na(deces.1991_spread2$`10-19`)]<-0
deces.1991_spread2$`20-39`[is.na(deces.1991_spread2$`20-39`)]<-0
deces.1991_spread2$`40-59`[is.na(deces.1991_spread2$`40-59`)]<-0
deces.1991_spread2$`60-64`[is.na(deces.1991_spread2$`60-64`)]<-0
deces.1991_spread2$`65-69`[is.na(deces.1991_spread2$`65-69`)]<-0
deces.1991_spread2$`70-74`[is.na(deces.1991_spread2$`70-74`)]<-0
deces.1991_spread2$`75-79`[is.na(deces.1991_spread2$`75-79`)]<-0
deces.1991_spread2$`80+`[is.na(deces.1991_spread2$`80+`)]<-0


deces.1991_spread<-left_join(deces.1991_spread, deces.1991_spread2)


names(deces.1991_spread)[names(deces.1991_spread)=="datedeces"]<-"date"



fwrite(deces.1991_spread,"/fichier deces insee/décès travaillé/deces.1991_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1992 <- read_delim("/fichier deces insee/deces-1992.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1992<-left_join(deces.1992, table_passage_bis)


deces.1992$COM<-ifelse(!is.na(deces.1992$COM_AP),deces.1992$COM_AP,deces.1992$lieudeces)

deces.1992$datedeces <- as.character(deces.1992$datedeces)
deces.1992$datedeces <- as.Date(deces.1992$datedeces, format = "%Y%m%d")


deces.1992$datenaiss <- as.character(deces.1992$datenaiss)
deces.1992$datenaiss <- as.Date(deces.1992$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1992$age <- as.period(interval(deces.1992$datenaiss , deces.1992$datedeces ))
deces.1992$age_years <- year(deces.1992$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1992$sexe<-as.character(deces.1992$sexe)

deces.1992$SEX<-ifelse(deces.1992$sexe=="1","Homme","Femme")

deces.1992_final<-deces.1992[,c("COM","datedeces","age_years","SEX")]

deces.1992_final$nbr_mort<-1


deces.1992_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1992_final,sum)

library(tidyr)

deces.1992_spread <- deces.1992_final_ag %>%
  spread(SEX, nbr_mort)

deces.1992_spread$Femme[is.na(deces.1992_spread$Femme)]<-0
deces.1992_spread$Homme[is.na(deces.1992_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1992_final$tranche_age <- cut(deces.1992_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1992_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1992_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1992_final,sum)

library(tidyr)

deces.1992_spread2 <- deces.1992_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1992_spread2$`0-9`[is.na(deces.1992_spread2$`0-9`)]<-0
deces.1992_spread2$`10-19`[is.na(deces.1992_spread2$`10-19`)]<-0
deces.1992_spread2$`20-39`[is.na(deces.1992_spread2$`20-39`)]<-0
deces.1992_spread2$`40-59`[is.na(deces.1992_spread2$`40-59`)]<-0
deces.1992_spread2$`60-64`[is.na(deces.1992_spread2$`60-64`)]<-0
deces.1992_spread2$`65-69`[is.na(deces.1992_spread2$`65-69`)]<-0
deces.1992_spread2$`70-74`[is.na(deces.1992_spread2$`70-74`)]<-0
deces.1992_spread2$`75-79`[is.na(deces.1992_spread2$`75-79`)]<-0
deces.1992_spread2$`80+`[is.na(deces.1992_spread2$`80+`)]<-0


deces.1992_spread<-left_join(deces.1992_spread, deces.1992_spread2)


names(deces.1992_spread)[names(deces.1992_spread)=="datedeces"]<-"date"



fwrite(deces.1992_spread,"/fichier deces insee/décès travaillé/deces.1992_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1993 <- read_delim("/fichier deces insee/deces-1993.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1993<-left_join(deces.1993, table_passage_bis)


deces.1993$COM<-ifelse(!is.na(deces.1993$COM_AP),deces.1993$COM_AP,deces.1993$lieudeces)

deces.1993$datedeces <- as.character(deces.1993$datedeces)
deces.1993$datedeces <- as.Date(deces.1993$datedeces, format = "%Y%m%d")


deces.1993$datenaiss <- as.character(deces.1993$datenaiss)
deces.1993$datenaiss <- as.Date(deces.1993$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1993$age <- as.period(interval(deces.1993$datenaiss , deces.1993$datedeces ))
deces.1993$age_years <- year(deces.1993$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1993$sexe<-as.character(deces.1993$sexe)

deces.1993$SEX<-ifelse(deces.1993$sexe=="1","Homme","Femme")

deces.1993_final<-deces.1993[,c("COM","datedeces","age_years","SEX")]

deces.1993_final$nbr_mort<-1


deces.1993_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1993_final,sum)

library(tidyr)

deces.1993_spread <- deces.1993_final_ag %>%
  spread(SEX, nbr_mort)

deces.1993_spread$Femme[is.na(deces.1993_spread$Femme)]<-0
deces.1993_spread$Homme[is.na(deces.1993_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1993_final$tranche_age <- cut(deces.1993_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1993_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1993_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1993_final,sum)

library(tidyr)

deces.1993_spread2 <- deces.1993_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1993_spread2$`0-9`[is.na(deces.1993_spread2$`0-9`)]<-0
deces.1993_spread2$`10-19`[is.na(deces.1993_spread2$`10-19`)]<-0
deces.1993_spread2$`20-39`[is.na(deces.1993_spread2$`20-39`)]<-0
deces.1993_spread2$`40-59`[is.na(deces.1993_spread2$`40-59`)]<-0
deces.1993_spread2$`60-64`[is.na(deces.1993_spread2$`60-64`)]<-0
deces.1993_spread2$`65-69`[is.na(deces.1993_spread2$`65-69`)]<-0
deces.1993_spread2$`70-74`[is.na(deces.1993_spread2$`70-74`)]<-0
deces.1993_spread2$`75-79`[is.na(deces.1993_spread2$`75-79`)]<-0
deces.1993_spread2$`80+`[is.na(deces.1993_spread2$`80+`)]<-0


deces.1993_spread<-left_join(deces.1993_spread, deces.1993_spread2)


names(deces.1993_spread)[names(deces.1993_spread)=="datedeces"]<-"date"



fwrite(deces.1993_spread,"/fichier deces insee/décès travaillé/deces.1993_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1994 <- read_delim("/fichier deces insee/deces-1994.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1994<-left_join(deces.1994, table_passage_bis)


deces.1994$COM<-ifelse(!is.na(deces.1994$COM_AP),deces.1994$COM_AP,deces.1994$lieudeces)

deces.1994$datedeces <- as.character(deces.1994$datedeces)
deces.1994$datedeces <- as.Date(deces.1994$datedeces, format = "%Y%m%d")


deces.1994$datenaiss <- as.character(deces.1994$datenaiss)
deces.1994$datenaiss <- as.Date(deces.1994$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1994$age <- as.period(interval(deces.1994$datenaiss , deces.1994$datedeces ))
deces.1994$age_years <- year(deces.1994$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1994$sexe<-as.character(deces.1994$sexe)

deces.1994$SEX<-ifelse(deces.1994$sexe=="1","Homme","Femme")

deces.1994_final<-deces.1994[,c("COM","datedeces","age_years","SEX")]

deces.1994_final$nbr_mort<-1


deces.1994_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1994_final,sum)

library(tidyr)

deces.1994_spread <- deces.1994_final_ag %>%
  spread(SEX, nbr_mort)

deces.1994_spread$Femme[is.na(deces.1994_spread$Femme)]<-0
deces.1994_spread$Homme[is.na(deces.1994_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1994_final$tranche_age <- cut(deces.1994_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1994_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1994_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1994_final,sum)

library(tidyr)

deces.1994_spread2 <- deces.1994_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1994_spread2$`0-9`[is.na(deces.1994_spread2$`0-9`)]<-0
deces.1994_spread2$`10-19`[is.na(deces.1994_spread2$`10-19`)]<-0
deces.1994_spread2$`20-39`[is.na(deces.1994_spread2$`20-39`)]<-0
deces.1994_spread2$`40-59`[is.na(deces.1994_spread2$`40-59`)]<-0
deces.1994_spread2$`60-64`[is.na(deces.1994_spread2$`60-64`)]<-0
deces.1994_spread2$`65-69`[is.na(deces.1994_spread2$`65-69`)]<-0
deces.1994_spread2$`70-74`[is.na(deces.1994_spread2$`70-74`)]<-0
deces.1994_spread2$`75-79`[is.na(deces.1994_spread2$`75-79`)]<-0
deces.1994_spread2$`80+`[is.na(deces.1994_spread2$`80+`)]<-0


deces.1994_spread<-left_join(deces.1994_spread, deces.1994_spread2)


names(deces.1994_spread)[names(deces.1994_spread)=="datedeces"]<-"date"



fwrite(deces.1994_spread,"/fichier deces insee/décès travaillé/deces.1994_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1995 <- read_delim("/fichier deces insee/deces-1995.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1995<-left_join(deces.1995, table_passage_bis)


deces.1995$COM<-ifelse(!is.na(deces.1995$COM_AP),deces.1995$COM_AP,deces.1995$lieudeces)

deces.1995$datedeces <- as.character(deces.1995$datedeces)
deces.1995$datedeces <- as.Date(deces.1995$datedeces, format = "%Y%m%d")


deces.1995$datenaiss <- as.character(deces.1995$datenaiss)
deces.1995$datenaiss <- as.Date(deces.1995$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1995$age <- as.period(interval(deces.1995$datenaiss , deces.1995$datedeces ))
deces.1995$age_years <- year(deces.1995$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1995$sexe<-as.character(deces.1995$sexe)

deces.1995$SEX<-ifelse(deces.1995$sexe=="1","Homme","Femme")

deces.1995_final<-deces.1995[,c("COM","datedeces","age_years","SEX")]

deces.1995_final$nbr_mort<-1


deces.1995_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1995_final,sum)

library(tidyr)

deces.1995_spread <- deces.1995_final_ag %>%
  spread(SEX, nbr_mort)

deces.1995_spread$Femme[is.na(deces.1995_spread$Femme)]<-0
deces.1995_spread$Homme[is.na(deces.1995_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1995_final$tranche_age <- cut(deces.1995_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1995_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1995_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1995_final,sum)

library(tidyr)

deces.1995_spread2 <- deces.1995_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1995_spread2$`0-9`[is.na(deces.1995_spread2$`0-9`)]<-0
deces.1995_spread2$`10-19`[is.na(deces.1995_spread2$`10-19`)]<-0
deces.1995_spread2$`20-39`[is.na(deces.1995_spread2$`20-39`)]<-0
deces.1995_spread2$`40-59`[is.na(deces.1995_spread2$`40-59`)]<-0
deces.1995_spread2$`60-64`[is.na(deces.1995_spread2$`60-64`)]<-0
deces.1995_spread2$`65-69`[is.na(deces.1995_spread2$`65-69`)]<-0
deces.1995_spread2$`70-74`[is.na(deces.1995_spread2$`70-74`)]<-0
deces.1995_spread2$`75-79`[is.na(deces.1995_spread2$`75-79`)]<-0
deces.1995_spread2$`80+`[is.na(deces.1995_spread2$`80+`)]<-0


deces.1995_spread<-left_join(deces.1995_spread, deces.1995_spread2)


names(deces.1995_spread)[names(deces.1995_spread)=="datedeces"]<-"date"



fwrite(deces.1995_spread,"/fichier deces insee/décès travaillé/deces.1995_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1996 <- read_delim("/fichier deces insee/deces-1996.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1996<-left_join(deces.1996, table_passage_bis)


deces.1996$COM<-ifelse(!is.na(deces.1996$COM_AP),deces.1996$COM_AP,deces.1996$lieudeces)

deces.1996$datedeces <- as.character(deces.1996$datedeces)
deces.1996$datedeces <- as.Date(deces.1996$datedeces, format = "%Y%m%d")


deces.1996$datenaiss <- as.character(deces.1996$datenaiss)
deces.1996$datenaiss <- as.Date(deces.1996$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1996$age <- as.period(interval(deces.1996$datenaiss , deces.1996$datedeces ))
deces.1996$age_years <- year(deces.1996$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1996$sexe<-as.character(deces.1996$sexe)

deces.1996$SEX<-ifelse(deces.1996$sexe=="1","Homme","Femme")

deces.1996_final<-deces.1996[,c("COM","datedeces","age_years","SEX")]

deces.1996_final$nbr_mort<-1


deces.1996_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1996_final,sum)

library(tidyr)

deces.1996_spread <- deces.1996_final_ag %>%
  spread(SEX, nbr_mort)

deces.1996_spread$Femme[is.na(deces.1996_spread$Femme)]<-0
deces.1996_spread$Homme[is.na(deces.1996_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1996_final$tranche_age <- cut(deces.1996_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1996_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1996_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1996_final,sum)

library(tidyr)

deces.1996_spread2 <- deces.1996_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1996_spread2$`0-9`[is.na(deces.1996_spread2$`0-9`)]<-0
deces.1996_spread2$`10-19`[is.na(deces.1996_spread2$`10-19`)]<-0
deces.1996_spread2$`20-39`[is.na(deces.1996_spread2$`20-39`)]<-0
deces.1996_spread2$`40-59`[is.na(deces.1996_spread2$`40-59`)]<-0
deces.1996_spread2$`60-64`[is.na(deces.1996_spread2$`60-64`)]<-0
deces.1996_spread2$`65-69`[is.na(deces.1996_spread2$`65-69`)]<-0
deces.1996_spread2$`70-74`[is.na(deces.1996_spread2$`70-74`)]<-0
deces.1996_spread2$`75-79`[is.na(deces.1996_spread2$`75-79`)]<-0
deces.1996_spread2$`80+`[is.na(deces.1996_spread2$`80+`)]<-0


deces.1996_spread<-left_join(deces.1996_spread, deces.1996_spread2)


names(deces.1996_spread)[names(deces.1996_spread)=="datedeces"]<-"date"



fwrite(deces.1996_spread,"/fichier deces insee/décès travaillé/deces.1996_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1997 <- read_delim("/fichier deces insee/deces-1997.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1997<-left_join(deces.1997, table_passage_bis)


deces.1997$COM<-ifelse(!is.na(deces.1997$COM_AP),deces.1997$COM_AP,deces.1997$lieudeces)

deces.1997$datedeces <- as.character(deces.1997$datedeces)
deces.1997$datedeces <- as.Date(deces.1997$datedeces, format = "%Y%m%d")


deces.1997$datenaiss <- as.character(deces.1997$datenaiss)
deces.1997$datenaiss <- as.Date(deces.1997$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1997$age <- as.period(interval(deces.1997$datenaiss , deces.1997$datedeces ))
deces.1997$age_years <- year(deces.1997$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1997$sexe<-as.character(deces.1997$sexe)

deces.1997$SEX<-ifelse(deces.1997$sexe=="1","Homme","Femme")

deces.1997_final<-deces.1997[,c("COM","datedeces","age_years","SEX")]

deces.1997_final$nbr_mort<-1


deces.1997_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1997_final,sum)

library(tidyr)

deces.1997_spread <- deces.1997_final_ag %>%
  spread(SEX, nbr_mort)

deces.1997_spread$Femme[is.na(deces.1997_spread$Femme)]<-0
deces.1997_spread$Homme[is.na(deces.1997_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1997_final$tranche_age <- cut(deces.1997_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1997_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1997_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1997_final,sum)

library(tidyr)

deces.1997_spread2 <- deces.1997_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1997_spread2$`0-9`[is.na(deces.1997_spread2$`0-9`)]<-0
deces.1997_spread2$`10-19`[is.na(deces.1997_spread2$`10-19`)]<-0
deces.1997_spread2$`20-39`[is.na(deces.1997_spread2$`20-39`)]<-0
deces.1997_spread2$`40-59`[is.na(deces.1997_spread2$`40-59`)]<-0
deces.1997_spread2$`60-64`[is.na(deces.1997_spread2$`60-64`)]<-0
deces.1997_spread2$`65-69`[is.na(deces.1997_spread2$`65-69`)]<-0
deces.1997_spread2$`70-74`[is.na(deces.1997_spread2$`70-74`)]<-0
deces.1997_spread2$`75-79`[is.na(deces.1997_spread2$`75-79`)]<-0
deces.1997_spread2$`80+`[is.na(deces.1997_spread2$`80+`)]<-0


deces.1997_spread<-left_join(deces.1997_spread, deces.1997_spread2)


names(deces.1997_spread)[names(deces.1997_spread)=="datedeces"]<-"date"



fwrite(deces.1997_spread,"/fichier deces insee/décès travaillé/deces.1997_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1998 <- read_delim("/fichier deces insee/deces-1998.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1998<-left_join(deces.1998, table_passage_bis)


deces.1998$COM<-ifelse(!is.na(deces.1998$COM_AP),deces.1998$COM_AP,deces.1998$lieudeces)

deces.1998$datedeces <- as.character(deces.1998$datedeces)
deces.1998$datedeces <- as.Date(deces.1998$datedeces, format = "%Y%m%d")


deces.1998$datenaiss <- as.character(deces.1998$datenaiss)
deces.1998$datenaiss <- as.Date(deces.1998$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1998$age <- as.period(interval(deces.1998$datenaiss , deces.1998$datedeces ))
deces.1998$age_years <- year(deces.1998$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1998$sexe<-as.character(deces.1998$sexe)

deces.1998$SEX<-ifelse(deces.1998$sexe=="1","Homme","Femme")

deces.1998_final<-deces.1998[,c("COM","datedeces","age_years","SEX")]

deces.1998_final$nbr_mort<-1


deces.1998_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1998_final,sum)

library(tidyr)

deces.1998_spread <- deces.1998_final_ag %>%
  spread(SEX, nbr_mort)

deces.1998_spread$Femme[is.na(deces.1998_spread$Femme)]<-0
deces.1998_spread$Homme[is.na(deces.1998_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1998_final$tranche_age <- cut(deces.1998_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1998_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1998_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1998_final,sum)

library(tidyr)

deces.1998_spread2 <- deces.1998_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1998_spread2$`0-9`[is.na(deces.1998_spread2$`0-9`)]<-0
deces.1998_spread2$`10-19`[is.na(deces.1998_spread2$`10-19`)]<-0
deces.1998_spread2$`20-39`[is.na(deces.1998_spread2$`20-39`)]<-0
deces.1998_spread2$`40-59`[is.na(deces.1998_spread2$`40-59`)]<-0
deces.1998_spread2$`60-64`[is.na(deces.1998_spread2$`60-64`)]<-0
deces.1998_spread2$`65-69`[is.na(deces.1998_spread2$`65-69`)]<-0
deces.1998_spread2$`70-74`[is.na(deces.1998_spread2$`70-74`)]<-0
deces.1998_spread2$`75-79`[is.na(deces.1998_spread2$`75-79`)]<-0
deces.1998_spread2$`80+`[is.na(deces.1998_spread2$`80+`)]<-0


deces.1998_spread<-left_join(deces.1998_spread, deces.1998_spread2)


names(deces.1998_spread)[names(deces.1998_spread)=="datedeces"]<-"date"



fwrite(deces.1998_spread,"/fichier deces insee/décès travaillé/deces.1998_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.1999 <- read_delim("/fichier deces insee/deces-1999.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.1999<-left_join(deces.1999, table_passage_bis)


deces.1999$COM<-ifelse(!is.na(deces.1999$COM_AP),deces.1999$COM_AP,deces.1999$lieudeces)

deces.1999$datedeces <- as.character(deces.1999$datedeces)
deces.1999$datedeces <- as.Date(deces.1999$datedeces, format = "%Y%m%d")


deces.1999$datenaiss <- as.character(deces.1999$datenaiss)
deces.1999$datenaiss <- as.Date(deces.1999$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.1999$age <- as.period(interval(deces.1999$datenaiss , deces.1999$datedeces ))
deces.1999$age_years <- year(deces.1999$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.1999$sexe<-as.character(deces.1999$sexe)

deces.1999$SEX<-ifelse(deces.1999$sexe=="1","Homme","Femme")

deces.1999_final<-deces.1999[,c("COM","datedeces","age_years","SEX")]

deces.1999_final$nbr_mort<-1


deces.1999_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.1999_final,sum)

library(tidyr)

deces.1999_spread <- deces.1999_final_ag %>%
  spread(SEX, nbr_mort)

deces.1999_spread$Femme[is.na(deces.1999_spread$Femme)]<-0
deces.1999_spread$Homme[is.na(deces.1999_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.1999_final$tranche_age <- cut(deces.1999_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.1999_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.1999_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.1999_final,sum)

library(tidyr)

deces.1999_spread2 <- deces.1999_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.1999_spread2$`0-9`[is.na(deces.1999_spread2$`0-9`)]<-0
deces.1999_spread2$`10-19`[is.na(deces.1999_spread2$`10-19`)]<-0
deces.1999_spread2$`20-39`[is.na(deces.1999_spread2$`20-39`)]<-0
deces.1999_spread2$`40-59`[is.na(deces.1999_spread2$`40-59`)]<-0
deces.1999_spread2$`60-64`[is.na(deces.1999_spread2$`60-64`)]<-0
deces.1999_spread2$`65-69`[is.na(deces.1999_spread2$`65-69`)]<-0
deces.1999_spread2$`70-74`[is.na(deces.1999_spread2$`70-74`)]<-0
deces.1999_spread2$`75-79`[is.na(deces.1999_spread2$`75-79`)]<-0
deces.1999_spread2$`80+`[is.na(deces.1999_spread2$`80+`)]<-0


deces.1999_spread<-left_join(deces.1999_spread, deces.1999_spread2)


names(deces.1999_spread)[names(deces.1999_spread)=="datedeces"]<-"date"



fwrite(deces.1999_spread,"/fichier deces insee/décès travaillé/deces.1999_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2000 <- read_delim("/fichier deces insee/deces-2000.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2000<-left_join(deces.2000, table_passage_bis)


deces.2000$COM<-ifelse(!is.na(deces.2000$COM_AP),deces.2000$COM_AP,deces.2000$lieudeces)

deces.2000$datedeces <- as.character(deces.2000$datedeces)
deces.2000$datedeces <- as.Date(deces.2000$datedeces, format = "%Y%m%d")


deces.2000$datenaiss <- as.character(deces.2000$datenaiss)
deces.2000$datenaiss <- as.Date(deces.2000$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2000$age <- as.period(interval(deces.2000$datenaiss , deces.2000$datedeces ))
deces.2000$age_years <- year(deces.2000$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2000$sexe<-as.character(deces.2000$sexe)

deces.2000$SEX<-ifelse(deces.2000$sexe=="1","Homme","Femme")

deces.2000_final<-deces.2000[,c("COM","datedeces","age_years","SEX")]

deces.2000_final$nbr_mort<-1


deces.2000_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2000_final,sum)

library(tidyr)

deces.2000_spread <- deces.2000_final_ag %>%
  spread(SEX, nbr_mort)

deces.2000_spread$Femme[is.na(deces.2000_spread$Femme)]<-0
deces.2000_spread$Homme[is.na(deces.2000_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2000_final$tranche_age <- cut(deces.2000_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2000_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2000_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2000_final,sum)

library(tidyr)

deces.2000_spread2 <- deces.2000_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2000_spread2$`0-9`[is.na(deces.2000_spread2$`0-9`)]<-0
deces.2000_spread2$`10-19`[is.na(deces.2000_spread2$`10-19`)]<-0
deces.2000_spread2$`20-39`[is.na(deces.2000_spread2$`20-39`)]<-0
deces.2000_spread2$`40-59`[is.na(deces.2000_spread2$`40-59`)]<-0
deces.2000_spread2$`60-64`[is.na(deces.2000_spread2$`60-64`)]<-0
deces.2000_spread2$`65-69`[is.na(deces.2000_spread2$`65-69`)]<-0
deces.2000_spread2$`70-74`[is.na(deces.2000_spread2$`70-74`)]<-0
deces.2000_spread2$`75-79`[is.na(deces.2000_spread2$`75-79`)]<-0
deces.2000_spread2$`80+`[is.na(deces.2000_spread2$`80+`)]<-0


deces.2000_spread<-left_join(deces.2000_spread, deces.2000_spread2)


names(deces.2000_spread)[names(deces.2000_spread)=="datedeces"]<-"date"



fwrite(deces.2000_spread,"/fichier deces insee/décès travaillé/deces.2000_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2001 <- read_delim("/fichier deces insee/deces-2001.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2001<-left_join(deces.2001, table_passage_bis)


deces.2001$COM<-ifelse(!is.na(deces.2001$COM_AP),deces.2001$COM_AP,deces.2001$lieudeces)

deces.2001$datedeces <- as.character(deces.2001$datedeces)
deces.2001$datedeces <- as.Date(deces.2001$datedeces, format = "%Y%m%d")


deces.2001$datenaiss <- as.character(deces.2001$datenaiss)
deces.2001$datenaiss <- as.Date(deces.2001$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2001$age <- as.period(interval(deces.2001$datenaiss , deces.2001$datedeces ))
deces.2001$age_years <- year(deces.2001$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2001$sexe<-as.character(deces.2001$sexe)

deces.2001$SEX<-ifelse(deces.2001$sexe=="1","Homme","Femme")

deces.2001_final<-deces.2001[,c("COM","datedeces","age_years","SEX")]

deces.2001_final$nbr_mort<-1


deces.2001_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2001_final,sum)

library(tidyr)

deces.2001_spread <- deces.2001_final_ag %>%
  spread(SEX, nbr_mort)

deces.2001_spread$Femme[is.na(deces.2001_spread$Femme)]<-0
deces.2001_spread$Homme[is.na(deces.2001_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2001_final$tranche_age <- cut(deces.2001_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2001_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2001_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2001_final,sum)

library(tidyr)

deces.2001_spread2 <- deces.2001_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2001_spread2$`0-9`[is.na(deces.2001_spread2$`0-9`)]<-0
deces.2001_spread2$`10-19`[is.na(deces.2001_spread2$`10-19`)]<-0
deces.2001_spread2$`20-39`[is.na(deces.2001_spread2$`20-39`)]<-0
deces.2001_spread2$`40-59`[is.na(deces.2001_spread2$`40-59`)]<-0
deces.2001_spread2$`60-64`[is.na(deces.2001_spread2$`60-64`)]<-0
deces.2001_spread2$`65-69`[is.na(deces.2001_spread2$`65-69`)]<-0
deces.2001_spread2$`70-74`[is.na(deces.2001_spread2$`70-74`)]<-0
deces.2001_spread2$`75-79`[is.na(deces.2001_spread2$`75-79`)]<-0
deces.2001_spread2$`80+`[is.na(deces.2001_spread2$`80+`)]<-0


deces.2001_spread<-left_join(deces.2001_spread, deces.2001_spread2)


names(deces.2001_spread)[names(deces.2001_spread)=="datedeces"]<-"date"



fwrite(deces.2001_spread,"/fichier deces insee/décès travaillé/deces.2001_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2002 <- read_delim("/fichier deces insee/deces-2002.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2002<-left_join(deces.2002, table_passage_bis)


deces.2002$COM<-ifelse(!is.na(deces.2002$COM_AP),deces.2002$COM_AP,deces.2002$lieudeces)

deces.2002$datedeces <- as.character(deces.2002$datedeces)
deces.2002$datedeces <- as.Date(deces.2002$datedeces, format = "%Y%m%d")


deces.2002$datenaiss <- as.character(deces.2002$datenaiss)
deces.2002$datenaiss <- as.Date(deces.2002$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2002$age <- as.period(interval(deces.2002$datenaiss , deces.2002$datedeces ))
deces.2002$age_years <- year(deces.2002$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2002$sexe<-as.character(deces.2002$sexe)

deces.2002$SEX<-ifelse(deces.2002$sexe=="1","Homme","Femme")

deces.2002_final<-deces.2002[,c("COM","datedeces","age_years","SEX")]

deces.2002_final$nbr_mort<-1


deces.2002_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2002_final,sum)

library(tidyr)

deces.2002_spread <- deces.2002_final_ag %>%
  spread(SEX, nbr_mort)

deces.2002_spread$Femme[is.na(deces.2002_spread$Femme)]<-0
deces.2002_spread$Homme[is.na(deces.2002_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2002_final$tranche_age <- cut(deces.2002_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2002_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2002_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2002_final,sum)

library(tidyr)

deces.2002_spread2 <- deces.2002_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2002_spread2$`0-9`[is.na(deces.2002_spread2$`0-9`)]<-0
deces.2002_spread2$`10-19`[is.na(deces.2002_spread2$`10-19`)]<-0
deces.2002_spread2$`20-39`[is.na(deces.2002_spread2$`20-39`)]<-0
deces.2002_spread2$`40-59`[is.na(deces.2002_spread2$`40-59`)]<-0
deces.2002_spread2$`60-64`[is.na(deces.2002_spread2$`60-64`)]<-0
deces.2002_spread2$`65-69`[is.na(deces.2002_spread2$`65-69`)]<-0
deces.2002_spread2$`70-74`[is.na(deces.2002_spread2$`70-74`)]<-0
deces.2002_spread2$`75-79`[is.na(deces.2002_spread2$`75-79`)]<-0
deces.2002_spread2$`80+`[is.na(deces.2002_spread2$`80+`)]<-0


deces.2002_spread<-left_join(deces.2002_spread, deces.2002_spread2)


names(deces.2002_spread)[names(deces.2002_spread)=="datedeces"]<-"date"



fwrite(deces.2002_spread,"/fichier deces insee/décès travaillé/deces.2002_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2003 <- read_delim("/fichier deces insee/deces-2003.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2003<-left_join(deces.2003, table_passage_bis)


deces.2003$COM<-ifelse(!is.na(deces.2003$COM_AP),deces.2003$COM_AP,deces.2003$lieudeces)

deces.2003$datedeces <- as.character(deces.2003$datedeces)
deces.2003$datedeces <- as.Date(deces.2003$datedeces, format = "%Y%m%d")


deces.2003$datenaiss <- as.character(deces.2003$datenaiss)
deces.2003$datenaiss <- as.Date(deces.2003$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2003$age <- as.period(interval(deces.2003$datenaiss , deces.2003$datedeces ))
deces.2003$age_years <- year(deces.2003$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2003$sexe<-as.character(deces.2003$sexe)

deces.2003$SEX<-ifelse(deces.2003$sexe=="1","Homme","Femme")

deces.2003_final<-deces.2003[,c("COM","datedeces","age_years","SEX")]

deces.2003_final$nbr_mort<-1


deces.2003_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2003_final,sum)

library(tidyr)

deces.2003_spread <- deces.2003_final_ag %>%
  spread(SEX, nbr_mort)

deces.2003_spread$Femme[is.na(deces.2003_spread$Femme)]<-0
deces.2003_spread$Homme[is.na(deces.2003_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2003_final$tranche_age <- cut(deces.2003_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2003_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2003_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2003_final,sum)

library(tidyr)

deces.2003_spread2 <- deces.2003_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2003_spread2$`0-9`[is.na(deces.2003_spread2$`0-9`)]<-0
deces.2003_spread2$`10-19`[is.na(deces.2003_spread2$`10-19`)]<-0
deces.2003_spread2$`20-39`[is.na(deces.2003_spread2$`20-39`)]<-0
deces.2003_spread2$`40-59`[is.na(deces.2003_spread2$`40-59`)]<-0
deces.2003_spread2$`60-64`[is.na(deces.2003_spread2$`60-64`)]<-0
deces.2003_spread2$`65-69`[is.na(deces.2003_spread2$`65-69`)]<-0
deces.2003_spread2$`70-74`[is.na(deces.2003_spread2$`70-74`)]<-0
deces.2003_spread2$`75-79`[is.na(deces.2003_spread2$`75-79`)]<-0
deces.2003_spread2$`80+`[is.na(deces.2003_spread2$`80+`)]<-0


deces.2003_spread<-left_join(deces.2003_spread, deces.2003_spread2)


names(deces.2003_spread)[names(deces.2003_spread)=="datedeces"]<-"date"



fwrite(deces.2003_spread,"/fichier deces insee/décès travaillé/deces.2003_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2004 <- read_delim("/fichier deces insee/deces-2004.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2004<-left_join(deces.2004, table_passage_bis)


deces.2004$COM<-ifelse(!is.na(deces.2004$COM_AP),deces.2004$COM_AP,deces.2004$lieudeces)

deces.2004$datedeces <- as.character(deces.2004$datedeces)
deces.2004$datedeces <- as.Date(deces.2004$datedeces, format = "%Y%m%d")


deces.2004$datenaiss <- as.character(deces.2004$datenaiss)
deces.2004$datenaiss <- as.Date(deces.2004$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2004$age <- as.period(interval(deces.2004$datenaiss , deces.2004$datedeces ))
deces.2004$age_years <- year(deces.2004$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2004$sexe<-as.character(deces.2004$sexe)

deces.2004$SEX<-ifelse(deces.2004$sexe=="1","Homme","Femme")

deces.2004_final<-deces.2004[,c("COM","datedeces","age_years","SEX")]

deces.2004_final$nbr_mort<-1


deces.2004_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2004_final,sum)

library(tidyr)

deces.2004_spread <- deces.2004_final_ag %>%
  spread(SEX, nbr_mort)

deces.2004_spread$Femme[is.na(deces.2004_spread$Femme)]<-0
deces.2004_spread$Homme[is.na(deces.2004_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2004_final$tranche_age <- cut(deces.2004_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2004_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2004_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2004_final,sum)

library(tidyr)

deces.2004_spread2 <- deces.2004_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2004_spread2$`0-9`[is.na(deces.2004_spread2$`0-9`)]<-0
deces.2004_spread2$`10-19`[is.na(deces.2004_spread2$`10-19`)]<-0
deces.2004_spread2$`20-39`[is.na(deces.2004_spread2$`20-39`)]<-0
deces.2004_spread2$`40-59`[is.na(deces.2004_spread2$`40-59`)]<-0
deces.2004_spread2$`60-64`[is.na(deces.2004_spread2$`60-64`)]<-0
deces.2004_spread2$`65-69`[is.na(deces.2004_spread2$`65-69`)]<-0
deces.2004_spread2$`70-74`[is.na(deces.2004_spread2$`70-74`)]<-0
deces.2004_spread2$`75-79`[is.na(deces.2004_spread2$`75-79`)]<-0
deces.2004_spread2$`80+`[is.na(deces.2004_spread2$`80+`)]<-0


deces.2004_spread<-left_join(deces.2004_spread, deces.2004_spread2)


names(deces.2004_spread)[names(deces.2004_spread)=="datedeces"]<-"date"



fwrite(deces.2004_spread,"/fichier deces insee/décès travaillé/deces.2004_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2005 <- read_delim("/fichier deces insee/deces-2005.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2005<-left_join(deces.2005, table_passage_bis)


deces.2005$COM<-ifelse(!is.na(deces.2005$COM_AP),deces.2005$COM_AP,deces.2005$lieudeces)

deces.2005$datedeces <- as.character(deces.2005$datedeces)
deces.2005$datedeces <- as.Date(deces.2005$datedeces, format = "%Y%m%d")


deces.2005$datenaiss <- as.character(deces.2005$datenaiss)
deces.2005$datenaiss <- as.Date(deces.2005$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2005$age <- as.period(interval(deces.2005$datenaiss , deces.2005$datedeces ))
deces.2005$age_years <- year(deces.2005$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2005$sexe<-as.character(deces.2005$sexe)

deces.2005$SEX<-ifelse(deces.2005$sexe=="1","Homme","Femme")

deces.2005_final<-deces.2005[,c("COM","datedeces","age_years","SEX")]

deces.2005_final$nbr_mort<-1


deces.2005_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2005_final,sum)

library(tidyr)

deces.2005_spread <- deces.2005_final_ag %>%
  spread(SEX, nbr_mort)

deces.2005_spread$Femme[is.na(deces.2005_spread$Femme)]<-0
deces.2005_spread$Homme[is.na(deces.2005_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2005_final$tranche_age <- cut(deces.2005_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2005_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2005_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2005_final,sum)

library(tidyr)

deces.2005_spread2 <- deces.2005_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2005_spread2$`0-9`[is.na(deces.2005_spread2$`0-9`)]<-0
deces.2005_spread2$`10-19`[is.na(deces.2005_spread2$`10-19`)]<-0
deces.2005_spread2$`20-39`[is.na(deces.2005_spread2$`20-39`)]<-0
deces.2005_spread2$`40-59`[is.na(deces.2005_spread2$`40-59`)]<-0
deces.2005_spread2$`60-64`[is.na(deces.2005_spread2$`60-64`)]<-0
deces.2005_spread2$`65-69`[is.na(deces.2005_spread2$`65-69`)]<-0
deces.2005_spread2$`70-74`[is.na(deces.2005_spread2$`70-74`)]<-0
deces.2005_spread2$`75-79`[is.na(deces.2005_spread2$`75-79`)]<-0
deces.2005_spread2$`80+`[is.na(deces.2005_spread2$`80+`)]<-0


deces.2005_spread<-left_join(deces.2005_spread, deces.2005_spread2)


names(deces.2005_spread)[names(deces.2005_spread)=="datedeces"]<-"date"



fwrite(deces.2005_spread,"/fichier deces insee/décès travaillé/deces.2005_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2006 <- read_delim("/fichier deces insee/deces-2006.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2006<-left_join(deces.2006, table_passage_bis)


deces.2006$COM<-ifelse(!is.na(deces.2006$COM_AP),deces.2006$COM_AP,deces.2006$lieudeces)

deces.2006$datedeces <- as.character(deces.2006$datedeces)
deces.2006$datedeces <- as.Date(deces.2006$datedeces, format = "%Y%m%d")


deces.2006$datenaiss <- as.character(deces.2006$datenaiss)
deces.2006$datenaiss <- as.Date(deces.2006$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2006$age <- as.period(interval(deces.2006$datenaiss , deces.2006$datedeces ))
deces.2006$age_years <- year(deces.2006$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2006$sexe<-as.character(deces.2006$sexe)

deces.2006$SEX<-ifelse(deces.2006$sexe=="1","Homme","Femme")

deces.2006_final<-deces.2006[,c("COM","datedeces","age_years","SEX")]

deces.2006_final$nbr_mort<-1


deces.2006_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2006_final,sum)

library(tidyr)

deces.2006_spread <- deces.2006_final_ag %>%
  spread(SEX, nbr_mort)

deces.2006_spread$Femme[is.na(deces.2006_spread$Femme)]<-0
deces.2006_spread$Homme[is.na(deces.2006_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2006_final$tranche_age <- cut(deces.2006_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2006_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2006_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2006_final,sum)

library(tidyr)

deces.2006_spread2 <- deces.2006_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2006_spread2$`0-9`[is.na(deces.2006_spread2$`0-9`)]<-0
deces.2006_spread2$`10-19`[is.na(deces.2006_spread2$`10-19`)]<-0
deces.2006_spread2$`20-39`[is.na(deces.2006_spread2$`20-39`)]<-0
deces.2006_spread2$`40-59`[is.na(deces.2006_spread2$`40-59`)]<-0
deces.2006_spread2$`60-64`[is.na(deces.2006_spread2$`60-64`)]<-0
deces.2006_spread2$`65-69`[is.na(deces.2006_spread2$`65-69`)]<-0
deces.2006_spread2$`70-74`[is.na(deces.2006_spread2$`70-74`)]<-0
deces.2006_spread2$`75-79`[is.na(deces.2006_spread2$`75-79`)]<-0
deces.2006_spread2$`80+`[is.na(deces.2006_spread2$`80+`)]<-0


deces.2006_spread<-left_join(deces.2006_spread, deces.2006_spread2)


names(deces.2006_spread)[names(deces.2006_spread)=="datedeces"]<-"date"



fwrite(deces.2006_spread,"/fichier deces insee/décès travaillé/deces.2006_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2007 <- read_delim("/fichier deces insee/deces-2007.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2007<-left_join(deces.2007, table_passage_bis)


deces.2007$COM<-ifelse(!is.na(deces.2007$COM_AP),deces.2007$COM_AP,deces.2007$lieudeces)

deces.2007$datedeces <- as.character(deces.2007$datedeces)
deces.2007$datedeces <- as.Date(deces.2007$datedeces, format = "%Y%m%d")


deces.2007$datenaiss <- as.character(deces.2007$datenaiss)
deces.2007$datenaiss <- as.Date(deces.2007$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2007$age <- as.period(interval(deces.2007$datenaiss , deces.2007$datedeces ))
deces.2007$age_years <- year(deces.2007$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2007$sexe<-as.character(deces.2007$sexe)

deces.2007$SEX<-ifelse(deces.2007$sexe=="1","Homme","Femme")

deces.2007_final<-deces.2007[,c("COM","datedeces","age_years","SEX")]

deces.2007_final$nbr_mort<-1


deces.2007_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2007_final,sum)

library(tidyr)

deces.2007_spread <- deces.2007_final_ag %>%
  spread(SEX, nbr_mort)

deces.2007_spread$Femme[is.na(deces.2007_spread$Femme)]<-0
deces.2007_spread$Homme[is.na(deces.2007_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2007_final$tranche_age <- cut(deces.2007_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2007_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2007_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2007_final,sum)

library(tidyr)

deces.2007_spread2 <- deces.2007_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2007_spread2$`0-9`[is.na(deces.2007_spread2$`0-9`)]<-0
deces.2007_spread2$`10-19`[is.na(deces.2007_spread2$`10-19`)]<-0
deces.2007_spread2$`20-39`[is.na(deces.2007_spread2$`20-39`)]<-0
deces.2007_spread2$`40-59`[is.na(deces.2007_spread2$`40-59`)]<-0
deces.2007_spread2$`60-64`[is.na(deces.2007_spread2$`60-64`)]<-0
deces.2007_spread2$`65-69`[is.na(deces.2007_spread2$`65-69`)]<-0
deces.2007_spread2$`70-74`[is.na(deces.2007_spread2$`70-74`)]<-0
deces.2007_spread2$`75-79`[is.na(deces.2007_spread2$`75-79`)]<-0
deces.2007_spread2$`80+`[is.na(deces.2007_spread2$`80+`)]<-0


deces.2007_spread<-left_join(deces.2007_spread, deces.2007_spread2)


names(deces.2007_spread)[names(deces.2007_spread)=="datedeces"]<-"date"



fwrite(deces.2007_spread,"/fichier deces insee/décès travaillé/deces.2007_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2008 <- read_delim("/fichier deces insee/deces-2008.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2008<-left_join(deces.2008, table_passage_bis)


deces.2008$COM<-ifelse(!is.na(deces.2008$COM_AP),deces.2008$COM_AP,deces.2008$lieudeces)

deces.2008$datedeces <- as.character(deces.2008$datedeces)
deces.2008$datedeces <- as.Date(deces.2008$datedeces, format = "%Y%m%d")


deces.2008$datenaiss <- as.character(deces.2008$datenaiss)
deces.2008$datenaiss <- as.Date(deces.2008$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2008$age <- as.period(interval(deces.2008$datenaiss , deces.2008$datedeces ))
deces.2008$age_years <- year(deces.2008$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2008$sexe<-as.character(deces.2008$sexe)

deces.2008$SEX<-ifelse(deces.2008$sexe=="1","Homme","Femme")

deces.2008_final<-deces.2008[,c("COM","datedeces","age_years","SEX")]

deces.2008_final$nbr_mort<-1


deces.2008_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2008_final,sum)

library(tidyr)

deces.2008_spread <- deces.2008_final_ag %>%
  spread(SEX, nbr_mort)

deces.2008_spread$Femme[is.na(deces.2008_spread$Femme)]<-0
deces.2008_spread$Homme[is.na(deces.2008_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2008_final$tranche_age <- cut(deces.2008_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2008_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2008_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2008_final,sum)

library(tidyr)

deces.2008_spread2 <- deces.2008_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2008_spread2$`0-9`[is.na(deces.2008_spread2$`0-9`)]<-0
deces.2008_spread2$`10-19`[is.na(deces.2008_spread2$`10-19`)]<-0
deces.2008_spread2$`20-39`[is.na(deces.2008_spread2$`20-39`)]<-0
deces.2008_spread2$`40-59`[is.na(deces.2008_spread2$`40-59`)]<-0
deces.2008_spread2$`60-64`[is.na(deces.2008_spread2$`60-64`)]<-0
deces.2008_spread2$`65-69`[is.na(deces.2008_spread2$`65-69`)]<-0
deces.2008_spread2$`70-74`[is.na(deces.2008_spread2$`70-74`)]<-0
deces.2008_spread2$`75-79`[is.na(deces.2008_spread2$`75-79`)]<-0
deces.2008_spread2$`80+`[is.na(deces.2008_spread2$`80+`)]<-0


deces.2008_spread<-left_join(deces.2008_spread, deces.2008_spread2)


names(deces.2008_spread)[names(deces.2008_spread)=="datedeces"]<-"date"



fwrite(deces.2008_spread,"/fichier deces insee/décès travaillé/deces.2008_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2009 <- read_delim("/fichier deces insee/deces-2009.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2009<-left_join(deces.2009, table_passage_bis)


deces.2009$COM<-ifelse(!is.na(deces.2009$COM_AP),deces.2009$COM_AP,deces.2009$lieudeces)

deces.2009$datedeces <- as.character(deces.2009$datedeces)
deces.2009$datedeces <- as.Date(deces.2009$datedeces, format = "%Y%m%d")


deces.2009$datenaiss <- as.character(deces.2009$datenaiss)
deces.2009$datenaiss <- as.Date(deces.2009$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2009$age <- as.period(interval(deces.2009$datenaiss , deces.2009$datedeces ))
deces.2009$age_years <- year(deces.2009$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2009$sexe<-as.character(deces.2009$sexe)

deces.2009$SEX<-ifelse(deces.2009$sexe=="1","Homme","Femme")

deces.2009_final<-deces.2009[,c("COM","datedeces","age_years","SEX")]

deces.2009_final$nbr_mort<-1


deces.2009_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2009_final,sum)

library(tidyr)

deces.2009_spread <- deces.2009_final_ag %>%
  spread(SEX, nbr_mort)

deces.2009_spread$Femme[is.na(deces.2009_spread$Femme)]<-0
deces.2009_spread$Homme[is.na(deces.2009_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2009_final$tranche_age <- cut(deces.2009_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2009_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2009_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2009_final,sum)

library(tidyr)

deces.2009_spread2 <- deces.2009_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2009_spread2$`0-9`[is.na(deces.2009_spread2$`0-9`)]<-0
deces.2009_spread2$`10-19`[is.na(deces.2009_spread2$`10-19`)]<-0
deces.2009_spread2$`20-39`[is.na(deces.2009_spread2$`20-39`)]<-0
deces.2009_spread2$`40-59`[is.na(deces.2009_spread2$`40-59`)]<-0
deces.2009_spread2$`60-64`[is.na(deces.2009_spread2$`60-64`)]<-0
deces.2009_spread2$`65-69`[is.na(deces.2009_spread2$`65-69`)]<-0
deces.2009_spread2$`70-74`[is.na(deces.2009_spread2$`70-74`)]<-0
deces.2009_spread2$`75-79`[is.na(deces.2009_spread2$`75-79`)]<-0
deces.2009_spread2$`80+`[is.na(deces.2009_spread2$`80+`)]<-0


deces.2009_spread<-left_join(deces.2009_spread, deces.2009_spread2)


names(deces.2009_spread)[names(deces.2009_spread)=="datedeces"]<-"date"



fwrite(deces.2009_spread,"/fichier deces insee/décès travaillé/deces.2009_age_sexe.csv")





##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2010 <- read_delim("/fichier deces insee/Deces_2010.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2010<-left_join(deces.2010, table_passage_bis)


deces.2010$COM<-ifelse(!is.na(deces.2010$COM_AP),deces.2010$COM_AP,deces.2010$lieudeces)

deces.2010$datedeces <- as.character(deces.2010$datedeces)
deces.2010$datedeces <- as.Date(deces.2010$datedeces, format = "%Y%m%d")


deces.2010$datenaiss <- as.character(deces.2010$datenaiss)
deces.2010$datenaiss <- as.Date(deces.2010$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2010$age <- as.period(interval(deces.2010$datenaiss , deces.2010$datedeces ))
deces.2010$age_years <- year(deces.2010$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2010$sexe<-as.character(deces.2010$sexe)

deces.2010$SEX<-ifelse(deces.2010$sexe=="1","Homme","Femme")

deces.2010_final<-deces.2010[,c("COM","datedeces","age_years","SEX")]

deces.2010_final$nbr_mort<-1


deces.2010_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2010_final,sum)

library(tidyr)

deces.2010_spread <- deces.2010_final_ag %>%
  spread(SEX, nbr_mort)

deces.2010_spread$Femme[is.na(deces.2010_spread$Femme)]<-0
deces.2010_spread$Homme[is.na(deces.2010_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2010_final$tranche_age <- cut(deces.2010_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2010_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2010_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2010_final,sum)

library(tidyr)

deces.2010_spread2 <- deces.2010_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2010_spread2$`0-9`[is.na(deces.2010_spread2$`0-9`)]<-0
deces.2010_spread2$`10-19`[is.na(deces.2010_spread2$`10-19`)]<-0
deces.2010_spread2$`20-39`[is.na(deces.2010_spread2$`20-39`)]<-0
deces.2010_spread2$`40-59`[is.na(deces.2010_spread2$`40-59`)]<-0
deces.2010_spread2$`60-64`[is.na(deces.2010_spread2$`60-64`)]<-0
deces.2010_spread2$`65-69`[is.na(deces.2010_spread2$`65-69`)]<-0
deces.2010_spread2$`70-74`[is.na(deces.2010_spread2$`70-74`)]<-0
deces.2010_spread2$`75-79`[is.na(deces.2010_spread2$`75-79`)]<-0
deces.2010_spread2$`80+`[is.na(deces.2010_spread2$`80+`)]<-0


deces.2010_spread<-left_join(deces.2010_spread, deces.2010_spread2)


names(deces.2010_spread)[names(deces.2010_spread)=="datedeces"]<-"date"



fwrite(deces.2010_spread,"/fichier deces insee/décès travaillé/deces.2010_age_sexe.csv")





##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2011 <- read_delim("/fichier deces insee/Deces_2011.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2011<-left_join(deces.2011, table_passage_bis)


deces.2011$COM<-ifelse(!is.na(deces.2011$COM_AP),deces.2011$COM_AP,deces.2011$lieudeces)

deces.2011$datedeces <- as.character(deces.2011$datedeces)
deces.2011$datedeces <- as.Date(deces.2011$datedeces, format = "%Y%m%d")


deces.2011$datenaiss <- as.character(deces.2011$datenaiss)
deces.2011$datenaiss <- as.Date(deces.2011$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2011$age <- as.period(interval(deces.2011$datenaiss , deces.2011$datedeces ))
deces.2011$age_years <- year(deces.2011$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2011$sexe<-as.character(deces.2011$sexe)

deces.2011$SEX<-ifelse(deces.2011$sexe=="1","Homme","Femme")

deces.2011_final<-deces.2011[,c("COM","datedeces","age_years","SEX")]

deces.2011_final$nbr_mort<-1


deces.2011_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2011_final,sum)

library(tidyr)

deces.2011_spread <- deces.2011_final_ag %>%
  spread(SEX, nbr_mort)

deces.2011_spread$Femme[is.na(deces.2011_spread$Femme)]<-0
deces.2011_spread$Homme[is.na(deces.2011_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2011_final$tranche_age <- cut(deces.2011_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2011_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2011_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2011_final,sum)

library(tidyr)

deces.2011_spread2 <- deces.2011_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2011_spread2$`0-9`[is.na(deces.2011_spread2$`0-9`)]<-0
deces.2011_spread2$`10-19`[is.na(deces.2011_spread2$`10-19`)]<-0
deces.2011_spread2$`20-39`[is.na(deces.2011_spread2$`20-39`)]<-0
deces.2011_spread2$`40-59`[is.na(deces.2011_spread2$`40-59`)]<-0
deces.2011_spread2$`60-64`[is.na(deces.2011_spread2$`60-64`)]<-0
deces.2011_spread2$`65-69`[is.na(deces.2011_spread2$`65-69`)]<-0
deces.2011_spread2$`70-74`[is.na(deces.2011_spread2$`70-74`)]<-0
deces.2011_spread2$`75-79`[is.na(deces.2011_spread2$`75-79`)]<-0
deces.2011_spread2$`80+`[is.na(deces.2011_spread2$`80+`)]<-0


deces.2011_spread<-left_join(deces.2011_spread, deces.2011_spread2)


names(deces.2011_spread)[names(deces.2011_spread)=="datedeces"]<-"date"



fwrite(deces.2011_spread,"/fichier deces insee/décès travaillé/deces.2011_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2012 <- read_delim("/fichier deces insee/Deces_2012.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2012<-left_join(deces.2012, table_passage_bis)


deces.2012$COM<-ifelse(!is.na(deces.2012$COM_AP),deces.2012$COM_AP,deces.2012$lieudeces)

deces.2012$datedeces <- as.character(deces.2012$datedeces)
deces.2012$datedeces <- as.Date(deces.2012$datedeces, format = "%Y%m%d")


deces.2012$datenaiss <- as.character(deces.2012$datenaiss)
deces.2012$datenaiss <- as.Date(deces.2012$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2012$age <- as.period(interval(deces.2012$datenaiss , deces.2012$datedeces ))
deces.2012$age_years <- year(deces.2012$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2012$sexe<-as.character(deces.2012$sexe)

deces.2012$SEX<-ifelse(deces.2012$sexe=="1","Homme","Femme")

deces.2012_final<-deces.2012[,c("COM","datedeces","age_years","SEX")]

deces.2012_final$nbr_mort<-1


deces.2012_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2012_final,sum)

library(tidyr)

deces.2012_spread <- deces.2012_final_ag %>%
  spread(SEX, nbr_mort)

deces.2012_spread$Femme[is.na(deces.2012_spread$Femme)]<-0
deces.2012_spread$Homme[is.na(deces.2012_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2012_final$tranche_age <- cut(deces.2012_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2012_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2012_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2012_final,sum)

library(tidyr)

deces.2012_spread2 <- deces.2012_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2012_spread2$`0-9`[is.na(deces.2012_spread2$`0-9`)]<-0
deces.2012_spread2$`10-19`[is.na(deces.2012_spread2$`10-19`)]<-0
deces.2012_spread2$`20-39`[is.na(deces.2012_spread2$`20-39`)]<-0
deces.2012_spread2$`40-59`[is.na(deces.2012_spread2$`40-59`)]<-0
deces.2012_spread2$`60-64`[is.na(deces.2012_spread2$`60-64`)]<-0
deces.2012_spread2$`65-69`[is.na(deces.2012_spread2$`65-69`)]<-0
deces.2012_spread2$`70-74`[is.na(deces.2012_spread2$`70-74`)]<-0
deces.2012_spread2$`75-79`[is.na(deces.2012_spread2$`75-79`)]<-0
deces.2012_spread2$`80+`[is.na(deces.2012_spread2$`80+`)]<-0


deces.2012_spread<-left_join(deces.2012_spread, deces.2012_spread2)


names(deces.2012_spread)[names(deces.2012_spread)=="datedeces"]<-"date"



fwrite(deces.2012_spread,"/fichier deces insee/décès travaillé/deces.2012_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2013 <- read_delim("/fichier deces insee/Deces_2013.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2013<-left_join(deces.2013, table_passage_bis)


deces.2013$COM<-ifelse(!is.na(deces.2013$COM_AP),deces.2013$COM_AP,deces.2013$lieudeces)

deces.2013$datedeces <- as.character(deces.2013$datedeces)
deces.2013$datedeces <- as.Date(deces.2013$datedeces, format = "%Y%m%d")


deces.2013$datenaiss <- as.character(deces.2013$datenaiss)
deces.2013$datenaiss <- as.Date(deces.2013$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2013$age <- as.period(interval(deces.2013$datenaiss , deces.2013$datedeces ))
deces.2013$age_years <- year(deces.2013$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2013$sexe<-as.character(deces.2013$sexe)

deces.2013$SEX<-ifelse(deces.2013$sexe=="1","Homme","Femme")

deces.2013_final<-deces.2013[,c("COM","datedeces","age_years","SEX")]

deces.2013_final$nbr_mort<-1


deces.2013_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2013_final,sum)

library(tidyr)

deces.2013_spread <- deces.2013_final_ag %>%
  spread(SEX, nbr_mort)

deces.2013_spread$Femme[is.na(deces.2013_spread$Femme)]<-0
deces.2013_spread$Homme[is.na(deces.2013_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2013_final$tranche_age <- cut(deces.2013_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2013_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2013_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2013_final,sum)

library(tidyr)

deces.2013_spread2 <- deces.2013_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2013_spread2$`0-9`[is.na(deces.2013_spread2$`0-9`)]<-0
deces.2013_spread2$`10-19`[is.na(deces.2013_spread2$`10-19`)]<-0
deces.2013_spread2$`20-39`[is.na(deces.2013_spread2$`20-39`)]<-0
deces.2013_spread2$`40-59`[is.na(deces.2013_spread2$`40-59`)]<-0
deces.2013_spread2$`60-64`[is.na(deces.2013_spread2$`60-64`)]<-0
deces.2013_spread2$`65-69`[is.na(deces.2013_spread2$`65-69`)]<-0
deces.2013_spread2$`70-74`[is.na(deces.2013_spread2$`70-74`)]<-0
deces.2013_spread2$`75-79`[is.na(deces.2013_spread2$`75-79`)]<-0
deces.2013_spread2$`80+`[is.na(deces.2013_spread2$`80+`)]<-0


deces.2013_spread<-left_join(deces.2013_spread, deces.2013_spread2)


names(deces.2013_spread)[names(deces.2013_spread)=="datedeces"]<-"date"



fwrite(deces.2013_spread,"/fichier deces insee/décès travaillé/deces.2013_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2014 <- read_delim("/fichier deces insee/Deces_2014.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2014<-left_join(deces.2014, table_passage_bis)


deces.2014$COM<-ifelse(!is.na(deces.2014$COM_AP),deces.2014$COM_AP,deces.2014$lieudeces)

deces.2014$datedeces <- as.character(deces.2014$datedeces)
deces.2014$datedeces <- as.Date(deces.2014$datedeces, format = "%Y%m%d")


deces.2014$datenaiss <- as.character(deces.2014$datenaiss)
deces.2014$datenaiss <- as.Date(deces.2014$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2014$age <- as.period(interval(deces.2014$datenaiss , deces.2014$datedeces ))
deces.2014$age_years <- year(deces.2014$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2014$sexe<-as.character(deces.2014$sexe)

deces.2014$SEX<-ifelse(deces.2014$sexe=="1","Homme","Femme")

deces.2014_final<-deces.2014[,c("COM","datedeces","age_years","SEX")]

deces.2014_final$nbr_mort<-1


deces.2014_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2014_final,sum)

library(tidyr)

deces.2014_spread <- deces.2014_final_ag %>%
  spread(SEX, nbr_mort)

deces.2014_spread$Femme[is.na(deces.2014_spread$Femme)]<-0
deces.2014_spread$Homme[is.na(deces.2014_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2014_final$tranche_age <- cut(deces.2014_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2014_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2014_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2014_final,sum)

library(tidyr)

deces.2014_spread2 <- deces.2014_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2014_spread2$`0-9`[is.na(deces.2014_spread2$`0-9`)]<-0
deces.2014_spread2$`10-19`[is.na(deces.2014_spread2$`10-19`)]<-0
deces.2014_spread2$`20-39`[is.na(deces.2014_spread2$`20-39`)]<-0
deces.2014_spread2$`40-59`[is.na(deces.2014_spread2$`40-59`)]<-0
deces.2014_spread2$`60-64`[is.na(deces.2014_spread2$`60-64`)]<-0
deces.2014_spread2$`65-69`[is.na(deces.2014_spread2$`65-69`)]<-0
deces.2014_spread2$`70-74`[is.na(deces.2014_spread2$`70-74`)]<-0
deces.2014_spread2$`75-79`[is.na(deces.2014_spread2$`75-79`)]<-0
deces.2014_spread2$`80+`[is.na(deces.2014_spread2$`80+`)]<-0


deces.2014_spread<-left_join(deces.2014_spread, deces.2014_spread2)


names(deces.2014_spread)[names(deces.2014_spread)=="datedeces"]<-"date"



fwrite(deces.2014_spread,"/fichier deces insee/décès travaillé/deces.2014_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2015 <- read_delim("/fichier deces insee/Deces_2015.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2015<-left_join(deces.2015, table_passage_bis)


deces.2015$COM<-ifelse(!is.na(deces.2015$COM_AP),deces.2015$COM_AP,deces.2015$lieudeces)

deces.2015$datedeces <- as.character(deces.2015$datedeces)
deces.2015$datedeces <- as.Date(deces.2015$datedeces, format = "%Y%m%d")


deces.2015$datenaiss <- as.character(deces.2015$datenaiss)
deces.2015$datenaiss <- as.Date(deces.2015$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2015$age <- as.period(interval(deces.2015$datenaiss , deces.2015$datedeces ))
deces.2015$age_years <- year(deces.2015$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2015$sexe<-as.character(deces.2015$sexe)

deces.2015$SEX<-ifelse(deces.2015$sexe=="1","Homme","Femme")

deces.2015_final<-deces.2015[,c("COM","datedeces","age_years","SEX")]

deces.2015_final$nbr_mort<-1


deces.2015_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2015_final,sum)

library(tidyr)

deces.2015_spread <- deces.2015_final_ag %>%
  spread(SEX, nbr_mort)

deces.2015_spread$Femme[is.na(deces.2015_spread$Femme)]<-0
deces.2015_spread$Homme[is.na(deces.2015_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2015_final$tranche_age <- cut(deces.2015_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2015_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2015_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2015_final,sum)

library(tidyr)

deces.2015_spread2 <- deces.2015_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2015_spread2$`0-9`[is.na(deces.2015_spread2$`0-9`)]<-0
deces.2015_spread2$`10-19`[is.na(deces.2015_spread2$`10-19`)]<-0
deces.2015_spread2$`20-39`[is.na(deces.2015_spread2$`20-39`)]<-0
deces.2015_spread2$`40-59`[is.na(deces.2015_spread2$`40-59`)]<-0
deces.2015_spread2$`60-64`[is.na(deces.2015_spread2$`60-64`)]<-0
deces.2015_spread2$`65-69`[is.na(deces.2015_spread2$`65-69`)]<-0
deces.2015_spread2$`70-74`[is.na(deces.2015_spread2$`70-74`)]<-0
deces.2015_spread2$`75-79`[is.na(deces.2015_spread2$`75-79`)]<-0
deces.2015_spread2$`80+`[is.na(deces.2015_spread2$`80+`)]<-0


deces.2015_spread<-left_join(deces.2015_spread, deces.2015_spread2)


names(deces.2015_spread)[names(deces.2015_spread)=="datedeces"]<-"date"



fwrite(deces.2015_spread,"/fichier deces insee/décès travaillé/deces.2015_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2016 <- read_delim("/fichier deces insee/Deces_2016.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2016<-left_join(deces.2016, table_passage_bis)


deces.2016$COM<-ifelse(!is.na(deces.2016$COM_AP),deces.2016$COM_AP,deces.2016$lieudeces)

deces.2016$datedeces <- as.character(deces.2016$datedeces)
deces.2016$datedeces <- as.Date(deces.2016$datedeces, format = "%Y%m%d")


deces.2016$datenaiss <- as.character(deces.2016$datenaiss)
deces.2016$datenaiss <- as.Date(deces.2016$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2016$age <- as.period(interval(deces.2016$datenaiss , deces.2016$datedeces ))
deces.2016$age_years <- year(deces.2016$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2016$sexe<-as.character(deces.2016$sexe)

deces.2016$SEX<-ifelse(deces.2016$sexe=="1","Homme","Femme")

deces.2016_final<-deces.2016[,c("COM","datedeces","age_years","SEX")]

deces.2016_final$nbr_mort<-1


deces.2016_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2016_final,sum)

library(tidyr)

deces.2016_spread <- deces.2016_final_ag %>%
  spread(SEX, nbr_mort)

deces.2016_spread$Femme[is.na(deces.2016_spread$Femme)]<-0
deces.2016_spread$Homme[is.na(deces.2016_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2016_final$tranche_age <- cut(deces.2016_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2016_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2016_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2016_final,sum)

library(tidyr)

deces.2016_spread2 <- deces.2016_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2016_spread2$`0-9`[is.na(deces.2016_spread2$`0-9`)]<-0
deces.2016_spread2$`10-19`[is.na(deces.2016_spread2$`10-19`)]<-0
deces.2016_spread2$`20-39`[is.na(deces.2016_spread2$`20-39`)]<-0
deces.2016_spread2$`40-59`[is.na(deces.2016_spread2$`40-59`)]<-0
deces.2016_spread2$`60-64`[is.na(deces.2016_spread2$`60-64`)]<-0
deces.2016_spread2$`65-69`[is.na(deces.2016_spread2$`65-69`)]<-0
deces.2016_spread2$`70-74`[is.na(deces.2016_spread2$`70-74`)]<-0
deces.2016_spread2$`75-79`[is.na(deces.2016_spread2$`75-79`)]<-0
deces.2016_spread2$`80+`[is.na(deces.2016_spread2$`80+`)]<-0


deces.2016_spread<-left_join(deces.2016_spread, deces.2016_spread2)


names(deces.2016_spread)[names(deces.2016_spread)=="datedeces"]<-"date"



fwrite(deces.2016_spread,"/fichier deces insee/décès travaillé/deces.2016_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2017 <- read_delim("/fichier deces insee/Deces_2017.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2017<-left_join(deces.2017, table_passage_bis)


deces.2017$COM<-ifelse(!is.na(deces.2017$COM_AP),deces.2017$COM_AP,deces.2017$lieudeces)

deces.2017$datedeces <- as.character(deces.2017$datedeces)
deces.2017$datedeces <- as.Date(deces.2017$datedeces, format = "%Y%m%d")


deces.2017$datenaiss <- as.character(deces.2017$datenaiss)
deces.2017$datenaiss <- as.Date(deces.2017$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2017$age <- as.period(interval(deces.2017$datenaiss , deces.2017$datedeces ))
deces.2017$age_years <- year(deces.2017$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2017$sexe<-as.character(deces.2017$sexe)

deces.2017$SEX<-ifelse(deces.2017$sexe=="1","Homme","Femme")

deces.2017_final<-deces.2017[,c("COM","datedeces","age_years","SEX")]

deces.2017_final$nbr_mort<-1


deces.2017_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2017_final,sum)

library(tidyr)

deces.2017_spread <- deces.2017_final_ag %>%
  spread(SEX, nbr_mort)

deces.2017_spread$Femme[is.na(deces.2017_spread$Femme)]<-0
deces.2017_spread$Homme[is.na(deces.2017_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2017_final$tranche_age <- cut(deces.2017_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2017_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2017_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2017_final,sum)

library(tidyr)

deces.2017_spread2 <- deces.2017_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2017_spread2$`0-9`[is.na(deces.2017_spread2$`0-9`)]<-0
deces.2017_spread2$`10-19`[is.na(deces.2017_spread2$`10-19`)]<-0
deces.2017_spread2$`20-39`[is.na(deces.2017_spread2$`20-39`)]<-0
deces.2017_spread2$`40-59`[is.na(deces.2017_spread2$`40-59`)]<-0
deces.2017_spread2$`60-64`[is.na(deces.2017_spread2$`60-64`)]<-0
deces.2017_spread2$`65-69`[is.na(deces.2017_spread2$`65-69`)]<-0
deces.2017_spread2$`70-74`[is.na(deces.2017_spread2$`70-74`)]<-0
deces.2017_spread2$`75-79`[is.na(deces.2017_spread2$`75-79`)]<-0
deces.2017_spread2$`80+`[is.na(deces.2017_spread2$`80+`)]<-0


deces.2017_spread<-left_join(deces.2017_spread, deces.2017_spread2)


names(deces.2017_spread)[names(deces.2017_spread)=="datedeces"]<-"date"



fwrite(deces.2017_spread,"/fichier deces insee/décès travaillé/deces.2017_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2018 <- read_delim("/fichier deces insee/Deces_2018.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2018<-left_join(deces.2018, table_passage_bis)


deces.2018$COM<-ifelse(!is.na(deces.2018$COM_AP),deces.2018$COM_AP,deces.2018$lieudeces)

deces.2018$datedeces <- as.character(deces.2018$datedeces)
deces.2018$datedeces <- as.Date(deces.2018$datedeces, format = "%Y%m%d")


deces.2018$datenaiss <- as.character(deces.2018$datenaiss)
deces.2018$datenaiss <- as.Date(deces.2018$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2018$age <- as.period(interval(deces.2018$datenaiss , deces.2018$datedeces ))
deces.2018$age_years <- year(deces.2018$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2018$sexe<-as.character(deces.2018$sexe)

deces.2018$SEX<-ifelse(deces.2018$sexe=="1","Homme","Femme")

deces.2018_final<-deces.2018[,c("COM","datedeces","age_years","SEX")]

deces.2018_final$nbr_mort<-1


deces.2018_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2018_final,sum)

library(tidyr)

deces.2018_spread <- deces.2018_final_ag %>%
  spread(SEX, nbr_mort)

deces.2018_spread$Femme[is.na(deces.2018_spread$Femme)]<-0
deces.2018_spread$Homme[is.na(deces.2018_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2018_final$tranche_age <- cut(deces.2018_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2018_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2018_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2018_final,sum)

library(tidyr)

deces.2018_spread2 <- deces.2018_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2018_spread2$`0-9`[is.na(deces.2018_spread2$`0-9`)]<-0
deces.2018_spread2$`10-19`[is.na(deces.2018_spread2$`10-19`)]<-0
deces.2018_spread2$`20-39`[is.na(deces.2018_spread2$`20-39`)]<-0
deces.2018_spread2$`40-59`[is.na(deces.2018_spread2$`40-59`)]<-0
deces.2018_spread2$`60-64`[is.na(deces.2018_spread2$`60-64`)]<-0
deces.2018_spread2$`65-69`[is.na(deces.2018_spread2$`65-69`)]<-0
deces.2018_spread2$`70-74`[is.na(deces.2018_spread2$`70-74`)]<-0
deces.2018_spread2$`75-79`[is.na(deces.2018_spread2$`75-79`)]<-0
deces.2018_spread2$`80+`[is.na(deces.2018_spread2$`80+`)]<-0


deces.2018_spread<-left_join(deces.2018_spread, deces.2018_spread2)


names(deces.2018_spread)[names(deces.2018_spread)=="datedeces"]<-"date"



fwrite(deces.2018_spread,"/fichier deces insee/décès travaillé/deces.2018_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2019 <- read_delim("/fichier deces insee/Deces_2019.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2019<-left_join(deces.2019, table_passage_bis)


deces.2019$COM<-ifelse(!is.na(deces.2019$COM_AP),deces.2019$COM_AP,deces.2019$lieudeces)

deces.2019$datedeces <- as.character(deces.2019$datedeces)
deces.2019$datedeces <- as.Date(deces.2019$datedeces, format = "%Y%m%d")


deces.2019$datenaiss <- as.character(deces.2019$datenaiss)
deces.2019$datenaiss <- as.Date(deces.2019$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2019$age <- as.period(interval(deces.2019$datenaiss , deces.2019$datedeces ))
deces.2019$age_years <- year(deces.2019$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2019$sexe<-as.character(deces.2019$sexe)

deces.2019$SEX<-ifelse(deces.2019$sexe=="1","Homme","Femme")

deces.2019_final<-deces.2019[,c("COM","datedeces","age_years","SEX")]

deces.2019_final$nbr_mort<-1


deces.2019_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2019_final,sum)

library(tidyr)

deces.2019_spread <- deces.2019_final_ag %>%
  spread(SEX, nbr_mort)

deces.2019_spread$Femme[is.na(deces.2019_spread$Femme)]<-0
deces.2019_spread$Homme[is.na(deces.2019_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2019_final$tranche_age <- cut(deces.2019_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2019_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2019_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2019_final,sum)

library(tidyr)

deces.2019_spread2 <- deces.2019_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2019_spread2$`0-9`[is.na(deces.2019_spread2$`0-9`)]<-0
deces.2019_spread2$`10-19`[is.na(deces.2019_spread2$`10-19`)]<-0
deces.2019_spread2$`20-39`[is.na(deces.2019_spread2$`20-39`)]<-0
deces.2019_spread2$`40-59`[is.na(deces.2019_spread2$`40-59`)]<-0
deces.2019_spread2$`60-64`[is.na(deces.2019_spread2$`60-64`)]<-0
deces.2019_spread2$`65-69`[is.na(deces.2019_spread2$`65-69`)]<-0
deces.2019_spread2$`70-74`[is.na(deces.2019_spread2$`70-74`)]<-0
deces.2019_spread2$`75-79`[is.na(deces.2019_spread2$`75-79`)]<-0
deces.2019_spread2$`80+`[is.na(deces.2019_spread2$`80+`)]<-0


deces.2019_spread<-left_join(deces.2019_spread, deces.2019_spread2)


names(deces.2019_spread)[names(deces.2019_spread)=="datedeces"]<-"date"



fwrite(deces.2019_spread,"/fichier deces insee/décès travaillé/deces.2019_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2020 <- read_delim("/fichier deces insee/Deces_2020.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2020<-left_join(deces.2020, table_passage_bis)


deces.2020$COM<-ifelse(!is.na(deces.2020$COM_AP),deces.2020$COM_AP,deces.2020$lieudeces)

deces.2020$datedeces <- as.character(deces.2020$datedeces)
deces.2020$datedeces <- as.Date(deces.2020$datedeces, format = "%Y%m%d")


deces.2020$datenaiss <- as.character(deces.2020$datenaiss)
deces.2020$datenaiss <- as.Date(deces.2020$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2020$age <- as.period(interval(deces.2020$datenaiss , deces.2020$datedeces ))
deces.2020$age_years <- year(deces.2020$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2020$sexe<-as.character(deces.2020$sexe)

deces.2020$SEX<-ifelse(deces.2020$sexe=="1","Homme","Femme")

deces.2020_final<-deces.2020[,c("COM","datedeces","age_years","SEX")]

deces.2020_final$nbr_mort<-1


deces.2020_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2020_final,sum)

library(tidyr)

deces.2020_spread <- deces.2020_final_ag %>%
  spread(SEX, nbr_mort)

deces.2020_spread$Femme[is.na(deces.2020_spread$Femme)]<-0
deces.2020_spread$Homme[is.na(deces.2020_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2020_final$tranche_age <- cut(deces.2020_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2020_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2020_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2020_final,sum)

library(tidyr)

deces.2020_spread2 <- deces.2020_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2020_spread2$`0-9`[is.na(deces.2020_spread2$`0-9`)]<-0
deces.2020_spread2$`10-19`[is.na(deces.2020_spread2$`10-19`)]<-0
deces.2020_spread2$`20-39`[is.na(deces.2020_spread2$`20-39`)]<-0
deces.2020_spread2$`40-59`[is.na(deces.2020_spread2$`40-59`)]<-0
deces.2020_spread2$`60-64`[is.na(deces.2020_spread2$`60-64`)]<-0
deces.2020_spread2$`65-69`[is.na(deces.2020_spread2$`65-69`)]<-0
deces.2020_spread2$`70-74`[is.na(deces.2020_spread2$`70-74`)]<-0
deces.2020_spread2$`75-79`[is.na(deces.2020_spread2$`75-79`)]<-0
deces.2020_spread2$`80+`[is.na(deces.2020_spread2$`80+`)]<-0


deces.2020_spread<-left_join(deces.2020_spread, deces.2020_spread2)


names(deces.2020_spread)[names(deces.2020_spread)=="datedeces"]<-"date"



fwrite(deces.2020_spread,"/fichier deces insee/décès travaillé/deces.2020_age_sexe.csv")




##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2021 <- read_delim("/fichier deces insee/Deces_2021.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2021<-left_join(deces.2021, table_passage_bis)


deces.2021$COM<-ifelse(!is.na(deces.2021$COM_AP),deces.2021$COM_AP,deces.2021$lieudeces)

deces.2021$datedeces <- as.character(deces.2021$datedeces)
deces.2021$datedeces <- as.Date(deces.2021$datedeces, format = "%Y%m%d")


deces.2021$datenaiss <- as.character(deces.2021$datenaiss)
deces.2021$datenaiss <- as.Date(deces.2021$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2021$age <- as.period(interval(deces.2021$datenaiss , deces.2021$datedeces ))
deces.2021$age_years <- year(deces.2021$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2021$sexe<-as.character(deces.2021$sexe)

deces.2021$SEX<-ifelse(deces.2021$sexe=="1","Homme","Femme")

deces.2021_final<-deces.2021[,c("COM","datedeces","age_years","SEX")]

deces.2021_final$nbr_mort<-1


deces.2021_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2021_final,sum)

library(tidyr)

deces.2021_spread <- deces.2021_final_ag %>%
  spread(SEX, nbr_mort)

deces.2021_spread$Femme[is.na(deces.2021_spread$Femme)]<-0
deces.2021_spread$Homme[is.na(deces.2021_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2021_final$tranche_age <- cut(deces.2021_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2021_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2021_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2021_final,sum)

library(tidyr)

deces.2021_spread2 <- deces.2021_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2021_spread2$`0-9`[is.na(deces.2021_spread2$`0-9`)]<-0
deces.2021_spread2$`10-19`[is.na(deces.2021_spread2$`10-19`)]<-0
deces.2021_spread2$`20-39`[is.na(deces.2021_spread2$`20-39`)]<-0
deces.2021_spread2$`40-59`[is.na(deces.2021_spread2$`40-59`)]<-0
deces.2021_spread2$`60-64`[is.na(deces.2021_spread2$`60-64`)]<-0
deces.2021_spread2$`65-69`[is.na(deces.2021_spread2$`65-69`)]<-0
deces.2021_spread2$`70-74`[is.na(deces.2021_spread2$`70-74`)]<-0
deces.2021_spread2$`75-79`[is.na(deces.2021_spread2$`75-79`)]<-0
deces.2021_spread2$`80+`[is.na(deces.2021_spread2$`80+`)]<-0


deces.2021_spread<-left_join(deces.2021_spread, deces.2021_spread2)


names(deces.2021_spread)[names(deces.2021_spread)=="datedeces"]<-"date"



fwrite(deces.2021_spread,"/fichier deces insee/décès travaillé/deces.2021_age_sexe.csv")



##########

rm(list = ls())
gc()



table_passage_1970_2022 <- read_csv("/table passage 1970_2022/table_passage_1970_2022")

library(readr)
deces.2022 <- read_delim("/fichier deces insee/Deces_2022.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

#communes_dates_1970_2022<-fread("/données communes années/communes_dates_1970_2022.csv")
#communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)


table_passage_bis<-table_passage_1970_2022[,c("COM_AV","COM_AP")]
names(table_passage_bis)[names(table_passage_bis)=="COM_AV"]<-"lieudeces"

deces.2022<-left_join(deces.2022, table_passage_bis)


deces.2022$COM<-ifelse(!is.na(deces.2022$COM_AP),deces.2022$COM_AP,deces.2022$lieudeces)

deces.2022$datedeces <- as.character(deces.2022$datedeces)
deces.2022$datedeces <- as.Date(deces.2022$datedeces, format = "%Y%m%d")


deces.2022$datenaiss <- as.character(deces.2022$datenaiss)
deces.2022$datenaiss <- as.Date(deces.2022$datenaiss, format = "%Y%m%d")

library(lubridate)
deces.2022$age <- as.period(interval(deces.2022$datenaiss , deces.2022$datedeces ))
deces.2022$age_years <- year(deces.2022$age)

library(readr)
#metadonnees_deces <- read_csv("/fichier deces insee/metadonnees_deces.csv")
#les codes géo des fichiers deces sont bien code de l'epoque


deces.2022$sexe<-as.character(deces.2022$sexe)

deces.2022$SEX<-ifelse(deces.2022$sexe=="1","Homme","Femme")

deces.2022_final<-deces.2022[,c("COM","datedeces","age_years","SEX")]

deces.2022_final$nbr_mort<-1


deces.2022_final_ag<-aggregate(nbr_mort~COM+datedeces+SEX,deces.2022_final,sum)

library(tidyr)

deces.2022_spread <- deces.2022_final_ag %>%
  spread(SEX, nbr_mort)

deces.2022_spread$Femme[is.na(deces.2022_spread$Femme)]<-0
deces.2022_spread$Homme[is.na(deces.2022_spread$Homme)]<-0




#decoupage age 
#0-9
#10-19
#20-39
#40-59
#60-64
#65-69
#70-74
#75-79
#80+

#homme/femme

#


deces.2022_final$tranche_age <- cut(deces.2022_final$age_years, c(-Inf, 9, 19, 39, 59, 64, 69, 74, 79, Inf), 
                                    labels = c("0-9", "10-19", "20-39", "40-59", "60-64", "65-69", "70-74", "75-79", "80+"))
#table(deces.2022_final$tranche_age)
#repartition pas mal
#12 na pour absence de date

deces.2022_final_ag2<-aggregate(nbr_mort~COM+datedeces+tranche_age,deces.2022_final,sum)

library(tidyr)

deces.2022_spread2 <- deces.2022_final_ag2 %>%
  spread(tranche_age, nbr_mort)

deces.2022_spread2$`0-9`[is.na(deces.2022_spread2$`0-9`)]<-0
deces.2022_spread2$`10-19`[is.na(deces.2022_spread2$`10-19`)]<-0
deces.2022_spread2$`20-39`[is.na(deces.2022_spread2$`20-39`)]<-0
deces.2022_spread2$`40-59`[is.na(deces.2022_spread2$`40-59`)]<-0
deces.2022_spread2$`60-64`[is.na(deces.2022_spread2$`60-64`)]<-0
deces.2022_spread2$`65-69`[is.na(deces.2022_spread2$`65-69`)]<-0
deces.2022_spread2$`70-74`[is.na(deces.2022_spread2$`70-74`)]<-0
deces.2022_spread2$`75-79`[is.na(deces.2022_spread2$`75-79`)]<-0
deces.2022_spread2$`80+`[is.na(deces.2022_spread2$`80+`)]<-0


deces.2022_spread<-left_join(deces.2022_spread, deces.2022_spread2)


names(deces.2022_spread)[names(deces.2022_spread)=="datedeces"]<-"date"



#fwrite(deces.2022_spread,"/fichier deces insee/décès travaillé/deces.2022_age_sexe.csv")

















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




#enregistrer chaque date puis import 1 fois temp final : decouper en 40
#import chaque date fusionner par 40 coupe
#rbind le tout

communes_dates_1980_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1980-01-01")
end_date <- as.Date("1980-12-31")

communes_dates_1980_2022_temperature_final_1980 <- communes_dates_1980_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1980_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1980_age_sexe.csv")

communes_dates_1980_2022_temperature_final_1980<-left_join(communes_dates_1980_2022_temperature_final_1980,deces.1980_age_sexe)

communes_dates_1980_2022_temperature_final_1980[is.na(communes_dates_1980_2022_temperature_final_1980)]<-0

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



communes_dates_1980_2022_temperature_final_1980$mort_total<-communes_dates_1980_2022_temperature_final_1980$Femme+communes_dates_1980_2022_temperature_final_1980$Homme


communes_dates_1980_2022_temperature_final_1980$taux_mortalite_total<-communes_dates_1980_2022_temperature_final_1980$mort_total/communes_dates_1980_2022_temperature_final_1980$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980, communes_dates_1980_2022_temperature_final_1980$value_estimated_population>0)
communes_dates_1980_2022_temperature_final_1980<-filter(communes_dates_1980_2022_temperature_final_1980, communes_dates_1980_2022_temperature_final_1980$population_actif_25_54>0)



communes_dates_1980_2022_temperature_final_1980<-communes_dates_1980_2022_temperature_final_1980[,c(1:3,27,37:54,56)]


#il y a des NaN quand il y a des 0/0 c'est a dire quand il n'y a pas de population de ce type dans la commune
#generalement dans les petites communes
#on va laisser les NaN comme ca car c'est pas dans toutes les variables pour la commune 
#dans l'ols l'observation sera viré pour une categorie mais pour une autre si pas de NAn

#il y a des valeurs infinies du a des 1/0 des erreurs a supp

# il y a des hauts taux de mortalité mécaniques : communes des 2 habitants femmes : 1 mort et le taux monte a 50%



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


#761 valeurs inf

#il faut virer les part sup a 1

#communes_dates_1980_2022_temperature_final_1980<-communes_dates_1980_2022_temperature_final_1980[communes_dates_1980_2022_temperature_final_1980$taux_mortalite_10_19 != 1.4, ]

#nope ca supprime les nan




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


#concernant les 80plus il y a 1 million de NA soit 8% de l'enchantillon qui est NA car absence de cette population dans
#ces communes



#faire les opérations de calcul taux de mortalité sur 3 jours ou plus ici ? pour économiser lignes de base de données :non
#faire pour les csp le calcul de la part de chomeur etc... : ok


#il va falloir songer a faire une base de données pour chaque regression, c a dire pour chaque tx de mortalité etudié
#car les bases sont trop grosses max 8 var avec 500millions d'obs

#on va commencer par 80 plus pour voir





communes_dates_1980_2022_temperature_final_1980_80_plus<-communes_dates_1980_2022_temperature_final_1980[,c(1:11,22)]

communes_dates_1980_2022_temperature_final_1980_80_plus<-filter(communes_dates_1980_2022_temperature_final_1980_80_plus,  taux_mortalite_80_plus<=1)



#fwrite(communes_dates_1980_2022_temperature_final_1980_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1980_temperature_deces_80_plus.csv")


















##########

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




communes_dates_1981_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1981-01-01")
end_date <- as.Date("1981-12-31")

communes_dates_1981_2022_temperature_final_1981 <- communes_dates_1981_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1981_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1981_age_sexe.csv")

communes_dates_1981_2022_temperature_final_1981<-left_join(communes_dates_1981_2022_temperature_final_1981,deces.1981_age_sexe)

communes_dates_1981_2022_temperature_final_1981[is.na(communes_dates_1981_2022_temperature_final_1981)]<-0

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



communes_dates_1981_2022_temperature_final_1981$mort_total<-communes_dates_1981_2022_temperature_final_1981$Femme+communes_dates_1981_2022_temperature_final_1981$Homme


communes_dates_1981_2022_temperature_final_1981$taux_mortalite_total<-communes_dates_1981_2022_temperature_final_1981$mort_total/communes_dates_1981_2022_temperature_final_1981$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981, communes_dates_1981_2022_temperature_final_1981$value_estimated_population>0)
communes_dates_1981_2022_temperature_final_1981<-filter(communes_dates_1981_2022_temperature_final_1981, communes_dates_1981_2022_temperature_final_1981$population_actif_25_54>0)



communes_dates_1981_2022_temperature_final_1981<-communes_dates_1981_2022_temperature_final_1981[,c(1:3,27,37:54,56)]


#il y a des NaN quand il y a des 0/0 c'est a dire quand il n'y a pas de population de ce type dans la commune
#generalement dans les petites communes
#on va laisser les NaN comme ca car c'est pas dans toutes les variables pour la commune 
#dans l'ols l'observation sera viré pour une categorie mais pour une autre si pas de NAn

#il y a des valeurs infinies du a des 1/0 des erreurs a supp

# il y a des hauts taux de mortalité mécaniques : communes des 2 habitants femmes : 1 mort et le taux monte a 50%



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

#il faut virer les part sup a 1

#communes_dates_1981_2022_temperature_final_1981<-communes_dates_1981_2022_temperature_final_1981[communes_dates_1981_2022_temperature_final_1981$taux_mortalite_10_19 != 1.4, ]

#nope ca supprime les nan




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


#concernant les 80plus il y a 1 million de NA soit 8% de l'enchantillon qui est NA car absence de cette population dans
#ces communes



#faire les opérations de calcul taux de mortalité sur 3 jours ou plus ici ? pour économiser lignes de base de données :non
#faire pour les csp le calcul de la part de chomeur etc... : ok


#il va falloir songer a faire une base de données pour chaque regression, c a dire pour chaque tx de mortalité etudié
#car les bases sont trop grosses max 8 var avec 500millions d'obs

#on va commencer par 80 plus pour voir





communes_dates_1981_2022_temperature_final_1981_80_plus<-communes_dates_1981_2022_temperature_final_1981[,c(1:11,22)]

communes_dates_1981_2022_temperature_final_1981_80_plus<-filter(communes_dates_1981_2022_temperature_final_1981_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1981_2022_temperature_final_1981_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1981_temperature_deces_80_plus.csv")









##########

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




communes_dates_1982_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1982-01-01")
end_date <- as.Date("1982-12-31")

communes_dates_1982_2022_temperature_final_1982 <- communes_dates_1982_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1982_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1982_age_sexe.csv")

communes_dates_1982_2022_temperature_final_1982<-left_join(communes_dates_1982_2022_temperature_final_1982,deces.1982_age_sexe)

communes_dates_1982_2022_temperature_final_1982[is.na(communes_dates_1982_2022_temperature_final_1982)]<-0

RP_1982_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1982_age_sexe_final_2")

RP_1982_age_sexe_final_2<-RP_1982_age_sexe_final_2[,c(2:15)]

names(RP_1982_age_sexe_final_2)[names(RP_1982_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1982_2022_temperature_final_1982<-left_join(communes_dates_1982_2022_temperature_final_1982,RP_1982_age_sexe_final_2)


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



communes_dates_1982_2022_temperature_final_1982$mort_total<-communes_dates_1982_2022_temperature_final_1982$Femme+communes_dates_1982_2022_temperature_final_1982$Homme


communes_dates_1982_2022_temperature_final_1982$taux_mortalite_total<-communes_dates_1982_2022_temperature_final_1982$mort_total/communes_dates_1982_2022_temperature_final_1982$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982, communes_dates_1982_2022_temperature_final_1982$value_estimated_population>0)
communes_dates_1982_2022_temperature_final_1982<-filter(communes_dates_1982_2022_temperature_final_1982, communes_dates_1982_2022_temperature_final_1982$population_actif_25_54>0)



communes_dates_1982_2022_temperature_final_1982<-communes_dates_1982_2022_temperature_final_1982[,c(1:3,27,37:54,56)]




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





communes_dates_1982_2022_temperature_final_1982_80_plus<-communes_dates_1982_2022_temperature_final_1982[,c(1:11,22)]

communes_dates_1982_2022_temperature_final_1982_80_plus<-filter(communes_dates_1982_2022_temperature_final_1982_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1982_2022_temperature_final_1982_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1982_temperature_deces_80_plus.csv")








##########

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




communes_dates_1983_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1983-01-01")
end_date <- as.Date("1983-12-31")

communes_dates_1983_2022_temperature_final_1983 <- communes_dates_1983_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1983_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1983_age_sexe.csv")

communes_dates_1983_2022_temperature_final_1983<-left_join(communes_dates_1983_2022_temperature_final_1983,deces.1983_age_sexe)

communes_dates_1983_2022_temperature_final_1983[is.na(communes_dates_1983_2022_temperature_final_1983)]<-0

RP_1983_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1983_age_sexe_final_2")

RP_1983_age_sexe_final_2<-RP_1983_age_sexe_final_2[,c(2:15)]

names(RP_1983_age_sexe_final_2)[names(RP_1983_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1983_2022_temperature_final_1983<-left_join(communes_dates_1983_2022_temperature_final_1983,RP_1983_age_sexe_final_2)


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



communes_dates_1983_2022_temperature_final_1983$mort_total<-communes_dates_1983_2022_temperature_final_1983$Femme+communes_dates_1983_2022_temperature_final_1983$Homme


communes_dates_1983_2022_temperature_final_1983$taux_mortalite_total<-communes_dates_1983_2022_temperature_final_1983$mort_total/communes_dates_1983_2022_temperature_final_1983$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983, communes_dates_1983_2022_temperature_final_1983$value_estimated_population>0)
communes_dates_1983_2022_temperature_final_1983<-filter(communes_dates_1983_2022_temperature_final_1983, communes_dates_1983_2022_temperature_final_1983$population_actif_25_54>0)



communes_dates_1983_2022_temperature_final_1983<-communes_dates_1983_2022_temperature_final_1983[,c(1:3,27,37:54,56)]




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





communes_dates_1983_2022_temperature_final_1983_80_plus<-communes_dates_1983_2022_temperature_final_1983[,c(1:11,22)]

communes_dates_1983_2022_temperature_final_1983_80_plus<-filter(communes_dates_1983_2022_temperature_final_1983_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1983_2022_temperature_final_1983_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1983_temperature_deces_80_plus.csv")






##########

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




communes_dates_1984_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1984-01-01")
end_date <- as.Date("1984-12-31")

communes_dates_1984_2022_temperature_final_1984 <- communes_dates_1984_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1984_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1984_age_sexe.csv")

communes_dates_1984_2022_temperature_final_1984<-left_join(communes_dates_1984_2022_temperature_final_1984,deces.1984_age_sexe)

communes_dates_1984_2022_temperature_final_1984[is.na(communes_dates_1984_2022_temperature_final_1984)]<-0

RP_1984_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1984_age_sexe_final_2")

RP_1984_age_sexe_final_2<-RP_1984_age_sexe_final_2[,c(2:15)]

names(RP_1984_age_sexe_final_2)[names(RP_1984_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1984_2022_temperature_final_1984<-left_join(communes_dates_1984_2022_temperature_final_1984,RP_1984_age_sexe_final_2)


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



communes_dates_1984_2022_temperature_final_1984$mort_total<-communes_dates_1984_2022_temperature_final_1984$Femme+communes_dates_1984_2022_temperature_final_1984$Homme


communes_dates_1984_2022_temperature_final_1984$taux_mortalite_total<-communes_dates_1984_2022_temperature_final_1984$mort_total/communes_dates_1984_2022_temperature_final_1984$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984, communes_dates_1984_2022_temperature_final_1984$value_estimated_population>0)
communes_dates_1984_2022_temperature_final_1984<-filter(communes_dates_1984_2022_temperature_final_1984, communes_dates_1984_2022_temperature_final_1984$population_actif_25_54>0)



communes_dates_1984_2022_temperature_final_1984<-communes_dates_1984_2022_temperature_final_1984[,c(1:3,27,37:54,56)]




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





communes_dates_1984_2022_temperature_final_1984_80_plus<-communes_dates_1984_2022_temperature_final_1984[,c(1:11,22)]

communes_dates_1984_2022_temperature_final_1984_80_plus<-filter(communes_dates_1984_2022_temperature_final_1984_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1984_2022_temperature_final_1984_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1984_temperature_deces_80_plus.csv")






##########

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




communes_dates_1985_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1985-01-01")
end_date <- as.Date("1985-12-31")

communes_dates_1985_2022_temperature_final_1985 <- communes_dates_1985_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1985_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1985_age_sexe.csv")

communes_dates_1985_2022_temperature_final_1985<-left_join(communes_dates_1985_2022_temperature_final_1985,deces.1985_age_sexe)

communes_dates_1985_2022_temperature_final_1985[is.na(communes_dates_1985_2022_temperature_final_1985)]<-0

RP_1985_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1985_age_sexe_final_2")

RP_1985_age_sexe_final_2<-RP_1985_age_sexe_final_2[,c(2:15)]

names(RP_1985_age_sexe_final_2)[names(RP_1985_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1985_2022_temperature_final_1985<-left_join(communes_dates_1985_2022_temperature_final_1985,RP_1985_age_sexe_final_2)


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



communes_dates_1985_2022_temperature_final_1985$mort_total<-communes_dates_1985_2022_temperature_final_1985$Femme+communes_dates_1985_2022_temperature_final_1985$Homme


communes_dates_1985_2022_temperature_final_1985$taux_mortalite_total<-communes_dates_1985_2022_temperature_final_1985$mort_total/communes_dates_1985_2022_temperature_final_1985$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985, communes_dates_1985_2022_temperature_final_1985$value_estimated_population>0)
communes_dates_1985_2022_temperature_final_1985<-filter(communes_dates_1985_2022_temperature_final_1985, communes_dates_1985_2022_temperature_final_1985$population_actif_25_54>0)



communes_dates_1985_2022_temperature_final_1985<-communes_dates_1985_2022_temperature_final_1985[,c(1:3,27,37:54,56)]




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





communes_dates_1985_2022_temperature_final_1985_80_plus<-communes_dates_1985_2022_temperature_final_1985[,c(1:11,22)]

communes_dates_1985_2022_temperature_final_1985_80_plus<-filter(communes_dates_1985_2022_temperature_final_1985_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1985_2022_temperature_final_1985_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1985_temperature_deces_80_plus.csv")






##########

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




communes_dates_1986_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1986-01-01")
end_date <- as.Date("1986-12-31")

communes_dates_1986_2022_temperature_final_1986 <- communes_dates_1986_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1986_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1986_age_sexe.csv")

communes_dates_1986_2022_temperature_final_1986<-left_join(communes_dates_1986_2022_temperature_final_1986,deces.1986_age_sexe)

communes_dates_1986_2022_temperature_final_1986[is.na(communes_dates_1986_2022_temperature_final_1986)]<-0

RP_1986_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1986_age_sexe_final_2")

RP_1986_age_sexe_final_2<-RP_1986_age_sexe_final_2[,c(2:15)]

names(RP_1986_age_sexe_final_2)[names(RP_1986_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1986_2022_temperature_final_1986<-left_join(communes_dates_1986_2022_temperature_final_1986,RP_1986_age_sexe_final_2)


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



communes_dates_1986_2022_temperature_final_1986$mort_total<-communes_dates_1986_2022_temperature_final_1986$Femme+communes_dates_1986_2022_temperature_final_1986$Homme


communes_dates_1986_2022_temperature_final_1986$taux_mortalite_total<-communes_dates_1986_2022_temperature_final_1986$mort_total/communes_dates_1986_2022_temperature_final_1986$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986, communes_dates_1986_2022_temperature_final_1986$value_estimated_population>0)
communes_dates_1986_2022_temperature_final_1986<-filter(communes_dates_1986_2022_temperature_final_1986, communes_dates_1986_2022_temperature_final_1986$population_actif_25_54>0)



communes_dates_1986_2022_temperature_final_1986<-communes_dates_1986_2022_temperature_final_1986[,c(1:3,27,37:54,56)]




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





communes_dates_1986_2022_temperature_final_1986_80_plus<-communes_dates_1986_2022_temperature_final_1986[,c(1:11,22)]

communes_dates_1986_2022_temperature_final_1986_80_plus<-filter(communes_dates_1986_2022_temperature_final_1986_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1986_2022_temperature_final_1986_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1986_temperature_deces_80_plus.csv")








##########

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




communes_dates_1987_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1987-01-01")
end_date <- as.Date("1987-12-31")

communes_dates_1987_2022_temperature_final_1987 <- communes_dates_1987_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1987_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1987_age_sexe.csv")

communes_dates_1987_2022_temperature_final_1987<-left_join(communes_dates_1987_2022_temperature_final_1987,deces.1987_age_sexe)

communes_dates_1987_2022_temperature_final_1987[is.na(communes_dates_1987_2022_temperature_final_1987)]<-0

RP_1987_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1987_age_sexe_final_2")

RP_1987_age_sexe_final_2<-RP_1987_age_sexe_final_2[,c(2:15)]

names(RP_1987_age_sexe_final_2)[names(RP_1987_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1987_2022_temperature_final_1987<-left_join(communes_dates_1987_2022_temperature_final_1987,RP_1987_age_sexe_final_2)


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



communes_dates_1987_2022_temperature_final_1987$mort_total<-communes_dates_1987_2022_temperature_final_1987$Femme+communes_dates_1987_2022_temperature_final_1987$Homme


communes_dates_1987_2022_temperature_final_1987$taux_mortalite_total<-communes_dates_1987_2022_temperature_final_1987$mort_total/communes_dates_1987_2022_temperature_final_1987$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987, communes_dates_1987_2022_temperature_final_1987$value_estimated_population>0)
communes_dates_1987_2022_temperature_final_1987<-filter(communes_dates_1987_2022_temperature_final_1987, communes_dates_1987_2022_temperature_final_1987$population_actif_25_54>0)



communes_dates_1987_2022_temperature_final_1987<-communes_dates_1987_2022_temperature_final_1987[,c(1:3,27,37:54,56)]




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





communes_dates_1987_2022_temperature_final_1987_80_plus<-communes_dates_1987_2022_temperature_final_1987[,c(1:11,22)]

communes_dates_1987_2022_temperature_final_1987_80_plus<-filter(communes_dates_1987_2022_temperature_final_1987_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1987_2022_temperature_final_1987_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1987_temperature_deces_80_plus.csv")






##########

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




communes_dates_1988_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1988-01-01")
end_date <- as.Date("1988-12-31")

communes_dates_1988_2022_temperature_final_1988 <- communes_dates_1988_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1988_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1988_age_sexe.csv")

communes_dates_1988_2022_temperature_final_1988<-left_join(communes_dates_1988_2022_temperature_final_1988,deces.1988_age_sexe)

communes_dates_1988_2022_temperature_final_1988[is.na(communes_dates_1988_2022_temperature_final_1988)]<-0

RP_1988_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1988_age_sexe_final_2")

RP_1988_age_sexe_final_2<-RP_1988_age_sexe_final_2[,c(2:15)]

names(RP_1988_age_sexe_final_2)[names(RP_1988_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1988_2022_temperature_final_1988<-left_join(communes_dates_1988_2022_temperature_final_1988,RP_1988_age_sexe_final_2)


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



communes_dates_1988_2022_temperature_final_1988$mort_total<-communes_dates_1988_2022_temperature_final_1988$Femme+communes_dates_1988_2022_temperature_final_1988$Homme


communes_dates_1988_2022_temperature_final_1988$taux_mortalite_total<-communes_dates_1988_2022_temperature_final_1988$mort_total/communes_dates_1988_2022_temperature_final_1988$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988, communes_dates_1988_2022_temperature_final_1988$value_estimated_population>0)
communes_dates_1988_2022_temperature_final_1988<-filter(communes_dates_1988_2022_temperature_final_1988, communes_dates_1988_2022_temperature_final_1988$population_actif_25_54>0)



communes_dates_1988_2022_temperature_final_1988<-communes_dates_1988_2022_temperature_final_1988[,c(1:3,27,37:54,56)]




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





communes_dates_1988_2022_temperature_final_1988_80_plus<-communes_dates_1988_2022_temperature_final_1988[,c(1:11,22)]

communes_dates_1988_2022_temperature_final_1988_80_plus<-filter(communes_dates_1988_2022_temperature_final_1988_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1988_2022_temperature_final_1988_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1988_temperature_deces_80_plus.csv")






##########

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




communes_dates_1989_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1989-01-01")
end_date <- as.Date("1989-12-31")

communes_dates_1989_2022_temperature_final_1989 <- communes_dates_1989_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1989_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1989_age_sexe.csv")

communes_dates_1989_2022_temperature_final_1989<-left_join(communes_dates_1989_2022_temperature_final_1989,deces.1989_age_sexe)

communes_dates_1989_2022_temperature_final_1989[is.na(communes_dates_1989_2022_temperature_final_1989)]<-0

RP_1989_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1989_age_sexe_final_2")

RP_1989_age_sexe_final_2<-RP_1989_age_sexe_final_2[,c(2:15)]

names(RP_1989_age_sexe_final_2)[names(RP_1989_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1989_2022_temperature_final_1989<-left_join(communes_dates_1989_2022_temperature_final_1989,RP_1989_age_sexe_final_2)


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



communes_dates_1989_2022_temperature_final_1989$mort_total<-communes_dates_1989_2022_temperature_final_1989$Femme+communes_dates_1989_2022_temperature_final_1989$Homme


communes_dates_1989_2022_temperature_final_1989$taux_mortalite_total<-communes_dates_1989_2022_temperature_final_1989$mort_total/communes_dates_1989_2022_temperature_final_1989$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989, communes_dates_1989_2022_temperature_final_1989$value_estimated_population>0)
communes_dates_1989_2022_temperature_final_1989<-filter(communes_dates_1989_2022_temperature_final_1989, communes_dates_1989_2022_temperature_final_1989$population_actif_25_54>0)



communes_dates_1989_2022_temperature_final_1989<-communes_dates_1989_2022_temperature_final_1989[,c(1:3,27,37:54,56)]




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





communes_dates_1989_2022_temperature_final_1989_80_plus<-communes_dates_1989_2022_temperature_final_1989[,c(1:11,22)]

communes_dates_1989_2022_temperature_final_1989_80_plus<-filter(communes_dates_1989_2022_temperature_final_1989_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1989_2022_temperature_final_1989_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1989_temperature_deces_80_plus.csv")






##########

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




communes_dates_1990_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1990-01-01")
end_date <- as.Date("1990-12-31")

communes_dates_1990_2022_temperature_final_1990 <- communes_dates_1990_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1990_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1990_age_sexe.csv")

communes_dates_1990_2022_temperature_final_1990<-left_join(communes_dates_1990_2022_temperature_final_1990,deces.1990_age_sexe)

communes_dates_1990_2022_temperature_final_1990[is.na(communes_dates_1990_2022_temperature_final_1990)]<-0

RP_1990_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1990_age_sexe_final_2")

RP_1990_age_sexe_final_2<-RP_1990_age_sexe_final_2[,c(2:15)]

names(RP_1990_age_sexe_final_2)[names(RP_1990_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1990_2022_temperature_final_1990<-left_join(communes_dates_1990_2022_temperature_final_1990,RP_1990_age_sexe_final_2)


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



communes_dates_1990_2022_temperature_final_1990$mort_total<-communes_dates_1990_2022_temperature_final_1990$Femme+communes_dates_1990_2022_temperature_final_1990$Homme


communes_dates_1990_2022_temperature_final_1990$taux_mortalite_total<-communes_dates_1990_2022_temperature_final_1990$mort_total/communes_dates_1990_2022_temperature_final_1990$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990, communes_dates_1990_2022_temperature_final_1990$value_estimated_population>0)
communes_dates_1990_2022_temperature_final_1990<-filter(communes_dates_1990_2022_temperature_final_1990, communes_dates_1990_2022_temperature_final_1990$population_actif_25_54>0)



communes_dates_1990_2022_temperature_final_1990<-communes_dates_1990_2022_temperature_final_1990[,c(1:3,27,37:54,56)]




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





communes_dates_1990_2022_temperature_final_1990_80_plus<-communes_dates_1990_2022_temperature_final_1990[,c(1:11,22)]

communes_dates_1990_2022_temperature_final_1990_80_plus<-filter(communes_dates_1990_2022_temperature_final_1990_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1990_2022_temperature_final_1990_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1990_temperature_deces_80_plus.csv")







##########

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




communes_dates_1991_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1991-01-01")
end_date <- as.Date("1991-12-31")

communes_dates_1991_2022_temperature_final_1991 <- communes_dates_1991_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1991_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1991_age_sexe.csv")

communes_dates_1991_2022_temperature_final_1991<-left_join(communes_dates_1991_2022_temperature_final_1991,deces.1991_age_sexe)

communes_dates_1991_2022_temperature_final_1991[is.na(communes_dates_1991_2022_temperature_final_1991)]<-0

RP_1991_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1991_age_sexe_final_2")

RP_1991_age_sexe_final_2<-RP_1991_age_sexe_final_2[,c(2:15)]

names(RP_1991_age_sexe_final_2)[names(RP_1991_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1991_2022_temperature_final_1991<-left_join(communes_dates_1991_2022_temperature_final_1991,RP_1991_age_sexe_final_2)


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



communes_dates_1991_2022_temperature_final_1991$mort_total<-communes_dates_1991_2022_temperature_final_1991$Femme+communes_dates_1991_2022_temperature_final_1991$Homme


communes_dates_1991_2022_temperature_final_1991$taux_mortalite_total<-communes_dates_1991_2022_temperature_final_1991$mort_total/communes_dates_1991_2022_temperature_final_1991$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991, communes_dates_1991_2022_temperature_final_1991$value_estimated_population>0)
communes_dates_1991_2022_temperature_final_1991<-filter(communes_dates_1991_2022_temperature_final_1991, communes_dates_1991_2022_temperature_final_1991$population_actif_25_54>0)



communes_dates_1991_2022_temperature_final_1991<-communes_dates_1991_2022_temperature_final_1991[,c(1:3,27,37:54,56)]




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





communes_dates_1991_2022_temperature_final_1991_80_plus<-communes_dates_1991_2022_temperature_final_1991[,c(1:11,22)]

communes_dates_1991_2022_temperature_final_1991_80_plus<-filter(communes_dates_1991_2022_temperature_final_1991_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1991_2022_temperature_final_1991_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1991_temperature_deces_80_plus.csv")





##########

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




communes_dates_1992_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1992-01-01")
end_date <- as.Date("1992-12-31")

communes_dates_1992_2022_temperature_final_1992 <- communes_dates_1992_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1992_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1992_age_sexe.csv")

communes_dates_1992_2022_temperature_final_1992<-left_join(communes_dates_1992_2022_temperature_final_1992,deces.1992_age_sexe)

communes_dates_1992_2022_temperature_final_1992[is.na(communes_dates_1992_2022_temperature_final_1992)]<-0

RP_1992_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1992_age_sexe_final_2")

RP_1992_age_sexe_final_2<-RP_1992_age_sexe_final_2[,c(2:15)]

names(RP_1992_age_sexe_final_2)[names(RP_1992_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1992_2022_temperature_final_1992<-left_join(communes_dates_1992_2022_temperature_final_1992,RP_1992_age_sexe_final_2)


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



communes_dates_1992_2022_temperature_final_1992$mort_total<-communes_dates_1992_2022_temperature_final_1992$Femme+communes_dates_1992_2022_temperature_final_1992$Homme


communes_dates_1992_2022_temperature_final_1992$taux_mortalite_total<-communes_dates_1992_2022_temperature_final_1992$mort_total/communes_dates_1992_2022_temperature_final_1992$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992, communes_dates_1992_2022_temperature_final_1992$value_estimated_population>0)
communes_dates_1992_2022_temperature_final_1992<-filter(communes_dates_1992_2022_temperature_final_1992, communes_dates_1992_2022_temperature_final_1992$population_actif_25_54>0)



communes_dates_1992_2022_temperature_final_1992<-communes_dates_1992_2022_temperature_final_1992[,c(1:3,27,37:54,56)]




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





communes_dates_1992_2022_temperature_final_1992_80_plus<-communes_dates_1992_2022_temperature_final_1992[,c(1:11,22)]

communes_dates_1992_2022_temperature_final_1992_80_plus<-filter(communes_dates_1992_2022_temperature_final_1992_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1992_2022_temperature_final_1992_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1992_temperature_deces_80_plus.csv")





##########

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




communes_dates_1993_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1993-01-01")
end_date <- as.Date("1993-12-31")

communes_dates_1993_2022_temperature_final_1993 <- communes_dates_1993_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1993_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1993_age_sexe.csv")

communes_dates_1993_2022_temperature_final_1993<-left_join(communes_dates_1993_2022_temperature_final_1993,deces.1993_age_sexe)

communes_dates_1993_2022_temperature_final_1993[is.na(communes_dates_1993_2022_temperature_final_1993)]<-0

RP_1993_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1993_age_sexe_final_2")

RP_1993_age_sexe_final_2<-RP_1993_age_sexe_final_2[,c(2:15)]

names(RP_1993_age_sexe_final_2)[names(RP_1993_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1993_2022_temperature_final_1993<-left_join(communes_dates_1993_2022_temperature_final_1993,RP_1993_age_sexe_final_2)


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



communes_dates_1993_2022_temperature_final_1993$mort_total<-communes_dates_1993_2022_temperature_final_1993$Femme+communes_dates_1993_2022_temperature_final_1993$Homme


communes_dates_1993_2022_temperature_final_1993$taux_mortalite_total<-communes_dates_1993_2022_temperature_final_1993$mort_total/communes_dates_1993_2022_temperature_final_1993$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993, communes_dates_1993_2022_temperature_final_1993$value_estimated_population>0)
communes_dates_1993_2022_temperature_final_1993<-filter(communes_dates_1993_2022_temperature_final_1993, communes_dates_1993_2022_temperature_final_1993$population_actif_25_54>0)



communes_dates_1993_2022_temperature_final_1993<-communes_dates_1993_2022_temperature_final_1993[,c(1:3,27,37:54,56)]




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





communes_dates_1993_2022_temperature_final_1993_80_plus<-communes_dates_1993_2022_temperature_final_1993[,c(1:11,22)]

communes_dates_1993_2022_temperature_final_1993_80_plus<-filter(communes_dates_1993_2022_temperature_final_1993_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1993_2022_temperature_final_1993_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1993_temperature_deces_80_plus.csv")






##########

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




communes_dates_1994_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1994-01-01")
end_date <- as.Date("1994-12-31")

communes_dates_1994_2022_temperature_final_1994 <- communes_dates_1994_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1994_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1994_age_sexe.csv")

communes_dates_1994_2022_temperature_final_1994<-left_join(communes_dates_1994_2022_temperature_final_1994,deces.1994_age_sexe)

communes_dates_1994_2022_temperature_final_1994[is.na(communes_dates_1994_2022_temperature_final_1994)]<-0

RP_1994_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1994_age_sexe_final_2")

RP_1994_age_sexe_final_2<-RP_1994_age_sexe_final_2[,c(2:15)]

names(RP_1994_age_sexe_final_2)[names(RP_1994_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1994_2022_temperature_final_1994<-left_join(communes_dates_1994_2022_temperature_final_1994,RP_1994_age_sexe_final_2)


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



communes_dates_1994_2022_temperature_final_1994$mort_total<-communes_dates_1994_2022_temperature_final_1994$Femme+communes_dates_1994_2022_temperature_final_1994$Homme


communes_dates_1994_2022_temperature_final_1994$taux_mortalite_total<-communes_dates_1994_2022_temperature_final_1994$mort_total/communes_dates_1994_2022_temperature_final_1994$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994, communes_dates_1994_2022_temperature_final_1994$value_estimated_population>0)
communes_dates_1994_2022_temperature_final_1994<-filter(communes_dates_1994_2022_temperature_final_1994, communes_dates_1994_2022_temperature_final_1994$population_actif_25_54>0)



communes_dates_1994_2022_temperature_final_1994<-communes_dates_1994_2022_temperature_final_1994[,c(1:3,27,37:54,56)]




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





communes_dates_1994_2022_temperature_final_1994_80_plus<-communes_dates_1994_2022_temperature_final_1994[,c(1:11,22)]

communes_dates_1994_2022_temperature_final_1994_80_plus<-filter(communes_dates_1994_2022_temperature_final_1994_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1994_2022_temperature_final_1994_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1994_temperature_deces_80_plus.csv")





##########

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




communes_dates_1995_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1995-01-01")
end_date <- as.Date("1995-12-31")

communes_dates_1995_2022_temperature_final_1995 <- communes_dates_1995_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1995_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1995_age_sexe.csv")

communes_dates_1995_2022_temperature_final_1995<-left_join(communes_dates_1995_2022_temperature_final_1995,deces.1995_age_sexe)

communes_dates_1995_2022_temperature_final_1995[is.na(communes_dates_1995_2022_temperature_final_1995)]<-0

RP_1995_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1995_age_sexe_final_2")

RP_1995_age_sexe_final_2<-RP_1995_age_sexe_final_2[,c(2:15)]

names(RP_1995_age_sexe_final_2)[names(RP_1995_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1995_2022_temperature_final_1995<-left_join(communes_dates_1995_2022_temperature_final_1995,RP_1995_age_sexe_final_2)


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



communes_dates_1995_2022_temperature_final_1995$mort_total<-communes_dates_1995_2022_temperature_final_1995$Femme+communes_dates_1995_2022_temperature_final_1995$Homme


communes_dates_1995_2022_temperature_final_1995$taux_mortalite_total<-communes_dates_1995_2022_temperature_final_1995$mort_total/communes_dates_1995_2022_temperature_final_1995$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995, communes_dates_1995_2022_temperature_final_1995$value_estimated_population>0)
communes_dates_1995_2022_temperature_final_1995<-filter(communes_dates_1995_2022_temperature_final_1995, communes_dates_1995_2022_temperature_final_1995$population_actif_25_54>0)



communes_dates_1995_2022_temperature_final_1995<-communes_dates_1995_2022_temperature_final_1995[,c(1:3,27,37:54,56)]




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





communes_dates_1995_2022_temperature_final_1995_80_plus<-communes_dates_1995_2022_temperature_final_1995[,c(1:11,22)]

communes_dates_1995_2022_temperature_final_1995_80_plus<-filter(communes_dates_1995_2022_temperature_final_1995_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1995_2022_temperature_final_1995_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1995_temperature_deces_80_plus.csv")






##########

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




communes_dates_1996_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1996-01-01")
end_date <- as.Date("1996-12-31")

communes_dates_1996_2022_temperature_final_1996 <- communes_dates_1996_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1996_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1996_age_sexe.csv")

communes_dates_1996_2022_temperature_final_1996<-left_join(communes_dates_1996_2022_temperature_final_1996,deces.1996_age_sexe)

communes_dates_1996_2022_temperature_final_1996[is.na(communes_dates_1996_2022_temperature_final_1996)]<-0

RP_1996_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1996_age_sexe_final_2")

RP_1996_age_sexe_final_2<-RP_1996_age_sexe_final_2[,c(2:15)]

names(RP_1996_age_sexe_final_2)[names(RP_1996_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1996_2022_temperature_final_1996<-left_join(communes_dates_1996_2022_temperature_final_1996,RP_1996_age_sexe_final_2)


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



communes_dates_1996_2022_temperature_final_1996$mort_total<-communes_dates_1996_2022_temperature_final_1996$Femme+communes_dates_1996_2022_temperature_final_1996$Homme


communes_dates_1996_2022_temperature_final_1996$taux_mortalite_total<-communes_dates_1996_2022_temperature_final_1996$mort_total/communes_dates_1996_2022_temperature_final_1996$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996, communes_dates_1996_2022_temperature_final_1996$value_estimated_population>0)
communes_dates_1996_2022_temperature_final_1996<-filter(communes_dates_1996_2022_temperature_final_1996, communes_dates_1996_2022_temperature_final_1996$population_actif_25_54>0)



communes_dates_1996_2022_temperature_final_1996<-communes_dates_1996_2022_temperature_final_1996[,c(1:3,27,37:54,56)]




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





communes_dates_1996_2022_temperature_final_1996_80_plus<-communes_dates_1996_2022_temperature_final_1996[,c(1:11,22)]

communes_dates_1996_2022_temperature_final_1996_80_plus<-filter(communes_dates_1996_2022_temperature_final_1996_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1996_2022_temperature_final_1996_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1996_temperature_deces_80_plus.csv")






##########

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




communes_dates_1997_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1997-01-01")
end_date <- as.Date("1997-12-31")

communes_dates_1997_2022_temperature_final_1997 <- communes_dates_1997_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1997_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1997_age_sexe.csv")

communes_dates_1997_2022_temperature_final_1997<-left_join(communes_dates_1997_2022_temperature_final_1997,deces.1997_age_sexe)

communes_dates_1997_2022_temperature_final_1997[is.na(communes_dates_1997_2022_temperature_final_1997)]<-0

RP_1997_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1997_age_sexe_final_2")

RP_1997_age_sexe_final_2<-RP_1997_age_sexe_final_2[,c(2:15)]

names(RP_1997_age_sexe_final_2)[names(RP_1997_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1997_2022_temperature_final_1997<-left_join(communes_dates_1997_2022_temperature_final_1997,RP_1997_age_sexe_final_2)


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



communes_dates_1997_2022_temperature_final_1997$mort_total<-communes_dates_1997_2022_temperature_final_1997$Femme+communes_dates_1997_2022_temperature_final_1997$Homme


communes_dates_1997_2022_temperature_final_1997$taux_mortalite_total<-communes_dates_1997_2022_temperature_final_1997$mort_total/communes_dates_1997_2022_temperature_final_1997$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997, communes_dates_1997_2022_temperature_final_1997$value_estimated_population>0)
communes_dates_1997_2022_temperature_final_1997<-filter(communes_dates_1997_2022_temperature_final_1997, communes_dates_1997_2022_temperature_final_1997$population_actif_25_54>0)



communes_dates_1997_2022_temperature_final_1997<-communes_dates_1997_2022_temperature_final_1997[,c(1:3,27,37:54,56)]




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





communes_dates_1997_2022_temperature_final_1997_80_plus<-communes_dates_1997_2022_temperature_final_1997[,c(1:11,22)]

communes_dates_1997_2022_temperature_final_1997_80_plus<-filter(communes_dates_1997_2022_temperature_final_1997_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1997_2022_temperature_final_1997_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1997_temperature_deces_80_plus.csv")







##########

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




communes_dates_1998_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1998-01-01")
end_date <- as.Date("1998-12-31")

communes_dates_1998_2022_temperature_final_1998 <- communes_dates_1998_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1998_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1998_age_sexe.csv")

communes_dates_1998_2022_temperature_final_1998<-left_join(communes_dates_1998_2022_temperature_final_1998,deces.1998_age_sexe)

communes_dates_1998_2022_temperature_final_1998[is.na(communes_dates_1998_2022_temperature_final_1998)]<-0

RP_1998_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1998_age_sexe_final_2")

RP_1998_age_sexe_final_2<-RP_1998_age_sexe_final_2[,c(2:15)]

names(RP_1998_age_sexe_final_2)[names(RP_1998_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1998_2022_temperature_final_1998<-left_join(communes_dates_1998_2022_temperature_final_1998,RP_1998_age_sexe_final_2)


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



communes_dates_1998_2022_temperature_final_1998$mort_total<-communes_dates_1998_2022_temperature_final_1998$Femme+communes_dates_1998_2022_temperature_final_1998$Homme


communes_dates_1998_2022_temperature_final_1998$taux_mortalite_total<-communes_dates_1998_2022_temperature_final_1998$mort_total/communes_dates_1998_2022_temperature_final_1998$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998, communes_dates_1998_2022_temperature_final_1998$value_estimated_population>0)
communes_dates_1998_2022_temperature_final_1998<-filter(communes_dates_1998_2022_temperature_final_1998, communes_dates_1998_2022_temperature_final_1998$population_actif_25_54>0)



communes_dates_1998_2022_temperature_final_1998<-communes_dates_1998_2022_temperature_final_1998[,c(1:3,27,37:54,56)]




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





communes_dates_1998_2022_temperature_final_1998_80_plus<-communes_dates_1998_2022_temperature_final_1998[,c(1:11,22)]

communes_dates_1998_2022_temperature_final_1998_80_plus<-filter(communes_dates_1998_2022_temperature_final_1998_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1998_2022_temperature_final_1998_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1998_temperature_deces_80_plus.csv")






##########

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




communes_dates_1999_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("1999-01-01")
end_date <- as.Date("1999-12-31")

communes_dates_1999_2022_temperature_final_1999 <- communes_dates_1999_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.1999_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.1999_age_sexe.csv")

communes_dates_1999_2022_temperature_final_1999<-left_join(communes_dates_1999_2022_temperature_final_1999,deces.1999_age_sexe)

communes_dates_1999_2022_temperature_final_1999[is.na(communes_dates_1999_2022_temperature_final_1999)]<-0

RP_1999_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_1999_age_sexe_final_2")

RP_1999_age_sexe_final_2<-RP_1999_age_sexe_final_2[,c(2:15)]

names(RP_1999_age_sexe_final_2)[names(RP_1999_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_1999_2022_temperature_final_1999<-left_join(communes_dates_1999_2022_temperature_final_1999,RP_1999_age_sexe_final_2)


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



communes_dates_1999_2022_temperature_final_1999$mort_total<-communes_dates_1999_2022_temperature_final_1999$Femme+communes_dates_1999_2022_temperature_final_1999$Homme


communes_dates_1999_2022_temperature_final_1999$taux_mortalite_total<-communes_dates_1999_2022_temperature_final_1999$mort_total/communes_dates_1999_2022_temperature_final_1999$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999, communes_dates_1999_2022_temperature_final_1999$value_estimated_population>0)
communes_dates_1999_2022_temperature_final_1999<-filter(communes_dates_1999_2022_temperature_final_1999, communes_dates_1999_2022_temperature_final_1999$population_actif_25_54>0)



communes_dates_1999_2022_temperature_final_1999<-communes_dates_1999_2022_temperature_final_1999[,c(1:3,27,37:54,56)]




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





communes_dates_1999_2022_temperature_final_1999_80_plus<-communes_dates_1999_2022_temperature_final_1999[,c(1:11,22)]

communes_dates_1999_2022_temperature_final_1999_80_plus<-filter(communes_dates_1999_2022_temperature_final_1999_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_1999_2022_temperature_final_1999_80_plus,"/données communes années/données mortalité temperature final/communes_dates_1999_temperature_deces_80_plus.csv")







##########

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




communes_dates_2000_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2000-01-01")
end_date <- as.Date("2000-12-31")

communes_dates_2000_2022_temperature_final_2000 <- communes_dates_2000_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2000_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2000_age_sexe.csv")

communes_dates_2000_2022_temperature_final_2000<-left_join(communes_dates_2000_2022_temperature_final_2000,deces.2000_age_sexe)

communes_dates_2000_2022_temperature_final_2000[is.na(communes_dates_2000_2022_temperature_final_2000)]<-0

RP_2000_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2000_age_sexe_final_2")

RP_2000_age_sexe_final_2<-RP_2000_age_sexe_final_2[,c(2:15)]

names(RP_2000_age_sexe_final_2)[names(RP_2000_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2000_2022_temperature_final_2000<-left_join(communes_dates_2000_2022_temperature_final_2000,RP_2000_age_sexe_final_2)


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



communes_dates_2000_2022_temperature_final_2000$mort_total<-communes_dates_2000_2022_temperature_final_2000$Femme+communes_dates_2000_2022_temperature_final_2000$Homme


communes_dates_2000_2022_temperature_final_2000$taux_mortalite_total<-communes_dates_2000_2022_temperature_final_2000$mort_total/communes_dates_2000_2022_temperature_final_2000$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000, communes_dates_2000_2022_temperature_final_2000$value_estimated_population>0)
communes_dates_2000_2022_temperature_final_2000<-filter(communes_dates_2000_2022_temperature_final_2000, communes_dates_2000_2022_temperature_final_2000$population_actif_25_54>0)



communes_dates_2000_2022_temperature_final_2000<-communes_dates_2000_2022_temperature_final_2000[,c(1:3,27,37:54,56)]




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





communes_dates_2000_2022_temperature_final_2000_80_plus<-communes_dates_2000_2022_temperature_final_2000[,c(1:11,22)]

communes_dates_2000_2022_temperature_final_2000_80_plus<-filter(communes_dates_2000_2022_temperature_final_2000_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2000_2022_temperature_final_2000_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2000_temperature_deces_80_plus.csv")






##########

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




communes_dates_2001_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2001-01-01")
end_date <- as.Date("2001-12-31")

communes_dates_2001_2022_temperature_final_2001 <- communes_dates_2001_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2001_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2001_age_sexe.csv")

communes_dates_2001_2022_temperature_final_2001<-left_join(communes_dates_2001_2022_temperature_final_2001,deces.2001_age_sexe)

communes_dates_2001_2022_temperature_final_2001[is.na(communes_dates_2001_2022_temperature_final_2001)]<-0

RP_2001_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2001_age_sexe_final_2")

RP_2001_age_sexe_final_2<-RP_2001_age_sexe_final_2[,c(2:15)]

names(RP_2001_age_sexe_final_2)[names(RP_2001_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2001_2022_temperature_final_2001<-left_join(communes_dates_2001_2022_temperature_final_2001,RP_2001_age_sexe_final_2)


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



communes_dates_2001_2022_temperature_final_2001$mort_total<-communes_dates_2001_2022_temperature_final_2001$Femme+communes_dates_2001_2022_temperature_final_2001$Homme


communes_dates_2001_2022_temperature_final_2001$taux_mortalite_total<-communes_dates_2001_2022_temperature_final_2001$mort_total/communes_dates_2001_2022_temperature_final_2001$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001, communes_dates_2001_2022_temperature_final_2001$value_estimated_population>0)
communes_dates_2001_2022_temperature_final_2001<-filter(communes_dates_2001_2022_temperature_final_2001, communes_dates_2001_2022_temperature_final_2001$population_actif_25_54>0)



communes_dates_2001_2022_temperature_final_2001<-communes_dates_2001_2022_temperature_final_2001[,c(1:3,27,37:54,56)]




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





communes_dates_2001_2022_temperature_final_2001_80_plus<-communes_dates_2001_2022_temperature_final_2001[,c(1:11,22)]

communes_dates_2001_2022_temperature_final_2001_80_plus<-filter(communes_dates_2001_2022_temperature_final_2001_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2001_2022_temperature_final_2001_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2001_temperature_deces_80_plus.csv")







##########

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




communes_dates_2002_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2002-01-01")
end_date <- as.Date("2002-12-31")

communes_dates_2002_2022_temperature_final_2002 <- communes_dates_2002_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2002_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2002_age_sexe.csv")

communes_dates_2002_2022_temperature_final_2002<-left_join(communes_dates_2002_2022_temperature_final_2002,deces.2002_age_sexe)

communes_dates_2002_2022_temperature_final_2002[is.na(communes_dates_2002_2022_temperature_final_2002)]<-0

RP_2002_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2002_age_sexe_final_2")

RP_2002_age_sexe_final_2<-RP_2002_age_sexe_final_2[,c(2:15)]

names(RP_2002_age_sexe_final_2)[names(RP_2002_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2002_2022_temperature_final_2002<-left_join(communes_dates_2002_2022_temperature_final_2002,RP_2002_age_sexe_final_2)


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



communes_dates_2002_2022_temperature_final_2002$mort_total<-communes_dates_2002_2022_temperature_final_2002$Femme+communes_dates_2002_2022_temperature_final_2002$Homme


communes_dates_2002_2022_temperature_final_2002$taux_mortalite_total<-communes_dates_2002_2022_temperature_final_2002$mort_total/communes_dates_2002_2022_temperature_final_2002$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002, communes_dates_2002_2022_temperature_final_2002$value_estimated_population>0)
communes_dates_2002_2022_temperature_final_2002<-filter(communes_dates_2002_2022_temperature_final_2002, communes_dates_2002_2022_temperature_final_2002$population_actif_25_54>0)



communes_dates_2002_2022_temperature_final_2002<-communes_dates_2002_2022_temperature_final_2002[,c(1:3,27,37:54,56)]




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





communes_dates_2002_2022_temperature_final_2002_80_plus<-communes_dates_2002_2022_temperature_final_2002[,c(1:11,22)]

communes_dates_2002_2022_temperature_final_2002_80_plus<-filter(communes_dates_2002_2022_temperature_final_2002_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2002_2022_temperature_final_2002_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2002_temperature_deces_80_plus.csv")







##########

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




communes_dates_2003_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2003-01-01")
end_date <- as.Date("2003-12-31")

communes_dates_2003_2022_temperature_final_2003 <- communes_dates_2003_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2003_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2003_age_sexe.csv")

communes_dates_2003_2022_temperature_final_2003<-left_join(communes_dates_2003_2022_temperature_final_2003,deces.2003_age_sexe)

communes_dates_2003_2022_temperature_final_2003[is.na(communes_dates_2003_2022_temperature_final_2003)]<-0

RP_2003_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2003_age_sexe_final_2")

RP_2003_age_sexe_final_2<-RP_2003_age_sexe_final_2[,c(2:15)]

names(RP_2003_age_sexe_final_2)[names(RP_2003_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2003_2022_temperature_final_2003<-left_join(communes_dates_2003_2022_temperature_final_2003,RP_2003_age_sexe_final_2)


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



communes_dates_2003_2022_temperature_final_2003$mort_total<-communes_dates_2003_2022_temperature_final_2003$Femme+communes_dates_2003_2022_temperature_final_2003$Homme


communes_dates_2003_2022_temperature_final_2003$taux_mortalite_total<-communes_dates_2003_2022_temperature_final_2003$mort_total/communes_dates_2003_2022_temperature_final_2003$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003, communes_dates_2003_2022_temperature_final_2003$value_estimated_population>0)
communes_dates_2003_2022_temperature_final_2003<-filter(communes_dates_2003_2022_temperature_final_2003, communes_dates_2003_2022_temperature_final_2003$population_actif_25_54>0)



communes_dates_2003_2022_temperature_final_2003<-communes_dates_2003_2022_temperature_final_2003[,c(1:3,27,37:54,56)]




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





communes_dates_2003_2022_temperature_final_2003_80_plus<-communes_dates_2003_2022_temperature_final_2003[,c(1:11,22)]

communes_dates_2003_2022_temperature_final_2003_80_plus<-filter(communes_dates_2003_2022_temperature_final_2003_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2003_2022_temperature_final_2003_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2003_temperature_deces_80_plus.csv")






##########

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




communes_dates_2004_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2004-01-01")
end_date <- as.Date("2004-12-31")

communes_dates_2004_2022_temperature_final_2004 <- communes_dates_2004_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2004_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2004_age_sexe.csv")

communes_dates_2004_2022_temperature_final_2004<-left_join(communes_dates_2004_2022_temperature_final_2004,deces.2004_age_sexe)

communes_dates_2004_2022_temperature_final_2004[is.na(communes_dates_2004_2022_temperature_final_2004)]<-0

RP_2004_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2004_age_sexe_final_2")

RP_2004_age_sexe_final_2<-RP_2004_age_sexe_final_2[,c(2:15)]

names(RP_2004_age_sexe_final_2)[names(RP_2004_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2004_2022_temperature_final_2004<-left_join(communes_dates_2004_2022_temperature_final_2004,RP_2004_age_sexe_final_2)


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



communes_dates_2004_2022_temperature_final_2004$mort_total<-communes_dates_2004_2022_temperature_final_2004$Femme+communes_dates_2004_2022_temperature_final_2004$Homme


communes_dates_2004_2022_temperature_final_2004$taux_mortalite_total<-communes_dates_2004_2022_temperature_final_2004$mort_total/communes_dates_2004_2022_temperature_final_2004$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004, communes_dates_2004_2022_temperature_final_2004$value_estimated_population>0)
communes_dates_2004_2022_temperature_final_2004<-filter(communes_dates_2004_2022_temperature_final_2004, communes_dates_2004_2022_temperature_final_2004$population_actif_25_54>0)



communes_dates_2004_2022_temperature_final_2004<-communes_dates_2004_2022_temperature_final_2004[,c(1:3,27,37:54,56)]




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





communes_dates_2004_2022_temperature_final_2004_80_plus<-communes_dates_2004_2022_temperature_final_2004[,c(1:11,22)]

communes_dates_2004_2022_temperature_final_2004_80_plus<-filter(communes_dates_2004_2022_temperature_final_2004_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2004_2022_temperature_final_2004_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2004_temperature_deces_80_plus.csv")






##########

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




communes_dates_2005_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2005-01-01")
end_date <- as.Date("2005-12-31")

communes_dates_2005_2022_temperature_final_2005 <- communes_dates_2005_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2005_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2005_age_sexe.csv")

communes_dates_2005_2022_temperature_final_2005<-left_join(communes_dates_2005_2022_temperature_final_2005,deces.2005_age_sexe)

communes_dates_2005_2022_temperature_final_2005[is.na(communes_dates_2005_2022_temperature_final_2005)]<-0

RP_2005_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2005_age_sexe_final_2")

RP_2005_age_sexe_final_2<-RP_2005_age_sexe_final_2[,c(2:15)]

names(RP_2005_age_sexe_final_2)[names(RP_2005_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2005_2022_temperature_final_2005<-left_join(communes_dates_2005_2022_temperature_final_2005,RP_2005_age_sexe_final_2)


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



communes_dates_2005_2022_temperature_final_2005$mort_total<-communes_dates_2005_2022_temperature_final_2005$Femme+communes_dates_2005_2022_temperature_final_2005$Homme


communes_dates_2005_2022_temperature_final_2005$taux_mortalite_total<-communes_dates_2005_2022_temperature_final_2005$mort_total/communes_dates_2005_2022_temperature_final_2005$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005, communes_dates_2005_2022_temperature_final_2005$value_estimated_population>0)
communes_dates_2005_2022_temperature_final_2005<-filter(communes_dates_2005_2022_temperature_final_2005, communes_dates_2005_2022_temperature_final_2005$population_actif_25_54>0)



communes_dates_2005_2022_temperature_final_2005<-communes_dates_2005_2022_temperature_final_2005[,c(1:3,27,37:54,56)]




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





communes_dates_2005_2022_temperature_final_2005_80_plus<-communes_dates_2005_2022_temperature_final_2005[,c(1:11,22)]

communes_dates_2005_2022_temperature_final_2005_80_plus<-filter(communes_dates_2005_2022_temperature_final_2005_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2005_2022_temperature_final_2005_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2005_temperature_deces_80_plus.csv")






##########

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




communes_dates_2006_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2006-01-01")
end_date <- as.Date("2006-12-31")

communes_dates_2006_2022_temperature_final_2006 <- communes_dates_2006_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2006_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2006_age_sexe.csv")

communes_dates_2006_2022_temperature_final_2006<-left_join(communes_dates_2006_2022_temperature_final_2006,deces.2006_age_sexe)

communes_dates_2006_2022_temperature_final_2006[is.na(communes_dates_2006_2022_temperature_final_2006)]<-0

RP_2006_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2006_age_sexe_final_2")

RP_2006_age_sexe_final_2<-RP_2006_age_sexe_final_2[,c(2:15)]

names(RP_2006_age_sexe_final_2)[names(RP_2006_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2006_2022_temperature_final_2006<-left_join(communes_dates_2006_2022_temperature_final_2006,RP_2006_age_sexe_final_2)


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



communes_dates_2006_2022_temperature_final_2006$mort_total<-communes_dates_2006_2022_temperature_final_2006$Femme+communes_dates_2006_2022_temperature_final_2006$Homme


communes_dates_2006_2022_temperature_final_2006$taux_mortalite_total<-communes_dates_2006_2022_temperature_final_2006$mort_total/communes_dates_2006_2022_temperature_final_2006$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006, communes_dates_2006_2022_temperature_final_2006$value_estimated_population>0)
communes_dates_2006_2022_temperature_final_2006<-filter(communes_dates_2006_2022_temperature_final_2006, communes_dates_2006_2022_temperature_final_2006$population_actif_25_54>0)



communes_dates_2006_2022_temperature_final_2006<-communes_dates_2006_2022_temperature_final_2006[,c(1:3,27,37:54,56)]




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





communes_dates_2006_2022_temperature_final_2006_80_plus<-communes_dates_2006_2022_temperature_final_2006[,c(1:11,22)]

communes_dates_2006_2022_temperature_final_2006_80_plus<-filter(communes_dates_2006_2022_temperature_final_2006_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2006_2022_temperature_final_2006_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2006_temperature_deces_80_plus.csv")






##########

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




communes_dates_2007_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2007-01-01")
end_date <- as.Date("2007-12-31")

communes_dates_2007_2022_temperature_final_2007 <- communes_dates_2007_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2007_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2007_age_sexe.csv")

communes_dates_2007_2022_temperature_final_2007<-left_join(communes_dates_2007_2022_temperature_final_2007,deces.2007_age_sexe)

communes_dates_2007_2022_temperature_final_2007[is.na(communes_dates_2007_2022_temperature_final_2007)]<-0

RP_2007_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2007_age_sexe_final_2")

RP_2007_age_sexe_final_2<-RP_2007_age_sexe_final_2[,c(2:15)]

names(RP_2007_age_sexe_final_2)[names(RP_2007_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2007_2022_temperature_final_2007<-left_join(communes_dates_2007_2022_temperature_final_2007,RP_2007_age_sexe_final_2)


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



communes_dates_2007_2022_temperature_final_2007$mort_total<-communes_dates_2007_2022_temperature_final_2007$Femme+communes_dates_2007_2022_temperature_final_2007$Homme


communes_dates_2007_2022_temperature_final_2007$taux_mortalite_total<-communes_dates_2007_2022_temperature_final_2007$mort_total/communes_dates_2007_2022_temperature_final_2007$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007, communes_dates_2007_2022_temperature_final_2007$value_estimated_population>0)
communes_dates_2007_2022_temperature_final_2007<-filter(communes_dates_2007_2022_temperature_final_2007, communes_dates_2007_2022_temperature_final_2007$population_actif_25_54>0)



communes_dates_2007_2022_temperature_final_2007<-communes_dates_2007_2022_temperature_final_2007[,c(1:3,27,37:54,56)]




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





communes_dates_2007_2022_temperature_final_2007_80_plus<-communes_dates_2007_2022_temperature_final_2007[,c(1:11,22)]

communes_dates_2007_2022_temperature_final_2007_80_plus<-filter(communes_dates_2007_2022_temperature_final_2007_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2007_2022_temperature_final_2007_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2007_temperature_deces_80_plus.csv")







##########

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




communes_dates_2008_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2008-01-01")
end_date <- as.Date("2008-12-31")

communes_dates_2008_2022_temperature_final_2008 <- communes_dates_2008_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2008_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2008_age_sexe.csv")

communes_dates_2008_2022_temperature_final_2008<-left_join(communes_dates_2008_2022_temperature_final_2008,deces.2008_age_sexe)

communes_dates_2008_2022_temperature_final_2008[is.na(communes_dates_2008_2022_temperature_final_2008)]<-0

RP_2008_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2008_age_sexe_final_2")

RP_2008_age_sexe_final_2<-RP_2008_age_sexe_final_2[,c(2:15)]

names(RP_2008_age_sexe_final_2)[names(RP_2008_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2008_2022_temperature_final_2008<-left_join(communes_dates_2008_2022_temperature_final_2008,RP_2008_age_sexe_final_2)


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



communes_dates_2008_2022_temperature_final_2008$mort_total<-communes_dates_2008_2022_temperature_final_2008$Femme+communes_dates_2008_2022_temperature_final_2008$Homme


communes_dates_2008_2022_temperature_final_2008$taux_mortalite_total<-communes_dates_2008_2022_temperature_final_2008$mort_total/communes_dates_2008_2022_temperature_final_2008$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008, communes_dates_2008_2022_temperature_final_2008$value_estimated_population>0)
communes_dates_2008_2022_temperature_final_2008<-filter(communes_dates_2008_2022_temperature_final_2008, communes_dates_2008_2022_temperature_final_2008$population_actif_25_54>0)



communes_dates_2008_2022_temperature_final_2008<-communes_dates_2008_2022_temperature_final_2008[,c(1:3,27,37:54,56)]




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





communes_dates_2008_2022_temperature_final_2008_80_plus<-communes_dates_2008_2022_temperature_final_2008[,c(1:11,22)]

communes_dates_2008_2022_temperature_final_2008_80_plus<-filter(communes_dates_2008_2022_temperature_final_2008_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2008_2022_temperature_final_2008_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2008_temperature_deces_80_plus.csv")







##########

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




communes_dates_2009_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2009-01-01")
end_date <- as.Date("2009-12-31")

communes_dates_2009_2022_temperature_final_2009 <- communes_dates_2009_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2009_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2009_age_sexe.csv")

communes_dates_2009_2022_temperature_final_2009<-left_join(communes_dates_2009_2022_temperature_final_2009,deces.2009_age_sexe)

communes_dates_2009_2022_temperature_final_2009[is.na(communes_dates_2009_2022_temperature_final_2009)]<-0

RP_2009_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2009_age_sexe_final_2")

RP_2009_age_sexe_final_2<-RP_2009_age_sexe_final_2[,c(2:15)]

names(RP_2009_age_sexe_final_2)[names(RP_2009_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2009_2022_temperature_final_2009<-left_join(communes_dates_2009_2022_temperature_final_2009,RP_2009_age_sexe_final_2)


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



communes_dates_2009_2022_temperature_final_2009$mort_total<-communes_dates_2009_2022_temperature_final_2009$Femme+communes_dates_2009_2022_temperature_final_2009$Homme


communes_dates_2009_2022_temperature_final_2009$taux_mortalite_total<-communes_dates_2009_2022_temperature_final_2009$mort_total/communes_dates_2009_2022_temperature_final_2009$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009, communes_dates_2009_2022_temperature_final_2009$value_estimated_population>0)
communes_dates_2009_2022_temperature_final_2009<-filter(communes_dates_2009_2022_temperature_final_2009, communes_dates_2009_2022_temperature_final_2009$population_actif_25_54>0)



communes_dates_2009_2022_temperature_final_2009<-communes_dates_2009_2022_temperature_final_2009[,c(1:3,27,37:54,56)]




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





communes_dates_2009_2022_temperature_final_2009_80_plus<-communes_dates_2009_2022_temperature_final_2009[,c(1:11,22)]

communes_dates_2009_2022_temperature_final_2009_80_plus<-filter(communes_dates_2009_2022_temperature_final_2009_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2009_2022_temperature_final_2009_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2009_temperature_deces_80_plus.csv")







##########

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




communes_dates_2010_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2010-12-31")

communes_dates_2010_2022_temperature_final_2010 <- communes_dates_2010_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2010_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2010_age_sexe.csv")

communes_dates_2010_2022_temperature_final_2010<-left_join(communes_dates_2010_2022_temperature_final_2010,deces.2010_age_sexe)

communes_dates_2010_2022_temperature_final_2010[is.na(communes_dates_2010_2022_temperature_final_2010)]<-0

RP_2010_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2010_age_sexe_final_2")

RP_2010_age_sexe_final_2<-RP_2010_age_sexe_final_2[,c(2:15)]

names(RP_2010_age_sexe_final_2)[names(RP_2010_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2010_2022_temperature_final_2010<-left_join(communes_dates_2010_2022_temperature_final_2010,RP_2010_age_sexe_final_2)


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



communes_dates_2010_2022_temperature_final_2010$mort_total<-communes_dates_2010_2022_temperature_final_2010$Femme+communes_dates_2010_2022_temperature_final_2010$Homme


communes_dates_2010_2022_temperature_final_2010$taux_mortalite_total<-communes_dates_2010_2022_temperature_final_2010$mort_total/communes_dates_2010_2022_temperature_final_2010$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010, communes_dates_2010_2022_temperature_final_2010$value_estimated_population>0)
communes_dates_2010_2022_temperature_final_2010<-filter(communes_dates_2010_2022_temperature_final_2010, communes_dates_2010_2022_temperature_final_2010$population_actif_25_54>0)



communes_dates_2010_2022_temperature_final_2010<-communes_dates_2010_2022_temperature_final_2010[,c(1:3,27,37:54,56)]




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





communes_dates_2010_2022_temperature_final_2010_80_plus<-communes_dates_2010_2022_temperature_final_2010[,c(1:11,22)]

communes_dates_2010_2022_temperature_final_2010_80_plus<-filter(communes_dates_2010_2022_temperature_final_2010_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2010_2022_temperature_final_2010_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2010_temperature_deces_80_plus.csv")







##########

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




communes_dates_2011_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2011-01-01")
end_date <- as.Date("2011-12-31")

communes_dates_2011_2022_temperature_final_2011 <- communes_dates_2011_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2011_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2011_age_sexe.csv")

communes_dates_2011_2022_temperature_final_2011<-left_join(communes_dates_2011_2022_temperature_final_2011,deces.2011_age_sexe)

communes_dates_2011_2022_temperature_final_2011[is.na(communes_dates_2011_2022_temperature_final_2011)]<-0

RP_2011_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2011_age_sexe_final_2")

RP_2011_age_sexe_final_2<-RP_2011_age_sexe_final_2[,c(2:15)]

names(RP_2011_age_sexe_final_2)[names(RP_2011_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2011_2022_temperature_final_2011<-left_join(communes_dates_2011_2022_temperature_final_2011,RP_2011_age_sexe_final_2)


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



communes_dates_2011_2022_temperature_final_2011$mort_total<-communes_dates_2011_2022_temperature_final_2011$Femme+communes_dates_2011_2022_temperature_final_2011$Homme


communes_dates_2011_2022_temperature_final_2011$taux_mortalite_total<-communes_dates_2011_2022_temperature_final_2011$mort_total/communes_dates_2011_2022_temperature_final_2011$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011, communes_dates_2011_2022_temperature_final_2011$value_estimated_population>0)
communes_dates_2011_2022_temperature_final_2011<-filter(communes_dates_2011_2022_temperature_final_2011, communes_dates_2011_2022_temperature_final_2011$population_actif_25_54>0)



communes_dates_2011_2022_temperature_final_2011<-communes_dates_2011_2022_temperature_final_2011[,c(1:3,27,37:54,56)]




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





communes_dates_2011_2022_temperature_final_2011_80_plus<-communes_dates_2011_2022_temperature_final_2011[,c(1:11,22)]

communes_dates_2011_2022_temperature_final_2011_80_plus<-filter(communes_dates_2011_2022_temperature_final_2011_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2011_2022_temperature_final_2011_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2011_temperature_deces_80_plus.csv")







##########

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




communes_dates_2012_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2012-01-01")
end_date <- as.Date("2012-12-31")

communes_dates_2012_2022_temperature_final_2012 <- communes_dates_2012_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2012_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2012_age_sexe.csv")

communes_dates_2012_2022_temperature_final_2012<-left_join(communes_dates_2012_2022_temperature_final_2012,deces.2012_age_sexe)

communes_dates_2012_2022_temperature_final_2012[is.na(communes_dates_2012_2022_temperature_final_2012)]<-0

RP_2012_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2012_age_sexe_final_2")

RP_2012_age_sexe_final_2<-RP_2012_age_sexe_final_2[,c(2:15)]

names(RP_2012_age_sexe_final_2)[names(RP_2012_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2012_2022_temperature_final_2012<-left_join(communes_dates_2012_2022_temperature_final_2012,RP_2012_age_sexe_final_2)


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



communes_dates_2012_2022_temperature_final_2012$mort_total<-communes_dates_2012_2022_temperature_final_2012$Femme+communes_dates_2012_2022_temperature_final_2012$Homme


communes_dates_2012_2022_temperature_final_2012$taux_mortalite_total<-communes_dates_2012_2022_temperature_final_2012$mort_total/communes_dates_2012_2022_temperature_final_2012$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012, communes_dates_2012_2022_temperature_final_2012$value_estimated_population>0)
communes_dates_2012_2022_temperature_final_2012<-filter(communes_dates_2012_2022_temperature_final_2012, communes_dates_2012_2022_temperature_final_2012$population_actif_25_54>0)



communes_dates_2012_2022_temperature_final_2012<-communes_dates_2012_2022_temperature_final_2012[,c(1:3,27,37:54,56)]




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





communes_dates_2012_2022_temperature_final_2012_80_plus<-communes_dates_2012_2022_temperature_final_2012[,c(1:11,22)]

communes_dates_2012_2022_temperature_final_2012_80_plus<-filter(communes_dates_2012_2022_temperature_final_2012_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2012_2022_temperature_final_2012_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2012_temperature_deces_80_plus.csv")







##########

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




communes_dates_2013_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2013-01-01")
end_date <- as.Date("2013-12-31")

communes_dates_2013_2022_temperature_final_2013 <- communes_dates_2013_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2013_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2013_age_sexe.csv")

communes_dates_2013_2022_temperature_final_2013<-left_join(communes_dates_2013_2022_temperature_final_2013,deces.2013_age_sexe)

communes_dates_2013_2022_temperature_final_2013[is.na(communes_dates_2013_2022_temperature_final_2013)]<-0

RP_2013_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2013_age_sexe_final_2")

RP_2013_age_sexe_final_2<-RP_2013_age_sexe_final_2[,c(2:15)]

names(RP_2013_age_sexe_final_2)[names(RP_2013_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2013_2022_temperature_final_2013<-left_join(communes_dates_2013_2022_temperature_final_2013,RP_2013_age_sexe_final_2)


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



communes_dates_2013_2022_temperature_final_2013$mort_total<-communes_dates_2013_2022_temperature_final_2013$Femme+communes_dates_2013_2022_temperature_final_2013$Homme


communes_dates_2013_2022_temperature_final_2013$taux_mortalite_total<-communes_dates_2013_2022_temperature_final_2013$mort_total/communes_dates_2013_2022_temperature_final_2013$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013, communes_dates_2013_2022_temperature_final_2013$value_estimated_population>0)
communes_dates_2013_2022_temperature_final_2013<-filter(communes_dates_2013_2022_temperature_final_2013, communes_dates_2013_2022_temperature_final_2013$population_actif_25_54>0)



communes_dates_2013_2022_temperature_final_2013<-communes_dates_2013_2022_temperature_final_2013[,c(1:3,27,37:54,56)]




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





communes_dates_2013_2022_temperature_final_2013_80_plus<-communes_dates_2013_2022_temperature_final_2013[,c(1:11,22)]

communes_dates_2013_2022_temperature_final_2013_80_plus<-filter(communes_dates_2013_2022_temperature_final_2013_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2013_2022_temperature_final_2013_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2013_temperature_deces_80_plus.csv")







##########

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




communes_dates_2014_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2014-12-31")

communes_dates_2014_2022_temperature_final_2014 <- communes_dates_2014_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2014_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2014_age_sexe.csv")

communes_dates_2014_2022_temperature_final_2014<-left_join(communes_dates_2014_2022_temperature_final_2014,deces.2014_age_sexe)

communes_dates_2014_2022_temperature_final_2014[is.na(communes_dates_2014_2022_temperature_final_2014)]<-0

RP_2014_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2014_age_sexe_final_2")

RP_2014_age_sexe_final_2<-RP_2014_age_sexe_final_2[,c(2:15)]

names(RP_2014_age_sexe_final_2)[names(RP_2014_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2014_2022_temperature_final_2014<-left_join(communes_dates_2014_2022_temperature_final_2014,RP_2014_age_sexe_final_2)


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



communes_dates_2014_2022_temperature_final_2014$mort_total<-communes_dates_2014_2022_temperature_final_2014$Femme+communes_dates_2014_2022_temperature_final_2014$Homme


communes_dates_2014_2022_temperature_final_2014$taux_mortalite_total<-communes_dates_2014_2022_temperature_final_2014$mort_total/communes_dates_2014_2022_temperature_final_2014$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014, communes_dates_2014_2022_temperature_final_2014$value_estimated_population>0)
communes_dates_2014_2022_temperature_final_2014<-filter(communes_dates_2014_2022_temperature_final_2014, communes_dates_2014_2022_temperature_final_2014$population_actif_25_54>0)



communes_dates_2014_2022_temperature_final_2014<-communes_dates_2014_2022_temperature_final_2014[,c(1:3,27,37:54,56)]




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





communes_dates_2014_2022_temperature_final_2014_80_plus<-communes_dates_2014_2022_temperature_final_2014[,c(1:11,22)]

communes_dates_2014_2022_temperature_final_2014_80_plus<-filter(communes_dates_2014_2022_temperature_final_2014_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2014_2022_temperature_final_2014_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2014_temperature_deces_80_plus.csv")







##########

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




communes_dates_2015_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2015-12-31")

communes_dates_2015_2022_temperature_final_2015 <- communes_dates_2015_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2015_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2015_age_sexe.csv")

communes_dates_2015_2022_temperature_final_2015<-left_join(communes_dates_2015_2022_temperature_final_2015,deces.2015_age_sexe)

communes_dates_2015_2022_temperature_final_2015[is.na(communes_dates_2015_2022_temperature_final_2015)]<-0

RP_2015_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2015_age_sexe_final_2")

RP_2015_age_sexe_final_2<-RP_2015_age_sexe_final_2[,c(2:15)]

names(RP_2015_age_sexe_final_2)[names(RP_2015_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2015_2022_temperature_final_2015<-left_join(communes_dates_2015_2022_temperature_final_2015,RP_2015_age_sexe_final_2)


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



communes_dates_2015_2022_temperature_final_2015$mort_total<-communes_dates_2015_2022_temperature_final_2015$Femme+communes_dates_2015_2022_temperature_final_2015$Homme


communes_dates_2015_2022_temperature_final_2015$taux_mortalite_total<-communes_dates_2015_2022_temperature_final_2015$mort_total/communes_dates_2015_2022_temperature_final_2015$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015, communes_dates_2015_2022_temperature_final_2015$value_estimated_population>0)
communes_dates_2015_2022_temperature_final_2015<-filter(communes_dates_2015_2022_temperature_final_2015, communes_dates_2015_2022_temperature_final_2015$population_actif_25_54>0)



communes_dates_2015_2022_temperature_final_2015<-communes_dates_2015_2022_temperature_final_2015[,c(1:3,27,37:54,56)]




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





communes_dates_2015_2022_temperature_final_2015_80_plus<-communes_dates_2015_2022_temperature_final_2015[,c(1:11,22)]

communes_dates_2015_2022_temperature_final_2015_80_plus<-filter(communes_dates_2015_2022_temperature_final_2015_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2015_2022_temperature_final_2015_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2015_temperature_deces_80_plus.csv")








##########

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




communes_dates_2016_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2016-01-01")
end_date <- as.Date("2016-12-31")

communes_dates_2016_2022_temperature_final_2016 <- communes_dates_2016_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2016_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2016_age_sexe.csv")

communes_dates_2016_2022_temperature_final_2016<-left_join(communes_dates_2016_2022_temperature_final_2016,deces.2016_age_sexe)

communes_dates_2016_2022_temperature_final_2016[is.na(communes_dates_2016_2022_temperature_final_2016)]<-0

RP_2016_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2016_age_sexe_final_2")

RP_2016_age_sexe_final_2<-RP_2016_age_sexe_final_2[,c(2:15)]

names(RP_2016_age_sexe_final_2)[names(RP_2016_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2016_2022_temperature_final_2016<-left_join(communes_dates_2016_2022_temperature_final_2016,RP_2016_age_sexe_final_2)


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



communes_dates_2016_2022_temperature_final_2016$mort_total<-communes_dates_2016_2022_temperature_final_2016$Femme+communes_dates_2016_2022_temperature_final_2016$Homme


communes_dates_2016_2022_temperature_final_2016$taux_mortalite_total<-communes_dates_2016_2022_temperature_final_2016$mort_total/communes_dates_2016_2022_temperature_final_2016$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016, communes_dates_2016_2022_temperature_final_2016$value_estimated_population>0)
communes_dates_2016_2022_temperature_final_2016<-filter(communes_dates_2016_2022_temperature_final_2016, communes_dates_2016_2022_temperature_final_2016$population_actif_25_54>0)



communes_dates_2016_2022_temperature_final_2016<-communes_dates_2016_2022_temperature_final_2016[,c(1:3,27,37:54,56)]




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





communes_dates_2016_2022_temperature_final_2016_80_plus<-communes_dates_2016_2022_temperature_final_2016[,c(1:11,22)]

communes_dates_2016_2022_temperature_final_2016_80_plus<-filter(communes_dates_2016_2022_temperature_final_2016_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2016_2022_temperature_final_2016_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2016_temperature_deces_80_plus.csv")







##########

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




communes_dates_2017_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2017-01-01")
end_date <- as.Date("2017-12-31")

communes_dates_2017_2022_temperature_final_2017 <- communes_dates_2017_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2017_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2017_age_sexe.csv")

communes_dates_2017_2022_temperature_final_2017<-left_join(communes_dates_2017_2022_temperature_final_2017,deces.2017_age_sexe)

communes_dates_2017_2022_temperature_final_2017[is.na(communes_dates_2017_2022_temperature_final_2017)]<-0

RP_2017_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2017_age_sexe_final_2")

RP_2017_age_sexe_final_2<-RP_2017_age_sexe_final_2[,c(2:15)]

names(RP_2017_age_sexe_final_2)[names(RP_2017_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2017_2022_temperature_final_2017<-left_join(communes_dates_2017_2022_temperature_final_2017,RP_2017_age_sexe_final_2)


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



communes_dates_2017_2022_temperature_final_2017$mort_total<-communes_dates_2017_2022_temperature_final_2017$Femme+communes_dates_2017_2022_temperature_final_2017$Homme


communes_dates_2017_2022_temperature_final_2017$taux_mortalite_total<-communes_dates_2017_2022_temperature_final_2017$mort_total/communes_dates_2017_2022_temperature_final_2017$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017, communes_dates_2017_2022_temperature_final_2017$value_estimated_population>0)
communes_dates_2017_2022_temperature_final_2017<-filter(communes_dates_2017_2022_temperature_final_2017, communes_dates_2017_2022_temperature_final_2017$population_actif_25_54>0)



communes_dates_2017_2022_temperature_final_2017<-communes_dates_2017_2022_temperature_final_2017[,c(1:3,27,37:54,56)]




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





communes_dates_2017_2022_temperature_final_2017_80_plus<-communes_dates_2017_2022_temperature_final_2017[,c(1:11,22)]

communes_dates_2017_2022_temperature_final_2017_80_plus<-filter(communes_dates_2017_2022_temperature_final_2017_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2017_2022_temperature_final_2017_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2017_temperature_deces_80_plus.csv")







##########

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




communes_dates_2018_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2018-01-01")
end_date <- as.Date("2018-12-31")

communes_dates_2018_2022_temperature_final_2018 <- communes_dates_2018_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2018_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2018_age_sexe.csv")

communes_dates_2018_2022_temperature_final_2018<-left_join(communes_dates_2018_2022_temperature_final_2018,deces.2018_age_sexe)

communes_dates_2018_2022_temperature_final_2018[is.na(communes_dates_2018_2022_temperature_final_2018)]<-0

RP_2018_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2018_age_sexe_final_2")

RP_2018_age_sexe_final_2<-RP_2018_age_sexe_final_2[,c(2:15)]

names(RP_2018_age_sexe_final_2)[names(RP_2018_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2018_2022_temperature_final_2018<-left_join(communes_dates_2018_2022_temperature_final_2018,RP_2018_age_sexe_final_2)


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



communes_dates_2018_2022_temperature_final_2018$mort_total<-communes_dates_2018_2022_temperature_final_2018$Femme+communes_dates_2018_2022_temperature_final_2018$Homme


communes_dates_2018_2022_temperature_final_2018$taux_mortalite_total<-communes_dates_2018_2022_temperature_final_2018$mort_total/communes_dates_2018_2022_temperature_final_2018$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018, communes_dates_2018_2022_temperature_final_2018$value_estimated_population>0)
communes_dates_2018_2022_temperature_final_2018<-filter(communes_dates_2018_2022_temperature_final_2018, communes_dates_2018_2022_temperature_final_2018$population_actif_25_54>0)



communes_dates_2018_2022_temperature_final_2018<-communes_dates_2018_2022_temperature_final_2018[,c(1:3,27,37:54,56)]




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





communes_dates_2018_2022_temperature_final_2018_80_plus<-communes_dates_2018_2022_temperature_final_2018[,c(1:11,22)]

communes_dates_2018_2022_temperature_final_2018_80_plus<-filter(communes_dates_2018_2022_temperature_final_2018_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2018_2022_temperature_final_2018_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2018_temperature_deces_80_plus.csv")







##########

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




communes_dates_2019_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2019-12-31")

communes_dates_2019_2022_temperature_final_2019 <- communes_dates_2019_2022_temperature_final %>% filter(date >= start_date & date <= end_date)

deces.2019_age_sexe<-fread("/fichier deces insee/décès travaillé/deces.2019_age_sexe.csv")

communes_dates_2019_2022_temperature_final_2019<-left_join(communes_dates_2019_2022_temperature_final_2019,deces.2019_age_sexe)

communes_dates_2019_2022_temperature_final_2019[is.na(communes_dates_2019_2022_temperature_final_2019)]<-0

RP_2019_age_sexe_final_2 <- read_csv("/recensement 1980-2022/rp travaillé/RP_2019_age_sexe_final_2")

RP_2019_age_sexe_final_2<-RP_2019_age_sexe_final_2[,c(2:15)]

names(RP_2019_age_sexe_final_2)[names(RP_2019_age_sexe_final_2)=="COM_AP"]<-"COM"

communes_dates_2019_2022_temperature_final_2019<-left_join(communes_dates_2019_2022_temperature_final_2019,RP_2019_age_sexe_final_2)


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



communes_dates_2019_2022_temperature_final_2019$mort_total<-communes_dates_2019_2022_temperature_final_2019$Femme+communes_dates_2019_2022_temperature_final_2019$Homme


communes_dates_2019_2022_temperature_final_2019$taux_mortalite_total<-communes_dates_2019_2022_temperature_final_2019$mort_total/communes_dates_2019_2022_temperature_final_2019$value_estimated_population


#on enleve les communes avec des population de 0
communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019, communes_dates_2019_2022_temperature_final_2019$value_estimated_population>0)
communes_dates_2019_2022_temperature_final_2019<-filter(communes_dates_2019_2022_temperature_final_2019, communes_dates_2019_2022_temperature_final_2019$population_actif_25_54>0)



communes_dates_2019_2022_temperature_final_2019<-communes_dates_2019_2022_temperature_final_2019[,c(1:3,27,37:54,56)]




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





communes_dates_2019_2022_temperature_final_2019_80_plus<-communes_dates_2019_2022_temperature_final_2019[,c(1:11,22)]

communes_dates_2019_2022_temperature_final_2019_80_plus<-filter(communes_dates_2019_2022_temperature_final_2019_80_plus,  taux_mortalite_80_plus<=1)



fwrite(communes_dates_2019_2022_temperature_final_2019_80_plus,"/données communes années/données mortalité temperature final/communes_dates_2019_temperature_deces_80_plus.csv")





