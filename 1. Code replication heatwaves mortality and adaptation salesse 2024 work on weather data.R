



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
library(ncdf4)
library(raster)
library(rgdal)
library(sf)
library(dplyr)

# import of weather data



shapefile <- st_read("shapefile france 2022/communes-20220101.shp")

pre1.brick = brick("/tg_ens_mean_0.1deg_reg_v26.0e.nc")

shapefile<-filter(shapefile, insee<95600)

shp = st_transform(shapefile, crs(pre1.brick))

out_crop = crop(pre1.brick, extent(shp))
out_crop2 = raster::mask(out_crop,shp)

mean_value <- extract(out_crop2, shp, fun=mean)

#write.csv(mean_value, "données temperature/tg")


shp$nombre<-row.names(shp)
shp1<-shp[,c("insee","nombre","nom","surf_ha")]
shp1<-as.data.frame(shp1)
shp1<-shp1[,c("insee","nombre","nom","surf_ha")]

#write.csv(shp1, "shape_nom_communes")




###### humidity ########




shapefile <- st_read("shapefile france 2022/communes-20220101.shp")

pre1.brick = brick("/hu_ens_mean_0.1deg_reg_v26.0e.nc")

shapefile<-filter(shapefile, insee<95600)

shp = st_transform(shapefile, crs(pre1.brick))

out_crop = crop(pre1.brick, extent(shp))
out_crop2 = raster::mask(out_crop,shp)

mean_value <- extract(out_crop2, shp, fun=mean)


#write.csv(mean_value, "données temperature/hu")


shp$nombre<-row.names(shp)
shp1<-shp[,c("insee","nombre","nom","surf_ha")]
shp1<-as.data.frame(shp1)
shp1<-shp1[,c("insee","nombre","nom","surf_ha")]

#write.csv(shp1, "shape_nom_communes_hu")






###### rainfall ########




shapefile <- st_read("shapefile france 2022/communes-20220101.shp")

pre1.brick = brick("/rr_ens_mean_0.1deg_reg_v26.0e.nc")

shapefile<-filter(shapefile, insee<95600)

shp = st_transform(shapefile, crs(pre1.brick))

out_crop = crop(pre1.brick, extent(shp))
out_crop2 = raster::mask(out_crop,shp)

mean_value <- extract(out_crop2, shp, fun=mean)


#write.csv(mean_value, "données temperature/rr")


shp$nombre<-row.names(shp)
shp1<-shp[,c("insee","nombre","nom","surf_ha")]
shp1<-as.data.frame(shp1)
shp1<-shp1[,c("insee","nombre","nom","surf_ha")]

#write.csv(shp1, "shape_nom_communes_rr")








###### wind ########




rm(list = ls())
gc()





shapefile <- st_read("shapefile france 2022/communes-20220101.shp")

pre1.brick = brick("/fg_ens_mean_0.1deg_reg_v26.0e.nc")

shapefile<-filter(shapefile, insee<95600)

shp = st_transform(shapefile, crs(pre1.brick))

out_crop = crop(pre1.brick, extent(shp))
out_crop2 = raster::mask(out_crop,shp)

mean_value <- extract(out_crop2, shp, fun=mean)


#write.csv(mean_value, "données temperature/fg")


shp$nombre<-row.names(shp)
shp1<-shp[,c("insee","nombre","nom","surf_ha")]
shp1<-as.data.frame(shp1)
shp1<-shp1[,c("insee","nombre","nom","surf_ha")]

#write.csv(shp1, "shape_nom_communes_fg")






###### temperature max ########




shapefile <- st_read("shapefile france 2022/communes-20220101.shp")

pre1.brick = brick("/tx_ens_mean_0.1deg_reg_v26.0e.nc")

shapefile<-filter(shapefile, insee<95600)

shp = st_transform(shapefile, crs(pre1.brick))

out_crop = crop(pre1.brick, extent(shp))
out_crop2 = raster::mask(out_crop,shp)

mean_value <- extract(out_crop2, shp, fun=mean)


write.csv(mean_value, "données temperature/tx")


shp$nombre<-row.names(shp)
shp1<-shp[,c("insee","nombre","nom","surf_ha")]
shp1<-as.data.frame(shp1)
shp1<-shp1[,c("insee","nombre","nom","surf_ha")]

write.csv(shp1, "shape_nom_communes_tx")






#I work on temperature data E obs



temperature<-fread("/données temperature/tg")
#In this database there are 34,000 municipalities on the vertical axis and each day from 1950 to 2020 on the horizontal axis.

shape<-fread("/shape_nom_communes")
#shapefile


temp<-names(temperature)
temp<-as.data.frame(temp)

temperature<-temperature[,c(1,7307:26480)]

temperature<-left_join(temperature,shape)
temperature<-filter(temperature, !(is.na(temperature$X2022.01.01)))


temp<-names(temperature)
temp<-as.data.frame(temp)

temperature<-temperature[,c(2:19176)]

temperature <- temperature %>% gather(variable, value, -insee)
temperature$variable<-substring(temperature$variable,2,11)

temperature$variable <- gsub("\\.", "-", temperature$variable)


com_temp<-table(temperature$insee)
com_temp<-as.data.frame(com_temp)

names(com_temp)[names(com_temp)=="Var1"]<-"COM"

temperature$value <- round(temperature$value, 1)


#write.csv(temperature,"/données temperature/moyenne_temperature")


#fwrite(temperature,"/données temperature/moyenne_temperature.csv")

temperature<-fread("/données temperature/moyenne_temperature.csv")


names(temperature)[names(temperature)=="insee"]<-"COM"
names(temperature)[names(temperature)=="variable"]<-"date"

#cut into several small databases for easier data processing 
temperature1<-temperature[1:162327084,]
temperature2<-temperature[162327085:324654168,]
temperature3<-temperature[324654169:486981252,]
temperature4<-temperature[486981253:649308336,]

rm(temperature)
gc()

#fwrite(temperature1,"/données temperature/temperature1.csv")
#fwrite(temperature2,"/données temperature/temperature2.csv")
#fwrite(temperature3,"/données temperature/temperature3.csv")
#fwrite(temperature4,"/données temperature/temperature4.csv")



temperature1<-fread("/données temperature/temperature1.csv")
temperature2<-fread("/données temperature/temperature2.csv")
temperature3<-fread("/données temperature/temperature3.csv")
temperature4<-fread("/données temperature/temperature4.csv")



####

#work on municipalities
commune_2022 <- read_csv("/commune_2022.csv")
commune_2022<-filter(commune_2022, !is.na(DEP))
commune_2022<-filter(commune_2022, !(DEP=="976"|DEP=="974"|DEP=="973"|DEP=="972"|DEP=="971"))
commune_2022<-commune_2022[,c("COM","REG","DEP","LIBELLE")]
commune_2022$date<-NA
commune_2022$date<-as.character(commune_2022$date)

commune_2022<-left_join(commune_2022, com_temp)

commune_2022<-filter(commune_2022, !(is.na(Freq)))



# list of communes
communes <- commune_2022$COM




dates <- seq(as.Date('1970-01-01'), as.Date('2022-12-31'), by = 'days')
#dates<-as.data.frame(dates)

communes_dates_1970_2022<-expand.grid(communes, dates)

communes_dates_1970_2022<-as.data.frame(communes_dates_1970_2022)

communes_dates_1970_2022$Var2<-as.Date(communes_dates_1970_2022$Var2)
communes_dates_1970_2022$Var1<-as.character(communes_dates_1970_2022$Var1)

names(communes_dates_1970_2022)[names(communes_dates_1970_2022)=="Var1"]<-"COM"
names(communes_dates_1970_2022)[names(communes_dates_1970_2022)=="Var2"]<-"date"






# merge temperature 



communes_dates_1970_2022<-left_join(communes_dates_1970_2022,temperature1)
rm(temperature1, deces.1976,deces.1976_final,deces.1976_final_ag,deces.1976_spread,metadonnees_deces,temperature3,temperature4)
gc()
temperature2<-fread("/données temperature/temperature2.csv")

communes_dates_1970_2022<-left_join(communes_dates_1970_2022,temperature2, by=c("COM","date"))

#temperature3<-fread("/données temperature/temperature3.csv")

rm(temperature2)
gc()

com_test<-filter(communes_dates_1970_2022, !(is.na(communes_dates_1970_2022$value.x)) & !(is.na(communes_dates_1970_2022$value.y)))
com_test<-filter(communes_dates_1970_2022, !(is.na(communes_dates_1970_2022$value.x) &is.na(communes_dates_1970_2022$value.y)))

rm(com_test)
gc()

library(dplyr)
communes_dates_1970_2022 <- communes_dates_1970_2022 %>% 
  mutate(value = coalesce(value.x, value.y)) %>% 
  dplyr::select(-value.x, -value.y)


communes_dates_1970_2022_part1<-filter(communes_dates_1970_2022, !is.na(value))
communes_dates_1970_2022_part2<-filter(communes_dates_1970_2022, is.na(value))


#fwrite(communes_dates_1970_2022_part1,"/données communes années/communes_dates_1970_2022_temperature_part1.csv")
#fwrite(communes_dates_1970_2022_part2,"/données communes années/communes_dates_1970_2022_temperature_part2.csv")



rm()
gc()

communes_dates_1970_2022_part2<-fread("/données communes années/communes_dates_1970_2022_temperature_part2.csv")
temperature3<-fread("/données temperature/temperature3.csv")



communes_dates_1970_2022_part2<-left_join(communes_dates_1970_2022_part2,temperature3, by=c("COM","date"))


rm(temperature3)
gc()

temperature4<-fread("/données temperature/temperature4.csv")


communes_dates_1970_2022_part2<-left_join(communes_dates_1970_2022_part2,temperature4, by=c("COM","date"))


rm(temperature4)
gc()

library(dplyr)
communes_dates_1970_2022_part2 <- communes_dates_1970_2022_part2 %>% 
  mutate(value1 = coalesce(value.y, value)) %>% 
  dplyr::select(-value.x, -value.y, -value)


#fwrite(communes_dates_1970_2022_part2,"/données communes années/communes_dates_1970_2022_temperature_part2.csv")



#fwrite(communes_dates_1970_2022,"/données communes années/communes_dates_1970_2022_temperature.csv")







communes_dates_1970_2022_temperature_part1<-fread("/données communes années/communes_dates_1970_2022_temperature_part1.csv")
communes_dates_1970_2022_temperature_part2<-fread("/données communes années/communes_dates_1970_2022_temperature_part2.csv")
names(communes_dates_1970_2022_temperature_part1)[names(communes_dates_1970_2022_temperature_part1)=="value"]<-"value1"

communes_dates_1970_2022_temperature<-rbind(communes_dates_1970_2022_temperature_part1,communes_dates_1970_2022_temperature_part2)

communes_dates_1970_2022_temperature<-filter(communes_dates_1970_2022_temperature, !is.na(value1))

fwrite(communes_dates_1970_2022_temperature,"/données communes années/communes_dates_1970_2022_temperature_final.csv")

communes_dates_1970_2022_temperature_final<-fread("/données communes années/communes_dates_1970_2022_temperature_final.csv")
communes_dates_1970_2022_temperature_final$value1 <- round(communes_dates_1970_2022_temperature_final$value1, 1)

communes_dates_1970_2022_temperature_final$date <- as.Date(communes_dates_1970_2022_temperature_final$date)


start_date <- as.Date("1980-01-01")
end_date <- as.Date("2022-12-31")

communes_dates_1970_2022_temperature_final <- communes_dates_1970_2022_temperature_final %>% filter(date >= start_date & date <= end_date)


fwrite(communes_dates_1970_2022_temperature_final,"/données communes années/communes_dates_1980_2022_temperature_final.csv")

communes_dates_1980_2022_temperature_final<-fread("/données communes années/communes_dates_1980_2022_temperature_final.csv")






