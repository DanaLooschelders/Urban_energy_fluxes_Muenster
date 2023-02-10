####Albedo####
#Foken 
#grey, dry: 0,25–0,30 
#grey, wet: 0,10–0,12
#Gras: 0,18–0,20

#Oke
#concrete:0.1 - 0.35
#grass (short to long): 0.16 - 0.26

#####Beton####
#check albedo product from rad instrument
plot(dat.beton.flux.meteo$Albedo_Avg_beton, type="l") #makes no sense to calculate for night time
#calculate albedo for only daytime
mean(dat.beton.flux.meteo$Albedo_Avg_beton[dat.beton.flux.meteo$hour_num>=8&dat.beton.flux.meteo$hour_num<=20], na.rm=T)
#mean beton: 0.16055
range(dat.beton.flux.meteo$Albedo_Avg_beton[dat.beton.flux.meteo$hour_num>=8&dat.beton.flux.meteo$hour_num<=20], na.rm=T)
#0.1083333 0.6033333

#plot albedo components
ggplot(data=dat.beton.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg_beton, col="SUp"))+
  geom_line(aes(x=TIMESTAMP, y=SDn_Avg_beton, col="SDn"))+
  theme_bw()

#calculate albedo from averaged fluxes
dat.beton.flux.meteo$albedo<-dat.beton.flux.meteo$SDn_Avg_beton/dat.beton.flux.meteo$SUp_Avg_beton
#calculate daytime mean
mean(dat.beton.flux.meteo$albedo[dat.beton.flux.meteo$hour_num>=8&dat.beton.flux.meteo$hour_num<=20], na.rm=T)
#mean beton: 0.1601458

#time series of albedo
ggplot(data=dat.beton.flux.meteo[dat.beton.flux.meteo$hour_num>=8&dat.beton.flux.meteo$hour_num<=20,])+
  geom_line(aes(x=TIMESTAMP, y=albedo))+
  theme_bw()

####Kiebitz####
#plot albedo components
ggplot(data=dat.kiebitz.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg_kiebitz, col="SUp"))+
  geom_line(aes(x=TIMESTAMP, y=SDn_Avg_kiebitz, col="SDn"))+
  theme_bw()

#mean of albedo from rad instrument product
mean(dat.kiebitz.flux.meteo$Albedo_Avg_kiebitz[dat.kiebitz.flux.meteo$hour_num>=8&dat.kiebitz.flux.meteo$hour_num<=20], na.rm=T)
#0.2106841
#plot timeseries of daytime albedo product
plot(dat.kiebitz.flux.meteo$Albedo_Avg_kiebitz[dat.kiebitz.flux.meteo$hour_num>=8&dat.kiebitz.flux.meteo$hour_num<=20], type="l")

#calculate albedo from averged fluxes
dat.kiebitz.flux.meteo$albedo<-dat.kiebitz.flux.meteo$SDn_Avg_kiebitz/dat.kiebitz.flux.meteo$SUp_Avg_kiebitz
#get daytime mean of albedo product
mean(dat.kiebitz.flux.meteo$albedo[dat.kiebitz.flux.meteo$hour_num>=8&dat.kiebitz.flux.meteo$hour_num<=20], na.rm=T)
#mean kiebitz: 0.2120945
#time series of albedo
ggplot(data=dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$hour_num>=8&dat.kiebitz.flux.meteo$hour_num<=20,])+
  geom_line(aes(x=TIMESTAMP, y=albedo))+
  theme_bw()
