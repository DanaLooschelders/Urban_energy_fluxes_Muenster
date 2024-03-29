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
mean(dat.beton.flux.meteo$Albedo_Avg_beton[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13], na.rm=T)
#mean beton:0.1495431
sd(dat.beton.flux.meteo$Albedo_Avg_beton[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13], na.rm=T)
#0.01025985
range(dat.beton.flux.meteo$Albedo_Avg_beton[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13], na.rm=T)
# 0.1120000 0.1833333
hist(dat.beton.flux.meteo$Albedo_Avg_beton[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13])
#plot albedo components
ggplot(data=dat.beton.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg_beton, col="SUp"))+
  geom_line(aes(x=TIMESTAMP, y=SDn_Avg_beton, col="SDn"))+
  theme_bw()

#calculate albedo from averaged fluxes
dat.beton.flux.meteo$albedo<-dat.beton.flux.meteo$SDn_Avg_beton/dat.beton.flux.meteo$SUp_Avg_beton
#calculate daytime mean
mean(dat.beton.flux.meteo$albedo[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13], na.rm=T)
#mean beton: 0.1503788

#time series of albedo
ggplot(data=dat.beton.flux.meteo[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13,])+
  geom_line(aes(x=TIMESTAMP, y=albedo))+
  theme_bw()

####Kiebitz####
#plot albedo components
ggplot(data=dat.kiebitz.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=SUp_Avg_kiebitz, col="SUp"))+
  geom_line(aes(x=TIMESTAMP, y=SDn_Avg_kiebitz, col="SDn"))+
  theme_bw()

#mean of albedo from rad instrument product
mean(dat.kiebitz.flux.meteo$Albedo_Avg_kiebitz[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13], na.rm=T)
#0.2028889

sd(dat.kiebitz.flux.meteo$Albedo_Avg_kiebitz[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13], na.rm=T)


hist(dat.kiebitz.flux.meteo$Albedo_Avg_kiebitz[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13])
#plot timeseries of daytime albedo product
plot(dat.kiebitz.flux.meteo$Albedo_Avg_kiebitz[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13], type="l")

#calculate albedo from averged fluxes
dat.kiebitz.flux.meteo$albedo<-dat.kiebitz.flux.meteo$SDn_Avg_kiebitz/dat.kiebitz.flux.meteo$SUp_Avg_kiebitz
#get daytime mean of albedo product
mean(dat.kiebitz.flux.meteo$albedo[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13], na.rm=T)
range(dat.kiebitz.flux.meteo$albedo[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13], na.rm=T)


#mean kiebitz:  0.2045054
#time series of albedo
ggplot(data=dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13,])+
  geom_point(aes(x=as.POSIXct(date), y=albedo, col="albedo"), stat="summary", fun="mean")+
  geom_bar(data=dat.kiebitz.flux.meteo, aes(x=as.POSIXct(date), y=Rain_mm_Tot/100), stat="summary", fun="sum")+
  theme_bw()

ggplot(data=dat.beton.flux.meteo[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13,])+
  geom_point(aes(x=as.POSIXct(date), y=albedo, col="albedo"), stat="summary", fun="mean")+
  geom_bar(data=dat.beton.flux.meteo, aes(x=as.POSIXct(date), y=Rain_mm_Tot/100), stat="summary", fun="sum")+
  theme_bw()

#calculate albedo for every day
daily_rain_sum<-aggregate(data=dat.kiebitz.flux.meteo, Rain_mm_Tot~date, FUN=sum)
daily_albedo_mean<-aggregate(data=dat.kiebitz.flux.meteo[dat.kiebitz.flux.meteo$hour_num>=10&dat.kiebitz.flux.meteo$hour_num<=13,],
                             albedo~date, FUN=mean)
daily_LE_sum<-aggregate(data=dat.beton.flux.meteo, LE~date, FUN=sum)

daily_both<-full_join(daily_albedo_mean, daily_rain_sum, by="date")

ggplot(daily_both[daily_both$Rain_mm_Tot<28,])+
  geom_point(aes(x=albedo, y=Rain_mm_Tot))+
  theme_bw()

summary(lm(data = daily_both, albedo~Rain_mm_Tot)) #p-value: 0.7308

daily_both<-full_join(daily_albedo_mean, daily_LE_sum, by="date")

ggplot(daily_both)+
  geom_point(aes(x=albedo, y=LE))+
  theme_bw()

summary(lm(data = daily_both, albedo~LE)) #p:value 0.687 


range(daily_both$albedo, na.rm=T) # 0.1909904 0.2250322

#concrete
daily_rain_sum<-aggregate(data=dat.beton.flux.meteo, Rain_mm_Tot~date, FUN=sum)
daily_albedo_mean<-aggregate(data=dat.beton.flux.meteo[dat.beton.flux.meteo$hour_num>=10&dat.beton.flux.meteo$hour_num<=13,],
                             albedo~date, FUN=mean)
daily_LE_sum<-aggregate(data=dat.beton.flux.meteo, LE~date, FUN=sum)

daily_both<-full_join(daily_albedo_mean, daily_rain_sum, by="date")

ggplot(daily_both[daily_both$Rain_mm_Tot<28,])+
  geom_point(aes(x=albedo, y=Rain_mm_Tot))+
  theme_bw()

summary(lm(data = daily_both, albedo~Rain_mm_Tot)) #p-value: 0.3186

daily_both<-full_join(daily_albedo_mean, daily_LE_sum, by="date")

ggplot(daily_both)+
  geom_point(aes(x=albedo, y=LE))+
  theme_bw()

summary(lm(data = daily_both, albedo~LE)) #p-value: 0.5667

range(daily_both$albedo, na.rm=T) #0.1289948 0.1685013
