####Concrete ####
#compare flux ratios
#take only complete observations
EB_data_concrete_complete<-concrete.flux.meteo[complete.cases(concrete.flux.meteo),]
EB_data_concrete_complete$day<-date(EB_data_concrete_complete$TIMESTAMP)
#calculate ratios
EB_data_concrete_complete$hour_num<-as.numeric(EB_data_concrete_complete$hour)
#H/Rn - whole day
mean(EB_data_concrete_complete$H/EB_data_concrete_complete$TotRNet_Avg_2)*100
#-0.1693297 -> -16%

#H/Rn - daytime
mean(EB_data_concrete_complete$H[EB_data_concrete_complete$hour_num>=7&EB_data_concrete_complete$hour_num<=18]/
       EB_data_concrete_complete$TotRNet_Avg_2[EB_data_concrete_complete$hour_num>=7&EB_data_concrete_complete$hour_num<=18],
     na.rm=T)*100
# -0.3951083 -> -39.51%
#christen: -0.5

#H/Rn - nighttime
mean(EB_data_concrete_complete$H[EB_data_concrete_complete$hour_num<=3|EB_data_concrete_complete$hour_num>=22]/
       EB_data_concrete_complete$TotRNet_Avg_2[EB_data_concrete_complete$hour_num<=3|EB_data_concrete_complete$hour_num>=22],
     na.rm=T)*100
#0.1796386 -> 17.963 %
#+0.42

#LE/Rn - whole day
mean(EB_data_concrete_complete$LE/EB_data_concrete_complete$TotRNet_Avg_2)*100
#-0.05967662 -> -5.97%

#LE/Rn -daytime
mean(EB_data_concrete_complete$LE[EB_data_concrete_complete$hour_num>=7&EB_data_concrete_complete$hour_num<=18]/
       EB_data_concrete_complete$TotRNet_Avg_2[EB_data_concrete_complete$hour_num>=7&EB_data_concrete_complete$hour_num<=18],
     na.rm=T)*100
#-0.1705955 -> -17.0595%

#LE/Rn - nighttime
mean(EB_data_concrete_complete$LE[EB_data_concrete_complete$hour_num<=3|EB_data_concrete_complete$hour_num>=22]/
       EB_data_concrete_complete$TotRNet_Avg_2[EB_data_concrete_complete$hour_num<=3|EB_data_concrete_complete$hour_num>=22],
     na.rm=T)*100
#0.09522915 -> 9.52 %

#G/Rn - whole day
mean(EB_data_concrete_complete$shf/EB_data_concrete_complete$TotRNet_Avg_2)*100
#-1.947616 -> 194.761%

#G/Rn - daytime
mean(EB_data_concrete_complete$shf[EB_data_concrete_complete$hour_num>=7&EB_data_concrete_complete$hour_num<=18]/
       EB_data_concrete_complete$TotRNet_Avg_2[EB_data_concrete_complete$hour_num>=7&EB_data_concrete_complete$hour_num<=18],
     na.rm=T)*100
#0.9313354 -> 93.1335 %

#G/Rn - nighttime
mean(EB_data_concrete_complete$shf[EB_data_concrete_complete$hour_num<=3|EB_data_concrete_complete$hour_num>=22]/
       EB_data_concrete_complete$TotRNet_Avg_2[EB_data_concrete_complete$hour_num<=3|EB_data_concrete_complete$hour_num>=22],
     na.rm=T)*100
#-5.176741 -> -517.6741 %

####Grass####
#take only complete observations
EB_data_grass_complete<-grass.flux.meteo[complete.cases(grass.flux.meteo),]
EB_data_grass_complete$day<-date(EB_data_grass_complete$TIMESTAMP)
#calculate ratios
EB_data_grass_complete$hour_num<-as.numeric(EB_data_grass_complete$hour)
#H/Rn - whole day
mean(EB_data_grass_complete$H/EB_data_grass_complete$TotRNet_Avg_2)*100
#0.02320101 -> 2.320101%

#H/Rn - daytime
mean(EB_data_grass_complete$H[EB_data_grass_complete$hour_num>=7&EB_data_grass_complete$hour_num<=18]/
       EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$hour_num>=7&EB_data_grass_complete$hour_num<=18],
     na.rm=T)*100
# -0.2346425 ->  -23.46425%

#H/Rn - nighttime
mean(EB_data_grass_complete$H[EB_data_grass_complete$hour_num<=3|EB_data_grass_complete$hour_num>=22]/
       EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$hour_num<=3|EB_data_grass_complete$hour_num>=22],
     na.rm=T)*100
#-0.3616578 -> -36.16578 %

#LE/Rn - whole day
mean(EB_data_grass_complete$LE/EB_data_grass_complete$TotRNet_Avg_2)*100
#-0.05569134 -> -5.569134%

#LE/Rn -daytime
mean(EB_data_grass_complete$LE[EB_data_grass_complete$hour_num>=7&EB_data_grass_complete$hour_num<=18]/
       EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$hour_num>=7&EB_data_grass_complete$hour_num<=18],
     na.rm=T)*100
#-0.2875537 -> -28.75537%

#LE/Rn - nighttime
mean(EB_data_grass_complete$LE[EB_data_grass_complete$hour_num<=3|EB_data_grass_complete$hour_num>=22]/
       EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$hour_num<=3|EB_data_grass_complete$hour_num>=22],
     na.rm=T)*100
#0.06700469 ->  6.700469%

#G/Rn - whole day
mean(EB_data_grass_complete$shf/EB_data_grass_complete$TotRNet_Avg_2)*100
#-1.083478-> -108.3478%

#G/Rn - daytime
mean(EB_data_grass_complete$shf[EB_data_grass_complete$hour_num>=7&EB_data_grass_complete$hour_num<=18]/
       EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$hour_num>=7&EB_data_grass_complete$hour_num<=18],
     na.rm=T)*100
#-0.141087 ->  -14.1087 %

#G/Rn - nighttime
mean(EB_data_grass_complete$shf[EB_data_grass_complete$hour_num<=3|EB_data_grass_complete$hour_num>=22]/
       EB_data_grass_complete$TotRNet_Avg_2[EB_data_grass_complete$hour_num<=3|EB_data_grass_complete$hour_num>=22],
     na.rm=T)*100
# -1.786036 -> -178.6036 %