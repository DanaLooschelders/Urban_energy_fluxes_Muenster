#merge the 3 soil sensors

#aggregate soil water sensors to one mean
dat.kiebitz.flux.meteo$mean_3VWC<-rowMeans(x = dat.kiebitz.flux.meteo[,c("WC01_VWC_Avg",
                                                                        "WC02_VWC_Avg",
                                                                        "WC03_VWC_Avg")], 
                                          na.rm=T)
#calculate root mean square average
sqrt(mean(dat.kiebitz.flux.meteo$mean_3VWC^2, na.rm=T)) #0.13

dat.kiebitz.flux.meteo$mean_2VWC<-rowMeans(x = dat.kiebitz.flux.meteo[,c(#"WC01_VWC_Avg",
  "WC02_VWC_Avg",
  "WC03_VWC_Avg")], 
  na.rm=T)

#calculate root mean square average
sqrt(mean(dat.kiebitz.flux.meteo$mean_2VWC^2, na.rm=T)) #0.15

#calculate measurement error
#calculate SD between the three series -> average overall SD
rowSD <- apply(dat.kiebitz.flux.meteo[,c("WC01_VWC_Avg",
                                         "WC02_VWC_Avg",
                                         "WC03_VWC_Avg")], 1, sd)  
#calculate root mean square average
sqrt(mean(rowSD^2, na.rm=T)) #0.048m3/m3 ->48.016 mm

#calculate SD between only two series --> average overall SD
rowSD2 <- apply(dat.kiebitz.flux.meteo[,c("WC02_VWC_Avg",
                                         "WC03_VWC_Avg")], 1, sd)  
#calculate root mean square average
sqrt(mean(rowSD2^2, na.rm=T))*1000 #0.015 --> 14.71 mm

#check the difference when using both compared to only two
#calculate mean for 3
rowM3<- apply(dat.kiebitz.flux.meteo[,c("WC01_VWC_Avg",
                                        "WC02_VWC_Avg",
                                        "WC03_VWC_Avg")], 1, mean) 
#calculate mean for 2
rowM2 <- apply(dat.kiebitz.flux.meteo[,c("WC02_VWC_Avg",
                                          "WC03_VWC_Avg")], 1, mean) 
meandif<-rowM3-rowM2
mean(meandif, na.rm=T)*1000

plot(rowM3, type="l")
lines(rowM2, type="l", col="blue")
