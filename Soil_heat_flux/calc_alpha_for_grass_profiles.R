#calculate for individual soil profile
FO_grass_depth<-read.csv("FO_grass.csv")
#transpose
FO_grass_time<-data.frame(t(FO_grass_depth))
#set time as column name
colnames(FO_grass_time)<-FO_grass_depth$grass_time
#delete row with time
FO_grass_time<-FO_grass_time[-dim(FO_grass_time)[1],]
#add depth to plot
FO_grass_plot<-FO_grass_time
FO_grass_plot$depth<-as.numeric(substr(rownames(FO_grass_plot),start = 2, stop=100))

#plot all values
for(i in 132:144 ){
  plot(FO_grass_plot$depth, FO_grass_plot[,i],  type="l", 
       main=paste("all values - " ,colnames(FO_grass_plot[i])))
  points(FO_grass_plot$depth, FO_grass_plot[,i])
  Sys.sleep(2)
}

#take every fifth point (variying starting point i)
for(i in 1:4){
  FO_grass_temp <- FO_grass_time[seq(i, nrow(FO_grass_time), 5), ] #select every 5th row
  FO_grass_temp[] <- lapply(FO_grass_temp, as.numeric) #coerce to numeric
  FO_grass_temp$depth<-as.numeric(substr(rownames(FO_grass_temp),start = 2, stop=100))
  assign(paste0("FO_grass_", i), FO_grass_temp) #assign name to object
  rm(FO_grass_temp)#remove object
}

#"30.07.2021  08:08:00" to "30.07.2021 10:08:00"

#plot profiles
#first value
for(i in 132:144 ){
  plot(FO_grass_1$depth, FO_grass_1[,i],  type="l", main="1st value")
  Sys.sleep(2)
}
str(FO_grass_1)
#dT/dz
dT_dz<-diff(FO_grass_1$`2021-07-29 14:28:00`)/ diff(FO_grass_1$depth)
#d2T/d2z
diff(dT_dz)/diff(FO_grass_1$depth)[1:4] 
#dT/dt

#alpha(dT / dt) /  (d2T / dz2)

#second value
for(i in 132:144 ){
  plot(FO_grass_2$depth, FO_grass_2[,i], type="l", main="1st value")
  Sys.sleep(2)
}
