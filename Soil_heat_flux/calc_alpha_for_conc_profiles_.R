#calculate for individual soil profile
FO_concrete_depth<-read.csv("FO_concrete.csv")
#transpose
FO_concrete_time<-data.frame(t(FO_concrete_depth))
#set time as column name
colnames(FO_concrete_time)<-FO_concrete_depth$concrete_time
#delete row with time
FO_concrete_time<-FO_concrete_time[-dim(FO_concrete_time)[1],]
#add depth to plot
FO_concrete_plot<-FO_concrete_time
FO_concrete_plot$depth<-as.numeric(substr(rownames(FO_concrete_plot),start = 2, stop=100))

#first value
for(i in 107:119 ){
  plot(FO_concrete_plot$depth, FO_concrete_plot[,i],  
       type="l", main=paste("all values - " ,colnames(FO_concrete_plot[i])))
  Sys.sleep(2)
}

#take every fifth point (variying starting point i)
for(i in 1:4){
  FO_concrete_temp <- FO_concrete_time[seq(i, nrow(FO_concrete_time), 5), ] #select every 5th row
  FO_concrete_temp$depth<-as.numeric(substr(rownames(FO_concrete_temp),start = 2, stop=100))
  assign(paste0("FO_concrete_", i), FO_concrete_temp) #assign name to object
  rm(FO_concrete_temp)#remove object
}

#"30.07.2021  08:08:00" to "30.07.2021 10:08:00"

#plot profiles
#first value
for(i in 132:144 ){
  plot(FO_concrete_1$depth, FO_concrete_1[,i],  type="l", main="1st value")
  Sys.sleep(2)
}
#second value
for(i in 132:144 ){
  plot(FO_concrete_2$depth, FO_concrete_2[,i], type="l", main="1st value")
  Sys.sleep(2)
}
