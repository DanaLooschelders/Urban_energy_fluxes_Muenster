setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
#calculate for individual soil profile
FO_concrete_depth<-read.csv("FO_concrete_20cm.csv")
#transpose
FO_concrete_time<-data.frame(t(FO_concrete_depth))
#set time as column name
colnames(FO_concrete_time)<-FO_concrete_depth$concrete_time
#delete row with time
FO_concrete_time<-FO_concrete_time[-dim(FO_concrete_time)[1],]
#add depth to plot
FO_concrete_plot<-FO_concrete_time
FO_concrete_plot$depth<-as.numeric(substr(rownames(FO_concrete_plot),start = 2, stop=100))

#all values
for(i in 107:119 ){
  plot(FO_concrete_plot$depth, FO_concrete_plot[,i],  
       type="l", main=paste("all values - " ,colnames(FO_concrete_plot[i])))
  points(FO_concrete_plot$depth, FO_concrete_plot[,i])
  Sys.sleep(2)
}

#take every fifth point (variying starting point i)
for(i in 1:4){
  FO_concrete_temp <- FO_concrete_time[seq(i, nrow(FO_concrete_time), 5), ] #select every 5th row
  FO_concrete_temp[] <- lapply(FO_concrete_temp, as.numeric) #coerce to numeric
  FO_concrete_temp$depth<-as.numeric(substr(rownames(FO_concrete_temp),start = 2, stop=100))
  assign(paste0("FO_concrete_", i), FO_concrete_temp) #assign name to object
  rm(FO_concrete_temp)#remove object
}

#"30.07.2021  08:08:00" to "30.07.2021 10:08:00"

#plot profiles
plot_5th_value<-function(FO_concrete_x=FO_concrete_1, 
                         x_value="1st value",
                         range=107:119){
  for(i in range ){
   plot(FO_concrete_x$depth, FO_concrete_x[,i],  type="l",
         main=paste(x_value ,colnames(FO_concrete_plot[i])))
    points(FO_concrete_x$depth, FO_concrete_1[,i])

   Sys.sleep(2) #wait two seconds
  }
}

plot_5th_value()
#calculate alpha
alpha<-function(FO_concrete_x=FO_concrete_1, 
                range=107:119){
  alpha_depth_list<-list()
  alpha_list<-list()
  for(i in 1:length(range)){
  #dT/dz
  dT_dz<-diff(FO_concrete_x[,range[i]])/ diff(FO_concrete_x$depth)
  #d2T/d2z
  d2T_dz2<-diff(dT_dz)/diff(FO_concrete_x$depth)[1:7] 
  #dT/dt
  dT_dt<-(FO_concrete_x[1:7,range[i]]-FO_concrete_x[1:7,range[i]+1])/600
  alpha=dT_dt/d2T_dz2
  #alpha(dT / dt) /  (d2T / dz2)
  alpha_dat<-data.frame("depth"=FO_concrete_x$depth[1:7] , "alpha"=alpha)
  alpha_depth_list[[i]]<-alpha_dat
  alpha_list[[i]]<-alpha
  }
  names(alpha_list)<-colnames(FO_concrete_x[,range])
return(alpha_list)
}

#calculate for 1 value
alpha_1<-alpha()
#plot as boxplot
boxplot(alpha_1)

lapply(alpha_1, range)
