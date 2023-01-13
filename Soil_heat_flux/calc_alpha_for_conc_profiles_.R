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

#plot profiles with every xth point
plot_5th_value<-function(FO_concrete_x=FO_concrete_1, 
                         range=107:119){
  for(i in range ){
   plot(FO_concrete_x$depth, FO_concrete_x[,i],  type="l",
         main=paste(colnames(FO_concrete_plot[i])))
    points(FO_concrete_x$depth, FO_concrete_x[,i])

   Sys.sleep(1) #wait two seconds
  }
}

#plot all points but color only every fith
color_5th_value<-function(range=107:119, point=1){
  #create dataframe
  FO_concrete_temp <- FO_concrete_time
  #add index for color
  FO_concrete_temp$index<-"NO"
  FO_concrete_temp$index[seq(point, nrow(FO_concrete_temp), 5)]<-"YES" #select every 5th row
  #add depth
  FO_concrete_temp$depth<-as.numeric(substr(rownames(FO_concrete_temp),start = 2, stop=100))
  for(i in range ){
  p<-ggplot(data=FO_concrete_temp)+
      geom_point(aes(depth, as.numeric(FO_concrete_temp[,i]), col=index), size=3)+
      ggtitle(paste(colnames(FO_concrete_plot[i])))+
    geom_line(aes(depth, as.numeric(FO_concrete_temp[,i])))+
      theme_bw()
  print(p)
    Sys.sleep(3) #wait two seconds
  }
}
color_5th_value()

#calculate alpha
alpha<-function(FO_concrete_x=FO_concrete_1, 
                range=107:119){
  alpha_depth_list<-list()
  alpha_list<-list()
  for(i in 1:length(range)){
  #dT/dz
  dT_dz<-diff(FO_concrete_x[,range[i]])/ diff(FO_concrete_x$depth)
  #d2T/d2z
  d2T_dz2<-diff(dT_dz)/diff(FO_concrete_x$depth)[1:length(diff(dT_dz))] 
  #dT/dt
  dT_dt<-(FO_concrete_x[1:length(d2T_dz2),range[i]]-FO_concrete_x[1:length(d2T_dz2),range[i]+1])/600
  alpha=dT_dt/d2T_dz2
  #alpha(dT / dt) /  (d2T / dz2)
  alpha_dat<-data.frame("depth"=FO_concrete_x$depth[1:length(d2T_dz2)] , 
                        "alpha"=alpha)
  alpha_depth_list[[i]]<-alpha_dat
  alpha_list[[i]]<-alpha
  }
  names(alpha_list)<-colnames(FO_concrete_x[,range])
return(alpha_list)
}

#calculate for 1 value
alpha_x<-alpha()
#for second
alpha_x<-alpha(FO_concrete_x=FO_concrete_2)
#plot values
plot_5th_value(FO_concrete_2, x_value="2nd value")
plot_5th_value(FO_concrete_3, x_value="3rd value")
plot_5th_value(FO_concrete_4, x_value="4th value")
#plot as boxplot
boxplot(alpha_x)

range_alpha<-as.data.frame(lapply(alpha_x, range))
mean_alpha<-as.data.frame(lapply(alpha_x, mean))
