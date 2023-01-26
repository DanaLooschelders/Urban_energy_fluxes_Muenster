#functions
color_5th_value_grass<-function(range=821:835, point=1, 
                          FO_data_x=FO_grass_time){
  #add index for color
  FO_data_x$index<-"NO"
  FO_data_x$index[seq(point, nrow(FO_data_x), 4)]<-"YES" #select every 5th row
  #add depth
  FO_data_x$depth<-as.numeric(substr(rownames(FO_data_x),start = 2, stop=100))
  for(i in range ){
    p<-ggplot(data=FO_data_x)+
      geom_point(aes(depth, as.numeric(FO_data_x[,i]), col=index), size=3)+
      ggtitle(paste(colnames(FO_data_x[i])))+
      geom_vline(xintercept = 0.4722124, col="brown")+
      geom_vline(xintercept = 0.533174, col="green")+
      geom_line(aes(depth, as.numeric(FO_data_x[,i])))+
      theme_bw()
    print(p)
    Sys.sleep(3) #wait two seconds
  }
}
 
#functions
color_5th_value_concrete<-function(range=821:835, point=1, 
                                FO_data_x=FO_concrete_time){
  #add index for color
  FO_data_x$index<-"NO"
  FO_data_x$index[seq(point, nrow(FO_data_x), 4)]<-"YES" #select every 5th row
  #add depth
  FO_data_x$depth<-as.numeric(substr(rownames(FO_data_x),start = 2, stop=100))
  for(i in range ){
    p<-ggplot(data=FO_data_x)+
      geom_point(aes(depth, as.numeric(FO_data_x[,i]), col=index), size=3)+
      ggtitle(paste(colnames(FO_data_x[i])))+
      geom_vline(xintercept = 0.529, col="red")+
      geom_line(aes(depth, as.numeric(FO_data_x[,i])))+
      theme_bw()
    print(p)
    Sys.sleep(3) #wait two seconds
  }
}

#Concrete: plot profiles with every xth point
plot_5th_value_concrete<-function(FO_data_x=FO_concrete_1, 
                         range=821:835){
  for(i in range ){
    plot(FO_data_x$depth, FO_data_x[,i],  type="l",
         main=paste(colnames(FO_data_x[i])))
    abline(v=0.529, col="red")
    points(FO_data_x$depth, FO_data_x[,i])
    
    Sys.sleep(1) #wait two seconds
  }
}

#Grass: plot profiles with every xth point
plot_5th_value_grass<-function(FO_data_x=FO_concrete_1, 
                         range=821:835){
  for(i in range ){
    plot(FO_data_x$depth, FO_data_x[,i],  type="l",
         main=paste(colnames(FO_data_x[i])))
    abline(v=0.4722124, col="brown")
    abline(v=0.533174, col="green")
    points(FO_data_x$depth, FO_data_x[,i])
    
    Sys.sleep(1) #wait two seconds
  }
}

plot_meteo<-function(data=meteo_all_long, var){
  ggplot(data[data$variable==var,], aes(TIMESTAMP, value)) + 
    geom_line()+
    ylab(label=var)+
    theme_bw()
}

#calculate alpha
calc_alpha<-function(FO_data_x=FO_concrete_2, 
                range=821:835){ #701:716
  #reorder data 
  FO_data_x<-as.data.frame(apply(FO_data_x, 2, rev))
  alpha_depth_list<-list()
  alpha_list<-list()
  for(i in 1:length(range)){
    #dT/dz
    dT_dz<-diff(FO_data_x[,range[i]])/diff(FO_data_x$depth)
    #d2T/d2z
    d2T_dz2<-diff(dT_dz)/diff(FO_data_x$depth)[1:length(diff(dT_dz))] 
    #dT/dt
    dT_dt<-(FO_data_x[1:length(d2T_dz2),range[i]+1]-FO_data_x[1:length(d2T_dz2),range[i]])/600
    alpha=dT_dt/d2T_dz2
    #alpha(dT / dt) /  (d2T / dz2)
    alpha_dat<-data.frame("depth"=FO_data_x$depth[1:length(d2T_dz2)] , 
                          "alpha"=alpha)
    alpha_depth_list[[i]]<-alpha_dat
    alpha_list[[i]]<-alpha
    rm(dT_dz, d2T_dz2, dT_dt)
  }
  names(alpha_list)<-colnames(FO_data_x[,range])
  return(list(alpha_list, alpha_depth_list))
}

plot_temp_alpha<-function(FO_data_x=FO_grass_1, alpha_x=alpha_1, range=821:835){
  for(i in 1:length(alpha_x[[2]])){
    #temperature profile
    t<-ggplot(data=FO_data_x)+
      geom_line(aes(x=depth, y=as.numeric(FO_data_x[,range[i]])))+
      ylab(label="Temperature")+
      ggtitle(paste(colnames(FO_data_x[range[i]])))+
      geom_point(aes(depth, FO_data_x[,range[i]]))+
      theme_bw()
    #alpha
    a<-ggplot(data=alpha_x[[2]][[i]])+
      ggtitle(label=colnames(FO_data_x)[range])+
      geom_point(aes(depth,alpha))+
      geom_rect(mapping=aes(xmin=Inf, xmax=Inf, ymin=1e-08, 
                            ymax=1e-06), fill="red")+
      theme_bw()
    plot<-grid.arrange(t,a)
    print(plot)
    Sys.sleep(5)
  }
}

#calculate soil heat flux
rm(FO_data_x, range, k)
#calculate alpha
shf<-function(FO_data_x=FO_concrete_1, 
                range=821:835, k=2){ #701:716
  #reorder data 
  #FO_data_x<-as.data.frame(apply(FO_data_x, 2, rev))
  shf_depth_list<-list()
  shf_list<-list()
  for(i in 1:length(range)){
    #calculate dT/dz
    dT_dz<-diff(FO_data_x[,range[i]])/diff(FO_data_x$depth)
    #calculate shf
    shf<--k*dT_dz
    shf_dat<-data.frame("depth"=FO_data_x$depth[1:length(dT_dz)+1] , 
                          "shf"=shf)
    shf_depth_list[[i]]<-shf_dat
    shf_list[[i]]<-shf
    rm(dT_dz)
  }
  names(shf_list)<-colnames(FO_data_x[,range])
  return(list(shf_list, shf_depth_list))
}

plot_shf_concrete<-function(flux_dat=flux_lower){
  for(i in 1:length(flux_dat[[2]])){
    dat<-flux_dat[[2]][[i]]
    plot<-ggplot(data=dat, aes( depth, shf*-1))+
      geom_point()+
      xlab(label="height [m]")+
      ylab(label="shf [W/m^2]")+
      geom_line()+
      geom_vline(xintercept = 0.529, col="red")+
      theme_bw()+
      ggtitle(label=paste("shf - ", names(flux_dat[[1]][i])))
    print(plot)
    Sys.sleep(3)
  }
}

plot_shf_grass<-function(flux_dat=flux_lower){
  for(i in 1:length(flux_dat[[2]])){
    dat<-flux_dat[[2]][[i]]
    plot<-ggplot(data=dat, aes( depth, shf*-1))+
      geom_point()+
      xlab(label="height [m]")+
      ylab(label="shf [W/m^2]")+
      geom_line()+
      geom_vline(xintercept = 0.4722124, col="brown")+
      geom_vline(xintercept = 0.533174, col="green")+
      theme_bw()+
      ggtitle(label=paste("shf - ", names(flux_dat[[1]][i])))
    print(plot)
    Sys.sleep(3)
  }
}


#bootstrap k
bootstrap_k<-function(alpha=alpha){
#select values for specific heat capacity
#cp_soilds*rho_solids*v_solids+cp_water*rho_water*theta
#the density of water is 998 kg/m3
rho_water<- 998 #kg/m3
#density of soil soilds
#2.58 Mg m3
#2.58 *1000 -> 2580 kg m3
rho_solids=rnorm(n=1000, mean=2580, sd=5)
#hist(rho_solids)
#####cp = specific heat
#specific heat of soil solids for sandy loam soil
#from Ochsner, 2001 
cp_solids<- rnorm(n=1000, mean=801, sd=5) #J/(kg K) 
#hist(cp_solids)
#specific heat of water: 4182 J/kg/K
cp_water<- 4182 # J/kg/K
#soil water content for test days
dat.soil.merge$TIMESTAMP<-as.POSIXct(dat.soil.merge$TIMESTAMP)
dat.soil.merge$mean_VWC<-rowMeans(dat.soil.merge[,c("WC01_VWC_Avg","WC02_VWC_Avg", "WC03_VWC_Avg")], na.rm=T )
#calculate soil water content for each day
dat.soil.merge$day<-date(dat.soil.merge$TIMESTAMP)
#write in soil.dat.merge
dat.soil.merge<- dat.soil.merge %>%
  group_by(day) %>%
  mutate(daily_VWC = mean(mean_VWC))
#write in new dataframe (short)
daily_VWC<-dat.soil.merge %>%
  group_by(day) %>%
  summarise_at(vars(mean_VWC), list(VWC = mean))
#hist(daily_VWC$VWC)

#define function 
cond<-function(dat=dat, indices){
  dt<-dat[indices,]
  k<-c(dt[,1]*(dt[,2]*dt[,3]*dt[,4]+dt[,5]*dt[,6]*dt[,7]))
  return(k)
}

#create output dataframe
daily_VWC$lower_k<-NA
daily_VWC$upper_k<-NA
#bootstrap one value for every day
for (i in 1:dim(daily_VWC)[1]){
  if(!is.nan(daily_VWC$VWC[i])){
    dat_temp<-data.frame("alpha"=alpha, "rho_water"=998, 
                         "cp_water"= 4182,  "theta"=daily_VWC$VWC[i],
                         "rho_soilds"=rho_solids,
                         "cp_soilds"=cp_solids, 
                         "v_solids"= 1-daily_VWC$VWC[i])
    boot_temp<-boot::boot(data = dat_temp, statistic=cond, R=1000) #bootstrap
    ci_temp<-boot.ci(boot.out = boot_temp,  type = "perc", conf = 0.95) #calculate CI
    ci_k<-ci_temp$percent[4:5] #extract parameter
    daily_VWC$lower_k[i]<-ci_k[1] #write lower end of ci for k
    daily_VWC$upper_k[i]<-ci_k[2] #write upper end of ci for k
  }else{}
  
  }
return(daily_VWC)
}
