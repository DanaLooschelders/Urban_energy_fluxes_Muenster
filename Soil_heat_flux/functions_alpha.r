#functions
color_5th_value<-function(range=821:835, point=1){
  #create dataframe
  FO_concrete_temp <- FO_concrete_time
  #add index for color
  FO_concrete_temp$index<-"NO"
  FO_concrete_temp$index[seq(point, nrow(FO_concrete_temp), 4)]<-"YES" #select every 5th row
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
 
#plot profiles with every xth point
plot_5th_value<-function(FO_data_x=FO_concrete_1, 
                         range=821:835){
  for(i in range ){
    plot(FO_data_x$depth, FO_data_x[,i],  type="l",
         main=paste(colnames(FO_data_x[i])))
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

plot_shf<-function(flux_dat=flux_lower){
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

