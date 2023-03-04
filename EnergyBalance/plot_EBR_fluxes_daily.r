#####plot fluxes for every single day####
EB_data_complete<-EB_data_concrete_complete
EB_step<-EB_step_concrete
EB_day<-EB_day_concrete
meteo<-meteo_beton

EB_data_complete<-EB_data_grass_complete
EB_step<-EB_step_grass
EB_day<-EB_day_grass
meteo<-meteo_kiebitz


plot_EBR_fluxes<-function(day=1){
  i=unique(EB_data_complete$day)[day]
  EB_data_temp<-EB_data_complete[EB_data_complete$day==i,]
  #plot fluxes
  flux_plot<-ggplot(data=EB_data_temp)+
    geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg_2, col="Rn"))+
    geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
    geom_line(aes(x=TIMESTAMP, y=shf, col="G"))+
    geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
    geom_hline(aes(yintercept=0), col="black")+
    ylab(label="Flux [W/m^-2]")+
    theme_bw()
  #calculate closure
  EBR_Foken<-data.frame("TIMESTAMP"=EB_data_temp$TIMESTAMP, "EBR"=NA)
  EBR_Foken$EBR<-EB_data_temp$TotRNet_Avg_2-EB_data_temp$H-EB_data_temp$LE-EB_data_temp$shf
  #sum(EBR_Foken$EBR)/sum(EB_data_temp$Rn,EB_data_temp$H,EB_data_temp$LE,EB_data_temp$shf*-1)
  #plot closure
  energy_plot<-ggplot(data=EBR_Foken)+
    geom_line(aes(x=TIMESTAMP, y=EBR))+
    geom_point(aes(x=TIMESTAMP, y=EBR))+
    theme_bw()+
    ylab(label="Residual [W/m^2]")+
    geom_hline(aes(yintercept=0), col="red")+
    annotate(geom="text", x=max(EBR_Foken$TIMESTAMP), y=100, 
             label=paste("Day Res:  \n",  round(EB_day$Res[EB_day$day==i], 2), "[W/m^-2]"))+
    annotate(geom="text", x=min(EBR_Foken$TIMESTAMP), y=100, label="Res = Rn - H - LE - G")+
    ggtitle("EBR Foken")
  
  #bigleaf EBR plot
  EB_step_temp<-EB_step
  EB_step_temp$day<-date(EB_step_temp$datetime)
  EB_step_temp<-EB_step_temp[EB_step_temp$day==i,]
  #plot
  bigleaf_plot <- ggplot(data=EB_step_temp)+
    geom_line(aes(x=datetime, y=EB))+
    geom_point(aes(x=datetime, y=EB))+
    theme_bw()+
    ylab(label="EBR")+
    geom_hline(aes(yintercept=1), col="red")+
    annotate(geom="text", x=max(EB_step_temp$datetime), y=3, 
             label=paste("Day EBR:  \n", round(EB_day$EBR[EB_day$day==i], 2)))+
    annotate(geom="text", x=min(EB_step_temp$datetime), y=3, label="EBR=sum(LE+H)/sum(Rn-G)")
  
  #meteo plots
  meteo_plot<-ggplot(data=meteo[meteo$day==i,])+
    #geom_line(aes(x=TIMESTAMP, y=AirTC_Avg/2, color="AirTemp"))+
    geom_line(aes(x=TIMESTAMP, y=SUp_Avg))+
    ylab(label="SWUp [W m^-2]")+
    scale_y_continuous(sec.axis=sec_axis(trans=~./100, name="Precipitation [mm]"))+
    geom_bar(aes(x=TIMESTAMP, y=rain*100), stat="identity", fill="blue")+
    theme_bw()+
    scale_fill_discrete("blue")
  
  #print plots
  flux_plot + energy_plot + bigleaf_plot + meteo_plot + plot_layout(nrow=4)
  
}

plot_EBR_fluxes(day=3)
#grass: one major EBR outlier at 6am, little rain and cloudy during afternoon
#concrete:many outlier during the day
plot_EBR_fluxes(day=4)
#grass: one major EBR outlier at 6am, little rain during afternoon
#concrete: one major EBR outlier during the day, otherwise good closure
plot_EBR_fluxes(day=5)
#grass: one major EBR outlier at 7am,no rain, partly cloudy
#concrete: bad overall closure
plot_EBR_fluxes(day=6)
#grass: no major EBR outlier, minor round 6 am, no rain, overcast
#concrete: bad overall closure
plot_EBR_fluxes(day=7)
#grass: minor EBR outlier before 6 am, no rain
#concrete:good overall clousure, minor outlier in the afternoon
plot_EBR_fluxes(day=8)
#grass: minor EBR outlier after 6 am, no rain
#concrete: good overall closure
plot_EBR_fluxes(day=9)
#grass: little rain morning and evening, no outlier for EBR
#concrete:bad overall closure
plot_EBR_fluxes(day=10)
#grass: some rain during the day, mostly overcast, 
#concrete: one major outlier at 6 am, bad overall closure
plot_EBR_fluxes(day=11)
#grass: a lot of rain in the evening, mostly overcast, major EBR outlier at 6 am
#concrete: one major outlier at 6 am otherwise good overall closure
plot_EBR_fluxes(day=11)
#grass: a lot of rain in the evening, mostly overcast, major EBR outlier at 6 am, okay overall closure
#concrete: outlier at 6 am, otherwise good closure
plot_EBR_fluxes(day=12)
#grass: some rain in the morning, no major outlier
#concrete: outlier at 8 am, otherwise good closure
plot_EBR_fluxes(day=13)
#grass: some rain in the evening, major outlier at 6 am
#concrete: outlier before 6pm after rain event
plot_EBR_fluxes(day=14)
#grass: no rain, no major outliers
#concrete: major outlier at 6 am, why bad overall closure??
plot_EBR_fluxes(day=15)
#grass: no rain, major outlier at 6 am
#concrete: one major outlier at 5pm, one minor at 6 am
plot_EBR_fluxes(day=16)
#grass: no rain, minor outlier at 6 am, overcast
#concrete: no major outlier, why overall good closure?
plot_EBR_fluxes(day=17)
#grass: no rain, major outlier at 6 am, no clouds
#concrete: no major outlier, why overall good closure?
plot_EBR_fluxes(day=18)
#grass: no rain, minor outlier at 6 am, no clouds
#concrete: minor outlier around 6 am
plot_EBR_fluxes(day=19)
#grass: rain during midday, major outlier at and after 6 am, cloudy
#concrete: bad overall closure, many outliers
plot_EBR_fluxes(day=20)
#grass: rain after midday, minor outlier at 6 am, cloudy
#concrete: one major outlier at afternoon after rain event
plot_EBR_fluxes(day=21)
#grass: rain during night, major outlier at 6 am, overcast
#concrete: one major outlier at 6 am, overall good closure
plot_EBR_fluxes(day=22)
#grass: rain during the day, overcast, no outliers but overall bad closure
#concrete: overall bad closure