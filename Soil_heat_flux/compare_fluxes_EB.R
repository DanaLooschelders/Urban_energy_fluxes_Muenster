#compare fluxes for energy balance rations
#for 4th August
grass.flux.meteo<-cbind(dat.kiebitz.flux.meteo$LE,dat.kiebitz.flux.meteo$H, meteo_kiebitz)
colnames(grass.flux.meteo)[1:2]<-c("LE", "H")
#If Beton:
concrete.flux.meteo<-cbind(dat.beton.flux.meteo$LE,dat.beton.flux.meteo$H, meteo_beton)
colnames(concrete.flux.meteo)[1:2]<-c("LE", "H")
#check time
grass.flux.meteo$TIMESTAMP[604:651]
concrete.flux.meteo$TIMESTAMP[604:651]
#subset
grass.flux.meteo<-grass.flux.meteo[604:651,]
concrete.flux.meteo<-concrete.flux.meteo[604:651,]

#plot all fluxes of grass
ggplot(data=grass.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg, col="TotRNet_Avg"))+
  geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
  geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
  geom_line(aes(x=TIMESTAMP, y=shf, col="shf"))+
  theme_bw()
#plot all fluxes of concrete
ggplot(data=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg, col="TotRNet_Avg"))+
  geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
  geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
  geom_line(aes(x=TIMESTAMP, y=shf, col="shf"))+
  theme_bw()
##Function calculates energy balance ratio EBR = sum(LE + H)/sum(Rn − G − S)
#plot EBR components for grass
ggplot(dat=grass.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=LE+H, col="LE+H"))+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg-shf*-1, col="TotRad-shf"))+
  theme_bw()
#plot EBR components for concrete
ggplot(dat=concrete.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=LE+H, col="LE+H"))+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg-shf*-1, col="TotRad-shf"))+
  theme_bw()

#####compare fluxes between grass and concrete####
#Total Net Radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=TotRNet_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=TotRNet_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="Total Net Radiation")
#plot incoming shortwave radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=SUp_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="Incoming SW")
#check difference 
diff_SUp<-data.frame("TIMESTAMP"=grass.flux.meteo$TIMESTAMP, 
                     "diff"=grass.flux.meteo$SUp_Avg-concrete.flux.meteo$SUp_Avg)
any(grass.flux.meteo$TIMESTAMP!=concrete.flux.meteo$TIMESTAMP) #make sure timestamps fit
#plot difference
ggplot(data=diff_SUp)+
  geom_line(aes(x=TIMESTAMP, y=diff))+
  theme_bw()
#####maybe plot mean day to check patterns
#plot reflected shortwave radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=SDn_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=SDn_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="reflected SW")
#grass.flux.meteo$
#plot outpoing longwave radiation
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=LDnCo_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=LDnCo_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="reflected LW")

#Latent Heat
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=LE, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=LE, col="concrete"))+
  theme_bw()+
  ggtitle(label="Latent Heat")
grass.flux.meteo$LDnCo_Avg
#Sensible Heat
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=LDnCo_Avg, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=LDnCo_Avg, col="concrete"))+
  theme_bw()+
  ggtitle(label="LDnCo_Avg")
#Soil Heat flux
ggplot()+
  geom_line(data=grass.flux.meteo, aes(x=TIMESTAMP, y=shf, col="grass"))+
  geom_line(data=concrete.flux.meteo, aes(x=TIMESTAMP, y=shf, col="concrete"))+
  theme_bw()+
  ggtitle(label="Soil heat flux")

EB_data_complete$TIMESTAMP
#plot fluxes for every single day
plot_EBR_fluxes<-function(day=1){
  i=unique(EB_data_complete$day)[day]
  EB_data_temp<-EB_data_complete[EB_data_complete$day==i,]
  #plot fluxes
  flux_plot<-ggplot(data=EB_data_temp)+
    geom_line(aes(x=TIMESTAMP, y=Rn, col="Rn"))+
    geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
    geom_line(aes(x=TIMESTAMP, y=-G, col="G"))+
    geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
    ylab(label="Flux [W/m^-2]")+
    theme_bw()
  #calculate closure
  EBR_Foken<-data.frame("TIMESTAMP"=EB_data_temp$TIMESTAMP, "EBR"=NA)
  EBR_Foken$EBR<-EB_data_temp$Rn-EB_data_temp$H-EB_data_temp$LE-EB_data_temp$G
  #plot closure
  energy_plot<-ggplot(data=EBR_Foken)+
    geom_line(aes(x=TIMESTAMP, y=EBR))+
    theme_bw()+
    geom_hline(aes(yintercept=0), col="red")+
    annotate(geom="text", x=max(EBR_Foken$TIMESTAMP), y=100, 
             label=paste("Mean Res:  \n", round(mean(EBR_Foken$EBR), 2)))
  #print plots
  flux_plot + energy_plot + plot_layout(nrow=2)
}

plot_EBR_fluxes(day=16)
