#compare fluxes for energy balance rations
#for 4th August
grass.flux.meteo<-cbind(dat.kiebitz.flux.meteo$LE,dat.kiebitz.flux.meteo$H, meteo_kiebitz)
colnames(grass.flux.meteo)[1:2]<-c("LE", "H")
#If Beton:
concrete.flux.meteo<-cbind(dat.beton.flux.meteo$LE,dat.beton.flux.meteo$H, meteo_beton)
colnames(concrete.flux.meteo)[1:2]<-c("LE", "H")
#check time
concrete.flux.meteo$TIMESTAMP[604:651]
grass.flux.meteo$TIMESTAMP[604:651]
#subset
grass.flux.meteo<-grass.flux.meteo[604:651,]
concrete.flux.meteo<-concrete.flux.meteo[604:651,]


#plot
ggplot(data=grass.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=TotRNet_Avg, col="TotRNet_Avg"))+
  geom_line(aes(x=TIMESTAMP, y=LE, col="LE"))+
  geom_line(aes(x=TIMESTAMP, y=H, col="H"))+
  geom_line(aes(x=TIMESTAMP, y=shf, col="shf"))+
  theme_bw()

EB_day<-energy.closure(data=grass.flux.meteo, Rn = grass.flux.meteo$TotRNet_Avg, LE = grass.flux.meteo$LE, H=grass.flux.meteo$H,
                            instantaneous = TRUE, G=-grass.flux.meteo$shf)
EB_day<-data.frame("EB"=EB_day, "datetime"=grass.flux.meteo$TIMESTAMP)
#plot
ggplot(data=EB_day)+
  geom_line(aes(datetime, EB))+
  theme_bw()+
  ggtitle("Energy Balance - Concrete")+
  ylab(label="energy balance non-closure")

EB_whole<-energy.closure(data=grass.flux.meteo, Rn = grass.flux.meteo$TotRNet_Avg, LE = grass.flux.meteo$LE, H=grass.flux.meteo$H,
                        G=-grass.flux.meteo$shf)
EB_whole
