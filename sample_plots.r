#sample plots
#temperatur, bodenoberflächentemp
grass.flux.meteo$SurfaceTemp<-nthroot(grass.flux.meteo$LDnCo_Avg/(5.67*10^-8), 4)-272
plot(grass.flux.meteo$SurfaceTemp, type="l")
concrete.flux.meteo$SurfaceTemp<-nthroot(concrete.flux.meteo$LDnCo_Avg/(5.67*10^-8), 4)-272

grass.flux.meteo$index<-"grass"
concrete.flux.meteo$index<-"concrete"

grass.flux.meteo$hour<-hour(grass.flux.meteo$TIMESTAMP)
concrete.flux.meteo$hour<-hour(concrete.flux.meteo$TIMESTAMP)

grass_sub<-grass.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index","AirTC_Avg", "SurfaceTemp" )]
concrete_sub<-concrete.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index", "AirTC_Avg", "SurfaceTemp")]
both_sub<-rbind(grass_sub, concrete_sub)

#Median day of fluxes
ggplot(dat=both_sub)+
  ylab(bquote('Fluxes [W' ~m^-2* ']'))+
  stat_summary(aes(x=hour, y=LE, linetype="LE", color="LE"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=AirTC_Avg*10, linetype="AirTC_Avg", color="AirTC_Avg"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=SurfaceTemp*10, linetype="SurfaceTemp", color="SurfaceTemp"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=H, linetype="H", color="H"), fun.y=median, 
               size=0.8, geom="line")+
  stat_summary(aes(x=hour, y=TotRNet_Avg_2, linetype="TotRad", color="TotRad"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=shf, linetype="shf", color="shf"), size=0.8, 
               fun.y=median, geom="line")+
  theme_bw()+
  labs(color  = " ", linetype = " ")+
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "black", "red"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  scale_linetype_manual(values=c(1,2,3,4, 5,6))+
  facet_grid(cols=vars(index))

#whole closure concrete
sum(EB_data_concrete_complete$LE+EB_data_concrete_complete$H)/
  sum((EB_data_concrete_complete$TotRNet_Avg_2*-1)-EB_data_concrete_complete$shf)
#0.353082

400-175
#whole closure concrete without soil heat flux
sum(EB_data_concrete_complete$LE+EB_data_concrete_complete$H)/
  sum((EB_data_concrete_complete$TotRNet_Avg_2*-1))
#0.517111

#whole closure grass
sum(EB_data_grass_complete$LE+EB_data_grass_complete$H)/
  sum((EB_data_grass_complete$TotRNet_Avg_2*-1)-EB_data_grass_complete$shf)
#0.5991308

#whole closure grass without soil heat flux
sum(EB_data_grass_complete$LE+EB_data_grass_complete$H)/
  sum((EB_data_grass_complete$TotRNet_Avg_2*-1))
#0.5956571

#whole residual concrete
sum(EB_data_concrete_complete$TotRNet_Avg_2*-1)-
  sum(EB_data_concrete_complete$LE)-
  sum(EB_data_concrete_complete$H)-
  sum(EB_data_concrete_complete$shf)
#103596.8

#whole residual grass
sum(EB_data_grass_complete$TotRNet_Avg_2*-1)-
  sum(EB_data_grass_complete$LE)-
  sum(EB_data_grass_complete$H)-
  sum(EB_data_grass_complete$shf)
#36172.78

#daily ebr grass
ggplot(data=EB_day_grass)+
  geom_line(aes(x=day,y=EBR))+
  geom_point(aes(x=day,y=EBR))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")

#daily residual grass
ggplot(data=EB_day_grass)+
  geom_line(aes(x=day,y=Res))+
  geom_point(aes(x=day,y=Res))+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")

#daily ebr concrete
ggplot(data=EB_day_concrete)+
  geom_line(aes(x=day,y=EBR))+
  geom_point(aes(x=day,y=EBR))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme_bw()+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")

#daily residual concrete
ggplot(data=EB_day_concrete)+
  geom_line(aes(x=day,y=Res))+
  geom_point(aes(x=day,y=Res))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme_bw()+
  geom_hline(aes(yintercept=1), col="red")+
  ggtitle("Daily Energy Balance")

dat.beton.flux.meteo$LE[dat.beton.flux.meteo$TIMESTAMP=="2021-08-04 11:00:00"]
dat.beton.flux.meteo$H[dat.beton.flux.meteo$TIMESTAMP=="2021-08-04 11:00:00"]

#Res = -Rn-H-LE-G
#Residual over time concrete 
EBR_Foken_concrete<-data.frame("TIMESTAMP"=EB_data_concrete_complete$TIMESTAMP, "Res"=NA)
EBR_Foken_concrete$Res<-EB_data_concrete_complete$TotRNet_Avg_2*-1-
  EB_data_concrete_complete$H-
  EB_data_concrete_complete$LE-
  EB_data_concrete_complete$shf

ggplot(data=EBR_Foken_concrete)+ #[200:248,]
  geom_line(aes(x=TIMESTAMP, y=Res))+
  theme_bw()+
  ggtitle("EB Residual")+
  geom_hline(aes(yintercept=0), col="red")
#create column with only hour
EBR_Foken_concrete$hour<-hour(EBR_Foken_concrete$TIMESTAMP)

#resiudal over time grass
EBR_Foken_grass<-data.frame("TIMESTAMP"=EB_data_grass_complete$TIMESTAMP, "Res"=NA)
EBR_Foken_grass$Res<-EB_data_grass_complete$TotRNet_Avg_2*-1-
  EB_data_grass_complete$H-
  EB_data_grass_complete$LE-
  EB_data_grass_complete$shf

ggplot(data=EBR_Foken_grass)+ #[200:248,]
  geom_line(aes(x=TIMESTAMP, y=Res))+
  theme_bw()+
  ggtitle("EB Residual")+
  geom_hline(aes(yintercept=0), col="red")
#create column with only hour
EBR_Foken_grass$hour<-hour(EBR_Foken_grass$TIMESTAMP)

EBR_Foken_concrete$index<-"concrete"
EBR_Foken_grass$index<-"grass"
EBR_both<-rbind(EBR_Foken_concrete, EBR_Foken_grass)

ggplot()+
  geom_line(data=EBR_Foken_concrete, aes(x=TIMESTAMP, y=Res, col="concrete"))+
  geom_line(data=EBR_Foken_grass, aes(x=TIMESTAMP, y=Res, col="grass"))+
  theme_bw()

ggplot()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="black", linetype="dashed")+
  geom_rect( aes(xmin=-Inf, xmax=5, ymin=-Inf, ymax=Inf), alpha=.5, fill='grey')+
  geom_rect( aes(xmin=20, xmax=Inf, ymin=-Inf, ymax=Inf), alpha=.5, fill='grey')+
  geom_line(data = EBR_both,aes(x=hour, y=Res, group=index, col=index, linetype=index), stat="summary", fun="mean")+
  ylab(bquote('Residual [W' ~m^-2* ']'))+
  scale_color_manual("", values=c("black", "#009A17"))+
  scale_linetype_manual("", values=c(2,1))+
  theme_bw()

#regression of components for concrete
ggplot(dat=concrete.flux.meteo)+
  geom_point(aes(x=TotRNet_Avg_2*-1-shf, y=LE+H))+
  theme_bw()+
  ylab(label="LE+H [W m^-2]")+
  xlab(label="-NetR-SHF [W m^-2]")

#regression of components for grass
ggplot(dat=grass.flux.meteo)+
  geom_point(aes(x=TotRNet_Avg_2*-1-shf, y=LE+H))+
  theme_bw()+
  ylab(label="LE+H [W m^-2]")+
  xlab(label="-NetR-SHF [W m^-2]")

