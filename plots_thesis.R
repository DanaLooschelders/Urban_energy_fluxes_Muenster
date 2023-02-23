####pretty plots ####
#import font to match latex font
#install.packages("extrafont")
library(extrafont) 
# link www.fontsquirrel.com/fonts/latin-modern-roman

# execute once to add fonts:
#font_import(pattern = "lmroman*") 
#loadfonts(device="win")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

Bo<-ggplot(dat=concrete.flux.meteo, 
       aes(x=as.factor(hour), y=bowen_ratio))+
  geom_hline(aes(yintercept=1),color="black", linetype="dashed")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_summary(dat=concrete.flux.meteo, aes(x=as.factor(hour), y=bowen_ratio, col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(x=as.factor(hour), y=bowen_ratio, col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4, linewidth=1.1)+
  geom_point(dat= concrete.flux.meteo, aes(x=as.factor(hour), y=bowen_ratio, col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=bowen_ratio, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2,  stat="summary")+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
                   legend.position="bottom")+
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_shape_manual(" ", values=c(15, 19))+
  xlab("Hour of Day")+
  ylab("Bowen Ratio")

#setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("BowenRatio_both_diurnal_hourly.png", width=297, height=210, units = "mm", res=100 )
print(Bo)
dev.off()

summary(concrete.flux.meteo$shf)

summary(grass.flux.meteo$shf)
sum(abs(grass.flux.meteo$shf), na.rm=T)/sum(abs(concrete.flux.meteo$shf), na.rm=T)


#as aggregated  median_mad line for each hour with errorbars
shf<-ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=shf))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(yintercept=0, col="black", linetype="dashed")+
  stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4, linewidth=1.1)+
  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=shf, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2,  stat="summary")+
  ylab(bquote('G [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")+
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_shape_manual(" ", values=c(15, 19))

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("SHF_Flux_both_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(shf)
dev.off()

#net radiation
netrad<-ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=TotRNet_Avg_2))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(yintercept=0, col="black", linetype="dashed")+
  stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4, linewidth=1.1)+
  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=TotRNet_Avg_2, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2,  stat="summary")+
  
  ylab(bquote('Net Radiation [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png( "NetRad_both_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(netrad)
dev.off()

#latent heat
le<-ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=LE))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4, linewidth=1.1)+
  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=LE, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2,  stat="summary")+
  ylab(bquote('LE [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")
 
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("LE_both_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(le)
dev.off()

#sensible heat flux
h<-ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=H))+
  geom_hline(yintercept=0, col="black", linetype="dashed")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4, linewidth=1.1)+

  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=H, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2,  stat="summary")+
  ylab(bquote('H [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+
  theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("H_both_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(h)
dev.off()

#stat summary with lines
grass.flux.meteo$index<-"Grass"
concrete.flux.meteo$index<-"Concrete"
#calculate surface temperature
#W=σT4 
#W=5.67*10^-8*T^4

#install.packages("pracma")
library(pracma)
grass.flux.meteo$Surface_Temp<-nthroot(grass.flux.meteo$LDnCo_Avg/5.67*10^-8, 4)
plot(grass.flux.meteo$Surface_Temp)

grass_sub<-grass.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index" )]
concrete_sub<-concrete.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index")]
both_sub<-rbind(grass_sub, concrete_sub)

#Median day of grass fluxe
colnames(both_sub)[4]<-"G"

allflux<-ggplot(dat=both_sub)+
  ylab(bquote('Fluxes [W' ~m^-2* ']'))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_summary(aes(x=hour, y=LE, linetype="LE", color="LE"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=H, linetype="H", color="H"), fun.y=median, 
               size=0.8, geom="line")+
  stat_summary(aes(x=hour, y=TotRNet_Avg_2, linetype="TotRad", color="TotRad"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=G, linetype="G", color="G"), size=0.8, 
               fun.y=median, geom="line")+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
                   legend.position="bottom")+
  labs(color  = " ", linetype = " ")+
  scale_color_manual(values=c("black", "#a6cee3", "#1f78b4", "#33a02c"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_linetype_manual(values=c(3,1,2,4))+
  facet_grid(cols=vars(index))

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("allFluxes_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(allflux)
dev.off()

#Residual over time
res<-ggplot()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="grey", linetype="longdash")+
  geom_rect( aes(xmin=-Inf, xmax=5, ymin=-Inf, ymax=Inf), alpha=.5, fill='grey')+
  geom_rect( aes(xmin=20, xmax=Inf, ymin=-Inf, ymax=Inf), alpha=.5, fill='grey')+
  geom_line(data = EBR_both,aes(x=hour, y=Res, group=index, col=index, linetype=index), stat="summary", fun="mean")+
  ylab(bquote('Residual [W' ~m^-2* ']'))+
  scale_color_manual("", values=c("black", "#009A17"))+
  scale_linetype_manual("", values=c(2,1))+
  xlab("Hour of Day")+
  theme_bw()+ theme(text = element_text(size=30, family="LM Roman 10"), 
                    legend.position="bottom")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("residual_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(res)
dev.off()

#air temp and surface temp for concrete, grass and steinfurter
temp<-ggplot(dat=all_sub)+
  ylab(label="Temperature [°C]")+
  stat_summary(aes(x=hour, y=AirTC_Avg, linetype="Air", color="Air"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=SurfaceTemp, linetype="Surface", color="Surface"), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  labs(color  = " ", linetype = " ")+
  scale_color_manual(values=c( "#1f78b4", "#33a02c"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 13))+
  scale_linetype_manual(values=c(1,2))+
  theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")+
  facet_grid(cols=vars(index))

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("AirSurfaceTemp_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(temp)
dev.off()

ggplot(dat=all_sub_melt)+
  stat_summary(aes(x=hour, y=Temp, col=index, linetype=index), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  ylab(label="Temperature [°C]")+
  scale_color_manual(" ", values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(" ", values=c(1,2,3))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")+
  facet_grid(cols=vars(ID))
ggsave(filename = "AirSurfaceTemp_diurnal_hourly_Ver2.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#wind plot
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Data")
#read files
wind_beton<-read.csv(file="wind_beton.csv")
wind_kiebitz<-read.csv(file="wind_kiebitz.csv")
wind_beton$index<-"Concrete"
wind_kiebitz$index<-"Grass"

wind_both<-rbind(wind_beton, wind_kiebitz)

windplot<-ggplot(data=wind_both)+
  geom_histogram(aes(x=vector_windspeed))+
  theme_bw()+
  theme(text = element_text(size=30, family="LM Roman 10"))+
  xlab(bquote('Wind Speed [' ~ms^-1* ']'))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  facet_grid(cols=vars(index))

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("wind_hist.png", width=297, height=210, units = "mm" , res=100)
print(windplot)
dev.off()
