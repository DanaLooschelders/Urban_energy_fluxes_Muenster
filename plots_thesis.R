####pretty plots ####
#import font to match latex font
#install.packages("extrafont")
library(extrafont) 
library(tidyverse)

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
shf<-ggplot(concrete.flux.meteo[concrete.flux.meteo$TIMESTAMP>="2021-08-19 15:00:00",], aes(x=as.factor(hour), y=shf))+
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

concrete.flux.meteo$day<-date(concrete.flux.meteo$TIMESTAMP)
grass.flux.meteo$day<-date(grass.flux.meteo$TIMESTAMP)
day_shf<-data.frame("day"=concrete.flux.meteo$day[concrete.flux.meteo$day<="2021-08-18"], 
                    "shf"=concrete.flux.meteo$shf[concrete.flux.meteo$day<="2021-08-18"])
day_shf_gras<-data.frame("day"=grass.flux.meteo$day[grass.flux.meteo$day<="2021-08-18"], 
                         "shf"=grass.flux.meteo$shf[grass.flux.meteo$day<="2021-08-18"])
daily_agg_shf_c<-aggregate(shf~day, data=day_shf, FUN=sum)
daily_agg_shf_g<-aggregate(shf~day, data=day_shf_gras, FUN=sum)

daily_agg_shf_g$day[daily_agg_shf_g$shf/24>0]

dailysum_shf<-ggplot(daily_agg_shf_c)+
  geom_line(aes(x=day, y=shf/24, col="concrete", linetype="concrete"), size=2)+
  geom_line(data=daily_agg_shf_g, aes(x=day, y=shf/24, col="grass", linetype="grass"), size=2)+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
                   legend.position="bottom")+
  xlab(label="date")+
  scale_x_date(breaks = scales::pretty_breaks(n = 6))+
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_linetype_manual(" " ,values= c(2,1))+
  ylab(bquote('G [W' ~m^-2~h^-1*']'))+
  geom_hline(yintercept=0, col="black", linetype="dotted", size=2)

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("SHF_Flux_both_daily_sum.png", width=297, height=210, units = "mm" , res=100)
print(dailysum_shf)
dev.off()

diff_SHF<-data.frame("TIMESTAMP"=concrete.flux.meteo$TIMESTAMP, "hour"=hour(concrete.flux.meteo$TIMESTAMP),
                     "diff"=abs(concrete.flux.meteo$shf)-abs(grass.flux.meteo$shf))
ggplot(data=diff_SHF)+
  geom_line(aes(x=hour, y=diff), stat="summary", fun="median")+
  theme_bw()

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
  xlab("Hour of Day")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_summary(aes(x=hour, y=LE, linetype="LE", color="LE"), 
               size=0.8, fun.y=mean, geom="line")+
  stat_summary(aes(x=hour, y=H, linetype="H", color="H"), fun.y=mean, 
               size=0.8, geom="line")+
  stat_summary(aes(x=hour, y=TotRNet_Avg_2, linetype="Rn", color="Rn"), 
               size=0.8, fun.y=mean, geom="line")+
  stat_summary(aes(x=hour, y=G, linetype="G", color="G"), size=0.8, 
               fun.y=mean, geom="line")+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
                   legend.position="bottom")+
  labs(color  = " ", linetype = " ")+
  scale_color_manual(values=c("black", "#a6cee3", "#1f78b4", "#33a02c"))+
  scale_linetype_manual(values=c(3,1,2,4))+
  facet_grid(cols=vars(index))
###################################################

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("allFluxes_diurnal_mean_hourly.png", width=297, height=210, units = "mm" , res=100)
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
               size=0.8, fun.y=mean, geom="line")+
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

AirSurfTemp<-ggplot(dat=all_sub_melt)+
  stat_summary(aes(x=hour, y=Temp, col=index, linetype=index), 
               size=0.8, fun.y=mean, geom="line")+
  theme_bw()+
  ylab(label="Temperature [°C]")+
  scale_color_manual(" ", values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(" ", values=c(1,2,3))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")+
  facet_grid(cols=vars(ID))

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("AirSurfaceTemp_diurnal_hourly_Ver2.png", width=297, height=210, units = "mm" , res=100)
print(AirSurfTemp)
dev.off()

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

#radiation fluxes
grass_rad<-grass.flux.meteo[,c("hour", "SUp_Avg", "SDn_Avg", "LDnCo_Avg", "LUpCo_Avg")]
grass_rad$index<-"grass"
concrete_rad<-concrete.flux.meteo[,c("hour", "SUp_Avg", "SDn_Avg", "LDnCo_Avg", "LUpCo_Avg")]
concrete_rad$index<-"concrete"
steinf_rad<-dat.SS.cut[,c("hour", "SUp_Avg", "SDn_Avg", "LDnCo_Avg", "LUpCo_Avg")]
steinf_rad$index<-"rural"

all_rad<-rbind(grass_rad, concrete_rad, steinf_rad)
all_rad_melted<-melt(all_rad, id.vars=c("hour", "index"))

str(all_rad_melted)

#plot daily aggregate of fluxes
ggplot(data=all_rad_melted)+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  ylab(bquote('Radiation [W' ~m^-2* ']'))+
  theme_bw()+
  theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")+
  geom_line(aes(x=hour, y=value, group=index, col=index, linetype=index), stat="summary", fun="mean")+
  scale_color_manual(" ", values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(" ", values=c(1,2,3))+
  facet_wrap(~variable, scales="free_y", 
             labeller = as_labeller(c(SUp_Avg="SW incoming", SDn_Avg="SW reflected", 
                                      LDnCo_Avg="LW outgoing", LUpCo_Avg= "LW reflected")))

ggsave(path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis/",
       filename=paste("All_rad_daily_mean_concrete_grass_Steinf.png"),
       width=297, height=210, units = "mm")

#surface temp
ggplot(dat=all_sub)+
  stat_summary(aes(x=hour, y=SurfaceTemp, col=index, linetype=index), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  scale_color_manual(" ", values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(" ", values=c(1,2,3))
#both
ggplot(dat=all_sub_melt)+
  stat_summary(aes(x=hour, y=Temp, col=index, linetype=index), 
               size=0.8, fun.y=median, geom="line")+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 30))+
  scale_color_manual(" ", values=c( "#1f78b4", "#33a02c", "black"))+
  scale_linetype_manual(" ", values=c(1,2,3))+
  facet_grid(cols=vars(ID))


stability_1<-dat.beton.flux.meteo[, c("TIMESTAMP", "hour_num", "X.z.d..L")]
stability_1$index<-"concrete"
stability_2<-dat.kiebitz.flux.meteo[, c("TIMESTAMP", "hour_num", "X.z.d..L")]
stability_2$index<-"grass"

stability<-rbind(stability_1, stability_2)
#get into long format
means_long <- pivot_longer(stability, -c(TIMESTAMP, index, hour_num), values_to = "mean", names_to = "variable")
sd_long <- pivot_longer(stability, -c(TIMESTAMP, index, hour_num), values_to = "sd", names_to = "variable")

df_join <- means_long %>% 
  left_join(sd_long)


#plot
library(ggpubr)
stab<-ggplot(data = df_join, 
       aes(x = hour_num, group = index)) + 
  geom_line(aes(y = mean, color = index), stat="summary", fun="median", size = 2) + 
  stat_summary(aes(x=hour_num, y=mean, group=index, fill=index),geom="ribbon",
               fun.data = "median_mad",  alpha= 0.3)+
  scale_color_manual(" ", values=c( "black", "#33a02c"))+
  scale_fill_manual(" ", values=c( "black", "#33a02c"))+
  scale_linetype_manual(" ", values=c(1,2))+
  ylab(label="Monin-Obukhov stability parameter")+
  theme_bw()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")+
  xlab("Hour of Day")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("stability_median.png", width=297, height=210, units = "mm" , res=100)
print(stab)
dev.off()


#########################
#calculate gradients for different depths for concrete
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns/Agg_10min")
FO_concrete_1<-read.csv(file="temperature_profile_concrete.csv")
temp_diff_concrete<-data.frame("dT_02"= as.numeric(t(FO_concrete_1[10,1:3559]-FO_concrete_1[11,1:3559])),
                               "dT_24"= as.numeric(t(FO_concrete_1[9,1:3559]-FO_concrete_1[10,1:3559])),
                               "dT_46"= as.numeric(t(FO_concrete_1[8,1:3559]-FO_concrete_1[9,1:3559])),
                               "dT_68"= as.numeric(t(FO_concrete_1[7,1:3559]-FO_concrete_1[8,1:3559])),
                               "TIMESTAMP"= as.POSIXct(colnames(FO_concrete_1)[1:3559]), 
                               "hour"=hour(as.POSIXct(colnames(FO_concrete_1)[1:3559])),
                               "depth_02"=diff(FO_concrete_1$depth[10:11]),
                               "depth_24"=diff(FO_concrete_1$depth[9:10]),
                               "depth_46"=diff(FO_concrete_1$depth[8:9]),
                               "depth_68"=diff(FO_concrete_1$depth[8:9]),
                               "index"=rep("concrete"))
FO_grass_test<-read.csv("temperature_profile_grass.csv")

temp_diff_grass<-data.frame("dT_02"= as.numeric(t(FO_grass_3[9,1:2914]-FO_grass_3[10,1:2914])),
                            "dT_24"= as.numeric(t(FO_grass_3[8,1:2914]-FO_grass_3[9,1:2914])),
                            "dT_46"= as.numeric(t(FO_grass_3[7,1:2914]-FO_grass_3[8,1:2914])),
                            "dT_68"= as.numeric(t(FO_grass_3[6,1:2914]-FO_grass_3[7,1:2914])),
                            "TIMESTAMP"= as.POSIXct(substr(colnames(FO_grass_3), start=2, stop=30)[1:2914]), 
                            "hour"=hour(as.POSIXct(substr(colnames(FO_grass_3), start=2, stop=30)[1:2914])),
                            "depth_02"=diff(FO_grass_3$depth[9:10]),
                            "depth_24"=diff(FO_grass_3$depth[8:9]),
                            "depth_46"=diff(FO_grass_3$depth[7:8]),
                            "depth_68"=diff(FO_grass_3$depth[6:7]),
                            "index"=rep("grass"))
temp_diff_both<-rbind(temp_diff_concrete, temp_diff_grass)

ggplot(temp_diff_both)+
  geom_line(aes(x=hour, y=dT_02, color="0-2"), stat="summary", fun="mean", size=2)+
  geom_line(aes(x=hour, y=dT_24, color="2-4"), stat="summary", fun="mean", size=2)+
  geom_line(aes(x=hour, y=dT_46, color="4-6"), stat="summary", fun="mean", size=2)+
  geom_line(aes(x=hour, y=dT_68, color="6-8"), stat="summary", fun="mean", size=2)+
  theme_bw()+
  ylab(bquote(Delta~T~'[°C]'))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(text = element_text(size=30, family="LM Roman 10"), 
        legend.position="bottom")+
  xlab("Hour of Day")+
  scale_color_manual("Depth below ground [cm]", values=c("#a1dab4", "#41b6c4", "#2c7fb8", "#253494"))+
  facet_grid(cols=vars(index))

###new soil heat flux
shf_whole<-read.csv(file="shf_experiment_concrete_20230316.csv")
shf_whole$DATETIME<-as.POSIXct(shf_whole$DATETIME) #concrete
#aggregate to half hour
shf_30min <- aggregate(shf_whole$shf, 
                       list(TIMESTAMP=cut(shf_whole$DATETIME, "30 mins")),
                       mean)

shf_30min$TIMESTAMP<-as.POSIXct(shf_30min$TIMESTAMP)
range(shf_30min$TIMESTAMP)
shf_30min$hour<-hour(shf_30min$TIMESTAMP)

shf_experiment<-
  ggplot(shf_30min, aes(x=as.factor(hour), y=x))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  stat_summary(dat=shf_30min, fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  geom_point(stroke=2.5,
             fun = "median", stat="summary")+
  ylab(bquote('G [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+theme(text = element_text(size=30, family="LM Roman 10"), 
                   legend.position="bottom")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
png("G_experiment_diurnal_hourly.png", width=297, height=210, units = "mm" , res=100)
print(shf_experiment)
dev.off()
