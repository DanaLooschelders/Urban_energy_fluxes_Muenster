####pretty plots ####
ggplot(dat=concrete.flux.meteo, 
       aes(x=as.factor(hour), y=bowen_ratio))+
  geom_hline(aes(yintercept=1),color="black", linetype="dashed")+
  stat_summary(dat=concrete.flux.meteo, aes(x=as.factor(hour), y=bowen_ratio, col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(x=as.factor(hour), y=bowen_ratio, col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4, linewidth=1.1)+
  geom_point(dat= concrete.flux.meteo, aes(x=as.factor(hour), y=bowen_ratio, col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=bowen_ratio, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2,  stat="summary")+
  
  ggtitle(label="Aggregated Bowen Ratio EC04 and EC02", 
          subtitle = "Median with errorbars displaying median absolute deviation" )+
  theme_bw()+
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_shape_manual(" ", values=c(15, 19))+
  xlab("Hour of Day")+
  ylab("Bowen Ratio")

ggsave(filename="BowenRatio_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm", 
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#as aggregated  median_mad line for each hour with errorbars
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=shf))+
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
  scale_color_manual(" ", values=c("black", "#009A17"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "SHF_Flux_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#net radiation
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=TotRNet_Avg_2))+
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
  theme_bw()+
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "NetRad_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#latent heat
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=LE))+
 
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
  theme_bw()+
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "LE_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")


#sensible heat flux
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=H))+
  geom_hline(yintercept=0, col="black", linetype="dashed")+
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
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "H_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#stat summary with lines
grass.flux.meteo$index<-"Grass"
concrete.flux.meteo$index<-"Concrete"
#calculate surface temperature
#W=σT4 
#W=5.67*10^-8*T^4
nthroot()
install.packages("pracma")
library(pracma)
grass.flux.meteo$Surface_Temp<-nthroot(grass.flux.meteo$LDnCo_Avg/5.67*10^-8, 4)
plot(grass.flux.meteo$Surface_Temp)

grass_sub<-grass.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index" )]
concrete_sub<-concrete.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index")]
both_sub<-rbind(grass_sub, concrete_sub)

#Median day of grass fluxe
ggplot(dat=both_sub)+
  ylab(bquote('Fluxes [W' ~m^-2* ']'))+
  stat_summary(aes(x=hour, y=LE, linetype="LE", color="LE"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=H, linetype="H", color="H"), fun.y=median, 
               size=0.8, geom="line")+
  stat_summary(aes(x=hour, y=TotRNet_Avg_2, linetype="TotRad", color="TotRad"), 
               size=0.8, fun.y=median, geom="line")+
  stat_summary(aes(x=hour, y=shf, linetype="shf", color="shf"), size=0.8, 
               fun.y=median, geom="line")+
  theme_bw()+
  labs(color  = " ", linetype = " ")+
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_linetype_manual(values=c(1,2,3,4))+
  facet_grid(cols=vars(index))

ggsave(filename = "allFluxes_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#Residual over time

ggplot()+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_hline(aes(yintercept=0), col="black", linetype="dashed")+
  geom_rect( aes(xmin=-Inf, xmax=5, ymin=-Inf, ymax=Inf), alpha=.5, fill='grey')+
  geom_rect( aes(xmin=20, xmax=Inf, ymin=-Inf, ymax=Inf), alpha=.5, fill='grey')+
  geom_line(data = EBR_both,aes(x=hour, y=Res), stat="summary", fun="median")+
  ylab(bquote('Residual [W' ~m^-2* ']'))+
  theme_bw()+
  facet_grid(cols=vars(index))

ggsave(filename = "residual_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")


#maybe shade night areas? 
