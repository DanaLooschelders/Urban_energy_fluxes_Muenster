####pretty plots ####
#as aggregated  median_mad line for each hour with errorbars
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=shf))+
  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=shf, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2.5,  stat="summary")+
  stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4)+
  geom_hline(yintercept=0, col="black", linetype="dashed")+
  ylab(bquote('G [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual(" ", values=c("black", "#1b9e77"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "SHF_Flux_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#net radiation
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=TotRNet_Avg_2))+
  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
                   fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                             y=TotRNet_Avg_2, col="EC04 (grass)", shape="EC04 (grass)"), 
                   fun="median", stroke=2.5,  stat="summary")+
  stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4)+
  geom_hline(yintercept=0, col="black", linetype="dashed")+
  ylab(bquote('Net Radiation [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual(" ", values=c("black", "#1b9e77"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "NetRad_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#latent heat
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=LE))+
  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=LE, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2.5,  stat="summary")+
  stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4)+
  ylab(bquote('LE [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual(" ", values=c("black", "#1b9e77"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "LE_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")


#net radiation
ggplot(concrete.flux.meteo, aes(x=as.factor(hour), y=H))+
  geom_point(aes(col="EC02 (concrete)", shape="EC02 (concrete)"),stroke=2.5,
             fun = "median", stat="summary")+
  geom_point(dat=grass.flux.meteo, aes(x=as.factor(hour), 
                                       y=H, col="EC04 (grass)", shape="EC04 (grass)"), 
             fun="median", stroke=2.5,  stat="summary")+
  stat_summary(dat=concrete.flux.meteo, aes(col="EC02 (concrete)"), fun.data = "median_mad", 
               geom = "errorbar", alpha=0.8, width=0.9)+
  stat_summary(dat=grass.flux.meteo, aes(col="EC04 (grass)"),fun.data = "median_mad", 
               geom = "errorbar",alpha=0.8, width=0.4)+
  geom_hline(yintercept=0, col="black", linetype="dashed")+
  ylab(bquote('H [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_color_manual(" ", values=c("black", "#1b9e77"))+
  scale_shape_manual(" ", values=c(15, 19))+
  theme_bw()+
  theme(text = element_text(size=20), legend.position="bottom")

ggsave(filename = "H_both_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")

#stat summary with lines
grass.flux.meteo$index<-"Grass"
concrete.flux.meteo$index<-"Concrete"

grass_sub<-grass.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index")]
concrete_sub<-concrete.flux.meteo[, c("LE", "H", "TotRNet_Avg_2", "shf", "hour", "index")]
both_sub<-rbind(grass_sub, concrete_sub)

#Median day of grass fluxe
ggplot(dat=both_sub)+
  ylab(bquote('Fluxes [W' ~m^-2* ']'))+
  stat_summary(aes(x=hour, y=LE, linetype="LE", color="LE"), 
               size=0.8, fun.y=median, geom="line", show.legend=F)+
  stat_summary(aes(x=hour, y=H, linetype="H", color="H"), fun.y=median, 
               size=0.8, geom="line", show.legend=F)+
  stat_summary(aes(x=hour, y=TotRNet_Avg_2, linetype="TotRad", color="TotRad"), 
               size=0.8, fun.y=median, geom="line", show.legend=F)+
  stat_summary(aes(x=hour, y=shf, linetype="shf", color="shf"), size=0.8, 
               fun.y=median, geom="line", show.legend=F)+
  theme_bw()+
  labs(color  = " ", linetype = " ")+
  scale_color_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c"))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
  scale_linetype_manual(values=c(1,2,3,4))+
  facet_grid(cols=vars(index))

ggsave(filename = "allFluxes_diurnal_hourly.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/Thesis")
