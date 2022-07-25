#load scripts
#source script to load flux data from EC02 and EC04 and slow data
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Meteorology/heat_fluxes_with_meteorology.r")

#calculate Bowen ratio
  #Bo=sensible/latent   
dat.beton.flux.meteo$BR_beton<-dat.beton.flux.meteo$H/dat.beton.flux.meteo$LE
dat.kiebitz.flux.meteo$BR_kiebitz<-dat.kiebitz.flux.meteo$H/dat.kiebitz.flux.meteo$LE

      #timeseries
#Beton

ggplot(data=dat.beton.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=BR_beton))+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  ylab(label="Bowen ratio")+
  xlab(label="time")+
  ggtitle(label="Bowen ratio EC02 Beton", subtitle = "Bo = H/LE")

ggsave(filename="BowenRatio_ts_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")
#Kiebitz

ggplot(data=dat.kiebitz.flux.meteo)+
  geom_line(aes(x=TIMESTAMP, y=BR_kiebitz))+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  ylab(label="Bowen ratio")+
  xlab(label="time")+
  ggtitle(label="Bowen ratio EC04 Kiebitz", subtitle = "Bo = H/LE")

ggsave(filename="BowenRatio_ts_kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm")

  #mean day
ggplot(dat=subset(dat.beton.flux.meteo, !is.na(hour)), 
              aes(x=as.factor(hour), y=BR_beton))+
  stat_summary_bin(stroke=2.5,
                   fun = "mean",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=dat.beton.flux.meteo, fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1), alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Bowen Ratio EC02", 
          subtitle = "Mean with errorbars displaying 1 SD" )+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  xlab("Hour of Day")+
  ylab("Bowen Ratio")

ggsave(filename="BowenRatio_diurnal_mean_sd_beton.pdf",
       device="pdf",width=297, height=210, units = "mm")

ggplot(dat=subset(dat.kiebitz.flux.meteo, !is.na(hour)), 
       aes(x=as.factor(hour), y=BR_kiebitz))+
  stat_summary_bin(stroke=2.5,
                   fun = "mean",geom="point", show.legend = F, na.rm = T)+
  stat_summary(dat=dat.kiebitz.flux.meteo, fun.data = "mean_sdl", 
               geom = "errorbar", fun.args = list(mult = 1), alpha=0.4, width=0.4,
               show.legend = F, na.rm=T)+
  ggtitle(label="Aggregated Bowen Ratio EC04", 
          subtitle = "Mean with errorbars displaying 1 SD" )+
  theme_bw()+
  geom_hline(aes(yintercept=0),color="red")+
  xlab("Hour of Day")+
  ylab("Bowen Ratio")

ggsave(filename="BowenRatio_diurnal_mean_sd_kiebitz.pdf",
       device="pdf",width=297, height=210, units = "mm")
  
    #overall mean
mean(dat.beton.flux.meteo$BR_beton, na.rm=T) #3.31
mean(dat.kiebitz.flux.meteo$BR_kiebitz, na.rm=T) #0.86

#calculate ratio between EC02 and EC04
  #sensible EC02/EC04
    #timeseries
    #mean
  #latent EC02/EC04
    #timeseries
    #mean