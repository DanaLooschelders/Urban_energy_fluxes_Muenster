#source script to load flux and slow data and compare heatfluxes
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/sectorwise_heatflux_comparison.r")

#split into equally sized bins
#install.packages("funModeling")
library(funModeling)
#calculate frequencies for beton
test<-equal_freq(beton$wind_dir, 4)
#calculate frequencies for kiebitz
test2<-equal_freq(kiebitz$wind_dir, 4)
#calculate frequencies for both
test3<-equal_freq(c(kiebitz$wind_dir, beton$wind_dir), 4)
dirs<-levels(test3)
#make labels prettier
dirs<-gsub(pattern=",", replacement = " - ", dirs)
dirs<-gsub(pattern=")", replacement = "]", dirs)
#as numeric for breaks
dirs_num<-as.numeric(substr(dirs, start=nchar(dirs[1])-3, stop=nchar(dirs[1])-1))
#add 0 and 360
dirs_num<-c(0, dirs_num)
dirs_num[5]<-360
#cut wind directions according to breaks
beton$wind_sec_freq<-cut(beton$wind_dir, breaks=dirs_num, 
                         labels=dirs)
kiebitz$wind_sec_freq<-cut(kiebitz$wind_dir, 
                           breaks=dirs_num, labels=dirs)
#check
table(beton$wind_sec_freq)
table(kiebitz$wind_sec_freq)

#plot aggregated day for every wind sector
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec_freq)),], aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec_freq)),], aes(x=as.factor(hour), 
                                                                 y=H, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)\nWind sectors in [ °]")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec_freq,nrow = 2, drop=T)

#save plot
ggsave(filename = "H_Flux_diurnal_freq_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#plot aggregated day for every wind sector (4 sectors)
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec_freq)),], aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec_freq)),], aes(x=as.factor(hour), 
                                                                      y=H, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec_freq,nrow = 2, drop=T)

#save plot
ggsave(filename = "H_Flux_diurnal_freq_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#Latent heat flux
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec_freq)),], aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec_freq)),], aes(x=as.factor(hour), 
                                                                 y=LE, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec_freq,nrow = 2, drop=T)

#save plot
ggsave(filename = "LE_Flux_diurnal_8_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#Latent heat flux
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec_freq)),], aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec_freq)),], aes(x=as.factor(hour), 
                                                                      y=LE, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec_freq,nrow = 2, drop=T)

#save plot
ggsave(filename = "LE_Flux_diurnal_freq_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#plot difference against wind dir
dif$Bwindsec<-beton$wind_sec
dif$Bwindsecfreq<-beton$wind_sec_freq
dif$Kwindsec<-kiebitz$wind_sec
dif$Kwindsecfreq<-kiebitz$wind_sec_freq

#sensible heat flux difference against Beton flux with 8 wind sectors
ggplot(data=dif)+
  geom_point(aes(x=flux_dif_H, y=H_beton, color=Bwindsec))+
  theme_bw()
#sensible flux difference against Beton flux with 4 wind sectors
ggplot(data=dif)+
  geom_point(aes(x=flux_dif_H, y=H_beton, color=Bwindsecfreq))+
  theme_bw()

#flux kiebitz against Beton flux with 8 wind sectors from Beton
ggplot(data=dif)+
  geom_point(aes(H_kiebitz, y=H_beton, color=Bwindsec))+
  theme_bw()

#flux kiebitz against Beton flux with 8 wind sectors from Kiebitz
ggplot(data=dif)+
  geom_point(aes(H_kiebitz, y=H_beton, color=Kwindsec))+
  theme_bw()

#flux kiebitz against Beton flux with 4 wind sectors from Beton
ggplot(data=dif)+
  geom_point(aes(H_kiebitz, y=H_beton, color=Bwindsecfreq))+
  theme_bw()

#flux kiebitz against Beton flux with 4 wind sectors from Kiebitz
ggplot(data=dif)+
  geom_point(aes(H_kiebitz, y=H_beton, color=Kwindsecfreq))+
  theme_bw()

#plot regression between towers for 8 wind sectors from Kiebitz
ggplot(data=dif, aes(H_kiebitz, y=H_beton))+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              aes(color=Kwindsec))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()

#plot regression between towers for 4 wind sectors from Kiebitz
ggplot(data=dif, aes(H_kiebitz, y=H_beton))+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              aes(color=Kwindsecfreq))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()

#plot regression between towers for 8 wind sectors from beton
ggplot(data=dif, aes(H_kiebitz, y=H_beton))+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              aes(color=Bwindsec))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()

#plot regression between towers for 4 wind sectors from Beton
ggplot(data=dif, aes(H_kiebitz, y=H_beton))+
  geom_smooth(method=lm,se=FALSE,fullrange=TRUE,
              aes(color=Bwindsecfreq))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()
