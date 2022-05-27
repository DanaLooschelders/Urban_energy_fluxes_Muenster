#source script to load flux and slow data and compare heatfluxes
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Flux_data/heat_flux_comparison.r")

#first visualise
#Kiebitz sensible heat flux and wind_dir
ggplot()+
  geom_line(dat=kiebitz,aes(x=datetime,y=H, color=wind_dir))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Sensible Heat EC04 with wind direction")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_kiebitz_winddir.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = plot_dir)

#Beton sensible heat flux and wind_dir
ggplot()+
  geom_line(dat=beton,aes(x=datetime, y=H, color=wind_dir))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Sensible Heat flux EC02 and wind direction")+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "H_Flux_beton_winddir.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = plot_dir)

#create 8 wind sectors
#from klimaübungsskript
rad2deg <- function(rad) {(rad * 180) / (pi)}
grenzen_6<-c(0,rad2deg(seq(0,2*pi,by=pi/2)+pi/4))
grenzen_10<-c(0,rad2deg(seq(0,2*pi,by=pi/4)+pi/8))
grenzen_10[length(grenzen_10)]<-360
grenzen_6[length(grenzen_6)]<-360
#grenzen_17<-c(0,rad2deg(seq(0,2*pi,by=pi/8)+pi/16))
#grenzen_17[length(grenzen_17)]<-360
#beton_wind<-cut(beton$wind_dir, breaks=grenzen_17, 
#                labels=c("N1", "NNO", "NO", "ONO", "O", "OSO", "SO", "SSO",
#                         "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N2"))
#cut data in sectors
beton$wind_sec_four<-cut(beton$wind_dir, breaks=grenzen_6, 
                    labels=c("N1",  "O",  "S",  "W", "N2"))

beton$wind_sec<-cut(beton$wind_dir, breaks=grenzen_10, 
                labels=c("N1", "NO", "O", "SO", "S", "SW", "W", "NW", "N2"))
#rename levels
levels(beton$wind_sec_four)<-c("N",  "O",  "S",  "W", "N")
levels(beton$wind_sec)<-c("N", "NO", "O", "SO", "S", "SW", "W", "NW", "N")
#for kiebitz
kiebitz$wind_sec_four<-cut(kiebitz$wind_dir, breaks=grenzen_6, 
                         labels=c("N1",  "O",  "S",  "W", "N2"))
kiebitz$wind_sec<-cut(kiebitz$wind_dir, breaks=grenzen_10, 
                  labels=c("N1", "NO", "O", "SO", "S", "SW", "W", "NW", "N2"))
#rename levels
levels(kiebitz$wind_sec_four)<-c("N",  "O",  "S",  "W", "N")
levels(kiebitz$wind_sec)<-c("N", "NO", "O", "SO", "S", "SW", "W", "NW", "N")
#check
table(beton$wind_sec_four)
table(kiebitz$wind_sec_four)
table(beton$wind_sec)
table(kiebitz$wind_sec)

#plot aggregated day for every wind sector
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec)),], aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec)),], aes(x=as.factor(hour), 
                                    y=H, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec,nrow = 2, drop=T)

#save plot
ggsave(filename = "H_Flux_diurnal_8_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = plot_dir)

#plot aggregated day for every wind sector (4 sectors)
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec_four)),], aes(x=as.factor(hour), y=H))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec_four)),], aes(x=as.factor(hour), 
                                                                 y=H, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Sensible Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec_four,nrow = 2, drop=T)

#save plot
ggsave(filename = "H_Flux_diurnal_4_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = plot_dir)

#Latent heat flux
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec)),], aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec)),], aes(x=as.factor(hour), 
                                                                 y=LE, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec,nrow = 2, drop=T)

#save plot
ggsave(filename = "LE_Flux_diurnal_8_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = plot_dir)

#Latent heat flux
#as aggregated mean line for hour
ggplot(beton[!(is.na(beton$wind_sec_four)),], aes(x=as.factor(hour), y=LE))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point", size=2)+
  stat_summary_bin(dat=kiebitz[!(is.na(kiebitz$wind_sec_four)),], aes(x=as.factor(hour), 
                                                                 y=LE, col="kiebitz"), 
                   fun="mean", geom="point", size=2)+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated Latent Heat Flux per Wind Sector", 
          subtitle = "Mean of EC02 (Beton) and EC04 (Kiebitz)")+
  scale_color_manual(values=c("#a6cee3","#1f78b4"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()+
  facet_wrap(~wind_sec_four,nrow = 2, drop=T)

#save plot
ggsave(filename = "LE_Flux_diurnal_4_windsector_mean_both_hour.png",
       device="png",width=297, height=210, units = "mm",
       path = plot_dir)

#plot difference against wind dir
dif$Bwindsec<-beton$wind_sec
dif$Bwindsecfour<-beton$wind_sec_four
dif$Kwindsec<-kiebitz$wind_sec
dif$Kwindsecfour<-kiebitz$wind_sec_four

#sensible heat flux difference against Beton flux with 8 wind sectors
ggplot(data=dif)+
  geom_point(aes(x=flux_dif_H, y=H_beton, color=Bwindsec))+
  theme_bw()
#sensible flux difference against Beton flux with 4 wind sectors
ggplot(data=dif)+
  geom_point(aes(x=flux_dif_H, y=H_beton, color=Bwindsecfour))+
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
  geom_point(aes(H_kiebitz, y=H_beton, color=Bwindsecfour))+
  theme_bw()

#flux kiebitz against Beton flux with 4 wind sectors from Kiebitz
ggplot(data=dif)+
  geom_point(aes(H_kiebitz, y=H_beton, color=Kwindsecfour))+
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
              aes(color=Kwindsecfour))+
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
              aes(color=Bwindsecfour))+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()
