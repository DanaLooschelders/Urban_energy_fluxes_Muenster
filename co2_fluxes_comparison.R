#flux differences
#####co2 - time series####

#plot EC02 and EC04 together
ggplot()+
  geom_line(dat=beton,aes(x=datetime, y=co2_flux, color="EC02 Beton"))+
  geom_line(dat=kiebitz,aes(x=datetime,y=co2_flux, color="EC04 Kiebitz"))+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="CO2 flux EC02 and EC04")+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "CO2_Flux_both.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")
#
#plot difference between EC02 and EC04
dif<-data.frame(datetime=beton$datetime, co2_beton=beton$co2_flux)
dif$co2_kiebitz<-kiebitz$co2_flux
dif$flux_dif_co2<-beton$co2_flux-kiebitz$co2_flux

##Sensible Heat Flux difference
ggplot(dat=dif, aes(x=datetime, y=flux_dif_co2))+
  geom_line()+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label="CO2 Flux Difference EC02-EC04")+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("time")+
  theme_bw()
#save plot
ggsave(filename = "CO2_Flux_dif_both.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

##integrate area under curve (for negative values)
##to quatify how much heat is going in which tower

#####sensible heat - diurnal####
#as aggregated mean line for each hour with errorbars
ggplot(beton, aes(x=as.factor(hour), y=co2_flux))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=co2_flux, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar")+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated CO2 Flux EC02 and EC04")+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "CO2_Flux_diurnal_both_errorbars_hour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for each half hour
ggplot(beton, aes(x=as.factor(time), y=co2_flux))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=co2_flux, col="kiebitz"), 
                   fun="mean", geom="point")+
  stat_summary(dat=beton, aes(col="beton"), fun.data = "mean_sdl", 
               geom = "errorbar")+
  stat_summary(dat=kiebitz, aes(col="kiebitz"),fun.data = "mean_sdl", 
               geom = "errorbar")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated CO2 Flux EC02 and EC04")+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "CO2_Flux_diurnal_both_errorbars_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for hour
ggplot(beton, aes(x=as.factor(hour), y=co2_flux))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(hour), 
                                    y=co2_flux, col="kiebitz"), 
                   fun="mean", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated CO2 Flux EC02 and EC04")+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "CO2_Flux_diurnal_both_hour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#as aggregated mean line for halfhour
ggplot(beton, aes(x=as.factor(time), y=co2_flux))+
  stat_summary_bin(aes(col="beton"),
                   fun = "mean",geom="point")+
  stat_summary_bin(dat=kiebitz, aes(x=as.factor(time), 
                                    y=co2_flux, col="kiebitz"), 
                   fun="mean", geom="point")+
  geom_hline(yintercept=0, col="black")+
  ggtitle(label="Aggregated CO2 Flux EC02 and EC04")+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = "CO2_Flux_diurnal_both_halfhour.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

