#soil water content and latent heat flux kiebitz
#latent heat with soil moisture
dat.kiebitz.flux.meteo$VWC_Avg<-rowMeans(dat.kiebitz.flux.meteo[,c("WC01_VWC_Avg", "WC02_VWC_Avg","WC01_VWC_Avg")])
ggplot(dat=dat.kiebitz.flux.meteo, aes(x=TIMESTAMP, y=LE))+
  geom_line()+
  geom_line(aes(y=VWC_Avg*300), color="blue", linetype="dashed")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux"))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  scale_y_continuous(sec.axis = sec_axis(trans = ~ .x/300 ,
                                         name = bquote('Volumetric water content [' *m^3* '/' *m^3* ']')))+
  theme_bw()+
  theme(axis.text.y.right = element_text(colour = "blue"))

ggsave(filename = "LE_soilmoisture_kiebitz_timeseries.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

ggsave(filename = "LE_soilmoisture_kiebitz_timeseries.png",
       device="png",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))


#test regression
summary(lm(dat.kiebitz.flux.meteo$VWC_Avg~dat.kiebitz.flux.meteo$LE))
#p-value: 0.0001932
#Adjusted R-squared:  0.0118

ggplot(data=dat.kiebitz.flux.meteo,aes(x=LE, y=VWC_Avg)) +
  geom_point() +
  geom_smooth(method='lm')+
  theme_bw()+
  ylab(bquote('Volumetric water content [' *m^3* '/' *m^3* ']'))+
  xlab(bquote('Latent heat flux [W' ~m^-2* ']'))

ggsave(filename = "LE_soilmoisture_kiebitz_regression.pdf",
       device="pdf",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))

ggsave(filename = "LE_soilmoisture_kiebitz_regression.png",
       device="png",width=297, height=210, units = "mm",
       path = paste(plot_dir,"Meteorology", sep="/"))
