#load libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(Hmisc)

#this script works for both EC02 and EC04 
#just specify which one and outcomment the other option
tower=beton
towername="Beton"

#tower=kiebitz
#towername="Kiebitz"

#source script to load flux data from EC02 and EC04
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/prep_flux_data.R")

####data exploration####

##Sensible Heat Flux
ggplot(dat=tower, aes(x=datetime, y=H))+
  geom_line(aes(col=qc_H))+
  scale_color_binned(breaks=c(1:6), name="Quality \nFlags [1:6]")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Sensible Heat Flux", towername))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste("H_Flux_", towername, "_QF.pdf", sep=""),
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#aggregated sensible heat flux
#plot mean day as boxplot
ggplot(dat=tower, aes(x=hour, y=H))+
  geom_boxplot(aes())+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Aggregated Sensible Heat Flux", towername))+
  ylab(bquote('Sensible heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste("H_Flux_diurnal_",towername, "_boxplot.pdf", 
                        sep=""),
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")


##Latent Heat Flux
ggplot(dat=tower, aes(x=datetime, y=LE))+
  geom_line(aes(col=qc_LE))+
  scale_color_binned(breaks=c(1:6), name="Quality \nFlags [1:6]")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Latent Heat Flux", towername))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Time")+
  theme_bw()
#save plot
ggsave(filename = paste("LE_Flux_QF_",towername,".pdf"),
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#aggregated latent heat flux
#plot mean day as boxplot
ggplot(dat=beton, aes(x=hour, y=LE))+
  geom_boxplot(aes())+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Aggregated Latent Heat Flux",towername))+
  ylab(bquote('Latent heat flux [W' ~m^-2* ']'))+
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste("LE_Flux_QF_", towername, ".pdf", sep=""),
       device="pdf",width=297, height=210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

