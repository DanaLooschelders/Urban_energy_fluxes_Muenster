#load libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(Hmisc)

#this script works for both EC02 and EC04 
#just specify which one and outcomment the other option
#tower=beton
#towername="Beton"

tower=kiebitz
towername="Kiebitz"

#source script to load flux data from EC02 and EC04
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/prep_flux_data.R")

####data exploration####

##CO2 Flux
ggplot(dat=tower, aes(x=datetime, y=co2_flux))+
  geom_line(aes(col=qc_co2_flux))+
  scale_color_binned(breaks=c(1:6), name="Quality \nFlags [1:6]")+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("CO2 Flux", towername))+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("Time")+
  theme_bw()
#save plot
ggsave(filename = paste("CO2_Flux_", towername, "_QF.pdf", sep=""),
       device="pdf",width = 297, height = 210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")

#aggregated sensible heat flux
#plot mean day as boxplot
ggplot(dat=tower, aes(x=hour, y=co2_flux))+
  geom_boxplot(aes())+
  geom_hline(yintercept=0, col="red")+
  ggtitle(label=paste("Aggregated CO2 Flux", towername))+
  ylab(bquote('CO2 flux [µmol ' ~m^-2~~s^-1* ']'))+ #μmol m-2 s-1
  xlab("Hour of Day")+
  theme_bw()
#save plot
ggsave(filename = paste("CO2_Flux_diurnal_",towername, "_boxplot.pdf", 
                        sep=""),
       device="pdf",width = 297, height = 210, units = "mm",
       path = "Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")
