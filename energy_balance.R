#Energy Balance Calculation
#install.packages("bigleaf")
library(bigleaf)
library(ggplot2)
library(ggrepel)
library(grid)
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/heat_fluxes_with_meteorology.r")
####PREP####
#Prep data to do it for both EC towers splitting meteo.agg into kiebitz and beton
#soil is extra --> used for both
soil_heat<-dat.meteo.agg[,18:20]

#Beton
meteo_beton<-dat.meteo.agg[,1:13]
#assign new column names
colnames(meteo_beton)[2:13]<-substr(colnames(meteo_beton)[2:13],1, nchar(colnames(meteo_beton)[2:13])-6)
#add soil heat flux
meteo_beton<-cbind(meteo_beton, soil_heat)

#Kiebitz
meteo_kiebitz<-dat.meteo.agg[,c(1,15:17,21:29)]
#assign new column names
colnames(meteo_kiebitz)[2:13]<-substr(colnames(meteo_kiebitz)[2:13],1, nchar(colnames(meteo_kiebitz)[2:13])-8)
#add soil heat flux
meteo_kiebitz<-cbind(meteo_kiebitz, soil_heat)

#####Station####
#If Kiebitz:
dat.flux.meteo<-cbind(dat.kiebitz.flux.meteo,meteo_kiebitz)
station="EC04 Kiebitz"

#If Beton:
dat.flux.meteo<-cbind(dat.beton.flux.meteo, meteo_beton)
station="EC02 Beton"
#####EBR####
#QAQC
##exclude fluxes with qc values >6
dat.flux.meteo$co2_flux[dat.flux.meteo$qc_co2_flux>6]<-NA
dat.flux.meteo$h2o_flux[dat.flux.meteo$qc_h2o_flux>6]<-NA
dat.flux.meteo$LE[dat.flux.meteo$qc_LE>6]<-NA
dat.flux.meteo$Tau[dat.flux.meteo$qc_Tau>6]<-NA
dat.flux.meteo$H[dat.flux.meteo$qc_H>6]<-NA

#get the first row with no NA values for the column needed
index_start=which(!is.na(rowSums(dat.flux.meteo[,c("LE", 
                                                   "H", 
                                                   "TotRNet_Avg", 
                                                   "shf_Avg.1.", 
                                                   "shf_Avg.2.", 
                                                   "shf_Avg.3.")])))[1]
index_end=length(dat.flux.meteo$TIMESTAMP)
#change timespan to time where all parameters occured
dat.flux.meteo.cut<-dat.flux.meteo[index_start:index_end,]

#Formula
#Res = Rn - G - H - E
  #Rn: net radiation (slow data)
      #incoming - outgoing rad
      #Rn=(SW_up+LW_up)-(SW_dn+LW_dn)
      #Rn=dat.flux.meteo.cut$SUp_Avg_kiebitz+dat.flux.meteo.cut$LUpCo_Avg_kiebitz-
        #dat.flux.meteo.cut$SDn_Avg_kiebitz-dat.flux.meteo.cut$LDnCo_Avg_kiebitz
      Rn=dat.flux.meteo.cut$TotRNet_Avg
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,Rn, type="l")
      abline(h = 0, col="red")
  #G: soil heat flux (slow data)
      #average the three sensors
      G<-rowMeans(dat.flux.meteo.cut[,c("shf_Avg.1.",
                                     "shf_Avg.2.",
                                     "shf_Avg.3.")])
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,G, type="l")
      abline(h = 0, col="red")
      #Φsurface = Φ0.05 m + S (flux=soil_flux + Storage)
      #storage S = (T(t1) - T(t2)).cvolumic·x/(t1 - t2)
            #T(t1) - T(t2) is the temperature difference in the measurement interval
            #x the depth of installation
            #cvolumic = ρsoil,dry·csoil,dry + Q·cvolumic,water
                  #bulk density of the dry soil ρ,
                  #specific heat capacity of dry soil
                  #the water content (on mass basis), Q,
                  #cwater, the volumic heat capacity of water
  #H: sensible heat flux (flux data)
      H<-dat.flux.meteo.cut$H
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,H, type="l")
      abline(h = 0, col="red")
  #LE: latent heat flux (flux data)
      LE<-dat.flux.meteo.cut$LE
      #plot
      plot(dat.flux.meteo.cut$TIMESTAMP,LE, type="l")
      abline(h = 0, col="red")
      
#calculate energy balance      
EB<-Rn-G-H-LE      
plot(dat.flux.meteo.cut$TIMESTAMP, EB, type="l")
abline(h = 0, col="red")

#calculate mean energy balance
mean(EB, na.rm=T)

#With Bigleaf
#prep data frame
EB_data<-data.frame("TIMESTAMP"=dat.flux.meteo.cut$TIMESTAMP,
                    "Rn"=Rn, "G"=G, "LE"=LE, "H"=H)
#Function calculates energy balance ratio EBR = sum(LE + H)/sum(Rn − G − S)
#for time steps
EB_stepwise<-energy.closure(data=EB_data,instantaneous = TRUE, G=G)
plot(dat.flux.meteo.cut$TIMESTAMP, EB_stepwise, type="l")
abline(h = 0, col="red")
#QAQC
summary(EB_stepwise)
#Find time and check data of min
dat.flux.meteo.cut$TIMESTAMP[which.min(EB_stepwise)]
EB_data[which.min(EB_stepwise),]
#find time and check data of max
dat.flux.meteo.cut$TIMESTAMP[which.max(EB_stepwise)]
EB_data[which.max(EB_stepwise),]
#for whole time span with ground heat flux
EB_whole<-energy.closure(data=EB_data, G=G, Rn=Rn, LE=LE, H=H,
                         instantaneous = FALSE)
EB_whole   

#Get percentage of energy gap
(1-EB_whole[5])*100

#for whole time span without ground heat flux
EB_noG<-energy.closure(data=EB_data,Rn=Rn, LE=LE, H=H,
                         instantaneous = FALSE)
EB_noG
#Get percentage of energy gap
(1-EB_noG[5])*100

#look into conditions at that point
#add EB to dataframe
dat.flux.meteo.cut$EB<-EB_stepwise
dat.flux.meteo.cut$index<-rownames(dat.flux.meteo.cut)
#order by EB values (decreasing)
dat.flux.meteo.cut.ordered<- dat.flux.meteo.cut[order(dat.flux.meteo.cut$EB, decreasing = TRUE), ]  # Order data descending
#write 3 max values in dataframe
EB_max3 <- head(dat.flux.meteo.cut.ordered, n=4)
#keep only necessary columns
EB_max3 <- EB_max3[,c(40,42,156,158:163,167,169:173)]
#write 3 min values in dataframe
EB_min3 <- tail(dat.flux.meteo.cut.ordered[!is.na(dat.flux.meteo.cut.ordered$EB),], n=4)
#keep only necessary columns
EB_min3 <- EB_min3[,c(40,42,156,158:163,167,169:173)]
#bind together
EB_minmax<-rbind(EB_max3,EB_min3)
####Export to csv####
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Tabellen")
write.csv2(EB_minmax, file = paste(station, "EB_minmax.csv", sep="_"))


####Plot####
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken")
#plot individual fluxes
ggplot(data=EB_data)+
  geom_line(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_line(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_line(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_line(aes(x=TIMESTAMP, y=G, color="G"))+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
ggsave(filename=paste("EB_components", station,".pdf"), 
       width=30, height=20, units = "cm")
#plot EB
ggplot()+
  geom_line(aes(x=dat.flux.meteo.cut$TIMESTAMP, y=EB_stepwise))+
  ylab(label="EB Ratio")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Ratio of", 
                     station),
          sub="EBR = sum(LE + H)/sum(Rn − G − S)")+
  annotate("text", label=paste("Gap in %:\n",(1-EB_whole[5])*100), 
x=as.POSIXct(dat.flux.meteo.cut$TIMESTAMP[60]), 
           y=max(EB_stepwise,na.rm=T)-20, size=3)+
  geom_point(data=highlight, aes(x=x, y=y))+
  theme_bw()
ggsave(filename=paste("EB_ratio", station,".pdf"), 
       width=30, height=20, units = "cm")

#Plot to analyse minima and maxima of EB closure ratio
#create annotation dataframe (containing all the observations over the EB threshold)
highlight<-data.frame("x"=dat.flux.meteo.cut$TIMESTAMP)
highlight$y<-NA
highlight$y[as.integer(EB_minmax$index)]<-600

ggplot(data=EB_data)+
  geom_line(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_line(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_line(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_line(aes(x=TIMESTAMP, y=G, color="G"))+
  geom_point(data=highlight, aes(x=x, y=y))+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
ggsave(filename=paste("EB_ratio_with_highlights", station,".pdf"), 
       width=30, height=20, units = "cm")

#Plot only the values of every non-closure
EB_data$EB<-EB_stepwise #add EB values
EB_data$EB<-round(EB_data$EB, 1)
ggplot(data=EB_data[c(as.integer(rownames(EB_minmax))),])+
  geom_jitter(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_jitter(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_jitter(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_jitter(aes(x=TIMESTAMP, y=G, color="G"))+
  geom_text_repel(aes(x=TIMESTAMP, y=300, label=EB), 
                  size=3, angle=60)+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
grid.text("Values are \nthe EBR in %", 
          x = unit(.99, "npc"), y = unit(.15, "npc"), 
          just = c("right", "bottom"), gp = gpar(fontsize = 8))
ggsave(filename=paste("EB_ratio_only_nonclosure", station,".pdf"), 
       width=30, height=20, units = "cm")
#plot lines with nonclosure
ggplot(data=EB_data[c(as.integer(rownames(EB_minmax))),])+
  geom_jitter(aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_jitter(aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_jitter(aes(x=TIMESTAMP, y=H, color="H"))+
  geom_jitter(aes(x=TIMESTAMP, y=G, color="G"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=LE, color="LE"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=Rn, color="Rn"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=H, color="H"))+
  geom_line(data=EB_data, aes(x=TIMESTAMP, y=G, color="G"))+
  geom_text_repel(aes(x=TIMESTAMP, y=600, label=EB), size=3, angle=60)+
  ylab(label="Energy [W m^-2]")+
  xlab(label="Time")+
  ggtitle(paste("Energy Balance Components \nof", station))+
  scale_color_manual(values=c(  "#33a02c", "#1f78b4","#000000","#a6cee3"), 
                     name="Comp.\nof EB")+
  theme_bw()
grid.text("Values are gap \nof EBR in %", 
          x = unit(.99, "npc"), y = unit(.15, "npc"), 
          just = c("right", "bottom"), gp = gpar(fontsize = 8))
ggsave(filename=paste("EB_ratio_lines_nonclosure", station,".pdf"), 
       width=30, height=20, units = "cm")
