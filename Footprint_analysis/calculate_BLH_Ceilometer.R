library(dplyr)
#working directories
graph_dir<-"Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken"
output_dir<-"Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/03_Rohdaten_konvertiert"
#-----------------calculate boundary layer height from Ceilometer data--------------------#
setwd(output_dir)
Ceilometer<-read.csv(file="Ceilometer_BHL.csv")
#convert timestamp
Ceilometer$timestamp<-as.POSIXct(Ceilometer$timestamp)
#Aerosols that are detected near the ground spread in the lower air layer
#upper limit can be defined as a planetary boundary layer (onshore)
#lowest aerosol layer in the boundary layer can interpreted as mixing layer height

#get to same length as beton.dat...
Ceilometer$timestamp<-as.POSIXct(Ceilometer$timestamp)

#QAQC for Quality score >6 (pbs has quality score from 1 (good) to 9 (bad))
Ceilometer$pbl_layer1[Ceilometer$pbs_layer1>6]<-NA #layer 1
Ceilometer$pbl_layer1[Ceilometer$pbs_layer1>6]<-NA #layer 2
Ceilometer$pbl_layer3[Ceilometer$pbs_layer3>6]<-NA #layer 3

#QC: 
#  1.	If the PBL1 is 300 - 500 m and has a QC of > 3, PBL2 is taken as MLH
Ceilometer$MLH<-Ceilometer$pbl_layer1
Ceilometer$MLH[Ceilometer$MLH>300&Ceilometer$MLH<500&Ceilometer$pbs_layer1>3&!is.na(Ceilometer$MLH)]<-Ceilometer$pbl_layer2[Ceilometer$MLH>300&Ceilometer$MLH<500&Ceilometer$pbs_layer1>3&!is.na(Ceilometer$MLH)]
plot(Ceilometer$timestamp, Ceilometer$MLH, type="l")
#2.	Remove all values with sky condition index not equal to 0 (rain, fog, etc)
Ceilometer$MLH[Ceilometer$sci!=0]<-NA
plot(Ceilometer$timestamp, Ceilometer$MLH, type="l")
#3.	Remove all rain values
#4.	Remove values >4km
Ceilometer$MLH[Ceilometer$MLH>4000]<-NA
plot(Ceilometer$timestamp, Ceilometer$MLH, type="l")
#5.	False measurements: 
# •	Inner and outer case temperature (no outlier, should follow regime of air temperature)
plot(Ceilometer$timestamp,Ceilometer$temp_int, type="l")
plot(Ceilometer$timestamp,Ceilometer$temp_ext, type="l")
#•	Detector and laser optic module temperature (no outliers, no extreme values)
plot(Ceilometer$timestamp,Ceilometer$temp_det, type="l")
plot(Ceilometer$timestamp,Ceilometer$temp_lom, type="l")
#•	Quality index of optics, laser and detector (> 90 %)
Ceilometer$MLH[Ceilometer$state_laser<90]<-NA
Ceilometer$MLH[Ceilometer$state_optics<90]<-NA
Ceilometer$MLH[Ceilometer$state_detector<90]<-NA
#•	The number of laser pulses averaged in one measurement (around 105.000)
plot(Ceilometer$laser_pulses, type="l")
#2.	>60% data per hour

#aggregate to halfhour
Ceilometer_agg<-Ceilometer %>%
  group_by(timestamp = cut(timestamp, breaks="30 min")) %>%
  summarize(pbl_layer1 = mean(pbl_layer1, na.rm=T),
            pbl_layer2 = mean(pbl_layer2, na.rm=T),
            pbl_layer3 = mean(pbl_layer3, na.rm=T))
#convert timestamp
Ceilometer_agg$timestamp<-as.POSIXct(Ceilometer_agg$timestamp)

#set wd
setwd(graph_dir)

#plot the three layer
ggplot(dat=Ceilometer_agg)+
  geom_line(aes(x=timestamp, y=pbl_layer1, color="Layer 1"))+
  geom_line(aes(x=timestamp, y=pbl_layer2, color="Layer 2"))+
  geom_line(aes(x=timestamp, y=pbl_layer3, color="Layer 3"))+
  theme_bw()+
  scale_color_manual(values=c("lightblue", "darkblue","black" ))

#merge to BLH together in one dataframe
BLH_merge<-inner_join(Ceilometer_agg, blh)
#calculate difference between layer 1 and calculated blh
BLH_merge$diff<-BLH_merge$pbl_layer1-BLH_merge$blh
#plot Layer 1 with calculated blh
ggplot(dat=BLH_merge)+
  geom_line(aes(x=timestamp, y=pbl_layer1, color="Ceilometer \nLayer1"))+
  geom_line(aes(x=timestamp, y=blh, color="calculated"))+
  theme_bw()+
  ylab(label="Height [m]")+
  xlab(label="Time")+
  scale_color_manual("Boundary \nLayer Height", 
                     values=c("lightblue", "darkblue"))
ggsave(filename = "BLH_calc_layer1.pdf", height=21, width=30, units="cm")

#plot Layer 2 with calculated blh
ggplot(dat=BLH_merge)+
  geom_line(aes(x=timestamp, y=pbl_layer2, color="CHM Layer2"))+
  geom_line(aes(x=timestamp, y=blh, color="calculated"))+
  theme_bw()+
  scale_color_manual(values=c("lightblue", "darkblue"))

#plot Layer 3 with calculated blh
ggplot(dat=BLH_merge)+
  geom_line(aes(x=timestamp, y=pbl_layer3, color="Ceilometer \nLayer3"))+
  geom_line(aes(x=timestamp, y=blh, color="calculated"))+
  theme_bw()+
  ylab(label="Height [m]")+
  xlab(label="Time")+
  scale_color_manual("Boundary \nLayer Height", values=c("lightblue", "darkblue"))
ggsave(filename = "BLH_calc_layer3.pdf", height=21, width=30, units="cm")

#check
#Difference between the two
mean(BLH_merge$diff, na.rm=T) #mean difference
plot(BLH_merge$diff, type="l") #plot
