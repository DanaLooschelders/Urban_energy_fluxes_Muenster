################################################################
############ Skript zum Auslesen des Bedeckungsgrads ###########
################################################################
#Modifikation, um alle Parameter auszulesen von Dana Looschelders, 10.01.2021
# modifiziert von Dana Looschelders, 06.12.2020
# Jonathan Biehl, 30.10.2019
# nach Vorlage von j_boel04

#Working directory auswÃ¤hlen
setwd("Z:/klima/Projekte/2021_CalmCity/2_Daten/9_GeoDach/Ceilometer")

#netCDF-Package installieren
#install.packages("ncdf4")
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("data.table")
#install.packages("lubridate")
library(ncdf4)
library(reshape2)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(tidyr)

#Schleife, im gewuenschte Parameter aus den .nc Dateien in einen dataframe zu schreiben
process_info_nc = function(files) {
  for (i in 1:length(files)){
   tryCatch(expr= {
    #check if file exists
    if(file.exists(files[i])==TRUE && !is.null(files[i])){
    #check if file is empty (if empty, skip)
     info=file.info(files[i])
     if(info$size!=0){
       #all parameters
       #careful! some parameters are present for 3 cloud layer
       nc_tmp = nc_open(paste0(files[i]))
       nc_latitude = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[1])
       nc_longitude = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[2])
       nc_azimuth = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[3])
       nc_zenith = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[4])  
       nc_altitude = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[5])  
       nc_wavelength = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[6])  
       nc_life_time = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[7])
       nc_range_gate = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[8])
       nc_range_gate_hr = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[9])
       nc_average_time = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[10])
       nc_laser_pulses = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[11])
       nc_error_text = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[12])
       nc_temp_int = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[13])
       nc_temp_ext = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[14])
       nc_temp_det = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[15])
       nc_temp_lom = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[16])
       nc_state_laser = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[17])
       nc_state_detector = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[18])
       nc_state_optics = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[19])
       nc_base = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[20])
       nc_stddev = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[21])
       nc_p_calc = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[22])
       nc_scaling = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[23])
       #nc_beta_raw = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[24])
       #nc_beta_raw_hr = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[25])
       nc_nn1 = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[26])
       nc_nn2 = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[27])
       nc_nn3 = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[28])
       nc_pbl = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[29])
       nc_pbs = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[30])
       nc_tcc = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[31])
       nc_bcc = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[32])
       nc_sci = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[33])
       nc_vor = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[34])
       nc_voe = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[35])
       nc_mxd = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[36])
       nc_cbh = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[37])
       nc_cbe = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[38])
       nc_cdp = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[39])
       nc_cde = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[40])
       nc_cho = ncvar_get(nc_tmp, attributes(nc_tmp$var)$names[41])
       nc_time = as.POSIXct(ncvar_get( nc_tmp, attributes(nc_tmp$dim)$names[1]), origin = "1904-01-01")
       # besser noch erg?nzen: tz="UTC"
    nc_close(nc_tmp)
    #close file
   
    nc_pbl_layer1<-nc_pbl[1,] #layer 1
    nc_pbl_layer2=nc_pbl[2,] #layer 2
    nc_pbl_layer3=nc_pbl[3,]#layer 3
    
    nc_pbs_layer1=nc_pbs[1,] #layer 1
    nc_pbs_layer2=nc_pbs[2,]#layer 2
    nc_pbs_layer3=nc_pbs[3,]#layer 3
    
    nc_cbh_layer1=nc_cbh[1,]  #layer 1
    nc_cbh_layer2=nc_cbh[2,]#layer 2
    nc_cbh_layer3=nc_cbh[3,]#layer 3
    
    nc_cbe_layer1=nc_cbe[1,] #layer 1
    nc_cbe_layer2=nc_cbe[2,]#layer 2
    nc_cbe_layer3=nc_cbe[3,]#layer 3
    
    nc_cdp_layer1=nc_cdp[1,] #layer 1
    nc_cdp_layer2=nc_cdp[2,]#layer 2
    nc_cdp_layer3=nc_cdp[3,]#layer 3
    
    nc_cde_layer1=nc_cde[1,] #layer 1
    nc_cde_layer2=nc_cde[2,]#layer 2
    nc_cde_layer3=nc_cde[3,]#layer 3
    
    #added reshape2:: after warning about depractation
    tmp_info_df = reshape2::melt(nc_error_text, value.name = "error_text")
    tmp_info_df$latitude=nc_latitude #Breitengrad des Messstandortes in °
    tmp_info_df$longitude=nc_longitude #Laengengrad des Messstandortes in °
    tmp_info_df$azimuth=nc_azimuth #der Azimutwinkel der Zeigerichtung des Lasers
    tmp_info_df$zenith=nc_zenith #der Zenitwinkel der Zeigerichtung des Lasers (genutzt, wenn eine Neigungsadapterplatte fuer 5 oder 15 Grad genutzt wird)
    tmp_info_df$altitude=nc_altitude  #Hoehe des Messgeraetes über dem Meeresspiegel 
    tmp_info_df$wavelength=nc_wavelength  #die Wellenlaenge des Lasers in nm
    tmp_info_df$life_time=nc_life_time #Betriebsdauer des Lasers in Stunden
    tmp_info_df$range_gate=nc_range_gate #die raeumliche Aufloesung der Messung
    tmp_info_df$range_gate_hr=nc_range_gate_hr 
    tmp_info_df$average_time=nc_average_time #die Zeit, ueber die gemittelt wird
    tmp_info_df$laser_pulses=nc_laser_pulses #die Anzahl der Laserpulse gemittelt von einer Messung
    tmp_info_df$temp_int = nc_temp_int #Innentemperatur in Grad Celsius
    tmp_info_df$temp_ext = nc_temp_ext #AuÃentemperatur in Grad Celsius
    tmp_info_df$temp_det = nc_temp_det #EmpfÃ¤ngertemperatur in Grad Celsius
    tmp_info_df$temp_lom = nc_temp_lom #Temperatur der Messeinheit in Grad Celsius
    tmp_info_df$state_laser = nc_state_laser #LaserqualitÃ¤t in Prozent
    tmp_info_df$state_detector = nc_state_detector #EmpfÃ¤ngerqualitÃ¤t in Prozent
    tmp_info_df$state_optics = nc_state_optics #QualitÃ¤t der Optik in Prozent (Scheibenverschmutzung)
    tmp_info_df$base = nc_base #Baseline-HÃ¶he des Rohsignals in Photonen pro Laserschuss, hauptsÃ¤chlich vom Tageslicht beeinflusst
    tmp_info_df$stddev = nc_stddev #Standardabweichung des Rohsignals in Photonen pro Laserschuss
    tmp_info_df$p_calc = nc_p_calc #Kalibrierpuls zum Normalisieren der einzelnen Einheit Ã¼ber die Zeit
    tmp_info_df$scaling= nc_scaling #Scaling factor used to normalize individual units against reference system. (called TBcalibration in RS485).
    #tmp_info_df$beta_raw = nc_beta_raw #Lidar backscatter raw data with 5 - 15 m range resolution, normalized and range corrected. lp: laser pulses, b: baseline, c:  scaling, o(r): overlap correction function,p_calc: calibration signal
    #tmp_info_df$beta_raw_hr= nc_beta_raw_hr #Lidar backscatter with 5 m resolution, raw data normalized and range corrected.
    tmp_info_df$nn1= nc_nn1 #used by manufacturer
    tmp_info_df$nn2= nc_nn2 #used by manufacturer
    tmp_info_df$nn3= nc_nn3 #used by manufacturer
    tmp_info_df$pbl_layer1=nc_pbl_layer1 #Aerosol layer calculated within the planetary boundary layer.
    tmp_info_df$pbl_layer2=nc_pbl_layer2 #Aerosol layer calculated within the planetary boundary layer.
    tmp_info_df$pbl_layer3=nc_pbl_layer3 #Aerosol layer calculated within the planetary boundary layer.
    tmp_info_df$pbs_layer1= nc_pbs_layer1 #Quality score for aerosol layers.
    tmp_info_df$pbs_layer2= nc_pbs_layer2 #Quality score for aerosol layers.
    tmp_info_df$pbs_layer3= nc_pbs_layer3 #Quality score for aerosol layers.
    tmp_info_df$tcc = nc_tcc #Bedeckungsgrad (total)
    tmp_info_df$bcc = nc_bcc #Bedeckungsgrad der unteren Wolkenschicht
    tmp_info_df$sci = nc_sci #Sky condition index (Niederschlag, Nebel, ...)
    tmp_info_df$vor = nc_vor #Vertikale Sichtweite
    tmp_info_df$voe = nc_voe #Ungenauigkeit der vertikalen Sichtweite
    tmp_info_df$mxd = nc_mxd #Maximaler Detektionsbereich
    tmp_info_df$cbh_layer1= nc_cbh_layer1 #cloud base height
    tmp_info_df$cbh_layer2= nc_cbh_layer2 #cloud base height
    tmp_info_df$cbh_layer3= nc_cbh_layer3 #cloud base height
    tmp_info_df$cbe_layer1= nc_cbe_layer1 #cloud base uncertainty
    tmp_info_df$cbe_layer2= nc_cbe_layer2 #cloud base uncertainty
    tmp_info_df$cbe_layer3= nc_cbe_layer3 #cloud base uncertainty
    tmp_info_df$cdp_layer1= nc_cdp_layer1 #cloud penetration depth
    tmp_info_df$cdp_layer2= nc_cdp_layer2 #cloud penetration depth
    tmp_info_df$cdp_layer3= nc_cdp_layer3 #cloud penetration depth
    tmp_info_df$cde_layer1= nc_cde_layer1 #cloud penetration depth uncertainty
    tmp_info_df$cde_layer2= nc_cde_layer2 #cloud penetration depth uncertainty
    tmp_info_df$cde_layer3= nc_cde_layer3 #cloud penetration depth uncertainty
    tmp_info_df$cho= nc_cho #cloud height offset (available if, altitude(m) is set and usealtitude=1)
    tmp_info_df$time = nc_time #Endzeitpunkt der Messung (UTC)
    
    tmp_info_df[tmp_info_df==-1]<-NA
    
    if (exists("info_data_17months")){
      #changed bind_rows to rbind 
      info_data_17months = rbind(info_data_17months, tmp_info_df)
    }else {
      info_data_17months = tmp_info_df
    }
     }
     else{}
    }else{}
   }, error=function(e){}
     )
    }
  return(info_data_17months)
}

####Datenverarbeitung####

#Vektor für alle Monate eines Jahres erstellen
#bsp für 2020 
month=seq(from=202107, to=202108, by=1) #Juli und August
#month=month[-11] #skip November for 2016 --> no data
filename=paste(month, ".*.nc", sep = "")

#create continous Dataframe
CloudCoverAll=data.frame("timestamp"=NA, "tcc"=NA, "latitude"=NA, "longitude"=NA, "azimuth"=NA, "zenith"=NA, 
                         "altitude"=NA, "wavelength"=NA, "life_time"=NA, "range_gate"=NA, "range_gate_hr"=NA, 
                         "average_time"=NA, "laser_pulses"=NA, "temp_int"=NA,	"temp_ext"=NA, 
                         "temp_det"=NA,	"temp_lom"=NA, "state_laser"=NA,	"state_detector"=NA, 
                         "state_optics"=NA,	"base"=NA,	"stddev"=NA,	"p_calc"=NA, "scaling"=NA,
                         "nn1"=NA, "nn2"=NA, "nn3"=NA, "pbl_layer1"=NA, "pbl_layer2"=NA,
                         "pbl_layer3"=NA, "pbs_layer1"=NA,"pbs_layer2"=NA,"pbs_layer3"=NA,
                         "bcc"=NA,	"sci"=NA,	"vor"=NA, "voe"=NA,	"mxd"=NA, "cbh_layer1"=NA, 
                         "cbh_layer2"=NA,"cbh_layer3"=NA,"cbe_layer1"=NA,"cbe_layer2"=NA,"cbe_layer3"=NA,
                         "cdp_layer1"=NA, "cdp_layer2"=NA,"cdp_layer3"=NA,"cde_layer1"=NA,"cde_layer2"=NA,
                         "cde_layer3"=NA, "cho"=NA )

#delete empty first row
CloudCoverAll=CloudCoverAll[-1,]  

for(x in filename){
#list files for a specific year/month
flist=list.files(pattern = x)
#check if files exist for this month
if(length(flist)!=0){
data_info_all = process_info_nc(flist)

#UTC +1 einstellen
data_info_all$timestamp <- strftime(ceiling_date(as.POSIXct(data_info_all$time), '10 minutes'), format = '%Y-%m-%d %H:%M')
data_info_all$timestamp=as.POSIXct(data_info_all$timestamp, origin="1970-01-01")
str(data_info_all$timestamp)

# in case you want UTC??
#data_info_all$time_format=as.POSIXct(as.POSIXlt(data_info_all$time, tryFormats=c("%d.%m.%Y %H:%M"), tz="UTC"), by="10 min")

#Totaler Bedeckungsgrad
CloudCover <- aggregate(tcc~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$tcc <- round(CloudCover$tcc, digits=0) #Werte ganzzahlig runden
str(CloudCover$timestamp) #ueberpruefen

#Breitengrad des Messstandortes in °
latitude=aggregate(latitude~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$latitude <- round(latitude$latitude, digits=5) #Werte runden und in dataframe schreiben

#Laengengrad des Messstandortes in °
longitude=aggregate(longitude~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$longitude <- round(longitude$longitude, digits=5) #Werte runden und in dataframe schreiben

#der Azimutwinkel der Zeigerichtung des Lasers
azimuth=aggregate(azimuth~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$azimuth <- round(azimuth$azimuth, digits=2) #Werte runden und in dataframe schreiben

#der Zenitwinkel der Zeigerichtung des Lasers (genutzt, wenn eine Neigungsadapterplatte fuer 5 oder 15 Grad genutzt wird)
zenith=aggregate(zenith~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$zenith <- round(zenith$zenith, digits=2) #Werte runden und in dataframe schreiben

#Hoehe des Messgeraetes über dem Meeresspiegel
altitude=aggregate(altitude~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$altitude <- round(altitude$altitude, digits=2) #Werte runden und in dataframe schreiben

#die Wellenlaenge des Lasers in nm
wavelength=aggregate(wavelength~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$awavelength <- round(wavelength$wavelength, digits=2) #Werte runden und in dataframe schreiben

#Laser operating time in hours
life_time=aggregate(life_time~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$life_time <- round(life_time$life_time, digits=2) #Werte runden und in dataframe schreiben

#die raeumliche Aufloesung der Messung
range_gate=aggregate(range_gate~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$range_gate <- round(range_gate$range_gate, digits=2) #Werte runden und in dataframe schreiben

#???
#possibly: (Parameter is called range_hr)
#The distance from the CHM 15k in meter, independently of altitude of the instrument location. 
#The high resolution range (hr) ist fixed to 5 m range resolution.
range_gate_hr=aggregate(range_gate_hr~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$range_gate_hr <- round(range_gate_hr$range_gate_hr, digits=2) #Werte runden und in dataframe schreiben

#die Zeit, ueber die gemittelt wird
average_time=aggregate(average_time~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$average_time <- round(average_time$average_time, digits=2) #Werte runden und in dataframe schreiben

#die Anzahl der Laserpulse gemittelt von einer Messung
laser_pulses=aggregate(laser_pulses~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$laser_pulses <- round(laser_pulses$laser_pulses, digits=0) #Werte runden und in dataframe schreiben

#Innentemperatur in Grad Celsius
temp_int=aggregate(temp_int~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$temp_int <- round(temp_int$temp_int, digits=2) #Werte runden und in dataframe schreiben

#AuÃentemperatur in Grad Celsius
temp_ext=aggregate(temp_ext~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$temp_ext <- round(temp_ext$temp_ext, digits=2) #Werte runden und in dataframe schreiben

#EmpfÃ¤ngertemperatur in Grad Celsius  
temp_det=aggregate(temp_det~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$temp_det <- round(temp_det$temp_det, digits=2) #Werte runden und in dataframe schreiben

#Temperatur der Messeinheit in Grad Celsius 
temp_lom=aggregate(temp_lom~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$temp_lom <- round(temp_lom$temp_lom, digits=2) #Werte runden und in dataframe schreiben

#LaserqualitÃ¤t in Prozent
state_laser=aggregate(state_laser~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$state_laser <- round(state_laser$state_laser, digits=1) #Werte runden und in dataframe schreiben

#EmpfÃ¤ngerqualitÃ¤t in Prozent
state_detector=aggregate(state_detector~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$state_detector <- round(state_detector$state_detector, digits=1) #Werte runden und in dataframe schreiben

#QualitÃ¤t der Optik in Prozent (Scheibenverschmutzung)
state_optics=aggregate(state_optics~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$state_optics <- round(state_optics$state_optics, digits=1) #Werte runden und in dataframe schreiben

#Baseline-HÃ¶he des Rohsignals in Photonen pro Laserschuss, hauptsÃ¤chlich vom Tageslicht beeinflusst
base=aggregate(base~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$base <- round(base$base, digits=2) #Werte runden und in dataframe schreiben

#Standardabweichung des Rohsignals in Photonen pro Laserschuss
stddev=aggregate(stddev~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$stddev <- round(stddev$stddev, digits=2) #Werte runden und in dataframe schreiben

#Kalibrierpuls zum Normalisieren der einzelnen Einheit Ã¼ber die Zeit
p_calc=aggregate(p_calc~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$p_calc <- round(p_calc$p_calc, digits=2) #Werte runden und in dataframe schreiben

#Scaling factor used to normalize individual units against reference system. (called TBcalibration in RS485).
scaling=aggregate(scaling~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$scaling <- round(scaling$scaling, digits=2) #Werte runden und in dataframe schreiben

#Lidar backscatter raw data with 5 - 15 m range resolution, normalized and range corrected. lp: laser pulses, b: baseline, c:  scaling, o(r): overlap correction function,p_calc: calibration signal
#beta_raw
#Lidar backscatter with 5 m resolution, raw data normalized and range corrected.
#beta_raw_hr

#used by manufacturer
nn1=aggregate(nn1~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$nn1 <- round(nn1$nn1, digits=2) #Werte runden und in dataframe schreiben

#used by manufacturer
nn2=aggregate(nn2~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$nn2 <- round(nn2$nn2, digits=2) #Werte runden und in dataframe schreiben

#used by manufacturer
nn3=aggregate(nn3~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$nn3 <- round(nn3$nn3, digits=2) #Werte runden und in dataframe schreiben

#Aerosol layer 1 calculated within the planetary boundary layer.
pbl_layer1=aggregate(pbl_layer1~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$pbl_layer1 <- round(pbl_layer1$pbl_layer1, digits=2) #Werte runden und in dataframe schreiben

#Aerosol layer 2 calculated within the planetary boundary layer.
pbl_layer2=aggregate(pbl_layer2~timestamp, FUN=mean, data = data_info_all,na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$pbl_layer2 <- round(pbl_layer2$pbl_layer2, digits=2) #Werte runden und in dataframe schreiben

#Aerosol layer 3 calculated within the planetary boundary layer.
pbl_layer3=aggregate(pbl_layer3~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$pbl_layer3 <- round(pbl_layer3$pbl_layer3, digits=2) #Werte runden und in dataframe schreiben

#Quality score for aerosol layer 1.
pbs_layer1=aggregate(pbs_layer1~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$pbs_layer1 <- round(pbs_layer1$pbs_layer1, digits=2) #Werte runden und in dataframe schreiben

#Quality score for aerosol layer 2.
pbs_layer2=aggregate(pbs_layer2~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$pbs_layer2 <- round(pbs_layer2$pbs_layer2, digits=2) #Werte runden und in dataframe schreiben

#Quality score for aerosol layer 3.
pbs_layer3=aggregate(pbs_layer3~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$pbs_layer3 <- round(pbs_layer3$pbs_layer3, digits=2) #Werte runden und in dataframe schreiben

#Bedeckungsgrad der unteren Wolkenschicht
bcc=aggregate(bcc~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$bcc <- round(bcc$bcc, digits=0) #Werte runden und in dataframe schreiben

#Sky condition index (Niederschlag, Nebel, ...)
sci=aggregate(sci~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$sci <- round(sci$sci, digits=0) #Werte runden und in dataframe schreiben

#Vertikale Sichtweite
vor=aggregate(vor~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$vor <- round(vor$vor, digits=2) #Werte runden und in dataframe schreiben

#Ungenauigkeit der vertikalen Sichtweite
voe=aggregate(voe~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$voe <- round(voe$voe, digits=2) #Werte runden und in dataframe schreiben

#Maximaler Detektionsbereich
mxd=aggregate(mxd~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$mxd <- round(mxd$mxd, digits=2) #Werte runden und in dataframe schreiben

#cloud base height layer 1
cbh_layer1=aggregate(cbh_layer1~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cbh_layer1 <- round(cbh_layer1$cbh_layer1, digits=2) #Werte runden und in dataframe schreiben

#cloud base height layer 2
cbh_layer2=aggregate(cbh_layer2~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cbh_layer2 <- round(cbh_layer2$cbh_layer2, digits=2) #Werte runden und in dataframe schreiben

#cloud base height layer 3
cbh_layer3=aggregate(cbh_layer3~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cbh_layer3 <- round(cbh_layer3$cbh_layer3, digits=2) #Werte runden und in dataframe schreiben

#cloud base uncertainty layer 1
cbe_layer1=aggregate(cbe_layer1~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cbe_layer1 <- round(cbe_layer1$cbe_layer1, digits=2) #Werte runden und in dataframe schreiben

#cloud base uncertainty layer 2
cbe_layer2=aggregate(cbe_layer2~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cbe_layer2 <- round(cbe_layer2$cbe_layer2, digits=2) #Werte runden und in dataframe schreiben

#cloud base uncertainty layer 3
cbe_layer3=aggregate(cbe_layer3~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cbe_layer3 <- round(cbe_layer3$cbe_layer3, digits=2) #Werte runden und in dataframe schreiben

#cloud penetration depth layer 1
cdp_layer1=aggregate(cdp_layer1~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cdp_layer1 <- round(cdp_layer1$cdp_layer1, digits=2) #Werte runden und in dataframe schreiben

#cloud penetration depth layer 2
cdp_layer2=aggregate(cdp_layer2~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cdp_layer2 <- round(cdp_layer2$cdp_layer2, digits=2) #Werte runden und in dataframe schreiben

#cloud penetration depth layer 3
cdp_layer3=aggregate(cdp_layer3~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cdp_layer3 <- round(cdp_layer3$cdp_layer3, digits=2) #Werte runden und in dataframe schreiben

#cloud penetration depth uncertainty layer1 
cde_layer1=aggregate(cde_layer1~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cde_layer1 <- round(cde_layer1$cde_layer1, digits=2) #Werte runden und in dataframe schreiben

#cloud penetration depth uncertainty layer2 
cde_layer2=aggregate(cde_layer2~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cde_layer2 <- round(cde_layer2$cde_layer2, digits=2) #Werte runden und in dataframe schreiben

#cloud penetration depth uncertainty layer3 
cde_layer3=aggregate(cde_layer3~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cde_layer3 <- round(cde_layer3$cde_layer3, digits=2) #Werte runden und in dataframe schreiben

#cloud height offset (available if, altitude(m) is set and usealtitude=1)
cho=aggregate(cho~timestamp, FUN=mean, data = data_info_all, na.action = na.pass) #Daten in 10min Werten aggregieren
CloudCover$cho <- round(cho$cho, digits=2) #Werte runden und in dataframe schreiben

#write in continous dataframe
CloudCoverAll=rbind(CloudCoverAll, CloudCover)
}else{}
}

#Kontinuierlichen Zeitstempel erstellen
timestamp=seq.POSIXt(from=CloudCoverAll[1,1], to=CloudCoverAll[length(CloudCoverAll$timestamp),1], by="10 min")
timestamp=as.data.frame(timestamp)


#Cloudcover Daten zuordnen (so dass leere Zeilen als NA dargestellt werden)
CloudCoverAll_2=merge(x=timestamp, y=CloudCoverAll, by="timestamp", all.x = T)
#ueberpruefen, wie viele fehlende Werte es bei der Wolkenbedeckung gab
length(which(is.na(CloudCoverAll_2$tcc)))

#Working directory neu setzen
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/03_Rohdaten_konvertiert")
#in csv Datei schreiben
write.csv(CloudCoverAll_2, file = "Ceilometer_BHL.csv", row.names = F)

#str(CloudCoverAll_2$timestamp)


#testplot for cloud base height layer
plot(CloudCoverAll_2$timestamp,CloudCoverAll_2$cbh_layer1, type="l", col="green")
lines(CloudCoverAll_2$timestamp,CloudCoverAll_2$cbh_layer2, type="l", col="red")
lines(CloudCoverAll_2$timestamp,CloudCoverAll_2$cbh_layer3, type="l", col="blue")

#check for cloud height
any(CloudCoverAll_2$cbh_layer1>CloudCoverAll_2$cbh_layer2, na.rm=T)
any(CloudCoverAll_2$cbh_layer2>CloudCoverAll_2$cbh_layer3, na.rm=T)

#Beim Uebertragen der Daten in die GeoDach Excel darauf achten, dass die Zeitzonen uebereinstimmen!

##### Daten zusammenführen ####
setwd("Z:/klima/HiWi-Stunden-Nachweise/Dana Looschelders/Ceilometer/alle_Parameter_Korrektur/")
#.scv dateien einlesen
datname=list.files(pattern = ".csv")
dat=lapply(datname, read.csv)

#namen der liste zu jahreszahlen ändern
names(dat)=substr(datname,start = 16, stop=19)
#alle Daten in einen Dataframe schreiben
dataframe=do.call(rbind, dat)
#Datei abspeichern
write.csv(dataframe, file = "CloudCover_all_2015_to_2020.csv", row.names = F)

#quickview - data exploration
str(dataframe)
dataframe$timestamp=as.POSIXct(dataframe$timestamp)
plot(dataframe$timestamp, dataframe$tcc, type="p") #plot values
dataframe$tcc[which(dataframe$tcc<0)] #values below 0
dataframe$tcc[which(dataframe$tcc>9)] #values above 9
sum(is.na(dataframe$tcc)) #na.values
#cloud height - check if layer 1 is lower than layer 2 and layer 3
any(dataframe$cbh_layer1>dataframe$cbh_layer2, na.rm=T)
any(dataframe$cbh_layer2>dataframe$cbh_layer3, na.rm=T)

#get data for one year in tidy format
data_2020_long=as.data.frame(dat[["2020"]])%>%
  pivot_longer(c(-timestamp), 
               names_to="variable", 
               values_to = "value")
#timestamp as POSIXct
data_2020_long$timestamp=as.POSIXct(data_2020_long$timestamp)

#Variablen plotten
data_2020_long%>%
  ggplot(aes(timestamp,value))+
  geom_line()+
  facet_wrap(~variable, scales="free")+
  theme_minimal()

ggsave(filename = "quickview_2020.pdf", device = "pdf",
       width = 40, height = 20, units = "cm")
    