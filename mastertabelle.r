#Masterdatentabelle
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Flux_data/prep_flux_data.R")  #load heat fluxes
#Fluxes EC02
colnames(beton) 
master_beton_flux<-beton[,c("datetime", "H","TKE","u.",  "qc_H", "Tau", "LE", "qc_LE", "co2_flux", "qc_co2_flux","h2o_flux", 
  "qc_h2o_flux","sonic_temperature","air_temperature", "air_pressure", "u_unrot",                
  "v_unrot", "w_unrot", "u_rot", "v_rot", "w_rot" , "wind_speed", "wind_dir")]
colnames(master_beton_flux)[2:23]<-paste(colnames(master_beton_flux)[2:23], "_beton", sep="")
#Fluxes EC04
master_grass_flux<-kiebitz[,c("datetime", "H", "TKE", "qc_H", "u.", "Tau", "LE", "qc_LE", "co2_flux", "qc_co2_flux","h2o_flux", 
                            "qc_h2o_flux","sonic_temperature",  "air_temperature", "air_pressure", "u_unrot",                
                            "v_unrot", "w_unrot", "u_rot", "v_rot", "w_rot" , "wind_speed", "wind_dir")] 
colnames(master_grass_flux)[2:23]<-paste(colnames(master_grass_flux)[2:23], "_kiebitz", sep="")
#Meteo EC02 and EC04
colnames(dat.meteo.agg)
master_meteo<-dat.meteo.agg[,c("TIMESTAMP", "AirTC_Avg_beton", "RH_Avg_beton", "SUp_Avg_beton", "SDn_Avg_beton", 
                               "LUpCo_Avg_beton", "LDnCo_Avg_beton", "CNR4TC_Avg_beton", "Albedo_Avg_beton",
                               "SUp_Avg_kiebitz", "SDn_Avg_kiebitz", "LUpCo_Avg_kiebitz", "LDnCo_Avg_kiebitz", 
                               "CNR4TC_Avg_kiebitz","Albedo_Avg_kiebitz", "AirTC_Avg_kiebitz", "RH_Avg_kiebitz",
                               "CNR4TC_Avg_kiebitz", "WC01_VWC_Avg", "WC02_VWC_Avg","WC03_VWC_Avg","shf_Avg.1.", 
                               "shf_Avg.2.", "shf_Avg.3.", "Rain_mm_Tot", "AirP_Avg" )]

#calculated Net radiation EC02
master_meteo$NetRad_beton<-master_meteo$SDn_Avg_beton-master_meteo$SUp_Avg_beton+
  master_meteo$LDnCo_Avg_beton-master_meteo$LUpCo_Avg_beton
#calculated Net radiation EC04
master_meteo$NetRad_kiebitz<-master_meteo$SDn_Avg_kiebitz-master_meteo$SUp_Avg_kiebitz+
  master_meteo$LDnCo_Avg_kiebitz-master_meteo$LUpCo_Avg_kiebitz
#calculated shf EC04
shf_kiebitz<-shf_30min
colnames(shf_kiebitz)[2]<-"shf_kiebitz"
#calculated shf EC02
shf_beton<-shf_30min
colnames(shf_beton)[2]<-"shf_beton"
#calculated Bo Ratio EC04
master_grass_flux$BR_kiebitz<-master_grass_flux$H/master_grass_flux$LE
#calculated BO Ratio EC02
master_beton_flux$BR_beton<-master_beton_flux$H/master_beton_flux$LE
#calculated surface temp EC04
master_meteo$SurfaceTemp_kiebitz<-nthroot(master_meteo$LDnCo_Avg_kiebitz/(5.67*10^-8), 4)-272
#calculated surface temp EC04
master_meteo$SurfaceTemp_beton<-nthroot(master_meteo$LDnCo_Avg_beton/(5.67*10^-8), 4)-272
#Meteo SS
colnames(dat.SS.cut)
master_meteo_SS<-dat.SS.cut[,c("TIMESTAMP", "AirTC2m_Avg", "RH2m_Avg", "AirTC10m_Avg", "RH10m_Avg", 
                               "AirP_Avg", "LUpCo_Avg", "LDnCo_Avg", "SDn_Avg", "SUp_Avg", "albedo",     
                               "CNR4TC_Avg" )]
colnames(master_meteo_SS)<-paste(colnames(master_meteo_SS), "_Steinf", sep="")
colnames(master_meteo_SS)[1]<-"TIMESTAMP"
#calculated surface temp SS
master_meteo_SS$SurfaceTemp_Steinf<-nthroot(master_meteo_SS$LDnCo_Avg_Steinf/(5.67*10^-8), 4)-272

#merge them all together
master_meteo_all<-full_join(master_meteo, master_meteo_SS, by="TIMESTAMP")
master_flux_all<-full_join(master_grass_flux, master_beton_flux, by="datetime")
colnames(master_flux_all)[1]<-"TIMESTAMP"
shf_all<-full_join(shf_beton, shf_kiebitz, by="TIMESTAMP")

master_meteo_all<-full_join(master_meteo_all, shf_all, by="TIMESTAMP")
master_all<-full_join(master_meteo_all, master_flux_all, by="TIMESTAMP")
#QAQC
#Temp
range(master_all$AirTC_Avg_beton, na.rm=T)
range(master_all$AirTC_Avg_kiebitz, na.rm=T)
range(master_all$AirTC2m_Avg_Steinf, na.rm=T)
range(master_all$AirTC10m_Avg_Steinf, na.rm=T)
#RH
range(master_all$RH_Avg_beton, na.rm=T)
range(master_all$RH_Avg_kiebitz, na.rm=T)
range(master_all$RH2m_Avg_Steinf, na.rm=T)
range(master_all$RH10m_Avg_Steinf, na.rm=T)
#Radiation
range(master_all$SUp_Avg_beton, na.rm=T)
range(master_all$SUp_Avg_kiebitz, na.rm=T)
range(master_all$SUp_Avg_Steinf, na.rm=T)
range(master_all$SDn_Avg_beton, na.rm=T)
range(master_all$SDn_Avg_kiebitz, na.rm=T)
range(master_all$SDn_Avg_Steinf, na.rm=T)
master_all$SDn_Avg_Steinf[master_all$SDn_Avg_Steinf<0]<-0 #correct
range(master_all$LDnCo_Avg_beton, na.rm=T)
range(master_all$LDnCo_Avg_kiebitz, na.rm=T)
range(master_all$LDnCo_Avg_Steinf, na.rm=T)
range(master_all$LUpCo_Avg_beton, na.rm=T)
range(master_all$LUpCo_Avg_kiebitz, na.rm=T)
#Fluxes
range(master_all$H_beton, na.rm=T)
range(master_all$LE_beton, na.rm=T)
range(master_all$H_kiebitz, na.rm=T)
range(master_all$h2o_flux_beton, na.rm=T)
range(master_all$h2o_flux_kiebitz, na.rm=T)
range(master_all$H_kiebitz, na.rm=T)
range(master_all$LE_kiebitz, na.rm=T)
#df[ , -which(names(df) %in% c("z","u"))]
master_all<-master_all[,-which(names(master_all) %in% c("u_unrot_kiebitz",  "v_unrot_kiebitz", "w_unrot_kiebitz", 
                           "u_unrot_beton", "v_unrot_beton", "w_unrot_beton"))]
write.csv(master_all, file="mastertable_20230217.csv", row.names = F)
colnames(master_all)
master_all$LE

master_all_complete<-master_all[complete.cases(master_all[,c("H_kiebitz", "H_beton",
                                                             "LE_kiebitz", "LE_beton",
                                                             "NetRad_kiebitz", "NetRad_beton",
                                                             "shf_kiebitz", "shf_beton")]),]
sum(master_all_complete$LE_kiebitz+master_all_complete$H_kiebitz)/
  sum((master_all_complete$NetRad_kiebitz*-1)-master_all_complete$shf_kiebitz)

sum(master_all_complete$LE_beton+master_all_complete$H_beton)/
  sum((master_all_complete$NetRad_beton*-1)-master_all_complete$shf_beton)

