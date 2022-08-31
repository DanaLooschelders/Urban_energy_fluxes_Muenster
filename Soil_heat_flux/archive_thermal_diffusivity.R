####Thermal diffusivity####
####for concrete
z = as.vector(df_grass_soil$height)
T = as.vector(df_grass_soil$X2) #??????????
time = dg["time"].to_numpy()  #??????????

#create the gradiants 
dT = diff(as.numeric(df_grass_soil$X2)) #Temperature
dz = diff(as.numeric(df_grass_soil$height)) #height

#Python code
#T(t) as vector with all temperature for definite time
#z(t) as vector with all height for definite time

#e = {}
#steigungen = []

#for t in d: #loop through times
# Tt, zt = d[t]
#Tt = Tt[4:]
#zt = zt[4:]
# Nehme 0.075m als Schneehöhe an.
#idx = find_nearest(zt, 0.075)
# z Werte für Fit
#fit_z = zt[idx-2:idx+3]
#fit_T = Tt[idx-2:idx+3]
# dT/dz = m * z + b
# Primitive of dT/dz
# => T = m/2 * z**2 + b*z + c = a * z**2 + b * z + c
# => Fitte Polynom zweiter Ordnung an T(z)
# dann ist d²T/dz² = 2*a
#a, b, c = np.polyfit(fit_z, fit_T, 2)
sec_poly<-poly(dT, dz, degree = 2)
plot(sec_poly)
m=2*sec_poly
plot(m)
#m = 2*a
#e[t] = m
#steigungen.append(m)


####plot first prediction try####
#reshape into long format for plotting
FO_concrete_df_pred_t<-data.frame(t(FO_concrete_df_pred))
colnames(FO_concrete_df_pred_t)<-colnames(FO_concrete_df) #set proper colnames
FO_concrete_df_pred_t<-FO_concrete_df_pred_t[-c(dim(FO_concrete_df_pred_t)[1]),] #drop last empty row
FO_concrete_df_pred_t$time<-FO_concrete_temp_time_df_short$time #add time as var
FO_concrete_df_pred_t$time<-as.POSIXct(FO_concrete_df_pred_t$time) #reformat time
FO_concrete_df_pred_long<-gather(data = FO_concrete_df_pred_t, key, value, -time) #get into long format

#transform class of vars to numeric
FO_concrete_df_pred_long$key<-as.numeric(FO_concrete_df_pred_long$key)
FO_concrete_df_pred_long$value<-as.numeric(FO_concrete_df_pred_long$value)

#plot as heatmap
ggplot(FO_concrete_df_pred_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  #geom_hline(aes(yintercept=threshold_concrete, col="Boundary"))+
  scale_color_manual(values = c("black"))+
  theme_bw()+
  ggtitle(label="FO Column Concrete Prediction")

#prüfen hinreichend/notwenig --> bei Tiefpunktberechnung rms für alpha
