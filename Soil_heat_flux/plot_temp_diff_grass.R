#plot data of grass column
#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/solve_heat_eq_grass.R")

#calculate diff between heights
df_grass_diff<-as.data.frame(lapply(FO_grass_df_t, diff))
#transpose
df_grass_diff_t<-as.data.frame(t(df_grass_diff))
#change colnames to height
colnames(df_grass_diff_t)<-colnames(FO_grass_df)[1:116]
#add time
df_grass_diff_t$time<-FO_grass_temp_time_df_order$time

#reshape into long format for plotting
df_grass_diff_long<-gather(data = df_grass_diff_t, key, value, -time)
#transform class of vars to numeric
df_grass_diff_long$key<-as.numeric(df_grass_diff_long$key)
#df_grass_diff_long$value<-as.numeric(df_grass_diff_long$value)
#tidy up environment
rm(df_grass_diff, df_grass_diff_t)
rm(FO_grass_df_pred_1, FO_grass_df_pred_2, FO_grass_df_pred_3, FO_grass_df_test, FO_grass_df_validation)
rm(FO_grass_df_test_subset_1, FO_grass_df_test_subset_1_measured, FO_grass_df_test_subset_2, FO_grass_df_test_subset_2_measured)
rm(data_measured, data_predicted)
rm(FO_grass_list, FO_grass_temp_time, FO_grass_temp_time_df, FO_grass_only_temp)

library(ggplot2)
#plot as heatmap
ggplot(df_grass_diff_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column grass")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_diff_grass.png",
       device="png",width=297, height=210, units = "mm",)

#5-7 sunrise
#8-10 sunset
#calculate mean for time between 8 and 20--> day
#subset to daytime
library(lubridate)
df_grass_diff_t$hour<-hour(df_grass_diff_t$time)
grass_day<-df_grass_diff_t[df_grass_diff_t$hour>=8&df_grass_diff_t$hour<=20,]
day_means<-colMeans(grass_day[,1:116])
plot(day_means, type="l")
#calculate mean between 22 and 5 --> night
grass_night<-df_grass_diff_t[df_grass_diff_t$hour>=22|df_grass_diff_t$hour<=5,]
night_means<-colMeans(grass_night[,1:116])
plot(night_means, type="l")
