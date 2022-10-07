#plot data of grass column
#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/solve_heat_eq_grass.R")
library(lubridate)
library(ggplot2)
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
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
#5-7 sunrise
#8-10 sunset
#calculate mean for time between 8 and 20--> day
#subset to daytime
df_grass_diff_t$hour<-hour(df_grass_diff_t$time)
grass_day<-df_grass_diff_t[df_grass_diff_t$hour>=8&df_grass_diff_t$hour<=20,]
grass_day_means<-data.frame("mean"=colMeans(grass_day[,1:116]), 
                        "height"=as.numeric(colnames(grass_day)[1:116]))
ggplot(data=grass_day_means, aes(y=mean, x=height))+
  geom_line()+
  theme_bw()+
  ylab(label="mean temp. gradient [°C]")+
  xlab(label="height from bottom of column [m]")+
  ggtitle(label="Grass - Day", subtitle="Mean temp. gradient over day 8 am to 8 pm")
ggsave(filename="grass_mean_temp_gradient_day.png")
#calculate mean between 22 and 5 --> night
grass_night<-df_grass_diff_t[df_grass_diff_t$hour>=22|df_grass_diff_t$hour<=5,]
grass_night_means<-data.frame("mean"=colMeans(grass_night[,1:116]), 
                        "height"=as.numeric(colnames(grass_night)[1:116]))
ggplot(data=grass_night_means, aes(y=mean, x=height))+
  geom_line()+
  theme_bw()+
  ylab(label="mean temp. gradient [°C]")+
  xlab(label="height from bottom of column [m]")+
  ggtitle(label="Grass - Night", subtitle="Mean temp. gradient over night 10 pm to 5 am")
ggsave(filename="grass_mean_temp_gradient_night.png")
