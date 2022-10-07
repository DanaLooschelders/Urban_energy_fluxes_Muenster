#plot data of concrete column
#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/solve_heat_eq_concrete.R")

#calculate diff between heights
df_concrete_diff<-as.data.frame(lapply(FO_concrete_df_t, diff))
#transpose
df_concrete_diff_t<-as.data.frame(t(df_concrete_diff))
#change colnames to height
colnames(df_concrete_diff_t)<-colnames(FO_concrete_df)[1:134]
#add time
df_concrete_diff_t$time<-FO_concrete_temp_time_df_order$time

#reshape into long format for plotting
df_concrete_diff_long<-gather(data = df_concrete_diff_t, key, value, -time)
#transform class of vars to numeric
df_concrete_diff_long$key<-as.numeric(df_concrete_diff_long$key)
#df_concrete_diff_long$value<-as.numeric(df_concrete_diff_long$value)
#tidy up environment
rm(df_concrete_diff, df_concrete_diff_t)
rm(FO_concrete_df_pred_1, FO_concrete_df_pred_2, FO_concrete_df_pred_3, FO_concrete_df_test, FO_concrete_df_validation)
rm(FO_concrete_df_test_subset_1, FO_concrete_df_test_subset_1_measured, FO_concrete_df_test_subset_2, FO_concrete_df_test_subset_2_measured)
rm(data_measured, data_predicted)
rm(FO_concrete_list, FO_concrete_temp_time, FO_concrete_temp_time_df, FO_concrete_only_temp)

library(ggplot2)
#plot as heatmap
ggplot(df_concrete_diff_long, aes(time, key)) +
  geom_tile(aes(fill=value)) +
  scale_fill_viridis_c("Temperature [°C]")+
  ylab(label="Height [m]")+
  theme_bw()+
  ggtitle(label="FO Column Concrete")

setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggsave(filename="FO_Column_diff_concrete.png",
       device="png",width=297, height=210, units = "mm",)
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
#5-7 sunrise
#8-10 sunset
#calculate mean for time between 8 and 20--> day
#subset to daytime
df_concrete_diff_t$hour<-hour(df_concrete_diff_t$time)
concrete_day<-df_concrete_diff_t[df_concrete_diff_t$hour>=8&df_concrete_diff_t$hour<=20,]
concrete_day_means<-data.frame("mean"=colMeans(concrete_day[,1:134]), 
                      "height"=as.numeric(colnames(concrete_day)[1:134]))
ggplot(data=concrete_day_means, aes(y=mean, x=height))+
  geom_line()+
  theme_bw()+
  ylab(label="mean temp. gradient [°C]")+
  xlab(label="height from bottom of column [m]")+
  ggtitle(label="Concrete - day", subtitle="Mean temp. gradient over day 8 am to 8 pm")
ggsave(filename="concrete_mean_temp_gradient_day.png")
#calculate mean between 22 and 5 --> night
concrete_night<-df_concrete_diff_t[df_concrete_diff_t$hour>=22|df_concrete_diff_t$hour<=5,]
concrete_night_means<-data.frame("mean"=colMeans(concrete_night[,1:134]), 
                        "height"=as.numeric(colnames(concrete_night)[1:134]))
ggplot(data=concrete_night_means, aes(y=mean, x=height))+
  geom_line()+
  theme_bw()+
  ylab(label="mean temp. gradient [°C]")+
  xlab(label="height from bottom of column [m]")+
  ggtitle(label="Concrete - Night", subtitle="Mean temp. gradient over night 10 pm to 5 am")
ggsave(filename="concrete_mean_temp_gradient_night.png")
