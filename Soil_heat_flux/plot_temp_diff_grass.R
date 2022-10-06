#plot data of concrete column
#source script to loead netcdf files
source("C:/00_Dana/Uni/Masterarbeit/Urban_heat_fluxes/Soil_heat_flux/solve_heat_eq_grass.R")

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
