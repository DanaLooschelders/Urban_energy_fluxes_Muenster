#calculate optimal alpha for every hour
library(ggplot2)
#solve heat equation for alpha 
#modified from heat function (J. Howard, cmna package R)
#u = initial values of u
#nextu = values of u after 1 timestep
#xdelta = change in x (space) at each step in u
#tdelta = time step
#n = number of steps to take

#alpha = thermal diffusivity (to be calculated)
#xdelta: 0.005089005
#tdelta: 24 (check)
alpha <- function(u, nextu, xdelta, tdelta, n) {
  m <- length(u)
  alpha_array <- matrix(u, nrow = 1)
  for(i in 1:n) {
    for(j in 2:(m - 1)) {
      ustep <- (u[j - 1] + u[j + 1] - 2 * u[j])
      #solved for alpha
      alpha_array[j] <- (( nextu[j] - u[j] ) / ustep ) * xdelta[j - 1]^2 / tdelta  
    }
    alpha_array[1]<-NA #remove first value
    alpha_array[length(alpha_array)]<-NA #remove last value
    timestep_alpha <- mean(alpha_array, na.rm=T)
  }
  return(timestep_alpha)
}

####test####
FO_grass_df_t<-as.data.frame(t(FO_grass_df))
colnames(FO_grass_df_t)<-FO_grass_temp_time_df$time
#use first two columns of FO_grass_df_t
alpha(u=FO_grass_df_t$`2021-07-29 14:28:48`,  
      nextu=FO_grass_df_t$`2021-07-29 14:29:12`,
      xdelta=heights_grass, tdelta=24, n=2)
#2.681718e-06

####for 24 secs####
tdelta_grass<-as.vector(diff.POSIXt(FO_grass_temp_time_df$time))
#transpose data frame
FO_grass_df_t<-as.data.frame(t(FO_grass_df))
#create output data frame
alpha_values_grass<-rep(NA, times=ncol(FO_grass_df_t))
#run function for all grass data
for(i in 1:ncol(FO_grass_df_t[1:ncol(FO_grass_df_t)-1])){
  print(i)
  alpha_values_grass[i]<-alpha(u=FO_grass_df_t[,i], 
                                  nextu=FO_grass_df_t[,i+1],
                                  xdelta=heights_grass, 
                                  tdelta=tdelta_grass[i], n=2)
}

alphas_grass<-data.frame("time"=FO_grass_temp_time_df$time, 
                            "alpha"=alpha_values_grass)
#plot alpha values
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alphas_grass, aes(x=time, y=alpha))+
  geom_line()+
  theme_bw()
ggsave(filename="alpha_grass_24s.png",
       width=297, height=210, units = "mm")
#calculate mean
mean(alpha_values_grass, na.rm=T) #-7.302204e-07

#average by hour
alpha_hour_avg<-aggregate(list(alpha = alphas_grass$alpha), 
                          list(hourofday = cut(alphas_grass$time, "1 hour")), 
                          mean)
alpha_hour_avg$hourofday<-as.POSIXct(alpha_hour_avg$hourofday)

#plot
ggplot(alpha_hour_avg, aes(x=hourofday, y=alpha))+
  geom_line()+
  theme_bw()+
  xlab("time")
ggsave(filename="alpha_grass_avg_to_hour.png",
       width=297, height=210, units = "mm")

#average by day
alpha_day_avg<-aggregate(list(alpha = alphas_grass$alpha), 
                         list(day = cut(alphas_grass$time, "1 day")), 
                         mean, na.rm=T)
alpha_day_avg$day<-as.POSIXct(alpha_day_avg$day)

#plot
ggplot(alpha_day_avg, aes(x=day, y=alpha))+
  geom_line()+
  theme_bw()+
  xlab("time")
ggsave(filename="alpha_concrete_avg_to_day.png",
       width=297, height=210, units = "mm")

#check time difference of values that are not 24
#tdelta_grass[(tdelta_grass!=24)]
#get all zero columns
#zeros<-which(tdelta_grass==0)
#remove all columns with zero
#FO_grass_df_t_clean<-FO_grass_df_t[,-c(zeros)]


####for an hour####
#get time diff and convert to seconds
tdelta_grass<-as.vector(diff.POSIXt(df_grass$time))*60*60 
#use values aggregated to one hour to calculate alpha (use df_grass_short)

#remove last two cloumns
df_grass_short<-subset(df_grass_short, select = -c(file, time))
#transpose
df_grass_short_t<-as.data.frame(t(df_grass_short))
colnames(df_grass_short_t)<-df_grass$time
#create output data frame
alpha_values_grass<-rep(NA, times=ncol(df_grass_short_t))
#run function for all grass data
for(i in 1:ncol(df_grass_short_t[1:ncol(df_grass_short_t)-1])){
  print(i)
  alpha_values_grass[i]<-alpha(u=df_grass_short_t[,i], 
                                  nextu=df_grass_short_t[,i+1],
                                  xdelta=heights_grass, 
                                  tdelta=tdelta_grass[i], n=2)
}

alphas_grass<-data.frame("time"=df_grass$time, 
                            "alpha"=alpha_values_grass)

#plot alpha values
ggplot(alphas_grass, aes(x=time, y=alpha))+
  geom_line()+
  theme_bw()
ggsave(filename="alpha_grass_hour.png",
       width=297, height=210, units = "mm")

alphas_grass[which.max(alphas_grass$alpha),]
alphas_grass[which.min(alphas_grass$alpha),]

mean(alphas_grass$alpha[alphas_grass$alpha>=0], na.rm=T) #1.079778e-07
mean(alphas_grass$alpha, na.rm=T) #-1.208407e-08
