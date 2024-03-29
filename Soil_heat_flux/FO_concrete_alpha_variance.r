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
      alpha_array[j] <- (( nextu[j] - u[j] ) / ustep ) * xdelta^2 / tdelta  
    }
    alpha_array[1]<-NA #remove first value
    alpha_array[length(alpha_array)]<-NA #remove last value
    timestep_alpha <- mean(alpha_array, na.rm=T)
  }
  return(timestep_alpha)
}

####test####
#use first two columns of FO_concrete_df_t
alpha(u=FO_concrete_df_t$`2021-07-29 14:29:12`, 
      nextu=FO_concrete_df_t$`2021-07-29 14:29:36`,
      xdelta=0.005089005, tdelta=24, n=2)


####for 24 secs####
tdelta_concrete<-as.vector(diff.POSIXt(FO_concrete_temp_time_df$time))
#transpose data frame
FO_concrete_df_t<-as.data.frame(t(FO_concrete_df))
#create output data frame
alpha_values_concrete<-rep(NA, times=ncol(FO_concrete_df_t))
#run function for all concrete data
for(i in 1:ncol(FO_concrete_df_t[1:ncol(FO_concrete_df_t)-1])){
  print(i)
  alpha_values_concrete[i]<-alpha(u=FO_concrete_df_t[,i], 
                         nextu=FO_concrete_df_t[,i+1],
                         xdelta=0.005089005, 
                         tdelta=tdelta_concrete[i], n=2)
}

alphas_concrete_24s<-data.frame("time"=FO_concrete_temp_time_df$time, 
                            "alpha"=alpha_values_concrete)
#plot alpha values
setwd("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/Grafiken/FO_Columns")
ggplot(alphas_concrete_24s, aes(x=time, y=alpha))+
  geom_line()+
  theme_bw()+
  xlab("time")
ggsave(filename="alpha_concrete_24s.png",
       width=297, height=210, units = "mm")
#calculate mean
mean(alpha_values_concrete, na.rm=T) #4.185227e-07

#average by hour
alpha_hour_avg<-aggregate(list(alpha = alphas_concrete_24s$alpha), 
          list(hourofday = cut(alphas_concrete_24s$time, "1 hour")), 
          mean)
alpha_hour_avg$hourofday<-as.POSIXct(alpha_hour_avg$hourofday)

#plot
ggplot(alpha_hour_avg, aes(x=hourofday, y=alpha))+
  geom_line()+
  theme_bw()+
  xlab("time")
ggsave(filename="alpha_concrete_avg_to_hour.png",
       width=297, height=210, units = "mm")

#average by day
alpha_day_avg<-aggregate(list(alpha = alphas_concrete_24s$alpha), 
                          list(day = cut(alphas_concrete_24s$time, "1 day")), 
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
#tdelta_concrete[(tdelta_concrete!=24)]
#get all zero columns
#zeros<-which(tdelta_concrete==0)
#remove all columns with zero
#FO_concrete_df_t_clean<-FO_concrete_df_t[,-c(zeros)]

unrealistic_alphas<-alphas_concrete[alphas_concrete<0|alphas_concrete>1*10^-7]
####for an hour####
#get time diff and convert to seconds
tdelta_concrete<-as.vector(diff.POSIXt(df_concrete$time))*60*60 
#use values aggregated to one hour to calculate alpha (use df_concrete_short)

#remove last two cloumns
df_concrete_short<-subset(df_concrete_short, select = -c(file, time))
#transpose
df_concrete_short_t<-as.data.frame(t(df_concrete_short))
colnames(df_concrete_short_t)<-df_concrete$time
#create output data frame
alpha_values_concrete<-rep(NA, times=ncol(df_concrete_short_t))
#run function for all concrete data
for(i in 1:ncol(df_concrete_short_t[1:ncol(df_concrete_short_t)-1])){
  print(i)
  alpha_values_concrete[i]<-alpha(u=df_concrete_short_t[,i], 
                                  nextu=df_concrete_short_t[,i+1],
                                  xdelta=0.005089005, 
                                  tdelta=tdelta_concrete[i], n=2)
}

alphas_concrete_hour<-data.frame("time"=df_concrete$time, 
                            "alpha"=alpha_values_concrete)

#plot alpha values
ggplot(alphas_concrete_hour, aes(x=time, y=alpha))+
  geom_line()+
  theme_bw()+
ggsave(filename="alpha_concrete_hour.png",
         width=297, height=210, units = "mm")

alphas_concrete[which.max(alphas_concrete_hour$alpha),]
alphas_concrete[which.min(alphas_concrete_hour$alpha),]

mean(alphas_concrete$alpha[alphas_concrete_hour$alpha>=0], na.rm=T) #1.543402e-07
mean(alphas_concrete_hour$alpha, na.rm=T) #9.243853e-09
median(alphas_concrete_hour$alpha, na.rm=T)
