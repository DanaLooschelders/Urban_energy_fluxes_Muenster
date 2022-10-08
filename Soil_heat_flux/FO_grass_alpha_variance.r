#calculate optimal alpha for every hour

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

#test
tdelta_concrete<-as.vector(diff.POSIXt(FO_concrete_temp_time_df$time))
#use first two columns of FO_concrete_df_t
alpha(u=FO_concrete_df_t$`2021-07-29 14:29:12`, 
      nextu=FO_concrete_df_t$`2021-07-29 14:29:36`,
      xdelta=0.005089005, tdelta=24, n=2)

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

alphas_concrete<-data.frame("time"=FO_concrete_temp_time_df$time, 
                            "alpha"=alpha_values_concrete)
#plot alpha values
ggplot(alphas_concrete, aes(x=time, y=alpha))+
  geom_line()+
  theme_bw()
#calculate mean
mean(alpha_values_concrete, na.rm=T)
#check time difference of values that are not 24
tdelta_concrete[(tdelta_concrete!=24)]
#get all zero columns
zeros<-which(tdelta_concrete==0)
#remove all columns with zero
FO_concrete_df_t_clean<-FO_concrete_df_t[,-c(zeros)]
#get time differences again
tdelta_concrete<-tdelta_concrete[-zeros]
#run function again
for(i in 1:ncol(FO_concrete_df_t_clean[1:ncol(FO_concrete_df_t_clean)-1])){
  print(i)
  alpha_values_concrete[i]<-alpha(u=FO_concrete_df_t_clean[,i], 
                                  nextu=FO_concrete_df_t_clean[,i+1],
                                  xdelta=0.005089005, 
                                  tdelta=tdelta_concrete[i], n=2)
}

#plot alpha values
ggplot(alphas_concrete, aes(x=time, y=alpha))+
  geom_line()+
  theme_bw()

unrealistic_alphas<-alphas_concrete[alphas_concrete<0|alphas_concrete>]