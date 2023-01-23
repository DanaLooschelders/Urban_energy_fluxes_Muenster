library(boot)
#select alpha values with mean and sd of previously determined values
alpha_range<-rnorm(n=2000, mean=2.359007*10^-08, sd=9.987034*10^-09) #0 to 10 cm
alpha<-1.220921e-06
hist(alpha_range)
#select values for specific heat capacity
specific_heat_range<-runif(n=2000, min=1000, max=1200) #mean=1140, sd=25) 
hist(specific_heat_range)
range(specific_heat_range)
#density 
density<-2.409*1000 #kg/m^3

#create input dataframe to bootstrap from
dat<-data.frame("alpha"=alpha_range, "Cv"=specific_heat_range*density)
#dat<-data.frame("alpha"=rep(7.13*10^-8), "Cv"=specific_heat_range*density)
str(dat)
#define function 
cond<-function(dat=dat, indices){
  dt<-dat[indices,]
  c(dt[,1]*dt[,2])
}

#bootstrap
test<-boot::boot(data = dat, statistic=cond, R=1000)
plot(test)
#calculate confidence intervals
boot.ci(test,type = "perc", conf = 0.95)
#get value for k
