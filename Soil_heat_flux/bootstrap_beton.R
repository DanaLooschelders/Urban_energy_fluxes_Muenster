library(boot)
#select alpha values with mean and sd of previously determined values
alpha_range<-rnorm(n=2000, mean=7.13*10^-8, sd=2.73693*10^-08)
hist(alpha_range)
#select values for specific heat capacity
specific_heat_range<-rnorm(n=2000, mean=1000, sd=35)
hist(specific_heat_range)
#density 
density<-2.409*1000
#create input dataframe to bootstrap from
dat<-data.frame("alpha"=alpha_range, "Cv"=specific_heat_range*density)
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
boot.ci(test,type = "basic", conf = 0.95)

