library(boot)
#select alpha values with mean and sd of previously determined values
alpha_range<-rnorm(n=2000, mean=7.13*10^-8, sd=2.73693*10^-08)
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


#hist(test$t[1,], breaks = 100)
hist(test$t, breaks=8)
mean(test$t) #0.173
median(test$t) #0.173
#tail(test$t)
print(test)

t<-data.frame(test$t)
library(CAST)
library(randomForest)
?randomForest::predict
?caret::predict
??predict
