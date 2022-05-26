#orthogonal regression for slow data
#source script to process slow data
source("Z:/klima/Projekte/2021_CalmCity_Masterarbeit_Dana/02_Datenauswertung/R_Skripts/QAQC_slow_data.R")
library(grid)
#Code from klima wiki
mean_regression <- function(a,b,r=1,na.rm=T) {
  # "mittlere" Regressionsgerade nach Schönwiese (2013) bzw. Foken in VDI (2011)
  # a ... Messreihe A
  # b ... Messreihe B
  if(na.rm) {
    s <- complete.cases(a,b)
    a=a[s];b=b[s];
  }
  regression <- function(a,b,x,r=1) {
    # a... y-value
    # b... x-value
    # x... x-value(s) for which you want to obtain the y-value
    # r... regression, set 1 for mean regressionline
    return(mean(a)+r*(sd(a)/sd(b))*(x-mean(b)))
  }
  #slope=(regression(a,b,2)-regression(a,b,1))/(2-1)
  slope=sd(a)/sd(b)
  #intercept=regression(a,b,2)-slope*2
  intercept=mean(a)-slope*mean(b)
  
  a_hat <- regression(a,b,b)
  a_var <- a-mean(a)
  a_hm <- a_hat-mean(a)
  r_ab=sum(a_hm^2)/sum(a_var^2) # = 1, weil r=1 gesetzt wurde...
  
  #r_ab = cov(a,b)/(sd(a)*sd(b))
  
  return(c("intercept"=intercept,
           "slope"=slope,
           "r-squared"=r_ab^2))
}


#test for statistical difference?

#orthogonal regression
ortho_reg<-mean_regression(a=dat.meteo.merge$AirTC_Avg_beton, 
                b=dat.meteo.merge$AirTC_Avg_kiebitz)

ggplot(data=dat.meteo.merge, aes(AirTC_Avg_beton, AirTC_Avg_kiebitz))+
  geom_point(alpha=0.1)+
  theme_bw()+
  geom_abline(aes(slope=ortho_reg[2],intercept = ortho_reg[1], 
              color="orthogonal\nregression line"), size=1)+
  scale_color_manual(values="red")+
  labs(tag = paste("slope: ", round(ortho_reg[2], 2),
                   "\nr-squared: ",round(ortho_reg[1],2))) +
  theme(plot.tag.position = c(.9, .39),
        text=element_text(size=7),
        axis.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))

