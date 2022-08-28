#calculate diffusivity
install.packages("cmna")
library(cmna)
#use not aggregated data
FO_grass_list[[1]]$cal_temp

FO_grass_list_t<-do.call("rbind", lapply(FO_grass_list, "[[", 1))



heights<-diff(as.numeric(rownames(df_grass_soil)))
#heat function
    #u = initial values of u
    #alpha = thermal diffusivity
    #xdelta = change in x (space) at each step in u
    #tdelta = time step
    #n = number of steps to take
#original function
heat <- function (u, alpha , xdelta , tdelta , n) {
  m <- length (u)
  uarray <- matrix (u, nrow = 1)
  newu <- u
  for(i in 1:n) {
    for(j in 2:(m - 1)) {
      h <- alpha * tdelta / xdelta ^2 
      ustep <- (u[j - 1] + u[j + 1] - 2 * u[j])
      newu [j] <- u[j] + h * ustep
    }
    u <- newu
    u[1] <- u[m]
    uarray <- rbind (uarray , u)
  }
  return ( uarray )
}
#change original heat function to use a vector of heights

heat_heights <- function (u, alpha , xdelta , tdelta , n) {
  m <- length (u)
  uarray <- matrix (u, nrow = 1)
  newu <- u
  for(i in 1:n) {
    for(j in 2:(m - 1)) {
      h <- alpha * tdelta / xdelta[j - 1] ^2 #set xdelta to individual height
      ustep <- (u[j - 1] + u[j + 1] - 2 * u[j])
      newu [j] <- u[j] + h * ustep
    }
    u <- newu
    u[1] <- u[m]
    uarray <- rbind (uarray , u)
  }
  return ( uarray )
}

test<-heat(u=df_grass_soil[,1], alpha=0.5*10^-6, 
     xdelta=0.5, tdelta=60*60*60, n=10)

test<-as.data.frame(test)
test[1,]==df_grass_soil[,1]

15+17-(2*16)
13+17-(2*16)
15+18-(2*16)

#hinreichend/notwenig --> bei Tiefpunktberechnung
