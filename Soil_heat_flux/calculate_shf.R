#Soil heat flux
library(tidyverse)
library(reshape2)
#simulate some data to test

std<-data.frame("time"=seq.POSIXt(as.POSIXct("2022-08-16 10:00"), 
                                 as.POSIXct("2022-08-16 11:00"), by = "1 min"),
                "cm1"=seq(19,  by=0.009, length.out=61),
                "cm2"=seq(18.9,  by=0.008, length.out=61),
                "cm3"=seq(18.7,  by=0.007, length.out=61),
                "cm4"=seq(18.6,  by=0.006, length.out=61),
                "cm5"=seq(18.5,  by=0.005, length.out=61))
rownames(std)<-std$time

std<-std[,-1]
#reshape to long format
std_long <- melt(std, id="time")
#plot
ggplot() + 
  geom_line(data = std_long, 
            aes(x = time, y = value, color = variable, 
                group = variable), size = 1)

#test range of values 1*10^-9 m2 s1 and 1*10^-1 m2 s1

#heat flux as in Bonan (2016) Ecological Climatology
#F=-k(∆t/∆z)
  #F=heat flux [W/m^2]
  #-k = thermal conductivity
  #∆t/∆z = temperature gradient

# from Oldroyd (2013)
# heat equation
# dT/dt = alpha*(d^2T/dz^2)
  #alpha = thermal diffusivity
  #t = time
  #T = Temperature
  #z = distance

# ???? --> density was measured and specific heat capacity of ice was used

#

# alpha = k/(p*Cp)
  # alpha = thermal diffusivity
  # k = thermal conductivity
  # Cp = specific heat capacity

#intial condition: first measured temperature profile
#boundary condition: Neumann boundary condition
      #-> specifying heat flux in time, as determined by the temperature measurements
      #Neumann boundary condition: alpha = 0
      #--> gradient is specified at the boundaries

#solving PDEs in R 
#install.packages("ReacTran")
#install.packages("deSolve")
#install.packages("rootSolve")

library(ReacTran) #grid generation routines
                  #the discretization of the advective-diffusive 
                        #transport terms on these grids
                  #PDEs are either rewritten as ODEs or as algebraic equations
library(deSolve) #for initial value problem
library(rootSolve) #when all derivatives have been 
                  #approximated --> solve algebraic equation

#Solving a PDE in R 
#1. create a grid in one/many independant variables
    #approximate equations numerically to the grid
#2. solve resulting ODEs or algebraic equations

#create grid
xgrid<-setup.grid.1D(x.up = std[1,1], #position of upper boundary
              L = 5, #length of domain
              N = 5) #Number of grid points (divides L)
plot(xgrid)

#xgrid<-setup.grid.1D(x.up=0, x.down=1, N=N)
x<-xgrid$x.mid
D.coeff<-0.01

tran<-tran.1D(C=std[1,], C.up=std[1,1], 
              C.down=std[1,5], dx=xgrid)

?tran.1D

Diffusion <- function (t, Y, parms){
  tran <- tran.1D(C = Y, C.up = 0, C.down = 1,
                  D = D.coeff, dx = xgrid)
  list(dY = tran$dC, flux.up = tran$flux.up,
       flux.down = tran$flux.down)
}

Diffusion()
