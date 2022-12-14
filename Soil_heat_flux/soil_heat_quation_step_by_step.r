#heat equation grass  step by step

#####heat function####
#u = initial values of u
#alpha = thermal diffusivity
#xdelta = change in x (space) at each step in u
#tdelta = time step
#n = number of steps to take

#change original heat function to use a vector of heights and times
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

alpha.range<-seq_log(1*10^-15, 1*10^-3, length.out = 100)
#create output dataframe for alpha and RMSEs
alpha_rmse_1<-data.frame("alpha"=alpha.range, "RMSE"=rep(NA))
#create vector of measured data for RMSE calculation
FO_grass_df_test_subset_1_measured<-FO_grass_df_test_subset_1
FO_grass_df_test_subset_1_measured<-FO_grass_df_test_subset_1_measured[-1,] #remove first row (invalid)
FO_grass_df_test_subset_1_measured<-FO_grass_df_test_subset_1_measured[,-1]  #remove first column (cant be predicted)
data_measured<-as.vector(t(FO_grass_df_test_subset_1_measured))
#run loop for broad range of alphas

#set alpha value
x=55

  print(x)
  #create output dataframe for subset 1
  FO_grass_df_pred_1<-setNames(data.frame(matrix(ncol = ncol(FO_grass_df_test_subset_1), 
                                                 nrow = nrow(FO_grass_df_test_subset_1))), 
                               colnames(FO_grass_df_test_subset_1))
  rownames(FO_grass_df_pred_1)<-rownames(FO_grass_df_test_subset_1)
  
  for(i in 1:ncol(FO_grass_df_test_subset_1)){ #for every timestep of 24 sec
    i=1
    #print indivual inputs for heat function
    FO_grass_df_test_subset_1[,1] #print u -> first column (first timestep)
    alpha.range[x] #alpha
    heights_grass
    pred_temp<-heat_heights(u=FO_grass_df_test_subset_1[,i], alpha=alpha.range[x], 
                            xdelta=heights_grass, tdelta=difftime_grass_1[i], n=2)
    FO_grass_df_pred_1[,i+1]<-pred_temp[2,]
  }
  FO_grass_df_pred_1<-FO_grass_df_pred_1[-1,] #remove first row (invalid)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-1] #remove first column (cannot be predicted)
  FO_grass_df_pred_1<-FO_grass_df_pred_1[,-dim(FO_grass_df_pred_1)[2]] #remove last column -> no measured values
  data_predicted<-as.vector(t(FO_grass_df_pred_1))
  alpha_rmse_1$RMSE[x]<-sqrt(mean((data_measured - data_predicted)^2))
  
}
