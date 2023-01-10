#extract subsets for Klemm
FO_concrete_df_test_subset_1
ktest<-as.data.frame(t(FO_concrete_df_test_subset_1))
ktest$time<-as.POSIXct(rownames(ktest))
colnames(ktest)

#get 10min means
means <- aggregate(ktest, 
                   list(fiveMin=cut(ktest$time, "10 mins")),
                   mean)

test_Klemm<-means[1:12,-23]

test_Klemm<-as.data.frame(t(test_Klemm))
colnames(test_Klemm)<-test_Klemm[1,]
test_Klemm<-test_Klemm[-1,]

setwd("C:/00_Dana/Uni/Masterarbeit/Klemm_Bodenwärmestronm")
write.csv(test_Klemm, file="subset_concrete_10minmean_10cmdepth_for2h.csv")
