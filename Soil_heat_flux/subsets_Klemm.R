#extract subsets for Klemm
FO_concrete_df_test_subset_1
ktest<-as.data.frame(t(FO_concrete_df_test_subset_1))
ktest$time<-as.POSIXct(rownames(ktest))
colnames(ktest)

#get 10min means
means <- aggregate(ktest, 
                   list(fiveMin=cut(ktest$time, "10 mins")),
                   mean)

test_Klemm<-means[1:10,-23]

test_Klemm
