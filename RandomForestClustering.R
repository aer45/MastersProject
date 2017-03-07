# Random Forest Proximity Clustering

# Libraries
library(dendextend)

#names(redd)<-names(test3)

rfdist <- randomForest(redd,importance=T,proximity=T)
varImpPlot(rfdist,n.var = 5)

#classCenter(redd,label=id,rfdist$proximity)
dissrf<-sqrt(1-rfdist$proximity)

# Silhouettee Width
set.seed(10)
fviz_nbclust(redd, pam, diss=dissrf, method='silhouette')

# Gap Statistic
set.seed(10)
fviz_nbclust(redd, pam, diss=dissrf ,method='gap_stat')
gap.stat <- clusGap(dissrf, FUN= pam, K.max=10) 
fviz_gap_stat(gap.stat)

# sil_width <- c(NA)
# 
# for (i in 2:10){
#   foo <- pam(dissrf,diss=T, k = i)
#   sil_width[i] <- foo$silinfo$avg.width
# }
# 
# plot(1:10, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
# lines(1:10, sil_width)

set.seed(2)
rf.fit<-pam(dissrf,diss=T,k=10)

fviz_cluster(rf.fit,data=dissrf)

# rfplot <- cmdscale(dissrf)
# foo<-pam(dissrf,diss=T,k=2)
# plot(x=rfplot[,1],y=rfplot[,2],col=foo$clustering)

# Using hierarchical clustering

foo2<-agnes(dissrf,diss=T)
plot(foo2)

foo3<-diana(dissrf,diss=T)
plot(foo3)

# Create two dendrograms
dend1 <- as.dendrogram(foo2)
dend2 <- as.dendrogram(foo3)

# Create a list of dendrograms
dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2)

