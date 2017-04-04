# Entropy for comorbidities

library(entropy)

id <- test3[,c(1,2)]
test3_con <- test3[,c(3,4)]
test3_bin <- test3[,c(5:length(test3))]
test3_bin <- ifelse(test3_bin==0,0,1)

foo <- sapply(test3_bin,function(x) entropy(table(test3_bin[,x]),unit='log'))
foo <- rbind(dim(test3_bin)[1]-colSums(test3_bin),colSums(test3_bin))
foo1 <- apply(foo,2,entropy)


foo2 <- as.data.frame(sweep(scale(test3_bin),2,foo1,'*'))
foo2$Avg_Age <- scale(test3$AVERAGE_AGE)
foo2$Tot_Enc <- scale(test3$TotalEncounters)
#foo3 <- scale(foo2)
foo2 <- apply(foo2,2,as.numeric)

foo_dist <- daisy(foo2,  metric="euclidean")

sil_width <- c(NA)
for (i in 2:10){
  set.seed(10)
  foo_fit <- pam(foo_dist,diss=T, k = i)
  sil_width[i] <- foo_fit$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Average Silhouette Width',main='Optimal Number of Clusters (Gower)')
lines(1:10, sil_width)

set.seed(1)
foo_fit <- pam(foo_dist, diss=T, k=2)

foo_cmd<-cmdscale(foo_dist,k=3,eig=T)
scatterplot3d(foo_cmd$points[,1],foo_cmd$points[,2],foo_cmd$points[,3])

