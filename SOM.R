# Self Organizing Map

# Libraries
library(kohonen)
library(rpart)

# Colors
# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# Inialize grid
redd_mat<-as.matrix(redd)
som_grid <- somgrid(xdim=3, ydim=3, topo='hexagonal')
som_model <- som(redd_mat,grid=som_grid,rlen=2000,
                 alpha = c(0.05,0.01),
                 keep.data=T,
                 n.hood='circular')

plot(som_model, type='changes')
plot(som_model, type='count')
plot(som_model, type='dist.neighbours')


plot(som_model, type = "property", property = test3[,3],
     main='Total Encounters', bgcol=pretty_palette)

mydata <- som_model$codes 
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
# for (i in 2:15) {
#   wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
# }
# plot(wss)

# sil_width <- c(NA)
# 
# for (i in 2:5){
#   som.fit <- pam(mydata,diss=F, k = i)
#   sil_width[i] <- som.fit$silinfo$avg.width
# }
# 
# plot(1:5, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
# lines(1:5, sil_width)

set.seed(10)
fviz_nbclust(mydata, pam, method='silhouette',k.max=5)

set.seed(3)
som.fit<-pam(mydata,diss=F,k=2)

# plot these results:
plot(som_model, type="mapping", bgcol = som.fit$clustering, 
     main = "Clusters") 
add.cluster.boundaries(som_model, som.fit$clustering)

fviz_cluster(som.fit)






# Decision tree to pull out variable information
# rfdata<-data.frame('SOMCluster'=as.factor(som_cluster[som_model$unit.classif]),test3)
# rf<-randomForest(SOMCluster~.,rfdata,importance=T)
# 
# dt <- rpart(SOMCluster~.,rfdata)
# 
# 
# plot(som_model, type = "property", property = test3[,"ED"],
#      main='ED Encounters')
