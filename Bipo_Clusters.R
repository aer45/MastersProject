# Two step clustering for bipolar disorder

library(ade4)
library(NbClust)

# Cluster bipolar ICDs
bipo_df <- test3[Dx$Dx%in%c('Bipolar','Both'),]

# Only keep pheno icd's
bipo_id <- bipo_df[,c(1:2)]
bipo_df <- bipo_df[,names(bipo_df)%in%bipo]

# Change to binary
bipo_df <- ifelse(bipo_df==0,0,1)

# Take out redundant ICDs
bipo_df <- 


### Continuous Distance Metric (Euclidean) ###

# Scale
bipo_df_scale <- scale(bipo_df)

# SVD
bipo_decomp <- svd(bipo_df_scale)
svimp<-bipo_decomp$d^2/sum(bipo_decomp$d^2)
plot(svimp)
lines(svimp)
index<-which(cumsum(svimp)>0.95)[1]
bipo_red <- as.matrix(bipo_decomp$u[,1:index]%*%diag(bipo_decomp$d[1:index]))

# Optimal cluster number check
set.seed(10)
foo<-fviz_nbclust(bipo_df_scale, pam, method='silhouette', k.max=25)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
fooset.seed(11)
fviz_nbclust(bipo_red, pam, method='wss', k.max=50)
set.seed(12)
fviz_nbclust(bipo_red, pam, method='gap_stat', k.max=(dim(bipo_red)[1]-100))+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#best_clust_num <- as.integer(foo$data$clusters[order(foo$data$y,decreasing = T)][1])
# Take 'best' clusters and visualize
bipo_cluster <- pam(bipo_red, diss=F, k=best_clust_num)
fviz_cluster(bipo_cluster)
div_bipo <- diana(bipo_red)
plot(div_bipo)

ag_bipo <- agnes(bipo_red)
pltree(ag_bipo,cex=0.6, hang=-1)
#rect.hclust(ag_bipo,k=10)


### Dice Distance for Binary ###
bipo_bin_dist <- dist.binary(bipo_df,method=5)
sil_width <- c(NA)

for (i in 2:50){
  bipo_bin_fit <- pam(bipo_bin_dist,diss=T, k = i)
  sil_width[i] <- bipo_bin_fit$silinfo$avg.width
}

plot(1:50, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
lines(1:50, sil_width)

bipo_cluster <- pam(bipo_bin_dist, diss=T , k=2)
plot_ly(x=bipo_decomp$u[,1]*bipo_decomp$d[1],y=bipo_decomp$u[,2]*bipo_decomp$d[2],
        z=bipo_decomp$u[,3]*bipo_decomp$d[3],color=bipo_cluster$clustering)
