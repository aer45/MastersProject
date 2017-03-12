# Two step clustering for schizophrenia

# Cluster sczolar ICDs
sczo_df <- test3[Dx$Dx%in%c('Schizophrenia','Both'),]

# Only keep pheno icd's
sczo_id <- sczo_df[,c(1:2)]
sczo_df <- sczo_df[,names(sczo_df)%in%sczo]

# Change to binary
sczo_df <- ifelse(sczo_df==0,0,1)

# Take out no information ICDs
sczo_df <- sczo_df[,colSums(sczo_df)!=0]

# Scale
sczo_df_scale <- scale(sczo_df)

# SVD
sczo_decomp <- svd(sczo_df_scale)
svimp<-sczo_decomp$d^2/sum(sczo_decomp$d^2)
plot(svimp)
lines(svimp)
index<-which(cumsum(svimp)>0.95)[1]
sczo_red <- as.matrix(sczo_decomp$u[,1:index]%*%diag(sczo_decomp$d[1:index]))

# Optimal cluster number check
set.seed(10)
fviz_nbclust(sczo_df_scale, pam, method='silhouette', k.max=40)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
set.seed(11)
fviz_nbclust(sczo_red, pam, method='wss', k.max=50)
set.seed(12)
fviz_nbclust(sczo_df_scale, pam, method='gap_stat', k.max=40)+theme(axis.text.x = element_text(angle = 90, hjust = 1))

#best_clust_num <- as.integer(foo$data$clusters[order(foo$data$y,decreasing = T)][1])
# Take 'best' clusters and visualize
sczo_cluster <- pam(sczo_red, diss=F, k=best_clust_num)
fviz_cluster(sczo_cluster)
div_sczo <- diana(sczo_red)
plot(div_sczo)

ag_sczo <- agnes(sczo_red)
pltree(ag_sczo,cex=0.6, hang=-1)
#rect.hclust(ag_sczo,k=10)


# Binary test #
sczo_bin_dist <- dist.binary(sczo_df,method=5)
sil_width <- c(NA)

for (i in 2:40){
  sczo_bin_fit <- pam(sczo_bin_dist,diss=T, k = i)
  sil_width[i] <- sczo_bin_fit$silinfo$avg.width
}

plot(1:40, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
lines(1:40, sil_width)

ag_sczo_bin <- agnes(sczo_bin_dist,diss=T,method='complete')
pltree(ag_sczo_bin,cex=0.6,hang=-1)
rect.hclust(ag_sczo_bin,k=10)
