# Transform variables and perform SVD then cluster

# Libraries
library(cluster)

# Transformation Step
# Use log transformation first on all counts
id<-test3[,1:2]
test3.t<-log(test3[,-c(1,2)]+1)

# Use global entropy transformation
g_i<-apply(test3[,-c(1:5)],2,sum)
p_ij<-sweep(test3[,-c(1:5)],2,g_i,'/')
for(i in 1:dim(p_ij)[2]){
      for (j in 1:dim(p_ij)[1]){
        if (p_ij[j,i]!=0){
          p_ij[j,i]<-p_ij[j,i]*log(p_ij[j,i])/log(dim(p_ij)[1])
        } else {
          p_ij[j,i]<-0
        }
  }
}
G_i<-1+colSums(p_ij)

test3<-sweep(test3.t[,-c(1:3)],2,G_i,'*')
test3<-data.frame(test3.t[,c(1:3)],test3)
rm(test3.t,p_ij,G_i,g_i,i,j)

#test3.t<-t(test3)
s.decomp<-svd(test3)

# Plot singular value importance
svimp<-s.decomp$d^2/sum(s.decomp$d^2)
plot(svimp)
lines(svimp)
index<-which(cumsum(svimp)>0.95)[1]

# Reconstruct matrix with full rank from svd
redd<-data.frame(s.decomp$u[,1:index]%*%diag(s.decomp$d[1:index])%*%t(s.decomp$v)[1:index,])

# PAM
sil_width <- c(NA)

for (i in 2:10){
  pam.fit <- pam(redd,diss=F, k = i)
  sil_width[i] <- pam.fit$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
lines(1:10, sil_width)

# Choose n clusters
pam.fit<-pam(redd, diss=F ,k=4)


# Visualization
plot(s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
     col=pam.fit$clustering)
