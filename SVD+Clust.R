# Transform variables and perform SVD then cluster

# Libraries
library(cluster)
library(entropy)
library(scatterplot3d)
library(plotly)

# Transformation Step
test3<-test3[,colSums(test3)!=0]

# Split into ID, continous, and counts
id <- test3[,c(1:2)]
continous <- test3[,c('AverageDaysBetweenEncounters','AVERAGE_AGE','PovertyPercent','TotalEncounters')]
count <- test3[,!(names(test3)%in%names(continous))]
count <- count[,-c(1:2)]
test3 <- test3[,-c(1:2)]
# Use log transformation first on all counts
count_log<-log(count+1)

# Use global entropy transformation
g_i<-apply(count,2,sum)
p_ij<-sweep(count,2,g_i,'/')
G_i<-apply(p_ij,2,entropy)
count_t<-sweep(count_log,2,G_i,'*')

# Scale numeric data
continous_scaled <- scale(continous)

# Pull all count and numeric data back together
test3.scaled<-data.frame(continous_scaled,count_t)
rm(continous, count, count_log, p_ij, G_i, g_i, continous_scaled, count_t)

#test3.t<-t(test3)
s.decomp<-svd(test3.scaled)

# Plot singular value importance
svimp<-s.decomp$d^2/sum(s.decomp$d^2)
plot(svimp)
lines(svimp)
index<-which(cumsum(svimp)>0.95)[1]

# Find SVD importance with variables
decomp.imp <- data.frame('variable'=names(test3),s.decomp$v[,1:4])
d1 <- head(decomp.imp[order(abs(decomp.imp$X1),decreasing=T),c(1:2)])
d2 <- head(decomp.imp[order(abs(decomp.imp$X2),decreasing=T),c(1,3)])
d3 <- head(decomp.imp[order(abs(decomp.imp$X3),decreasing=T),c(1,4)])
d4 <- head(decomp.imp[order(abs(decomp.imp$X4),decreasing=T),c(1,5)])

vd1<-ggplot(d1,aes(x=variable,y=X1))+geom_bar(stat='identity')+coord_flip()
vd2<-ggplot(d2,aes(x=variable,y=X2))+geom_bar(stat='identity')+coord_flip()
vd3<-ggplot(d3,aes(x=variable,y=X3))+geom_bar(stat='identity')+coord_flip()
vd4<-ggplot(d4,aes(x=variable,y=X4))+geom_bar(stat='identity')+coord_flip()
multiplot(vd1,vd2,vd3,vd4,cols=2)

rm(vd1, vd2, vd3, vd4)

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
set.seed(1)
pam.fit<-pam(redd, diss=F ,k=2)


# Visualization
plot(s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
     col=pam.fit$clustering)

scatterplot3d(s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
              s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=pam.fit$clustering)

plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
         z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=pam.fit$clustering)
plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
        z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=Dx$Dx)


# Try with 3 clusters
set.seed(2)
pam.fit<-pam(redd, diss=F, k=3)
plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
        z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=pam.fit$clustering)
plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
        z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=Dx$Dx)

