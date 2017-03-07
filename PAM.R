#PAM from SVD

# Libraries
library(cluster)
library(entropy)
library(scatterplot3d)
library(ggplot2)
library(plotly)
library(factoextra) # fviz_cluster
library(GGally) #ggparcoord

# PAM
# Silhouettee Width
set.seed(10)
fviz_nbclust(redd, pam, method='silhouette')
# sil_width <- c(NA)
# 
# for (i in 2:10){
#   pam.fit <- pam(redd,diss=F, k = i)
#   sil_width[i] <- pam.fit$silinfo$avg.width
# }
# 
# plot(1:10, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
# lines(1:10, sil_width)

# Gap Statistic
set.seed(10)
gap.stat <- clusGap(redd, FUN= pam, K.max=10) 
fviz_gap_stat(gap.stat)

# Choose n clusters
set.seed(1)
pam.fit<-pam(redd, diss=F ,k=2)


# Visualization with singular values
plot(s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
     col=pam.fit$clustering)

scatterplot3d(s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
              s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=pam.fit$clustering)

plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
        z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=pam.fit$clustering)
plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
        z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=Dx$Dx)


disspam <- daisy(redd)
pamplot <- cmdscale(disspam,k=3)
plot(x=pamplot[,1],y=pamplot[,2],col=pam.fit$clustering)

fviz_cluster(pam.fit)

# Try with 3 clusters
# set.seed(2)
# pam.fit<-pam(redd, diss=F, k=3)
# plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
#         z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=pam.fit$clustering)
# plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
#         z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=Dx$Dx)
# plot_ly(x=pamplot[,1],y=pamplot[,2],z=pamplot[,3],color=pam.fit$clustering)


# Interpret 2 cluster ids
#test3$pam_cluster<-pam.fit$clustering


# Figure out what vars are important
continuous <- c(1:4,(last.icd+1):(first.gender-1))
binary <- c(5:last.icd,first.gender:length(test3))

fit_c <- c(NA)
fit_b <- c(NA)
fit_c <- apply(test3[,continuous], 2, function(x) kruskal.test(x~pam.fit$clustering,test3[,continuous])$p.value)

fit_b <- sapply(test3[,binary],function(x) fisher.test(x,pam.fit$clustering)$p.value)

fit<-c(fit,fit2)
top_vars <- names(fit[fit<=0.05])
top_vars2 <- names(head(fit[order(fit)],50))


pamdf <- data.frame('ID'=id$PatientIdentifier,'Cluster'=pam.fit$clustering,test3.scaled[,top_vars])
#pamdfd <- apply(pamdfd,2,as.numeric)
pamdfd <- melt(pamdf,id.vars=c('ID','Cluster'))
#ggplot(data=pamdfd1,aes(x=variable,y=value,fill=Cluster))+geom_boxplot()+facet_grid(.~Cluster)

pamdfd <- pamdfd[order(pamdfd$Cluster),]

ggplot(pamdfd, aes(factor(ID),variable)) +
  geom_tile(aes(fill = value), color = "green") +
  scale_fill_gradient(low = "white", high = "red") +
   theme(axis.text.y=element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1))
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank(),
  #       axis.title.y=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank())
  
plot_ly(x=as.factor(pamdfd$ID),y=as.factor(pamdfd$variable),
        z=as.matrix(pamdf[,-c(1:2)]),
            colors=colorRamp(c("white","blue")),type="heatmap")# %>%
           #layout(xaxis=list(title="ICD9 Code",tickangle=45),yaxis=list(title="Patient ID",showticklabels=F))
        


pamdf2 <- data.frame('ID'=id$PatientIdentifier,'Cluster'=factor(pam.fit$clustering),test3[,top_vars2])
pamdf2 <- pamdf2[order(pamdf2$Cluster),]
ggparcoord(pamdf2[,-1],groupColumn=1,columns=3:length(pamdf2)-1,scale='uniminmax')+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))+
  geom_line(size=1.5)
