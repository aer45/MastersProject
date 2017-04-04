# Self Organizing Map

# Libraries
library(kohonen)
library(rpart)
library(SOMbrero)

# Colors
# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# Inialize grid
redd_mat<-as.matrix(redd)
som_grid <- somgrid(xdim=15, ydim=15, topo='hexagonal')
set.seed(3)
som_model <- som(redd_mat,grid=som_grid,rlen=100,
                 alpha = c(0.05,0.01),
                 keep.data=T,
                 n.hood='circular')

plot(som_model, type='changes')
plot(som_model, type='count')
plot(som_model, type='dist.neighbours')


plot(som_model, type = "property", property = test3[,3],
     main='Total Encounters', bgcol=pretty_palette)

mydata <- som.model$prototypes 
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
# for (i in 2:15) {
#   wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
# }
# plot(wss)

sil_width <- c(NA)

for (i in 2:10){
  set.seed(10)
  som.fit <- pam(mydata,diss=F, k = i)
  sil_width[i] <- som.fit$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
lines(1:10, sil_width)

set.seed(3)
som.fit<-pam(mydata,diss=F,k=4)
som.cluster<-som.fit$clustering[som.model$clustering]

set.seed(10)
som.model<-trainSOM(redd_mat,dimension=c(10,10))
plot(superClass(som.model))
sc.som<-superClass(som.model,k=4)
table(sc.som$cluster)
plot(sc.som)

# plot these results:
# plot(som_model, type="mapping", bgcol = som.fit$clustering, 
#      main = "Clusters") 
# add.cluster.boundaries(som_model, som.fit$clustering)

scatterplot3d(s.decomp$u[,1]*s.decomp$d[1],s.decomp$u[,2]*s.decomp$d[2],
              s.decomp$u[,3]*s.decomp$d[3],color=som.cluster,pch=19,cex.symbols=1.5,
              main='2 Clusters (SOM)',
              xlab='PC1',
              ylab='PC2',
              zlab='PC3')

fit_c <- c(NA)
fit_b <- c(NA)
fit_c <- apply(test3[,continuous], 2, function(x) kruskal.test(x~som.fit$clustering,test3[,continuous])$p.value)

fit_b <- sapply(test3[,binary],function(x) fisher.test(x,som.fit$clustering)$p.value)

fit<-c(fit_c,fit_b)

top_vars <- names(fit[fit<=0.05])
top_vars <- names(head(fit[order(fit)],100))
top_vars2 <- names(fit[fit==min(fit)])
top_vars2 <- names(head(fit[order(fit)],10))


# Show both medoids
medoids <- test3[c(som.fit$id.med),]

pamdf <- data.frame('ID'=id$PatientIdentifier,'Cluster'=som.fit$clustering,test3.scaled[,top_vars])
pamdf <- pamdf[som.fit$id.med,]
#pamdfd <- apply(pamdfd,2,as.numeric)
pamdfd <- melt(pamdf,id.vars=c('ID','Cluster'))
#ggplot(data=pamdfd1,aes(x=variable,y=value,fill=Cluster))+geom_boxplot()+facet_grid(.~Cluster)

pamdfd <- pamdfd[order(pamdfd$Cluster),]
pamdfd$variable<-as.character(pamdfd$variable)
pamdfd$variable<-ifelse(substr(pamdfd$variable,1,1)!='X',pamdfd$variable,
                        substr(pamdfd$variable,2,nchar(pamdfd$variable)))
#pamdfd$variable<-gsub('\\_DXSUBCAT$',' (DXSC)',pamdfd$variable)

ggplot(pamdfd, aes(factor(Cluster),variable)) +
  geom_tile(aes(fill = value), color = "black") +
  scale_fill_gradient(low = "black", high = "green") +
  theme_grey(base_size=9)+labs(x="",y="",title='Medoids Heatmap (SOM)')+
  scale_x_discrete(expand=c(0,0))+#scale_y_discrete(expand=c(0,0))+
  theme(legend.position="none",axis.ticks=element_blank(),
        axis.text.x=element_text(size=14,angle=0,hjust=0)) 






# Decision tree to pull out variable information
# rfdata<-data.frame('SOMCluster'=as.factor(som_cluster[som_model$unit.classif]),test3)
# rf<-randomForest(SOMCluster~.,rfdata,importance=T)
# 
# dt <- rpart(SOMCluster~.,rfdata)
# 
# 
# plot(som_model, type = "property", property = test3[,"ED"],
#      main='ED Encounters')
