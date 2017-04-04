# Gower distance and then PAM, no SVD

# Libraries
library(cluster) # daisy
library(NbClust)

# Change all except first four columns to binary and factorize
if(names(test3)[1]=='PatientIdentifier'){
  id<-test3[,c(1:2)]
  test3<-test3[,-c(1:2)]
}

all_var <- length(test3)

#test3[,5:length(test3)]<-ifelse(test3[,5:length(test3)]<=0,0,1)
test3[,5:length(test3)]<-apply(test3[,5:length(test3)],2,as.factor)
#test3[,1:4]<-scale(test3[,1:4])

# Find dist using gower metric
gower_dist <- daisy(x=as.data.frame(test3), metric = "gower", type=list(asymm=c(5:all_var)))

#gower_clust <- NbClust(data=test3,diss=gower_dist,distance=NULL,min.nc=2,max.nc=10,method='centroid',index='silhouette')
#fviz_nbclust(gower_clust)

sil_width <- c(NA)
for (i in 2:10){
  set.seed(10)
  gow_fit <- pam(gower_dist,diss=T, k = i)
  sil_width[i] <- gow_fit$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Average Silhouette Width',main='Optimal Number of Clusters (Gower)')
lines(1:10, sil_width)

set.seed(5)
gow_fit <- pam(gower_dist,diss=T,k=2)

scatterplot3d(s.decomp$u[,1]*s.decomp$d[1],s.decomp$u[,2]*s.decomp$d[2],
              s.decomp$u[,3]*s.decomp$d[3],color=gow_fit$clustering,pch=19,cex.symbols=1.5,
              main='2 Clusters (Gower)',
              xlab='PC1',
              ylab='PC2',
              zlab='PC3')

continuous <- c(1:4)
binary <- c(5:length(test3))

fit_c <- c(NA)
fit_b <- c(NA)
fit_c <- apply(test3[,continuous], 2, function(x) kruskal.test(x~gow_fit$clustering,test3[,continuous])$p.value)

fit_b <- sapply(test3[,binary],function(x) fisher.test(x,gow_fit$clustering)$p.value)

fit<-c(fit_c,fit_b)

top_vars <- names(fit[fit<=0.05])
top_vars <- names(head(fit[order(fit)],100))
top_vars2 <- names(fit[fit==min(fit)])
top_vars2 <- names(head(fit[order(fit)],10))


# Show both medoids
medoids <- test3[c(gow_fit$id.med),]

pamdf <- data.frame('ID'=id$PatientIdentifier,'Cluster'=gow_fit$clustering,test3.scaled[,top_vars])
pamdf <- pamdf[gow_fit$id.med,]
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
  theme_grey(base_size=9)+labs(x="",y="",title='Medoids Heatmap (Gower)')+
  scale_x_discrete(expand=c(0,0))+#scale_y_discrete(expand=c(0,0))+
  theme(legend.position="none",axis.ticks=element_blank(),
        axis.text.x=element_text(size=14,angle=0,hjust=0)) 

# ggplot(pamdfd, aes(factor(Cluster),variable)) +
#   geom_tile(aes(fill = value), color = "black") +
#   scale_fill_gradient(low = "black", high = "red") +
#   theme_grey(base_size=9)+labs(x="",y="",title='Medoids Heatmap (Gower)')+
#   scale_x_discrete(expand=c(0,0))+scale_y_discrete(expand=c(0,0))+
#   theme(legend.position="none",axis.ticks=element_blank(),
#         axis.text.x=element_text(size=14,angle=0,hjust=0),
#         axis.text.y=element_blank())

pamdf2 <- data.frame('ID'=id$PatientIdentifier,'Cluster'=factor(gow_fit$clustering),test3[,top_vars2])
#pamdf2 <- pamdf2[order(pamdf2$Cluster),]
names(pamdf2)<-as.character(names(pamdf2))
names(pamdf2)<-ifelse(substr(names(pamdf2),1,1)!='X',names(pamdf2),
                      substr(names(pamdf2),2,nchar(names(pamdf2))))
names(pamdf2)<-gsub('\\_DXSUBCAT$',' (DXSC)',names(pamdf2))
names(pamdf2)<-gsub('\\_DXCAT$',' (DXC)',names(pamdf2))
names(pamdf2)<-ifelse(nchar(names(pamdf2))>=10,gsub('\\.',' ',names(pamdf2)),names(pamdf2))
#columns <- names(pamdf2)
#names(pamdf2)<-letters[1:12]

ggparcoord(pamdf2,groupColumn='Cluster',columns=3:length(pamdf2),scale='std')+
  theme(axis.text.x=element_text(angle = 90, hjust = 0,vjust=0),legend.position="none")+
  scale_y_discrete(expand=c(0,0))+scale_x_discrete(expand=c(0,0))+labs(x='',y='',title='Parallel Coordinates Plot (Gower)')+
  geom_line(size=1)+scale_colour_manual(values=c("1"="black","2"="red"))

# pamdf2 <- apply(pamdf2,2,as.numeric)
# pamdf2 <- as.data.frame(pamdf2)
# pamdf2 <- aggregate(.~Cluster,data=pamdf2[,-1],mean)
# pamdf2$n <- table(gow_fit$clustering)
# pamdf2 <- melt(pamdf2,id=c('Cluster','n'))
# pamdf2$n <- as.numeric(pamdf2$n)
# #pamdf2$value <- pamdf2$value/pamdf2$n
# 
# 
# ggplot(pamdf2, aes(x = variable, y = value, group = Cluster)) + 
#   geom_path(aes(size=n,colour = Cluster),
#             alpha = 1,
#             lineend = 'round', linejoin = 'round') +
#   labs(y='Average',x='')+theme(legend.position='none',axis.text.x=element_text(angle=45,hjust=1))

  #scale_color_manual(values=c("1"='black',"2"='red'))

pamdf3 <- data.frame(test3[,1:4],gow_fit$clustering)
describeBy(pamdf3[,-5],pamdf3$gow_fit.clustering)
