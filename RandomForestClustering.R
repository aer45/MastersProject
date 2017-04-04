# Random Forest Proximity Clustering

# Libraries
library(dendextend)

#names(redd)<-names(test3)
set.seed(8)
rfdist <- randomForest(test3,importance=T,proximity=T,ntree=100)
varImpPlot(rfdist,n.var = 5,type=2)

#classCenter(redd,label=id,rfdist$proximity)
dissrf<-1-rfdist$proximity

# Silhouettee Width

# Gap Statistic
set.seed(10)
fviz_nbclust(redd, pam, diss=dissrf ,method='gap_stat')
gap.stat <- clusGap(dissrf, FUN= pam, K.max=10) 
fviz_gap_stat(gap.stat)

sil_width <- c(NA)

for (i in 2:10){
  set.seed(10)
  rf_pam <- pam(dissrf,diss=T, k = i)
  sil_width[i] <- rf_pam$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Average Silhouette Width',main='Optimal Number of Clusters (RF)')
lines(1:10, sil_width)

set.seed(9)
rf_pam <- pam(dissrf,diss=T,k=2)

scatterplot3d(s.decomp$u[,1]*s.decomp$d[1],s.decomp$u[,2]*s.decomp$d[2],
              s.decomp$u[,3]*s.decomp$d[3],color=rf_pam$clustering,pch=19,cex.symbols=1.5,
              main='2 Clusters (RF)',
              xlab='PC1',
              ylab='PC2',
              zlab='PC3')

fit_c <- c(NA)
fit_b <- c(NA)
fit_c <- apply(test3[,continuous], 2, function(x) kruskal.test(x~rf_pam$clustering,test3[,continuous])$p.value)

fit_b <- sapply(test3[,binary],function(x) fisher.test(x,rf_pam$clustering)$p.value)

fit<-c(fit_c,fit_b)

top_vars <- names(fit[fit<=0.05])
top_vars <- names(head(fit[order(fit)],100))
top_vars2 <- names(fit[fit==min(fit)])
top_vars2 <- names(head(fit[order(fit)],10))


# Show both medoids
medoids <- test3[c(rf_pam$id.med),]

pamdf <- data.frame('ID'=id$PatientIdentifier,'Cluster'=rf_pam$clustering,test3.scaled[,top_vars])
pamdf <- pamdf[rf_pam$id.med,]
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
  theme_grey(base_size=9)+labs(x="",y="",title='Medoids Heatmap (RF)')+
  scale_x_discrete(expand=c(0,0))+#scale_y_discrete(expand=c(0,0))+
  theme(legend.position="none",axis.ticks=element_blank(),
        axis.text.x=element_text(size=14,angle=0,hjust=0)) 


        #axis.text.y=element_blank())

pamdf2 <- data.frame('ID'=id$PatientIdentifier,'Cluster'=factor(rf_pam$clustering),test3[,top_vars2])
#pamdf2 <- pamdf2[order(pamdf2$Cluster),]
names(pamdf2)<-as.character(names(pamdf2))
names(pamdf2)<-ifelse(substr(names(pamdf2),1,1)!='X',names(pamdf2),
                      substr(names(pamdf2),2,nchar(names(pamdf2))))
names(pamdf2)<-gsub('\\_DXSUBCAT$',' (DXSC)',names(pamdf2))
names(pamdf2)<-gsub('\\_DXCAT$',' (DXC)',names(pamdf2))
names(pamdf2)<-ifelse(nchar(names(pamdf2))>=10,gsub('\\.',' ',names(pamdf2)),names(pamdf2))
#columns <- names(pamdf2)
#names(pamdf2)<-letters[1:12]

pamdf2 <- apply(pamdf2,2,as.numeric)
pamdf2[,3:12] <- scale(pamdf2[,3:12])
pamdf2 <- as.data.frame(pamdf2)
pamdf2[pamdf2$ID%in%id$PatientIdentifier[as.numeric(rf_pam$medoids)],'Cluster']<-c(3,4)
pamdf2 <- aggregate(.~Cluster,data=pamdf2[,-1],mean)
# pamdf2$n <- table(gow_fit$clustering)
pamdf2 <- melt(pamdf2,id=c('Cluster'))
#pamdf2[pamdf2$ID%in%id$PatientIdentifier[as.numeric(rf_pam$medoids)],'Cluster']<-c(3,4)
pamdf2$Lab<-ifelse(pamdf2$Cluster%in%c(3,4),2,1)

# ggparcoord(pamdf2,groupColumn='Cluster',columns=2:length(pamdf2),scale='uniminmax')+
#   theme(axis.text.x=element_text(angle = 90, hjust = 0,vjust=0),legend.position="none")+
#   scale_y_discrete(expand=c(0,0))+scale_x_discrete(expand=c(0,0))+labs(x='',y='',title='Parallel Coordinates Plot (RF)')#+
#   geom_line(size=1)+scale_colour_manual(values=c("1"="black","2"="red"))

#pamdf2$Cluster<-as.factor(pamdf2$Cluster)  
ggplot(pamdf2, aes(variable,value,color=factor(Cluster),group=Cluster,size=factor(Lab)))+
  geom_line()+
  scale_colour_manual(values=c("black","red","black","red"))+
  scale_alpha_manual(values=c(.05,.05,1,1))+
  scale_size_manual(values=c(0.25,2))+
  labs(x='',y='',title='Parallel Coordinates Plot (RF)')+
  theme(axis.text.x=element_text(angle = 90, hjust = 0,vjust=0),legend.position="none")
 
pamdf3 <- data.frame(test3[,1:4],rf_pam$clustering)
describeBy(pamdf3[,-5],pamdf3$rf_pam.clustering)
