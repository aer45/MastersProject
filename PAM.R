#PAM from SVD

# Libraries
library(cluster)
library(entropy)
library(scatterplot3d)
library(ggplot2)
library(plotly)
library(factoextra) # fviz_cluster
library(GGally) #ggparcoord
library(ggparallel)

# PAM
# Silhouettee Width
#fviz_nbclust(redd, pam, method='silhouette')
sil_width <- c(NA)
for (i in 2:10){
  set.seed(10)
  pam.fit <- pam(redd,diss=F, k = i)
  sil_width[i] <- pam.fit$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Average Silhouette Width',main='Optimal Number of Clusters')
lines(1:10, sil_width)

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

scatterplot3d(s.decomp$u[,1]*s.decomp$d[1],s.decomp$u[,2]*s.decomp$d[2],
              s.decomp$u[,3]*s.decomp$d[3],color=pam.fit$clustering,pch=19,cex.symbols=1.5,
              main='2 Clusters',
              xlab='PC1',
              ylab='PC2',
              zlab='PC3')

plot_ly(x=s.decomp$u[,1]*s.decomp$d[1],y=s.decomp$u[,2]*s.decomp$d[2],
        z=s.decomp$u[,3]*s.decomp$d[3],color=pam.fit$clustering,type='scatter3d',mode='markers') %>% 
        layout(title="Two Clusters",
               scene = list(
                xaxis=list(title='PC1'),
                yaxis=list(title='PC2'),
                zaxis=list(title='PC3')),
               showlegend=FALSE)

plot_ly(x=s.decomp$u[,1]*s.decomp$d[1]%*%t(s.decomp$v)[1],y=s.decomp$u[,2]*s.decomp$d[2]%*%t(s.decomp$v)[2],
        z=s.decomp$u[,3]*s.decomp$d[3]%*%t(s.decomp$v)[3],color=Dx$Dx)

# Interpret 2 cluster ids
#test3$pam_cluster<-pam.fit$clustering


# Figure out what vars are important
continuous <- c(1:4)
binary <- c(5:length(test3))

fit_c <- c(NA)
fit_b <- c(NA)
fit_c <- apply(test3[,continuous], 2, function(x) kruskal.test(x~pam.fit$clustering,test3[,continuous])$p.value)

fit_b <- sapply(test3[,binary],function(x) fisher.test(x,pam.fit$clustering)$p.value)

fit<-c(fit_c,fit_b)

top_vars <- names(fit[fit<=0.05])
top_vars2 <- names(fit[fit==min(fit)])
top_vars <- names(head(fit[order(fit)],100))
top_vars2 <- names(head(fit[order(fit)],10))


# Show both medoids
medoids <- test3[c(pam.fit$id.med),]

pamdf <- data.frame('ID'=id$PatientIdentifier,'Cluster'=pam.fit$clustering,test3.scaled[,top_vars])
pamdf <- pamdf[pam.fit$id.med,]
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
  theme_grey(base_size=9)+labs(x="",y="",title='Medoids Heatmap')+
  scale_x_discrete(expand=c(0,0))+#scale_y_discrete(expand=c(0,0))+
  theme(legend.position="none",axis.ticks=element_blank(),
        axis.text.x=element_text(size=14,angle=0,hjust=0))

# ggplot(pamdfd, aes(factor(Cluster),variable)) +
#   geom_tile(aes(fill = value), color = "black") +
#   scale_fill_gradient(low = "black", high = "red") +
#   theme_grey(base_size=9)+labs(x="",y="",title='Medoids Heatmap')+
#   scale_x_discrete(expand=c(0,0))+scale_y_discrete(expand=c(0,0))+
#   theme(legend.position="none",axis.ticks=element_blank(),
#        axis.text.x=element_text(size=14,angle=0,hjust=0),
#        axis.text.y=element_blank())

pamdf2 <- data.frame('ID'=id$PatientIdentifier,'Cluster'=factor(pam.fit$clustering),test3[,top_vars2])
pamdf2 <- pamdf2[order(pamdf2$Cluster),]
names(pamdf2)<-as.character(names(pamdf2))
names(pamdf2)<-ifelse(substr(names(pamdf2),1,1)!='X',names(pamdf2),
                        substr(names(pamdf2),2,nchar(names(pamdf2))))
names(pamdf2)<-gsub('\\_DXSUBCAT$',' (DXSC)',names(pamdf2))
names(pamdf2)<-ifelse(nchar(names(pamdf2))>=10,gsub('\\.',' ',names(pamdf2)),names(pamdf2))

ggparcoord(pamdf2[,-1],groupColumn=1,columns=3:length(pamdf2)-1,scale='std')+
  #theme(axis.text.x=element_blank())+#
  theme(axis.text.x=element_text(angle = 90, hjust = 0, vjust=0),legend.position="none")+
  scale_y_discrete(expand=c(0,0))+scale_x_discrete(expand=c(0,0))+labs(x='',y='',title='Parallel Coordinates Plot')+
  geom_line(size=1.5)+scale_colour_manual(values=c("1"="black","2"="red"))

# pamdf2 <- aggregate(.~Cluster,data=pamdf2[,-1],sum)
# pamdf2$n <- table(pam.fit$clustering)
# pamdf2 <- melt(pamdf2,id=c('Cluster','n'))
# pamdf2$n <- as.numeric(pamdf2$n)
# pamdf2$value <- pamdf2$value/pamdf2$n
# 
# ggplot(pamdf2, aes(x = variable, y = value, group = Cluster)) + 
#   geom_path(aes(size=n,colour = Cluster),
#             alpha = 1,
#             lineend = 'round', linejoin = 'round') +
#   labs(y='Percentage',x='')+theme(legend.position='none',axis.text.x=element_text(angle=45,hjust=1))+
#   scale_color_manual(values=c("1"='black',"2"='red'))+scale_y_continuous(labels=scales::percent)

pamdf3 <- data.frame(test3[,1:4],pam.fit$clustering)
describeBy(pamdf3[,-5],pamdf3$pam.fit.clustering)
