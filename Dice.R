# Dice Calculation

library(ade4)


# Subset to only cat variables
first.cat<-first(grep('\\_DXCAT$',names(test3)))
last.cat<-last(grep('\\_DXCAT$',names(test3)))
test3_bin <- test3[,first.cat:last.cat]

# Calculate dice
bin_dist <- dist.binary(test3_bin,method=5)

# Dim red for visualizations
test3_bin_scaled <- scale(test3_bin)
d.decomp <- svd(test3_bin_scaled)

# Cluster with PAM
sil_width <- c(NA)

for (i in 2:10){
  set.seed(5)
  bin_fit <- pam(bin_dist,diss=T, k = i)
  sil_width[i] <- bin_fit$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Average Silhouette Width',main='Optimal Number of Clusters (Dice)')
lines(1:10, sil_width)

set.seed(10)
bin_cluster <- pam(bin_dist, diss=T , k=3)

# Visualize 

scatterplot3d(d.decomp$u[,1]*d.decomp$d[1],d.decomp$u[,2]*d.decomp$d[2],
              d.decomp$u[,3]*d.decomp$d[3],color=bin_cluster$clustering,pch=19,cex.symbols=1.5,
              main='3 Clusters (Dice)',
              xlab='PC1',
              ylab='PC2',
              zlab='PC3')

# Interpretation

fit <- sapply(test3_bin,function(x) fisher.test(x,bin_cluster$clustering)$p.value)
top_vars <- names(head(fit[order(fit)],100))
top_vars2 <- names(head(fit[order(fit)],10))

medoids <- test3_bin[c(bin_cluster$id.med),]

pamdf <- data.frame('ID'=id$PatientIdentifier,'Cluster'=bin_cluster$clustering,test3_bin_scaled[,top_vars])
pamdf <- pamdf[bin_cluster$id.med,]
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
