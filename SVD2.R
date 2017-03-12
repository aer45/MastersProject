# Transform variables and perform SVD then cluster

# Libraries
library(ggplot2)

# Transformation Step
test3<-test3[,colSums(test3)!=0]

# Take out scz+bipo icds
test3_nopheno<-test3[,!names(test3)%in%c(sczo,bipo)]

# Change ICD to binary
last.icd <- grep('\\_DXCAT$',names(test3))[1]-1
first.gender <- grep('\\_GENDER$',names(test3))[1]
test3[,7:last.icd]<-ifelse(test3[,7:last.icd]==0,-1,1)
test3[,first.gender:length(test3)]<-ifelse(test3[,first.gender:length(test3)]==0,-1,1)
# # Change binary to 1 -1 
# test3[,7:last.icd]<-ifelse(test3[,7:last.icd]==0,-1,1)

# Split into ID, continous, and counts
id <- test3[,c(1:2)]
test3 <- test3[,-c(1:2)]
 
# Scale and decompose df
test3.scaled<-scale(test3)
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
redd<-data.frame(s.decomp$u[,1:index]%*%diag(s.decomp$d[1:index]))
