# Gower distance and then PAM, no SVD

# Libraries
library(cluster) # daisy

# Check for normality
par(mfrow=c(2,2))
hist(test3$PovertyPercent)
hist(test3$TotalEncounters)
hist(test3$AverageDaysBetweenEncounters)
hist(test3$AVERAGE_AGE)

# Log transform
hist(log(test3$PovertyPercent+1))
hist(log(test3$TotalEncounters+1))
hist(log(test3$AverageDaysBetweenEncounters+1))

hist(sqrt(test3$PovertyPercent))
hist(sqrt(test3$AverageDaysBetweenEncounters))

hist(scale(test3$PovertyPercent))

# Change all except first four columns to binary and factorize
if(names(test3)[1]=='PatientIdentifier'){
  id<-test3[,c(1:2)]
  test3<-test3[,-c(1:2)]
}
test3[,5:length(test3)]<-ifelse(test3[,5:length(test3)]<=0,0,1)
#test3[,5:length(test3)]<-apply(test3[,5:length(test3)],2,as.factor)
test3[,1:4]<-scale(test3[,1:4])

# Find dist using gower metric
gower_dist <- daisy(test3, metric = "gower",
                    type=list(asymm=c(5:length(test3))))
sil_width <- c(NA)
set.seed(10)
for (i in 2:10){
  gow_fit <- pam(gower_dist,diss=T, k = i)
  sil_width[i] <- gow_fit$silinfo$avg.width
}

plot(1:10, sil_width, xlab='Number of clusters', ylab='Silhouette Width')
lines(1:10, sil_width)
