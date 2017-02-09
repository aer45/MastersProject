# Imputation of Missing Values

# Libraries
library(randomForest) # for missing value imputation
library(VIM) # aggr_plot for missing values
library(psych) # used for descriptives

# Test to see what variables have missing values
names(test3)[sapply(test3, function(x) sum(is.na(x)))>0]

aggr_plot <- aggr(test3[,c('AverageDaysBetweenEncounters','PovertyPercent')], 
                  col=c('green','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=c('ADBE','Poverty Percent'), cex.axis=.7, 
                  gap=3, ylab=c("Histogram of missing data","Pattern"))
# Average Days Between Encounters
# Input max possible value for time interval
test3$AverageDaysBetweenEncounters<-ifelse(is.na(test3$AverageDaysBetweenEncounters),30,test3$AverageDaysBetweenEncounters)

# PovertyPercent
describe(test3$PovertyPercent)

# Random Forest model inputation
test3$PIG<-as.factor(paste0(test3$PatientIdentifier,'_',test3$Group))
set.seed(1)
misspp<-rfImpute(x = PIG~.,data = test3[,-c(1:2)], iter=1, ntree=100, mtry=ceiling(dim(test3)[2]^(1/3)))

# Pull together dataframe and summarize Poverty Percent
test3<-data.frame(test3[,c(1:2)],misspp[,-1])
describe(test3$PovertyPercent)

# Visualize no missing data
aggr_plot <- aggr(test3[,c('AverageDaysBetweenEncounters','PovertyPercent')], 
                  col=c('green','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=c('ADBE','Poverty Percent'), cex.axis=.7, 
                  gap=3, ylab=c("Histogram of missing data","Pattern"))
