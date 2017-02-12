# Imputation of Missing Values and Phenotype Subsetting

# Libraries
library(randomForest) # for missing value imputation
library(VIM) # aggr_plot for missing values
library(psych) # used for descriptives

# Subset sample based on phenotype definitions
pheno_icd<-c("295.00","295.01","295.02","295.03","295.04","295.05","295.10","295.11","295.12",
"295.13","295.14","295.15","295.20","295.21","295.22","295.23","295.24","295.25",
"295.3","295.30","295.31","295.32","295.33","295.34","295.35","295.4","295.40",
"295.41","295.42","295.43","295.44","295.45","295.50","295.51","295.52","295.53",
"295.54","295.60","295.61","295.62","295.63","295.64","295.65","295.7","295.70",
"295.71","295.72","295.73","295.74","295.75","295.80","295.81","295.82","295.83",
"295.84","295.85","295.9","295.90","295.91","295.92","295.93","295.94","295.95",
"296.0","296.00","296.01","296.02","296.03","296.04","296.05","296.06","296.10",
"296.11","296.12","296.13","296.14","296.15","296.16","296.4","296.40","296.41",
"296.42","296.43","296.44","296.45","296.46","296.5","296.50","296.51","296.52",
"296.53","296.54","296.55","296.56","296.60","296.61","296.62","296.63","296.64",
"296.65","296.66","296.7","296.8","296.80","296.81","296.82","296.89","296.9",
"296.90","296.99","F30.10","F30.11","F30.2","F30.4","F30.8","F30.9","F31.0",
"F31.10","F31.11","F31.12","F31.13","F31.2","F31.30","F31.31","F31.32","F31.4",
"F31.5","F31.60","F31.61","F31.62","F31.63","F31.64","F31.70","F31.71","F31.72",
"F31.73","F31.74","F31.75","F31.76","F31.77","F31.78","F31.81","F31.89","F31.9",
"F32.8","F33.8","F34.8","F34.9","F39","F20.0","F20.1","F20.2","F20.3","F20.5",
"F20.81","F20.89","F20.9","F25.0","F25.1","F25.8","F25.9")

pheno_subset<-test3[,names(test3)%in%pheno_icd]
pheno_subset<-data.frame(test3$PatientIdentifier,pheno_subset)

count<-rowsum(pheno_subset,pheno_subset$test3.PatientIdentifier)
count2<-apply(count[,-1],1,sum)
drop<-count2<2
dropid<-count[drop==T,'test3.PatientIdentifier']

test3<-test3[!(test3$PatientIdentifier%in%dropid),]

rm(count, count2, drop, pheno_icd, pheno_outpatient, pheno_subset)

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
