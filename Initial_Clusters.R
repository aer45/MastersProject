#############################################
# New Script to Test Clinic/Hospital Merge  #
# And Include Med/Diag Venn                 #
#############################################

# Libraries
library(ggplot2)
library(dplyr)
library(scales)
library(sqldf)
library(VennDiagram)
library(gridExtra)
library(reshape2)
library(gtools)
library(cluster)

# Functions
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

# Read datasets

# Hospital
h.patient <- read.csv("C:/Data/Hospital/patient.csv",stringsAsFactors=F)
h.encounter <- read.csv("C:/Data/Hospital/encounter.csv",stringsAsFactors=F)
h.diagnoses <- read.csv("C:/Data/Hospital/patientdiagnoses.csv",stringsAsFactors = F)
h.medications <- read.csv("C:/Data/Hospital/patientmedications.csv",stringsAsFactors = F)
h.social <- read.csv("C:/Data/Hospital/socialhistory.csv",stringsAsFactors = F)
h.plabs <- read.csv("C:/Data/Hospital/patientlabs.csv",stringsAsFactors = F)
names(h.patient) <- gsub("\\.","",names(h.patient))
names(h.encounter) <- gsub("\\.","",names(h.encounter))
names(h.diagnoses) <- gsub("\\.","",names(h.diagnoses))
names(h.medications) <- gsub("\\.","",names(h.medications))
names(h.social) <- gsub("\\.","",names(h.social))

# Clinic
c.patient <- read.csv("C:/Data/Clinic/patient.csv",stringsAsFactors=F)
c.encounter <- read.csv("C:/Data/Clinic/encounter.csv",stringsAsFactors=F)
c.diagnoses <- read.csv("C:/Data/Clinic/patientdiagnoses.csv",stringsAsFactors = F)
c.medications <- read.csv("C:/Data/Clinic/patientmedications.csv",stringsAsFactors = F)
c.social <- read.csv("C:/Data/Clinic/socialhistory.csv",stringsAsFactors = F)
c.plabs <- read.csv("C:/Data/Clinic/patientlabs.csv",stringsAsFactors = F)
names(c.patient) <- gsub("\\.","",names(c.patient))
names(c.encounter) <- gsub("\\.","",names(c.encounter))
names(c.diagnoses) <- gsub("\\.","",names(c.diagnoses))
names(c.medications) <- gsub("\\.","",names(c.medications))
names(c.social) <- gsub("\\.","",names(c.social))
names(c.plabs) <- gsub("\\.","",names(c.plabs))


# Merge datasets
patient <- rbind(h.patient,c.patient)
patient <- patient[unique(patient$PatientIdentifier),]

diagnoses <- rbind(h.diagnoses,c.diagnoses)


# Create patient diagnosis venn
scz.pat<-sqldf('select distinct PatientIdentifier, 1 as "Schizophrenia Diagnosis"
               from diagnoses
               where substr(ICDDiagnosisCode,1,3)=="295" or
                      substr(ICDDiagnosisCode,1,2)=="F2"')

bip.pat<-sqldf('select distinct PatientIdentifier, 1 as "Bipolar Diagnosis"
               from diagnoses
               where substr(ICDDiagnosisCode,1,3)=="296" or
                      substr(ICDDiagnosisCode,1,2)=="F3"')

diagnum <- merge(scz.pat,bip.pat,all=T)
diagnum[is.na(diagnum)] <- 0
diagnum$Both<-ifelse(rowSums(diagnum[,-1])==2,1,0)
diagnum.sum<-colSums(diagnum[,-1])
grid.newpage()
draw.pairwise.venn(diagnum.sum[1],diagnum.sum[2],diagnum.sum[3],
                   category=c("Schizophrenia Phenotype Patients","Bipolar Phenotype Patients"),fill=c('green','yellow'),
                   cat.pos=c(0,0))

# Create heatmap and cluster using base package
all.schizo <- sqldf('select PatientIdentifier, EncounterIdentifier, ICDDiagnosisCode, 1 as "Count"
                    from diagnoses
                    where substr(ICDDiagnosisCode,1,3)=="295" or
                          substr(ICDDiagnosisCode,1,2)=="F2"')
all.bipo <- sqldf('select PatientIdentifier, EncounterIdentifier, ICDDiagnosisCode, 1 as "Count"
                  from diagnoses
                  where substr(ICDDiagnosisCode,1,3)=="296" or
                      substr(ICDDiagnosisCode,1,2)=="F3"')
scz.data <- dcast(all.schizo,PatientIdentifier~ICDDiagnosisCode,value.var='Count',fun.aggregate=sum)
row.names(scz.data) <- scz.data$PatientIdentifier
scz.data <- scz.data[,-1]
heatmap(data.matrix(scz.data),scale='column',Colv = NA)

bip.data<-dcast(all.bipo,PatientIdentifier~ICDDiagnosisCode,value.var='Count',fun.aggregate=sum)
row.names(bip.data) <- bip.data$PatientIdentifier
bip.data <- bip.data[,-1]
heatmap(data.matrix(bip.data),scale='column',Colv = NA)

scz.data.s<-scale(scz.data)
scz.d<-dist(scz.data.s,method='euclidean')
scz.fit<-hclust(scz.d,method='ward')
plot(scz.fit)
scz.groups<-cutree(scz.fit,k=4)
rect.hclust(scz.fit,k=4,border='red')
clusplot(scz.data.s, scz.groups, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

bip.data.s<-scale(bip.data)
bip.d<-dist(bip.data.s,method='euclidean')
bip.fit<-hclust(bip.d,method='ward')
plot(bip.fit)


# Create clusters based on all ICD codes for patient
all.schizo.icd<-diagnoses[diagnoses$PatientIdentifier%in%all.schizo$PatientIdentifier,c('PatientIdentifier','ICDDiagnosisCode')]
all.schizo.icd<-data.frame(all.schizo.icd,'Count'=1)
scz.data.all<-dcast(all.schizo.icd,PatientIdentifier~ICDDiagnosisCode,value.var='Count',fun.aggregate=sum)
scz.data.all$Var.2<-scz.data.all$Var.2+scz.data.all[,3]
scz.data.all<-scz.data.all[,-3]
names(scz.data.all)[2]<-'Missing'
row.names(scz.data.all)<-scz.data.all$PatientIdentifier
scz.data.all<-scz.data.all[,-1]
scz.data.all.s<-scale(scz.data.all)

scz.d.all<-dist(scz.data.all.s,method='euclidean')
scz.fit.all<-hclust(scz.d.all,method='ward')
plot(scz.fit.all)
scz.groups.all<-cutree(scz.fit.all,k=3)
rect.hclust(scz.fit.all,k=3,border='red')
clusplot(scz.data.all.s, scz.groups.all, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# For BOTH diagnoses of interest
all.icd<-diagnoses[diagnoses$PatientIdentifier%in%c(all.schizo$PatientIdentifier,all.bipo$PatientIdentifier),c('PatientIdentifier','ICDDiagnosisCode')]
all.icd<-data.frame(all.icd,'Count'=1)
data.all<-dcast(all.icd,PatientIdentifier~ICDDiagnosisCode,value.var='Count',fun.aggregate=sum)
data.all$Var.2<-data.all$Var.2+scz.data.all[,3]
data.all<-data.all[,-3]
names(scz.data.all)[2]<-'Missing'
row.names(scz.data.all)<-scz.data.all$PatientIdentifier
scz.data.all<-scz.data.all[,-1]
scz.data.all.s<-scale(scz.data.all)

scz.d.all<-dist(scz.data.all.s,method='euclidean')
scz.fit.all<-hclust(scz.d.all,method='ward')
plot(scz.fit.all)
scz.groups.all<-cutree(scz.fit.all,k=3)
rect.hclust(scz.fit.all,k=3,border='red')
clusplot(scz.data.all.s, scz.groups.all, color=TRUE, shade=TRUE, 
         labels=2, lines=0)
