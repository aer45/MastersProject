# Plots for Talk

# Libraries
library(ggplot2)
library(dplyr)
library(scales)
library(sqldf)
library(VennDiagram)
library(gridExtra)
library(reshape2)
library(gtools)

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

# Read datasets in
patient<-read.csv("C:/Data/patient.csv",stringsAsFactors=F)
encounter<-read.csv("C:/Data/encounter.csv",stringsAsFactors=F)
diagnoses <- read.csv("C:/Data/patientdiagnoses.csv",stringsAsFactors = F)
medications <- read.csv("C:/Data/patientmedications.csv",stringsAsFactors = F)
outmed <- read.csv("C:/Data/outpatientmedrecon.csv",stringsAsFactors = F)
plabs <- read.csv("C:/Data/patientlabs.csv",stringsAsFactors = F)
names(patient) <- gsub("\\.","",names(patient))
names(encounter) <- gsub("\\.","",names(encounter))
names(diagnoses) <- gsub("\\.","",names(diagnoses))
names(medications) <- gsub("\\.","",names(medications))
names(outmed) <- gsub("\\.","",names(outmed))
names(plabs) <- gsub("\\.","",names(plabs))

# Demographic Plots
# Patient Race
# Combine like races
patient$PatientRace[patient$PatientRace=='CAUCASIAN/WHITE']<-'WHITE OR CAUCASIAN'
patient$PatientRace[patient$PatientRace=='AMERICAN INDIAN']<-'AMERICAN INDIAN OR ALASKAN NATIVE'
patient$PatientRace[patient$PatientRace=='2 OR MORE RACES']<-'MULTIRACIAL'
patient$PatientRace[patient$PatientRace=='NOT REPORTED/DECLINED']<-'UNAVAILABLE'

ggplot(patient,aes(PatientRace))+geom_bar(fill='black')+
  geom_text(stat='count',aes(label=..count..),vjust=-0.5,size=7)+
  labs(x='Reported Race',y='Patient Count',title='Patient Race Barplot')+
  scale_x_discrete(labels=c("American Indian or\nAlaskan Native","Asian","Black or\nAfrican American",
                  "Multiracial","Native Hawaiian or\nOther Pacific Islander","Other","Unavailable",
                  "White or\nCaucasian"))

# Patient Age
ggplot(patient,aes(PatientYearofBirth))+geom_histogram(binwidth=2)
ggplot(encounter,aes(PatientAgeatArrivalinyears))+geom_histogram(binwidth=2)

# Patient Gender
ggplot(patient,aes(PatientGender))+geom_bar(fill=c("pink","blue"),alpha=0.8)+
  geom_text(stat='count',aes(label=..count..),vjust=-0.5,size=7)+
  labs(x="Patient Gender",y="Patient Count",title="Barplot of Patient Gender")

# Patient Ethnicity
# Collapse groups
patient$PatientEthnicGroup[patient$PatientEthnicGroup=='NOT REPORTED/DECLINED']<-'DECLINED'
patient$PatientEthnicGroup[patient$PatientEthnicGroup=='DECLINED']<-'DECLINED OR UNAVAILABLE'
patient$PatientEthnicGroup[patient$PatientEthnicGroup=='UNAVAILABLE']<-'DECLINED OR UNAVAILABLE'
patient$PatientEthnicGroup[patient$PatientEthnicGroup=='OTHER']<-'NOT HISPANIC/LATINO'
EthnicityCount<-data.frame(Ethnicity=patient$PatientEthnicGroup,Count=rep(1,length(patient$PatientEthnicGroup)))
EthnicityCount<-as.data.frame(EthnicityCount %>% group_by(Ethnicity) %>% summarise(no_rows=length(Ethnicity)))
EthnicityCount$prop<-EthnicityCount$no_rows/sum(EthnicityCount$no_rows)
EthnicityCount<-EthnicityCount[order(-EthnicityCount$prop),]
EthnicityCount$pos<-cumsum(EthnicityCount$no_rows)-EthnicityCount$no_rows/2

ggplot(EthnicityCount,aes(x=factor(1),y=no_rows,fill=Ethnicity))+geom_bar(width=1,stat='identity')+
  coord_polar(theta='y')+theme(panel.grid=element_blank(),axis.text.x=element_blank(),
  axis.ticks=element_blank(),axis.title.x = element_blank(),axis.title.y=element_blank())+
  geom_text(EthnicityCount[1,],aes(x=factor(1),y=pos ,label=percent(prop)))+
  scale_fill_brewer(palette="Spectral")+labs(title="Piechart of Patient Ethnicity")



# ICD9 Barplot
all.schizo <- sqldf('select PatientIdentifier, EncounterIdentifier, ICDDiagnosisCode, PatientGender, PatientRace , 1 as "Count"
                    from diagnoses
                    where substr(ICDDiagnosisCode,1,3)=="295"')
all.schizo<-all.schizo[!substr(all.schizo$ICDDiagnosisCode,1,5)%in%c('295.3','295.4','295.7','295.9'),]
s.icd <- ggplot(data=all.schizo)+geom_bar(aes(x=ICDDiagnosisCode, fill=PatientGender),stat='count')+
  labs(title="Schizophrenia ICD9 Diagnosis Count",x="",y="Count")+theme(axis.text.x=element_text(angle=45,hjust=1))

all.bipo <- sqldf('select PatientIdentifier, EncounterIdentifier, ICDDiagnosisCode, PatientGender, PatientRace, 1 as "Count"
                  from diagnoses
                  where substr(ICDDiagnosisCode,1,3)=="296"')
all.bipo<-all.bipo[!substr(all.bipo$ICDDiagnosisCode,1,5)%in%c('296.0','296.2','296.20','296.21','296.22',
          '296.23','296.24','296.25','296.26','296.3','296.30','296.31','296.32','296.33','296.34','296.35',
          '296.36','296.4','296.5','296.8','296.9'),]
b.icd <- ggplot(data=all.bipo)+geom_bar(aes(x=ICDDiagnosisCode, fill=PatientGender),stat='count')+
  labs(title="Bipolar ICD9 Diagnosis Count",x="ICD9 Diagnosis Code",y="Count")+theme(axis.text.x=element_text(angle=45,hjust=1))
grid_arrange_shared_legend(s.icd,b.icd,nrow=2)


# Venn Diagram

schizo <- sqldf('select distinct PatientIdentifier, 1 as "Schizophrenia Diagnosis"
                from "all.schizo" 
                where substr(ICDDiagnosisCode,1,3)=="295"')
bipo <- sqldf('select distinct PatientIdentifier, 1 as "Bipolar Diagnosis"
              from "all.bipo"
              where substr(ICDDiagnosisCode,1,3)=="296"')
diagnum <- merge(schizo,bipo,all=T)
diagnum[is.na(diagnum)] <- 0
diagnum$Both<-ifelse(sum(diagnum[,-1])==2,1,0)
diagnum[,2]<-ifelse(diagnum$Both==1,0,diagnum[,2])
diagnum[,3]<-ifelse(diagnum$Both==1,0,diagnum[,3])
grid.newpage()
draw.pairwise.venn(colSums(diagnum)[2],colSums(diagnum)[3],colSums(diagnum)[4],
                   category=c("Schizophrenia Phenotype Patients","Bipolar Phenotype Patients"),fill=c('green','yellow'),
                   cat.pos=c(0,0))

# Heatmap
# Use only frequent codes
# Find mean count for diagnoses
scz.data<-dcast(all.schizo,ICDDiagnosisCode~Count,value.var='Count',fun.aggregate=sum)
bip.data<-dcast(all.bipo,ICDDiagnosisCode~Count,value.var='Count',fun.aggregate=sum)
# Subset diagnosis data and take codes that appear greater than scz:25 bip:100
scz.cut<-scz.data[scz.data$`1`>=25,1]
bip.cut<-bip.data[bip.data$`1`>=100,1]
short.schizo<-all.schizo[all.schizo$ICDDiagnosisCode%in%scz.cut,]
short.bipo<-all.bipo[all.bipo$ICDDiagnosisCode%in%bip.cut,]
wide.short.schizo <- dcast(short.schizo, PatientIdentifier~ICDDiagnosisCode,sum)
wide.short.bipo <- dcast(short.bipo, PatientIdentifier~ICDDiagnosisCode,sum)
wide.short <- smartbind(wide.short.schizo,wide.short.bipo,fill=0)
wide.short.scale <- log10(wide.short[,-1]+1)
wide.short.scale[,"PatientIdentifier"] <- wide.short[,"PatientIdentifier"]

# wide.short.scale <- wide.short %>% mutate_all(funs(scale)) 
# wide.short.scale$PatientIdentifier <- wide.short$PatientIdentifier
# wide.short.scale[is.na(wide.short.scale)]<-0

# plot_ly(x=names(wide.short.scale)[-1],y=as.factor(wide.short.scale[,1]),z=as.matrix(wide.short.scale[,-1]),
#         colors=colorRamp(c("white","black")),type="heatmap") %>%
#   layout(xaxis=list(title="ICD9 Code",tickangle=45),yaxis=list(title="Patient ID",showticklabels=F))

long.short.scale <- melt(wide.short.scale,"PatientIdentifier")

heat <- ggplot(long.short.scale, aes(variable, as.factor(PatientIdentifier))) + geom_tile(aes(fill = value),
                                                                                          colour = "white") + scale_fill_gradient(low = "white",high = "darkblue",name="Transformed\nCount")+
  theme(axis.text.x=element_text(angle=45,hjust=1),axis.text.y =element_blank(),
        axis.ticks.y=element_blank())+labs(x="ICD Code",y="Patient",title="Log Transformed ICD Code Count by Patient")+
  geom_vline(xintercept = 296,size=10,color="black")
heat
diagnosis.sub <- sqldf('select distinct ICDDiagnosisCode, DiagnosisName
                       from diagnoses
                       where substr(ICDDiagnosisCode,1,3)=="295" or
                       substr(ICDDiagnosisCode,1,3)=="296"')

diagnosis.sub <- unique(diagnoses[diagnoses$ICDDiagnosisCode %in% unique(long.short.scale$variable),c("ICDDiagnosisCode","DiagnosisName")])
diagnosis.sub <- diagnosis.sub[order(as.numeric(diagnosis.sub$ICDDiagnosisCode)),]
diagnosis.sub <- diagnosis.sub[!duplicated(diagnosis.sub$ICDDiagnosisCode),]
names(diagnosis.sub)<-c("ICD Code","Description")

tab <- tableGrob(diagnosis.sub,rows=NULL)
tab$widths <- unit(c(1/6,5/6),"npc")
grid.arrange(tab)
grid.newpage()
grid.arrange(heat,tab, nrow=2)

# Reset patientid based on phenotypes
patient<-patient[patient$PatientIdentifier%in%c(all.schizo$PatientIdentifier,all.bipo$PatientIdentifier),]
encounter<-encounter[encounter$PatientIdentifier%in%c(all.schizo$PatientIdentifier,all.bipo$PatientIdentifier),]

patient$PatientRace[patient$PatientRace=='CAUCASIAN/WHITE']<-'WHITE OR CAUCASIAN'
patient$PatientRace[patient$PatientRace=='AMERICAN INDIAN']<-'AMERICAN INDIAN OR ALASKAN NATIVE'
patient$PatientRace[patient$PatientRace=='2 OR MORE RACES']<-'MULTIRACIAL'
patient$PatientRace[patient$PatientRace=='NOT REPORTED/DECLINED']<-'UNAVAILABLE'
ggplot(patient,aes(PatientRace))+geom_bar(fill='black')+
  geom_text(stat='count',aes(label=..count..),vjust=-0.5,size=7)+
  labs(x='Reported Race',y='Patient Count',title='Patient Race Barplot')+
  scale_x_discrete(labels=c("American Indian or\nAlaskan Native","Asian","Black or\nAfrican American",
                            "Multiracial","Other","Unavailable",
                            "White or\nCaucasian"))

# Patient Age
ggplot(encounter,aes(PatientAgeatArrivalinyears))+geom_histogram(binwidth=3)+labs(x='Patient Age At Arrival',
                y='Frequency',title='Patient Age at Arrival in Years')

# Patient Gender
ggplot(patient,aes(PatientGender))+geom_bar(fill=c("pink","blue"),alpha=0.8)+
  geom_text(stat='count',aes(label=..count..),vjust=-0.5,size=7)+
  labs(x="Patient Gender",y="Patient Count",title="Barplot of Patient Gender")

# Patient Ethnicity
EthnicityCount<-data.frame(Ethnicity=patient$PatientEthnicGroup,Count=rep(1,length(patient$PatientEthnicGroup)))
EthnicityCount<-as.data.frame(EthnicityCount %>% group_by(Ethnicity) %>% summarise(no_rows=length(Ethnicity)))
EthnicityCount$prop<-EthnicityCount$no_rows/sum(EthnicityCount$no_rows)
EthnicityCount<-EthnicityCount[order(-EthnicityCount$prop),]
EthnicityCount$pos<-cumsum(EthnicityCount$no_rows)-EthnicityCount$no_rows/2

ggplot(EthnicityCount,aes(x=factor(1),y=no_rows,fill=Ethnicity))+geom_bar(width=1,stat='identity')+
  coord_polar(theta='y')+theme(panel.grid=element_blank(),axis.text.x=element_blank(),
                               axis.ticks=element_blank(),axis.title.x = element_blank(),axis.title.y=element_blank(),
                               axis.text.y=element_blank())+
  geom_text(aes(x=factor(1),y=pos ,label=percent(prop)))+
  scale_fill_brewer(palette="Spectral")+labs(title="Piechart of Patient Ethnicity")
