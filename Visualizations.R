## Heatmap Code ##

# Packages
library(sqldf)
library(plyr)
library(reshape2)
library(gplots)
library(VennDiagram)
library(ggplot2)
library(gridExtra)
library(plotly)
library(dplyr)

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


# Datasets
diagnoses <- read.csv("C:/Data/patientdiagnoses.csv",stringsAsFactors = F)
medications <- read.csv("C:/Data/patientmedications.csv",stringsAsFactors = F)
outmed <- read.csv("C:/Data/outpatientmedrecon.csv",stringsAsFactors = F)
plabs <- read.csv("C:/Data/patientlabs.csv",stringsAsFactors = F)
names(diagnoses) <- gsub("\\.","",names(diagnoses))
names(medications) <- gsub("\\.","",names(medications))
names(outmed) <- gsub("\\.","",names(outmed))
names(plabs) <- gsub("\\.","",names(plabs))


icd9 <- sqldf('select PatientIdentifier, ICDDiagnosisCode, 1 as "count"
  from diagnoses 
  where substr(ICDDiagnosisCode,1,3)=="295" or
              substr(ICDDiagnosisCode,1,3)=="296"')
icd9 <- dcast(icd9, PatientIdentifier ~ ICDDiagnosisCode,sum,value.var='count')

meds <- sqldf('select PatientIdentifier, GenericName, 1 as "count"
              from medications')

#test <- dcast(meds, PatientIdentifier ~ GenericName,sum,value.var='count')


#################################################################################
#                                                                               #
# Summary Statistics                                                            #
#                                                                               #
#################################################################################

summary(diagnoses$PatientYearofBirth)
summary(plabs$AgeontheDayoftheVisitinyears)
summary(plabs$PatientAgeatArrivalinyears)


#################################################################################
#                                                                               #
# Venn Diagram                                                                  #
#                                                                               #
#################################################################################

schizo <- sqldf('select distinct PatientIdentifier, 1 as "Schizophrenia Diagnosis"
              from diagnoses 
              where substr(ICDDiagnosisCode,1,3)=="295"')
bipo <- sqldf('select distinct PatientIdentifier, 1 as "Bipolar Diagnosis"
              from diagnoses
              where substr(ICDDiagnosisCode,1,3)=="296"')
diagnum <- merge(schizo,bipo,all=T)
diagnum[is.na(diagnum)] <- 0
venn(diagnum[,-1])



###############################################################################
#                                                                             #
# ICD9 Plot                                                                   #
#                                                                             #
###############################################################################

all.schizo <- sqldf('select PatientIdentifier, EncounterIdentifier, ICDDiagnosisCode, PatientGender, PatientRace , 1 as "Count"
                    from diagnoses
                    where substr(ICDDiagnosisCode,1,3)=="295"')
s.icd <- ggplot(data=all.schizo)+geom_bar(aes(x=ICDDiagnosisCode, fill=PatientGender),stat='count')+
          labs(title="Schizophrenia ICD9 Codes Barplot",x="",y="Count")+theme(axis.text.x=element_text(angle=45,hjust=1))

all.bipo <- sqldf('select PatientIdentifier, EncounterIdentifier, ICDDiagnosisCode, PatientGender, PatientRace, 1 as "Count"
                    from diagnoses
                    where substr(ICDDiagnosisCode,1,3)=="296"')
b.icd <- ggplot(data=all.bipo)+geom_bar(aes(x=ICDDiagnosisCode, fill=PatientGender),stat='count')+
          labs(title="Bipolar ICD9 Codes Barplot",x="ICD9 Diagnosis Code",y="Count")+theme(axis.text.x=element_text(angle=45,hjust=1))
grid_arrange_shared_legend(s.icd,b.icd,nrow=2)


#############################################################################
#                                                                           #
# Medications Plot                                                          #
#                                                                           #
#############################################################################

med.list <- toupper(sapply(strsplit(medications$MedicationName," "), "[[", 1))

ggplot(medications,aes(x=PharmaceuticalClass,fill=PharmaceuticalClass))+geom_bar(stat='count')

#############################################################################
#                                                                           #
# Heatmap                                                                   #
#                                                                           #
#############################################################################
wide.all.schizo <- dcast(all.schizo, PatientIdentifier~ICDDiagnosisCode,sum)
wide.all.bipo <- dcast(all.bipo, PatientIdentifier~ICDDiagnosisCode,sum)
wide.all <- merge(wide.all.schizo,wide.all.bipo,by="PatientIdentifier")
wide.all.scale <- apply(wide.all,2,scale)
wide.all.scale[,"PatientIdentifier"] <- wide.all[,"PatientIdentifier"]

wide.all.scale <- wide.all %>% mutate_all(funs(scale)) 
wide.all.scale$PatientIdentifier <- wide.all$PatientIdentifier
wide.all.scale[is.na(wide.all.scale)]<-0

plot_ly(x=names(wide.all.scale)[-1],y=wide.all.scale[,1],z=as.matrix(wide.all.scale[,-1]),
        colors=colorRamp(c("white","black")),type="heatmap") %>%
        layout(xaxis=list(title="ICD9 Code",tickangle=45),yaxis=list(title="Patient ID",showticklabels=F))

long.all.scale <- melt(wide.all.scale,"PatientIdentifier")
#long.all.scale <- long.all.scale[long.all.scale$PatientIdentifier<10000,]

heat <- ggplot(long.all.scale, aes(variable, as.factor(PatientIdentifier))) + geom_tile(aes(fill = value),
          colour = "white") + scale_fill_gradient(low = "white",high = "darkblue",name="Normalized\nCount")+
          theme(axis.text.x=element_text(angle=90,hjust=1),axis.text.y =element_blank(),
          axis.ticks.y=element_blank())+labs(x="ICD Code",y="Patient",title="Normalized ICD Code Count by Patient")+
          geom_vline(xintercept = 296,size=10,color="black")
heat
diagnosis.sub <- sqldf('select distinct ICDDiagnosisCode, DiagnosisName
                       from diagnoses
                       where substr(ICDDiagnosisCode,1,3)=="295" or
                            substr(ICDDiagnosisCode,1,3)=="296"')

diagnosis.sub <- unique(diagnoses[diagnoses$ICDDiagnosisCode %in% unique(long.all.scale$variable),c("ICDDiagnosisCode","DiagnosisName")])
diagnosis.sub <- diagnosis.sub[order(as.numeric(diagnosis.sub$ICDDiagnosisCode)),]
diagnosis.sub <- diagnosis.sub[!duplicated(diagnosis.sub$ICDDiagnosisCode),]

tab <- tableGrob(diagnosis.sub,rows=NULL)
tab$widths <- unit(c(1/5,4/5),"npc")


lay <- rbind(c(1,1,NA),
             c(1,1,2),
             c(1,1,NA))
grid.newpage()
grid.arrange(heat,tab,layout_matrix=lay)
#grid.arrange(heat,tab, ncol=2)
grid.newpage()
grid.arrange(tab)


#########################################################
#                                                       #
# Labs                                                  #
#                                                       #
#########################################################

plabs$datec<-as.Date(plabs$SpecimenCollectionDate,format="%m/%d/%Y")

# Number of unique panels and tests
length(unique(plabs$TestName))
length(unique(plabs$PanelName))

# plabs.wide<-reshape(plabs,
#                     timevar="PanelName",
#                     idvar=c("PatientIdentifier","datec"),
#                     direction="wide")
# 
# 
# plot_ly(plabs, x = ~datec, y = ~PatientIdentifier, z = ~qsec) %>%
# add_markers() %>%
# layout(scene = list(xaxis = list(title = 'Weight'),
#                     yaxis = list(title = 'Gross horsepower'),
#                     zaxis = list(title = '1/4 mile time')))
