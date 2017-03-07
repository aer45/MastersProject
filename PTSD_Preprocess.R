# Pull in other Dx for comparison

# PTSD

# Hospital
h.patient <- read.csv("C:/Data/Other Dx/Hospital/PTSD/patient.csv",stringsAsFactors=F)
h.encounter <- read.csv("C:/Data/Other Dx/Hospital/PTSD/encounter.csv",stringsAsFactors=F)
h.diagnoses <- read.csv("C:/Data/Other Dx/Hospital/PTSD/patientdiagnoses.csv",stringsAsFactors = F)
h.medications <- read.csv("C:/Data/Other Dx/Hospital/PTSD/patientmedications.csv",stringsAsFactors = F)
h.social <- read.csv("C:/Data/Other Dx/Hospital/PTSD/socialhistory.csv",stringsAsFactors = F)
h.patp <- read.csv("C:/Data/Other Dx/Hospital/PTSD/patientgeography_demographics_acs5yrestimates_20062011estimates_povertystatistics.csv",stringsAsFactors = F)
names(h.patient) <- gsub("\\.","",names(h.patient))
names(h.encounter) <- gsub("\\.","",names(h.encounter))
names(h.diagnoses) <- gsub("\\.","",names(h.diagnoses))
names(h.medications) <- gsub("\\.","",names(h.medications))
names(h.social) <- gsub("\\.","",names(h.social))
names(h.patp) <- gsub("\\.","",names(h.patp))

# Clinic
c.patient <- read.csv("C:/Data/Other Dx/Clinic/PTSD/patient.csv",stringsAsFactors=F)
c.encounter <- read.csv("C:/Data/Other Dx/Clinic/PTSD/encounter.csv",stringsAsFactors=F)
c.diagnoses <- read.csv("C:/Data/Other Dx/Clinic/PTSD/patientdiagnoses.csv",stringsAsFactors = F)
c.medications <- read.csv("C:/Data/Other Dx/Clinic/PTSD/patientmedications.csv",stringsAsFactors = F)
c.social <- read.csv("C:/Data/Other Dx/Clinic/PTSD/socialhistory.csv",stringsAsFactors = F)
c.patp <- read.csv("C:/Data/Other Dx/Clinic/PTSD/patientgeography_demographics_acs5yrestimates_20062011estimates_povertystatistics.csv",stringsAsFactors = F)
names(c.patient) <- gsub("\\.","",names(c.patient))
names(c.encounter) <- gsub("\\.","",names(c.encounter))
names(c.diagnoses) <- gsub("\\.","",names(c.diagnoses))
names(c.medications) <- gsub("\\.","",names(c.medications))
names(c.social) <- gsub("\\.","",names(c.social))
names(c.patp) <- gsub("\\.","",names(c.patp))

# Put hospital and clinic datasets together
patient <- unique(rbind(h.patient, c.patient))
encounter <- bind_rows(h.encounter, c.encounter)
diagnoses <- bind_rows(h.diagnoses, c.diagnoses)
medications <- rbind(h.medications, c.medications)
social <- unique(rbind(h.social, c.social))
patp <- unique(rbind(h.patp, c.patp))

# Subset inpatient and outpatient visits to gather correct phenotypes
pheno_outpatient<-sqldf('select PatientIdentifier, encounteridentifier
                        from encounter
                        where (inpatientoutpatientbillingcode=="O" or inpatientoutpatientbillingcode=="NA") 
                        and admitdate<"7/1/2015 12:00:00 AM" or arrivaldate<"7/1/2015 12:00:00 AM"')

# Remove unnecessary datasets
rm(h.patient, h.encounter, h.diagnoses, h.medications, h.social, h.patp,
   c.patient, c.encounter, c.diagnoses, c.medications, c.social, c.patp)


# Clean up diagnosis dataset
diagnoses$EncounterDate <- as.Date(ifelse(is.na(diagnoses$ArrivalDate),diagnoses$AdmitDate,diagnoses$ArrivalDate), 
                                   format="%m/%d/%Y")
#diagnoses$DiagnosisDate <- as.Date(diagnoses$DiagnosisDate, format="%m/%d/%Y")
diagnoses$LOShours <- ifelse(is.na(diagnoses$TotalHospitalLOSindays), diagnoses$LengthofStayLOSinhours,
                             24*diagnoses$TotalHospitalLOSindays)
diagnoses$EncounterAge <- ifelse(is.na(diagnoses$AgeontheDayoftheVisitinyears),diagnoses$PatientAgeatArrivalinyears,
                                 diagnoses$AgeontheDayoftheVisitinyears)
#diagnoses <- subset(diagnoses, , -c(TotalHospitalLOSindays, LengthofStayLOSinhours,
#                                    AgeontheDayoftheVisitinyears,PatientAgeatArrivalinyears,
#                                    DiagnosisName,VisitTypeCategory,ArrivalDateTime,VisitType))
diagnoses$Encounter <- ifelse(is.na(diagnoses$EncounterType),
                              ifelse(diagnoses$EDIndicator=='Y','ED','Hospital Encounter'),
                              diagnoses$EncounterType)
#diagnoses <- subset(diagnoses,,-c(EncounterType, EDIndicator,VisitReason))
diagnoses$Location <- ifelse(is.na(diagnoses$ClinicLocationName),diagnoses$Hospital,
                             diagnoses$ClinicLocationName)
#diagnoses <- subset(diagnoses,,-c(Hospital, ClinicLocationName, PatientYearofBirth))
diagnoses <- diagnoses %>%
  dplyr::select(-TotalHospitalLOSindays, -LengthofStayLOSinhours,
         -AgeontheDayoftheVisitinyears, -PatientAgeatArrivalinyears,
         -DiagnosisName, -VisitTypeCategory, -VisitType,
         -EncounterType, -EDIndicator, -VisitReason, -Hospital, -ClinicLocationName, 
         -PatientYearofBirth, -DiagnosisDate, -AdmitDate, -ArrivalDate,
         -LOShours, -InpatientOutpatientBillingCode, -ArrivalDateTime, -DepartureDateTime)

diagnoses$PatientRace[diagnoses$PatientRace=='CAUCASIAN/WHITE']<-'WHITE OR CAUCASIAN'
diagnoses$PatientRace[diagnoses$PatientRace=='AMERICAN INDIAN']<-'AMERICAN INDIAN OR ALASKAN NATIVE'
diagnoses$PatientRace[diagnoses$PatientRace=='2 OR MORE RACES']<-'MULTIRACIAL'
diagnoses$PatientRace[diagnoses$PatientRace=='NOT REPORTED/DECLINED']<-'UNAVAILABLE'
diagnoses$PatientRace[diagnoses$PatientRace=='']<-'UNAVAILABLE'
diagnoses$PatientRace[diagnoses$PatientRace=='UNAVAILABLE']<-'UNKNOWN'

diagnoses$PatientEthnicGroup[diagnoses$PatientEthnicGroup=='']<-'NOT REPORTED/DECLINED'
diagnoses$PatientEthnicGroup[diagnoses$PatientEthnicGroup=='DECLINED']<-'NOT REPORTED/DECLINED'
diagnoses$PatientEthnicGroup[diagnoses$PatientEthnicGroup=='UNAVAILABLE']<-'NOT REPORTED/DECLINED'

diagnoses$Location[diagnoses$Location=='DUKE UNIVERSITY HOSPITAL']<-'DUH'
diagnoses$Location[diagnoses$Location=='DUKE REGIONAL HOSPITAL']<-'DRH'
diagnoses$Location[diagnoses$Location=='DUKE HEALTH AT SOUTHPOINT PROFESSIONAL CENTER']<-'DUKE HEALTH CENTER AT SOUTHPOINT'
diagnoses$Location[diagnoses$Location=='DUKE HEALTH CENTER OF CLAYTON']<-'DUKE HEALTH CLAYTON'

patp$PovertyPercent<-(patp$C17002_002_PCT+patp$C17002_003_PCT)/100
patp<-patp[,c('PatientIdentifier','PovertyPercent')]
social <- social[,c(1,6:13)]

# Join datasets and filter to get final analysis set

diagnoses2<-join(diagnoses,social,by='PatientIdentifier',type='left')
diagnoses3<-join(diagnoses2,patp,by='PatientIdentifier',type='left')
rm(diagnoses, diagnoses2, patp, social, patient, encounter)

diagnoses.c.ptsd<-diagnoses3[diagnoses3$EncounterDate<=as.Date("2015-06-30"),]
diagnoses.f.ptsd<-diagnoses3[diagnoses3$EncounterDate>as.Date("2015-06-30"),]
rm(diagnoses3)


# Read in dataset subset
test.ptsd<-diagnoses.c.ptsd[diagnoses.c.ptsd$PatientIdentifier%in%sample(unique(diagnoses.c.ptsd$PatientIdentifier),100),]

# Create group ID  
test.ptsd$Group<-datediff(test.ptsd$EncounterDate,"2014-07-01","1 year")

# Order data by Pat ID, Group ID, and Encounter Date
test.ptsd<-test.ptsd[order(test.ptsd$PatientIdentifier,test.ptsd$Group,test.ptsd$EncounterDate),]

# Code for missing values in categorical values in groups and attach tag for no duplicates
test.ptsd$DiagnosisGroup <- gsub('^$','NO DX GROUP', test.ptsd$DiagnosisGroup)
test.ptsd$DiagnosisGroup <- paste0(test.ptsd$DiagnosisGroup,'_GROUP')
test.ptsd$DiagnosisGroup <- NULL

# Code for more potential missing values in categorical groups
test.ptsd$ICDDiagnosisCode <- gsub('^$','NO ICD',test.ptsd$ICDDiagnosisCode)
test.ptsd$ICDDiagnosisCategoryDescription <- gsub('^$','NO DX CATEGORY',test.ptsd$ICDDiagnosisCategoryDescription)
test.ptsd$ICDDiagnosisSubcategoryDescription <- gsub('^$','NO DX SUBCATEGORY',test.ptsd$ICDDiagnosisSubcategoryDescription)
test.ptsd$ICDDiagnosisCategoryDescription <- paste0(test.ptsd$ICDDiagnosisCategoryDescription,'_DXCAT')
test.ptsd$ICDDiagnosisSubcategoryDescription <- paste0(test.ptsd$ICDDiagnosisSubcategoryDescription,'_DXSUBCAT')


test.ptsd2<-test.ptsd %>%
  
  # Average Days Between Encounters
  # group_by(PatientIdentifier, Group) %>% 
  # mutate(avgdaycount = mean(as.numeric(difftime(unique(EncounterDate),
  #           lag(unique(EncounterDate),1))),na.rm=T),AverageDaysBetweenEncounters=ifelse(is.nan(avgdaycount),NA,
  #           avgdaycount)) %>% 
  # select(-avgdaycount) %>% 
  # ungroup() %>%
  
  # Encounter Count
  group_by(PatientIdentifier, Group) %>% 
  mutate(TotalEncounters = length(unique(EncounterIdentifier))) %>%
  ungroup() %>%
  
  # Average Days Between Encounters
  group_by(PatientIdentifier, Group) %>% 
  mutate(AverageDaysBetweenEncounters = 365/TotalEncounters) %>%
  ungroup() %>%
  
  group_by(PatientIdentifier, Group) %>% 
  mutate(AVERAGE_AGE = mean(EncounterAge)) %>%
  #mutate(AVERAGE_LOS = mean(LOShours)) %>%
  dplyr::select(-EncounterAge) %>% #, -LOShours) %>%
  
  # ICD Codes
  group_by(PatientIdentifier, Group, ICDDiagnosisCode) %>% 
  mutate(ICD_TEMP_COUNT = 1, ICD_COUNT = sum(ICD_TEMP_COUNT)) %>%
  dplyr::select(-ICD_TEMP_COUNT) %>%
  filter(row_number()==1) %>%
  spread(key = ICDDiagnosisCode, value = ICD_COUNT, fill = 0) %>% 
  ungroup() %>%
  
  # Diagnosis Group
  # group_by(PatientIdentifier, Group, DiagnosisGroup) %>% 
  # mutate(DG_TEMP_COUNT = 1, DG_COUNT = sum(DG_TEMP_COUNT)) %>%
  # select(-DG_TEMP_COUNT) %>% 
  # spread(key = DiagnosisGroup, value = DG_COUNT, fill = 0) %>% 
  # ungroup() %>%
  
  # ICD Diagnosis Category
  group_by(PatientIdentifier, Group, ICDDiagnosisCategoryDescription) %>% 
  mutate(DC_TEMP_COUNT = 1, DC_COUNT = sum(DC_TEMP_COUNT)) %>%
  dplyr::select(-DC_TEMP_COUNT) %>% 
  spread(key = ICDDiagnosisCategoryDescription, value = DC_COUNT, fill = 0) %>% 
  ungroup() %>%
  
  # ICD Diagnosis Subcategory
  group_by(PatientIdentifier, Group, ICDDiagnosisSubcategoryDescription) %>%
  mutate(DSC_TEMP_COUNT = 1, DSC_COUNT=sum(DSC_TEMP_COUNT)) %>%
  dplyr::select(-DSC_TEMP_COUNT) %>%
  spread(key = ICDDiagnosisSubcategoryDescription, value = DSC_COUNT, fill = 0) %>%
  ungroup() %>%
  
  # group_by(PatientIdentifier, Group) %>% 
  # mutate(AVERAGE_AGE = mean(EncounterAge)) %>%
  # #mutate(AVERAGE_LOS = mean(LOShours)) %>%
  # dplyr::select(-EncounterAge) %>% #, -LOShours) %>%
  
  # Location
  group_by(PatientIdentifier, EncounterIdentifier, Group, Location) %>% 
  mutate(LOC_TEMP_COUNT = length(unique(EncounterIdentifier)), LOC_COUNT = sum(LOC_TEMP_COUNT)) %>%
  dplyr::select(-LOC_TEMP_COUNT) %>% 
  spread(key = Location, value = LOC_COUNT, fill = 0) %>% 
  ungroup() %>%
  
  # Encounter Type
  group_by(PatientIdentifier, EncounterIdentifier, Group, Encounter) %>% 
  mutate(ENC_TEMP_COUNT = 1, ENC_COUNT = sum(ENC_TEMP_COUNT)) %>%
  dplyr::select(-ENC_TEMP_COUNT) %>% 
  spread(key = Encounter, value = ENC_COUNT, fill = 0) %>% 
  ungroup() %>%
  
  group_by(PatientIdentifier, Group) %>% 
  summarise_each(funs(max)) %>% 
  dplyr::select(-EncounterIdentifier, -EncounterDate)


test.ptsd2$PatientGender<-paste0(test.ptsd2$PatientGender,"_GENDER")
test.ptsd2$PatientRace<-paste0(test.ptsd2$PatientRace,"_PR")
test.ptsd2$PatientEthnicGroup<-paste0(test.ptsd2$PatientEthnicGroup,"_PE")
test.ptsd2$EverReportedAlcoholUse<-paste0(test.ptsd2$EverReportedAlcoholUse,"_EA")
test.ptsd2$EverReportedIllicitDrugUse<-paste0(test.ptsd2$EverReportedIllicitDrugUse,"_ED")
test.ptsd2$EverReportedTobaccoUse<-paste0(test.ptsd2$EverReportedAlcoholUse,"_ET")
test.ptsd2$MostRecentlyReportedIllicitDrugUse<-paste0(test.ptsd2$MostRecentlyReportedIllicitDrugUse,"_RD")
test.ptsd2$MostRecentlyReportedAlcoholUse<-paste0(test.ptsd2$MostRecentlyReportedAlcoholUse,"_RA")
test.ptsd2$MostRecentlyReportedTobaccoUse<-paste0(test.ptsd2$MostRecentlyReportedTobaccoUse,"_RT")
test.ptsd2$MostRecentlyReportedBirthControl<-paste0(test.ptsd2$MostRecentlyReportedBirthControl,"_RBC")
test.ptsd2$CurrentlySexuallyActive<-paste0(test.ptsd2$CurrentlySexuallyActive,"_CSA")

# Make demographic info numeric
test.ptsd3 <- test.ptsd2 %>% 
  group_by(PatientIdentifier) %>% 
  mutate(A_COUNT = 1) %>%
  spread(key = PatientGender, value = A_COUNT, fill = 0) %>%
  mutate(B_COUNT = 1) %>% 
  spread(key= PatientRace, value = B_COUNT, fill = 0) %>% 
  mutate(C_COUNT = 1) %>% 
  spread(key= PatientEthnicGroup, value = C_COUNT, fill = 0) %>% 
  mutate(D_COUNT = 1) %>% 
  spread(key= EverReportedAlcoholUse, value = D_COUNT, fill = 0) %>% 
  mutate(E_COUNT = 1) %>% 
  spread(key= EverReportedIllicitDrugUse, value = E_COUNT, fill = 0) %>% 
  mutate(F_COUNT = 1) %>% 
  spread(key= EverReportedTobaccoUse, value = F_COUNT, fill = 0) %>% 
  mutate(G_COUNT = 1) %>% 
  spread(key= MostRecentlyReportedIllicitDrugUse, value = G_COUNT, fill = 0) %>% 
  mutate(H_COUNT = 1) %>% 
  spread(key= MostRecentlyReportedAlcoholUse, value = H_COUNT, fill = 0) %>% 
  mutate(I_COUNT = 1) %>% 
  spread(key= MostRecentlyReportedTobaccoUse, value = I_COUNT, fill = 0) %>% 
  mutate(J_COUNT = 1) %>% 
  spread(key= MostRecentlyReportedBirthControl, value = J_COUNT, fill = 0) %>% 
  mutate(K_COUNT = 1) %>% 
  spread(key= CurrentlySexuallyActive, value = K_COUNT, fill = 0) %>% 
  ungroup()  

ptsd<-c("309.81","F43.10","F43.11","F43.12")

pheno_icd<-c(ptsd)

pheno_subset<-test.ptsd3[,names(test.ptsd3)%in%pheno_icd]
pheno_subset<-data.frame(test.ptsd3$PatientIdentifier,pheno_subset)

count<-rowsum(pheno_subset,pheno_subset$test.ptsd3.PatientIdentifier)
count2<-apply(count[,-1],1,sum)
drop<-count2<2
dropid<-count[drop==T,'test.ptsd3.PatientIdentifier']

test.ptsd3<-test.ptsd3[!(test.ptsd3$PatientIdentifier%in%dropid),]

rm(count, count2, drop, pheno_icd, pheno_outpatient, pheno_subset)


# Random Forest model inputation
test.ptsd3$PIG<-as.factor(paste0(test.ptsd3$PatientIdentifier,'_',test.ptsd3$Group))
set.seed(1)
misspp<-rfImpute(x = PIG~.,data = test.ptsd3[,-c(1:2)], iter=1, ntree=10, mtry=ceiling(dim(test3)[2]^(1/3)))

# Pull together dataframe and summarize Poverty Percent
test.ptsd3<-data.frame(test.ptsd3[,c(1:2)],misspp[,-1])

test.ptsd3<-test.ptsd3[,colSums(test.ptsd3)!=0]

# Change ICD to binary
last.icd <- grep('\\_DXCAT$',names(test.ptsd3))[1]-1
first.gender <- grep('\\_GENDER$',names(test.ptsd3))[1]
test.ptsd3[,7:last.icd]<-ifelse(test.ptsd3[,7:last.icd]==0,-1,1)
test.ptsd3[,first.gender:length(test.ptsd3)]<-ifelse(test.ptsd3[,first.gender:length(test.ptsd3)]==0,-1,1)

# Join PTSD set with Bipolar set
ptsd.id<-test.ptsd3[,c(1:2)]
test.ptsd3<-test.ptsd3[,-c(1:2)]

set.seed(10)
ptsd.bipo.scz <- rbind.fill(test.ptsd3[sample(nrow(test.ptsd3),100),],test3[sample(nrow(test3),100),])
ptsd.bipo.scz[is.na(ptsd.bipo.scz)]<- -1
ptsd.bipo.scz<-ptsd.bipo.scz[,apply(ptsd.bipo.scz,2,sd)!=0]

pbs.scaled <- scale(ptsd.bipo.scz)

pbs.decomp<-svd(pbs.scaled)

# Plot singular value importance
svimp<-pbs.decomp$d^2/sum(pbs.decomp$d^2)
plot(svimp)
lines(svimp)
index<-which(cumsum(svimp)>0.95)[1]

redd.pbs<-data.frame(pbs.decomp$u[,1:index]%*%diag(pbs.decomp$d[1:index]))

# Cluster
set.seed(10)
fviz_nbclust(redd.pbs, pam, method='silhouette')

set.seed(11)
pam.fit.pbs <- pam(redd.pbs,diss=F,k=2)


Dx2 <- c(rep('PTSD',100),rep('Bipo/Scz',100))

# Plotting
plot_ly(x=pbs.decomp$u[,1]*pbs.decomp$d[1]%*%t(pbs.decomp$v)[1],y=pbs.decomp$u[,2]*pbs.decomp$d[2]%*%t(pbs.decomp$v)[2],
        z=pbs.decomp$u[,3]*pbs.decomp$d[3]%*%t(pbs.decomp$v)[3],color=pam.fit.pbs$clustering)
plot_ly(x=pbs.decomp$u[,1]*pbs.decomp$d[1]%*%t(pbs.decomp$v)[1],y=pbs.decomp$u[,2]*pbs.decomp$d[2]%*%t(pbs.decomp$v)[2],
        z=pbs.decomp$u[,3]*pbs.decomp$d[3]%*%t(pbs.decomp$v)[3],color=Dx2)
#fviz_cluster(pam.fit.ptsd)

rfdist.pbs <- randomForest(redd.pbs,importance=T,proximity=T)
dissrf.pbs<-sqrt(1-rfdist.pbs$proximity)

# Silhouettee Width
set.seed(10)
fviz_nbclust(redd.pbs, pam, diss=dissrf.pbs, method='silhouette')
