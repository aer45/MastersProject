# Pull in other Dx for comparison

# LC

# Hospital
h.patient <- read.csv("C:/Data/Other Dx/Hospital/LC/patient.csv",stringsAsFactors=F)
h.encounter <- read.csv("C:/Data/Other Dx/Hospital/LC/encounter.csv",stringsAsFactors=F)
h.diagnoses <- read.csv("C:/Data/Other Dx/Hospital/LC/patientdiagnoses.csv",stringsAsFactors = F)
h.medications <- read.csv("C:/Data/Other Dx/Hospital/LC/patientmedications.csv",stringsAsFactors = F)
h.social <- read.csv("C:/Data/Other Dx/Hospital/LC/socialhistory.csv",stringsAsFactors = F)
h.patp <- read.csv("C:/Data/Other Dx/Hospital/LC/patientgeography_demographics_acs5yrestimates_20062011estimates_povertystatistics.csv",stringsAsFactors = F)
names(h.patient) <- gsub("\\.","",names(h.patient))
names(h.encounter) <- gsub("\\.","",names(h.encounter))
names(h.diagnoses) <- gsub("\\.","",names(h.diagnoses))
names(h.medications) <- gsub("\\.","",names(h.medications))
names(h.social) <- gsub("\\.","",names(h.social))
names(h.patp) <- gsub("\\.","",names(h.patp))

# Clinic
c.patient <- read.csv("C:/Data/Other Dx/Clinic/LC/patient.csv",stringsAsFactors=F)
c.encounter <- read.csv("C:/Data/Other Dx/Clinic/LC/encounter.csv",stringsAsFactors=F)
c.diagnoses <- read.csv("C:/Data/Other Dx/Clinic/LC/patientdiagnoses.csv",stringsAsFactors = F)
c.medications <- read.csv("C:/Data/Other Dx/Clinic/LC/patientmedications.csv",stringsAsFactors = F)
c.social <- read.csv("C:/Data/Other Dx/Clinic/LC/socialhistory.csv",stringsAsFactors = F)
c.patp <- read.csv("C:/Data/Other Dx/Clinic/LC/patientgeography_demographics_acs5yrestimates_20062011estimates_povertystatistics.csv",stringsAsFactors = F)
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

diagnoses.c.LC<-diagnoses3[diagnoses3$EncounterDate<=as.Date("2015-06-30"),]
diagnoses.f.LC<-diagnoses3[diagnoses3$EncounterDate>as.Date("2015-06-30"),]
rm(diagnoses3)


# Read in dataset subset
test.LC<-diagnoses.c.LC[diagnoses.c.LC$PatientIdentifier%in%sample(unique(diagnoses.c.LC$PatientIdentifier),300),]

# Create group ID  
test.LC$Group<-datediff(test.LC$EncounterDate,"2014-07-01","1 year")

# Order data by Pat ID, Group ID, and Encounter Date
test.LC<-test.LC[order(test.LC$PatientIdentifier,test.LC$Group,test.LC$EncounterDate),]

# Code for missing values in categorical values in groups and attach tag for no duplicates
test.LC$DiagnosisGroup <- gsub('^$','NO DX GROUP', test.LC$DiagnosisGroup)
test.LC$DiagnosisGroup <- paste0(test.LC$DiagnosisGroup,'_GROUP')
test.LC$DiagnosisGroup <- NULL

# Code for more potential missing values in categorical groups
test.LC$ICDDiagnosisCode <- gsub('^$','NO ICD',test.LC$ICDDiagnosisCode)
test.LC$ICDDiagnosisCategoryDescription <- gsub('^$','NO DX CATEGORY',test.LC$ICDDiagnosisCategoryDescription)
test.LC$ICDDiagnosisSubcategoryDescription <- gsub('^$','NO DX SUBCATEGORY',test.LC$ICDDiagnosisSubcategoryDescription)
test.LC$ICDDiagnosisCategoryDescription <- paste0(test.LC$ICDDiagnosisCategoryDescription,'_DXCAT')
test.LC$ICDDiagnosisSubcategoryDescription <- paste0(test.LC$ICDDiagnosisSubcategoryDescription,'_DXSUBCAT')


test.LC2<-test.LC %>%
  
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


test.LC2$PatientGender<-paste0(test.LC2$PatientGender,"_GENDER")
test.LC2$PatientRace<-paste0(test.LC2$PatientRace,"_PR")
test.LC2$PatientEthnicGroup<-paste0(test.LC2$PatientEthnicGroup,"_PE")
test.LC2$EverReportedAlcoholUse<-paste0(test.LC2$EverReportedAlcoholUse,"_EA")
test.LC2$EverReportedIllicitDrugUse<-paste0(test.LC2$EverReportedIllicitDrugUse,"_ED")
test.LC2$EverReportedTobaccoUse<-paste0(test.LC2$EverReportedAlcoholUse,"_ET")
test.LC2$MostRecentlyReportedIllicitDrugUse<-paste0(test.LC2$MostRecentlyReportedIllicitDrugUse,"_RD")
test.LC2$MostRecentlyReportedAlcoholUse<-paste0(test.LC2$MostRecentlyReportedAlcoholUse,"_RA")
test.LC2$MostRecentlyReportedTobaccoUse<-paste0(test.LC2$MostRecentlyReportedTobaccoUse,"_RT")
test.LC2$MostRecentlyReportedBirthControl<-paste0(test.LC2$MostRecentlyReportedBirthControl,"_RBC")
test.LC2$CurrentlySexuallyActive<-paste0(test.LC2$CurrentlySexuallyActive,"_CSA")

# Make demographic info numeric
test.LC3 <- test.LC2 %>% 
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

LC<-c("162.2","162.3","162.4","162.5","162.8","162.9","231.2","V10.11","C34.00","C34.01",
      "C34.02","C34.10","C34.11","C34.12","C34.2","C34.30","C34.31","C34.32","C34.80","C34.81",
      "C34.82","C34.90","C34.91","C34.92","D02.20","D02.21","D02.22","Z85.118")

pheno_icd<-c(LC)

pheno_subset<-test.LC3[,names(test.LC3)%in%pheno_icd]
pheno_subset<-data.frame(test.LC3$PatientIdentifier,pheno_subset)

count<-rowsum(pheno_subset,pheno_subset$test.LC3.PatientIdentifier)
count2<-apply(count[,-1],1,sum)
drop<-count2<2
dropid<-count[drop==T,'test.LC3.PatientIdentifier']

test.LC3<-test.LC3[!(test.LC3$PatientIdentifier%in%dropid),]

rm(count, count2, drop, pheno_icd, pheno_outpatient, pheno_subset)


# Random Forest model inputation
test.LC3$PIG<-as.factor(paste0(test.LC3$PatientIdentifier,'_',test.LC3$Group))
set.seed(1)
misspp<-rfImpute(x = PIG~.,data = test.LC3[,-c(1:2)], iter=1, ntree=10, mtry=ceiling(dim(test3)[2]^(1/3)))

# Pull together dataframe and summarize Poverty Percent
test.LC3<-data.frame(test.LC3[,c(1:2)],misspp[,-1])

test.LC3<-test.LC3[,colSums(test.LC3)!=0]

# Change ICD to binary
last.icd <- grep('\\_DXCAT$',names(test.LC3))[1]-1
first.gender <- grep('\\_GENDER$',names(test.LC3))[1]
test.LC3[,7:last.icd]<-ifelse(test.LC3[,7:last.icd]==0,-1,1)
test.LC3[,first.gender:length(test.LC3)]<-ifelse(test.LC3[,first.gender:length(test.LC3)]==0,-1,1)

# Join LC set with Bipolar set
LC.id<-test.LC3[,c(1:2)]
test.LC3<-test.LC3[,-c(1:2)]

set.seed(10)
LC.bipo.scz <- rbind.fill(test.LC3[sample(nrow(test.LC3),100),],test3[sample(nrow(test3),100),])
LC.bipo.scz[is.na(LC.bipo.scz)]<- -1
LC.bipo.scz<-LC.bipo.scz[,apply(LC.bipo.scz,2,sd)!=0]

lcbs.scaled <- scale(LC.bipo.scz)

lcbs.decomp<-svd(lcbs.scaled)

# Plot singular value importance
svimp<-lcbs.decomp$d^2/sum(lcbs.decomp$d^2)
plot(svimp)
lines(svimp)
index<-which(cumsum(svimp)>0.95)[1]

redd.lcbs<-data.frame(lcbs.decomp$u[,1:index]%*%diag(lcbs.decomp$d[1:index]))

# Cluster
set.seed(10)
fviz_nbclust(redd.lcbs, pam, method='silhouette')

set.seed(11)
pam.fit.lcbs <- pam(redd.lcbs,diss=F,k=2)


Dx2 <- c(rep('LC',100),rep('Bipo/Scz',100))

# Plotting
plot_ly(x=lcbs.decomp$u[,1]*lcbs.decomp$d[1]%*%t(lcbs.decomp$v)[1],y=lcbs.decomp$u[,2]*lcbs.decomp$d[2]%*%t(lcbs.decomp$v)[2],
        z=lcbs.decomp$u[,3]*lcbs.decomp$d[3]%*%t(lcbs.decomp$v)[3],color=pam.fit.lcbs$clustering)
plot_ly(x=lcbs.decomp$u[,1]*lcbs.decomp$d[1]%*%t(lcbs.decomp$v)[1],y=lcbs.decomp$u[,2]*lcbs.decomp$d[2]%*%t(lcbs.decomp$v)[2],
        z=lcbs.decomp$u[,3]*lcbs.decomp$d[3]%*%t(lcbs.decomp$v)[3],color=Dx2)
#fviz_cluster(pam.fit.LC)

rfdist.lcbs <- randomForest(redd.lcbs,importance=T,proximity=T)
dissrf.lcbs<-sqrt(1-rfdist.lcbs$proximity)

# Silhouettee Width
set.seed(10)
fviz_nbclust(redd.lcbs, pam, diss=dissrf.lcbs, method='silhouette')
