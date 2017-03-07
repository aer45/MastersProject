# Function to cut dataset by time period
# Allen Ross
# 2/12/17

# Libraries
library(sqldf)
library(plyr)
library(dplyr)
library(reshape2)
library(tidyr)

# Read datasets in
# Hospital
h.patient <- read.csv("C:/Data/Hospital/patient.csv",stringsAsFactors=F)
h.encounter <- read.csv("C:/Data/Hospital/encounter.csv",stringsAsFactors=F)
h.diagnoses <- read.csv("C:/Data/Hospital/patientdiagnoses.csv",stringsAsFactors = F)
h.medications <- read.csv("C:/Data/Hospital/patientmedications.csv",stringsAsFactors = F)
h.social <- read.csv("C:/Data/Hospital/socialhistory.csv",stringsAsFactors = F)
h.patp <- read.csv("C:/Data/Hospital/patientgeography_demographics_acs5yrestimates_20062011estimates_povertystatistics.csv",stringsAsFactors = F)
names(h.patient) <- gsub("\\.","",names(h.patient))
names(h.encounter) <- gsub("\\.","",names(h.encounter))
names(h.diagnoses) <- gsub("\\.","",names(h.diagnoses))
names(h.medications) <- gsub("\\.","",names(h.medications))
names(h.social) <- gsub("\\.","",names(h.social))
names(h.patp) <- gsub("\\.","",names(h.patp))

# Clinic
c.patient <- read.csv("C:/Data/Clinic/patient.csv",stringsAsFactors=F)
c.encounter <- read.csv("C:/Data/Clinic/encounter.csv",stringsAsFactors=F)
c.diagnoses <- read.csv("C:/Data/Clinic/patientdiagnoses.csv",stringsAsFactors = F)
c.medications <- read.csv("C:/Data/Clinic/patientmedications.csv",stringsAsFactors = F)
c.social <- read.csv("C:/Data/Clinic/socialhistory.csv",stringsAsFactors = F)
c.patp <- read.csv("C:/Data/Clinic/patientgeography_demographics_acs5yrestimates_20062011estimates_povertystatistics.csv",stringsAsFactors = F)
names(c.patient) <- gsub("\\.","",names(c.patient))
names(c.encounter) <- gsub("\\.","",names(c.encounter))
names(c.diagnoses) <- gsub("\\.","",names(c.diagnoses))
names(c.medications) <- gsub("\\.","",names(c.medications))
names(c.social) <- gsub("\\.","",names(c.social))
names(c.patp) <- gsub("\\.","",names(c.patp))

# # Fix LOS in clinic data
# c.encounter$ArrivalDateTime<-mdy_hms(c.encounter$ArrivalDateTime)
# c.encounter$DepartureDateTime<-mdy_hms(c.encounter$DepartureDateTime)
# 
# # AM/PM difference
# c.encounter$LengthofStayLOSinhours<-ifelse(c.encounter$LengthofStayLOSinhours<0,c.encounter$DepartureDateTime+hours(12)-c.encounter$ArrivalDateTime,c.encounter$LengthofStayLOSinhours)
# # Past midnight difference
# c.encounter$LengthofStayLOSinhours<-ifelse(c.encounter$LengthofStayLOSinhours<0,c.encounter$DepartureDateTime+days(1)-c.encounter$ArrivalDateTime,c.encounter$LengthofStayLOSinhours)
# # Other outliers
# c.encounter$LengthofStayLOSinhours<-ifelse(c.encounter$LengthofStayLOSinhours>20000,c.encounter$DepartureDateTime+days(1)-c.encounter$ArrivalDateTime,c.encounter$LengthofStayLOSinhours)


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
              select(-TotalHospitalLOSindays, -LengthofStayLOSinhours,
                     -AgeontheDayoftheVisitinyears, -PatientAgeatArrivalinyears,
                     -DiagnosisName, -VisitTypeCategory, -VisitType,
                     -EncounterType, -EDIndicator, -VisitReason, -Hospital, -ClinicLocationName, 
                     -PatientYearofBirth, -DiagnosisDate, -AdmitDate, -ArrivalDate,
                     -LOShours, -InpatientOutpatientBillingCode)

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

diagnoses.c<-diagnoses3[diagnoses3$EncounterDate<=as.Date("2015-06-30"),]
diagnoses.f<-diagnoses3[diagnoses3$EncounterDate>as.Date("2015-06-30"),]
rm(diagnoses3)




