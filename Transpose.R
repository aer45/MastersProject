# Transpose original dataset to have counts of ICDs and Visits

# Functions
datediff <- function(datecol,startdate,interval){
  # Function inputs date column in df, start date of period, and interal (e.g. week, month, 6 months).
  # Outputs vector of group ID
  
  diff<-rep(0,length(datecol))
  for (i in 1:length(datecol)){
    diff[i]<-length(seq(as.Date(startdate),datecol[i],interval))
  }
  return(diff)
}




# Read in dataset subset
test<-diagnoses.c[diagnoses.c$PatientIdentifier%in%sample(unique(diagnoses.c$PatientIdentifier),500),]

# Create group ID  
test$Group<-datediff(test$EncounterDate,"2014-07-01","1 year")

# Order data by Pat ID, Group ID, and Encounter Date
test<-test[order(test$PatientIdentifier,test$Group,test$EncounterDate),]

# Code for missing values in categorical values in groups and attach tag for no duplicates
test$DiagnosisGroup <- gsub('^$','NO DX GROUP', test$DiagnosisGroup)
test$DiagnosisGroup <- paste0(test$DiagnosisGroup,'_GROUP')
test$DiagnosisGroup <- NULL

# Code for more potential missing values in categorical groups
test$ICDDiagnosisCode <- gsub('^$','NO ICD',test$ICDDiagnosisCode)
test$ICDDiagnosisCategoryDescription <- gsub('^$','NO DX CATEGORY',test$ICDDiagnosisCategoryDescription)
test$ICDDiagnosisSubcategoryDescription <- gsub('^$','NO DX SUBCATEGORY',test$ICDDiagnosisSubcategoryDescription)
test$ICDDiagnosisCategoryDescription <- paste0(test$ICDDiagnosisCategoryDescription,'_DXCAT')
test$ICDDiagnosisSubcategoryDescription <- paste0(test$ICDDiagnosisSubcategoryDescription,'_DXSUBCAT')


test2<-test %>%
  
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
  
  
test2$PatientGender<-paste0(test2$PatientGender,"_GENDER")
test2$PatientRace<-paste0(test2$PatientRace,"_PR")
test2$PatientEthnicGroup<-paste0(test2$PatientEthnicGroup,"_PE")
test2$EverReportedAlcoholUse<-paste0(test2$EverReportedAlcoholUse,"_EA")
test2$EverReportedIllicitDrugUse<-paste0(test2$EverReportedIllicitDrugUse,"_ED")
test2$EverReportedTobaccoUse<-paste0(test2$EverReportedAlcoholUse,"_ET")
test2$MostRecentlyReportedIllicitDrugUse<-paste0(test2$MostRecentlyReportedIllicitDrugUse,"_RD")
test2$MostRecentlyReportedAlcoholUse<-paste0(test2$MostRecentlyReportedAlcoholUse,"_RA")
test2$MostRecentlyReportedTobaccoUse<-paste0(test2$MostRecentlyReportedTobaccoUse,"_RT")
test2$MostRecentlyReportedBirthControl<-paste0(test2$MostRecentlyReportedBirthControl,"_RBC")
test2$CurrentlySexuallyActive<-paste0(test2$CurrentlySexuallyActive,"_CSA")

# Make demographic info numeric
test3 <- test2 %>% 
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
  