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


#### TEST FOR ALL SCHIZOPHRENIA PATIENTS ###
#sczo_all <- unique(diagnoses3$PatientIdentifier[diagnoses3$ICDDiagnosisCode%in%sczo])
#test<-diagnoses3[diagnoses3$PatientIdentifier%in%sczo_all,]
# Read in dataset subset
#set.seed(1)
#test<-diagnoses3[diagnoses3$PatientIdentifier%in%sample(unique(diagnoses3$PatientIdentifier),1000),]
test<-diagnoses3
#set.seed(2)
#test_samp2 <- diagnoses3[diagnoses3$PatientIdentifier%in%sample(unique(diagnoses3$PatientIdentifier)[!unique(diagnoses3$PatientIdentifier)%in%test$PatientIdentifier],1000),]
#test <- test_samp2
# Create group ID  
test$Group<-datediff(test$EncounterDate,"2014-07-01","2 years")


# Code for more potential missing values in categorical groups
test$ICDDiagnosisCode <- gsub('^$','NO ICD',test$ICDDiagnosisCode)
test$ICDDiagnosisCategoryDescription <- gsub('^$','NO DX CATEGORY',test$ICDDiagnosisCategoryDescription)
test$ICDDiagnosisSubcategoryDescription <- gsub('^$','NO DX SUBCATEGORY',test$ICDDiagnosisSubcategoryDescription)
test$ICDDiagnosisCategoryDescription <- paste0(test$ICDDiagnosisCategoryDescription,'_DXCAT')
test$ICDDiagnosisSubcategoryDescription <- paste0(test$ICDDiagnosisSubcategoryDescription,'_DXSUBCAT')
test$Location <- paste0(test$Location,'_LOC')

test_f <- test %>% select(PatientIdentifier, Group, EncounterIdentifier, EncounterAge,
                        ICDDiagnosisSubcategoryDescription, PatientGender, PatientRace,
                        EverReportedAlcoholUse, EverReportedIllicitDrugUse,
                        EverReportedTobaccoUse,MostRecentlyReportedIllicitDrugUse,
                        MostRecentlyReportedAlcoholUse,MostRecentlyReportedTobaccoUse,
                        MostRecentlyReportedBirthControl,CurrentlySexuallyActive)

test2<-test_f %>%
  
  # Encounter Count
  group_by(PatientIdentifier, Group) %>% 
  mutate(TotalEncounters = length(unique(EncounterIdentifier))) %>%
  ungroup() %>%
  
  
  group_by(PatientIdentifier, Group) %>% 
  mutate(AVERAGE_AGE = mean(EncounterAge)) %>%
  #mutate(AVERAGE_LOS = mean(LOShours)) %>%
  dplyr::select(-EncounterAge,-EncounterIdentifier) %>% #, -LOShours) %>%
  
  # ICD Diagnosis Subcategory
  group_by(PatientIdentifier, Group, ICDDiagnosisSubcategoryDescription) %>%
  mutate(DSC_TEMP_COUNT = 1, DSC_COUNT=sum(DSC_TEMP_COUNT)) %>%
  dplyr::select(-DSC_TEMP_COUNT) %>%
  filter(row_number()==1) %>% 
  spread(key = ICDDiagnosisSubcategoryDescription, value = DSC_COUNT, fill = 0) %>%
  ungroup() %>%
  
  
  group_by(PatientIdentifier, Group) %>% 
  summarise_each(funs(max))


test2$PatientGender<-paste0(test2$PatientGender,"_GENDER")
test2$PatientRace<-paste0(test2$PatientRace,"_PR")
test2$EverReportedAlcoholUse<-paste0(test2$EverReportedAlcoholUse,"_EA")
test2$EverReportedIllicitDrugUse<-paste0(test2$EverReportedIllicitDrugUse,"_ED")
test2$EverReportedTobaccoUse<-paste0(test2$EverReportedTobaccoUse,"_ET")
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

