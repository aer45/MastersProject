# Subset patients by phenotype and identify diagnostic phenotype

# Libraries



# Subset sample based on phenotype definitions
sczo<-c("295.00","295.01","295.02","295.03","295.04","295.05","295.10","295.11","295.12",
        "295.13","295.14","295.15","295.20","295.21","295.22","295.23","295.24","295.25",
        "295.3","295.30","295.31","295.32","295.33","295.34","295.35","295.4","295.40",
        "295.41","295.42","295.43","295.44","295.45","295.50","295.51","295.52","295.53",
        "295.54","295.60","295.61","295.62","295.63","295.64","295.65","295.7","295.70",
        "295.71","295.72","295.73","295.74","295.75","295.80","295.81","295.82","295.83",
        "295.84","295.85","295.9","295.90","295.91","295.92","295.93","295.94","295.95",
        "296.0","F20.0","F20.1","F20.2","F20.3","F20.5","F20.81","F20.89","F20.9","F25.0",
        "F25.1","F25.8","F25.9")

bipo<-c("296.00","296.01","296.02","296.03","296.04","296.05","296.06","296.10",
        "296.11","296.12","296.13","296.14","296.15","296.16","296.4","296.40","296.41",
        "296.42","296.43","296.44","296.45","296.46","296.5","296.50","296.51","296.52",
        "296.53","296.54","296.55","296.56","296.60","296.61","296.62","296.63","296.64",
        "296.65","296.66","296.7","296.8","296.80","296.81","296.82","296.89","296.9",
        "296.90","296.99","F30.10","F30.11","F30.2","F30.4","F30.8","F30.9","F31.0",
        "F31.10","F31.11","F31.12","F31.13","F31.2","F31.30","F31.31","F31.32","F31.4",
        "F31.5","F31.60","F31.61","F31.62","F31.63","F31.64","F31.70","F31.71","F31.72",
        "F31.73","F31.74","F31.75","F31.76","F31.77","F31.78","F31.81","F31.89","F31.9",
        "F32.8","F33.8","F34.8","F34.9","F39")

lc<-c("162.2","162.3","162.4","162.5","162.8","162.9","231.2","V10.11","C34.00","C34.01",
      "C34.02","C34.10","C34.11","C34.12","C34.2","C34.30","C34.31","C34.32","C34.80","C34.81",
      "C34.82","C34.90","C34.91","C34.92","D02.20","D02.21","D02.22","Z85.118")

ptsd<-c("309.81","F43.10","F43.11","F43.12")

pheno_icd<-c(bipo,sczo)

pheno_subset<-test3[,names(test3)%in%pheno_icd]
pheno_subset<-data.frame(test3$PatientIdentifier,pheno_subset)

count<-rowsum(pheno_subset,pheno_subset$test3.PatientIdentifier)
count2<-apply(count[,-1],1,sum)
drop<-count2<2
dropid<-count[drop==T,'test3.PatientIdentifier']

test3<-test3[!(test3$PatientIdentifier%in%dropid),]

rm(count, count2, drop, pheno_icd, pheno_outpatient, pheno_subset)



# Tag patients based on phenotypic dx
# Scizophrenia
sczo_subset<-data.frame(test3$PatientIdentifier,test3[,names(test3)%in%sczo])
sczo_count<-rowsum(sczo_subset,sczo_subset$test3.PatientIdentifier)
sczo_count<-apply(sczo_count[,-1],1,sum)
schizophrenia<-sczo_count>0
# Bioplar
bipo_subset<-data.frame(test3$PatientIdentifier,test3[,names(test3)%in%bipo])
bipo_count<-rowsum(bipo_subset,bipo_subset$test3.PatientIdentifier)
bipo_count<-apply(bipo_count[,-1],1,sum)
bipolar<-bipo_count>0


# Collapse and pull info back into df
Dx <- ifelse(schizophrenia==T & bipolar==T,'Both',
             ifelse(schizophrenia==T & bipolar==F,'Schizophrenia',
                    ifelse(schizophrenia==F & bipolar==T,'Bipolar',
                           'Neither')))
table(Dx)
Dx<-data.frame('PatientIdentifier'=unique(test3$PatientIdentifier),Dx)
Dx<-merge(test3[,1],Dx,by='PatientIdentifier')

rm(sczo_subset, sczo_count, schizophrenia, bipo_subset, bipo_count, bipolar)
