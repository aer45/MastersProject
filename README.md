# MastersProject
Repo for Master's Project on clustering for mental health diagnoses

Filters for dataset:

ICD Diagnosis Code based on Phenotype definitions for Schizophrenia and Bipolar diagnoses found in CCW Chronic Conditions Categories   
Patient Age >= 18 at visit  
Admit date between [7/1/2014 - 6/30/2015]  

To use this program the steps are:
1. Run functions in MultiPlotFunction.R
2. Run DatasetSetup.R to load data
3. Run PhenoDef.R to check for phenotypes
4. Run Missing Values.R to impute missing values via RF
5. Choose dissimilarity method to compute distance and cluster
