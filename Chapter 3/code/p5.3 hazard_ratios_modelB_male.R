##### This program will calculate the hazard ratios for model B for comparison with QRISK3

rm(list=ls())

## Start by loading packages

library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)


### PART 0) LOAD IMPUTED DATA
### PART 0) LOAD IMPUTED DATA
### PART 0) LOAD IMPUTED DATA

## The imputed data in long format is stored in a list with 20 elements: long_data_parallel
## Each element of the list is a different imputed dataset
## There is also a dataset called anal.dat, which is the original unimputed dataset
setwd("A")
load("imputed_datasets_loaded_male.RData")


### PART 1) GET DATA IN CORRECT FORM FOR MODELLING
### PART 1) GET DATA IN CORRECT FORM FOR MODELLING
### PART 1) GET DATA IN CORRECT FORM FOR MODELLING

## Need to calculate the fractional polynomials that are to be included in the model
## Also calculate the interaction terms between age and othe predictor variables

## Continuous variables will be centered before interaction
## Need to calculate the mean age BMI and SBP
## Need to be in the scale that I change the variables to in the conversion
mean.age.long1 <- mean(long_data_parallel[[1]]$age + 16.9/10)
mean.BMI.long1 <- mean(long_data_parallel[[1]]$BMI/10)
mean.SBP.long1 <- mean(long_data_parallel[[1]]$SBP/100)

### NB
## In long_data_parallel, age was centered
## This means it need to be repositioned so that its smallest value is zero (as per the required fractional polynomial)
## Therefore we take the mean of age after applying this repositioning, so that that we can actually calculated a centered age variable when required
## Also note that BMI and SBP undergo scaling, although not repositioned

## Create a function that will generate the relevant covariates for a given dataset
convert<-function(model){
  ## Now add the fractional polynomials I found to be present
  model$BMI<-model$BMI/10
  model$age<-(model$age+16.9)/10
  model$agefrac1 <- (model$age)^(2)
  model$agefrac2 <- (model$age)^0.5
  
  ## Need to re-do interaction terms now that they aren't centered around zero (continuous variables)
  model$age.Atrialfib <- (model$age-mean.age.long1)*(model$Atrialfib==1)
  model$age.Corticosteroid_use <- (model$age-mean.age.long1)*(model$Corticosteroid_use==1)
  model$age.CKD345 <- (model$age-mean.age.long1)*(model$CKD345==1)
  model$age.CKD45 <- (model$age-mean.age.long1)*(model$CKD45==1)
  model$age.Famhis_lstrict <- (model$age-mean.age.long1)*(model$Famhis_lstrict==1)
  model$age.Hypertension <- (model$age-mean.age.long1)*(model$Hypertension==1)
  model$age.T1dia <- (model$age-mean.age.long1)*(model$T1dia==1)
  model$age.T2dia <- (model$age-mean.age.long1)*(model$T2dia==1)
  model$age.Migraine <- (model$age-mean.age.long1)*(model$Migraine==1)
  
  
  ## Need to re-do interaction terms now that they aren't centered around zero (categorical variables)
  model$age.Smoking.1 <- ((model$age-mean.age.long1)*(model$Smoking==1)) 
  model$age.Smoking.2 <- ((model$age-mean.age.long1)*(model$Smoking==2)) 
  model$age.Townsend.1 <- ((model$age-mean.age.long1)*(model$Townsend==2)) 
  model$age.Townsend.2 <- ((model$age-mean.age.long1)*(model$Townsend==3))
  model$age.Townsend.3 <- ((model$age-mean.age.long1)*(model$Townsend==4))
  model$age.Townsend.4 <- ((model$age-mean.age.long1)*(model$Townsend==5))
  
  ## Also get SBP on similar scale to age and BMI
  model$SBP <- model$SBP/100
  
  ## Create a centered BMI and SBP in order to develop model
  model$BMI.cent <- model$BMI - mean.BMI.long1
  model$SBP.cent <- model$SBP - mean.SBP.long1
  model$age.cent <- model$age - mean.age.long1
  
  ## Creat interaction terms
  model$age.BMI <- model$age.cent*model$BMI.cent
  model$age.SBP <- model$age.cent*model$SBP.cent
  return(model)}

## Apply this function to all the long data parallels and also mean.dat
for (i in 1:20){long_data_parallel[[i]]<-convert(long_data_parallel[[i]])}

### Sort so we can have it in the same order
for (i in 1:20){long_data_parallel[[i]]<-arrange(long_data_parallel[[i]],patid)}


### PART 2) SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 2) SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 2) SPLIT INTO DEVELOPMENT AND TEST COHORT


## Set seed
set.seed(100)

## Create lists to store the development and test data
long_data_parallel_test<-vector("list",20)
long_data_parallel_devel<-vector("list",20)

## Pick the 200,000 rows that we will extract for test data
random.rows <- sample(1:dim(long_data_parallel[[i]])[1],200000,replace = F)

## Extract the development and test data
for (i in 1:20){long_data_parallel_devel[[i]]<-long_data_parallel[[i]][-random.rows,]
                long_data_parallel_test[[i]]<-long_data_parallel[[i]][random.rows,]}


## Remove some large files we no longer need
rm(long_data_parallel_test, mean.dat, long_data_parallel)


### PART 3) Fit models
### PART 3) Fit models
### PART 3) Fit models

### Model B
### Model B
### Model B

## Do the first 10 models

## Analyse the imputed datasets, parallelising over 10 cores
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.1<-(foreach(input=list(long_data_parallel_devel[[1]],long_data_parallel_devel[[2]],long_data_parallel_devel[[3]],long_data_parallel_devel[[4]],long_data_parallel_devel[[5]],
                                         long_data_parallel_devel[[6]],long_data_parallel_devel[[7]],long_data_parallel_devel[[8]],long_data_parallel_devel[[9]],long_data_parallel_devel[[10]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMI + Atrialfib+ Atypical_antipsy_med+ 
                                      Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                                      age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)

print("first half of models fitted")


## Analyse the imputed datasets, parallelising over 10 cores
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.2<-(foreach(input=list(long_data_parallel_devel[[11]],long_data_parallel_devel[[12]],long_data_parallel_devel[[13]],long_data_parallel_devel[[14]],long_data_parallel_devel[[15]],
                                         long_data_parallel_devel[[16]],long_data_parallel_devel[[17]],long_data_parallel_devel[[18]],long_data_parallel_devel[[19]],long_data_parallel_devel[[20]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMI + Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + Erec_dysfunc + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                                      age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)



print("second half of models fitted")

## Now I need to get the average of the log hazard ratios, and take exponent to get the hazard ratio for that covariate
all.coeffs<-cbind(fit_data_parallel.1[[1]]$coefficients,fit_data_parallel.1[[2]]$coefficients,fit_data_parallel.1[[3]]$coefficients,fit_data_parallel.1[[4]]$coefficients,
                  fit_data_parallel.1[[5]]$coefficients,fit_data_parallel.1[[6]]$coefficients,fit_data_parallel.1[[7]]$coefficients,fit_data_parallel.1[[8]]$coefficients,
                  fit_data_parallel.1[[9]]$coefficients,fit_data_parallel.1[[10]]$coefficients,fit_data_parallel.2[[1]]$coefficients,fit_data_parallel.2[[2]]$coefficients,
                  fit_data_parallel.2[[3]]$coefficients,fit_data_parallel.2[[4]]$coefficients,fit_data_parallel.2[[5]]$coefficients,fit_data_parallel.2[[6]]$coefficients,
                  fit_data_parallel.2[[7]]$coefficients,fit_data_parallel.2[[8]]$coefficients,fit_data_parallel.2[[9]]$coefficients,fit_data_parallel.2[[10]]$coefficients)

means.all.coeffs<-rowMeans(all.coeffs)
means.HR<-exp(means.all.coeffs)

setwd("B")

rm(list=setdiff(ls(),list("basehaz10y.average","means.HR","means.all.coeffs","mean.dat.test","risk1","surv1","patient")))
save.image("hazard_ratios_modelB_male.RData")

