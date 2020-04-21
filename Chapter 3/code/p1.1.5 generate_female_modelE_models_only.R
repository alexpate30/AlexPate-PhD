##### This program will develop the random intercept model used for models E and F (female cohort)
##### This program will develop the random intercept model used for models E and F (female cohort)
##### This program will develop the random intercept model used for models E and F (female cohort)

## Note that we do not generate risks in the same program
## These models take hours to fit and therefore it is better to develop them and then save the model
## rather than re-fitting it every time we want to re-run. When saved this is a large .RData file

## First load relevant packages (not all are specifically required for this program)
library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)
library(dplyr)
library(coxme)

### PART 0) LOAD IMPUTED DATA
### PART 0) LOAD IMPUTED DATA
### PART 0) LOAD IMPUTED DATA

## The imputed data in long format is stored in a list with 20 elements: long_data_parallel
## Each element of the list is a different imputed dataset
## There is also a dataset called anal.dat, which is the original unimputed dataset
setwd("A")
load("imputed_datasets_loaded_female.RData")



### PART 1) GET DATA IN CORRECT FORM FOR MODELLING
### PART 1) GET DATA IN CORRECT FORM FOR MODELLING
### PART 1) GET DATA IN CORRECT FORM FOR MODELLING


## Need to calculate the fractional polynomials that are to be included in the model
## Also calculate the interaction terms between age and othe predictor variables

## Continuous variables will be centered before interaction
## Need to calculate the mean age BMI and SBP
## Need to be in the scale that I change the variables to in the conversion
mean.age.long1 <- mean(long_data_parallel[[1]]$age + 18.1/10)
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
  model$age<-(model$age+18.1)/10
  model$agefrac1 <- (model$age)^0.5
  model$agefrac2 <- log(model$age)*(model$age)^0.5
  model$BMIfrac1 <- (model$BMI)^-2
  model$BMIfrac2 <- ((model$BMI)^-2)*log(model$BMI)
  
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

## Apply this function to all the long data parallels
for (i in 1:20){long_data_parallel[[i]]<-convert(long_data_parallel[[i]])}


## Add region variable

### First have to read in dataset containing region
## The dataset is called region.dat
## It contains two columns, pracid and region
## It contains the region for each practice
setwd("C")
load("region.dat.RData")
print("region.dat created")

## Merge each long data parallel with the region.dat dataset, by pracid, so that we have the region variable in long data parallel
for (i in 1:20){long_data_parallel[[i]]<-merge(long_data_parallel[[i]],region.dat,by = "pracid")}

### Then need to sort each long data parallel again
for (i in 1:20){long_data_parallel[[i]]<-arrange(long_data_parallel[[i]],patid)}


### PART 3) SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 3) SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 3) SPLIT INTO DEVELOPMENT AND TEST COHORT

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



### PART 3) GENERATE RANDOM INTERCEPT MODELS
### PART 3) GENERATE RANDOM INTERCEPT MODELS
### PART 3) GENERATE RANDOM INTERCEPT MODELS


## Analyse the imputed datasets using coxme, fitting the random intercept model
## First 10 models
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.1<-(foreach(input=list(long_data_parallel_devel[[1]],long_data_parallel_devel[[2]],long_data_parallel_devel[[3]],long_data_parallel_devel[[4]],long_data_parallel_devel[[5]],
                                         long_data_parallel_devel[[6]],long_data_parallel_devel[[7]],long_data_parallel_devel[[8]],long_data_parallel_devel[[9]],long_data_parallel_devel[[10]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxme(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 +  Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP + time + (1|region), data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)


print("first half of models fitted")

## Next 10 models
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.2<-(foreach(input=list(long_data_parallel_devel[[11]],long_data_parallel_devel[[12]],long_data_parallel_devel[[13]],long_data_parallel_devel[[14]],long_data_parallel_devel[[15]],
                                         long_data_parallel_devel[[16]],long_data_parallel_devel[[17]],long_data_parallel_devel[[18]],long_data_parallel_devel[[19]],long_data_parallel_devel[[20]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxme(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP + time + (1|region), data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)

## Check models fitted correctly
str(fit_data_parallel.1[[1]])
str(fit_data_parallel.1[[2]])
str(fit_data_parallel.1[[3]])
str(fit_data_parallel.1[[4]])
str(fit_data_parallel.1[[5]])
str(fit_data_parallel.1[[6]])
str(fit_data_parallel.1[[7]])
str(fit_data_parallel.1[[8]])
str(fit_data_parallel.1[[9]])
str(fit_data_parallel.1[[10]])
str(fit_data_parallel.2[[1]])
str(fit_data_parallel.2[[2]])
str(fit_data_parallel.2[[3]])
str(fit_data_parallel.2[[4]])
str(fit_data_parallel.2[[5]])
str(fit_data_parallel.2[[6]])
str(fit_data_parallel.2[[7]])
str(fit_data_parallel.2[[8]])
str(fit_data_parallel.2[[9]])
str(fit_data_parallel.2[[10]])

## Save half way through program
setwd("B")
save.image("models_female_modelE.RData")

print("first image saved, half way through")


### PART 4) GENERATE COXPH MODELS EXCLUDING REGION
### PART 4) GENERATE COXPH MODELS EXCLUDING REGION
### PART 4) GENERATE COXPH MODELS EXCLUDING REGION

## There was no way to calculate risk scores directly from a cox mixed effects model
## We therefore undergo a two stage process where we calculate the value of the linear predictor from a standard coxph
## model with all the same covariates except region (this is equivalent to model D). We then adjust the linear predictor
## by the coefficients of the random effects (frailities) depending on the region that patient is in.


## Analyse the imputed datasets, first 10 models
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.1.coxph<-(foreach(input=list(long_data_parallel_devel[[1]],long_data_parallel_devel[[2]],long_data_parallel_devel[[3]],long_data_parallel_devel[[4]],long_data_parallel_devel[[5]],
                                         long_data_parallel_devel[[6]],long_data_parallel_devel[[7]],long_data_parallel_devel[[8]],long_data_parallel_devel[[9]],long_data_parallel_devel[[10]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP + time, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)


print("first half of coxph models fitted")

## Analyse the next 10 models
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.2.coxph<-(foreach(input=list(long_data_parallel_devel[[11]],long_data_parallel_devel[[12]],long_data_parallel_devel[[13]],long_data_parallel_devel[[14]],long_data_parallel_devel[[15]],
                                         long_data_parallel_devel[[16]],long_data_parallel_devel[[17]],long_data_parallel_devel[[18]],long_data_parallel_devel[[19]],long_data_parallel_devel[[20]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP + time, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)

print("second half of coxph models fitted")


### PART 5) EXTRACT FRAIL COEFFS
### PART 5) EXTRACT FRAIL COEFFS
### PART 5) EXTRACT FRAIL COEFFS

## We must extract the frailties from each random intercept model.
## In order to be able to generate risks for these it is easier to have one region as the baseline/reference region and
## add this to each persons LP. We then deduct the amount off the effect of each of the other regions
## This is done purely to make the coding easier, as when we extract the model matrix, it will assume one of the region
## values is the reference category

## Generate risks from each model
## Note that we must use the one dataset to generate the risk each time
create_frail_coeffs<-function(model){
  ## Extract important coefficients and design matrix
  B.frail<-model$frail
  B.frail<-unlist(B.frail)
  B.frail.alt<-B.frail-B.frail[1]
  B.frail.alt<-B.frail.alt[-1]
  out <- list("frail.coeffs" = B.frail.alt, "frail.baseline" = B.frail[1])
  return(out)
}

## Extract the frailties from each of the first 10 models
cl <- makeCluster(11)
registerDoParallel(11)
frail.coeffs.all.1<-(foreach(input=fit_data_parallel.1, .combine=list, .multicombine=TRUE, 
                             .packages=c("dplyr","mice","tidyr","survival"))
                     %dopar%{create_frail_coeffs(input)
                     })
stopCluster(cl)

## Extract the frailties from each of the next 10 models
cl <- makeCluster(11)
registerDoParallel(11)
frail.coeffs.all.2<-(foreach(input=fit_data_parallel.2, .combine=list, .multicombine=TRUE, 
                             .packages=c("dplyr","mice","tidyr","survival"))
                     %dopar%{create_frail_coeffs(input)
                     })
stopCluster(cl)

### Now this is done save the models
save.image("models_female_modelE.RData")

print("image saved, DONE!!")
