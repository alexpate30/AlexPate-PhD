##### This program will calculate Harrels C, O'quigleys  et al rho_k, Roystons explained variation term of O'quigleys et als rho_k (R2_R), Roystons and Saubrei's D,
##### ,Roystons and Saubrei's R^2_D and Gonen and Hellers C for model C (male cohort)

## First load relevant packages
library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)
# Load packages
library(survcomp)
library(survAUC)
library(Hmisc)
library(CPE)


### PART 0) LOAD IMPUTED DATA
### PART 0) LOAD IMPUTED DATA
### PART 0) LOAD IMPUTED DATA

## The imputed data in long format is stored in a list with 20 elements: long_data_parallel
## Each element of the list is a different imputed dataset
## There is also a dataset called anal.dat, which is the original unimputed dataset
setwd("A")
load("imputed_datasets_loaded_male.RData")


### PART 1) CREATE DATASET TO MAKE PREDICTIONS WITH
### PART 1) CREATE DATASET TO MAKE PREDICTIONS WITH
### PART 1) CREATE DATASET TO MAKE PREDICTIONS WITH

## The first step will be to generate the dataset for which we will make predictions for
## For models A - E this data is mean imputed based on age, gender and ethnicity
## Given ethnicity itself was imputed, we must make some assumption for what this is
## We will choose to base the mean imputation on the ethnicity values from the first multiply imputed dataset

## Extract data
comp.data<-long_data_parallel[[1]]

# Add the Ethnicity variable into anal.dat, the original dataset
anal.dat$Ethnicity.imp<-comp.data$Ethnicity

## Create a dataset of the patients with missing values for each of BMI, SBP and Cholesterol/HDL ratio
BMImiss<-anal.dat[is.na(anal.dat$BMI),]
BMImiss<- select(BMImiss,c("patid","Ethnicity.imp","age"))

SBPmiss<-anal.dat[is.na(anal.dat$SBP),]
SBPmiss<- select(SBPmiss,c("patid","Ethnicity.imp","age"))

SBP_stdmiss<-anal.dat[is.na(anal.dat$SBP_std),]
SBP_stdmiss<- select(SBP_stdmiss,c("patid","Ethnicity.imp","age"))

Cholmiss<-anal.dat[is.na(anal.dat$Cholesterol_HDL_Ratio),]
Cholmiss<- select(Cholmiss,c("patid","Ethnicity.imp","age"))


### Also create the vectors of not missing values
BMInmiss<-anal.dat[is.na(anal.dat$BMI)==0,]
BMInmiss<- select(BMInmiss,c("patid","BMI"))

SBPnmiss<-anal.dat[is.na(anal.dat$SBP)==0,]
SBPnmiss<- select(SBPnmiss,c("patid","SBP"))

SBP_stdnmiss<-anal.dat[is.na(anal.dat$SBP_std)==0,]
SBP_stdnmiss<- select(SBP_stdnmiss,c("patid","SBP_std"))

Cholnmiss<-anal.dat[is.na(anal.dat$Cholesterol_HDL_Ratio)==0,]
Cholnmiss<- select(Cholnmiss,c("patid","Cholesterol_HDL_Ratio"))



# Create the model for every other variable, note it will only use the 'complete' values, for the outcome variable, from the original dataset
# However the prediction is made for every variable in that dataset

## BMI
# Fit model
fit.BMI<-lm(BMI ~ age + Ethnicity.imp, data=anal.dat)

#Create predictions
pred.BMI<-predict(fit.BMI,newdata=BMImiss)
pred.BMI.miss<-data.frame('BMI'=pred.BMI,'patid'=BMImiss$patid)


## Cholesterol_HDL_Ratio
# Fit model
fit.Cholesterol_HDL_Ratio<-lm(Cholesterol_HDL_Ratio ~ age + Ethnicity.imp, data=anal.dat)

#Create predictions
pred.Cholesterol_HDL_Ratio<-predict(fit.Cholesterol_HDL_Ratio,newdata=Cholmiss)
pred.Chol.miss<-data.frame('Cholesterol_HDL_Ratio'=pred.Cholesterol_HDL_Ratio,'patid'=Cholmiss$patid)



## SBP
# Fit model
fit.SBP<-lm(SBP ~ age + Ethnicity.imp, data=anal.dat)

#Create predictions
pred.SBP<-predict(fit.SBP,newdata=SBPmiss)
pred.SBP.miss<-data.frame('SBP'=pred.SBP,'patid'=SBPmiss$patid)


## SBP_std
# Fit model
fit.SBP_std<-lm(SBP_std ~ age + Ethnicity.imp, data=anal.dat)

#Create predictions
pred.SBP_std<-predict(fit.SBP_std,newdata=SBP_stdmiss)
pred.SBP_std.miss<-data.frame('SBP_std'=pred.SBP_std,'patid'=SBP_stdmiss$patid)


### These are the values which have been predicted
### I need to join these with the non missing values
all.BMI<-rbind(pred.BMI.miss,BMInmiss)
all.SBP<-rbind(pred.SBP.miss,SBPnmiss)
all.SBP_std<-rbind(pred.SBP_std.miss,SBP_stdnmiss)
all.Chol<-rbind(pred.Chol.miss,Cholnmiss)


## Check the values make sense - they do
mean(pred.SBP.miss$SBP)
mean(SBPnmiss$SBP)

mean(pred.BMI.miss$BMI)
mean(BMInmiss$BMI)

mean(pred.SBP_std.miss$SBP_std)
mean(SBP_stdnmiss$SBP_std)

mean(pred.Chol.miss$Cholesterol_HDL_Ratio)
mean(Cholnmiss$Cholesterol_HDL_Ratio)


## Order the predictions in a sensible way
all.BMI<-arrange(all.BMI,patid)
all.SBP<-arrange(all.SBP,patid)
all.SBP_std<-arrange(all.SBP_std,patid)
all.Chol<-arrange(all.Chol,patid)


## Take the first multiply imputed dataset (this has the correct ethnicity values).
## Extract it and call it mean.dat, this will be the name of the mean imputed dataset
mean.dat<-long_data_parallel[[1]]
mean.dat<-arrange(mean.dat,patid)

## Replace values which had been multipled imputed with mean imputations
mean.dat$BMI<-all.BMI$BMI
mean.dat$Cholesterol_HDL_Ratio<-all.Chol$Cholesterol_HDL_Ratio
mean.dat$SBP<-all.SBP$SBP
mean.dat$SBP_std<-all.SBP_std$SBP_std

## Each time I extract the design matrix (i.e. choosing different variables), I will do it using mean.dat


### PART 2) GET DATA IN CORRECT FORM FOR MODELLING
### PART 2) GET DATA IN CORRECT FORM FOR MODELLING
### PART 2) GET DATA IN CORRECT FORM FOR MODELLING

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
mean.dat<-convert(mean.dat)

### Sort so we can have it in the same order
for (i in 1:20){long_data_parallel[[i]]<-arrange(long_data_parallel[[i]],patid)}
mean.dat <- arrange(mean.dat, patid)


## Apply this function to all the long data parallels and also mean.dat
for (i in 1:20){long_data_parallel[[i]]<-convert(long_data_parallel[[i]])}
mean.dat<-convert(mean.dat)

### Sort so we can have it in the same order
for (i in 1:20){long_data_parallel[[i]]<-arrange(long_data_parallel[[i]],patid)}
mean.dat <- arrange(mean.dat, patid)


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

## Do the same for anal.dat and mean.dat
anal.dat.test<-anal.dat[random.rows,]
mean.dat.test <- mean.dat[random.rows,]

## Check long data parallel test and mean.dat.test are same
head(long_data_parallel_test[[1]])
head(mean.dat.test)

## Remove some large files we no longer need
rm(long_data_parallel_test, mean.dat, long_data_parallel)


### PART 4) FIT MODELS AND GENERATE LINEAR PREDICTORS
### PART 4) FIT MODELS AND GENERATE LINEAR PREDICTORS
### PART 4) FIT MODELS AND GENERATE LINEAR PREDICTORS

### Model A
### Model A
### Model A

## Do the first 10 models
## Analyse the imputed datasets, parallelising over 10 cores
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.1<-(foreach(input=list(long_data_parallel_devel[[1]],long_data_parallel_devel[[2]],long_data_parallel_devel[[3]],long_data_parallel_devel[[4]],long_data_parallel_devel[[5]],
                                         long_data_parallel_devel[[6]],long_data_parallel_devel[[7]],long_data_parallel_devel[[8]],long_data_parallel_devel[[9]],long_data_parallel_devel[[10]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMI + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Erec_dysfunc + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
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
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMI + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Erec_dysfunc + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)

print("second half of models fitted")


## Create linear predictors for both the development cohort and test cohort
## Create linear predictors for both the development cohort and test cohort

## First make the linear predictors for the test cohort
cl <- makeCluster(11)
registerDoParallel(11)
pred.model.1<-(foreach(input=fit_data_parallel.1, .combine=list, .multicombine=TRUE, 
                       .packages=c("dplyr","mice","tidyr","survival"))
               %dopar%{predict(input, newdata= mean.dat.test)
               })
stopCluster(cl)

print("first half of linear predictors done")

cl <- makeCluster(11)
registerDoParallel(11)
pred.model.2<-(foreach(input=fit_data_parallel.2, .combine=list, .multicombine=TRUE, 
                       .packages=c("dplyr","mice","tidyr","survival"))
               %dopar%{predict(input, newdata= mean.dat.test)
               })
stopCluster(cl)

print("second half of linear predictors done")

## Now make the linear predictors for the development cohort
cl <- makeCluster(11)
registerDoParallel(11)
pred.model.1.devel<-(foreach(input=fit_data_parallel.1, .combine=list, .multicombine=TRUE, 
                             .packages=c("dplyr","mice","tidyr","survival"))
                     %dopar%{predict(input)
                     })
stopCluster(cl)

print("first half of linear predictors done")

cl <- makeCluster(11)
registerDoParallel(11)
pred.model.2.devel<-(foreach(input=fit_data_parallel.2, .combine=list, .multicombine=TRUE, 
                             .packages=c("dplyr","mice","tidyr","survival"))
                     %dopar%{predict(input)
                     })
stopCluster(cl)

print("second half of linear predictors done")

### Now make the models which only use 200000 people
### These will only be used for Gonen and Hellers C, as the algorithm will not work on cohort's much bigger than this
### Note Gonen and Hellers is calculated directly from the model, so just going to use the test data for this
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.1.m200000<-(foreach(input=list(long_data_parallel_test[[1]],long_data_parallel_test[[2]],long_data_parallel_test[[3]],long_data_parallel_test[[4]],long_data_parallel_test[[5]],
                                         long_data_parallel_test[[6]],long_data_parallel_test[[7]],long_data_parallel_test[[8]],long_data_parallel_test[[9]],long_data_parallel_test[[10]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMI + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Erec_dysfunc + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)

print("first half of models fitted")


cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.2.m200000<-(foreach(input=list(long_data_parallel_test[[11]],long_data_parallel_test[[12]],long_data_parallel_test[[13]],long_data_parallel_test[[14]],long_data_parallel_test[[15]],
                                         long_data_parallel_test[[16]],long_data_parallel_test[[17]],long_data_parallel_test[[18]],long_data_parallel_test[[19]],long_data_parallel_test[[20]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMI + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Erec_dysfunc + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                      age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)

print("second half of models fitted")


### NOW CALCULATE METRICS
### NOW CALCULATE METRICS
### NOW CALCULATE METRICS


Sys.time()
print("Harrels C starts")

## Harrels C
## Harrels C        
## Harrels C
cl <- makeCluster(11)
registerDoParallel(11)
HarrelsC.1<-(foreach(input=pred.model.1, .combine=list, .multicombine=TRUE, 
                     .packages=c("dplyr","mice","tidyr","survival"))
             %dopar%{HC<-rcorr.cens(x=input,S=Surv(anal.dat.test$CVD_time,anal.dat.test$CVD_cens_R))
                     HC["C Index"]
             })
stopCluster(cl)

cl <- makeCluster(11)
registerDoParallel(11)
HarrelsC.2<-(foreach(input=pred.model.2, .combine=list, .multicombine=TRUE, 
                     .packages=c("dplyr","mice","tidyr","survival"))
             %dopar%{HC<-rcorr.cens(x=input,S=Surv(anal.dat.test$CVD_time,anal.dat.test$CVD_cens_R))
                     HC["C Index"]
             })
stopCluster(cl)

str(anal.dat.test)
str(anal.dat.test$CVD_cens_R)

HarrelsC<-c(HarrelsC.1,HarrelsC.2)
HarrelsC
HarrelsC.A<-unlist(HarrelsC)
HarrelsC.A
mean(HarrelsC.A)

Sys.time()

## Roystons and Saubrei's D
## Roystons and Saubrei's D
## Roystons and Saubrei's D

print("royston and saeubreis D")

cl <- makeCluster(11)
registerDoParallel(11)
D_RS.1<-(foreach(input=pred.model.1, .combine=list, .multicombine=TRUE, 
                 .packages=c("dplyr","mice","tidyr","survival"))
         %dopar%{RD<-D.index(x=input,surv.time=anal.dat.test$CVD_time,surv.event=anal.dat.test$CVD_cens_R)
                 RD$d.index
         })
stopCluster(cl)

cl <- makeCluster(11)
registerDoParallel(11)
D_RS.2<-(foreach(input=pred.model.2, .combine=list, .multicombine=TRUE, 
                 .packages=c("dplyr","mice","tidyr","survival"))
         %dopar%{RD<-D.index(x=input,surv.time=anal.dat.test$CVD_time,surv.event=anal.dat.test$CVD_cens_R)
                 RD$d.index
         })
stopCluster(cl)

D_RS<-c(D_RS.1,D_RS.2)
D_RS
D_RS.A<-unlist(D_RS)
D_RS.A
mean(D_RS.A)

Sys.time()

## Roystons and Saubrei's R^2_D
## Roystons and Saubrei's R^2_D

print("royston and saeubreis R^2_D")

## Simply need to apply transformation from the D statistic
kappa<-sqrt(8/pi)
sigma.sq<-(pi^2)/6

R2_D_RS.A<-((D_RS.A^2)/(kappa^2))/(sigma.sq + (D_RS.A^2)/(kappa^2))
R2_D_RS.A
mean(D_RS.A)


Sys.time()
print("O'quigleys  et al rho_k")

## O'quigleys  et al rho_k
## O'quigleys  et al rho_k
## O'quigleys  et al rho_k
create_rsq<-function(model){
  logtest <- -2*(model$loglik[1] - model$loglik[2])
  rsq.r<-1-exp(-logtest/model$n)
  rsq.quig<-1-exp(-logtest/model$nevent)
  return(rsq.quig)
}

cl <- makeCluster(11)
registerDoParallel(11)
rho_k_manual.1<-(foreach(input=fit_data_parallel.1, .combine=list, .multicombine=TRUE, 
                         .packages=c("dplyr","mice","tidyr","survival"))
                 %dopar%{create_rsq(input)
                 })
stopCluster(cl)

cl <- makeCluster(11)
registerDoParallel(11)
rho_k_manual.2<-(foreach(input=fit_data_parallel.2, .combine=list, .multicombine=TRUE, 
                         .packages=c("dplyr","mice","tidyr","survival"))
                 %dopar%{create_rsq(input)
                 })
stopCluster(cl)

rho_k_manual<-c(rho_k_manual.1,rho_k_manual.2)
rho_k_manual
rho_k_manual.A<-unlist(rho_k_manual)
rho_k_manual.A
mean(rho_k_manual.A)



Sys.time()
print("Roystons explained variation term of O'quigleys et als rho_k")

## Roystons explained variation term of O'quigleys et als rho_k
## Roystons explained variation term of O'quigleys et als rho_k
## Roystons explained variation term of O'quigleys et als rho_k

# Roystons R2_R
R2_R.A<-(rho_k_manual.A^2)/(rho_k_manual.A^2+((pi^2)/6)*(1-rho_k_manual.A^2))
R2_R.A
mean(R2_R.A)



Sys.time()
print("Gonen and Hellers C starts")

### Gonen and Hellers C, GH_C
### Gonen and Hellers C, GH_C
### Gonen and Hellers C, GH_C

## The way I think works properly, cant be done for 'new data', so I am going to do it two ways and store them both

## CPE package
cl <- makeCluster(11)
registerDoParallel(11)
GH_C_v1.1<-(foreach(input=fit_data_parallel.1.m200000, .combine=list, .multicombine=TRUE, 
                .packages=c("dplyr","mice","tidyr","survival"))
        %dopar%{GH<-phcpe(input)
                GH
        })
stopCluster(cl)

cl <- makeCluster(11)
registerDoParallel(11)
GH_C_v1.2<-(foreach(input=fit_data_parallel.2.m200000, .combine=list, .multicombine=TRUE, 
                    .packages=c("dplyr","mice","tidyr","survival"))
            %dopar%{GH<-phcpe(input)
                    GH
            })
stopCluster(cl)

GH_C_v1<-c(GH_C_v1.1,GH_C_v1.2)
GH_C_v1
GH_C_v1.A<-unlist(GH_C_v1)
GH_C_v1.A
mean(GH_C_v1.A)


print("all done - ready to save")

rm(list=setdiff(ls(),list("D_RS.A","GH_C_v1.A","GH_C_v2.A","HarrelsC.A","R2_D_RS.A","R2_R.A","rho_k_manual.A")))
setwd("B")
save.image("discrim_mesaures_male_modelC.RData")

print("image saved")
rm(list=ls())
