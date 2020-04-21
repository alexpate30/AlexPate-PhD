##### This program will generate the risks for each patient in the 2016 cohort, for models A, B, C and D (female cohort)
##### This program will generate the risks for each patient in the 2016 cohort, for models A, B, C and D (female cohort)
##### This program will generate the risks for each patient in the 2016 cohort, for models A, B, C and D (female cohort)


## First load relevant packages (not all are specifically required for this program)
library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)

## Define gender.var which will be used to subset a read in dataset to either females (<- 1) or males (<- 0)
gender.var <- 1

### PART 0) LOAD IMPUTED DATA FROM 2016 COHORT
### PART 0) LOAD IMPUTED DATA FROM 2016 COHORT
### PART 0) LOAD IMPUTED DATA FROM 2016 COHORT

### First we load the  imputed data from cohort 2016 (we will load in the data from developing the models later)

## The imputed data in long format is stored in a list with 20 elements: long_data_parallel
## Each element of the list is a different imputed dataset
## There is also a dataset called anal.dat, which is the original unimputed dataset
setwd("A")
load("imputed_datasets_loaded_female_C.RData")

### PART 1) CREATE DATASET TO MAKE PREDICTIONS WITH
### PART 1) CREATE DATASET TO MAKE PREDICTIONS WITH
### PART 1) CREATE DATASET TO MAKE PREDICTIONS WITH

## The first step will be to generate the dataset for which we will make predictions for
## For models A - E this data is mean imputed based on age, gender and ethnicity
## Given ethnicity itself was imputed, we must make some assumption for what this is
## We will choose to base the mean imputation on the ethnicity values from the first multiply imputed dataset

## Assign the completed dataset to comp.data
comp.data<-long_data_parallel
print('2016 cohort loaded')

# Next I need to create the 'mean.dat' dataset

# Add the Ethnicity variable into anal.dat, the original dataset
anal.dat$Ethnicity.imp<-comp.data$Ethnicity

## Create a dataset of the patients with missing values for each of BMI, SBP, Cholesterol HDL and Smoking (I really should include this given it has missing data...)
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
## The only other multiply imputed variable are smoking and Townsend. All others will be replaced by mean imputation
mean.dat<-long_data_parallel
mean.dat<-arrange(mean.dat,patid)

## Replace values with mean imputations
mean.dat$BMI<-all.BMI$BMI
mean.dat$Cholesterol_HDL_Ratio<-all.Chol$Cholesterol_HDL_Ratio
mean.dat$SBP<-all.SBP$SBP
mean.dat$SBP_std<-all.SBP_std$SBP_std

## Each time I extract the design matrix (i.e. choosing different variables), I will do it using mean.dat


### PART 2) GET DATA IN CORRECT FORM FOR MODELLING
### PART 2) GET DATA IN CORRECT FORM FOR MODELLING
### PART 2) GET DATA IN CORRECT FORM FOR MODELLING

### Now this is important

## To fit the models, need it so that at age 25, age = 0 in our cohort
## However I also centered age in the 2016 cohort before imputing
## This means a different amount was deducted off age in the 2016 cohort, and in the development cohort
## Therefore I want to add back on the 'mean age' that was deducted off everyone to centre the variable in the first place
## Then I want to deduct the amount that was deducted from the development cohort so that they are on the same scale
## Finally I make the transformation that is required by the fractional polynomial

###############################################################################################################
###### IF AGE IS ON THE SAME SCALE IN YOUR 2016 COHORT AND DEVELOPMENT COHORT, NO NEED TO DO THIS STUFF #######
###### IF AGE IS ON THE SAME SCALE IN YOUR 2016 COHORT AND DEVELOPMENT COHORT, NO NEED TO DO THIS STUFF #######
###############################################################################################################


## Read in original 2016 dataset
anal.dat <- read.table("D/analysis_dataset_C_comb.csv", sep="," , header=TRUE)
anal.dat<-select(anal.dat, pracid, patid, index_date_C_r, dtvalid_r, dtcens_r, first_statin_r, first_cvd_all_r, 
                 gender,age)
## Subset gender to gender var
anal.dat<-subset(anal.dat,gender==gender.var)  

## Mean.age is what we originally deducted off the 2016 cohort (so need to add this back on to cohort 2016 ages)
mean.age <- mean(anal.dat$age)

## Read in original development cohort
temp.dat.for.original.age <- read.table("D/analysis_dataset_A_comb.csv", sep="," , header=TRUE)
## Subset gender to gender.var
temp.dat.for.original.age<-subset(temp.dat.for.original.age,gender==gender.var) 

## Mean.age.orig is mean age deducted off in development cohort, so want to deduct this from 2016 cohort
mean.age.orig <- mean(temp.dat.for.original.age$age)

rm(temp.dat.for.original.age)


## We now need to define the functions which will convert long_data of the 2016 cohort, and the long data of the development cohort
## into the data form we want. This must be consistent with other models


## Need to calculate the mean age BMI and SBP, for deduction when calculating interaction terms
## Need to be in the scale that I change the variables to in the conversion
mean.age.long1 <- mean(long_data_parallel[[1]]$age + 18.1/10)
mean.BMI.long1 <- mean(long_data_parallel[[1]]$BMI/10)
mean.SBP.long1 <- mean(long_data_parallel[[1]]$SBP/100)


## Create a function that will generate the relevant covariates for cohort 2016
## Note the + mean.age - mean.age.orig before adding 18.1 and dividng by 10
convert_2016<-function(model){
  ## Now add the fractional polynomials I found to be present
  model$BMI<-model$BMI/10
  model$age<-(model$age+mean.age - mean.age.orig + 18.1)/10
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


## Create a function that will generate the relevant covariates for the development cohort
## Note no need here for the +mean.age - mean.age.orig, otherwise the transformation is the same
convert_women<-function(model){
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


## Convert mean.dat
## Note mean.dat is made up of patients from the 2016 oohort (see PART 1)
mean.dat<-convert_2016(mean.dat)

## Want to look at the dataset
head(mean.dat)


### Now load the development data
load("imputed_datasets_loaded_female.RData")

## Apply the tranformation function to all the long data parallels
for (i in 1:20){long_data_parallel[[i]]<-convert_women(long_data_parallel[[i]])}


### PART 3) SPLIT INTO DEVELOPMENT AND TEST COHORT (only development cohort)
### PART 3) SPLIT INTO DEVELOPMENT AND TEST COHORT (only development cohort)
### PART 3) SPLIT INTO DEVELOPMENT AND TEST COHORT (only development cohort)

### Sort long data parallels and mean.dat
for (i in 1:20){long_data_parallel[[i]]<-arrange(long_data_parallel[[i]],patid)}
mean.dat <- arrange(mean.dat, patid)

### Now create the development and test data
## Note we are using test data from cohort 2016, there is no need to reduce it to 200,000
## We want to reduce long_data_parallel to the same development cohort from the main analysis
set.seed(100)
long_data_parallel_test<-vector("list",20)
long_data_parallel_devel<-vector("list",20)
random.rows <- sample(1:dim(long_data_parallel[[i]])[1],200000,replace = F)
for (i in 1:20){long_data_parallel[[i]]<-arrange(long_data_parallel[[i]],patid)}
for (i in 1:20){long_data_parallel_devel[[i]]<-long_data_parallel[[i]][-random.rows,]
                long_data_parallel_test[[i]]<-long_data_parallel[[i]][random.rows,]}

rm(long_data_parllel_test,long_data_parallel,comp.data)


### PART 4) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 4) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 4) GENERATE RISKS, POOL ON log(-log) LEVEL

## We fit models on the development cohort, and generate risks for mean.dat, which is derived from cohort 2016 data

## MODEL A
## MODEL A
## MODEL A


## Fit first 10 models
fit_data_parallel.1 <- vector("list",10)
for (i in 1:10){fit_data_parallel.1[[i]] <- coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Atrialfib+ 
                                                    Cholesterol_HDL_Ratio + CKD45 + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                    RA+ SBP+ Smoking+ T1dia+ T2dia+ Townsend+
                                                    age.Atrialfib + age.CKD45 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 + age.Townsend.3 + age.Townsend.4 +
                                                    age.BMI + age.SBP, data=long_data_parallel_devel[[i]])}


print("first half of models fitted")

## Fit next 10 models
fit_data_parallel.2 <- vector("list",10)
for (i in 1:10){fit_data_parallel.2[[i]] <- coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Atrialfib+ 
                                                    Cholesterol_HDL_Ratio + CKD45 + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                    RA+ SBP+ Smoking+ T1dia+ T2dia+ Townsend+
                                                    age.Atrialfib + age.CKD45 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 + age.Townsend.3 + age.Townsend.4 +
                                                    age.BMI + age.SBP, data=long_data_parallel_devel[[(i+10)]])}


## Generate risks from each model
## Note that we must use the one dataset to generate the risk each time
create_risks<-function(model){
  ## Extract important coefficients and design matrix
  B<-model$coefficients
  C<-model$var
  S<-model.matrix(Surv(patid,.imp, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Atrialfib+ 
                    Cholesterol_HDL_Ratio + CKD45 + Ethnicity + Famhis_lstrict + Hypertension+ 
                    RA+ SBP+ Smoking+ T1dia+ T2dia+ Townsend+
                    age.Atrialfib + age.CKD45 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 + age.Townsend.3 + age.Townsend.4 +
                    age.BMI + age.SBP, data=mean.dat)
  S<-S[,-1]
  p<-(S%*%B)[,1]
  lp<-data.frame(lin.pred=p)
  basehaz<-basehaz(model,centered=FALSE)
  basehaz10y<-basehaz[basehaz$time==3650,]$hazard
  head(basehaz)
  surv<-exp(-basehaz10y*exp(lp))
  return(surv)
}


print("create survival probabilities, first half of models")

surv.probs.1<-vector("list",10)
for (i in 1:10){surv.probs.1[[i]] <- create_risks(fit_data_parallel.1[[i]])}


str(surv.probs.1[[1]])
str(surv.probs.1[[2]])
str(surv.probs.1[[3]])
str(surv.probs.1[[4]])
str(surv.probs.1[[5]])
str(surv.probs.1[[6]])
str(surv.probs.1[[7]])
str(surv.probs.1[[8]])
str(surv.probs.1[[9]])
str(surv.probs.1[[10]])

print("create survival probabilities, second half of models")

surv.probs.2<-vector("list",10)
for (i in 1:10){surv.probs.2[[i]] <- create_risks(fit_data_parallel.2[[i]])}

str(surv.probs.2[[1]])
str(surv.probs.2[[2]])
str(surv.probs.2[[3]])
str(surv.probs.2[[4]])
str(surv.probs.2[[5]])
str(surv.probs.2[[6]])
str(surv.probs.2[[7]])
str(surv.probs.2[[8]])
str(surv.probs.2[[9]])
str(surv.probs.2[[10]])

print('combine survival probs')

## Combine into one dataset
surv.all<-data.frame(mod1=surv.probs.1[[1]],mod2=surv.probs.1[[2]],mod3=surv.probs.1[[3]],mod4=surv.probs.1[[4]],mod5=surv.probs.1[[5]],
                     mod6=surv.probs.1[[6]],mod7=surv.probs.1[[7]],mod8=surv.probs.1[[8]],mod9=surv.probs.1[[9]],mod10=surv.probs.1[[10]],
                     mod11=surv.probs.2[[1]],mod12=surv.probs.2[[2]],mod13=surv.probs.2[[3]],mod14=surv.probs.2[[4]],mod15=surv.probs.2[[5]],
                     mod16=surv.probs.2[[6]],mod17=surv.probs.2[[7]],mod18=surv.probs.2[[8]],mod19=surv.probs.2[[9]],mod20=surv.probs.2[[10]])

## Combine on log(-log) level
surv.all.loglog<-log(-log(surv.all))
surv.all.loglog$surv.av=rowMeans(surv.all.loglog)
surv.all.loglog<-exp(-exp(surv.all.loglog))
surv.all.loglog$risk<-1-surv.all.loglog$surv.av
surv.all.A.loglog<-surv.all.loglog


print("model A done")

### Model B
### Model B
### Model B

## Repeat the process for model B

## Analyse the imputed datasets
fit_data_parallel.1 <- vector("list",10)
for (i in 1:10){fit_data_parallel.1[[i]] <- coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 +  Atrialfib+ Atypical_antipsy_med+ 
                                                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                    Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                                                    age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=long_data_parallel_devel[[i]])}



print("first half of models fitted")


fit_data_parallel.2 <- vector("list",10)
for (i in 1:10){fit_data_parallel.2[[i]] <- coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 +  Atrialfib+ Atypical_antipsy_med+ 
                                                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                    Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                                                    age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=long_data_parallel_devel[[(i+10)]])}
print("second half of models fitted")


## Generate risks from each model
## Note that we must use the one dataset to generate the risk each time
create_risks<-function(model){
  ## Extract important coefficients and design matrix
  B<-model$coefficients
  C<-model$var
  S<-model.matrix(Surv(patid,.imp, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 +  Atrialfib+ Atypical_antipsy_med+ 
                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + Ethnicity + Famhis_lstrict + Hypertension+ 
                    Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                    age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=mean.dat)
  S<-S[,-1]
  p<-(S%*%B)[,1]
  lp<-data.frame(lin.pred=p)
  basehaz<-basehaz(model,centered=FALSE)
  basehaz10y<-basehaz[basehaz$time==3650,]$hazard
  head(basehaz)
  surv<-exp(-basehaz10y*exp(lp))
  return(surv)
}


print("create survival probabilities, first half of models")

## Now I need to parallelise this and do for each dataset
print("create survival probabilities, first half of models")

surv.probs.1<-vector("list",10)
for (i in 1:10){surv.probs.1[[i]] <- create_risks(fit_data_parallel.1[[i]])}


str(surv.probs.1[[1]])
str(surv.probs.1[[2]])
str(surv.probs.1[[3]])
str(surv.probs.1[[4]])
str(surv.probs.1[[5]])
str(surv.probs.1[[6]])
str(surv.probs.1[[7]])
str(surv.probs.1[[8]])
str(surv.probs.1[[9]])
str(surv.probs.1[[10]])

print("create survival probabilities, second half of models")

surv.probs.2<-vector("list",10)
for (i in 1:10){surv.probs.2[[i]] <- create_risks(fit_data_parallel.2[[i]])}

str(surv.probs.2[[1]])
str(surv.probs.2[[2]])
str(surv.probs.2[[3]])
str(surv.probs.2[[4]])
str(surv.probs.2[[5]])
str(surv.probs.2[[6]])
str(surv.probs.2[[7]])
str(surv.probs.2[[8]])
str(surv.probs.2[[9]])
str(surv.probs.2[[10]])

print('combine survival probs')

surv.all<-data.frame(mod1=surv.probs.1[[1]],mod2=surv.probs.1[[2]],mod3=surv.probs.1[[3]],mod4=surv.probs.1[[4]],mod5=surv.probs.1[[5]],
                     mod6=surv.probs.1[[6]],mod7=surv.probs.1[[7]],mod8=surv.probs.1[[8]],mod9=surv.probs.1[[9]],mod10=surv.probs.1[[10]],
                     mod11=surv.probs.2[[1]],mod12=surv.probs.2[[2]],mod13=surv.probs.2[[3]],mod14=surv.probs.2[[4]],mod15=surv.probs.2[[5]],
                     mod16=surv.probs.2[[6]],mod17=surv.probs.2[[7]],mod18=surv.probs.2[[8]],mod19=surv.probs.2[[9]],mod20=surv.probs.2[[10]])

surv.all.loglog<-log(-log(surv.all))
surv.all.loglog$surv.av=rowMeans(surv.all.loglog)
surv.all.loglog<-exp(-exp(surv.all.loglog))
surv.all.loglog$risk<-1-surv.all.loglog$surv.av
surv.all.B.loglog<-surv.all.loglog


setwd("B")
save.image("risks_female_cohort2016.RData")
print('image saved half way through')

### Model C
### Model C
### Model C

## Repeat process for model C

## Analyse the imputed datasets
fit_data_parallel.1 <- vector("list",10)
for (i in 1:10){fit_data_parallel.1[[i]]<-coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                                  Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                  Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                                  age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                  age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                                  age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=long_data_parallel_devel[[i]])}
                


print("first half of models fitted")

fit_data_parallel.2 <- vector("list",10)
for (i in 1:10){fit_data_parallel.2[[i]]<-coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                                  Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                  Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                                  age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                  age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                                  age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=long_data_parallel_devel[[(i+10)]])}

print("second half of models fitted")


## Generate risks from each model
## Note that we must use the one dataset to generate the risk each time
create_risks<-function(model){
  ## Extract important coefficients and design matrix
  B<-model$coefficients
  C<-model$var
  S<-model.matrix(Surv(patid,.imp, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                    Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                    age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=mean.dat)
  S<-S[,-1]
  p<-(S%*%B)[,1]
  lp<-data.frame(lin.pred=p)
  basehaz<-basehaz(model,centered=FALSE)
  basehaz10y<-basehaz[basehaz$time==3650,]$hazard
  head(basehaz)
  surv<-exp(-basehaz10y*exp(lp))
  return(surv)
}


## Now I need to parallelise this and do for each dataset
print("create survival probabilities, first half of models")

surv.probs.1<-vector("list",10)
for (i in 1:10){surv.probs.1[[i]] <- create_risks(fit_data_parallel.1[[i]])}


str(surv.probs.1[[1]])
str(surv.probs.1[[2]])
str(surv.probs.1[[3]])
str(surv.probs.1[[4]])
str(surv.probs.1[[5]])
str(surv.probs.1[[6]])
str(surv.probs.1[[7]])
str(surv.probs.1[[8]])
str(surv.probs.1[[9]])
str(surv.probs.1[[10]])

print("create survival probabilities, second half of models")

surv.probs.2<-vector("list",10)
for (i in 1:10){surv.probs.2[[i]] <- create_risks(fit_data_parallel.2[[i]])}

str(surv.probs.2[[1]])
str(surv.probs.2[[2]])
str(surv.probs.2[[3]])
str(surv.probs.2[[4]])
str(surv.probs.2[[5]])
str(surv.probs.2[[6]])
str(surv.probs.2[[7]])
str(surv.probs.2[[8]])
str(surv.probs.2[[9]])
str(surv.probs.2[[10]])

print('combine survival probs')

surv.all<-data.frame(mod1=surv.probs.1[[1]],mod2=surv.probs.1[[2]],mod3=surv.probs.1[[3]],mod4=surv.probs.1[[4]],mod5=surv.probs.1[[5]],
                     mod6=surv.probs.1[[6]],mod7=surv.probs.1[[7]],mod8=surv.probs.1[[8]],mod9=surv.probs.1[[9]],mod10=surv.probs.1[[10]],
                     mod11=surv.probs.2[[1]],mod12=surv.probs.2[[2]],mod13=surv.probs.2[[3]],mod14=surv.probs.2[[4]],mod15=surv.probs.2[[5]],
                     mod16=surv.probs.2[[6]],mod17=surv.probs.2[[7]],mod18=surv.probs.2[[8]],mod19=surv.probs.2[[9]],mod20=surv.probs.2[[10]])


surv.all.loglog<-log(-log(surv.all))
surv.all.loglog$surv.av=rowMeans(surv.all.loglog)
surv.all.loglog<-exp(-exp(surv.all.loglog))
surv.all.loglog$risk<-1-surv.all.loglog$surv.av
surv.all.C.loglog<-surv.all.loglog


### Model D
### Model D
### Model D

## Repeat process for model D

## Analyse the imputed datasets
fit_data_parallel.1 <- vector("list",10)
for (i in 1:10){fit_data_parallel.1[[i]]<-coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                                  Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                  Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                                  age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                  age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                                  age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP+ time, data=long_data_parallel_devel[[i]])}



print("first half of models fitted")

fit_data_parallel.2 <- vector("list",10)
for (i in 1:10){fit_data_parallel.2[[i]]<-coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                                                  Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                                                  Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                                  age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                                  age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                                                  age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP+ time, data=long_data_parallel_devel[[(i+10)]])}

print("second half of models fitted")


## Generate risks from each model
## Note that we must use the one dataset to generate the risk each time
create_risks<-function(model){
  ## Extract important coefficients and design matrix
  B<-model$coefficients
  C<-model$var
  S<-model.matrix(Surv(patid,.imp, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                    Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                    age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP+ time, data=mean.dat)
  S<-S[,-1]
  p<-(S%*%B)[,1]
  lp<-data.frame(lin.pred=p)
  basehaz<-basehaz(model,centered=FALSE)
  basehaz10y<-basehaz[basehaz$time==3650,]$hazard
  head(basehaz)
  surv<-exp(-basehaz10y*exp(lp))
  return(surv)
}


## Now I need to parallelise this and do for each dataset
print("create survival probabilities, first half of models")

surv.probs.1<-vector("list",10)
for (i in 1:10){surv.probs.1[[i]] <- create_risks(fit_data_parallel.1[[i]])}


str(surv.probs.1[[1]])
str(surv.probs.1[[2]])
str(surv.probs.1[[3]])
str(surv.probs.1[[4]])
str(surv.probs.1[[5]])
str(surv.probs.1[[6]])
str(surv.probs.1[[7]])
str(surv.probs.1[[8]])
str(surv.probs.1[[9]])
str(surv.probs.1[[10]])

print("create survival probabilities, second half of models")

surv.probs.2<-vector("list",10)
for (i in 1:10){surv.probs.2[[i]] <- create_risks(fit_data_parallel.2[[i]])}

str(surv.probs.2[[1]])
str(surv.probs.2[[2]])
str(surv.probs.2[[3]])
str(surv.probs.2[[4]])
str(surv.probs.2[[5]])
str(surv.probs.2[[6]])
str(surv.probs.2[[7]])
str(surv.probs.2[[8]])
str(surv.probs.2[[9]])
str(surv.probs.2[[10]])

print('combine survival probs')

surv.all<-data.frame(mod1=surv.probs.1[[1]],mod2=surv.probs.1[[2]],mod3=surv.probs.1[[3]],mod4=surv.probs.1[[4]],mod5=surv.probs.1[[5]],
                     mod6=surv.probs.1[[6]],mod7=surv.probs.1[[7]],mod8=surv.probs.1[[8]],mod9=surv.probs.1[[9]],mod10=surv.probs.1[[10]],
                     mod11=surv.probs.2[[1]],mod12=surv.probs.2[[2]],mod13=surv.probs.2[[3]],mod14=surv.probs.2[[4]],mod15=surv.probs.2[[5]],
                     mod16=surv.probs.2[[6]],mod17=surv.probs.2[[7]],mod18=surv.probs.2[[8]],mod19=surv.probs.2[[9]],mod20=surv.probs.2[[10]])


surv.all.loglog<-log(-log(surv.all))
surv.all.loglog$surv.av=rowMeans(surv.all.loglog)
surv.all.loglog<-exp(-exp(surv.all.loglog))
surv.all.loglog$risk<-1-surv.all.loglog$surv.av
surv.all.D.loglog<-surv.all.loglog

library(dplyr)
surv.all.A.loglog<-select(surv.all.A.loglog,c("surv.av","risk"))
surv.all.B.loglog<-select(surv.all.B.loglog,c("surv.av","risk"))
surv.all.C.loglog<-select(surv.all.C.loglog,c("surv.av","risk"))
surv.all.D.loglog<-select(surv.all.D.loglog,c("surv.av","risk"))
mean.dat <- select(mean.dat,"patid")

## Remoe all unnceesary data
rm(list=setdiff(ls(),list("surv.all.A.loglog","surv.all.B.loglog","surv.all.C.loglog","surv.all.D.loglog","mean.dat")))

setwd("B")
save.image("risks_female_cohort2016.RData")
print('image saved')
