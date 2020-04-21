##### This program will generate the risks for each patient according to model E (male cohort)
##### This program will generate the risks for each patient according to model E (male cohort)
##### This program will generate the risks for each patient according to model E (male cohort)

## First load relevant packages (not all are specifically required for this program)
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

## Apply this function to mean.dat
## Only need to apply to mean.dat, as this has been done already to long data parallel when developing the models in previous program
mean.dat<-convert(mean.dat)


### PART 3) ADD REGION TO MEAN.DAT AND SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 3) ADD REGION TO MEAN.DAT AND SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 3) ADD REGION TO MEAN.DAT AND SPLIT INTO DEVELOPMENT AND TEST COHORT

### Start by loading the models that were fitted in previous program
setwd("B")
load("models_male_modelE.RData")

## Want to add region to mean.dat
print('add region to mean.dat')

## Start by turning it into a factor variable
long_data_parallel[[1]]$region<-as.factor(long_data_parallel[[1]]$region)

### Need to add the region variable to mean.dat
## create a dataset that is just the patid and region from long_data_parallel
region.dat.all<-select(long_data_parallel[[1]],c("patid","region"))

## Arrange by patid, this is the same order that mean.dat is in
region.dat.all<-arrange(region.dat.all,patid)

## Assign region to mean.dat
mean.dat$region<-region.dat.all$region

print('check str of region variable is correct')
str(mean.dat$region)

## Now remove long_data_parallel for some space
rm(long_data_parallel)

## Extract test data from mean.dat
## Note random.rows is already defined from the previous program
mean.dat.test<-mean.dat[random.rows,]



### PART 4) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 4) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 4) GENERATE RISKS, POOL ON log(-log) LEVEL



## Define the function that will calculate LP, adjust given the region variable, then calculate survival probability assuming baseline hazard from coxph model
## calculating risks for mean.dat.test
create_risks<-function(model,frailties){
  ## Extract important coefficients and design matrix
  B.coef<-model$coefficients
  B<-c(B.coef,frailties$frail.coeffs)
  C<-model$var
  S<-model.matrix(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMI + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Erec_dysfunc + Ethnicity + Famhis_lstrict + Hypertension+ 
                    Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                    age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP + time + region, data=mean.dat.test)
  S<-S[,-1]
  p<-(S%*%B)[,1] + frailties$frail.baseline
  lp<-data.frame(lin.pred=p)
  basehaz<-basehaz(model,centered=FALSE)
  basehaz10y<-basehaz[basehaz$time==3650,]$hazard
  head(basehaz)
  surv<-exp(-basehaz10y*exp(lp))
  return(surv)
}

print('parallelise starting')

## Generate survival probs for first 10 models
cl <- makeCluster(11)
registerDoParallel(11)
surv.probs.1<-(foreach(input=list(list(fit_data_parallel.1.coxph[[1]],frail.coeffs.all.1[[1]]),
                                  list(fit_data_parallel.1.coxph[[2]],frail.coeffs.all.1[[2]]),
                                  list(fit_data_parallel.1.coxph[[3]],frail.coeffs.all.1[[3]]),
                                  list(fit_data_parallel.1.coxph[[4]],frail.coeffs.all.1[[4]]),
                                  list(fit_data_parallel.1.coxph[[5]],frail.coeffs.all.1[[5]]),
                                  list(fit_data_parallel.1.coxph[[6]],frail.coeffs.all.1[[6]]),
                                  list(fit_data_parallel.1.coxph[[7]],frail.coeffs.all.1[[7]]),
                                  list(fit_data_parallel.1.coxph[[8]],frail.coeffs.all.1[[8]]),
                                  list(fit_data_parallel.1.coxph[[9]],frail.coeffs.all.1[[9]]),
                                  list(fit_data_parallel.1.coxph[[10]],frail.coeffs.all.1[[10]])),
                       .combine=list, .multicombine=TRUE, 
                       .packages=c("dplyr","mice","tidyr","survival"))
               %dopar%{create_risks(input[[1]],input[[2]])
               })
stopCluster(cl)

print("first half of surv probs calculated")

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


## Generate survival probs for next 10 models
cl <- makeCluster(11)
registerDoParallel(11)
surv.probs.2<-(foreach(input=list(list(fit_data_parallel.2.coxph[[1]],frail.coeffs.all.2[[1]]),
                                  list(fit_data_parallel.2.coxph[[2]],frail.coeffs.all.2[[2]]),
                                  list(fit_data_parallel.2.coxph[[3]],frail.coeffs.all.2[[3]]),
                                  list(fit_data_parallel.2.coxph[[4]],frail.coeffs.all.2[[4]]),
                                  list(fit_data_parallel.2.coxph[[5]],frail.coeffs.all.2[[5]]),
                                  list(fit_data_parallel.2.coxph[[6]],frail.coeffs.all.2[[6]]),
                                  list(fit_data_parallel.2.coxph[[7]],frail.coeffs.all.2[[7]]),
                                  list(fit_data_parallel.2.coxph[[8]],frail.coeffs.all.2[[8]]),
                                  list(fit_data_parallel.2.coxph[[9]],frail.coeffs.all.2[[9]]),
                                  list(fit_data_parallel.2.coxph[[10]],frail.coeffs.all.2[[10]])),
                       .combine=list, .multicombine=TRUE, 
                       .packages=c("dplyr","mice","tidyr","survival"))
               %dopar%{create_risks(input[[1]],input[[2]])
               })
stopCluster(cl)

print("second half of surv probs calculated")

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

## Combine into dataset
surv.all<-data.frame(mod1=surv.probs.1[[1]],mod2=surv.probs.1[[2]],mod3=surv.probs.1[[3]],mod4=surv.probs.1[[4]],mod5=surv.probs.1[[5]],
                     mod6=surv.probs.1[[6]],mod7=surv.probs.1[[7]],mod8=surv.probs.1[[8]],mod9=surv.probs.1[[9]],mod10=surv.probs.1[[10]],
                     mod11=surv.probs.2[[1]],mod12=surv.probs.2[[2]],mod13=surv.probs.2[[3]],mod14=surv.probs.2[[4]],mod15=surv.probs.2[[5]],
                     mod16=surv.probs.2[[6]],mod17=surv.probs.2[[7]],mod18=surv.probs.2[[8]],mod19=surv.probs.2[[9]],mod20=surv.probs.2[[10]])

## Combine on log(-log) level
surv.all.loglog<-log(-log(surv.all))
surv.all.loglog$surv.av=rowMeans(surv.all.loglog)
surv.all.loglog<-exp(-exp(surv.all.loglog))
surv.all.loglog$risk<-1-surv.all.loglog$surv.av
surv.all.E.loglog<-surv.all.loglog

## Remove unneccsary data before saving
rm(list=setdiff(ls(),list("surv.all.E.loglog")))

surv.all.E.loglog<-select(surv.all.E.loglog,c("surv.av","risk"))

print("stuff removed")
save.image("risks_male_modelE.RData")