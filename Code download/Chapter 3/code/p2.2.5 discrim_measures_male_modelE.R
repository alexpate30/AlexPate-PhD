##### This program will calculate Harrels C, Roystons and Saubrei's D and
##### Roystons and Saubrei's R^2_D  for model E (male cohort)

## First load relevant packageslibrary(mice)
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

## Apply this function to mean.dat
## Only need to apply to mean.dat, as this has been done already to long data parallel when developing the models in previous program
mean.dat<-convert(mean.dat)


### PART 3) ADD REGION TO MEAN.DAT AND SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 3) ADD REGION TO MEAN.DAT AND SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 3) ADD REGION TO MEAN.DAT AND SPLIT INTO DEVELOPMENT AND TEST COHORT

### Start by loading the models that were fitted in previous program
setwd("/mnt/ja01-home01/mbrxsap3/phd_risk/R/p4_run_analysis/")
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


### Also sort anal.dat by patid
anal.dat<-arrange(anal.dat,patid)
mean.dat<- arrange(mean.dat,patid)

### Reduce to 100,000
print("reduce anal.dat and mean.dat to 100,000")
anal.dat.test<-anal.dat[random.rows,]
mean.dat.test<-mean.dat[random.rows,]
anal.dat.devel<-anal.dat[-random.rows,]
mean.dat.devel<-mean.dat[-random.rows,]




## Now define the function to creaet risks (just want lp as opposed to actual survival probs)
create_lp<-function(model,frailties,newdat){
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
  return(p)
}

print("start linear predictors")


## First make the linear predictors for test data
pred.model.1 <- vector("list",10)
pred.model.1[[1]] <- create_lp(fit_data_parallel.1.coxph[[1]],frail.coeffs.all.1[[1]])
pred.model.1[[2]] <- create_lp(fit_data_parallel.1.coxph[[2]],frail.coeffs.all.1[[2]])
pred.model.1[[3]] <- create_lp(fit_data_parallel.1.coxph[[3]],frail.coeffs.all.1[[3]])
pred.model.1[[4]] <- create_lp(fit_data_parallel.1.coxph[[4]],frail.coeffs.all.1[[4]])
pred.model.1[[5]] <- create_lp(fit_data_parallel.1.coxph[[5]],frail.coeffs.all.1[[5]])
pred.model.1[[6]] <- create_lp(fit_data_parallel.1.coxph[[6]],frail.coeffs.all.1[[6]])
pred.model.1[[7]] <- create_lp(fit_data_parallel.1.coxph[[7]],frail.coeffs.all.1[[7]])
pred.model.1[[8]] <- create_lp(fit_data_parallel.1.coxph[[8]],frail.coeffs.all.1[[8]])
pred.model.1[[9]] <- create_lp(fit_data_parallel.1.coxph[[9]],frail.coeffs.all.1[[9]])
pred.model.1[[10]] <- create_lp(fit_data_parallel.1.coxph[[10]],frail.coeffs.all.1[[10]])

print("first half of linear predictors done")

pred.model.2 <- vector("list",10)
pred.model.2[[1]] <- create_lp(fit_data_parallel.2.coxph[[1]],frail.coeffs.all.2[[1]])
pred.model.2[[2]] <- create_lp(fit_data_parallel.2.coxph[[2]],frail.coeffs.all.2[[2]])
pred.model.2[[3]] <- create_lp(fit_data_parallel.2.coxph[[3]],frail.coeffs.all.2[[3]])
pred.model.2[[4]] <- create_lp(fit_data_parallel.2.coxph[[4]],frail.coeffs.all.2[[4]])
pred.model.2[[5]] <- create_lp(fit_data_parallel.2.coxph[[5]],frail.coeffs.all.2[[5]])
pred.model.2[[6]] <- create_lp(fit_data_parallel.2.coxph[[6]],frail.coeffs.all.2[[6]])
pred.model.2[[7]] <- create_lp(fit_data_parallel.2.coxph[[7]],frail.coeffs.all.2[[7]])
pred.model.2[[8]] <- create_lp(fit_data_parallel.2.coxph[[8]],frail.coeffs.all.2[[8]])
pred.model.2[[9]] <- create_lp(fit_data_parallel.2.coxph[[9]],frail.coeffs.all.2[[9]])
pred.model.2[[10]] <- create_lp(fit_data_parallel.2.coxph[[10]],frail.coeffs.all.2[[10]])


### NOW CALCULATE METRICS
### NOW CALCULATE METRICS
### NOW CALCULATE METRICS

Sys.time()
print("Harrels C starts")

## Harrels C
## Harrels C

HarrelsC.1 <- vector("list",10)
for (i in 1:10){HarrelsC.1[[i]] <- rcorr.cens(x=pred.model.1[[i]],S=Surv(anal.dat.test$CVD_time,anal.dat.test$CVD_cens_R))["C Index"]
                print(i)}
HarrelsC.2 <- vector("list",10)
for (i in 1:10){HarrelsC.2[[i]] <- rcorr.cens(x=pred.model.2[[i]],S=Surv(anal.dat.test$CVD_time,anal.dat.test$CVD_cens_R))["C Index"]
                print(i)}


HarrelsC<-c(HarrelsC.1,HarrelsC.2)
HarrelsC
HarrelsC.A<-unlist(HarrelsC)
HarrelsC.A
mean(HarrelsC.A)

Sys.time()
print(" Roystons and Saubrei's D")



## Roystons and Saubrei's D
## Roystons and Saubrei's D

print("royston and saeubreis D")

D_RS.1 <- vector("list",10)
for (i in 1:10){D_RS.1[[i]] <- log(D.index(x=pred.model.1[[i]],surv.time=anal.dat.test$CVD_time,surv.event=anal.dat.test$CVD_cens_R)$d.index)
                print(i)}
D_RS.2 <- vector("list",10)
for (i in 1:10){D_RS.2[[i]] <- log(D.index(x=pred.model.2[[i]],surv.time=anal.dat.test$CVD_time,surv.event=anal.dat.test$CVD_cens_R)$d.index)
                print(i)}

D_RS<-c(D_RS.1,D_RS.2)
D_RS
D_RS.A<-unlist(D_RS)
D_RS.A
mean(D_RS.A)

Sys.time()
print(" Roystons and Saubrei's R^2_D")




## Roystons and Saubrei's R^2_D
## Roystons and Saubrei's R^2_D

print("royston and saeubreis R^2_D")

## Simply need to apply transformation from the D statistic
kappa<-sqrt(8/pi)
sigma.sq<-(pi^2)/6

R2_D_RS.A<-((D_RS.A^2)/(kappa^2))/(sigma.sq + (D_RS.A^2)/(kappa^2))
R2_D_RS.A
mean(D_RS.A)


rm(list=setdiff(ls(),list("D_RS.A","GH_C_v1.A","GH_C_v2.A","HarrelsC.A","R2_D_RS.A","R2_R.A","rho_k_manual.A")))

setwd("B")
save.image("discrim_mesaures_male_modelE.RData")

print("image saved")
