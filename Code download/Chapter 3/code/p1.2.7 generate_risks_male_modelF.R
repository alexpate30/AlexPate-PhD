##### This program will generate the risks for each patient according to model F (male cohort)
##### This program will generate the risks for each patient according to model F (male cohort)
##### This program will generate the risks for each patient according to model F (male cohort)

## First load relevant packages (not all are specifically required for this program)
library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)

## There is no need to load the imputed data here or calculate the mean imputed dataset
## The data we will use to generate the risks is just long_data_parallel_test[[1]]
## Which represents a best guess at the real values

### PART 1) LOAD MODELS
### PART 1) LOAD MODELS
### PART 1) LOAD MODELS

### Next load the actual models
setwd("B")
load("models_male_modelE.RData")

## Make region into factor (neccesary when creating model.matrix dataset)
long_data_parallel_test[[1]]$region <- as.factor(long_data_parallel_test[[1]]$region)


### PART 2) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 2) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 2) GENERATE RISKS, POOL ON log(-log) LEVEL

## Define the function that will calculate LP, adjust given the region variable, then calculate survival probability assuming baseline hazard from coxph model
## calculating risks for long_data_parallel_test[[1]]
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
                    age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP + time + region, data=long_data_parallel_test[[1]])
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

## combine into dataset
surv.all<-data.frame(mod1=surv.probs.1[[1]],mod2=surv.probs.1[[2]],mod3=surv.probs.1[[3]],mod4=surv.probs.1[[4]],mod5=surv.probs.1[[5]],
                     mod6=surv.probs.1[[6]],mod7=surv.probs.1[[7]],mod8=surv.probs.1[[8]],mod9=surv.probs.1[[9]],mod10=surv.probs.1[[10]],
                     mod11=surv.probs.2[[1]],mod12=surv.probs.2[[2]],mod13=surv.probs.2[[3]],mod14=surv.probs.2[[4]],mod15=surv.probs.2[[5]],
                     mod16=surv.probs.2[[6]],mod17=surv.probs.2[[7]],mod18=surv.probs.2[[8]],mod19=surv.probs.2[[9]],mod20=surv.probs.2[[10]])

## Combine on log(-log) scale
surv.all.loglog<-log(-log(surv.all))
surv.all.loglog$surv.av=rowMeans(surv.all.loglog)
surv.all.loglog<-exp(-exp(surv.all.loglog))
surv.all.loglog$risk<-1-surv.all.loglog$surv.av
surv.all.F.loglog<-surv.all.loglog

## Remove unnccesary stuff
rm(list=setdiff(ls(),list("surv.all.F.loglog")))

surv.all.F.loglog<-select(surv.all.F.loglog,c("surv.av","risk"))

print("stuff removed")
save.image("risks_male_modelF.RData")

rm(list=ls())