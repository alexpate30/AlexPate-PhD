##### This program will calculate Harrels C, Roystons and Saubrei's D and
##### Roystons and Saubrei's R^2_D  for model F (female cohort)

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
load("imputed_datasets_loaded_female.RData")

### PART 1) LOAD MODELS
### PART 1) LOAD MODELS
### PART 1) LOAD MODELS

### Start by loading the models that were fitted in previous program
setwd("B")
load("models_female_modelE.RData")

## Start by turning it into a factor variable
long_data_parallel_test[[1]]$region<-as.factor(long_data_parallel_test[[1]]$region)


### Also sort anal.dat by patid
anal.dat<-arrange(anal.dat,patid)

### Reduce these to 100,000
print("reduce anal.dat and mean.dat to 100,000")
anal.dat.test<-anal.dat[random.rows,]
anal.dat.devel<-anal.dat[-random.rows,]


## Now remove long_data_parallel for some space
rm(long_data_parallel)


### PART 2) GENERATE LP
### PART 2) GENERATE LP
### PART 2) GENERATE LP

## Now define the function to creaet risks (just want lp as opposed to actual survival probs)
create_lp<-function(model,frailties,newdat){
  ## Extract important coefficients and design matrix
  B.coef<-model$coefficients
  B<-c(B.coef,frailties$frail.coeffs)
  C<-model$var
  S<-model.matrix(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Alcohol_abuse+ Anxiety+ Atrialfib+ Atypical_antipsy_med+ 
                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + LVH + Ethnicity + Famhis_lstrict + Hypertension+ 
                    Medrec1yo50+ Migraine+ Prescrip1yo50+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + 
                    age.Townsend.2 + age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP + time + region, data=long_data_parallel_test[[1]])
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
save.image("discrim_mesaures_female_modelF.RData")

print("image saved")
