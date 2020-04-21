##### This program calculates the hazard ratios associated with variables introduced to the models (beyond those included in QRISK3)
##### All HR's ratios are taken from model E
## Confidence intervals are calculated after combining the within and between imputation variance

rm(list=ls())

## First load relevant packages (not all are specifically required for this program)
library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)

### Load the models
setwd("B")
load("models_male_modelE.RData")

## Now I need to get the average of the log hazard ratios
## Combine coeffs from each model into one dataset
all.coeffs<-cbind(fit_data_parallel.1.coxph[[1]]$coefficients,fit_data_parallel.1.coxph[[2]]$coefficients,fit_data_parallel.1.coxph[[3]]$coefficients,fit_data_parallel.1.coxph[[4]]$coefficients,
                  fit_data_parallel.1.coxph[[5]]$coefficients,fit_data_parallel.1.coxph[[6]]$coefficients,fit_data_parallel.1.coxph[[7]]$coefficients,fit_data_parallel.1.coxph[[8]]$coefficients,
                  fit_data_parallel.1.coxph[[9]]$coefficients,fit_data_parallel.1.coxph[[10]]$coefficients,fit_data_parallel.2.coxph[[1]]$coefficients,fit_data_parallel.2.coxph[[2]]$coefficients,
                  fit_data_parallel.2.coxph[[3]]$coefficients,fit_data_parallel.2.coxph[[4]]$coefficients,fit_data_parallel.2.coxph[[5]]$coefficients,fit_data_parallel.2.coxph[[6]]$coefficients,
                  fit_data_parallel.2.coxph[[7]]$coefficients,fit_data_parallel.2.coxph[[8]]$coefficients,fit_data_parallel.2.coxph[[9]]$coefficients,fit_data_parallel.2.coxph[[10]]$coefficients)

## Calculate the mean of each coefficient across the models
means.all.coeffs<-rowMeans(all.coeffs)

## Tale exponent to get the hazard ratio
means.HR<-exp(means.all.coeffs)

### Now lets get some confidence intervals
## Use mice functionality to create an object which represents 20 different models from 20 different imputed datasets
mira.obj<-as.mira(list(fit_data_parallel.1.coxph[[1]],fit_data_parallel.1.coxph[[2]],fit_data_parallel.1.coxph[[3]],fit_data_parallel.1.coxph[[4]],
                       fit_data_parallel.1.coxph[[5]],fit_data_parallel.1.coxph[[6]],fit_data_parallel.1.coxph[[7]],fit_data_parallel.1.coxph[[8]],
                       fit_data_parallel.1.coxph[[9]],fit_data_parallel.1.coxph[[10]],fit_data_parallel.2.coxph[[1]],fit_data_parallel.2.coxph[[2]],
                       fit_data_parallel.2.coxph[[3]],fit_data_parallel.2.coxph[[4]],fit_data_parallel.2.coxph[[5]],fit_data_parallel.2.coxph[[6]],
                       fit_data_parallel.2.coxph[[7]],fit_data_parallel.2.coxph[[8]],fit_data_parallel.2.coxph[[9]],fit_data_parallel.2.coxph[[10]]))

## Use the pool function from mice to pool them
pool.res<-pool(mira.obj)

## Want the standard errors of the variance covariance matrix
standard.errors<-sqrt(diag(pool.res$t))

## Get the coeffs
HR.av <- exp(pool.res$qbar)

## Compare the ones from pooled results to manual
print('pooled')
HR.av
print('manual')
means.HR

## They match

## CI upper 
CI.upper <- exp(pool.res$qbar + 1.96*standard.errors)

## CI lower 
CI.lower <- exp(pool.res$qbar - 1.96*standard.errors)

## For time, want to coefficient associated with one year, rather than one day
time.year<-exp(pool.res$qbar["time"]*365)
time.year.upper<-exp((pool.res$qbar["time"] + 1.96*standard.errors["time"])*365)
time.year.lower<-exp((pool.res$qbar["time"] - 1.96*standard.errors["time"])*365)


setwd("B")

rm(long_data_parallel, long_data_parallel_devel, long_data_parallel_test,mean.dat.test,mean.dat.devel, anal.dat, 
   fit_data_parallel.1.coxph,fit_data_parallel.2.coxph,fit_data_parallel.1,fit_data_parallel.2, fit.BMI, fit.SBP, fit.SBP_std, fit.Cholesterol_HDL_Ratio, pred.BMI, pred.Cholesterol_HDL_Ratio,pred.SBP,pred.SBP_std)
rm(mira.obj)
rm(anal.dat)

save.image("hazard_ratios_modelE_male.RData")


