### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## This program will develop the model that is used to calculate CVD event probabilities, and the CVD transition probabilities


### PART 1) Start by reading in that data
### PART 1) Start by reading in that data
### PART 1) Start by reading in that data


library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)

load("imputed_datasets_loaded_male_FAKE.RData")

## I will choose the first one (at random)
comp.data<-long_data_parallel[[1]]

## I need to generate the age from age = 25
summary(comp.data$age)

anal.dat <- read.table("data/analysis_dataset_A_comb.csv", sep="," , header=TRUE)
gender.var <- 0
anal.dat<-subset(anal.dat,gender==gender.var)

library(dplyr)
anal.dat <- arrange(anal.dat,patid)
comp.data <- arrange(comp.data,patid)
head(anal.dat$patid)
head(comp.data$patid)

summary(anal.dat$age)
comp.data$age.time.start <- round(anal.dat$age - 25,digits = 2)
comp.data$age.time.end <- round(anal.dat$age + anal.dat$CVD_time/365.25- 25, digits = 2)


## Try fitting the model
agetime.model <- coxph(Surv(time = age.time.start,time2 = age.time.end, CVD_cens_R) ~ 1, data=comp.data)

## First fit the model with covariates and check out the survival function for the average
agetime.model.covar <- coxph(Surv(time = age.time.start,time2 = age.time.end, CVD_cens_R) ~ BMI + Atrialfib+ Atypical_antipsy_med+ 
                               Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + HIV + Ethnicity + Famhis_lstrict + Hypertension+ 
                               Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE, data=comp.data)

basehaz.model <- basehaz(agetime.model.covar,centered = FALSE)

basehaz.haz.model <- basehaz.model$hazard[1:7000]
basehaz.time.model <- basehaz.model$time[1:7000]

rm(list=setdiff(ls(),list("basehaz.haz.model")))

save.image("R_out_C6/generate_model_agetime_male_FAKE.RData")