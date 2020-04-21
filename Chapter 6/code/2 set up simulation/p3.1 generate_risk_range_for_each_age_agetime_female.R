### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## I want to also calculate the risk range for each age as calculated by the agetime model
## This is the range of risks I will consider (from 1st percentile to 99th percentile) when running the simulation

load("generate_model_agetime_female_FAKE.RData")

library(svMisc)
library(survival)
library(doParallel)
library(foreach)
library(ggplot2)
library(reshape2)
library(dplyr)

## Want to generate risks for each patient
## Note that risk will also be dependent on age, i.e. where they start

## Going to briefly check the conditional risks are sensible
## Going to briefly check the conditional risks are sensible
## Going to briefly check the conditional risks are sensible
basehaz.model <- basehaz(agetime.model.covar,centered = FALSE)
basehaz.haz.model <- basehaz.model$hazard[1:7000]

summary(agetime.model.covar)

haz.by.year <- rep(0,70)
for (i in 1:70){haz.by.year[i] <- basehaz.haz.model[(100*i)]}

## Multiply by HR and calculate  cumulative survival probabilities
haz.by.year <- haz.by.year
surv.by.year <- exp(-haz.by.year)
risk.by.year <- 1 - surv.by.year

# Convert these into marginal and conditional probabilities
marg.by.year <- c(risk.by.year[1],diff(risk.by.year))

cond.by.year<-rep(0,70)
cond.by.year[1]<-marg.by.year[1]
for (i in 2:70){cond.by.year[i]<-marg.by.year[i]/(prod(1-cond.by.year))}

## They get bigger which makes sense!!
plot(1:70,cond.by.year)

rm(cond.by.year,risk.by.year,haz.by.year,surv.by.year)



## Going to create a function which will calculate the risk for each patient
## Going to create a function which will calculate the risk for each patient
## Going to create a function which will calculate the risk for each patient

## Each patient has a different HR, and a different age, which both contribute to the risk score

## Start off  by generating the things which are the same for each patient
## To what increments am I going to do this model, to each year, 1/10 of a year??
haz.by.year <- rep(0,70)
for (i in 1:70){haz.by.year[i] <- basehaz.haz.model[(100*i)]}

## Now get all the hazard ratios into a vector
B <- agetime.model.covar$coefficients
S<-model.matrix(Surv(CVD_time,CVD_cens_R, type='right') ~ BMIfrac1 + BMIfrac2 +  Atrialfib+ Atypical_antipsy_med+ 
                  Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + HIV + Ethnicity + Famhis_lstrict + Hypertension+ 
                  Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE, data=comp.data)
S<-S[,-1]
p<-(S%*%B)[,1]
HR <- exp(p)


## Now want to multiplty two, but resulting in a matrix, where each row is the cumulative hazard for a given patient over the
## whole age range (25 - 84)
haz.by.year.pers <- matrix(rep(haz.by.year,dim(comp.data)[1]),nrow=dim(comp.data)[1],ncol=70,byrow = TRUE)
haz.by.year.pers <- haz.by.year.pers*HR

# Convert these into survival probabilities and risks
surv.by.year.pers <- exp(-haz.by.year.pers)
risks.by.year.pers <- 1 - surv.by.year.pers

# Convert into marginal probabilities
marg.by.year.pers <-  cbind(risks.by.year.pers[,1],t(diff(t(risks.by.year.pers))))

rm(S,agetime.model,surv.by.year.pers,haz.by.year.pers)

## Now create a new matrix of conditional risks for each patient

## Want to just do this for the first say 100,000
Sys.time()
cond.by.year.pers <- matrix(0,nrow=dim(marg.by.year.pers)[1],ncol = 70)

cond.by.year.pers[,1]<-marg.by.year.pers[,1]
for (i in 1:dim(marg.by.year.pers)[1]){
  for (j in 2:70){
    cond.by.year.pers[i,j]<-marg.by.year.pers[i,j]/(prod(1-cond.by.year.pers[i,]))
  }
}


## Then after all this need to calculate the risks between two given ages
## In particular we want the 10 year risks and the life time risks
get.marg.risk.nodeath <- function(cond.risks.in){
  n<-length(cond.risks.in)
  marg.risks <- rep(0,length(cond.risks.in))
  marg.risks[1]<-cond.risks.in[1]
  # Caclulaet all subsequent marginal risks
  for (j in 2:n){marg.risks[j]<-prod((1-cond.risks.in)[1:(j-1)])*cond.risks.in[j]}
  return(sum(marg.risks))
}

## Define the ages
anal.dat <- read.table("data/analysis_dataset_A_comb.csv", sep="," , header=TRUE)
gender.var <- 1
anal.dat<-subset(anal.dat,gender==gender.var)
anal.dat <- arrange(anal.dat,patid)

all.ages <- round(anal.dat$age)

## Create risk scores
all.risks.10y <-rep(0,length(all.ages))
for (i in 1:dim(marg.by.year.pers)[1]){
  all.risks.10y[i] <- get.marg.risk.nodeath(cond.by.year.pers[i,(all.ages[i]-24):(all.ages[i]+9-24)])
}

all.risks.life <-rep(0,length(all.ages))
for (i in 1:dim(marg.by.year.pers)[1]){
  all.risks.life[i] <- get.marg.risk.nodeath(cond.by.year.pers[i,(all.ages[i]-24):(89-24)])
}


### Create a matrix with the risks and outcomes
risk.all <- data.frame("risk.10y" = all.risks.10y,"risk.lifetime" = all.risks.life)
risk.all$age.round <- all.ages
risk.all$age <- anal.dat$age


############# ************************************************************** #################
############# ************************************************************** #################
############# ************************* BREAK POINT ************************ #################
############# ************************* BREAK POINT ************************ #################
############# ************************* BREAK POINT ************************ #################
############# ************************* BREAK POINT ************************ #################
############# ************************* BREAK POINT ************************ #################
############# ************************************************************** #################
############# ************************************************************** #################

# The rest of the code simpoly puts these risks into a table for use in the simulation

## First define the ages vector in which we are interested

## Now want to calculate the minimum and maximum risk for each age
age25 <- c(100*min(risk.all$risk.10y[which(risk.all$age >= 24.9 & risk.all$age <= 26)]),
           100*max(risk.all$risk.10y[which(risk.all$age >= 24.9 & risk.all$age <= 26)]))

ages <- seq(from = 25, to = 85, by = 5)

min.max.age <- data.frame("min" = rep(0,length(ages)), "max" = rep(0,length(ages)))

min.max.age[1,] <- age25

for (i in 2:length(ages)){
  min.max.age[i,] <- c(100*min(risk.all$risk.10y[which(risk.all$age >= ages[i] - 1 & risk.all$age <= ages[i] + 1)]),
                       100*max(risk.all$risk.10y[which(risk.all$age >= ages[i] - 1 & risk.all$age < ages[i] + 1)]))
}


min.max.prange.age <- data.frame("min" = rep(0,length(ages)), "p1" = rep(0,length(ages)), "p2.5" = rep(0,length(ages)),
                                 "p97.5" = rep(0,length(ages)), "p99" = rep(0,length(ages)), "max" = rep(0,length(ages)))

age25 <- 100*quantile(risk.all$risk.10y[which(risk.all$age >= 24.9 & risk.all$age <= 26)],
                      c(0,.01,.025,.975,0.99,1))

min.max.prange.age[1,] <- age25

for (i in 2:length(ages)){
  min.max.prange.age[i,] <- 100*quantile(risk.all$risk.10y[which(risk.all$age >= ages[i] - 1 & risk.all$age <= ages[i] + 1)],
                              c(0,.01,.025,.975,0.99,1))
}


min.max.median.age <- data.frame("min" = rep(0,length(ages)), "p1" = rep(0,length(ages)), "p2.5" = rep(0,length(ages)),"p50"=rep(0,length(ages)),
                                 "p97.5" = rep(0,length(ages)), "p99" = rep(0,length(ages)), "max" = rep(0,length(ages)))

age25 <- 100*quantile(risk.all$risk.10y[which(risk.all$age >= 24.9 & risk.all$age <= 26)],
                      c(0,.01,.025,.5,.975,0.99,1))

min.max.median.age[1,] <- age25

for (i in 2:length(ages)){
  min.max.median.age[i,] <- 100*quantile(risk.all$risk.10y[which(risk.all$age >= ages[i] - 1 & risk.all$age <= ages[i] + 1)],
                                         c(0,.01,.025,.5,.975,0.99,1))
}


min.max.age.agetime <- cbind(ages,min.max.age)
min.max.prange.age.agetime <- cbind(ages,min.max.prange.age)
min.max.median.age.agetime <- cbind(ages,min.max.median.age)

for (i in 2:length(ages)){
  min.max.median.age.agetime[i,9] <- 100*mean(risk.all$risk.10y[which(risk.all$age >= ages[i] - 1 & risk.all$age <= ages[i] + 1)])
}
colnames(min.max.median.age.agetime)[9] <- "mean"


rm(list=setdiff(ls(), list("min.max.prange.age.agetime","ages")))

save.image("R_out_C6/generate_risk_range_for_each_age_agetime_female_FAKE.RData")
