##### This program will do calibration plots for female cohort, model B
##### This program will do calibration plots for female cohort, model B
##### This program will do calibration plots for female cohort, model B

## First load relevant packages (not all are specifically required for this program)
library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)

## set the gender.var variable, (female <- 1), (male <- 0)
gender.var <- 1

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

## Apply this function to all the long data parallels and also mean.dat
for (i in 1:20){long_data_parallel[[i]]<-convert(long_data_parallel[[i]])}

### Sort so we can have it in the same order
for (i in 1:20){long_data_parallel[[i]]<-arrange(long_data_parallel[[i]],patid)}

### PART 2) SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 2) SPLIT INTO DEVELOPMENT AND TEST COHORT
### PART 2) SPLIT INTO DEVELOPMENT AND TEST COHORT


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


### PART 3) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 3) GENERATE RISKS, POOL ON log(-log) LEVEL
### PART 3) GENERATE RISKS, POOL ON log(-log) LEVEL

### Model B
### Model B
### Model B

## Do the first 10 models

# print("check str of long data parallel that its all there")
# str(long_data_parallel[[1]])
# str(long_data_parallel[[2]])
# str(long_data_parallel[[3]])
# str(long_data_parallel[[4]])
# str(long_data_parallel[[5]])
# str(long_data_parallel[[6]])
# str(long_data_parallel[[7]])
# str(long_data_parallel[[8]])
# str(long_data_parallel[[9]])
# str(long_data_parallel[[10]])
# str(long_data_parallel[[11]])
# str(long_data_parallel[[12]])
# str(long_data_parallel[[13]])
# str(long_data_parallel[[14]])
# str(long_data_parallel[[15]])
# str(long_data_parallel[[16]])
# str(long_data_parallel[[17]])
# str(long_data_parallel[[18]])
# str(long_data_parallel[[19]])
# str(long_data_parallel[[20]])

## Analyse the imputed datasets, parallelising over 10 cores
cl <- makeCluster(11)
registerDoParallel(11)
start_time_fit_parallel <- Sys.time()
fit_data_parallel.1<-(foreach(input=list(long_data_parallel_devel[[1]],long_data_parallel_devel[[2]],long_data_parallel_devel[[3]],long_data_parallel_devel[[4]],long_data_parallel_devel[[5]],
                                         long_data_parallel_devel[[6]],long_data_parallel_devel[[7]],long_data_parallel_devel[[8]],long_data_parallel_devel[[9]],long_data_parallel_devel[[10]]), .combine=list, .multicombine=TRUE, 
                              .packages=c("dplyr","mice","tidyr","survival"))
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 +  Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                                      age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
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
                      %dopar%{coxph(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 +  Atrialfib+ Atypical_antipsy_med+ 
                                      Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + Ethnicity + Famhis_lstrict + Hypertension+ 
                                      Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                                      age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                                      age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                                      age.Townsend.3 + age.Townsend.4 + age.BMI + age.SBP, data=input)
                      })
end_time_fit_parallel <- Sys.time()
diff_fit_parallel <- start_time_fit_parallel - end_time_fit_parallel
stopCluster(cl)

print("second half models fitted")

## Need to use each model to generate survival probabilities
## For this need a function

## Could use survfit in order to do this, but it is way more computationally intensive
create_risks<-function(model,datain){
  ## Extract important coefficients and design matrix
  B<-model$coefficients
  C<-model$var
  S<-model.matrix(Surv(CVD_time,CVD_cens_R, type='right') ~ agefrac1 + agefrac2 + BMIfrac1 + BMIfrac2 + Atrialfib+ Atypical_antipsy_med+ 
                    Cholesterol_HDL_Ratio+ Corticosteroid_use + CKD345 + Ethnicity + Famhis_lstrict + Hypertension+ 
                    Migraine+ RA+ SBP+ SBP_std+ Sev_men_ill_qrisk+ Smoking+ T1dia+ T2dia+ Townsend+ SLE+
                    age.Atrialfib + age.Corticosteroid_use + age.CKD345 + age.Famhis_lstrict + age.Hypertension + age.T1dia + 
                    age.T2dia + age.Smoking.1 + age.Smoking.2 + age.Townsend.1 + age.Townsend.2 +
                    age.Townsend.3 + age.Townsend.4, data=datain)
  S<-S[,-1]
  p<-(S%*%B)[,1]
  lp<-data.frame(lin.pred=p)
  basehaz<-basehaz(model,centered=FALSE)
  basehaz10y<-basehaz[basehaz$time==3653,]$hazard
  head(basehaz)
  surv<-exp(-basehaz10y*exp(lp))
  return(surv)
}

print("create survival probabilities, first half of models")

## Now I need to parallelise this and do for each dataset
cl <- makeCluster(11)
registerDoParallel(11)
surv.probs.1<-(foreach(input=list(list(fit_data_parallel.1[[1]],long_data_parallel_test[[1]]),
                                  list(fit_data_parallel.1[[2]],long_data_parallel_test[[2]]),
                                  list(fit_data_parallel.1[[3]],long_data_parallel_test[[3]]),
                                  list(fit_data_parallel.1[[4]],long_data_parallel_test[[4]]),
                                  list(fit_data_parallel.1[[5]],long_data_parallel_test[[5]]),
                                  list(fit_data_parallel.1[[6]],long_data_parallel_test[[6]]),
                                  list(fit_data_parallel.1[[7]],long_data_parallel_test[[7]]),
                                  list(fit_data_parallel.1[[8]],long_data_parallel_test[[8]]),
                                  list(fit_data_parallel.1[[9]],long_data_parallel_test[[9]]),
                                  list(fit_data_parallel.1[[10]],long_data_parallel_test[[10]])), .combine=list, .multicombine=TRUE, 
                       .packages=c("dplyr","mice","tidyr","survival"))
               %dopar%{create_risks(input[[1]],input[[2]])
               })
stopCluster(cl)

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

 print("second half of models")
 
## Now I need to parallelise this and do for each dataset
cl <- makeCluster(11)
registerDoParallel(11)
surv.probs.2<-(foreach(input=list(list(fit_data_parallel.2[[1]],long_data_parallel_test[[11]]),
                                  list(fit_data_parallel.2[[2]],long_data_parallel_test[[12]]),
                                  list(fit_data_parallel.2[[3]],long_data_parallel_test[[13]]),
                                  list(fit_data_parallel.2[[4]],long_data_parallel_test[[14]]),
                                  list(fit_data_parallel.2[[5]],long_data_parallel_test[[15]]),
                                  list(fit_data_parallel.2[[6]],long_data_parallel_test[[16]]),
                                  list(fit_data_parallel.2[[7]],long_data_parallel_test[[17]]),
                                  list(fit_data_parallel.2[[8]],long_data_parallel_test[[18]]),
                                  list(fit_data_parallel.2[[9]],long_data_parallel_test[[19]]),
                                  list(fit_data_parallel.2[[10]],long_data_parallel_test[[20]])), 
                       .combine=list, .multicombine=TRUE,.packages=c("dplyr","mice","tidyr","survival"))
               %dopar%{create_risks(input[[1]],input[[2]])
               })
stopCluster(cl)

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

surv.all<-data.frame(mod1=surv.probs.1[[1]],mod2=surv.probs.1[[2]],mod3=surv.probs.1[[3]],mod4=surv.probs.1[[4]],mod5=surv.probs.1[[5]],
                     mod6=surv.probs.1[[6]],mod7=surv.probs.1[[7]],mod8=surv.probs.1[[8]],mod9=surv.probs.1[[9]],mod10=surv.probs.1[[10]],
                     mod11=surv.probs.2[[1]],mod12=surv.probs.2[[2]],mod13=surv.probs.2[[3]],mod14=surv.probs.2[[4]],mod15=surv.probs.2[[5]],
                     mod16=surv.probs.2[[6]],mod17=surv.probs.2[[7]],mod18=surv.probs.2[[8]],mod19=surv.probs.2[[9]],mod20=surv.probs.2[[10]])


surv.all.loglog<-log(-log(surv.all))
surv.all.loglog$risk.av=rowMeans(surv.all.loglog)

surv.all<-exp(-exp(surv.all.loglog))
risk.all<-1-surv.all

## Now we have the risks


### PART 4, DO CALIBRATION PLOTS
### PART 4, DO CALIBRATION PLOTS
### PART 4, DO CALIBRATION PLOTS

 ## Read in original data in order to get everyones actual age, CVD_time and CVD_censored
 anal.dat <- read.table("D/analysis_dataset_A_comb.csv", sep="," , header=TRUE)
 anal.dat<-subset(anal.dat,gender==gender.var) 
 anal.dat$CVD_cens_R<-(1-anal.dat$CVD_cens)
 anal.dat<-select(anal.dat, pracid, patid, age, CVD_time, CVD_cens_R)
 
 ## Now split in half and recombine, as was done in imputation, so patid's will match up
 anal.dat<-arrange(anal.dat,patid)
 anal.dat.test<-anal.dat[random.rows,]
 
 ## Assign the ages
 risk.all$patid<-anal.dat.test$patid
 risk.all$pracid<-anal.dat.test$pracid
 risk.all$age<-anal.dat.test$age
 risk.all$CVD_time<-anal.dat.test$CVD_time
 risk.all$CVD_cens_R<-anal.dat.test$CVD_cens_R
 
 rm(anal.dat.raw,anal.dat.raw0,anal.dat.raw1,anal.dat.test)
 rm(anal.dat)


## Create a variable that groups per 10th of predicted risk
risk.all$centile<-as.integer(cut(risk.all$risk.av, 
                       breaks=quantile(risk.all$risk.av,c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
                      )))


## Now calculate the kaplan meier survival for each of these subgroups
cl <- makeCluster(11)
registerDoParallel(11)
km.overall<-(foreach(input=c(1,2,3,4,5,6,7,8,9,10), .combine=list, .multicombine=TRUE, 
                            .packages=c("dplyr","mice","tidyr","survival"))
                    %dopar%{temp.dat<-subset(risk.all,centile==input)
                            temp<-survfit(Surv(CVD_time,CVD_cens_R, type='right') ~ 1,
                                          data=temp.dat)
                            # temp$time does not always have a 3653 value, so find a value that it does have
                            numbers<-3653
                            range<-0
                            arbitrary.numbers<-sort(temp.dat$CVD_time)
                            nearest<-findInterval(numbers, arbitrary.numbers - range)
                            return(1-temp$surv[temp$time==arbitrary.numbers[nearest]]) 
                    })
stopCluster(cl)

km.overall[[1]]
km.overall[[2]]
km.overall[[3]]
km.overall[[4]]
km.overall[[5]]
km.overall[[6]]
km.overall[[7]]
km.overall[[8]]
km.overall[[9]]
km.overall[[10]]

km.all<-unlist(km.overall)

## Now need to calculate the average predicted risk per group
cl <- makeCluster(11)
registerDoParallel(11)
predrisk.overall<-(foreach(input=c(1,2,3,4,5,6,7,8,9,10), .combine=list, .multicombine=TRUE, 
                     .packages=c("dplyr","mice","tidyr","survival"))
             %dopar%{temp<-subset(risk.all,centile==input)
                     return(mean(temp$risk.av))
                     
             })
stopCluster(cl)

predrisk.overall[[1]]
predrisk.overall[[2]]
predrisk.overall[[3]]
predrisk.overall[[4]]
predrisk.overall[[5]]
predrisk.overall[[6]]
predrisk.overall[[7]]
predrisk.overall[[8]]
predrisk.overall[[9]]
predrisk.overall[[10]]

predrisk.all<-unlist(predrisk.overall)

km.all
predrisk.all

## I also want to calculate the mean 10 year predicted risk, and the observed 10 year risk for everybody
## No need to slit by 10th percentiles

## Now calculate the kaplan meier survival for each of these subgroups
temp.dat<-risk.all
                     temp<-survfit(Surv(CVD_time,CVD_cens_R, type='right') ~ 1,
                                   data=temp.dat)
                     # temp$time does not always have a 3653 value, so find a value that it does have
                     numbers<-3653
                     range<-0
                     arbitrary.numbers<-sort(temp.dat$CVD_time)
                     nearest<-findInterval(numbers, arbitrary.numbers - range)
obs.risk<-(1-temp$surv[temp$time==arbitrary.numbers[nearest]])

## Now need to calculate the average predicted risk per group

pred.risk<- mean(temp.dat$risk.av)
 
print("observed and predicted risks are:")
obs.risk
pred.risk

## save image with all the risks calcualted
rm(list=setdiff(ls(),list("km.all","predrisk.all")))
 
setwd("B")
save.image("calibration_female_split.RData")

print("image saved")

# Now make a data frame with everything I want to plot
plot.data<-data.frame(xvals=1:10,km.all,km.2539,km.4059,km.6084,predrisk.all,predrisk.2539,predrisk.4059,predrisk.6084)

library(ggplot2)

## For each plot I need to extract the right data, and reshape it

## All patients
temp.dat<-select(plot.data,c("xvals","km.all","predrisk.all"))
colnames(temp.dat)<-c("xvals","observed","predicted")

# Now reshapre into long format
temp.dat<-melt(temp.dat,id="xvals")

# Now plot
p<-ggplot(temp.dat) 
p+ geom_point(aes(x=xvals,y=value,shape=variable)) + scale_shape_manual(values=c(1,19)) + ggtitle("Female - All patients")
