### This program will combine the risks from models A to F into one RData file
rm(list=ls())

## Load required packages
library(dplyr)

## Start by loading the imputed datasets
## This is purely so we can get the list of patids from long_data_parallel
setwd("A")
load("imputed_datasets_loaded_male.RData")

## Order long data parallel
long_data_parallel[[1]]<-arrange(long_data_parallel[[1]],patid)

## Create the random rows vector
set.seed(100)
random.rows <- sample(1:dim(long_data_parallel[[i]])[1],200000,replace = F)

## Extract the same set of rows
## The patids from each row will correspond to the risks in our output datasets
long_data_parallel_test <- long_data_parallel[[1]][random.rows,]

setwd("B")
load("risks_male_modelA.RData")
surv.all.A.loglog$patid<-long_data_parallel_test$patid
surv.all.A.cloglog$patid<-long_data_parallel_test$patid
print("risks A loaded")
load("risks_male_modelB.RData")
surv.all.B.loglog$patid<-long_data_parallel_test$patid
surv.all.B.cloglog$patid<-long_data_parallel_test$patid
print("risks B loaded")
load("risks_male_modelC.RData")
surv.all.C.loglog$patid<-long_data_parallel_test$patid
surv.all.C.cloglog$patid<-long_data_parallel_test$patid
print("risks C loaded")
load("risks_male_modelD.RData")
surv.all.D.loglog$patid<-long_data_parallel_test$patid
surv.all.D.cloglog$patid<-long_data_parallel_test$patid
print("risks D loaded")
load("risks_male_modelE.RData")
surv.all.E.loglog$patid<-long_data_parallel_test$patid
surv.all.E.cloglog$patid<-long_data_parallel_test$patid
print("risks E loaded")
load("risks_male_modelF.RData")
surv.all.F.loglog$patid<-long_data_parallel_test$patid
surv.all.F.cloglog$patid<-long_data_parallel_test$patid
print("risks F loaded")


## Now sort all the datasets by patid and make suer only have relevant variables
surv.all.A.loglog <- arrange(surv.all.A.loglog,patid)
surv.all.B.loglog <- arrange(surv.all.B.loglog,patid)
surv.all.C.loglog <- arrange(surv.all.C.loglog,patid)
surv.all.D.loglog <- arrange(surv.all.D.loglog,patid)
surv.all.E.loglog <- arrange(surv.all.E.loglog,patid)
surv.all.F.loglog <- arrange(surv.all.F.loglog,patid)

surv.all.A.loglog<-select(surv.all.A.loglog,c("patid","surv.av","risk"))
surv.all.B.loglog<-select(surv.all.B.loglog,c("patid","surv.av","risk"))
surv.all.C.loglog<-select(surv.all.C.loglog,c("patid","surv.av","risk"))
surv.all.D.loglog<-select(surv.all.D.loglog,c("patid","surv.av","risk"))
surv.all.E.loglog<-select(surv.all.E.loglog,c("patid","surv.av","risk"))
surv.all.F.loglog<-select(surv.all.F.loglog,c("patid","surv.av","risk"))

rm(list=setdiff(ls(),list("surv.all.A.loglog","surv.all.B.loglog","surv.all.C.loglog",
                          "surv.all.D.loglog","surv.all.E.loglog","surv.all.F.loglog","random.rows")))

save.image("risks_male_all.RData")
print('image saved')

