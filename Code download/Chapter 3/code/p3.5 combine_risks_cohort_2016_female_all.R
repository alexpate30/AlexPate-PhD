## This programs combines the risks for the 2016 cohort from each model

library(dplyr)
rm(list=ls())
setwd("B")

## First load the data from models A to D
load("risks_female_cohort2016.RData")
## Next load the models E and F
load("risks_female_cohort2016_EandF.RData")

## Create a list of the patid's in numerical order, as they are in A/B/C/D
patid.ord<-mean.dat$patid
patid.ord<-sort(patid.ord)

## Assign to the vectors
surv.all.A.loglog$patid<-patid.ord
print("risks A loaded")

surv.all.B.loglog$patid<-patid.ord
print("risks B loaded")

surv.all.C.loglog$patid<-patid.ord
print("risks C loaded")

surv.all.D.loglog$patid<-patid.ord
print("risks D loaded")

surv.all.E.loglog$patid<-patid.ord
print("risks E loaded")

surv.all.F.loglog$patid<-patid.ord
print("risks E loaded")

head(mean.dat)

## Now sort all the datasets by patid
surv.all.A.loglog <- arrange(surv.all.A.loglog,patid)
surv.all.B.loglog <- arrange(surv.all.B.loglog,patid)
surv.all.C.loglog <- arrange(surv.all.C.loglog,patid)
surv.all.D.loglog <- arrange(surv.all.D.loglog,patid)
surv.all.E.loglog <- arrange(surv.all.E.loglog,patid)
surv.all.F.loglog <- arrange(surv.all.F.loglog,patid)


rm(fit_data_parallel.1)
rm(fit_data_parallel.2)

rm(fit_data_parallel.1.coxph)
rm(fit_data_parallel.2.coxph)
rm(surv.probs.1)
rm(surv.probs.2)

rm(region.dat,region.dat.all,surv.all.cloglog,surv.all.loglog)
rm(long_data_parallel)

surv.all.A.loglog<-select(surv.all.A.loglog,c("patid","surv.av","risk"))
surv.all.A.cloglog<-select(surv.all.A.cloglog,c("patid","surv.av","risk"))
surv.all.B.loglog<-select(surv.all.B.loglog,c("patid","surv.av","risk"))
surv.all.B.cloglog<-select(surv.all.B.cloglog,c("patid","surv.av","risk"))
surv.all.C.loglog<-select(surv.all.C.loglog,c("patid","surv.av","risk"))
surv.all.C.cloglog<-select(surv.all.C.cloglog,c("patid","surv.av","risk"))
surv.all.D.loglog<-select(surv.all.D.loglog,c("patid","surv.av","risk"))
surv.all.D.cloglog<-select(surv.all.D.cloglog,c("patid","surv.av","risk"))
surv.all.E.loglog<-select(surv.all.E.loglog,c("patid","surv.av","risk"))
surv.all.E.cloglog<-select(surv.all.E.cloglog,c("patid","surv.av","risk"))
surv.all.F.loglog<-select(surv.all.F.loglog,c("patid","surv.av","risk"))
surv.all.F.cloglog<-select(surv.all.F.cloglog,c("patid","surv.av","risk"))

save.image("risks_female_all_cohort2016.RData")
print('image saved')
