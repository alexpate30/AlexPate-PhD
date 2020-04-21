### SET ROOT DIRECTORY

### SET ROOR DIRECTORY

library(reshape)
library(dplyr)
library(survival)
library(survminer)
library(mfp)

statin.cohort <- read.table("data/statin_users_cohort.csv", sep="," , header=TRUE)

## Need to remove patients whose entry date is prior to age 85
anal.dat <- read.table("data/analysis_dataset_D_comb.csv", sep="," , header=TRUE)
## Only need the age variable, and merging variables
anal.dat <- select(anal.dat,patid,index_date_D_r,age)
anal.dat <- arrange(anal.dat,patid)

## Add a variable called index_date_D_r which will be used for merging
statin.cohort$index_date_D_r <- statin.cohort$stat_init_r

## Creaet new dataset of interest
statin.cohort <- merge(anal.dat,statin.cohort)
statin.cohort <- arrange(statin.cohort,patid)

## Want to 'carry' the first age variable, and remove any entries where it is smaller than 85
# First create the variable to store this value, using age at each statin period
statin.cohort$age.init <- statin.cohort$age

# Now carry it over from the first value for each patient, whenever patient id is the same
for (i in 2:nrow(statin.cohort)){
  if (statin.cohort$patid[i] == statin.cohort$patid[i-1]){
    statin.cohort$age.init[i] <- statin.cohort$age.init[i-1]
  }}

## Now remove entries that start before 85
statin.cohort <- statin.cohort[statin.cohort$age.init < 85, ]

# Want to save a cohort to count number of patients (i.e. just one obs per patient)
statin.cohort.temp.unres <- statin.cohort[!duplicated(statin.cohort$patid), ]

statin.cohort[1:50,c("patid","dtvalid","stat_init","stat_end","index_date")]

## Want to remove all entries where there is not a year follow up before the first statin prescription
## Start by removing any entries where dtvalid + 365 > index_date_D
statin.cohort$dtvalid <- as.numeric(as.Date(statin.cohort$dtvalid, format = "%d/%m/%Y"))
statin.cohort.restricted <- statin.cohort[(statin.cohort$dtvalid + 365) < statin.cohort$index_date_D_r, ]

## However there will now be lots of entries where the initial statin treatment started within a year  of
## dtvalid, so its been removed, but follow up statin initiations are still there
## Need to remove these people, as they started as 'current users'
## The variable index_date represents which number statin treatment it is, can carry the  first index_date
## and see remove any > 1, just like the above code
head(statin.cohort[c("patid","stat_init","stat_end","index_date","index.init")],n=50)


# First create the variable to store this value, using age at each statin period
statin.cohort.restricted$index.init <- statin.cohort.restricted$index_date

# Now carry it over from the first value for each patient, whenever patient id is the same
for (i in 2:nrow(statin.cohort.restricted)){
  if (statin.cohort.restricted$patid[i] == statin.cohort.restricted$patid[i-1]){
    statin.cohort.restricted$index.init[i] <- statin.cohort.restricted$index.init[i-1]
  }}

# Now remove all entries where index init isn't the first statin treatment period
statin.cohort.restricted <- statin.cohort.restricted[statin.cohort.restricted$index.init == 1, ]


### Finally, one more step
### There are patients in this cohort who had statins prior to 1998 or outside of valid follow up period which I removed
### Meaning their 'first statin' here isn't actually their first, and there first happened
### prior to the study start date, so need to remove these patients.
### Any of these patients will have been excluded from the primary prevention cohort,
### as we didn't remove the statins outside of valid follow up period here. So read this in
### and merge to exclude these patients.
prim.prev <- read.table("data/analysis_dataset_A_comb.csv", sep="," , header=TRUE)
prim.prev <- select(prim.prev, patid)
prim.prev <- arrange(prim.prev, patid)

### Create a list of all the patid's in prim.prev
prim.prev.patid <- prim.prev$patid
rm(prim.prev)
#statin.cohort.restricted <- merge(statin.cohort.restricted, prim.prev)


### Only retain patients where they are also part of prim.prev
statin.cohort.restricted <- statin.cohort.restricted[(statin.cohort.restricted$patid %in% prim.prev.patid), ]

# No double check how many entries are left when we just keep the first entry for each patient
statin.cohort.temp.restricted <- statin.cohort.restricted[!duplicated(statin.cohort.restricted$patid), ]

# Want to compare this to the number of patients when we just take first entry, before this extra restriction was applied
nrow(statin.cohort.temp.restricted)
nrow(statin.cohort.temp.unres) 


### FIRST CALCULATE DISCONTINUATION RATE
### FIRST CALCULATE DISCONTINUATION RATE
### FIRST CALCULATE DISCONTINUATION RATE


### Rename the statin cohort to anal.dat.all and select vars
colnames(statin.cohort)
anal.dat.all <- statin.cohort.restricted
anal.dat.all <- select(anal.dat.all,pracid, patid, stat_init_r, stat_end_r, cvd_event, index_date_D_r, index_date, dtvalid_r, dtcens_r, first_cvd_all_r, 
                       gender,
                       age, age.init)

# ## First remove duplicates so we have first statin period for each patient
anal.dat.all.dedup <- anal.dat.all[!duplicated(anal.dat.all$patid),]


## Next create a variable which has the 'end' of the statin treatment, but if this happens after dtcens or first_cvd,
## bring forward the end of the statin treatment to this date
anal.dat.all.dedup$stat_end_cens_r <- pmin(anal.dat.all.dedup$stat_end_r,anal.dat.all.dedup$dtcens_r,anal.dat.all.dedup$first_cvd_all_r)

## Now need to calculate the time until either censored or finish statin
## Add 1 so no zero values (the day on the date of censoring counts as day of follow up)
anal.dat.all.dedup$statin_usage <- anal.dat.all.dedup$stat_end_cens_r - anal.dat.all.dedup$stat_init_r + 1

## Now I need to deduce if a patient is censored or not
anal.dat.all.dedup$is.cens90 <- rep(0,dim(anal.dat.all.dedup)[1])

## We want the patient to have had an event (i.e. a discontinuation) only if they end their treatment prior to being censored
## We will also allow for the fact that the patient may have been censored shortly after their last prescription, with two different limits
anal.dat.all.dedup$is.cens90[anal.dat.all.dedup$stat_end_cens_r + 90 < pmin(anal.dat.all.dedup$dtcens_r,anal.dat.all.dedup$first_cvd_all_r)] <- 1


### Calculate the discontinuation rate
surv.obj.disc <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup)
disc_sum <-surv_summary(surv.obj.disc)



### CALCULATE RESTARTING RATE
### CALCULATE RESTARTING RATE
### CALCULATE RESTARTING RATE

## Extract a list of all patients who not censored during their statin treatment (i.e. actually discontinued so may start again)
head(anal.dat.all.dedup)
pats.cens <- anal.dat.all.dedup$patid[anal.dat.all.dedup$is.cens90 == 1]

anal.dat.notcens <- anal.dat.all[(anal.dat.all$patid %in% pats.cens),]



### Now I have all the patients who were not censored

## Create a variable that indicates which number statin prescription it is
## Create a counter that counts the number of statin prescriptions of each patient
head(anal.dat.notcens)
test <- anal.dat.notcens %>% group_by(patid) %>% mutate(Count = row_number())
test<-data.frame(test)
head(test,n=30)

## Create a dataset that will contain statin initiation dates and statin discontinuation dates
## Reduce to variables of interest
test.init <- test[,c("patid","stat_init_r","Count")]
test.end <- test[,c("patid","stat_end_r","Count")]
head(test.init)

## Use cast to turn this into wide format
test.init.cast <- cast(test.init,patid ~ Count, value = "stat_init_r")
test.end.cast <- cast(test.end,patid ~ Count, value = "stat_end_r")
head(test.init.cast)
head(test.end.cast)


## Also create a data frame with the other things of interest such as date of censoring ,cvd event, etc
anal.dat.notcens.dedup <- anal.dat.notcens[!duplicated(anal.dat.notcens$patid),]
head(anal.dat.notcens.dedup)
anal.dat.notcens.dedup <- anal.dat.notcens.dedup[,c("patid","dtcens_r","first_cvd_all_r","age.init")]
head(anal.dat.notcens.dedup)


## Create the time until first re-continuation and whether it is censored or an actual re-continuation
## Time until an event
restart.event <- test.init.cast[,"2"] - test.end.cast[,"1"]
head(restart.event)


## Turn all the NA's into something really big (i.e. no event/restarting ever happens)
restart.event[is.na(restart.event)] <- 40000
head(restart.event,n=30)

head(test.end.cast)
head(anal.dat.notcens.dedup)

## Calculate the time until censored (note this includes having a cvd event)
restart.cens <- pmin(anal.dat.notcens.dedup$dtcens_r, anal.dat.notcens.dedup$first_cvd_all_r) - test.end.cast[,"1"]

## The restart time is the smaller of the two
restart.time <- pmin(restart.event,restart.cens)

head(restart.event)
head(restart.cens)

## It is censored if the restart time = restart cens or higher
is.cens <- as.numeric(restart.event < restart.cens)

## Restart.dat
## This has got the data for the time until restarting
restart.dat <- data.frame("patid"=anal.dat.notcens.dedup$patid,restart.time,is.cens,"age.init"=anal.dat.notcens.dedup$age.init)

## Final thing to note is that no-one can actually have an event prior to time = 91, given the set up and washout period
## Therefore I should deduct this time period
surv.obj.restart <- survfit(Surv(restart.time,is.cens) ~ 1, data=restart.dat)

summary(restart.cens)
summary(restart.event)

head(restart.dat)

ggsurvplot(surv.obj.restart,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*20), risk.table = TRUE, censor = FALSE)

restart_sum <- surv_summary(surv.obj.restart)
head(restart_sum)

### END OF FIRST RESTARTING RATE
### END OF FIRST RESTARTING RATE
### END OF FIRST RESTARTING RATE


### SECOND DISCONTINUATION RATE
### SECOND DISCONTINUATION RATE
### SECOND DISCONTINUATION RATE

##### OK
##### Now I want to look at how long these restarters actually take their drugs for
##### I am interested in people who haven't been censored
pats.cens2 <- restart.dat$patid[restart.dat$is.cens == 1]
anal.dat.notcens2 <- anal.dat.notcens[(anal.dat.notcens$patid %in% pats.cens2),]
##### We have the rates of restarting


test.init.cast2 <- test.init.cast[(test.init.cast$patid %in% pats.cens2),]
test.end.cast2 <- test.end.cast[(test.end.cast$patid %in% pats.cens2),]
head(test.init.cast2)

## Also create a data frame with the other things of interest such as date of censoring ,cvd event, etc
anal.dat.notcens.dedup2 <- anal.dat.notcens2[!duplicated(anal.dat.notcens2$patid),]
head(anal.dat.notcens.dedup2)
anal.dat.notcens.dedup2 <- anal.dat.notcens.dedup2[,c("patid","dtcens_r","first_cvd_all_r","age.init")]
head(anal.dat.notcens.dedup2)


## Create the time until first re-continuation and whether it is censored or an actual re-continuation
## Time until an event
restart.event2 <- test.end.cast2[,"2"] - test.init.cast2[,"2"]

## Turn all the NA's into something really big (i.e. no event/restarting ever happens)
restart.event2[is.na(restart.event2)] <- 40000


## Calculate the time until censored
restart.cens2 <- pmin(anal.dat.notcens.dedup2$dtcens_r, anal.dat.notcens.dedup2$first_cvd_all_r)  - test.init.cast2[,"2"]

## The restart time is the smaller of the two
restart.time2 <- pmin(restart.event2,restart.cens2)
head(restart.event2)
head(restart.cens2)

## It is an event if the restart time + 90 < restart cens
## This means it is censored if we have not had the full 90 day window in order to look for further prescription
is.cens2 <- as.numeric(restart.event2 + 90 < restart.cens2)

## Restart.dat
restart.dat2 <- data.frame("patid"=anal.dat.notcens.dedup2$patid,restart.time2,is.cens2,"age.init"=anal.dat.notcens.dedup2$age.init)
surv.obj.disc2 <- survfit(Surv(restart.time2,is.cens2) ~ 1, data=restart.dat2)

summary(restart.cens2)
summary(restart.event2)


ggsurvplot(surv.obj.disc2,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*20),censor = FALSE, risk.table = TRUE)

disc2_sum <- surv_summary(surv.obj.disc2)
head(disc2_sum)

disc2_sum[disc2_sum$time %in% round(seq(365.25,5478.75,365.25)),]
### END OF SECOND DISCONTINUATION
### END OF SECOND DISCONTINUATION
### END OF SECOND DISCONTINUATION


### START OF SECOND RESTARTING
### START OF SECOND RESTARTING
### START OF SECOND RESTARTING

## Now I can extract a list of all patients who had an event during their second period of treatment, that is they actually stopped without
## being censored
head(anal.dat.all.dedup)
pats.cens3 <- restart.dat2$patid[restart.dat2$is.cens2 == 1]

## Restrict anal.dat to people within this
anal.dat.notcens3 <- anal.dat.all[(anal.dat.all$patid %in% pats.cens3),]


### Now I have all the patients who were not censored

## Create a variable that indicates which number statin prescription it is
## Create a counter that counts the number of statin prescriptions of each patient
head(anal.dat.notcens3)
test <- anal.dat.notcens3 %>% group_by(patid) %>% mutate(Count = row_number())
test<-data.frame(test)
head(test,n=30)

## Create a dataset that will contain statin initiation dates and statin discontinuation dates
## Reduce to variables of interest
test.init3 <- test[,c("patid","stat_init_r","Count")]
test.end3 <- test[,c("patid","stat_end_r","Count")]
head(test.init3)

## Use cast to turn this into wide format
test.init.cast3 <- cast(test.init3,patid ~ Count, value = "stat_init_r")
test.end.cast3 <- cast(test.end3,patid ~ Count, value = "stat_end_r")
head(test.init.cast3)
head(test.end.cast3)


## Also create a data frame with the other things of interest such as date of censoring ,cvd event, etc
anal.dat.notcens3.dedup <- anal.dat.notcens3[!duplicated(anal.dat.notcens3$patid),]
head(anal.dat.notcens3.dedup)
anal.dat.notcens3.dedup <- anal.dat.notcens3.dedup[,c("patid","dtcens_r","first_cvd_all_r","age.init")]
head(anal.dat.notcens3.dedup)


## Create the time until second re-continuation and whether it is censored or an actual re-continuation
## Time until an event
head(test.init.cast)

restart.event3 <- test.init.cast3[,"3"] - test.end.cast3[,"2"]

## Turn all the NA's into something really big (i.e. no event/restarting ever happens)
restart.event3[is.na(restart.event3)] <- 40000
head(restart.event3,n=30)

head(test.end.cast3)
head(anal.dat.notcens3.dedup)

## Calculate the time until censored
restart.cens3 <- pmin(anal.dat.notcens3.dedup$dtcens_r, anal.dat.notcens3.dedup$first_cvd_all_r) - test.end.cast3[,"2"]

## The restart time is the smaller of the two
restart.time3 <- pmin(restart.event3,restart.cens3)
head(restart.event3)
head(restart.cens3)

## It is censored if the restart time = restart cens or higher
is.cens3 <- as.numeric(restart.event3 < restart.cens3)

## Restart.dat
restart.dat3 <- data.frame("patid"=anal.dat.notcens3.dedup$patid,restart.time3,is.cens3,"age.init"=anal.dat.notcens3.dedup$age.init)

## Final thing to note is that no-one can actually have an event prior to time = 91, given the set up and washout period
## Therefore I should deduct this time period
surv.obj.restart2 <- survfit(Surv(restart.time3,is.cens3) ~ 1, data=restart.dat3)

summary(restart.cens3)
summary(restart.event3)

ggsurvplot(surv.obj.restart2,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*20), risk.table = TRUE, censor = FALSE)

restart2_sum <- surv_summary(surv.obj.restart2)
head(restart2_sum)


### END OF SECOND RESTARTING
### END OF SECOND RESTARTING
### END OF SECOND RESTARTING


### NOW IM GOING TO CONTINUE AND DO THE THIRD DISCONTINUATION AND THIRD RESTARTING
### NOW IM GOING TO CONTINUE AND DO THE THIRD DISCONTINUATION AND THIRD RESTARTING

### THIRD DISCONTINUATION
### THIRD DISCONTINUATION 
### THIRD DISCONTINUATION 

##### OK
##### Now I want to look at how long these restarters actually take their drugs for
##### I am interested in people who haven't been censored
str(restart.dat3)
table(restart.dat3$is.cens3)
### This is saying that two thirds of the patients that quit actually do restart

### Think anal.dat.notcens3 should contain the patient information I want??
str(anal.dat.notcens3)
head(anal.dat.notcens3)
## Yes it contains all the episodes

pats.cens4 <- restart.dat3$patid[restart.dat3$is.cens == 1]
anal.dat.notcens4 <- anal.dat.notcens3[(anal.dat.notcens3$patid %in% pats.cens4),]
##### We have the rates of restarting


head(test.init.cast3)

test.init.cast4 <- test.init.cast3[(test.init.cast3$patid %in% pats.cens4),]
test.end.cast4 <- test.end.cast3[(test.end.cast3$patid %in% pats.cens4),]
head(test.init.cast3)
head(test.end.cast3)

## This next group we are looking at patients who reststarting for the second time, therefore they all have at least three 
## statin treatment periods
head(test.init.cast4)
head(test.end.cast4)

## Also create a data frame with the other things of interest such as date of censoring ,cvd event, etc
anal.dat.notcens.dedup4 <- anal.dat.notcens4[!duplicated(anal.dat.notcens4$patid),]
head(anal.dat.notcens.dedup4)
anal.dat.notcens.dedup4 <- anal.dat.notcens.dedup4[,c("patid","dtcens_r","first_cvd_all_r","age.init")]
head(anal.dat.notcens.dedup4)


## Create the time until the next discontinuation
## It is siply the difference between statin start and statin end
restart.event4 <- test.end.cast4[,"3"] - test.init.cast4[,"3"]

## Turn all the NA's into something really big (i.e. no event/restarting ever happens)
restart.event4[is.na(restart.event4)] <- 40000
head(restart.event4,n=30)


## Calculate the time until censored
restart.cens4 <- pmin(anal.dat.notcens.dedup4$dtcens_r, anal.dat.notcens.dedup4$first_cvd_all_r)  - test.init.cast4[,"3"]

## The restart time is the smaller of the two
restart.time4 <- pmin(restart.event4,restart.cens4)
head(restart.event2)
head(restart.cens2)

## It is an event if the restart time + 90 < restart cens
## This means it is censored if we have not had the full 90 day window in order to look for further prescription
is.cens4 <- as.numeric(restart.event4 + 90 < restart.cens4)

## Restart.dat
restart.dat4 <- data.frame("patid"=anal.dat.notcens.dedup4$patid,restart.time4,is.cens4,"age.init"=anal.dat.notcens.dedup4$age.init)
surv.obj.disc3 <- survfit(Surv(restart.time4,is.cens4) ~ 1, data=restart.dat4)

summary(restart.cens4)
summary(restart.event4)

ggsurvplot(surv.obj.disc3,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*20),censor = FALSE, risk.table = TRUE)

disc3_sum <- surv_summary(surv.obj.disc3)

disc3_sum[disc3_sum$time %in% round(seq(365.25,5478.75,365.25)),]


### END OF THIRD DISCONTINUATION
### END OF THIRD DISCONTINUATION
### END OF THIRD DISCONTINUATION

### THIRD RESTARTING
### THIRD RESTARTING
### THIRD RESTARTING


## Now I can extract a list of all patients who had an event during their second period of treatment, that is they actually stopped without
## being censored
pats.cens5 <- restart.dat4$patid[restart.dat4$is.cens4 == 1]

## Restrict anal.dat to people within this
anal.dat.notcens5 <- anal.dat.all[(anal.dat.all$patid %in% pats.cens5),]


### Now I have all the patients who were not censored

## Create a variable that indicates which number statin prescription it is
## Create a counter that counts the number of statin prescriptions of each patient
head(anal.dat.notcens5)
test <- anal.dat.notcens5 %>% group_by(patid) %>% mutate(Count = row_number())
test<-data.frame(test)
head(test,n=30)

## Create a dataset that will contain statin initiation dates and statin discontinuation dates
## Reduce to variables of interest
test.init5 <- test[,c("patid","stat_init_r","Count")]
test.end5 <- test[,c("patid","stat_end_r","Count")]
head(test.init5)

## Use cast to turn this into wide format
test.init.cast5 <- cast(test.init5,patid ~ Count, value = "stat_init_r")
test.end.cast5 <- cast(test.end5,patid ~ Count, value = "stat_end_r")
head(test.init.cast5)
head(test.end.cast5)


## Also create a data frame with the other things of interest such as date of censoring ,cvd event, etc
anal.dat.notcens5.dedup <- anal.dat.notcens5[!duplicated(anal.dat.notcens5$patid),]
head(anal.dat.notcens5.dedup)
anal.dat.notcens5.dedup <- anal.dat.notcens5.dedup[,c("patid","dtcens_r","first_cvd_all_r","age.init")]
head(anal.dat.notcens5.dedup)


## Create the time until second re-continuation and whether it is censored or an actual re-continuation
## Time until an event
head(test.init.cast)

restart.event5 <- test.init.cast5[,"4"] - test.end.cast5[,"3"]
summary(restart.event5)

## Turn all the NA's into something really big (i.e. no event/restarting ever happens)
restart.event5[is.na(restart.event5)] <- 40000
head(restart.event5,n=30)

head(test.end.cast5)
head(anal.dat.notcens5.dedup)

## Calculate the time until censored
restart.cens5 <- pmin(anal.dat.notcens5.dedup$dtcens_r, anal.dat.notcens5.dedup$first_cvd_all_r) - test.end.cast5[,"3"]

## The restart time is the smaller of the two
restart.time5 <- pmin(restart.event5,restart.cens5)
head(restart.event5)
head(restart.cens5)

## It is censored if the restart time = restart cens
is.cens5 <- as.numeric(restart.event5 < restart.cens5)

## Restart.dat
restart.dat5 <- data.frame("patid"=anal.dat.notcens5.dedup$patid,restart.time5,is.cens5)

## Final thing to note is that no-one can actually have an event prior to time = 91, given the set up and washout period
## Therefore I should deduct this time period
surv.obj.restart3<- survfit(Surv(restart.time5,is.cens5) ~ 1, data=restart.dat5)

summary(restart.cens5)
summary(restart.event5)

### END OFTHIRD RESTARTING
### END OFTHIRD RESTARTING
### END OFTHIRD RESTARTING
ggsurvplot(surv.obj.restart3,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*20), risk.table = TRUE, censor = FALSE)

restart3_sum <- surv_summary(surv.obj.restart3)
head(restart3_sum)



### GOING TO DO FOURTH DISCONTINUATION
### GOING TO DO FOURTH DISCONTINUATION
### GOING TO DO FOURTH DISCONTINUATION

pats.cens6 <- restart.dat5$patid[restart.dat5$is.cens == 1]
anal.dat.notcens6 <- anal.dat.notcens5[(anal.dat.notcens5$patid %in% pats.cens6),]
##### We have the rates of restarting


head(test.init.cast3)

test.init.cast6 <- test.init.cast5[(test.init.cast5$patid %in% pats.cens6),]
test.end.cast6 <- test.end.cast5[(test.end.cast5$patid %in% pats.cens6),]
head(test.init.cast5)
head(test.end.cast5)

## This next group we are looking at patients who reststarting for the second time, therefore they all have at least three 
## statin treatment periods
head(test.init.cast6)
head(test.end.cast6)

## Also create a data frame with the other things of interest such as date of censoring ,cvd event, etc
anal.dat.notcens.dedup6 <- anal.dat.notcens6[!duplicated(anal.dat.notcens6$patid),]
head(anal.dat.notcens.dedup6)
anal.dat.notcens.dedup6 <- anal.dat.notcens.dedup6[,c("patid","dtcens_r","first_cvd_all_r","age.init")]
head(anal.dat.notcens.dedup6)


## Create the time until the next discontinuation
## It is siply the difference between statin start and statin end
restart.event6 <- test.end.cast6[,"3"] - test.init.cast6[,"3"]

## Turn all the NA's into something really big (i.e. no event/restarting ever happens)
restart.event6[is.na(restart.event6)] <- 40000
head(restart.event6,n=30)


## Calculate the time until censored
restart.cens6 <- pmin(anal.dat.notcens.dedup6$dtcens_r, anal.dat.notcens.dedup6$first_cvd_all_r)  - test.init.cast6[,"3"]

## The restart time is the smaller of the two
restart.time6 <- pmin(restart.event6,restart.cens6)


## It is an event if the restart time + 90 < restart cens
## This means it is censored if we have not had the full 90 day window in order to look for further prescription
is.cens6 <- as.numeric(restart.event6 + 90 < restart.cens6)

## Restart.dat
restart.dat6 <- data.frame("patid"=anal.dat.notcens.dedup6$patid,restart.time6,is.cens6)
surv.obj.disc4 <- survfit(Surv(restart.time6,is.cens6) ~ 1, data=restart.dat6)

summary(restart.cens6)
summary(restart.event6)

ggsurvplot(surv.obj.disc4,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*20),censor = FALSE, risk.table = TRUE)

disc4_sum <- surv_summary(surv.obj.disc4)

disc4_sum[disc4_sum$time %in% round(seq(365.25,5478.75,365.25)),]



### Summarise

disc_sum[disc_sum$time %in% round(seq(365.25,5478.75,365.25)),]
restart_sum[restart_sum$time %in% round(seq(365.25,5478.75,365.25)),]
disc2_sum[disc2_sum$time %in% round(seq(365.25,5478.75,365.25)),]
restart2_sum[restart2_sum$time %in% round(seq(365.25,5478.75,365.25)),]
disc3_sum[disc2_sum$time %in% round(seq(365.25,5478.75,365.25)),]
restart3_sum[restart3_sum$time %in% round(seq(365.25,5478.75,365.25)),]


# a <- ggsurvplot(surv.obj.disc,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*10), risk.table = TRUE, cumevents = TRUE, cumcensor = TRUE, censor = FALSE, title = "Initial Discontinuation", risk.table.height = 0.15, cumevents.height = 0.15, cumcensor.height= 0.15)
# ggsave("Disc1.jpg",print(a))
# 
# b <- ggsurvplot(surv.obj.restart,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*10), risk.table = TRUE, cumevents = TRUE, cumcensor = TRUE,  censor = FALSE, title = "Initial Restarting", risk.table.height = 0.15, cumevents.height = 0.15, cumcensor.height= 0.15)
# ggsave("Restart1.jpg",print(b))
# 
# c <- ggsurvplot(surv.obj.disc2,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*10), risk.table = TRUE, cumevents = TRUE, cumcensor = TRUE,  censor = FALSE, title = "Second Discontinuation", risk.table.height = 0.15, cumevents.height = 0.15, cumcensor.height= 0.15)
# ggsave("Disc2.jpg",print(c))
# 
# d <- ggsurvplot(surv.obj.restart2,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*10), risk.table = TRUE, cumevents = TRUE, cumcensor = TRUE,  censor = FALSE, title = "Second Restarting", risk.table.height = 0.15, cumevents.height = 0.15, cumcensor.height= 0.15)
# ggsave("Restart2.jpg",print(d))
# 
# e <- ggsurvplot(surv.obj.disc3,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*10), risk.table = TRUE, cumevents = TRUE, cumcensor = TRUE,  censor = FALSE, title = "Third Discontinuation", risk.table.height = 0.15, cumevents.height = 0.15, cumcensor.height= 0.15)
# ggsave("Disc3.jpg",print(e))
# 
# f <- ggsurvplot(surv.obj.restart3,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*10), risk.table = TRUE, cumevents = TRUE, cumcensor = TRUE,  censor = FALSE, title = "Third Restarting", risk.table.height = 0.15, cumevents.height = 0.15, cumcensor.height= 0.15)
# ggsave("Restart3.jpg",print(f))
# 
# g <- ggsurvplot(surv.obj.disc4,xscale ="d_y",break.x.by = 2.5*365.25,xlim=c(0,365.25*20), risk.table = TRUE, cumevents = TRUE, 
#                 cumcensor = TRUE,  censor = FALSE, title = "Fourth Discontinuation", risk.table.height = 0.15, cumevents.height = 0.15, cumcensor.height= 0.15)
# ggsave("Disc4.jpg",print(g))



#### Summary of datasets used to derive the discontinuation and restarting rates
#### And calculation of survival curves with age as an effect modifier

## Disc 1
# Dataset: anal.dat.all.dedup
# Timevar: statin_usage
# censvar: is.cens90

## Check for fractional polynomials of the age at first initiation date
disc1_frac_age <- mfp(Surv(statin_usage,is.cens90, type='right') ~ fp(age.init), 
                      family=cox, data=anal.dat.all.dedup,
                      keep = c("age.init"))

disc1_frac_age

## Derive these frac polys in the analysis dataset
anal.dat.all.dedup$age.init.frac1 <- (anal.dat.all.dedup$age.init/100)^3
anal.dat.all.dedup$age.init.frac2 <- ((anal.dat.all.dedup$age.init/100)^3)*(log(anal.dat.all.dedup$age.init/100))

## Fit the cox model
disc1.cox <- coxph(Surv(statin_usage,is.cens90, type='right') ~ age.init.frac1 + age.init.frac2, data = anal.dat.all.dedup)

## Create seperate survival curves for age = 40 -> 89
# Create empty vector
disc1.survival.curves <- vector("list",50)

# Define function to create a dataframe of newdata, to generate the survival curbe for, for a given age
disc1_create_fracs <- function(x){
  return(data.frame("age.init.frac1" = (x/100)^3, "age.init.frac2" = ((x/100)^3)*log(x/100)))
}

# Create the survival curves
for (i in 1: 50){disc1.survival.curves[[i]] <- survfit(disc1.cox,newdata = disc1_create_fracs(i+39))}

plot(disc1.survival.curves[[1]]$time,disc1.survival.curves[[1]]$surv, type = "l", lwd = 1)
lines(disc1.survival.curves[[11]]$time,disc1.survival.curves[[11]]$surv, type = "l", lwd = 1, col="red")
lines(disc1.survival.curves[[21]]$time,disc1.survival.curves[[21]]$surv, type = "l", lwd = 1, col="blue")
lines(disc1.survival.curves[[31]]$time,disc1.survival.curves[[31]]$surv, type = "l", lwd = 1, col="green")
lines(disc1.survival.curves[[41]]$time,disc1.survival.curves[[41]]$surv, type = "l", lwd = 1, col="purple")


# Define function to create a data
# Dataset: restart.dat
# Timevar: restart.time
# censvar: is.cens

## Check for fractional polynomials of the age at first initiation date
restart1_frac_age <- mfp(Surv(restart.time,is.cens, type='right') ~ fp(age.init), 
                         family=cox, data=restart.dat,
                         keep = c("age.init"))

restart1_frac_age

## Derive these frac polys in the analysis dataset
restart.dat$age.init.frac1 <- (restart.dat$age.init/100)^2
restart.dat$age.init.frac2 <- ((restart.dat$age.init/100)^3)

## Fit the cox model
restart1.cox <- coxph(Surv(restart.time,is.cens, type='right') ~ age.init.frac1 + age.init.frac2, data = restart.dat)

## Create seperate survival curves for age = 40 -> 89
# Create empty vector
restart1.survival.curves <- vector("list",50)

# Define function to create a dataframe of newdata, to generate the survival curbe for, for a given age
restart1_create_fracs <- function(x){
  return(data.frame("age.init.frac1" = (x/100)^2, "age.init.frac2" = (x/100)^3))
}

# Create the survival curves
for (i in 1: 50){restart1.survival.curves[[i]] <- survfit(restart1.cox,newdata = restart1_create_fracs(i+39))}


plot(restart1.survival.curves[[1]]$time,restart1.survival.curves[[1]]$surv, type = "l", lwd = 1)
lines(restart1.survival.curves[[11]]$time,restart1.survival.curves[[11]]$surv, type = "l", lwd = 1, col="red")
lines(restart1.survival.curves[[21]]$time,restart1.survival.curves[[21]]$surv, type = "l", lwd = 1, col="blue")
lines(restart1.survival.curves[[31]]$time,restart1.survival.curves[[31]]$surv, type = "l", lwd = 1, col="green")
lines(restart1.survival.curves[[41]]$time,restart1.survival.curves[[41]]$surv, type = "l", lwd = 1, col="purple")


## Disc 2
# Dataset: restart.dat2
# Timevar: restart.time2
# censvar: is.cens2

## Check for fractional polynomials of the age at first initiation date
disc2_frac_age <- mfp(Surv(restart.time2,is.cens2, type='right') ~ fp(age.init), 
                      family=cox, data=restart.dat2,
                      keep = c("age.init"))

disc2_frac_age

## Derive these frac polys in the analysis dataset
restart.dat2$age.init.frac1 <- (restart.dat2$age.init/100)^2
restart.dat2$age.init.frac2 <- ((restart.dat2$age.init/100)^2)*(log(restart.dat2$age.init/100))

## Fit the cox model
disc2.cox <- coxph(Surv(restart.time2,is.cens2, type='right') ~ age.init.frac1 + age.init.frac2, data = restart.dat2)

## Create seperate survival curves for age = 40 -> 89
# Create empty vector
disc2.survival.curves <- vector("list",50)

# Define function to create a dataframe of newdata, to generate the survival curbe for, for a given age
disc2_create_fracs <- function(x){
  return(data.frame("age.init.frac1" = (x/100)^2, "age.init.frac2" = ((x/100)^2)*log(x/100)))
}

# Create the survival curves
for (i in 1: 50){disc2.survival.curves[[i]] <- survfit(disc2.cox,newdata = disc2_create_fracs(i+39))}


plot(disc2.survival.curves[[1]]$time,disc2.survival.curves[[1]]$surv, type = "l", lwd = 1)
lines(disc2.survival.curves[[11]]$time,disc2.survival.curves[[11]]$surv, type = "l", lwd = 1, col="red")
lines(disc2.survival.curves[[21]]$time,disc2.survival.curves[[21]]$surv, type = "l", lwd = 1, col="blue")
lines(disc2.survival.curves[[31]]$time,disc2.survival.curves[[31]]$surv, type = "l", lwd = 1, col="green")
lines(disc2.survival.curves[[41]]$time,disc2.survival.curves[[41]]$surv, type = "l", lwd = 1, col="purple")

## Restart 2
# Dataset: restart.dat3
# Timevar: restart.time3
# censvar: is.cens3

## Check for fractional polynomials of the age at first initiation date
restart2_frac_age <- mfp(Surv(restart.time3,is.cens3, type='right') ~ fp(age.init), 
                         family=cox, data=restart.dat3,
                         keep = c("age.init"))

restart2_frac_age

## Derive these frac polys in the analysis dataset
restart.dat3$age.init.frac1 <- (restart.dat3$age.init/100)^0.5
restart.dat3$age.init.frac2 <- ((restart.dat3$age.init/100)^2)

## Fit the cox model
restart2.cox <- coxph(Surv(restart.time3,is.cens3, type='right') ~ age.init.frac1 + age.init.frac2, data = restart.dat3)

## Create seperate survival curves for age = 40 -> 89
# Create empty vector
restart2.survival.curves <- vector("list",50)

# Define function to create a dataframe of newdata, to generate the survival curbe for, for a given age
restart2_create_fracs <- function(x){
  return(data.frame("age.init.frac1" = (x/100)^0.5, "age.init.frac2" = (x/100)^2))
}

# Create the survival curves
for (i in 1: 50){restart2.survival.curves[[i]] <- survfit(restart2.cox,newdata = restart2_create_fracs(i+39))}


plot(restart2.survival.curves[[1]]$time,restart2.survival.curves[[1]]$surv, type = "l", lwd = 1)
lines(restart2.survival.curves[[11]]$time,restart2.survival.curves[[11]]$surv, type = "l", lwd = 1, col="red")
lines(restart2.survival.curves[[21]]$time,restart2.survival.curves[[21]]$surv, type = "l", lwd = 1, col="blue")
lines(restart2.survival.curves[[31]]$time,restart2.survival.curves[[31]]$surv, type = "l", lwd = 1, col="green")
lines(restart2.survival.curves[[41]]$time,restart2.survival.curves[[41]]$surv, type = "l", lwd = 1, col="purple")

## Disc 3
# Dataset: restart.dat4
# Timevar: restart.time4
# censvar: is.cens4

## Check for fractional polynomials of the age at first initiation date
disc3_frac_age <- mfp(Surv(restart.time4,is.cens4, type='right') ~ fp(age.init), 
                      family=cox, data=restart.dat4,
                      keep = c("age.init"))

disc3_frac_age

## Derive these frac polys in the analysis dataset
restart.dat4$age.init.frac1 <- (restart.dat4$age.init/100)
restart.dat4$age.init.frac2 <- (restart.dat4$age.init/100)*log((restart.dat4$age.init/100))

## Fit the cox model
disc3.cox <- coxph(Surv(restart.time4,is.cens4, type='right') ~ age.init.frac1 + age.init.frac2, data = restart.dat4)

## Create seperate survival curves for age = 40 -> 89
# Create empty vector
disc3.survival.curves <- vector("list",50)

# Define function to create a dataframe of newdata, to generate the survival curbe for, for a given age
disc3_create_fracs <- function(x){
  return(data.frame("age.init.frac1" = (x/100), "age.init.frac2" = (x/100)*log(x/100)))
}

# Create the survival curves
for (i in 1: 50){disc3.survival.curves[[i]] <- survfit(disc3.cox,newdata = disc3_create_fracs(i+39))}


plot(disc3.survival.curves[[1]]$time,disc3.survival.curves[[1]]$surv, type = "l", lwd = 1)
lines(disc3.survival.curves[[11]]$time,disc3.survival.curves[[11]]$surv, type = "l", lwd = 1, col="red")
lines(disc3.survival.curves[[21]]$time,disc3.survival.curves[[21]]$surv, type = "l", lwd = 1, col="blue")
lines(disc3.survival.curves[[31]]$time,disc3.survival.curves[[31]]$surv, type = "l", lwd = 1, col="green")
lines(disc3.survival.curves[[41]]$time,disc3.survival.curves[[41]]$surv, type = "l", lwd = 1, col="purple")

### Save both the original survival object (not age modified) and the lists with the age modified survival curves

rm(list=setdiff(ls(),list("disc1.survival.curves","disc2.survival.curves","disc3.survival.curves",
                          "restart1.survival.curves","restart2.survival.curves",
                          "surv.obj.disc","surv.obj.disc2","surv.obj.disc3","surv.obj.restart","surv.obj.restart2")))


save.image("R_out_C6/survival_curves_1year_restricted_agemod_FAKE.RData")