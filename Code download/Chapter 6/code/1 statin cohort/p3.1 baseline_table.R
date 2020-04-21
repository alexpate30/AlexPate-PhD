### SET ROOT DIRECTORY

library(dplyr)

statin.cohort <- read.table("data/statin_users_cohort.csv", sep="," , header=TRUE)

## All the variables are stored in anal.dat, but anal.dat also contains statin treatment periods after cvd events
## So will neeed to merge it with the statin users cohort, statin.cohort

## Load anal.dat
anal.dat <- read.table("data/analysis_dataset_D_comb.csv", sep="," , header=TRUE)
anal.dat <- arrange(anal.dat,patid)

## Add a variable called index_date_D_r which will be used for merging (note stat_init_r = index_date_r)
statin.cohort$index_date_D_r <- statin.cohort$stat_init_r

## Create new dataset of interest
statin.cohort <- merge(anal.dat,statin.cohort)
statin.cohort <- arrange(statin.cohort,patid,index_date_D_r)

## Want to remove duplicate entries and those that were over age of 85
statin.cohort <- statin.cohort[!duplicated(statin.cohort$patid), ]
statin.cohort <- statin.cohort[statin.cohort$age < 85, ]

## Also want to remove patients without a year prior follow up
statin.cohort$dtvalid <- as.numeric(as.Date(statin.cohort$dtvalid, format = "%d/%m/%Y"))
statin.cohort <- statin.cohort[(statin.cohort$dtvalid + 365) < statin.cohort$index_date_D_r, ]

## There are also some patients, whose first statin has already been removed from the statin cohort (before reading it in)
## These are patients whose first statin was prior to age 25. W e want to remove these patients
## The 'index_date' variable indicates which number treatment period an observation is. If this != 1, this means the 
## patients first entry has been removed for being underage, so we want to excldue this patient
statin.cohort <- statin.cohort[statin.cohort$index_date == 1, ]

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
statin.cohort <- statin.cohort[(statin.cohort$patid %in% prim.prev.patid), ]


###### Rename cohort to cohort.for.summary #########
cohort.for.summary <- statin.cohort

## Assign correct levels for ethnicity variable
levels(cohort.for.summary$Ethnicity) <- c("Other Asian","Bangladesh","Black","Chinese","Indian","Missing","Mixed","Other Asian","Other","Pakistan","Pakistan","Missing","White")

cohort.for.summary$Ethnicity[cohort.for.summary$Ethnicity == "Missing"] <- NA

statin.cohort.locf.both.women <- cohort.for.summary[cohort.for.summary$gender == 1,]
statin.cohort.locf.both.men <- cohort.for.summary[cohort.for.summary$gender == 0,]
N.women <- nrow(statin.cohort.locf.both.women)
N.men <- nrow(statin.cohort.locf.both.men)

# str(cohort.for.summary)
# head(df.sum.women)

## DO THE WOMEN FIRST
## DO THE WOMEN FIRST
df.sum.women <- statin.cohort.locf.both.women %>%
  select(age,SBP,SBP_std,BMI,Cholesterol_HDL_Ratio,Atrialfib, CKD345,Famhis_lstrict, Hypertension, RA, Smoking, T1dia, T2dia, Townsend, Ethnicity)


df.sum.cont.women.mean <- summarise(df.sum.women, Age = mean(age,na.rm = TRUE), SBP = mean(SBP,na.rm = TRUE), SBP_std = mean(SBP_std, na.rm = TRUE), BMI = mean(BMI,na.rm = TRUE), Cholesterol_HDL_Ratio = mean(Cholesterol_HDL_Ratio,na.rm = TRUE))
df.sum.cont.women.sd <- summarise(df.sum.women, Age = sd(age,na.rm = TRUE), SBP = sd(SBP,na.rm = TRUE), SBP_std = sd(SBP_std, na.rm = TRUE), BMI = sd(BMI,na.rm = TRUE), Cholesterol_HDL_Ratio = sd(Cholesterol_HDL_Ratio,na.rm = TRUE))

## Do the categorical variables seperately
df.sum.bin.women <- c(100*prop.table(table(df.sum.women$Atrialfib))[2],
                          100*prop.table(table(df.sum.women$CKD345))[2], 
                          100*prop.table(table(df.sum.women$Famhis_lstrict))[2],
                          100*prop.table(table(df.sum.women$Hypertension))[2],
                          100*prop.table(table(df.sum.women$RA))[2],
                          100*prop.table(table(df.sum.women$T1dia))[2],
                          100*prop.table(table(df.sum.women$T2dia))[2])

names(df.sum.bin.women) <- c("Atrialfib", "CKD345","Famhis_lstrict", "Hypertension", "RA", "T1dia", "T2dia")

## Probably easier just to do these seperately...

# 100*prop.table(table(df.sum.women$Smoking))
# 100*prop.table(table(df.sum.women$Townsend))

## Combine into one dataset with the other categorical variables
df.sum.cat.women <- data.frame("Var" = c("Atrialfib", "CKD345","Famhis_lstrict", "Hypertension", "RA", "T1dia", "T2dia","Never","Ex","Current",
                                         "1","2","3","4","5","Other Asian","Bangladesh","Black","Chinese","Indian","Missing",
                                         "Mixed","Other","Pakistan","White"),
                               "Prop" = c(df.sum.bin.women,100*prop.table(table(df.sum.women$Smoking)),
                                          100*prop.table(table(df.sum.women$Townsend)), 100*prop.table(table(df.sum.women$Ethnicity))))


## DO THE MEN NEXT
## DO THE MEN NEXT
df.sum.men <- statin.cohort.locf.both.men %>%
  select(age,SBP,SBP_std,BMI,Cholesterol_HDL_Ratio,Atrialfib, CKD345,Famhis_lstrict, Hypertension, RA, Smoking, T1dia, T2dia, Townsend, Ethnicity)

df.sum.cont.men.mean <- summarise(df.sum.men, Age = mean(age,na.rm = TRUE), SBP = mean(SBP,na.rm = TRUE), SBP_std = mean(SBP_std, na.rm = TRUE), BMI = mean(BMI,na.rm = TRUE), Cholesterol_HDL_Ratio = mean(Cholesterol_HDL_Ratio,na.rm = TRUE))
df.sum.cont.men.sd <- summarise(df.sum.men, Age = sd(age,na.rm = TRUE), SBP = sd(SBP,na.rm = TRUE),  SBP_std = sd(SBP_std, na.rm = TRUE), BMI = sd(BMI,na.rm = TRUE), Cholesterol_HDL_Ratio = sd(Cholesterol_HDL_Ratio,na.rm = TRUE))

## Do the categorical variables seperately
df.sum.bin.men <- c(100*prop.table(table(df.sum.men$Atrialfib))[2],
                      100*prop.table(table(df.sum.men$CKD345))[2], 
                      100*prop.table(table(df.sum.men$Famhis_lstrict))[2],
                      100*prop.table(table(df.sum.men$Hypertension))[2],
                      100*prop.table(table(df.sum.men$RA))[2],
                      100*prop.table(table(df.sum.men$T1dia))[2],
                      100*prop.table(table(df.sum.men$T2dia))[2])

names(df.sum.bin.men) <- c("Atrialfib", "CKD345","Famhis_lstrict", "Hypertension", "RA", "T1dia", "T2dia")

## Probably easier just to do these seperately...

# 100*prop.table(table(df.sum.men$Smoking))
# 100*prop.table(table(df.sum.men$Townsend))

## Combine into one dataset with the other categorical variables
df.sum.cat.men <- data.frame("Var" = c("Atrialfib", "CKD345","Famhis_lstrict", "Hypertension", "RA", "T1dia", "T2dia","Never","Ex","Current",
                                         "1","2","3","4","5","Other Asian","Bangladesh","Black","Chinese","Indian","Missing",
                                         "Mixed","Other","Pakistan","White"),
                               "Prop" = c(df.sum.bin.men,100*prop.table(table(df.sum.men$Smoking)),
                                          100*prop.table(table(df.sum.men$Townsend)), 100*prop.table(table(df.sum.men$Ethnicity))))

rm(anal.dat,cohort.for.summary,statin.cohort.locf.both,statin.cohort.locf.both.women,statin.cohort.locf.both.men)
save.image("summarise_statin_cohort.RData")


## NOw want to summarise the original cohort in the same way (stuff like ethnicity was summarised slightly differently)
prim.prev.cohort <- read.table("data/analysis_dataset_A_comb.csv", sep="," , header=TRUE)

summary(prim.prev.cohort$gender)
levels(prim.prev.cohort$Ethnicity) <- c("Other Asian","Bangladesh","Black","Chinese","Indian","Missing","Mixed","Other Asian","Other","Pakistan","Pakistan","Missing","White")
prim.prev.cohort$Ethnicity[prim.prev.cohort$Ethnicity == "Missing"] <- NA

prim.prev.cohort.women <- prim.prev.cohort[prim.prev.cohort$gender == 1,]
prim.prev.cohort.men <- prim.prev.cohort[prim.prev.cohort$gender == 0,]
rm(prim.prev.cohort)

## wOMEN COHORT
prim.prev.bin.women <- c(100*prop.table(table(prim.prev.cohort.women$Atrialfib))[2],
                      100*prop.table(table(prim.prev.cohort.women$CKD345))[2], 
                      100*prop.table(table(prim.prev.cohort.women$Famhis_lstrict))[2],
                      100*prop.table(table(prim.prev.cohort.women$Hypertension))[2],
                      100*prop.table(table(prim.prev.cohort.women$RA))[2],
                      100*prop.table(table(prim.prev.cohort.women$T1dia))[2],
                      100*prop.table(table(prim.prev.cohort.women$T2dia))[2])


prim.prev.women <- data.frame("Var" = c("Atrialfib", "CKD345","Famhis_lstrict", "Hypertension", "RA", "T1dia", "T2dia","Never","Ex","Current",
                                         "1","2","3","4","5","Other Asian","Bangladesh","Black","Chinese","Indian","Missing",
                                         "Mixed","Other","Pakistan","White"),
                               "Prop" = c(prim.prev.bin.women,100*prop.table(table(prim.prev.cohort.women$Smoking)),
                                          100*prop.table(table(prim.prev.cohort.women$Townsend)), 100*prop.table(table(prim.prev.cohort.women$Ethnicity))))


## MALE COHORT
prim.prev.bin.men <- c(100*prop.table(table(prim.prev.cohort.men$Atrialfib))[2],
                         100*prop.table(table(prim.prev.cohort.men$CKD345))[2], 
                         100*prop.table(table(prim.prev.cohort.men$Famhis_lstrict))[2],
                         100*prop.table(table(prim.prev.cohort.men$Hypertension))[2],
                         100*prop.table(table(prim.prev.cohort.men$RA))[2],
                         100*prop.table(table(prim.prev.cohort.men$T1dia))[2],
                         100*prop.table(table(prim.prev.cohort.men$T2dia))[2])


prim.prev.men <- data.frame("Var" = c("Atrialfib", "CKD345","Famhis_lstrict", "Hypertension", "RA", "T1dia", "T2dia","Never","Ex","Current",
                                         "1","2","3","4","5","Other Asian","Bangladesh","Black","Chinese","Indian","Missing",
                                         "Mixed","Other","Pakistan","White"),
                               "Prop" = c(prim.prev.bin.men,100*prop.table(table(prim.prev.cohort.men$Smoking)),
                                          100*prop.table(table(prim.prev.cohort.men$Townsend)), 100*prop.table(table(prim.prev.cohort.men$Ethnicity))))


rm(prim.prev.cohort.men,prim.prev.cohort.women)
rm(anal.dat,statin.cohort)
rm(df.sum.men,df.sum.women)

save.image("R_out_C6/summarise_statin_cohort_FAKE.RData")
