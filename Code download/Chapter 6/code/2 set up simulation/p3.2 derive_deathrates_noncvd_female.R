### SET ROOT DIRECTORY

### SET ROOT DIRECTORY


## This code is going to calculate the death rates using the CPRD data and ONS data
## The output of this code is to create a CSV file, which will be used in the simulation

library(dplyr)

## Read in the death data on all patients
deathdates_CPRD_ONS <- read.table("data/deathdates_entire_population_CPRD_ONS.csv", sep="," , header=TRUE)

deathdates_CPRD_ONS <- arrange(deathdates_CPRD_ONS,patid)

head(deathdates_CPRD_ONS)
table(deathdates_CPRD_ONS$death_cens)
## Reduce to female cohort
deathdates_CPRD_ONS <- deathdates_CPRD_ONS[deathdates_CPRD_ONS$gender == 2,]

## Calculate age
deathdates_CPRD_ONS$age <- (deathdates_CPRD_ONS$index_date_A_r - deathdates_CPRD_ONS$dob_r)/365.25

## Create a new death censoring variable called, death_noncvd_cens
## This will = 1 if the death is a non CVD death, and 0 if the patient has a cardiovascular death or is censored
deathdates_CPRD_ONS$death_noncvd_cens <- deathdates_CPRD_ONS$death_cens
deathdates_CPRD_ONS$death_noncvd_cens[deathdates_CPRD_ONS$death_noncvd_cens == 2] <- 0
table(deathdates_CPRD_ONS$death_noncvd_cens)

## Now calculate death rates of non cardiovascular disease as i was doing before


### STEP 1 ###
### STEP 1 ###
### STEP 1 ###
### CALCULATE NON CVD DEATH RATES AMONGST THE ENTIRE UK POPULATION ###
### CALCULATE NON CVD DEATH RATES AMONGST THE ENTIRE UK POPULATION ###
### CALCULATE NON CVD DEATH RATES AMONGST THE ENTIRE UK POPULATION ###
### STEP 1 ###
### STEP 1 ###
### STEP 1 ###


## Unweighted risk group size
risk.contrib <- matrix(0,ncol=80,nrow=dim(deathdates_CPRD_ONS)[1])
for (i in 1:dim(deathdates_CPRD_ONS)[1]){risk.contrib[i,(round(deathdates_CPRD_ONS$age[i])-24):(round(deathdates_CPRD_ONS$age[i])-24+floor(deathdates_CPRD_ONS$death_time[i]/365.25))] <- 1}

head(risk.contrib)

## Event contribution
event.contrib <- matrix(0,ncol=80,nrow=dim(deathdates_CPRD_ONS)[1])
for (i in 1:dim(deathdates_CPRD_ONS)[1]){event.contrib[i,(round(deathdates_CPRD_ONS$age[i])-
                                          24+floor(deathdates_CPRD_ONS$death_time[i]/365.25))] <- 
                                           deathdates_CPRD_ONS$death_noncvd_cens[i]}




## Weighted risk group size
weighted.risk.contrib <- matrix(0,ncol=80,nrow=dim(deathdates_CPRD_ONS)[1])
for (i in 1:dim(deathdates_CPRD_ONS)[1]){weighted.risk.contrib[i,(round(deathdates_CPRD_ONS$age[i])-24):(round(deathdates_CPRD_ONS$age[i])-24+floor(deathdates_CPRD_ONS$death_time[i]/365.25))] <- 1
                               weighted.risk.contrib[i,(round(deathdates_CPRD_ONS$age[i])-24+floor(deathdates_CPRD_ONS$death_time[i]/365.25))] <- 1-min(0.5,deathdates_CPRD_ONS$death_noncvd_cens[i])}
head(weighted.risk.contrib)

### Now I can calculate a table with data for each year
inc.rates.table <- matrix(,ncol=3,nrow=80)

inc.rates.table[,1] <- colSums(risk.contrib)
inc.rates.table[,2] <- colSums(event.contrib)
inc.rates.table[,3] <- colSums(weighted.risk.contrib)
rownames(inc.rates.table) <- paste("Age ",25:104,sep="")
colnames(inc.rates.table) <- c("Risk group","Num events","Weighted risk group")
inc.rates.table <- data.frame(inc.rates.table)

## Calculate the hazards
inc.rates.table$hazard <- inc.rates.table$Num.events/inc.rates.table$Risk.group
inc.rates.table$hazard.weight <- inc.rates.table$Num.events/inc.rates.table$Weighted.risk.group

## Creata a vectpr of incidence rate per 1000 people
inc.rates.table$events.per.1000.people <- inc.rates.table$hazard*1000


## 
plot(25:89,compare.female.deathrates[,2],type = "l",col="red",xlab="Age",ylab="Incidence rate per 1000 people")
compare.female.deathrates[,2]

### STEP 2 ###
### STEP 2 ###
### STEP 2 ###
### Next up is to do this for the cohort of patients who are at risk of CVD (but do not have it yet) ###
### Next up is to do this for the cohort of patients who are at risk of CVD (but do not have it yet) ###
### Next up is to do this for the cohort of patients who are at risk of CVD (but do not have it yet) ###
### STEP 2 ###
### STEP 2 ###
### STEP 2 ###


## All I am interested in here is censoring people when they get CVD
## Load in the QRISK2 cohort and merge
anal.dat <- read.table("/mnt/ja01-home01/mbrxsap3/phd_risk/csv_data/p2_derive_variables/analysis_dataset_A_comb.csv", sep="," , header=TRUE)
gender.var <- 1
anal.dat<-subset(anal.dat,gender==gender.var)
anal.dat <- select(anal.dat,"patid","CVD_time","CVD_cens")
data.comb <- merge(anal.dat,deathdates_CPRD_ONS,by=c("patid"))

head(anal.dat)
head(data.comb)
## FIRST THING TO DO
## I need to derive a 'time until censored or death' variable

## A patient will be censored if they have a CVD event
data.comb <- select(data.comb,c("patid","deathdate","deathdate_r","dtvalid","dtvalid_r","dtcens","dtcens_r",
                                "index_date_A","index_date_A_r","CVD_time","age","death_cens",
                                "death_noncvd_cens","death_time"))

head(deathdates_CPRD_ONS)


## Create a new time until death variable, which also stops if the patient has a CVD event
data.comb$death_time_adj <- pmin(data.comb$death_time,data.comb$CVD_time + 30)

## Set the censoring variable to the same as the original variable, which is 0 if censoring, 1 if non cvd death, 2 if cvd death
data.comb$death_cens_adj <- data.comb$death_cens

## Change the non cvd death to a 0 if a cvd event happeneed first
data.comb$death_cens_adj[(data.comb$death_time_adj < data.comb$death_time) & (data.comb$death_cens == 1)] <- 0

## Finally creaet a new variable so that a cvd death also counts as censoring
data.comb$death_noncvd_cens_adj <- data.comb$death_cens_adj
data.comb$death_noncvd_cens_adj[data.comb$death_cens_adj == 2] <- 0
  
table(data.comb$death_cens_adj)

## Unweighted risk group size
risk.contrib <- matrix(0,ncol=80,nrow=dim(data.comb)[1])
for (i in 1:dim(data.comb)[1]){risk.contrib[i,(round(data.comb$age[i])-24):(round(data.comb$age[i])-24+floor(data.comb$death_time_adj[i]/365.25))] <- 1}

head(risk.contrib)

## Event contribution
event.contrib <- matrix(0,ncol=80,nrow=dim(data.comb)[1])
for (i in 1:dim(data.comb)[1]){event.contrib[i,(round(data.comb$age[i])-
                                                            24+floor(data.comb$death_time_adj[i]/365.25))] <- 
                                           data.comb$death_noncvd_cens_adj[i]}




## Weighted risk group size
weighted.risk.contrib <- matrix(0,ncol=80,nrow=dim(data.comb)[1])
for (i in 1:dim(data.comb)[1]){weighted.risk.contrib[i,(round(data.comb$age[i])-24):(round(data.comb$age[i])-24+floor(data.comb$death_time_adj[i]/365.25))] <- 1
                                         weighted.risk.contrib[i,(round(data.comb$age[i])-24+floor(data.comb$death_time_adj[i]/365.25))] <- 1-min(0.5,data.comb$death_noncvd_cens_adj[i])}
head(weighted.risk.contrib)

### Now I can calculate a table with data for each year
inc.rates.table.riskcohort <- matrix(,ncol=3,nrow=80)

inc.rates.table.riskcohort[,1] <- colSums(risk.contrib)
inc.rates.table.riskcohort[,2] <- colSums(event.contrib)
inc.rates.table.riskcohort[,3] <- colSums(weighted.risk.contrib)
rownames(inc.rates.table.riskcohort) <- paste("Age ",25:104,sep="")
colnames(inc.rates.table.riskcohort) <- c("Risk group","Num events","Weighted risk group")
inc.rates.table.riskcohort <- data.frame(inc.rates.table.riskcohort)

## Calculate the hazards
inc.rates.table.riskcohort$hazard <- inc.rates.table.riskcohort$Num.events/inc.rates.table$Risk.group
inc.rates.table.riskcohort$hazard.weight <- inc.rates.table.riskcohort$Num.events/inc.rates.table$Weighted.risk.group

## Creata a vectpr of incidence rate per 1000 people
inc.rates.table.riskcohort$events.per.1000.people <- inc.rates.table.riskcohort$hazard*1000

write.csv(inc.rates.table.riskcohort,"data/noncvd death rates riskcohort CPRD female_FAKE.csv")
