### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## Read in packages
library(dplyr)

## Define variable for gender (male = 0, female = 1)
gender.var <- 1

## Load in the historical cohort
data.historical <- read.table("/data/data_historical.csv", sep="," , header=TRUE)

## Load in the 2016 data
data.2016 <- read.table("/data/data_2016.csv", sep="," , header=TRUE)

## Restrict both to female cohort
data.historical <- data.historical[data.historical$gender == gender.var,]
data.2016 <- data.2016[data.2016$gender == gender.var,]

## Now need to get the data in the same order as it is after imputation
## For imputation had to split it into two datasets and re-combine
set.seed(10)
data.historical$rand.bern<-rbinom(dim(data.historical)[1],1,0.5)
data.historical.0 <- data.historical[data.historical$rand.bern==0,]
data.historical.1 <- data.historical[data.historical$rand.bern==1,]

data.historical.0 <- subset(data.historical.0,select=-c(rand.bern))
data.historical.1 <- subset(data.historical.1,select=-c(rand.bern))

data.historical <- rbind(data.historical.0,data.historical.1)

## Now I can split it into development and validation
set.seed(501)

row.ids <- sample(1:nrow(data.historical),100000, replace = FALSE)
data_devel <- data.historical[-row.ids,]
data_valid <- data.historical[row.ids,]

### Now I can produce the table for them
rm(list=setdiff(ls(),list("data_devel","data_valid","data.2016")))



## First going to summarise the data we have (i.e. will do missing seperately)
## First going to get the mean and standard deviation of all the continuous variables
devel.mean <- c(mean(data_devel$age),mean(data_devel$SBP,na.rm = TRUE),
                          mean(data_devel$BMI,na.rm = TRUE),mean(data_devel$Cholesterol_HDL_Ratio,na.rm = TRUE))
devel.sd <- c(sd(data_devel$age),sd(data_devel$SBP,na.rm = TRUE),
                          sd(data_devel$BMI,na.rm = TRUE),sd(data_devel$Cholesterol_HDL_Ratio,na.rm = TRUE))

valid.mean <- c(mean(data_valid$age),mean(data_valid$SBP,na.rm = TRUE),
                mean(data_valid$BMI,na.rm = TRUE),mean(data_valid$Cholesterol_HDL_Ratio,na.rm = TRUE))
valid.sd <- c(sd(data_valid$age),sd(data_valid$SBP,na.rm = TRUE),
              sd(data_valid$BMI,na.rm = TRUE),sd(data_valid$Cholesterol_HDL_Ratio,na.rm = TRUE))

data.2016.mean <- c(mean(data.2016$age),mean(data.2016$SBP,na.rm = TRUE),
                mean(data.2016$BMI,na.rm = TRUE),mean(data.2016$Cholesterol_HDL_Ratio,na.rm = TRUE))
data.2016.sd <- c(sd(data.2016$age),sd(data.2016$SBP,na.rm = TRUE),
              sd(data.2016$BMI,na.rm = TRUE),sd(data.2016$Cholesterol_HDL_Ratio,na.rm = TRUE))


## Also get the number of events and total follow up time (just for devel and valid, no point for 2016 cohort)
events <- c(sum(1-data_devel$CVD_cens),sum(1-data_valid$CVD_cens))
followup <- c(sum(as.numeric(data_devel$CVD_time)),sum(as.numeric(data_valid$CVD_time)))/365.25


## Next want to summarise the categorical variables
Smoking.prop <- cbind(100*prop.table(table(data_devel$Smoking)),100*prop.table(table(data_valid$Smoking)),
                      100*prop.table(table(data.2016$Smoking)))

Townsend.prop <- cbind(100*prop.table(table(data_devel$Townsend)),100*prop.table(table(data_valid$Townsend)),
                      100*prop.table(table(data.2016$Townsend)))

Hypertension.prop <- c(100*prop.table(table(data_devel$Hypertension))[2],100*prop.table(table(data_valid$Hypertension))[2],
                       100*prop.table(table(data.2016$Hypertension))[2])

Famhis_lstrict.prop <- c(100*prop.table(table(data_devel$Famhis_lstrict))[2],100*prop.table(table(data_valid$Famhis_lstrict))[2],
                       100*prop.table(table(data.2016$Famhis_lstrict))[2])

T2dia.prop <- c(100*prop.table(table(data_devel$T2dia))[2],100*prop.table(table(data_valid$T2dia))[2],
                         100*prop.table(table(data.2016$T2dia))[2])


## Now to summarise missingness
SBP.miss <- c(100*sum(is.na(data_devel$SBP))/nrow(data_devel),
              100*sum(is.na(data_valid$SBP))/nrow(data_valid),
              100*sum(is.na(data.2016$SBP))/nrow(data.2016))

BMI.miss <- c(100*sum(is.na(data_devel$BMI))/nrow(data_devel),
              100*sum(is.na(data_valid$BMI))/nrow(data_valid),
              100*sum(is.na(data.2016$BMI))/nrow(data.2016))

Chol.miss <- c(100*sum(is.na(data_devel$Cholesterol_HDL_Ratio))/nrow(data_devel),
              100*sum(is.na(data_valid$Cholesterol_HDL_Ratio))/nrow(data_valid),
              100*sum(is.na(data.2016$Cholesterol_HDL_Ratio))/nrow(data.2016))

Smoking.miss <- c(100*sum(is.na(data_devel$Smoking))/nrow(data_devel),
              100*sum(is.na(data_valid$Smoking))/nrow(data_valid),
              100*sum(is.na(data.2016$Smoking))/nrow(data.2016))


## Create continuous table
cont.vars <- data.frame(cbind(
      paste(round(devel.mean,2)," (",round(devel.sd,2),")",sep=""),
      paste(round(valid.mean,2)," (",round(valid.sd,2),")",sep=""),
      paste(round(data.2016.mean,2)," (",round(data.2016.sd,2),")",sep="")))
colnames(cont.vars) <- c("Development","Validation","2016")
rownames(cont.vars) <- c("Age","SBP","BMI","Chol/HDL")


## Create categorical table
cat.vars <- rbind(Smoking.prop,Townsend.prop,Hypertension.prop,Famhis_lstrict.prop,T2dia.prop)
colnames(cat.vars) <- c("Development","Validation","2016")

## Create missingness values
miss.vars <- rbind(SBP.miss,BMI.miss,Chol.miss,Smoking.miss)
colnames(miss.vars) <- c("Development","Validation","2016")
rownames(miss.vars) <- c("SBP","BMI","Chol/HDL","Smoking")

## Also count number of patients in each cohort
cohort.sizes <- c(nrow(data_devel),
                  nrow(data_valid),
                  nrow(data.2016))
names(cohort.sizes) <- c("Development","Validation","2016")

rm(list=setdiff(ls(),list("miss.vars","cat.vars","cont.vars","cohort.sizes","events","followup")))

save.image("R_out_C4/baseline_table_female.RData")
