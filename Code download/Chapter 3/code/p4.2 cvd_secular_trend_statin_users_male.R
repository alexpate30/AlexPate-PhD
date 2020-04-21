### Fit the Poisson model for the statin cohort
rm(list=ls())

library(dplyr)

## Set gender var (female <- 1, male <- 0)
gender.var <- 0

## Read in the statin users cohort
statin.cohort <- read.table("D/statin_users_cohort.csv", sep="," , header=TRUE)
statin.cohort <- statin.cohort[statin.cohort$gender == gender.var, ]

### They key variables for this are:
# risk10y is the 10 year risk score for patients, calculated according to same models from p1.1.2: generate_risks_female_model_B
# first_cvd_all_r: this is a number which represents the date of the patients first cvd event (0 = 1st Jan 1970)
# dtvalid_r: this is a number which represents the date of the patients start of follow up (0 = 1st Jan 1970)
# dtcens_r: this is a number which represents the date of the patients end of follow up (0 = 1st Jan 1970)
# stat_init_r: this is a number which represents the date of the start of the period of statin treatment (0 = 1st Jan 1970)
# stat_end_r: this is a number which represents the date of the end of the period of statin treatment (0 = 1st Jan 1970)
# cvd event = 1 if when a patient is censored, it is because they had a cvd event


## Arrange the cohort
statin.cohort<-arrange(statin.cohort,patid)

## Retain only neccesary variables
statin.cohort.for.model <- select(statin.cohort, patid, pracid, stat_init_r, stat_end_r, dtvalid_r, dtcens_r, first_cvd_all_r, cvd_event, risk10y)


### Need to create a data frame with a different line for every different year in follow up for each treatment period

# First create the values for the cut-offs
d<-rep(0,19)
for (i in 1:19){d[i]<-round(365.25*(i-1))}
d<-d+10227

## Calculate follow up times in each year per person
ftime.list<-vector("list",18)

for (j in 1:18){
  ftime.list[[j]] <- pmax(rep(0,nrow(statin.cohort.for.model)), 
                          pmin(rep(d[(j+1)],nrow(statin.cohort.for.model)),pmin(statin.cohort.for.model$stat_end_r,statin.cohort.for.model$dtcens_r,statin.cohort.for.model$first_cvd_all_r)) - 
                            pmax(rep(d[j],nrow(statin.cohort.for.model)),statin.cohort.for.model$dtvalid_r,statin.cohort.for.model$stat_init_r)
  )
}

## Next do the number of events in each year per person
event.list<-vector("list",18)
for (j in 1:18){
  event.list[[j]]<-pmin(statin.cohort.for.model$cvd_event,as.numeric( (d[j] < statin.cohort.for.model$first_cvd_all_r & 
                                                                         statin.cohort.for.model$first_cvd_all_r <= d[(j+1)]) ) )
}


## Now add both of these into the dataset
statin.cohort.for.model<-cbind(statin.cohort.for.model,ftime.list[[1]],ftime.list[[2]],ftime.list[[3]],ftime.list[[4]],ftime.list[[5]],ftime.list[[6]],ftime.list[[7]],
                           ftime.list[[8]],ftime.list[[9]],ftime.list[[10]],ftime.list[[11]],ftime.list[[12]],ftime.list[[13]],ftime.list[[14]],ftime.list[[15]],
                           ftime.list[[16]],ftime.list[[17]],ftime.list[[18]],event.list[[1]],event.list[[2]],event.list[[3]],event.list[[4]],
                           event.list[[5]],event.list[[6]],event.list[[7]],event.list[[8]],event.list[[9]],event.list[[10]],event.list[[11]],event.list[[12]],
                           event.list[[13]],event.list[[14]],event.list[[15]],event.list[[16]],event.list[[17]],event.list[[18]])

## Now the sums of each row should match my previous summaries
sum(statin.cohort.for.model$"ftime.list[[1]]")
sum(statin.cohort.for.model$"ftime.list[[18]]")


## Now I need to transpose this and give it better names
colnames(statin.cohort.for.model)[(ncol(statin.cohort.for.model)-35):ncol(statin.cohort.for.model)]<-c("ftime1","ftime2","ftime3","ftime4","ftime5","ftime6","ftime7","ftime8",
                                                                                           "ftime9","ftime10","ftime11","ftime12","ftime13","ftime14","ftime15","ftime16",
                                                                                           "ftime17","ftime18",
                                                                                           "event1","event2","event3","event4","event5","event6","event7","event8",
                                                                                           "event9","event10","event11","event12","event13","event14","event15","event16",
                                                                                           "event17","event18")


## Now lets subset this for analysis
## Make two different subsets, so I can transpose both the event times and events seperately, before recombining
## Eventual aim is to have a separate row for each year for each person
statin.users.cohort.red1<-statin.cohort.for.model[,c("patid","pracid","risk10y","ftime1","ftime2","ftime3","ftime4","ftime5","ftime6","ftime7","ftime8",
                                               "ftime9","ftime10","ftime11","ftime12","ftime13","ftime14","ftime15","ftime16",
                                               "ftime17","ftime18")]

statin.users.cohort.red2<-statin.cohort.for.model[,c("patid","pracid","risk10y","event1","event2","event3","event4","event5","event6","event7","event8",
                                                 "event9","event10","event11","event12","event13","event14","event15","event16",
                                                 "event17","event18")]


library(reshape)
## Reshapre each seperately
reshape.data1<-melt(statin.users.cohort.red1, id=c("patid","pracid","risk10y"))
reshape.data2<-melt(statin.users.cohort.red2, id=c("patid","pracid","risk10y"))

head(reshape.data1)
head(reshape.data2)

## recombine and remove unncessary columns
reshape.data<-cbind(reshape.data1[,],reshape.data2[,-c(1,2,3)])
reshape.data<-reshape.data[,c(1,2,3,5,7)]

colnames(reshape.data)[c(4,5)]<-c("ftime","event")

## Now add the year to each row
year<-c(rep(0,nrow(statin.cohort.for.model)),rep(1,nrow(statin.cohort.for.model)),rep(2,nrow(statin.cohort.for.model)),rep(3,nrow(statin.cohort.for.model)),
        rep(4,nrow(statin.cohort.for.model)),rep(5,nrow(statin.cohort.for.model)),rep(6,nrow(statin.cohort.for.model)),rep(7,nrow(statin.cohort.for.model)),
        rep(8,nrow(statin.cohort.for.model)),rep(9,nrow(statin.cohort.for.model)),rep(10,nrow(statin.cohort.for.model)),rep(11,nrow(statin.cohort.for.model)),
        rep(12,nrow(statin.cohort.for.model)),rep(13,nrow(statin.cohort.for.model)),rep(14,nrow(statin.cohort.for.model)),rep(15,nrow(statin.cohort.for.model)),
        rep(16,nrow(statin.cohort.for.model)),rep(17,nrow(statin.cohort.for.model)))                                                                            
reshape.data$year<-year

str(reshape.data)

## I now want to remove any columns where the follow up time is zero
reshape.data.anal<-reshape.data[(reshape.data$ftime > 0),]

reshape.data.anal<-arrange(reshape.data.anal,patid,year)

head(reshape.data.anal,n=10)

## Also change the follow up to in terms of years, so the results match the cvd secular trend without adjusting
reshape.data.anal$ftime<-reshape.data.anal$ftime/365.25
reshape.data.anal$risk10y <- 100*reshape.data.anal$risk10y


## Fit the Poisson model just with year
pois.mod.time<-glm(event~offset(log(ftime)) + year, family=poisson(link="log"), data=reshape.data.anal)
pois.mod.time

## Now add in the adjusting for QRISK
pois.mod<-glm(event~offset(log(ftime)) + year + risk10y, family=poisson(link="log"), data=reshape.data.anal)
pois.mod

summary(pois.mod.time)
summary(pois.mod)

rm(reshape.data,statin.users.cohort.red1,statin.users.cohort.red2,statin.users.cohort,reshape.data1,reshape.data2)
rm(statin.cohort.locf.conv, statin.users.init, statin.users.subseq, surv.all)

rm(list=setdiff(ls(),list("pois.mod","pois.mod.time")))

setwd("B")
save.image("cvd_secular_trend_statin_users_male.RData")
