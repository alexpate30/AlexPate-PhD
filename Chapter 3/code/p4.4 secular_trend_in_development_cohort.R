## This will take the development cohort, and calculate the incidence of cvd events each year (used in Figure 4)

rm(list=ls())

setwd("A")

library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)


## Male cohort first
load("imputed_datasets_loaded_male.RData")

rm(long_data_parallel)

## anal.dat contains variables of interest
### They key variables for this are:
# first_cvd_all_r: this is a number which represents the date of the patients first cvd event (0 = 1st Jan 1970)
# dtvalid_r: this is a number which represents the date of the patients start of follow up (0 = 1st Jan 1970)
# dtcens_r: this is a number which represents the date of the patients end of follow up (0 = 1st Jan 1970)
# cvd event = 1 if when a patient is censored, it is because they had a cvd event

# First create the values for the cut-offs
d<-rep(0,19)
for (i in 1:19){d[i]<-round(365.25*(i-1))}
d<-d+10227

## Do first follow times
ftime.list<-vector("list",18)

for (j in 1:18){
ftime.list[[j]] <- pmax(rep(0,nrow(anal.dat)), 
                        pmin(rep(d[(j+1)],nrow(anal.dat)),pmin(anal.dat$dtcens_r,anal.dat$first_cvd_all_r)) - 
                          pmax(rep(d[j],nrow(anal.dat)),anal.dat$index_date_A_r)
                        )
}

## Now do wether a patient had an event in each year
event.list<-vector("list",18)
for (j in 1:18){
event.list[[j]]<-pmin(anal.dat$CVD_cens_R,as.numeric( (d[j] < pmin(anal.dat$dtcens_r,anal.dat$first_cvd_all_r)) & (pmin(anal.dat$dtcens_r,anal.dat$first_cvd_all_r) <= d[(j+1)]) ) )
}

table(anal.dat$CVD_cens_R)                      

## Incidence rates
t.followup<-rep(0,18)
t.events<-rep(0,18)

for (j in 1:18){t.followup[j]<-sum(ftime.list[[j]])/365.25}
for (j in 1:18){t.events[j]<-sum(event.list[[j]])}

incidence<-rep(0,18)
for (j in 1:18){incidence[j]<-t.events[j]/t.followup[j]}

inc.tab<-cbind(t.followup,t.events,1000*incidence)
rownames(inc.tab)<-1998:2015
colnames(inc.tab)<-c("followup","t.events","incidence")

inc.tab<-data.frame(inc.tab)
total<-c(sum(inc.tab[,1]),sum(inc.tab[,2]))
total<-c(total,1000*total[2]/total[1])

inc.tab.male<-rbind(inc.tab,total)
rownames(inc.tab.male)[19]<-c("total")

setwd("B")
save.image("secular_trend.RData")


### Do the same for female cohort
### Do the same for female cohort
### Do the same for female cohort

setwd("A")
load("imputed_datasets_loaded_female.RData")


# First create the values for the cut-offs
d<-rep(0,19)
for (i in 1:19){d[i]<-round(365.25*(i-1))}
d<-d+10227

## Do first follow times
ftime.list<-vector("list",18)

for (j in 1:18){
  ftime.list[[j]] <- pmax(rep(0,nrow(anal.dat)), 
                          pmin(rep(d[(j+1)],nrow(anal.dat)),pmin(anal.dat$dtcens_r,anal.dat$first_cvd_all_r)) - 
                            pmax(rep(d[j],nrow(anal.dat)),anal.dat$index_date_A_r)
  )
}

## Now do wether a patient had an event in each year
event.list<-vector("list",18)
for (j in 1:18)
  event.list[[j]]<-pmin(anal.dat$CVD_cens_R,as.numeric( (d[j] < pmin(anal.dat$dtcens_r,anal.dat$first_cvd_all_r)) & (pmin(anal.dat$dtcens_r,anal.dat$first_cvd_all_r) <= d[(j+1)]) ) )

table(anal.dat$CVD_cens_R)                      

## Incidence rates
t.followup<-rep(0,18)
t.events<-rep(0,18)

for (j in 1:18){t.followup[j]<-sum(ftime.list[[j]])/365.25}
for (j in 1:18){t.events[j]<-sum(event.list[[j]])}

incidence<-rep(0,18)
for (j in 1:18){incidence[j]<-t.events[j]/t.followup[j]}

inc.tab<-cbind(t.followup,t.events,1000*incidence)
rownames(inc.tab)<-1998:2015
colnames(inc.tab)<-c("followup","t.events","incidence")

inc.tab<-data.frame(inc.tab)
total<-c(sum(inc.tab[,1]),sum(inc.tab[,2]))
total<-c(total,1000*total[2]/total[1])

inc.tab.female<-rbind(inc.tab,total)
rownames(inc.tab.female)[19]<-c("total")



### Do models
mod.tab.female<-cbind(inc.tab.female,"year"=0:18)
mod.tab.female<-mod.tab.female[1:18,]

pois.model.female<-glm(t.events ~ year, offset = log(t.followup), family = "poisson"(link = "log"), data=mod.tab.female)
summary(pois.model.female)

exp(pois.model.female$coefficients)



mod.tab.male<-cbind(inc.tab.male,"year"=0:18)
mod.tab.male<-mod.tab.male[1:18,]

pois.model.male<-glm(t.events ~ year, offset = log(t.followup), family = "poisson"(link = "log"), data=mod.tab.male)
summary(pois.model.male)

exp(pois.model.male$coefficients)

rm(list=setdiff(ls(),list("pois.model.female","pois.model.male","mod.tab.female","mod.tab.male","inc.tab.male",
                "inc.tab.female")))
setwd("B")
save.image("secular_trend.RData")



