### This program develops the tables that are found in the manuscript, as well as some other tables, but for cohort 2016

## Mostly it derives the equivalent to Table 5 for the 2016 cohort, which results in Figure 3

rm(list=ls())
setwd("B")
load("risks_female_all_cohort2016.RData")
gender.var<-1

library(mice)
library(foreach)
library(doParallel)
library(plyr)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)

### Combine survival probs and risks into one dataset, and get into percentage form
risk.all<-data.frame("patid"=surv.all.A.loglog$patid,"surv.mA"=100*surv.all.A.loglog$surv.av,"surv.mB"=100*surv.all.B.loglog$surv.av,"surv.mC"=100*surv.all.C.loglog$surv.av,
                     "surv.mD"=100*surv.all.D.loglog$surv.av,"surv.mE"=100*surv.all.E.loglog$surv.av,"surv.mF"=100*surv.all.F.loglog$surv.av)


risk.all$risk.mA<-100-risk.all$surv.mA
risk.all$risk.mB<-100-risk.all$surv.mB
risk.all$risk.mC<-100-risk.all$surv.mC
risk.all$risk.mD<-100-risk.all$surv.mD
risk.all$risk.mE<-100-risk.all$surv.mE
risk.all$risk.mF<-100-risk.all$surv.mF

head(risk.all)

## Create variable to say which risk group each patients belongs in (1% width)
risk.all$risk.mA.cat<-cut(risk.all$risk.mA,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mB.cat<-cut(risk.all$risk.mB,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mC.cat<-cut(risk.all$risk.mC,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mD.cat<-cut(risk.all$risk.mD,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mE.cat<-cut(risk.all$risk.mE,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mF.cat<-cut(risk.all$risk.mF,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)

## Create variable to say whether patient has risk over 10%
risk.all$risk.mA.o10<-cut(risk.all$risk.mA,breaks=c(-Inf,10,Inf),labels=c("< 10%","> 10%"))
risk.all$risk.mB.o10<-cut(risk.all$risk.mB,breaks=c(-Inf,10,Inf),labels=c("< 10%","> 10%"))
risk.all$risk.mC.o10<-cut(risk.all$risk.mC,breaks=c(-Inf,10,Inf),labels=c("< 10%","> 10%"))
risk.all$risk.mD.o10<-cut(risk.all$risk.mD,breaks=c(-Inf,10,Inf),labels=c("< 10%","> 10%"))
risk.all$risk.mE.o10<-cut(risk.all$risk.mE,breaks=c(-Inf,10,Inf),labels=c("< 10%","> 10%"))
risk.all$risk.mF.o10<-cut(risk.all$risk.mF,breaks=c(-Inf,10,Inf),labels=c("< 10%","> 10%"))


### Start by summarising results for each model
### Sumarise risks for each model
sum.mA<-summary(risk.all$risk.mA)
sum.mA<-c(sum.mA,"sd"=sd(risk.all$risk.mA))
sum.mA<-sum.mA[c(1,2,3,5,6,4,7)]
sum.mB<-summary(risk.all$risk.mB)
sum.mB<-c(sum.mB,"sd"=sd(risk.all$risk.mB))
sum.mB<-sum.mB[c(1,2,3,5,6,4,7)]
sum.mC<-summary(risk.all$risk.mC)
sum.mC<-c(sum.mC,"sd"=sd(risk.all$risk.mC))
sum.mC<-sum.mC[c(1,2,3,5,6,4,7)]
sum.mD<-summary(risk.all$risk.mD)
sum.mD<-c(sum.mD,"sd"=sd(risk.all$risk.mD))
sum.mD<-sum.mD[c(1,2,3,5,6,4,7)]
sum.mE<-summary(risk.all$risk.mE)
sum.mE<-c(sum.mE,"sd"=sd(risk.all$risk.mE))
sum.mE<-sum.mE[c(1,2,3,5,6,4,7)]
sum.mF<-summary(risk.all$risk.mF)
sum.mF<-c(sum.mF,"sd"=sd(risk.all$risk.mF))
sum.mF<-sum.mF[c(1,2,3,5,6,4,7)]


## Calculate proportion for each category and put into a vector
prop.o10<-rep(0,6)
size.data<-dim(risk.all)[1]

prop.o10[1]<-100*sum(risk.all$risk.mA.o10=="> 10%")/size.data
prop.o10[2]<-100*sum(risk.all$risk.mB.o10=="> 10%")/size.data
prop.o10[3]<-100*sum(risk.all$risk.mC.o10=="> 10%")/size.data
prop.o10[4]<-100*sum(risk.all$risk.mD.o10=="> 10%")/size.data
prop.o10[5]<-100*sum(risk.all$risk.mE.o10=="> 10%")/size.data
prop.o10[6]<-100*sum(risk.all$risk.mF.o10=="> 10%")/size.data

summary.dat<-rbind(sum.mA,sum.mB,sum.mC,sum.mD,sum.mE,sum.mF)
summary.dat<-cbind(summary.dat,prop.o10)

rownames(summary.dat)<-c("model A","model B","model C", "model D","model E","model F")

rm(anal.dat)


### Generate equivalent of Table 2 from the QRISK paper (but for the 2016 cohort, so this is not directly comparable)
## This has the incidence rate by age
## First split into age groups, for this we need to assign age, CVD_time and whether they were censored or not
## To do this I need to add in peoples CVD_time and CVD_censored and age

## I cannot use age from anal.dat directly as they have all been normalised
## Read in original data in order to get everyones actual age, CVD_time and CVD_censored

anal.dat <- read.table("D/analysis_dataset_C_comb.csv", sep="," , header=TRUE)


## Subset to females
anal.dat<-subset(anal.dat,gender==gender.var) 
anal.dat.raw<-select(anal.dat, pracid, patid, age)
## Arrange to same order (by patid) that data is in
anal.dat.raw<-arrange(anal.dat.raw,patid)


## Assign the ages
risk.all$age<-anal.dat.raw$age

rm(anal.dat.raw,anal.dat.raw0,anal.dat.raw1)
rm(anal.dat)

## Check everything looks normal
head(risk.all,n=20)

## Asign a variable for age group
risk.all$age.cat<-cut(risk.all$age,breaks=c(-Inf,seq(30,85,5)),labels=1:12)
head(risk.all)

## For each age group calculate
# Number event
# Time at risk
# Rate per 1000 person years
get_incidence_rates<-function(i){
  temp<-risk.all[risk.all$age.cat==i,]
  cases<-sum(temp$CVD_cens_R)
  time.at.risk<-sum(as.numeric(temp$CVD_time))
  rate<-1000*365.25*cases/time.at.risk
  output<-data.frame("cases"=cases,"person years"=time.at.risk/365.25,"rate"=rate)
  return(output)
}

## Put output into list elements
incidence.list<-vector("list",12)
for (j in 1:12){
  incidence.list[[j]]<-get_incidence_rates(j)
}

# Get total number of cases, time at risk and rate
cases<-sum(risk.all$CVD_cens_R)
time.at.risk<-sum(as.numeric(risk.all$CVD_time))
rate<-1000*365.25*cases/time.at.risk
all<-data.frame("cases"=cases,"person years"=time.at.risk/365.25,"rate"=rate)

## Combine into table
incidence.rates<-rbind(incidence.list[[1]],incidence.list[[2]],incidence.list[[3]],incidence.list[[4]],incidence.list[[5]],
                   incidence.list[[6]],incidence.list[[7]],incidence.list[[8]],incidence.list[[9]],incidence.list[[10]],
                   incidence.list[[11]],incidence.list[[12]],all)
rownames(incidence.rates)<-c("25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","Total")


### Create Table 5 - percentage of patients from each risk group that have a risk > 10% according to each model
### NB THIS IS EQUIVALENT TO TABLE 5 FOR 2016 COHORT - USED TO GENERATE FIGURE 3 ###
### NB THIS IS EQUIVALENT TO TABLE 5 FOR 2016 COHORT - USED TO GENERATE FIGURE 3 ###
### NB THIS IS EQUIVALENT TO TABLE 5 FOR 2016 COHORT - USED TO GENERATE FIGURE 3 ###
### NB THIS IS EQUIVALENT TO TABLE 5 FOR 2016 COHORT - USED TO GENERATE FIGURE 3 ###
### NB THIS IS EQUIVALENT TO TABLE 5 FOR 2016 COHORT - USED TO GENERATE FIGURE 3 ###


# How do to it for group i and model m.
# I want this function to go through every i, and store the results, I will do each m separately
head(risk.all)


### For each group, I want to go through each model, and calculate the proportion of patients that are above 10% according to that model
## This function does that, for group i
get_prop_o10<-function(i){
  # Extract patients in risk group i
  temp<-risk.all[risk.all$risk.mA.cat==i,]
  size.temp<-dim(temp)[1]
  # Calculate the number still in the same group, for each model A,B,C,D,E
  num.o10.A<-sum(temp$risk.mA.o10=="> 10%")
  num.o10.B<-sum(temp$risk.mB.o10=="> 10%")
  num.o10.C<-sum(temp$risk.mC.o10=="> 10%")
  num.o10.D<-sum(temp$risk.mD.o10=="> 10%")
  num.o10.E<-sum(temp$risk.mE.o10=="> 10%")
  num.o10.F<-sum(temp$risk.mF.o10=="> 10%")
  # Calculate the number transferred out of each group
  prop.o10.A<-100*num.o10.A/size.temp
  prop.o10.B<-100*num.o10.B/size.temp
  prop.o10.C<-100*num.o10.C/size.temp
  prop.o10.D<-100*num.o10.D/size.temp
  prop.o10.E<-100*num.o10.E/size.temp
  prop.o10.F<-100*num.o10.F/size.temp
  # Put into dataframe
  output<-data.frame("prop"=c(size.temp,prop.o10.A,prop.o10.B,prop.o10.C,prop.o10.D,prop.o10.E,prop.o10.F),
                     "num"=c(size.temp,num.o10.A,num.o10.B,num.o10.C,num.o10.D,num.o10.E,num.o10.F))
  return(output)
}

### For each group, I want to go through each model, and calculate the proportion of patients that are under 10% according to that model
## This function does that, for group i
get_prop_u10<-function(i){
  # Extract patients in risk group i
  temp<-risk.all[risk.all$risk.mA.cat==i,]
  size.temp<-dim(temp)[1]
  # Calculate the number still in the same group, for each model A,B,C,D,E
  num.o10.A<-sum(temp$risk.mA.o10=="< 10%")
  num.o10.B<-sum(temp$risk.mB.o10=="< 10%")
  num.o10.C<-sum(temp$risk.mC.o10=="< 10%")
  num.o10.D<-sum(temp$risk.mD.o10=="< 10%")
  num.o10.E<-sum(temp$risk.mE.o10=="< 10%")
  num.o10.F<-sum(temp$risk.mF.o10=="< 10%")
  # Calculate the number transferred out of each group
  prop.o10.A<-100*num.o10.A/size.temp
  prop.o10.B<-100*num.o10.B/size.temp
  prop.o10.C<-100*num.o10.C/size.temp
  prop.o10.D<-100*num.o10.D/size.temp
  prop.o10.E<-100*num.o10.E/size.temp
  prop.o10.F<-100*num.o10.F/size.temp
  # Put into dataframe
  output<-data.frame("prop"=c(size.temp,prop.o10.A,prop.o10.B,prop.o10.C,prop.o10.D,prop.o10.E,prop.o10.F),
                     "num"=c(size.temp,num.o10.A,num.o10.B,num.o10.C,num.o10.D,num.o10.E,num.o10.F))
  return(output)
}


## Create a list to store output
table5.list<-vector("list",21)
## For each group below 10% threshold, calculate the proportion of patients that are above (i.e. cross) threshold according to each model
for (j in 1:10){
  table5.list[[j]]<-get_prop_o10(j)
}
## For each group above 10% threshold, calculate the proportion of patients that are below (i.e. cross) threshold according to each model
for (j in 11:21){
  table5.list[[j]]<-get_prop_u10(j)
}

## Get proportions and put into table
table5.prop<-cbind(table5.list[[1]]$prop,table5.list[[2]]$prop,table5.list[[3]]$prop,table5.list[[4]]$prop,table5.list[[5]]$prop,
                   table5.list[[6]]$prop,table5.list[[7]]$prop,table5.list[[8]]$prop,table5.list[[9]]$prop,table5.list[[10]]$prop,
                   table5.list[[11]]$prop,table5.list[[12]]$prop,table5.list[[13]]$prop,table5.list[[14]]$prop,table5.list[[15]]$prop,
                   table5.list[[16]]$prop,table5.list[[17]]$prop,table5.list[[18]]$prop,table5.list[[19]]$prop,table5.list[[20]]$prop,
                   table5.list[[21]]$prop)

## Get numbers and put into table
table5.num<-cbind(table5.list[[1]]$num,table5.list[[2]]$num,table5.list[[3]]$num,table5.list[[4]]$num,table5.list[[5]]$num,
                  table5.list[[6]]$num,table5.list[[7]]$num,table5.list[[8]]$num,table5.list[[9]]$num,table5.list[[10]]$num,
                  table5.list[[11]]$num,table5.list[[12]]$num,table5.list[[13]]$num,table5.list[[14]]$num,table5.list[[15]]$num,
                  table5.list[[16]]$num,table5.list[[17]]$num,table5.list[[18]]$num,table5.list[[19]]$num,table5.list[[20]]$num,
                  table5.list[[21]]$num)

## Assign row and column names
rownames(table5.prop)<-c("n","model A","model B","model C", "model D","model E","model F")
colnames(table5.prop)<-c("0-1%","1-2%","2-3%","3-4%","4-5%","5-6%","6-7%","7-8%","8-9%","9-10%",
                         "10-11%","11-12%","12-13%","13-14%","14-15%","15-16%","16-17%","17-18%","18-19%","19-20%","> 20%")
rownames(table5.num)<-c("n","model A","model B","model C", "model D","model E","model F")
colnames(table5.num)<-c("0-1%","1-2%","2-3%","3-4%","4-5%","5-6%","6-7%","7-8%","8-9%","9-10%",
                        "10-11%","11-12%","12-13%","13-14%","14-15%","15-16%","16-17%","17-18%","18-19%","19-20%","> 20%")

## add extra column which summarises all risk catergories below 10%, and all risk categories above 10%
table5.num<-cbind(table5.num,rowSums(table5.num[,1:10]),rowSums(table5.num[,11:21]))
colnames(table5.num)[c(22,23)]<-c("Total over","Total under")

table5.prop<-cbind(table5.prop,100*rowSums(table5.num[,1:10])/rowSums(table5.num[,1:10])[1],100*rowSums(table5.num[,11:21]/rowSums(table5.num[,11:21])[1]))

print("table 5 done")

## Table 4
# Want to generate a summary for each group, i.e. range of risks

## NB EQUIVALENT TO TABLE 4 FROM MANUSCRIPT FOR 2016 COHORT ##
## NB EQUIVALENT TO TABLE 4 FROM MANUSCRIPT FOR 2016 COHORT ##
## NB EQUIVALENT TO TABLE 4 FROM MANUSCRIPT FOR 2016 COHORT ##
## NB EQUIVALENT TO TABLE 4 FROM MANUSCRIPT FOR 2016 COHORT ##

# Want to generate a summary for each group, i.e. range of risks according to each model, for patients within the same group
get_summary<-function(i){
  temp<-risk.all[risk.all$risk.mA.cat==i,]
  #sum<-quantile(temp$risk.mA,probs=c(.025,.25,.5,.75,.975))
  #sum<-sum[c("2.5%","25%","75%","97.5%")]
  #sum.mA<-c(sum,"sd"=sd(temp$risk.mA))
  
  sum<-quantile(temp$risk.mB,probs=c(.025,.5,.975))
  sum<-sum[c("2.5%","97.5%")]
  sum.mB<-sum
  #sum.mB<-c(sum,"sd"=sd(temp$risk.mB))
  
  sum<-quantile(temp$risk.mC,probs=c(.025,.5,.975))
  sum<-sum[c("2.5%","97.5%")]
  sum.mC<-sum
  #sum.mC<-c(sum,"sd"=sd(temp$risk.mC))
  
  sum<-quantile(temp$risk.mD,probs=c(.025,.5,.975))
  sum<-sum[c("2.5%","97.5%")]
  sum.mD<-sum
  #sum.mD<-c(sum,"sd"=sd(temp$risk.mD))
  
  sum<-quantile(temp$risk.mE,probs=c(.025,.5,.975))
  sum<-sum[c("2.5%","97.5%")]
  sum.mE<-sum
  #sum.mE<-c(sum,"sd"=sd(temp$risk.mE))
  
  sum<-quantile(temp$risk.mF,probs=c(.025,.5,.975))
  sum<-sum[c("2.5%","97.5%")]
  sum.mF<-sum
  #sum.mF<-c(sum,"sd"=sd(temp$risk.mF))
  
  output<-c(sum.mB,sum.mC,sum.mD,sum.mE,sum.mF)
  #rownames(output)<-c("Model A","Model B","Model C","Model D","Model E","model F")
  return(output)
}


table4.list<-vector("list",21)
for (j in 1:21){
  table4.list[[j]]<-get_summary(j)
}

table4<-rbind(table4.list[[1]],table4.list[[2]],table4.list[[3]],table4.list[[4]],table4.list[[5]],table4.list[[6]],table4.list[[7]],table4.list[[8]],
              table4.list[[9]],table4.list[[10]],table4.list[[11]],table4.list[[12]],table4.list[[13]],table4.list[[14]],table4.list[[15]],table4.list[[16]],
              table4.list[[17]],table4.list[[18]],table4.list[[19]],table4.list[[20]])

print("table 4 done")


setwd("B")
save.image("female_tables_generated_cohort2016.RData")
print("image saved")



test<-matrix(c(1,4,7,0),nrow=2)
b<-9
c<-11
test<-data.frame(test)

test[1,1]<-paste(b,"+-",c)
test
