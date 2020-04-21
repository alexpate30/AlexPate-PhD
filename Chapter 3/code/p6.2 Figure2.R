## This program develops Figure 2 from the main manuscript
rm(list=ls())

## Load the risks from the primary analysis
setwd("B")
load("risks_female_all.RData")
gender.var<-1

library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)
library(reshape2)

### Also summarise survival probabilities and risks for each model
risk.all<-data.frame("surv.mA"=100*surv.all.A.loglog$surv.av,"surv.mB"=100*surv.all.B.loglog$surv.av,"surv.mC"=100*surv.all.C.loglog$surv.av,
                     "surv.mD"=100*surv.all.D.loglog$surv.av,"surv.mE"=100*surv.all.E.loglog$surv.av,"surv.mF"=100*surv.all.F.loglog$surv.av)

risk.all$risk.mA<-100-risk.all$surv.mA
risk.all$risk.mB<-100-risk.all$surv.mB
risk.all$risk.mC<-100-risk.all$surv.mC
risk.all$risk.mD<-100-risk.all$surv.mD
risk.all$risk.mE<-100-risk.all$surv.mE
risk.all$risk.mF<-100-risk.all$surv.mF

## Create variable to say which group each patients belongs in
risk.all$risk.mA.cat<-cut(risk.all$risk.mA,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mB.cat<-cut(risk.all$risk.mB,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mC.cat<-cut(risk.all$risk.mC,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mD.cat<-cut(risk.all$risk.mD,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mE.cat<-cut(risk.all$risk.mE,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mF.cat<-cut(risk.all$risk.mF,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)

## Create a dataset with only patients who have a 9-10% risk
temp<-risk.all[risk.all$risk.mA.cat==10,]

## Add a patient number row
temp<-cbind("pat"=1:nrow(temp),temp)

## Turn data into format that can be used in ggplot (long format)
## All risk scores from all models in one column
temp2<-melt(temp,id.vars="pat",measure.vars=c("risk.mA","risk.mB","risk.mC","risk.mD","risk.mE","risk.mF"))

## Assign levels to the variable of interest (says which model each risk score is in)
levels(temp2$variable)<-c("Model A","Model B","Model C","Model D","Model E","Model F")
colnames(temp2)[2] <- "Model"

print("place marker")

## Create femal density plot
library(ggplot2)
ggplot(temp2, aes(x = value, group = Model, color= Model)) +
  xlab("10 year risk") + 
  coord_cartesian(xlim=c(2.5,17.5)) + 
  scale_x_continuous(breaks=c(2.5,5,7.5,10,12.5,15,17.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  stat_density(geom = "line", position = "identity") +
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = 2.5, size = 0.25)

print("place marker")

ggsave("DensityPlotFemale_final.jpeg", dpi = 600)

## Want to rename temp2 to something else, as I call the male data the same thing
## Stops me from plotting the same graph twice when I save the ggplots
temp3 <- temp2

## Assign the efmale ggplot to "a"
a <- ggplot(temp3, aes(x = value, group = Model, color= Model)) +
  xlab("10 year risk") +
  ggtitle("Female cohort") + 
  coord_cartesian(xlim=c(2.5,17.5)) + 
  scale_x_continuous(breaks=c(2.5,5,7.5,10,12.5,15,17.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=10), legend.text = element_text(size = 8)) +
  stat_density(geom = "line", position = "identity", size = 0.9) +
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = 2.5, size = 0.25)

## Load male data and repeat the process
load("risks_male_all_final.RData")
gender.var<-0

library(mice)
library(foreach)
library(doParallel)
library(dplyr)
library(survival)
library(nnet)
library(VGAM)
library(knitr)

### Create table to give proportion in each category for each model
### Also summarise risks for each model
risk.all<-data.frame("surv.mA"=100*surv.all.A.loglog$surv.av,"surv.mB"=100*surv.all.B.loglog$surv.av,"surv.mC"=100*surv.all.C.loglog$surv.av,
                     "surv.mD"=100*surv.all.D.loglog$surv.av,"surv.mE"=100*surv.all.E.loglog$surv.av,"surv.mF"=100*surv.all.F.loglog$surv.av)

risk.all$risk.mA<-100-risk.all$surv.mA
risk.all$risk.mB<-100-risk.all$surv.mB
risk.all$risk.mC<-100-risk.all$surv.mC
risk.all$risk.mD<-100-risk.all$surv.mD
risk.all$risk.mE<-100-risk.all$surv.mE
risk.all$risk.mF<-100-risk.all$surv.mF

## Create variable to say which group each patients belongs in
risk.all$risk.mA.cat<-cut(risk.all$risk.mA,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mB.cat<-cut(risk.all$risk.mB,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mC.cat<-cut(risk.all$risk.mC,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mD.cat<-cut(risk.all$risk.mD,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mE.cat<-cut(risk.all$risk.mE,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)
risk.all$risk.mF.cat<-cut(risk.all$risk.mF,breaks=c(-Inf,seq(1,20,1),Inf),labels=1:21)

temp<-risk.all[risk.all$risk.mA.cat==10,]

library(reshape2)
temp<-cbind("pat"=1:nrow(temp),temp)
temp2<-melt(temp,id.vars="pat",measure.vars=c("risk.mA","risk.mB","risk.mC","risk.mD","risk.mE","risk.mF"))

levels(temp2$variable)<-c("Model A","Model B","Model C","Model D","Model E","Model F")
colnames(temp2)[2] <- "Model"

library(ggplot2)
ggplot(temp2, aes(x = value, group = Model, color= Model)) +
  xlab("10 year risk") + 
  coord_cartesian(xlim=c(2.5,17.5)) + 
  scale_x_continuous(breaks=c(2.5,5,7.5,10,12.5,15,17.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  stat_density(geom = "line", position = "identity") +
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = 2.5, size = 0.25)

ggsave("DensityPlotMale_final.jpeg", dpi = 600)

## Assign male ggplot to the letter "b"
b <- ggplot(temp2, aes(x = value, group = Model, color= Model)) +
  xlab("10 year risk") + 
  ggtitle("Male cohort") + 
  coord_cartesian(xlim=c(2.5,17.5)) + 
  scale_x_continuous(breaks=c(2.5,5,7.5,10,12.5,15,17.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), text = element_text(size=10), legend.text = element_text(size = 8)) + 
  stat_density(geom = "line", position = "identity", size = 0.9) +
  geom_hline(yintercept = 0, size = 0.25) +
  geom_vline(xintercept = 2.5, size = 0.25)

library(ggpubr)

## Combine into Figure 2 using ggarrange
Figure2 <- ggarrange(a,b,nrow = 2, ncol = 1, common.legend = TRUE, legend = "bottom")
Figure2
ggsave("Fig2.tiff", height = 7, width = 3.5, dpi = 300)
