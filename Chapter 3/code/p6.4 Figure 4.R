## Code for Figure 4

rm(list=ls())

## Should contain secular trend for:
# Female derivation cohort
# Male derivation cohort
# Female statin users
# Male statin users

library(ggplot2)
library(ggpubr)


## First load the relevant plots
setwd("B")
load("secular_trend.RData")

inc.tab.female.deriv <- inc.tab.female
inc.tab.male.deriv <- inc.tab.male

colnames(inc.tab.female)<-c("follow up (years)","events","incidence (per 1000 person years)")
female.deriv <- ggplot() +
  geom_point(aes(x = 1998:2015,y = inc.tab.female.deriv[1:18,3])) +
  xlab("Year") +
  ylab("Incidence per 1000 person years") +
  ggtitle("Model derivation cohort - Female")+
  theme(text = element_text(size = 8))
             

colnames(inc.tab.male)<-c("follow up (years)","events","incidence (per 1000 person years)")
male.deriv <- ggplot() +
  geom_point(aes(x = 1998:2015,y = inc.tab.male.deriv[1:18,3])) +
  xlab("Year") +
  ylab("Incidence per 1000 person years")+
  ggtitle("Model derivation cohort - Male") +
  theme(axis.title.y = element_blank(), text = element_text(size = 8))

## LOAD THE SECULAR TREND IN STATIN USERS DATA
load("secular_trend_statin_users.RData")
colnames(inc.tab.female)<-c("follow up (years)","events","incidence (per 1000 person years)")
female.stat<- ggplot() +
  geom_point(aes(x = 1998:2015,y = inc.tab.female[1:18,3])) +
  xlab("Year") +
  ylab("Incidence per 1000 person years") +
  ggtitle("Statin users cohort - Female")+
  theme(text = element_text(size = 8))


colnames(inc.tab.male)<-c("follow up (years)","events","incidence (per 1000 person years)")
male.stat <- ggplot() +
  geom_point(aes(x = 1998:2015,y = inc.tab.male[1:18,3])) +
  xlab("Year") +
  ylab("Incidence per 1000 person years") +
  ggtitle("Statin users cohort - Male") +
  theme(axis.title.y = element_blank(), text = element_text(size = 8))


Figure4 <- ggarrange(female.deriv,male.deriv,female.stat,male.stat,ncol=2, nrow=2)
Figure4
ggsave("Fig4.tiff", width = 5, height = 5, dpi = 300)
