### This program plots Figure 3 from the main manuscript
rm(list=ls())

library(tibble)
library(ggplot2)
library(reshape)

## Load the tables from the cohort 2016 analysis
setwd("B")
load("female_tables_cohort2016.RData")

## For each group, calculate the propertion over and under 10% according to each model
prop.less10<-(-cbind(100-table5.prop[3:7,1:10],table5.prop[3:7,11:21]))
prop.over10<-cbind(table5.prop[3:7,1:10],100-table5.prop[3:7,11:21])

## Combine into one matrix
prop.over.under<-rbind(prop.less10,prop.over10)

str(prop.over.under)

## Turn into a dataframe and assign row and column names
prop.over.under.mat<-data.frame(prop.over.under)
prop.over.under.mat<-cbind(prop.over.under.mat,c("< 10%","< 10%","< 10%","< 10%","< 10%","> 10%",
                                                  "> 10%","> 10%","> 10%","> 10%"))
colnames(prop.over.under.mat)<-c("0-1%","1-2%","2-3%","3-4%","4-5%","5-6%","6-7%","7-8%","8-9%","9-10%",
                                 "10-11%","11-12%","12-13%","13-14%","14-15%","15-16%","16-17%","17-18%","18-19%","19-20%","> 20%","Group")

## Extract the specific risk groups we want for the plot
plot.dat<-prop.over.under.mat[c(5,10),6:22]

## Get into format that can be used in ggplot
plot.dat.female<-melt(plot.dat,id=c("Group"))
colnames(plot.dat.female)[2]<-c("risk.group")


## Repeat process for male cohort
setwd("B")
load("male_tables_generated_cohort2016_final.RData")


prop.less10<-(-cbind(100-table5.prop[3:7,1:10],table5.prop[3:7,11:21]))
prop.over10<-cbind(table5.prop[3:7,1:10],100-table5.prop[3:7,11:21])

prop.over.under<-rbind(prop.less10,prop.over10)

str(prop.over.under)

prop.over.under.mat<-data.frame(prop.over.under)
prop.over.under.mat<-cbind(prop.over.under.mat,c("< 10%","< 10%","< 10%","< 10%","< 10%","> 10%",
                                                 "> 10%","> 10%","> 10%","> 10%"))
colnames(prop.over.under.mat)<-c("0-1%","1-2%","2-3%","3-4%","4-5%","5-6%","6-7%","7-8%","8-9%","9-10%",
                                 "10-11%","11-12%","12-13%","13-14%","14-15%","15-16%","16-17%","17-18%","18-19%","19-20%","> 20%","Group")

plot.dat<-prop.over.under.mat[c(5,10),6:22]

plot.dat.male<-melt(plot.dat,id=c("Group"))
colnames(plot.dat.male)[2]<-c("risk.group")

## Define function for labels
abs_format <- function(){function(x){
  abs(x)}
  }

## Do male and female plots seperately
ggplot() + 
  geom_bar(data=plot.dat.female, aes(x=risk.group, y=value, fill = Group), stat="identity") + xlab("Risk group according to model A") +
  ylab(expression(atop("Percentage of risk group above and",paste("below threshold (model F)")))) + 
  scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100), labels = abs_format()) +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Female cohort")

ggsave("ProportionAboveBelowFemale.jpeg", dpi = 600)

ggplot() + 
  geom_bar(data=plot.dat.male, aes(x=risk.group, y=value, fill = Group), stat="identity") + xlab("Risk group according to model A") +
  ylab(expression(atop("Percentage of risk group above and",paste("below threshold (model F)")))) + 
  scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100), labels = abs_format()) +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Male cohort")

ggsave("ProportionAboveBelowMale.jpeg", dpi = 600)


## Going to create two new objects slightly different
# No legend on graph 1, no y axis label on graph 2, so can combine
a <- ggplot() + 
  geom_bar(data=plot.dat.female, aes(x=risk.group, y=value, fill = Group), stat="identity") + xlab("Risk group according to model A") +
  ylab(expression(atop("Percentage of risk group above and",paste("below threshold (model F)")))) + 
  scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100), labels = abs_format()) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 10), legend.text = element_text(size = 10)) + 
  ggtitle("Female cohort")

b <- ggplot() + 
  geom_bar(data=plot.dat.male, aes(x=risk.group, y=value, fill = Group), stat="identity") + xlab("Risk group according to model A") +
  ylab(expression(atop("Percentage of risk group above and",paste("below threshold (model F)")))) + 
  scale_y_continuous(breaks=seq(-100,100,10), limits=c(-100,100), labels = abs_format()) +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size = 10), legend.text = element_text(size = 10)) + 
  ggtitle("Male cohort")



library(ggpubr)

## Now combine into one graph and use the common legend function
Figure3 <- ggarrange(a,b,nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
Figure3
ggsave("Fig3.tiff", width = 7, height = 3.5, dpi = 300)
