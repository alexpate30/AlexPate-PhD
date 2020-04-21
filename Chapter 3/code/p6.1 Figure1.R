library(dplyr)
library(knitr)
library(reshape2)
library(ggplot2)
library(ggpubr)

rm(list=ls())

setwd("B")
load("calibration_female_split.RData")

plot.data<-data.frame(percentile=1:10,km.all,predrisk.all)
plot.data<-100*plot.data
plot.data$percentile <- (plot.data$percentile)/10


## All patients
temp.dat<-select(plot.data,c("percentile","km.all","predrisk.all"))
colnames(temp.dat)<-c("percentile","observed","predicted")

# Now reshapre into long format
temp.dat<-melt(temp.dat,id="percentile")

# Now plot
p<-ggplot(temp.dat) 
p+ geom_point(aes(x=percentile,y=value,shape=variable)) + scale_shape_manual(values=c(1,19)) + ggtitle("Female cohort") + 
  ylab("Average 10 year risk") + 
  theme(legend.title = element_blank())
a <- ggplot(temp.dat) + geom_point(aes(x=percentile,y=value,shape=variable), size = 1.5) + scale_shape_manual(values=c(1,19)) + ggtitle("Female cohort") + 
  ylab("Average 10 year risk") + xlab("Percentile") +
  theme(legend.title = element_blank(), text = element_text(size=10))

ggsave("CalibrationFemale.jpeg", dpi = 600)

setwd("B")
load("calibration_male_split.RData")

plot.data<-data.frame(percentile=1:10,km.all,predrisk.all)
plot.data<-100*plot.data
plot.data$percentile <- (plot.data$percentile)/10


## All patients
temp.dat<-select(plot.data,c("percentile","km.all","predrisk.all"))
colnames(temp.dat)<-c("percentile","observed","predicted")

# Now reshapre into long format
temp.dat<-melt(temp.dat,id="percentile")

# Now plot
p<-ggplot(temp.dat) 
p+ geom_point(aes(x=percentile,y=value,shape=variable)) + scale_shape_manual(values=c(1,19)) + ggtitle("Male cohort") + 
  ylab("Average 10 year risk") + xlab("Percentile") +
  theme(legend.title = element_blank())

b <- ggplot(temp.dat) + geom_point(aes(x=percentile,y=value,shape=variable), size = 1.5) + scale_shape_manual(values=c(1,19)) +
  ggtitle("Male cohort") + 
  ylab("Average 10 year risk") + xlab("Percentile") + 
  theme(legend.title = element_blank() , text = element_text(size=10))

ggsave("CalibrationMale.jpeg", dpi = 600)


Figure1 <- ggarrange(a,b,nrow = 1, ncol = 2, common.legend = TRUE, legend = "bottom")
Figure1

ggsave("Fig1.tiff", dpi = 300, width = 7, height = 3.5)
