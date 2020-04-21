### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

library(dplyr)
library(ggpubr)
library(ggplot2)

load("R_out_C6/runsim_female_RR065_no1pres.RData")

str(res.multiple.treatment.periods)

## The first two elements of the output are NULL, as is the last, therefore I will remove them from the object
  res.multiple.treatment.periods[[1]] <- NULL
  res.multiple.treatment.periods[[1]] <- NULL
  res.multiple.treatment.periods[[11]] <- NULL

str(res.multiple.treatment.periods)

## Also remove the elements of age that are irrelevant
ages <- seq(25,85,5)
ages <-  ages[3:12]
ages

## Now all I need to extract is the risks when initiating statins in different years of follow up
## And I need to deduct from this the risks assuming no statin treatment was done
## Will do each age seperately

## Only going to do ages 40/50/60 because its all I am going to report in the paper


## First check the range of risks for each age
min.max.prange.age.agetime

## At age 40 they go from 1% to 4%
## At age 50 they go from 2% to 10%
## At age 60 they go from 3% to 20%

#### Will do age = 40 first

## First calculate lifetime risk no statin treatment
temp.1 <- run.simulation.agetime(1, 40, 1, 1, 1)
temp.2 <- run.simulation.agetime(2, 40, 1, 1, 1)
temp.3 <- run.simulation.agetime(3, 40, 1, 1, 1)
temp.4 <- run.simulation.agetime(4, 40, 1, 1, 1)

## Define the risks
## This is the risks for each risk category (1 - 4%) if you initiate statins during each year of follow up
disc.age.40 <- res.multiple.treatment.periods$"Age 40"

## Now create a new list, where each element will contain the risk, deducted from the risk from no statin treatment
## This should give us number of events prevented
# Start by setting it to be the same as the output from the same
disc.age.40.adj <- disc.age.40
str(disc.age.40.adj)

## Now deduct from each row from the risk assuming no statin treatment
disc.age.40.adj[1,] <- 100*(temp.1 - disc.age.40[1,])
disc.age.40.adj[2,] <- 100*(temp.2 - disc.age.40[2,])
disc.age.40.adj[3,] <- 100*(temp.3 - disc.age.40[3,])
disc.age.40.adj[4,] <- 100*(temp.4 - disc.age.40[4,])


# plot(40:89,disc.age.40.adj[1,],type = "p", col="blue",ylim=c(0,8), cex = 0.7,xlab="Age at statin initiation",
#      ylab="Number of events prevented per 100 patients treated",main="Age 40")
# points(40:89,disc.age.40.adj[2,] , col= "red", cex = 0.7)
# points(40:89,disc.age.40.adj[3,],col="green", cex = 0.7)
# points(40:89,disc.age.40.adj[4,],col="purple", cex = 0.7)
# legend(80,8,c("1%","2%","3%","4%"),col=c("blue","red","green","purple"),lwd=c(1,1,1,1),title = "10 year risk")
# 


### Next do age 50
### Next do age 50
### Next do age 50
temp.2 <- run.simulation.agetime(2, 50, 1, 1, 1)
temp.3 <- run.simulation.agetime(3, 50, 1, 1, 1)
temp.4 <- run.simulation.agetime(4, 50, 1, 1, 1)
temp.5 <- run.simulation.agetime(5, 50, 1, 1, 1)
temp.6 <- run.simulation.agetime(6, 50, 1, 1, 1)
temp.7 <- run.simulation.agetime(7, 50, 1, 1, 1)
temp.8 <- run.simulation.agetime(8, 50, 1, 1, 1)
temp.9 <- run.simulation.agetime(9, 50, 1, 1, 1)
temp.10 <- run.simulation.agetime(10, 50, 1, 1, 1)


## Define the risks
disc.age.50 <- res.multiple.treatment.periods$"Age 50"

## Deduct these from the various risks with no statin treatment
disc.age.50.adj <- disc.age.50

## Then deduct it from the risk assuming no statin treatment
## I only go up to 20% if risks are beyond this
disc.age.50.adj[1,] <- 100*(temp.2 - disc.age.50[1,])
disc.age.50.adj[2,] <- 100*(temp.3 - disc.age.50[2,])
disc.age.50.adj[3,] <- 100*(temp.4 - disc.age.50[3,])
disc.age.50.adj[4,] <- 100*(temp.5 - disc.age.50[4,])
disc.age.50.adj[5,] <- 100*(temp.6 - disc.age.50[5,])
disc.age.50.adj[6,] <- 100*(temp.7 - disc.age.50[6,])
disc.age.50.adj[7,] <- 100*(temp.8 - disc.age.50[7,])
disc.age.50.adj[8,] <- 100*(temp.9 - disc.age.50[8,])
disc.age.50.adj[9,] <- 100*(temp.10 - disc.age.50[9,])


# plot(50:89,disc.age.50.adj[1,],type = "p", col="blue", ylim=c(0,8), cex = 0.7,xlab="Age at statin initiation",
#      ylab="Number of events prevented per 100 patients treated",main="Age 50")
# points(50:89,disc.age.50.adj[3,], col= "red", cex = 0.7)
# points(50:89,disc.age.50.adj[5,],col="green", cex = 0.7)
# points(50:89,disc.age.50.adj[7,],col="purple", cex = 0.7)
# points(50:89,disc.age.50.adj[9,],col="black", cex = 0.7)
# legend(80,8,c("2%","4%","6%","8%","10%"),col=c("blue","red","green","purple","black"),lwd=c(1,1,1,1,1),title ="10 year risk")



### Next do age 60
### Next do age 60
### Next do age 60
temp.3 <- run.simulation.agetime(3, 60, 1, 1, 1)
temp.4 <- run.simulation.agetime(4, 60, 1, 1, 1)
temp.5 <- run.simulation.agetime(5, 60, 1, 1, 1)
temp.6 <- run.simulation.agetime(6, 60, 1, 1, 1)
temp.7 <- run.simulation.agetime(7, 60, 1, 1, 1)
temp.8 <- run.simulation.agetime(8, 60, 1, 1, 1)
temp.9 <- run.simulation.agetime(9, 60, 1, 1, 1)
temp.10 <- run.simulation.agetime(10, 60, 1, 1, 1)
temp.11 <- run.simulation.agetime(11, 60, 1, 1, 1)
temp.12 <- run.simulation.agetime(12, 60, 1, 1, 1)
temp.13 <- run.simulation.agetime(13, 60, 1, 1, 1)
temp.14 <- run.simulation.agetime(14, 60, 1, 1, 1)
temp.15 <- run.simulation.agetime(15, 60, 1, 1, 1)
temp.16 <- run.simulation.agetime(16, 60, 1, 1, 1)
temp.17 <- run.simulation.agetime(17, 60, 1, 1, 1)
temp.18 <- run.simulation.agetime(18, 60, 1, 1, 1)
temp.19 <- run.simulation.agetime(19, 60, 1, 1, 1)
temp.20 <- run.simulation.agetime(20, 60, 1, 1, 1)


## Define the risks
disc.age.60 <- res.multiple.treatment.periods$"Age 60"


## Deduct these from the various risks with no statin treatment
disc.age.60.adj <- disc.age.60

## Then deduct it from the risk assuming no statin treatment
## I only go up to 20% if risks are beyond this
disc.age.60.adj[1,] <- 100*(temp.3 - disc.age.60[1,])
disc.age.60.adj[2,] <- 100*(temp.4 - disc.age.60[2,])
disc.age.60.adj[3,] <- 100*(temp.5 - disc.age.60[3,])
disc.age.60.adj[4,] <- 100*(temp.6 - disc.age.60[4,])
disc.age.60.adj[5,] <- 100*(temp.7 - disc.age.60[5,])
disc.age.60.adj[6,] <- 100*(temp.8 - disc.age.60[6,])
disc.age.60.adj[7,] <- 100*(temp.9 - disc.age.60[7,])
disc.age.60.adj[8,] <- 100*(temp.10 - disc.age.60[8,])
disc.age.60.adj[9,] <- 100*(temp.11 - disc.age.60[9,])
disc.age.60.adj[10,] <- 100*(temp.12 - disc.age.60[10,])
disc.age.60.adj[11,] <- 100*(temp.13 - disc.age.60[11,])
disc.age.60.adj[12,] <- 100*(temp.14 - disc.age.60[12,])
disc.age.60.adj[13,] <- 100*(temp.15 - disc.age.60[13,])
disc.age.60.adj[14,] <- 100*(temp.16 - disc.age.60[14,])
disc.age.60.adj[15,] <- 100*(temp.17 - disc.age.60[15,])
disc.age.60.adj[16,] <- 100*(temp.18 - disc.age.60[16,])
disc.age.60.adj[17,] <- 100*(temp.19 - disc.age.60[17,])
disc.age.60.adj[18,] <- 100*(temp.20 - disc.age.60[18,])



# plot(60:89,disc.age.60.adj[3,],type = "p", col="blue",ylim=c(0,8), cex = 0.7,xlab="Age at statin initiation",
#      ylab="Number of events prevented per 100 patients treated",main="Age 60")
# points(60:89,disc.age.60.adj[5,], col= "red", cex = 0.7)
# points(60:89,disc.age.60.adj[7,],col="green", cex = 0.7)
# points(60:89,disc.age.60.adj[9,],col="purple", cex = 0.7)
# points(60:89,disc.age.60.adj[11,],col="black", cex = 0.7)
# points(60:89,disc.age.60.adj[13,],col="orange", cex = 0.7)
# legend(80,8,c("6%","8%","10%","12%","14%","16%","18%"),col=c("blue","red","green","purple","black","orange"),lwd=c(1,1,1,1,1,1),title = "10 year risk score")

save.image("res_female_RR07_no1pres.RData")

## Create datasets for plots, and save the ggplots
## Not using a y axis label, as I add this when arranging the plots
plot.data.40 <- data.frame("age" = rep(40:89,4),
                           "ev.prevented" = c(disc.age.40.adj[1,],disc.age.40.adj[2,], 
                                                                 disc.age.40.adj[3,],disc.age.40.adj[4,]),
                           "risk" = c(rep("1%",50),rep("2%",50),rep("3%",50),rep("4%",50)))
plot.data.40$risk <- factor(plot.data.40$risk, levels = c("1%","2%","3%","4%"))

plot.40 <- ggplot(plot.data.40, aes(x = age, y = ev.prevented, col = risk)) + geom_point(size = 0.7) + xlab("Age at statin initiation") +
  ggtitle("Age 40") + ylab("Events prevented") + theme(text = element_text(size = 10), plot.title = element_text(size = 10), 
                                       legend.key.size = unit(10,"point")) +
  labs(col = "10 year\nrisk") + scale_y_continuous(limits = c(0,12),breaks = seq(0,12,2)) 


plot.data.50 <- data.frame("age" = rep(50:89,5),
                           "ev.prevented" = c(disc.age.50.adj[1,],disc.age.50.adj[3,], 
                                              disc.age.50.adj[5,],disc.age.50.adj[7,],
                                              disc.age.50.adj[9,]),
                           "risk" = c(rep("2%",40),rep("4%",40),rep("6%",40),rep("8%",40),rep("10%",40)))
plot.data.50$risk <- factor(plot.data.50$risk, levels = c("2%","4%","6%","8%","10%"))

plot.50 <- ggplot(plot.data.50, aes(x = age, y = ev.prevented, col = risk)) + geom_point(size = 0.7) + xlab("Age at statin initiation") +
  ggtitle("Age 50") + ylab("Events prevented") + theme(text = element_text(size = 10), plot.title = element_text(size = 10), 
                                       legend.key.size = unit(10,"point")) +
  labs(col = "10 year\nrisk") + scale_y_continuous(limits = c(0,12),breaks = seq(0,12,2)) 
plot.50


plot.data.60 <- data.frame("age" = rep(60:89,6),
                           "ev.prevented" = c(disc.age.60.adj[2,],disc.age.60.adj[4,], 
                                              disc.age.60.adj[6,],disc.age.60.adj[8,],
                                              disc.age.60.adj[10,],disc.age.60.adj[12,]),
                           "risk" = c(rep("4%",30),rep("6%",30),rep("8%",30),rep("10%",30),rep("12%",30),rep("14%",30)))
plot.data.60$risk <- factor(plot.data.60$risk, levels = c("4%","6%","8%","10%","12%","14%"))

plot.60 <- ggplot(plot.data.60, aes(x = age, y = ev.prevented, col = risk)) + geom_point(size = 0.7) + xlab("Age at statin initiation") +
  ggtitle("Age 60") + ylab("Events prevented") + theme(text = element_text(size = 10), plot.title = element_text(size = 10), 
                                       legend.key.size = unit(10,"point")) + 
  labs(col = "10 year\nrisk") + scale_y_continuous(limits = c(0,12),breaks = seq(0,12,2)) 
plot.60


comb.plot <- ggarrange(plot.40,plot.50,plot.60,nrow = 3, ncol = 1, 
                                     common.legend = FALSE)
ggsave("figures/res_female_RR065_no1pres.jpg",comb.plot, height = 9, width = 4.5)

save.image("R_out_C6/res_female_RR065_no1pres.RData")
print("image saved")