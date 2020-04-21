### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

library(dplyr)
library(ggplot2)
library(ggpubr)

load("R_out_C6/runsim_male_RR065_discadj.RData")

res.list <- vector("list",5)
res.list[[1]] <- res.multiple.treatment.periods
res.list[[2]] <- res.multiple.treatment.periods.sixth
res.list[[3]] <- res.multiple.treatment.periods.third
res.list[[4]] <- res.multiple.treatment.periods.half
res.list[[5]] <- res.multiple.treatment.periods.nodisc

for (i in 1:5){
  res.list[[i]][[1]] <- NULL
  res.list[[i]][[1]] <- NULL
  res.list[[i]][[11]]<- NULL}

## Also remove the elements of age that are irrelevant
ages <- seq(25,85,5)
ages <-  ages[3:12]
ages

# names(risks) <- paste("Age ",ages,sep="")
# names(HRs) <- paste("Age ",ages,sep="")

## Now all I need to extract is the risks when initiating statins in different years of follow up
## Going to have a different plot for each element of disc

#### Will do age = 40 first

## First calculate lifetime risk no statin treatment
temp.1 <- run.simulation.agetime.male(1, 40, 1, 1, 1)
temp.2 <- run.simulation.agetime.male(2, 40, 1, 1, 1)
temp.3 <- run.simulation.agetime.male(3, 40, 1, 1, 1)
temp.4 <- run.simulation.agetime.male(4, 40, 1, 1, 1)
temp.5 <- run.simulation.agetime.male(5, 40, 1, 1, 1)
temp.6 <- run.simulation.agetime.male(6, 40, 1, 1, 1)

## Create a list to store the results
res.age.40 <- vector("list",5)

for (i in 1:5){
  res.age.40[[i]] <- res.list[[i]]$"Age 40"}

## Now create a new list, where each element will contain the risk, minus the risk from no statin treatment
## This should give us number of events prevented
res.age.40.adj <- vector("list",5)

for (i in 1:5){
  res.age.40.adj[[i]] <- res.age.40[[i]]}


## Then deduct it from the risk assuming no statin treatment
## I only go up to 20% if risks are beyond this
for (i in 1:5){
res.age.40.adj[[i]][1,] <- 100*(temp.1 - res.age.40[[i]][1,])
res.age.40.adj[[i]][2,] <- 100*(temp.2 - res.age.40[[i]][2,])
res.age.40.adj[[i]][3,] <- 100*(temp.3 - res.age.40[[i]][3,])
res.age.40.adj[[i]][4,] <- 100*(temp.4 - res.age.40[[i]][4,])
res.age.40.adj[[i]][5,] <- 100*(temp.5 - res.age.40[[i]][5,])
res.age.40.adj[[i]][6,] <- 100*(temp.6 - res.age.40[[i]][6,])}


# plot(40:89,res.age.40.adj[[i]][1,],type = "p", col="blue",ylim=c(0,8), cex = 0.7,xlab="Age at statin initiation",
#      ylab="Number of events prevented per 100 patients treated",main="Age 40")
# points(40:89,res.age.40.adj[[i]][2,] , col= "red", cex = 0.7)
# points(40:89,res.age.40.adj[[i]][3,],col="green", cex = 0.7)
# points(40:89,res.age.40.adj[[i]][4,],col="purple", cex = 0.7)
# legend(80,8,c("1%","2%","3%","4%"),col=c("blue","red","green","purple"),lwd=c(1,1,1,1),title = "10 year risk")
# 


### Next do age 50
### Next do age 50
### Next do age 50
temp.3 <- run.simulation.agetime.male(3, 50, 1, 1, 1)
temp.4 <- run.simulation.agetime.male(4, 50, 1, 1, 1)
temp.5 <- run.simulation.agetime.male(5, 50, 1, 1, 1)
temp.6 <- run.simulation.agetime.male(6, 50, 1, 1, 1)
temp.7 <- run.simulation.agetime.male(7, 50, 1, 1, 1)
temp.8 <- run.simulation.agetime.male(8, 50, 1, 1, 1)
temp.9 <- run.simulation.agetime.male(9, 50, 1, 1, 1)
temp.10 <- run.simulation.agetime.male(10, 50, 1, 1, 1)
temp.11 <- run.simulation.agetime.male(11, 50, 1, 1, 1)
temp.12 <- run.simulation.agetime.male(12, 50, 1, 1, 1)
temp.13 <- run.simulation.agetime.male(13, 50, 1, 1, 1)
temp.14 <- run.simulation.agetime.male(14, 50, 1, 1, 1)
temp.15 <- run.simulation.agetime.male(15, 50, 1, 1, 1)
temp.16 <- run.simulation.agetime.male(16, 50, 1, 1, 1)
temp.17 <- run.simulation.agetime.male(17, 50, 1, 1, 1)
temp.18 <- run.simulation.agetime.male(18, 50, 1, 1, 1)
temp.19 <- run.simulation.agetime.male(19, 50, 1, 1, 1)
temp.20 <- run.simulation.agetime.male(20, 50, 1, 1, 1)


## Create a list to store the results
res.age.50 <- vector("list",5)

for (i in 1:5){
  res.age.50[[i]] <- res.list[[i]]$"Age 50"}

## Now create a new list, where each element will contain the risk, minus the risk from no statin treatment
## This should give us number of events prevented
res.age.50.adj <- vector("list",5)

for (i in 1:5){
  res.age.50.adj[[i]] <- res.age.50[[i]]}

## Then deduct it from the risk assuming no statin treatment
## I only go up to 20% if risks are beyond this
for (i in 1:5){
res.age.50.adj[[i]][1,] <- 100*(temp.3 - res.age.50[[i]][1,])
res.age.50.adj[[i]][2,] <- 100*(temp.4 - res.age.50[[i]][2,])
res.age.50.adj[[i]][3,] <- 100*(temp.5 - res.age.50[[i]][3,])
res.age.50.adj[[i]][4,] <- 100*(temp.6 - res.age.50[[i]][4,])
res.age.50.adj[[i]][5,] <- 100*(temp.7 - res.age.50[[i]][5,])
res.age.50.adj[[i]][6,] <- 100*(temp.8 - res.age.50[[i]][6,])
res.age.50.adj[[i]][7,] <- 100*(temp.9 - res.age.50[[i]][7,])
res.age.50.adj[[i]][8,] <- 100*(temp.10 - res.age.50[[i]][8,])
res.age.50.adj[[i]][9,] <- 100*(temp.11 - res.age.50[[i]][9,])
res.age.50.adj[[i]][10,] <- 100*(temp.12 - res.age.50[[i]][10,])
res.age.50.adj[[i]][11,] <- 100*(temp.13 - res.age.50[[i]][11,])
res.age.50.adj[[i]][12,] <- 100*(temp.14 - res.age.50[[i]][12,])
res.age.50.adj[[i]][13,] <- 100*(temp.15 - res.age.50[[i]][13,])
res.age.50.adj[[i]][14,] <- 100*(temp.16 - res.age.50[[i]][14,])
res.age.50.adj[[i]][15,] <- 100*(temp.17 - res.age.50[[i]][15,])
res.age.50.adj[[i]][16,] <- 100*(temp.18 - res.age.50[[i]][16,])
res.age.50.adj[[i]][17,] <- 100*(temp.19 - res.age.50[[i]][17,])
res.age.50.adj[[i]][18,] <- 100*(temp.20 - res.age.50[[i]][18,])}


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
temp.7 <- run.simulation.agetime.male(7, 60, 1, 1, 1)
temp.8 <- run.simulation.agetime.male(8, 60, 1, 1, 1)
temp.9 <- run.simulation.agetime.male(9, 60, 1, 1, 1)
temp.10 <- run.simulation.agetime.male(10, 60, 1, 1, 1)
temp.11 <- run.simulation.agetime.male(11, 60, 1, 1, 1)
temp.12 <- run.simulation.agetime.male(12, 60, 1, 1, 1)
temp.13 <- run.simulation.agetime.male(13, 60, 1, 1, 1)
temp.14 <- run.simulation.agetime.male(14, 60, 1, 1, 1)
temp.15 <- run.simulation.agetime.male(15, 60, 1, 1, 1)
temp.16 <- run.simulation.agetime.male(16, 60, 1, 1, 1)
temp.17 <- run.simulation.agetime.male(17, 60, 1, 1, 1)
temp.18 <- run.simulation.agetime.male(18, 60, 1, 1, 1)
temp.19 <- run.simulation.agetime.male(19, 60, 1, 1, 1)
temp.20 <- run.simulation.agetime.male(20, 60, 1, 1, 1)


## Create a list to store the results
res.age.60 <- vector("list",5)

for (i in 1:5){
  res.age.60[[i]] <- res.list[[i]]$"Age 60"}

## Now create a new list, where each element will contain the risk, minus the risk from no statin treatment
## This should give us number of events prevented
res.age.60.adj <- vector("list",5)

for (i in 1:5){
  res.age.60.adj[[i]] <- res.age.60[[i]]}


## Then deduct it from the risk assuming no statin treatment
## I only go up to 20% if risks are beyond this
for (i in 1:5){
res.age.60.adj[[i]][1,] <- 100*(temp.7 - res.age.60[[i]][1,])
res.age.60.adj[[i]][2,] <- 100*(temp.8 - res.age.60[[i]][2,])
res.age.60.adj[[i]][3,] <- 100*(temp.9 - res.age.60[[i]][3,])
res.age.60.adj[[i]][4,] <- 100*(temp.10 - res.age.60[[i]][4,])
res.age.60.adj[[i]][5,] <- 100*(temp.11 - res.age.60[[i]][5,])
res.age.60.adj[[i]][6,] <- 100*(temp.12 - res.age.60[[i]][6,])
res.age.60.adj[[i]][7,] <- 100*(temp.13 - res.age.60[[i]][7,])
res.age.60.adj[[i]][8,] <- 100*(temp.14 - res.age.60[[i]][8,])
res.age.60.adj[[i]][9,] <- 100*(temp.15 - res.age.60[[i]][9,])
res.age.60.adj[[i]][10,] <- 100*(temp.16 - res.age.60[[i]][10,])
res.age.60.adj[[i]][11,] <- 100*(temp.17 - res.age.60[[i]][11,])
res.age.60.adj[[i]][12,] <- 100*(temp.18 - res.age.60[[i]][12,])
res.age.60.adj[[i]][13,] <- 100*(temp.19 - res.age.60[[i]][13,])
res.age.60.adj[[i]][14,] <- 100*(temp.20 - res.age.60[[i]][14,])}


# plot(60:89,disc.age.60.adj[3,],type = "p", col="blue",ylim=c(0,8), cex = 0.7,xlab="Age at statin initiation",
#      ylab="Number of events prevented per 100 patients treated",main="Age 60")
# points(60:89,disc.age.60.adj[5,], col= "red", cex = 0.7)
# points(60:89,disc.age.60.adj[7,],col="green", cex = 0.7)
# points(60:89,disc.age.60.adj[9,],col="purple", cex = 0.7)
# points(60:89,disc.age.60.adj[11,],col="black", cex = 0.7)
# points(60:89,disc.age.60.adj[13,],col="orange", cex = 0.7)
# legend(80,8,c("6%","8%","10%","12%","14%","16%","18%"),col=c("blue","red","green","purple","black","orange"),lwd=c(1,1,1,1,1,1),title = "10 year risk score")

## Check age ranges again
min.max.prange.age.agetime
## At age 40 they go from 1% to 7%
## At age 50 they go from 3% to 21%
## At age 60 they go from 7% to 39%


## For age 40, I decide to present patients with 3% risk
## Create datasets for plots, and save the ggplots
## Not using a y axis label, as I add this when arranging the plots
plot.data.40 <- data.frame("age" = rep(40:89,5),
                           "ev.prevented" = c(res.age.40.adj[[1]][3,],res.age.40.adj[[2]][3,], 
                                              res.age.40.adj[[3]][3,],res.age.40.adj[[4]][3,],
                                              res.age.40.adj[[5]][3,]),
                           "risk" = c(rep("Real data",50),rep("5/6",50),rep("2/3",50),rep("1/2",50),rep("No Disc",50)))
plot.data.40$risk <- factor(plot.data.40$risk, levels = c("Real data","5/6","2/3","1/2","No Disc"))

plot.40 <- ggplot(plot.data.40, aes(x = age, y = ev.prevented, col = risk)) + geom_point(size = 0.7) + xlab("Age at statin initiation") +
  ggtitle("Age 40 - 10 year 3% risk") + ylab("Events prevented") + theme(text = element_text(size = 10), plot.title = element_text(size = 10), 
                                       legend.key.size = unit(10,"point")) +
  labs(col = "Discont\ninuation\nrate") + scale_y_continuous(limits = c(0,16),breaks = seq(0,16,2)) 

## Age 50, for this I do patients with a 7% risk
plot.data.50 <- data.frame("age" = rep(50:89,5),
                           "ev.prevented" = c(res.age.50.adj[[1]][5,],res.age.50.adj[[2]][5,], 
                                              res.age.50.adj[[3]][5,],res.age.50.adj[[4]][5,],
                                              res.age.50.adj[[5]][5,]),
                           "risk" = c(rep("Real data",40),rep("5/6",40),rep("2/3",40),rep("1/2",40),rep("No Disc",40)))
plot.data.50$risk <- factor(plot.data.50$risk, levels = c("Real data","5/6","2/3","1/2","No Disc"))

plot.50 <- ggplot(plot.data.50, aes(x = age, y = ev.prevented, col = risk)) + geom_point(size = 0.7) + xlab("Age at statin initiation") +
  ggtitle("Age 50 - 10 year 7% risk") + ylab("Events prevented") + theme(text = element_text(size = 10), plot.title = element_text(size = 10), 
                                       legend.key.size = unit(10,"point")) +
  labs(col = "Discont\ninuation\nrate") + scale_y_continuous(limits = c(0,16),breaks = seq(0,16,2)) 


## Age 60, for this I do patients with a 12% risk
plot.data.60 <- data.frame("age" = rep(60:89,5),
                           "ev.prevented" = c(res.age.60.adj[[1]][6,],res.age.60.adj[[2]][6,], 
                                              res.age.60.adj[[3]][6,],res.age.60.adj[[4]][6,],
                                              res.age.60.adj[[5]][6,]),
                           "risk" = c(rep("Real data",30),rep("5/6",30),rep("2/3",30),rep("1/2",30),rep("No Disc",30)))
plot.data.60$risk <- factor(plot.data.60$risk, levels = c("Real data","5/6","2/3","1/2","No Disc"))

plot.60 <- ggplot(plot.data.60, aes(x = age, y = ev.prevented, col = risk)) + geom_point(size = 0.7) + xlab("Age at statin initiation") +
  ggtitle("Age 60 - 10 year 12% risk") + ylab("Events prevented") + theme(text = element_text(size = 10), plot.title = element_text(size = 10), 
                                       legend.key.size = unit(10,"point")) +
  labs(col = "Discont\ninuation\nrate") + scale_y_continuous(limits = c(0,16),breaks = seq(0,16,2)) 


comb.plot <- ggarrange(plot.40,plot.50,plot.60,nrow = 3, ncol = 1, 
                       common.legend = FALSE)
ggsave("figures/res_male_RR065_discadj.jpg",comb.plot, height = 9, width = 4.5)

rm(anal.dat, comp.data)

save.image("R_out_C6/res_male_RR065_discadj.RData")




