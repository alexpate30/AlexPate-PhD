### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

library(pracma)
library(survminer)
library(survival)

load("R_out_C6/statin_discontinuation_rates_for_sim_continuous_multiple_treatment_periods_agemod_FAKE.RData")


### I am now going to create the quantiles list for each disc1, disc2, disc3, but the rate is reduced by 1/6, 1/3, 1/2 and no discontinuation.

## Initial discontinuation
## Initial discontinuation

## For this I am going to need another 4 lists of size 50
disc1.extrapolated.half <- vector("list",50)
disc1.extrapolated.third <- vector("list",50)
disc1.extrapolated.sixth <- vector("list",50)
disc1.extrapolated.nodisc <- vector("list",50)

for (i in 1:50){
## Quantiles adjusted so that discontinuation rate is havled
disc1.extrapolated.half[[i]] <- disc1.extrapolated[[i]][c(TRUE,FALSE)]
disc1.extrapolated.half[[i]] <- c(disc1.extrapolated.half[[i]],rep(NA,5000))

## Quantiles adjusted so that discontinuation rate is a third
disc1.extrapolated.third[[i]] <- disc1.extrapolated[[i]][-(2+seq(1,10001,3))]
disc1.extrapolated.third[[i]]  <- c(disc1.extrapolated.third[[i]] ,rep(NA,3333))

## Quantiles adjusted so that discontinuation rate is sixth
disc1.extrapolated.sixth[[i]] <- disc1.extrapolated[[i]][-(5+seq(1,10001,6))]
disc1.extrapolated.sixth[[i]] <- c(disc1.extrapolated.sixth[[i]],rep(NA,1666))

## Quantiles adjusted so that discontinuation rate no discontinuation
disc1.extrapolated.nodisc[[i]] <- rep(NA,10001)}

plot(disc1.extrapolated[[30]],seq(0,1,0.0001),cex=0.5)
points(disc1.extrapolated.half[[30]],seq(0,1,0.0001),cex=0.5,col="blue")
points(disc1.extrapolated.third[[30]],seq(0,1,0.0001),cex=0.5,col="green")
points(disc1.extrapolated.sixth[[30]],seq(0,1,0.0001),cex=0.5,col="purple")


## Second discontinuation
## Second discontinuation

## Quantiles adjusted so that discontinuation rate is havled
plot(disc2.extrapolated,seq(0,1,0.0001),cex=0.5)

disc2.extrapolated.half <- disc2.extrapolated[c(TRUE,FALSE)]
disc2.extrapolated.half <- c(disc2.extrapolated.half,rep(NA,5000))
points(disc2.extrapolated.half,seq(0,1,0.0001),cex=0.5,col="blue")


disc2.extrapolated.third <- disc2.extrapolated[-(2+seq(1,10001,3))]
disc2.extrapolated.third <- c(disc2.extrapolated.third,rep(NA,3333))
points(disc2.extrapolated.third,seq(0,1,0.0001),cex=0.5,col="green")


disc2.extrapolated.sixth <- disc2.extrapolated[-(5+seq(1,10001,6))]
disc2.extrapolated.sixth <- c(disc2.extrapolated.sixth,rep(NA,1666))
points(disc2.extrapolated.sixth,seq(0,1,0.0001),cex=0.5,col="purple")

disc2.extrapolated.nodisc <- rep(NA,10001)



## Third discontinuation
## Third discontinuation

## Quantiles adjusted so that discontinuation rate is havled
plot(disc3.extrapolated,seq(0,1,0.0001),cex=0.5)

disc3.extrapolated.half <- disc3.extrapolated[c(TRUE,FALSE)]
disc3.extrapolated.half <- c(disc3.extrapolated.half,rep(NA,5000))
points(disc3.extrapolated.half,seq(0,1,0.0001),cex=0.5,col="blue")


disc3.extrapolated.third <- disc3.extrapolated[-(2+seq(1,10001,3))]
disc3.extrapolated.third <- c(disc3.extrapolated.third,rep(NA,3333))
points(disc3.extrapolated.third,seq(0,1,0.0001),cex=0.5,col="green")


disc3.extrapolated.sixth <- disc3.extrapolated[-(5+seq(1,10001,6))]
disc3.extrapolated.sixth <- c(disc3.extrapolated.sixth,rep(NA,1666))
points(disc3.extrapolated.sixth,seq(0,1,0.0001),cex=0.5,col="purple")

disc3.extrapolated.nodisc <- rep(NA,10001)

### Combine these into lists that can be used in the simulation/for better organisation
disc1.extrapolated.list <- vector("list",5)
disc1.extrapolated.list[[1]] <- disc1.extrapolated
disc1.extrapolated.list[[2]] <- disc1.extrapolated.sixth
disc1.extrapolated.list[[3]] <- disc1.extrapolated.third
disc1.extrapolated.list[[4]] <- disc1.extrapolated.half
disc1.extrapolated.list[[5]] <- disc1.extrapolated.nodisc

disc2.extrapolated.list <- vector("list",5)
disc2.extrapolated.list[[1]] <- disc2.extrapolated
disc2.extrapolated.list[[2]] <- disc2.extrapolated.sixth
disc2.extrapolated.list[[3]] <- disc2.extrapolated.third
disc2.extrapolated.list[[4]] <- disc2.extrapolated.half
disc2.extrapolated.list[[5]] <- disc2.extrapolated.nodisc

disc3.extrapolated.list <- vector("list",5)
disc3.extrapolated.list[[1]] <- disc3.extrapolated
disc3.extrapolated.list[[2]] <- disc3.extrapolated.sixth
disc3.extrapolated.list[[3]] <- disc3.extrapolated.third
disc3.extrapolated.list[[4]] <- disc3.extrapolated.half
disc3.extrapolated.list[[5]] <- disc3.extrapolated.nodisc

rm(list=setdiff(ls(),list("disc1.extrapolated.list","disc2.extrapolated.list","disc3.extrapolated.list",
                          "restart1.extrapolated","restart2.extrapolated")))

save.image("R_out_C6/statin_discontinuation_rates_for_sim_continuous_multiple_treatment_periods_rates_agemod_FAKE.RData")

