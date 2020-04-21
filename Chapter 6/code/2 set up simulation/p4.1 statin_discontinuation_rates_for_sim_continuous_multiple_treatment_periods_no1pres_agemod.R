### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

load("R_out_C6/survival_curves_no1pres_1year_restricted_agemod_FAKE.RData")

## The survival objets of interest are:
# disc1.survival.curves (this is a list of survival objects, for initial discontinuation rate stratified by age)

# surv.obj.disc2 (these are all subsequent survival objects for second/third discontinuation and 1st and 2nd restarting)
# surv.obj.disc3
# surv.obj.restart
# surv.obj.restart2


### However, I have also got the discontinuation and restarting rates stratified by age if required for subsequent treatment periods
# disc2.survival.curves
# disc3.survival.curves
# restart.survival.curves
# restart2.survival.curves

###### Extrapolation of first discontinuation curves
###### Extrapolation of first discontinuation curves
###### Extrapolation of first discontinuation curves

### First I want to find at what point I want to extrapolate from (i.e. when amount of data becomes small and 
### event probabilities erratic)

## Assign a survival curve (note they are all the same just with a multiplier, so for assessing a trend can pick any
## of the 50 different survival curves)
survival.curve <- disc1.survival.curves[[30]]

## First calculate quantiles and a survival table from the survival object
disc.quantiles <- quantile(survival.curve, probs = seq(0,1,0.0001))
disc.summary <-surv_summary(survival.curve)

## And then extract rows at yearly intervals, will use these to calculate yeary probability of an event
disc.summary.reduc <- disc.summary[disc.summary$time %in% round(seq(365.25, 7305, 365.25)),]

## Get event probabilities by year
event.probs <- rep(0,17)
event.probs[1] <- 1 - disc.summary.reduc$surv[1]
for (i in 2:17){event.probs[i] <- 1-disc.summary.reduc$surv[i]/disc.summary.reduc$surv[i-1]}

plot(1:17,event.probs)

rm(event.probs, disc.quantiles, disc.summary,disc.summary.reduc,survival.curve)

## Can see the normal trend stops at 14 years with a sharp increase in the probaility of discontinuing
## This makes no sense and assume it is due to informative censoring and small sample size
## Therefore I will take the cumulative probability of an event in year 13
## I will then get a daily probabilty of an event (assuming probability of event is constant)
## This will be done within the function to follow

##### DEFINITION OF FUNCTION FOR EXTRAPOLATION ######

### This function extrapolates the survival curve from year 13 onwards. 
### It assumes a constant daily probability of discontinuation from this point onwards

extrapolate.quantiles <- function(survival.curve){
  
  ## First calculate quantiles and a survival table from the survival object
  disc.quantiles <- quantile(survival.curve, probs = seq(0,1,0.0001))
  disc.summary <-surv_summary(survival.curve)
  
  ## And then extract rows at yearly intervals, will use these to calculate yeary probability of an event
  disc.summary.reduc <- disc.summary[disc.summary$time %in% round(seq(365.25, 7305, 365.25)),]
  
  ## Get event probabilities by year
  event.probs <- rep(0,17)
  event.probs[1] <- 1 - disc.summary.reduc$surv[1]
  for (i in 2:17){event.probs[i] <- 1-disc.summary.reduc$surv[i]/disc.summary.reduc$surv[i-1]}
  
  ## Given the data size at this point, I will exclude the final year and take the mean of years 12 - 16 
  # to get the yearly discontinuation rate. The yearly event rate is also constant across this point
  extrapolate.yearly.disc.rate <- event.probs[13]
  
  ## Now calculate if this is the yearly probability, what is the probability on a given day
  prob <- 1 - nthroot(1-extrapolate.yearly.disc.rate,364)
  
  
  ## Now after 16 years, this is the 16*365.25 = 5844th day
  ## I want to find which quantile this day represents (i.e. is it q0.5 ? q0.08?)
  day4748 <- min(which(disc.quantiles$quantile > 4748))
  
  ## I want to extrapolate from this quantile forwards
  ## To do this I will calculate the probability of having had an event by each day of follow up, then when the probability crosses
  ## a 0.0001 threshold, we have our next 'quantile'
  
  # Create a vector which will store the probability of having an event before each subsequent day
  # I use the probability of day4748 as a jump off, this is the cumulative probability of having an event before 5845 days
  
  # Create a vector of the maximum length of the simulation, minus 16 years, which is where we are up to
  new.probs <- rep(0,60*365.25-13*365.25)
  
  # Probability starts of the quantile equating to day 4748
  new.probs[1] <- day4748/10000
  
  # The probbility of having an event by each subsequent day, is the cumulative probability of the day before, plus the probability of
  # not having the event up to that point multiplied by the probability of having the  event on that day (prob)
  # i.e. probability of having an event before day 5846, is the probability of having before 5845, plus (1-prob before 5845)*(conditional prob each year)
  for (i in 2:length(new.probs)){new.probs[i] <- new.probs[i-1] + (1-new.probs[i-1])*prob}
  
  
  # Create a vector of the days which these probabilities correspond to, 
  # the days gives 60 years follow up (allows to start simulations at age 30)
  new.days <- 4749:21915
  
  ## Create somewhere to store the extrapolated quantiles, that we will add to the current quantiles
  ## We already have quantile up until day4748, meaning there are 10000 - day4748 + 1 quantiles left to fill
  new.quantile <- rep(0,(10000 - day4748 + 1))
  
  ## Now derive the day which corresponds to each quantile and assign it to the new quantiles vector
  for (i in 1:(10000 - day4748 + 1)){
    new.quantile[i] <- new.days[min(which(new.probs > (new.probs[1]+i/10000)))]
  }
  
  head(new.quantile)
  
  ## Combine with the original quantiles, to create the extrapolated set of quantiles, that goes beyond 16 years
  disc.quantiles.all <- c(as.numeric(disc.quantiles$quantile)[1:day4748], new.quantile)
  
  return(disc.quantiles.all)}

### Create a list to store extrapolated quantiles (one for each age group)
disc1.extrapolated <- vector("list",50)

### Then do the extrapolation
for (i in 1:50){disc1.extrapolated[[i]] <- extrapolate.quantiles(disc1.survival.curves[[i]])}

plot(disc1.extrapolated[[1]],seq(0,1,0.0001),type = "l", lwd = 1)
lines(disc1.extrapolated[[11]],seq(0,1,0.0001),type = "l", lwd = 1,col="red")
lines(disc1.extrapolated[[21]],seq(0,1,0.0001),type = "l", lwd = 1,col="blue")
lines(disc1.extrapolated[[31]],seq(0,1,0.0001),type = "l", lwd = 1,col="green")
lines(disc1.extrapolated[[41]],seq(0,1,0.0001),type = "l", lwd = 1,col="yellow")



###########################################################################
###########################################################################
###########################################################################
###########################################################################

##### Now do extrapolation for remaining discontinuation and restarting periods, which 
## will not be stratified by age

#### For these I will extrapolate from the 10th year onwards, as there is less data and we see 
#### an inconsistent event probability from this year onwards

## Assign a survival curve (note they are all the same just with a multiplier, so for assessing a trend can pick any
## of the 50 different survival curves)
survival.curve <- surv.obj.disc3

## First calculate quantiles and a survival table from the survival object
disc.quantiles <- quantile(survival.curve, probs = seq(0,1,0.0001))
disc.summary <-surv_summary(survival.curve)

## And then extract rows at yearly intervals, will use these to calculate yeary probability of an event
disc.summary.reduc <- disc.summary[disc.summary$time %in% round(seq(365.25, 7305, 365.25)),]

## Get event probabilities by year
event.probs <- rep(0,17)
event.probs[1] <- 1 - disc.summary.reduc$surv[1]
for (i in 2:17){event.probs[i] <- 1-disc.summary.reduc$surv[i]/disc.summary.reduc$surv[i-1]}

plot(1:17,event.probs)

rm(event.probs, disc.quantiles, disc.summary,disc.summary.reduc,survival.curve)



### This function extrapolates the survival curve from year 10 onwards. 
### It assumes a constant daily probability of discontinuation from this point onwards

extrapolate.quantiles.10y <- function(survival.curve){
  
  ## First calculate quantiles and a survival table from the survival object
  disc.quantiles <- quantile(survival.curve, probs = seq(0,1,0.0001))
  disc.summary <-surv_summary(survival.curve)
  
  ## Not all the multiples of 365.25 have an event time in the dataset, so need to look for the nearest ones
  positions.yearly <- rep(0,10)
  for (i in 1:10){
    positions.yearly[i] <- min(which(disc.summary$time > round(seq(365.25, 7305, 365.25))[i]))}
  
  ## Now extract the probabilty table at yearly intervals, will use these to calculate yeary probability of an event
  disc.summary.reduc <- disc.summary[positions.yearly,]
  
  
  ## Get event probabilities by year
  event.probs <- rep(0,10)
  event.probs[1] <- 1 - disc.summary.reduc$surv[1]
  for (i in 2:10){event.probs[i] <- 1-disc.summary.reduc$surv[i]/disc.summary.reduc$surv[i-1]}
  
  ## Given the data size at this point, I will exclude the final year and take the mean of years 12 - 16 
  # to get the yearly discontinuation rate. The yearly event rate is also constant across this point
  extrapolate.yearly.disc.rate <- event.probs[10]
  
  ## Now calculate if this is the yearly probability, what is the probability on a given day
  prob <- 1 - nthroot(1-extrapolate.yearly.disc.rate,364)
  
  
  ## Now after 16 years, this is the 16*365.25 = 3652th day
  ## I want to find which quantile this day represents (i.e. is it q0.5 ? q0.08?)
  day3652 <- min(which(disc.quantiles$quantile > 3652))
  
  ## I want to extrapolate from this quantile forwards
  ## To do this I will calculate the probability of having had an event by each day of follow up, then when the probability crosses
  ## a 0.0001 threshold, we have our next 'quantile'
  
  # Create a vector which will store the probability of having an event before each subsequent day
  # I use the probability of day5844 as a jump off, this is the cumulative probability of having an event before 5845 days
  
  # Create a vector of the maximum length of the simulation, minus 16 years, which is where we are up to
  new.probs <- rep(0,60*365.25-10*365.25)
  
  # Probability starts of the quantile equating to day 5844
  new.probs[1] <- day3652/10000
  
  # The probbility of having an event by each subsequent day, is the cumulative probability of the day before, plus the probability of
  # not having the event up to that point multiplied by the probability of having the  event on that day (prob)
  # i.e. probability of having an event before day 5846, is the probability of having before 5845, plus (1-prob before 5845)*(conditional prob each year)
  for (i in 2:length(new.probs)){new.probs[i] <- new.probs[i-1] + (1-new.probs[i-1])*prob}
  
  
  # Create a vector of the days which these probabilities correspond to, 
  # the days gives 60 years follow up (allows to start simulations at age 30)
  new.days <- 3653:21915
  
  ## Create somewhere to store the extrapolated quantiles, that we will add to the current quantiles
  ## We already have quantile up until day5844, meaning there are 10000 - day5844 + 1 quantiles left to fill
  new.quantile <- rep(0,(10000 - day3652 + 1))
  
  ## Now derive the day which corresponds to each quantile and assign it to the new quantiles vector
  for (i in 1:(10000 - day3652 + 1)){
    new.quantile[i] <- new.days[min(which(new.probs > (new.probs[1]+i/10000)))]
  }
  
  head(new.quantile)
  
  ## Combine with the original quantiles, to create the extrapolated set of quantiles, that goes beyond 16 years
  disc.quantiles.all <- c(as.numeric(disc.quantiles$quantile)[1:day3652], new.quantile)
  
  return(disc.quantiles.all)}


### This function extrapolates the survival curve from year 6 onwards. 
### It assumes a constant daily probability of discontinuation from this point onwards

extrapolate.quantiles.6y <- function(survival.curve){
  
  ## First calculate quantiles and a survival table from the survival object
  disc.quantiles <- quantile(survival.curve, probs = seq(0,1,0.0001))
  disc.summary <-surv_summary(survival.curve)
  
  ## Not all the multiples of 365.25 have an event time in the dataset, so need to look for the nearest ones
  positions.yearly <- rep(0,10)
  for (i in 1:10){
    positions.yearly[i] <- min(which(disc.summary$time > round(seq(365.25, 7305, 365.25))[i]))}
  
  ## Now extract the probabilty table at yearly intervals, will use these to calculate yeary probability of an event
  disc.summary.reduc <- disc.summary[positions.yearly,]
  
  
  ## Get event probabilities by year
  event.probs <- rep(0,10)
  event.probs[1] <- 1 - disc.summary.reduc$surv[1]
  for (i in 2:10){event.probs[i] <- 1-disc.summary.reduc$surv[i]/disc.summary.reduc$surv[i-1]}
  
  ## Given the data size at this point, I will exclude the final year and take the mean of years 12 - 16 
  # to get the yearly discontinuation rate. The yearly event rate is also constant across this point
  extrapolate.yearly.disc.rate <- event.probs[6]
  
  ## Now calculate if this is the yearly probability, what is the probability on a given day
  prob <- 1 - nthroot(1-extrapolate.yearly.disc.rate,364)
  
  
  ## Now after 16 years, this is the 8*365.25 = 2191th day
  ## I want to find which quantile this day represents (i.e. is it q0.5 ? q0.08?)
  day2191 <- min(which(disc.quantiles$quantile > 2191))
  
  ## I want to extrapolate from this quantile forwards
  ## To do this I will calculate the probability of having had an event by each day of follow up, then when the probability crosses
  ## a 0.0001 threshold, we have our next 'quantile'
  
  # Create a vector which will store the probability of having an event before each subsequent day
  # I use the probability of day5844 as a jump off, this is the cumulative probability of having an event before 5845 days
  
  # Create a vector of the maximum length of the simulation, minus 16 years, which is where we are up to
  new.probs <- rep(0,60*365.25-6*365.25)
  
  # Probability starts of the quantile equating to day 5844
  new.probs[1] <- day2191/10000
  
  # The probbility of having an event by each subsequent day, is the cumulative probability of the day before, plus the probability of
  # not having the event up to that point multiplied by the probability of having the  event on that day (prob)
  # i.e. probability of having an event before day 5846, is the probability of having before 5845, plus (1-prob before 5845)*(conditional prob each year)
  for (i in 2:length(new.probs)){new.probs[i] <- new.probs[i-1] + (1-new.probs[i-1])*prob}
  
  
  # Create a vector of the days which these probabilities correspond to, 
  # the days gives 60 years follow up (allows to start simulations at age 30)
  new.days <- 2192:21915
  
  ## Create somewhere to store the extrapolated quantiles, that we will add to the current quantiles
  ## We already have quantile up until day5844, meaning there are 10000 - day5844 + 1 quantiles left to fill
  new.quantile <- rep(0,(10000 - day2191 + 1))
  
  ## Now derive the day which corresponds to each quantile and assign it to the new quantiles vector
  for (i in 1:(10000 - day2191 + 1)){
    new.quantile[i] <- new.days[min(which(new.probs > (new.probs[1]+i/10000)))]
  }
  
  head(new.quantile)
  
  ## Combine with the original quantiles, to create the extrapolated set of quantiles, that goes beyond 16 years
  disc.quantiles.all <- c(as.numeric(disc.quantiles$quantile)[1:day2191], new.quantile)
  
  return(disc.quantiles.all)}



## Create the extrapolated discontinuation rates for second and third treatment periods
disc2.extrapolated <- extrapolate.quantiles.10y(surv.obj.disc2)
disc3.extrapolated <- extrapolate.quantiles.6y(surv.obj.disc3)

## Create the extrapolated discontinuation rates for first and second restarting
restart1.extrapolated <- extrapolate.quantiles.10y(surv.obj.restart)
restart2.extrapolated <- extrapolate.quantiles.10y(surv.obj.restart2)


## Plot as a sense check
plot(disc1.extrapolated[[30]],seq(0,1,0.0001),type = "l", lwd = 1)
lines(disc2.extrapolated,seq(0,1,0.0001),type = "l", lwd = 1,col="green")
lines(disc3.extrapolated,seq(0,1,0.0001),type = "l", lwd = 1,col="red")

plot(restart1.extrapolated,seq(0,1,0.0001),type = "l", lwd = 1,col="green")
lines(restart2.extrapolated,seq(0,1,0.0001),type = "l", lwd = 1,col="blue")

rm(list=setdiff(ls(),list("disc1.extrapolated","disc2.extrapolated","disc3.extrapolated","restart1.extrapolated","restart2.extrapolated")))

save.image("R_out_C6/statin_discontinuation_rates_for_sim_continuous_multiple_treatment_periods_no1pres_agemod_FAKE.RData")

