### SET ROOT DIRECTORY

### SET ROOT DIRECTORY


## I am loading the age time models for the female cohort here just to test the functions work
## They will be removed before saving the functions
## The age time models, along with the basehaz.model, basehaz.haz.model and comp.data need to be loaded in
## before doing the simulation


library(svMisc)
library(survival)
library(doParallel)
library(foreach)
library(ggplot2)
library(reshape2)


## This first function calculates the marginal risks from a set of conditional risks
get.marg.risk.nodeath <- function(cond.risks.in){
  n<-length(cond.risks.in)
  marg.risks <- rep(0,length(cond.risks.in))
  marg.risks[1]<-cond.risks.in[1]
  # Caclulaet all subsequent marginal risks
  for (j in 2:n){marg.risks[j]<-prod((1-cond.risks.in)[1:(j-1)])*cond.risks.in[j]}
  return(sum(marg.risks))
}

## First going to create a function which for a given HR, will calculate the 10 year risk
## Input is a baseline hazard, where each year is divided up into 100 time points
## We create this so that we can find the relevant HR for a given 10 year risk (can then calculate life time risk
## associated with this HR value)
get.10y.risk.nodeath <- function(HR, age, basehaz){
  
  # Calculate the hazard
  haz.by.year <- rep(0,70)
  for (i in 1:70){haz.by.year[i] <- basehaz[(100*i)]}
  
  ## Multiply by HR and calculate  cumulative survival probabilities
  haz.by.year <- haz.by.year*HR
  surv.by.year <- exp(-haz.by.year)
  risk.by.year <- 1 - surv.by.year
  
  # Convert these into marginal and conditional probabilities
  marg.by.year <- c(risk.by.year[1],diff(risk.by.year))
  
  cond.by.year<-rep(0,70)
  cond.by.year[1]<-marg.by.year[1]
  for (i in 2:70){cond.by.year[i]<-marg.by.year[i]/(prod(1-cond.by.year))}
  
  ## Now calculate the risk between two specific ages
  return(get.marg.risk.nodeath(cond.by.year[(age-24):(age + 9-24)]))
}



## Now write a function that return that HR for a given age and risk, that results in the 10 year risk of interest
get.HR <- function(risk1,age1,basehaz1){
  
  ## Now want to find the HR which gives a risk of r
  HR.in <- seq(0,50,by=0.001)
  
  # Turn these into risks
  risks.10y <-rep(0,length(HR.in))
  for (i in 1:length(HR.in)){risks.10y[i] <- get.10y.risk.nodeath(HR.in[i],age1,basehaz1)}

  ## Now return the minimum HR for which the risk is bigger than r
  return(HR.in[min(which(risks.10y > risk1/100))])}


#get.HR(5,50,basehaz.haz.model)


## Using this HR I can then calculate the life time risks for a patient with the 10 year risk of interest
## That is the conditional risks over their lifetime
get.cond.lifetime.risks <- function(risk2,age2,basehaz2){
  ## First get the HR of interest
  HR <- get.HR(risk2,age2,basehaz2)
  
  # Calculate the hazard
  haz.by.year <- rep(0,70)
  for (i in 1:70){haz.by.year[i] <- basehaz2[(100*i)]}
  
  ## Multiply by HR and calculate  cumulative survival probabilities
  haz.by.year <- haz.by.year*HR
  surv.by.year <- exp(-haz.by.year)
  risk.by.year <- 1 - surv.by.year
  
  # Convert these into marginal and conditional probabilities
  marg.by.year <- c(risk.by.year[1],diff(risk.by.year))
  
  cond.by.year<-rep(0,70)
  cond.by.year[1]<-marg.by.year[1]
  for (i in 2:70){cond.by.year[i]<-marg.by.year[i]/(prod(1-cond.by.year))}
  return(cond.by.year[(age2-24):(89-24)])
}

#get.cond.lifetime.risks(5,50,basehaz.haz.model)

#summary(basehaz.haz.model)


### So in order to get the risks for a given patients age, required risk score, and baseline hazard (the given model),
### I just have to run
#get.cond.lifetime.risks(5,50,basehaz.haz.model)
#get.cond.lifetime.risks(10,60,basehaz.haz.model)

## Or for the 10 year risks
#get.cond.10y.risks(5,50,basehaz.haz.model)
#get.cond.10y.risks(10,60,basehaz.haz.model)

## Note that if I get the total marginal risk from these, it should equal the inputted risk
#get.marg.risk.nodeath(get.cond.10y.risks(5,50,basehaz.haz.model))



## This function calculates the marginal risk up until age 90, for a given risk, age, discontinuation rate, 
## treatment start date and treatment effect
## However the discontinuation survival curve must have yearly intervals (not used in main simulation)
## Also patients may not restart treatment
run.simulation.agetime <- function(risk.in, age.in, discontinuation.in, treatment.start.in, treatment.effect.in){
  
  ## Calculate the conditional risks for a patient of the given age and risk
  cond.risks <- get.cond.lifetime.risks(risk.in,age.in,basehaz.haz.model)
  
  ## Set the age
  age <- age.in
  
  # Create number of cycles given the patients age
  n <- 90 - age
  
  ## Now put these into a table of say 10,000 people, so I can generate and assign risk reduction at random
  ## Just create an empty matrix and assign conditional probabilities to every row
  cond.risks.adj<-matrix(nrow=10000,ncol=n)
  for (i in 1:10000){cond.risks.adj[i,]<-cond.risks}
  
  ## Define treatment effect
  treatment.effect <- treatment.effect.in
  
  ## Define the discontinuation rate
  adherence.rate <- discontinuation.in
  
  ## Generate the discontinuation time at random
  set.seed(101)
  adherence.year<-rgeom(10000,adherence.rate)  ## Note this geometric dfistreibution in R is defined to take values of zero, which is what we want
  adherence.year[1:100]
  
  ## Define the year in which people start treatment
  ## Note that treatment.start.in = 1 refers to starting treatment straight away
  ## But a patient can still discontinue straight away
  year <- treatment.start.in
  
  adherence.start <- year
  adherence.end<-adherence.year + adherence.start - 1
  adherence.end[adherence.end > n] <- n
  adherence.end[1:100]
  
  ## An adherence.year value of zero means discontinue striaght away, aka no treatment
  ## An adherence.year value of one means they receive treatment for the first year
  ## Therefore if adherence.end = 1, we actually want to deduct one off this, so when we apply the risk reduction to risks[start:end], if adherence.year = 1
  ## then we have start = end, and we apply to 1 year
  
  ## Now change the row accordingly to the statin prescriptions
  for (i in 1:10000){
    # First, we dont want to do anything if adherence.year = 0, as they didnt take their statins at all
    if (adherence.year[i] > 0){
      # If adherence.year is bigger than zero, want to apply reduction between adherence.start, and adherence.end
      # Note if adherence.year = 1 (only took for one year), then adherence.start = adherence.end, and we apply reduction to 1 year, as req
      
      # Here we replace the relevant entries of cond.risks.adj, with the cond.risks*treatment effect
      cond.risks.adj[i,][adherence.start:adherence.end[i]]<-cond.risks[adherence.start:adherence.end[i]]*treatment.effect}
  }
  
  ### So now want to get the marginal for each
  marg.risks.adj<-matrix(nrow=10000,ncol=n)
  for (i in 1:10000){
    # Marginal and conditional risk in first year is the same
    marg.risks.adj[i,1]<-cond.risks.adj[i,1]
    # Caclulaet all subsequent marginal risks
    for (j in 2:n){marg.risks.adj[i,j]<-prod((1-cond.risks.adj[i,])[1:(j-1)])*cond.risks.adj[i,j]}
  }
  

  ## So it appears to work
  
  ## Now I have the marginal risks, I want to curtail this for risk of death
  ## I have the risks of death saved in a file
  death.rates <- read.csv("data/noncvd death rates riskcohort CPRD female.csv",header=TRUE)
  
  ## Need to create a death date for each person
  cond.risks.death <- rep(0,n)
  cond.risks.death <- death.rates$hazard[(age-24):(65)]
  
  marg.risks.death <- rep(0,n)
  marg.risks.death[1] <- cond.risks.death[1]
  
  for (j in 2:n){marg.risks.death[j]<-prod((1-cond.risks.death)[1:(j-1)])*cond.risks.death[j]}
  
  cum.risks.death <- cumsum(marg.risks.death)
  
  ## Create a vector of length 10000 from uniform 0,1
  
  r.unif <- runif(10000,0,1)
  
  ## Create a variable that equals to year or the event, depending where r.unif lies in the  cum.risks.death vector
  death.year <- rep(0,10000)
  death.year[between(r.unif,0,cum.risks.death[1])] <- 1
  for (i in 2:n){
    death.year[between(r.unif,cum.risks.death[i-1],cum.risks.death[i])] <- i
  }
  
  table(death.year)
  
  ## Now for all people with a death, remove all marginal probability of them having an event beyond this death date
  head(marg.risks.adj)
  
  for (i in 1:10000){
    # Only make any changes if they die, aka death date > 0
    if (death.year[i] > 0){ 
      marg.risks.adj[i,death.year[i]:n] <- 0
    }
  }
  
  ## I now have the marginal probabilities of having an event for each individual in the simulation
  ## Can sum these to get the risks of interest
  risks.total <- mean(rowSums(marg.risks.adj))
  
  return(risks.total)}



## THIS IS USED IN MAIN SIMULATION ##
## THIS IS USED IN MAIN SIMULATION ##
## This function calculates the marginal risk up until age 90, for a given risk, age, 
## treatment start date and treatment effect. The discontinuation rate used is input, however it allows
## continuous survival distributions, like those derived from CPRD
## This allows for three discontinuation periods, and two restarting periods
## The input for the survival distribution is quantiles (days), of size 1/10000 (so vector of length 10001),
## which gives the day at which we expect 100*(x/10000)% of the population to have had an event
## THIS IS USED IN MAIN SIMULATION ##
## THIS IS USED IN MAIN SIMULATION ##
run.simulation.agetime.CPRDdisc.cont.multiple.treat <- function(risk.in, age.in, treatment.start.in, treatment.effect.in,
                                                                disc1.quantiles.in,disc2.quantiles.in,disc3.quantiles.in,restart1.quantiles.in,
                                                                restart2.quantiles.in){
  
  ## Calculate the conditional risks for a patient of the given age and risk
  cond.risks <- get.cond.lifetime.risks(risk.in,age.in,basehaz.haz.model)
  
  ## Set the age
  age <- age.in
  
  # Create number of cycles given the patients age
  n <- 90 - age
  
  ## Now put these into a table of say 10,000 people, so I can generate and assign risk reduction at random
  ## Just create an empty matrix and assign conditional probabilities to every row
  cond.risks.adj<-matrix(nrow=10000,ncol=n)
  for (i in 1:10000){cond.risks.adj[i,]<-cond.risks}
  
  ## Define treatment effect
  treatment.effect <- treatment.effect.in
  

  ### Now generate 10,000 patients using the discontinuation probabilities
  ## First generate a random uniform 0/1 variable
  set.seed(101)
  disc1.unif <-runif(10000,0,1)
  
  ## Now want to multiply this to make it a number between 1 and 10000, which are the indexes for the different quantiles
  ## Also note we have to add 1, as the first quantile is zero (which we dont want, as every in the simulation at least has started statins) so
  ## we want to have a simulated value of 1, correspond to the second quantile (which is like the 0.0001th), and the final number is 10001
  disc1.ceil <- ceiling(10000*disc1.unif)+1
  
  ## Want to do this for disc1, 2, 3, restart 1 and 2:
  disc2.unif <-runif(10000,0,1)
  disc3.unif <-runif(10000,0,1)
  restart1.unif <-runif(10000,0,1)
  restart2.unif <-runif(10000,0,1)
  
  disc2.ceil <- ceiling(10000*disc2.unif)+1
  disc3.ceil <- ceiling(10000*disc3.unif)+1
  restart1.ceil <- ceiling(10000*restart1.unif)+1
  restart2.ceil <- ceiling(10000*restart2.unif)+1
  
  
  ## Now extract the event/discontinuation times
  disc1.event <-as.numeric(disc1.quantiles.in[disc1.ceil])
  disc1.event[is.na(disc1.event)] <- 25000
  
  disc2.event <-as.numeric(disc2.quantiles.in[disc2.ceil])
  disc2.event[is.na(disc2.event)] <- 25000
  
  disc3.event <-as.numeric(disc3.quantiles.in[disc3.ceil])
  disc3.event[is.na(disc3.event)] <- 25000
  
  restart1.event <-as.numeric(restart1.quantiles.in[restart1.ceil])
  restart1.event[is.na(restart1.event)] <- 25000
  
  restart2.event <-as.numeric(restart2.quantiles.in[restart2.ceil])
  restart2.event[is.na(restart2.event)] <- 25000
  
  ## Combine into one dataset, by column
  disc.res.comb <- cbind(disc1.event, restart1.event, disc2.event, restart2.event, disc3.event)
  
  ## Create cumsum of each row
  disc.res.cumsum <-   t(apply(disc.res.comb,1, cumsum))
  
  ## Calculate divided by 365
  disc.res.cumsum365 <- disc.res.cumsum/365.25
  
  ## Get the floor and ceiling tables
  floor.cumsum365 <- floor(disc.res.cumsum365)
  ceil.cumsum365 <- ceiling(disc.res.cumsum365)
  
  ## Now want to calculate the table of 'multipliers'
  ## I will then multiply the conditional risks by these multipliers (one row at a time)
  
  ## Create empty table
  multipliers.table <- matrix(0, nrow = 10000, ncol = n) ## (This should actually be the length of the simulation)
  
  ## Assign treatment start date
  start <- treatment.start.in
  
  ## Do each row seperately, play it safe
  for (i in 1:10000){
    
  multipliers1 <- rep(0,n)
  if (floor.cumsum365[i,1] > 1){
    multipliers1[start:(start + floor.cumsum365[i,1] - 1)] <- 1}
  multipliers1[(start + floor.cumsum365[i,1])] <- disc.res.cumsum365[i,1] - floor.cumsum365[i,1]

  
  multipliers2 <- rep(0,n)
  # if floor.cumsum365[5] > floor.cumsum365[2] (i.e. cross the threshold, we want to calculate all the way up to ceil.cumsum4)
  if (floor.cumsum365[i,3] > floor.cumsum365[i,2]){
    multipliers2[start + floor.cumsum365[i,2]] <- ceil.cumsum365[i,2] - disc.res.cumsum365[i,2]
    # We then want to check if the difference is bigger than 1, if it is, we assign 1's to all inbetween
    if (floor.cumsum365[i,3] > ceil.cumsum365[i,2]){
      multipliers2[(start+ceil.cumsum365[i,2]):(start+floor.cumsum365[i,3] - 1)] <- 1}
    # Regardless of previous step, as we already know that we have crossed the threshold, we now want to calculate the proportion of the final year
    multipliers2[start + floor.cumsum365[i,3]] <- disc.res.cumsum365[i,3] - floor.cumsum365[i,3]
  }
  
  # if floor.cumsum365[3] = floor.cumsum365[2] (i.e. don't cross the threshold, we want to calculate all the way up to ceil.cumsum4)
  if (floor.cumsum365[i,3] == floor.cumsum365[i,2]){
    ## Calculate the difference between the two and apply it to the same year
    multipliers2[start+floor.cumsum365[i,3]] <- disc.res.cumsum365[i,3] - disc.res.cumsum365[i,2]}
  
  
  multipliers3 <- rep(0,n)
  # if floor.cumsum365[5] > floor.cumsum365[4] (i.e. cross the threshold, we want to calculate all the way up to ceil.cumsum4)
  if (floor.cumsum365[i,5] > floor.cumsum365[i,4]){
    multipliers3[start + floor.cumsum365[i,4]] <- ceil.cumsum365[i,4] - disc.res.cumsum365[i,4]
    # We then want to check if the difference is bigger than 1, if it is, we assign 1's to all inbetween
    if (floor.cumsum365[i,5] > ceil.cumsum365[i,4]){
      multipliers3[(start+ceil.cumsum365[i,4]):(start+floor.cumsum365[i,5] - 1)] <- 1}
    # Regardless of previous step, as we already know that we have crossed the threshold, we now want to calculate the proportion of the final year
    multipliers3[start + floor.cumsum365[i,5]] <- disc.res.cumsum365[i,5] - floor.cumsum365[i,5]
  }
  
  # if floor.cumsum365[5] = floor.cumsum365[4] (i.e. don't cross the threshold, we want to calculate all the way up to ceil.cumsum4)
  if (floor.cumsum365[i,5] == floor.cumsum365[i,4]){
    ## Calculate the difference between the two and apply it to the same year
    multipliers3[start+floor.cumsum365[i,5]] <- disc.res.cumsum365[i,5] - disc.res.cumsum365[i,4]}
  
  multipliers1 <- multipliers1[1:n]
  multipliers2 <- multipliers2[1:n]
  multipliers3 <- multipliers3[1:n]
  multipliers.comb <- multipliers1 + multipliers2 + multipliers3
  
  multipliers.table[i,] <- multipliers.comb}
  
  ## Create the treatment table, what we multiply the conditional risks by
  treatment.table <- 1-(1-treatment.effect)*multipliers.table

  ## Multiply cond.risks by treatment table to get the adjusted risks, cond.risks.adj
  cond.risks.adj <- sweep(treatment.table,MARGIN=2,cond.risks,'*')
  
  ### So now want to get the marginal for each
  marg.risks.adj<-matrix(nrow=10000,ncol=n)
  for (i in 1:10000){
    # Marginal and conditional risk in first year is the same
    marg.risks.adj[i,1]<-cond.risks.adj[i,1]
    # Caclulaet all subsequent marginal risks
    for (j in 2:n){marg.risks.adj[i,j]<-prod((1-cond.risks.adj[i,])[1:(j-1)])*cond.risks.adj[i,j]}
  }
  
  
  ## So it appears to work
  
  ## Now I have the marginal risks, I want to curtail this for risk of death
  ## I have the risks of death saved in a file
  death.rates <- read.csv("data/noncvd death rates riskcohort CPRD female.csv",header=TRUE)
  
  ## Need to create a death date for each person
  cond.risks.death <- rep(0,n)
  cond.risks.death <- death.rates$hazard[(age-24):(65)]
  
  marg.risks.death <- rep(0,n)
  marg.risks.death[1] <- cond.risks.death[1]
  
  for (j in 2:n){marg.risks.death[j]<-prod((1-cond.risks.death)[1:(j-1)])*cond.risks.death[j]}
  
  cum.risks.death <- cumsum(marg.risks.death)
  
  ## Create a vector of length 10000 from uniform 0,1
  
  r.unif <- runif(10000,0,1)
  
  ## Create a variable that equals to year or the event, depending where r.unif lies in the  cum.risks.death vector
  death.year <- rep(0,10000)
  death.year[between(r.unif,0,cum.risks.death[1])] <- 1
  for (i in 2:n){
    death.year[between(r.unif,cum.risks.death[i-1],cum.risks.death[i])] <- i
  }
  
  table(death.year)
  
  ## Now for all people with a death, remove all marginal probability of them having an event beyond this death date
  head(marg.risks.adj)
  
  for (i in 1:10000){
    # Only make any changes if they die, aka death date > 0
    if (death.year[i] > 0){ 
      marg.risks.adj[i,death.year[i]:n] <- 0
    }
  }
  
  ## I now have the marginal probabilities of having an event for each individual in the simulation
  ## Can sum these to get the risks of interest
  risks.total <- mean(rowSums(marg.risks.adj))
  
  return(risks.total)}


## Note I am removing basehaz and stuff because I am just saving these functions for either male or female cohort
rm(list=setdiff(ls(),list("run.simulation.agetime.CPRDdisc.cont.multiple.treat","run.simulation.agetime","get.cond.lifetime.risks","get.HR",
                          "get.10y.risk.nodeath","get.marg.risk.nodeath")))

save.image("R_out_C6/simulation_functions_agetime_models_female_FAKE.RData")
