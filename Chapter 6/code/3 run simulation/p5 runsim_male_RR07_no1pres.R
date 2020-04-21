### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

library(foreach)
library(doParallel)
library(dplyr)
library(survival)

load("R_out_C6/generate_model_agetime_male.RData")
load("R_out_C6/simulation_functions_agetime_models_male.RData")
load("R_out_C6/generate_risk_range_for_each_age_agetime_male.RData")
load("R_out_C6/statin_discontinuation_rates_for_sim_continuous_multiple_treatment_periods_no1pres_agemod.RData")


## min.max.age contains min and max risk for each age
min.max.prange.age.agetime

## risks.1ys contains the risks in the first year (what I apply my function to)
## risks.10ys contains the cumulative risk for the first 10 years (calculaet 10 year risk from this)

## Going to create a different list 

#ages <- seq(from=25,to=85,by=5)
#risk
#disc
#treatment start

## Create a function that for a given discontinuation level, will calculate all the risks, for all the treatment initiation times, for all the risk scores and ages  
## and store it in a list
create.output <- function(quant1,quant2,quant3,quant4,quant5){
  # Create another list, each entry referring to a different age
  func.out <- vector("list",length(ages))
  # Name each entry according to age
  names(func.out) <- paste("Age", ages)
  
  ## For each age and discontinuation rate, we want to know the risk for each risk score, when you prescribe statins at various pieces of follow up
  ## So each entry of this will be a table of risk score vs statin initiation date
  
  ## I have opted not to simulate for ages 25 and 30, given there are no risks higher than 1% is the 1 - 99th 
  ## percentile range
  for (i in c(4,6,8)){
    # Create output dataset empty
    data.out <- matrix(,nrow=(min(20,round(min.max.prange.age.agetime[i,6])) - max(1,round(min.max.prange.age.agetime[i,3])) + 1),
                       ncol= 90 - ages[i])
    # Create a for loop for each risk score in the relevant age range
    for (j in max(1,round(min.max.prange.age.agetime[i,3])):min(20,round(min.max.prange.age.agetime[i,6]))){
      # Create a for loop for each statin initiation date
      for (k in 1:(90-ages[i])){
        data.out[(j-max(1,round(min.max.prange.age.agetime[i,3]))+1),k] <- run.simulation.agetime.CPRDdisc.cont.multiple.treat(j,ages[i],k,0.7,
                                                                                                                               quant1[[(k+(ages[i]-40))]],quant2,quant3,
                                                                                                                               quant4,quant5)
        #print(paste("disc", discontinuations[h], "age", ages[i], "risk", j, "delay", k))
      }
    }
    ## Assign the output dataset for disc[h] and ages[i] to its place
    print(i)
    func.out[[i]] <- data.out}
  return(func.out)
}

## Parallelise and apply the function across nodes for each discontinuation rate
## Parallelise and apply the function across nodes for each discontinuation rate
res.multiple.treatment.periods <- create.output(disc1.extrapolated,
                                                disc2.extrapolated,
                                                disc3.extrapolated,
                                                restart1.extrapolated,
                                                restart2.extrapolated)

rm(anal.dat,basehaz.model,comp.data, agetime.model.covar, agetime.model)

save.image("R_out_C6/runsim_male_RR07_no1pres.RData")
print("image saved")
