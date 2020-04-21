rm(list=ls())

### Do the female models first

## Load the models
setwd("B")
load("models_female_modelE.RData")

library(bdsmatrix)
library(coxme)

## Extract the random coefficient from each model and average
female.LP<-rowMeans(cbind(unlist(fit_data_parallel.1[[1]]$frail),
                                 unlist(fit_data_parallel.1[[2]]$frail),
                                        unlist(fit_data_parallel.1[[3]]$frail),
                                        unlist(fit_data_parallel.1[[4]]$frail),
                                        unlist(fit_data_parallel.1[[5]]$frail),
                                        unlist(fit_data_parallel.1[[6]]$frail),
                                        unlist(fit_data_parallel.1[[7]]$frail),
                                        unlist(fit_data_parallel.1[[8]]$frail),
                                        unlist(fit_data_parallel.1[[9]]$frail),
                                        unlist(fit_data_parallel.1[[10]]$frail),
                                        unlist(fit_data_parallel.2[[1]]$frail),
                                        unlist(fit_data_parallel.2[[2]]$frail),
                                        unlist(fit_data_parallel.2[[3]]$frail),
                                        unlist(fit_data_parallel.2[[4]]$frail),
                                        unlist(fit_data_parallel.2[[5]]$frail),
                                        unlist(fit_data_parallel.2[[6]]$frail),
                                        unlist(fit_data_parallel.2[[7]]$frail),
                                        unlist(fit_data_parallel.2[[8]]$frail),
                                        unlist(fit_data_parallel.2[[9]]$frail),
                                        unlist(fit_data_parallel.2[[10]]$frail)))

## Take exponent to get the HR
female.HR <- exp(female.LP)

## Need to get confidence intervals for each random effect

print("second bit")


## Extract variance of random coeffcients from each model and take the average to get within imputation variance
within.var<-rowMeans(cbind(diag(fit_data_parallel.1[[1]]$variance)[1:10],
                           diag(fit_data_parallel.1[[2]]$variance)[1:10],
                           diag(fit_data_parallel.1[[3]]$variance)[1:10],
                           diag(fit_data_parallel.1[[4]]$variance)[1:10],
                           diag(fit_data_parallel.1[[5]]$variance)[1:10],
                           diag(fit_data_parallel.1[[6]]$variance)[1:10],
                           diag(fit_data_parallel.1[[7]]$variance)[1:10],
                           diag(fit_data_parallel.1[[8]]$variance)[1:10],
                           diag(fit_data_parallel.1[[9]]$variance)[1:10],
                           diag(fit_data_parallel.1[[10]]$variance)[1:10],
                           diag(fit_data_parallel.2[[1]]$variance)[1:10],
                           diag(fit_data_parallel.2[[2]]$variance)[1:10],
                           diag(fit_data_parallel.2[[3]]$variance)[1:10],
                           diag(fit_data_parallel.2[[4]]$variance)[1:10],
                           diag(fit_data_parallel.2[[5]]$variance)[1:10],
                           diag(fit_data_parallel.2[[6]]$variance)[1:10],
                           diag(fit_data_parallel.2[[7]]$variance)[1:10],
                           diag(fit_data_parallel.2[[8]]$variance)[1:10],
                           diag(fit_data_parallel.2[[9]]$variance)[1:10],
                           diag(fit_data_parallel.2[[10]]$variance)[1:10]))

## Assign the average of the coefficients to a vector called av.coeff
av.coeff<-female.LP

print("third bit")

## Calculate the between imputation variance
between.var<-((unlist(fit_data_parallel.1[[1]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[2]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[3]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[4]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[5]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[6]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[7]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[8]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[9]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[10]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[1]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[2]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[3]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[4]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[5]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[6]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[7]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[8]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[9]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[10]]$frail)-av.coeff)^2)/19


## Combine formula T = within.var + (1+1/m)*between.var
total.var <- within.var + (1+1/19)*between.var

## Take square root to get the standard error
total.se <- sqrt(total.var)

## Calculate CI's
female.CI.upper <- exp(female.LP + 1.96*total.se)
female.CI.lower <- exp(female.LP - 1.96*total.se)

female.HR
female.CI.lower
female.CI.upper

print('female done')

### REPEAT PROCESS FOR MALE COHORT
### REPEAT PROCESS FOR MALE COHORT
### REPEAT PROCESS FOR MALE COHORT

load("models_male_modelE.RData")


## Extract the random coefficient from each model and average
male.LP<-rowMeans(cbind(unlist(fit_data_parallel.1[[1]]$frail),
                          unlist(fit_data_parallel.1[[2]]$frail),
                          unlist(fit_data_parallel.1[[3]]$frail),
                          unlist(fit_data_parallel.1[[4]]$frail),
                          unlist(fit_data_parallel.1[[5]]$frail),
                          unlist(fit_data_parallel.1[[6]]$frail),
                          unlist(fit_data_parallel.1[[7]]$frail),
                          unlist(fit_data_parallel.1[[8]]$frail),
                          unlist(fit_data_parallel.1[[9]]$frail),
                          unlist(fit_data_parallel.1[[10]]$frail),
                          unlist(fit_data_parallel.2[[1]]$frail),
                          unlist(fit_data_parallel.2[[2]]$frail),
                          unlist(fit_data_parallel.2[[3]]$frail),
                          unlist(fit_data_parallel.2[[4]]$frail),
                          unlist(fit_data_parallel.2[[5]]$frail),
                          unlist(fit_data_parallel.2[[6]]$frail),
                          unlist(fit_data_parallel.2[[7]]$frail),
                          unlist(fit_data_parallel.2[[8]]$frail),
                          unlist(fit_data_parallel.2[[9]]$frail),
                          unlist(fit_data_parallel.2[[10]]$frail)))

## Take exponent to get HR
male.HR <- exp(male.LP)



## Extract variance of random coeffcients from each model and take the average to get within imputation variance
within.var<-rowMeans(cbind(diag(fit_data_parallel.1[[1]]$variance)[1:10],
                           diag(fit_data_parallel.1[[2]]$variance)[1:10],
                           diag(fit_data_parallel.1[[3]]$variance)[1:10],
                           diag(fit_data_parallel.1[[4]]$variance)[1:10],
                           diag(fit_data_parallel.1[[5]]$variance)[1:10],
                           diag(fit_data_parallel.1[[6]]$variance)[1:10],
                           diag(fit_data_parallel.1[[7]]$variance)[1:10],
                           diag(fit_data_parallel.1[[8]]$variance)[1:10],
                           diag(fit_data_parallel.1[[9]]$variance)[1:10],
                           diag(fit_data_parallel.1[[10]]$variance)[1:10],
                           diag(fit_data_parallel.2[[1]]$variance)[1:10],
                           diag(fit_data_parallel.2[[2]]$variance)[1:10],
                           diag(fit_data_parallel.2[[3]]$variance)[1:10],
                           diag(fit_data_parallel.2[[4]]$variance)[1:10],
                           diag(fit_data_parallel.2[[5]]$variance)[1:10],
                           diag(fit_data_parallel.2[[6]]$variance)[1:10],
                           diag(fit_data_parallel.2[[7]]$variance)[1:10],
                           diag(fit_data_parallel.2[[8]]$variance)[1:10],
                           diag(fit_data_parallel.2[[9]]$variance)[1:10],
                           diag(fit_data_parallel.2[[10]]$variance)[1:10]))

## Next calculate the between imputation variance
## First need to calculate the average for each random effect across imputations
## This is male.LP
av.coeff<-male.LP

between.var<-((unlist(fit_data_parallel.1[[1]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[2]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[3]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[4]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[5]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[6]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[7]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[8]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[9]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.1[[10]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[1]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[2]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[3]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[4]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[5]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[6]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[7]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[8]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[9]]$frail)-av.coeff)^2 +
                (unlist(fit_data_parallel.2[[10]]$frail)-av.coeff)^2)/19


## Combine formula T = within.var + (1+1/m)*between.var
total.var <- within.var + (1+1/19)*between.var

## Calculate standard error
total.se <- sqrt(total.var)

## Calculate CI
male.CI.upper <- exp(male.LP + 1.96*total.se)
male.CI.lower <- exp(male.LP - 1.96*total.se)


save.image("region_HR_calculations.RData")
