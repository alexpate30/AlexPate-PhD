### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## This code generates the boxplots of confidence interval size, when sample size = 100,000, but straitified by C statistic (low, medium and high)

library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggpubr)

## Load the discriminations measures and population derived risks
load("R_out_C4/generate_risks_calibration_population_female.RData")

## Turn risks into percentages
risks.all.population <- 100*risks.all.population

## Load the risks from simulation
load("R_out_C4/generate_risks_different_cohort_sizes_female_100000.RData")

# str(risk.all)
str(risks.bootstrap)
str(HarrelsC.bootstrap)

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## First, want to report range of C statistics
range.C <- quantile(HarrelsC.bootstrap,probs = c(0,0.025,0.05,0.25,0.5,0.75,0.95,0.975,1))
range.C

## Now maybe I need to change this  to some extent, but good for now
## Next step is to do the analsis split, by C statistic

# Step 1: Split everyone into groups that have similar C statistics (not entirely sure how, but general idea)
# Step 2: Repeat the same analysis as above, would expect among models with similar C statistics, patients should have similar risks
######### Should also expect the higher C statistic models to have less variation
# risks.bootstrap
# HarrelsC.bootstrap

## Group the remaining ones into three groups, according to the C statistic

## First calculate the quantiles for thirds of the C statistics, so I can subset based on these values
quantiles.C <- quantile(HarrelsC.bootstrap,probs = c(0,0.333,0.667,1))

## Now extract based on this
C.groups <- vector("list",3)

## First group extracts all columns of risks.bootstrap, where the HarrelsC value from HarrelsC.bootstrap is between 0% and 33% quantiles
C.groups[[1]] <- risks.bootstrap[, (HarrelsC.bootstrap >= quantiles.C["0%"]) & (HarrelsC.bootstrap <= quantiles.C["33.3%"])]
## Second group 33% and 67%
C.groups[[2]] <- risks.bootstrap[, (HarrelsC.bootstrap > quantiles.C["33.3%"]) & (HarrelsC.bootstrap <= quantiles.C["66.7%"])]
## Last group 67% and 100%
C.groups[[3]] <- risks.bootstrap[, (HarrelsC.bootstrap > quantiles.C["66.7%"]) & (HarrelsC.bootstrap <= quantiles.C["100%"])]

## Check sizes
print("Check dimensions of subsets of all bootstrapped models, should all have same number of rows, but 1000/3 columns")
dim(C.groups[[1]])
dim(C.groups[[2]])
dim(C.groups[[3]])

## Now for each of these, I effectively want to produce the same plot as before
## I will write a function to do this, so I just have to enter the data

######### FUNCTION STARTS HERE
## Next define the function to create the boxplot for a dataframe (data.in), which has a row for each patient (same order 
## as risks.all.population), and a column for each bootstrapped risk score. The output is a list, which also stores the data
## required to do the actual plot
create.box.plot <- function(data.in){
  
  ## Now split into groups by this average risk
  groups <- vector("list",30)
  for (i in 1:30){groups[[i]] <- data.in[(risks.all.population > (i-1)) & (risks.all.population <= i),]}
  
  ## Create data which contains the CI size for each patient
  CI.size.all <- vector("list",30)
  for (i in 1:30){CI.size.all[[i]] <- data.frame("lower" = rowQuantiles(as.matrix(groups[[i]]),probs=0.025), "upper" = rowQuantiles(as.matrix(groups[[i]]),probs=0.975), 
                                                 "CIsize" = rowQuantiles(as.matrix(groups[[i]]),probs=0.975) - rowQuantiles(as.matrix(groups[[i]]),probs=0.025))}
  
  ## Each in each 'CI.size.all' list element, I have a dataframe with each patients from that risk group, columns being
  ## lower bound on CI, upper bound on CI, and size of CI.
  
  ## Make boxplots of this
  ## First I need to restructure my data
  ## For each group I want to add a column called 'risk' with the risk score for that group
  for (i in 1:30){CI.size.all[[i]]$"risk" <- rep(paste(i-1,"-",i,"%",sep=""),nrow(CI.size.all[[i]]))}
  
  ## Now concatenate them into one big dataset
  plot.data.all <- do.call("rbind",CI.size.all)
  
  
  ## Now extract only risk scores we want to plot
  plot.data <- plot.data.all[plot.data.all$risk %in% c("4-5%","9-10%","14-15%","19-20%"),]
  plot.data$risk <- factor(plot.data$risk, levels <- c("4-5%","9-10%","14-15%","19-20%"))
  
  ## Now create the boxplot
  
  ## First create dataframe with data for labels
  label.data <- data.frame("x" = c("4-5%","9-10%","14-15%","19-20%"),y = 30, label = paste("n = ", c(sum(plot.data$risk == "4-5%"),sum(plot.data$risk == "9-10%"),
                                                                                                     sum(plot.data$risk == "14-15%"),sum(plot.data$risk == "19-20%"))))
  
  
  ## Now do the boxplot
  plot.box <- ggplot(data = plot.data, aes(x=risk,y=CIsize)) + geom_boxplot(outlier.size = 0.01, lwd = 0.25) + stat_boxplot(geom = "errorbar", lwd = 0.25) +
    xlab("Population derived risk") + ylab("2.5 - 97.5 percentile range across models") + scale_y_continuous(minor_breaks = NULL, breaks = seq(0,30,5), limits = c(0,30)) +
    theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size = .5, color = "white"), plot.title = element_text(size=10),
          axis.title = element_text(size = 9)) +
    geom_text(data = label.data, aes(x=x,y=y,label = label), size = 3)
  
  
  ## Return both the data derived to do the plot, and the boxplot itself, this means the whole thing doesnt have to be run again
  ## if you want to edit the boxplot only
  return(list("plot.data.all" = plot.data.all,"plot.data" = plot.data,"plot.box" = plot.box))}

print("start low")
Sys.time()
res.C.low.obj <- create.box.plot(C.groups[[1]])
res.C.low <- res.C.low.obj$"plot.box" + ggtitle(paste("Low C statistic\nNumber of models =",dim(C.groups[[1]])[2],sep=" "))
print("start med")
Sys.time()
res.C.med.obj <- create.box.plot(C.groups[[2]])
res.C.med <- res.C.med.obj$"plot.box" + ggtitle(paste("Medium C statistic\nNumber of models =",dim(C.groups[[2]])[2],sep=" "))
print("start high")
Sys.time()
res.C.high.obj <- create.box.plot(C.groups[[3]])
res.C.high <- res.C.high.obj$"plot.box" + ggtitle(paste("High C statistic\nNumber of models =",dim(C.groups[[3]])[2],sep=" "))
Sys.time()

str(res.C.low)

#rm(list=setdiff(ls(),list("res.C.low","res.C.med","res.C.high","res.C.all","boxplot.comb.female","quantiles.C")))

## Make combined plot
Figure3_female_CI_boxplot_by_C_statistic_100000 <- ggarrange(res.C.low,res.C.med,res.C.high,nrow = 1,ncol=3)
Figure3_female_CI_boxplot_by_C_statistic_100000
ggsave("figures/Figure3_female_CI_boxplot_by_C_statistic_100000.jpeg",Figure3_female_CI_boxplot_by_C_statistic_100000, dpi = 300, height = 3.5, width = 10.5)


## Let's also get information for the text
summary(res.C.low$"data"$CIsize[res.C.low$"data"$risk == "4-5%"])
summary(res.C.low$"data"$CIsize[res.C.low$"data"$risk == "9-10%"])
summary(res.C.low$"data"$CIsize[res.C.low$"data"$risk == "14-15%"])
summary(res.C.low$"data"$CIsize[res.C.low$"data"$risk == "19-20%"])

summary(res.C.med$"data"$CIsize[res.C.med$"data"$risk == "4-5%"])
summary(res.C.med$"data"$CIsize[res.C.med$"data"$risk == "9-10%"])
summary(res.C.med$"data"$CIsize[res.C.med$"data"$risk == "14-15%"])
summary(res.C.med$"data"$CIsize[res.C.med$"data"$risk == "19-20%"])

summary(res.C.high$"data"$CIsize[res.C.high$"data"$risk == "4-5%"])
summary(res.C.high$"data"$CIsize[res.C.high$"data"$risk == "9-10%"])
summary(res.C.high$"data"$CIsize[res.C.high$"data"$risk == "14-15%"])
summary(res.C.high$"data"$CIsize[res.C.high$"data"$risk == "19-20%"])


