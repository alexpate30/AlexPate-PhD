### SET ROOT DIRECTORY

### SET ROOT DIRECTORY

## First load packages
library(dplyr)
library(matrixStats)
library(ggplot2)
library(ggpubr)

### Load the population level derived risks
load("R_out_C4/generate_risks_calibration_population_female.RData")

## Turn risks into percentages
risks.all.population <- 100*risks.all.population

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


###### Because the scale is so different for N = 1434, we have to create a seperate function to create this box plot, which needs very differnt y axis limits
###### Because the scale is so different for N = 1434, we have to create a seperate function to create this box plot, which needs very differnt y axis limits
###### Because the scale is so different for N = 1434, we have to create a seperate function to create this box plot, which needs very differnt y axis limits
create.box.plot.1434 <- function(data.in){
  
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
  label.data <- data.frame("x" = c("4-5%","9-10%","14-15%","19-20%"),y = 70, label = paste("n = ", c(sum(plot.data$risk == "4-5%"),sum(plot.data$risk == "9-10%"),
                                                                                                     sum(plot.data$risk == "14-15%"),sum(plot.data$risk == "19-20%"))))
  
  
  ## Now do the boxplot
  plot.box <- ggplot(data = plot.data, aes(x=risk,y=CIsize)) + geom_boxplot(outlier.size = 0.01, lwd = 0.25) + stat_boxplot(geom = "errorbar", lwd = 0.25) +
    xlab("Population derived risk") + ylab("2.5 - 97.5 percentile range across models") + scale_y_continuous(minor_breaks = NULL, breaks = seq(0,70,5), limits = c(0,70)) +
    theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(size = .5, color = "white"), plot.title = element_text(size=10),
          axis.title = element_text(size = 9)) +
    geom_text(data = label.data, aes(x=x,y=y,label = label), size = 3)
  
  
  ## Return both the data derived to do the plot, and the boxplot itself, this means the whole thing doesnt have to be run again
  ## if you want to edit the boxplot only
  return(list("plot.data.all" = plot.data.all,"plot.data" = plot.data,"plot.box" = plot.box))}


### First load the data from when N = 10000
load("R_out_C4/generate_risks_different_cohort_sizes_female_10000.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Create boxplot and associated data
boxplot.obj.10000 <- create.box.plot(risks.bootstrap)

## Add a ggtitle
boxplot.10000 <- boxplot.obj.10000$"plot.box" + ggtitle("Sample size = 10000")
Sys.time()

### First load the data from whenNext do it when N = 1434
load("R_out_C4/generate_risks_different_cohort_sizes_female_1434.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Create boxplot and associated data
boxplot.obj.1434 <- create.box.plot.1434(risks.bootstrap)

## Add a ggtitle
boxplot.1434 <- boxplot.obj.1434$"plot.box" + ggtitle(expression("Sample size = N"[min]))

Sys.time()

### First load the data from whenNext do it when N = 50000
load("R_out_C4/generate_risks_different_cohort_sizes_female_50000.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Create boxplot and associated data
boxplot.obj.50000 <- create.box.plot(risks.bootstrap)

## Add a ggtitle
boxplot.50000 <- boxplot.obj.50000$"plot.box" + ggtitle("Sample size = 50000")
Sys.time()

### First load the data from whenNext do it when N = 100000
load("R_out_C4/generate_risks_different_cohort_sizes_female_100000.RData")

## Turn risks into percentages
risks.bootstrap <- 100*risks.bootstrap

## Create boxplot and associated data
boxplot.obj.100000 <- create.box.plot(risks.bootstrap)

## Add a ggtitle
boxplot.100000 <- boxplot.obj.100000$"plot.box" + ggtitle("Sample size = 100000")
Sys.time()

## Make combined plot
Figure2_CI_boxplot_by_sample_size_female <- ggarrange(boxplot.1434,boxplot.10000,boxplot.50000,boxplot.100000,nrow = 2,ncol=2)
ggsave("figures/Figure2_CI_boxplot_by_sample_size_female.png",Figure2_CI_boxplot_by_sample_size_female, dpi = 600)


## Let's also get information for the text
print("1434")
boxplot.obj.1434 <- boxplot.obj.1434$"plot.box"
summary(boxplot.obj.1434$"data"$CIsize[boxplot.obj.1434$"data"$risk == "4-5%"])
summary(boxplot.obj.1434$"data"$CIsize[boxplot.obj.1434$"data"$risk == "9-10%"])
summary(boxplot.obj.1434$"data"$CIsize[boxplot.obj.1434$"data"$risk == "14-15%"])
summary(boxplot.obj.1434$"data"$CIsize[boxplot.obj.1434$"data"$risk == "19-20%"])

print("10000")
boxplot.obj.10000 <- boxplot.obj.10000$"plot.box"
summary(boxplot.obj.10000$"data"$CIsize[boxplot.obj.10000$"data"$risk == "4-5%"])
summary(boxplot.obj.10000$"data"$CIsize[boxplot.obj.10000$"data"$risk == "9-10%"])
summary(boxplot.obj.10000$"data"$CIsize[boxplot.obj.10000$"data"$risk == "14-15%"])
summary(boxplot.obj.10000$"data"$CIsize[boxplot.obj.10000$"data"$risk == "19-20%"])

print("50000")
boxplot.obj.50000 <- boxplot.obj.50000$"plot.box"
summary(boxplot.obj.50000$"data"$CIsize[boxplot.obj.50000$"data"$risk == "4-5%"])
summary(boxplot.obj.50000$"data"$CIsize[boxplot.obj.50000$"data"$risk == "9-10%"])
summary(boxplot.obj.50000$"data"$CIsize[boxplot.obj.50000$"data"$risk == "14-15%"])
summary(boxplot.obj.50000$"data"$CIsize[boxplot.obj.50000$"data"$risk == "19-20%"])

print("100000")
boxplot.obj.100000 <- boxplot.obj.100000$"plot.box"
summary(boxplot.obj.100000$"data"$CIsize[boxplot.obj.100000$"data"$risk == "4-5%"])
summary(boxplot.obj.100000$"data"$CIsize[boxplot.obj.100000$"data"$risk == "9-10%"])
summary(boxplot.obj.100000$"data"$CIsize[boxplot.obj.100000$"data"$risk == "14-15%"])
summary(boxplot.obj.100000$"data"$CIsize[boxplot.obj.100000$"data"$risk == "19-20%"])
