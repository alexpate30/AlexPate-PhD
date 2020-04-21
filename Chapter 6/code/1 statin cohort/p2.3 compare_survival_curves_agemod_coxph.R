### SET ROOT DIRECTORY

### SET ROOR DIRECTORY

### First get the normal straified kp plot

### Load the image
load("R_out_C6/survival_curves_restricted_1_year_FAKE.RData")

### Create a factor variable for age
anal.dat.all.dedup$age.real.cat <- cut(anal.dat.all.dedup$age.init, c(25,40,50,60,70,80,90),labels = c(1,2,3,4,5,6))

### Fit seperate km's within categories of this variable
fit.age.25.40 <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup[anal.dat.all.dedup$age.real.cat == 1,])
fit.age.40.50 <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup[anal.dat.all.dedup$age.real.cat == 2,])
fit.age.50.60 <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup[anal.dat.all.dedup$age.real.cat == 3,])
fit.age.60.70 <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup[anal.dat.all.dedup$age.real.cat == 4,])
fit.age.70.80 <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup[anal.dat.all.dedup$age.real.cat == 5,])
fit.age.80.90 <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup[anal.dat.all.dedup$age.real.cat == 6,])

### Create a dataset of the discontinuation probaibilities and point of follow up, and age
plot.data.disc.kp <- data.frame("Years" = c(fit.age.40.50$time/365.25, 
                                          fit.age.50.60$time/365.25, 
                                          fit.age.60.70$time/365.25,
                                          fit.age.70.80$time/365.25),
                              "Age" = c(rep("40 - 50",length(fit.age.40.50$time)),
                                        rep("50 - 60",length(fit.age.50.60$time)),
                                        rep("60 - 70",length(fit.age.60.70$time)),
                                        rep("70 - 80",length(fit.age.70.80$time))),
                              "Discontinuation probability" = c(1-fit.age.40.50$surv,
                                                                1-fit.age.50.60$surv,
                                                                1-fit.age.60.70$surv,
                                                                1-fit.age.70.80$surv))

### Turn age into a factor
plot.data.disc.kp$Age <- factor(plot.data.disc.kp$Age, levels = c("40 - 50","50 - 60","60 - 70","70 - 80"))
                              
### Cretea a ggplot
plot.disc.kp <- ggplot(plot.data.disc.kp, aes(x = Years, y = Discontinuation.probability, col = Age)) + 
  geom_line(size = 0.7) + xlab("Time in years") + ylab("Discontinuation probability") + 
  scale_x_continuous(limits = c(0,15), breaks = seq(0,15,1))

plot.disc.kp
ggsave("figures/discontinuation1 by age stratified kp_FAKE.jpeg", plot.disc.kp)

### Now get the equivalent graph using the coxph model
load("R_outC6/survival_curves_1year_restricted_agemod_FAKE.RData")


### Create a dataset of the discontinuation probaibilities and point of follow up, and age
plot.data.disc.coxph <- data.frame("Years" = c(disc1.survival.curves[[6]]$time/365.25, 
                                          disc1.survival.curves[[16]]$time/365.25, 
                                          disc1.survival.curves[[26]]$time/365.25,
                                          disc1.survival.curves[[36]]$time/365.25),
                              "Age" = c(rep("45",length(disc1.survival.curves[[6]]$time)),
                                        rep("55",length(disc1.survival.curves[[16]]$time)),
                                        rep("65",length(disc1.survival.curves[[26]]$time)),
                                        rep("75",length(disc1.survival.curves[[36]]$time))),
                              "Discontinuation probability" = c(1-disc1.survival.curves[[6]]$surv,
                                                                1-disc1.survival.curves[[16]]$surv,
                                                                1-disc1.survival.curves[[26]]$surv,
                                                                1-disc1.survival.curves[[36]]$surv))

### Turn age into a factor
plot.data.disc.coxph$Age <- factor(plot.data.disc.coxph$Age, levels = c("45","55","65","75"))

### Cretea a ggplot
plot.disc.coxph <- ggplot(plot.data.disc.coxph, aes(x = Years, y = Discontinuation.probability, col = Age)) + 
  geom_line(size = 0.7) + xlab("Time in years") + ylab("Discontinuation probability") + 
  scale_x_continuous(limits = c(0,15), breaks = seq(0,15,1))

plot.disc.coxph
ggsave("figures/discontinuation1 by age coxph_FAKE.jpeg", plot.disc.coxph)
                              