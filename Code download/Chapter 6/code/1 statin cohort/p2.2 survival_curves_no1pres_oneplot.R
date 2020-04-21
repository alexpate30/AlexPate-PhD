### SET ROOT DIRECTORY

### SET ROOR DIRECTORY

library(survival)
library(survminer)

load("R_out_C6/survival_curves_restricted_1_year_no1pres_FAKE.RData")

## First need to combine the datasets
# surv.obj.disc <- survfit(Surv(statin_usage,is.cens90) ~ 1, data=anal.dat.all.dedup)
# surv.obj.disc2 <- survfit(Surv(restart.time2,is.cens2) ~ 1, data=restart.dat2)
# surv.obj.disc3 <- survfit(Surv(restart.time4,is.cens4) ~ 1, data=restart.dat4)
# 
# surv.obj.restart <- survfit(Surv(restart.time,is.cens) ~ 1, data=restart.dat)
# surv.obj.restart2 <- survfit(Surv(restart.time3,is.cens3) ~ 1, data=restart.dat3)
# surv.obj.restart3<- survfit(Surv(restart.time5,is.cens5) ~ 1, data=restart.dat5)

## Create new datasets with the right variables names
disc1.dat <- anal.dat.all.dedup
disc1.dat$is.cens <- disc1.dat$is.cens90
disc1.dat$TP <- 1
disc1.dat <- disc1.dat[,c("statin_usage","is.cens","TP")]

disc2.dat <- restart.dat2
disc2.dat$is.cens <- disc2.dat$is.cens2
disc2.dat$statin_usage<- disc2.dat$restart.time2
disc2.dat$TP <- 2
disc2.dat <- disc2.dat[,c("statin_usage","is.cens","TP")]

disc3.dat <- restart.dat4
disc3.dat$is.cens <- disc3.dat$is.cens4
disc3.dat$statin_usage<- disc3.dat$restart.time4
disc3.dat$TP <- 3
disc3.dat <- disc3.dat[,c("statin_usage","is.cens","TP")]


disc4.dat <- restart.dat6
disc4.dat$is.cens <- disc4.dat$is.cens6
disc4.dat$statin_usage<- disc4.dat$restart.time6
disc4.dat$TP <- 4
disc4.dat <- disc4.dat[,c("statin_usage","is.cens","TP")]

res1.dat <- restart.dat
res1.dat$is.cens <- res1.dat$is.cens
res1.dat$statin_usage<- res1.dat$restart.time
res1.dat$TP <- 1
res1.dat <- res1.dat[,c("statin_usage","is.cens","TP")]

res2.dat <- restart.dat3
res2.dat$is.cens <- res2.dat$is.cens3
res2.dat$statin_usage<- res2.dat$restart.time3
res2.dat$TP <- 2
res2.dat <- res2.dat[,c("statin_usage","is.cens","TP")]

res3.dat <- restart.dat5
res3.dat$is.cens <- res3.dat$is.cens5
res3.dat$statin_usage<- res3.dat$restart.time5
res3.dat$TP <- 3
res3.dat <- res3.dat[,c("statin_usage","is.cens","TP")]


## Now concatenate them
disc.data.3 <- rbind(disc1.dat,disc2.dat,disc3.dat)
disc.data.4 <- rbind(disc1.dat,disc2.dat,disc3.dat,disc4.dat)
res.data <- rbind(res1.dat,res2.dat,res3.dat)

disc.gg.3 <- survfit(Surv(statin_usage,is.cens) ~ TP, data=disc.data.3)
disc.gg.plot.3 <- ggsurvplot(disc.gg.3,data = disc.data.3,fun = "event",xscale ="d_y",break.x.by = 2.5*365.25, risk.table = TRUE, xlim=c(0,365.25*10),
                             xlab = "Years", ylab = "Discontinuation probability", size = 1, censor = FALSE, fontsize=2.5, risk.table.y.text = FALSE, 
                             legend.labs = c("First treatment period", "Second treatment period", "Third treatment period")) +
  guides(colour = guide_legend(nrow = 2))

disc.gg.4 <- survfit(Surv(statin_usage,is.cens) ~ TP, data=disc.data.4)
disc.gg.plot.4 <- ggsurvplot(disc.gg.4,data = disc.data.4,fun = "event", xscale ="d_y",break.x.by = 2.5*365.25, risk.table = TRUE, xlim=c(0,365.25*10), 
                             xlab = "Years", ylab = "Discontinuation probability", size = 1, censor = FALSE,fontsize=2.5, risk.table.y.text = FALSE, title = "A Discontinuation",
                             legend.labs = c("First treatment period", "Second treatment period", "Third treatment period", "Fourth treatment period")) +
  guides(colour = guide_legend(nrow = 2))


res.gg <- survfit(Surv(statin_usage,is.cens) ~ TP, data=res.data)
res.gg.plot <- ggsurvplot(res.gg,data = res.data,fun = "event",xscale ="d_y",break.x.by = 2.5*365.25, risk.table = TRUE, xlim=c(0,365.25*10),
                          xlab = "Years", ylab = "Restarting probability", size = 1, censor = FALSE, fontsize = 2.5, risk.table.y.text = FALSE, title = "B Restarting",
                          legend.labs = c("First restarting period", "Second restarting period", "Third restarting period")) +
  guides(colour = guide_legend(nrow = 2))


ggsave("figures/Discontinuation survival plots one graph first three 1year restricted no1pres_FAKE.jpg",print(disc.gg.plot.3))
ggsave("figures/Discontinuation survival plots one graph first four 1year restricted no1pres_FAKE.jpg",print(disc.gg.plot.4))
ggsave("figures/Restarting survival plots one graph 1year restricted no1pres_FAKE.jpg",print(res.gg.plot))
