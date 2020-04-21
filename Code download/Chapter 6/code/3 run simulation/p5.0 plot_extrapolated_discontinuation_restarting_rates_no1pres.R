### SET ROOT DIRECTORY

### SET ROOT DIRECTORY


load("R_out_C6/statin_discontinuation_rates_for_sim_continuous_multiple_treatment_periods_no1pres_agemod.RData")

library(ggplot2)

## First get rid of NA's so we can plot
for (i in 1:50){disc1.extrapolated[[i]][is.na(disc1.extrapolated[[i]])] <- 21912}
plot.data.disc1 <- data.frame("Years" = c(disc1.extrapolated[[6]][disc1.extrapolated[[6]] < 16437], 
                                          disc1.extrapolated[[16]][disc1.extrapolated[[16]] < 12784], 
                                          disc1.extrapolated[[26]][disc1.extrapolated[[26]] < 9131],
                                          disc1.extrapolated[[36]][disc1.extrapolated[[36]] < 5479])/365.25,
                             "Age" = c(rep("45",sum(disc1.extrapolated[[6]] < 16437)),
                                       rep("55",sum(disc1.extrapolated[[16]] < 12784)),
                                       rep("65",sum(disc1.extrapolated[[26]] < 9131)),
                                       rep("75",sum(disc1.extrapolated[[36]] < 5479))),
                             "Discontinuation probability" = c(seq(0,1,0.0001)[disc1.extrapolated[[6]] < 16437],
                                                               seq(0,1,0.0001)[disc1.extrapolated[[16]] < 12784],
                                                               seq(0,1,0.0001)[disc1.extrapolated[[26]] < 9131],
                                                               seq(0,1,0.0001)[disc1.extrapolated[[36]] < 5479]))


plot.data.disc1$Age <- factor(plot.data.disc1$Age, levels = c("45","55","65","75"))

plot.disc1 <- ggplot(plot.data.disc1, aes(x = Years, y = Discontinuation.probability, col = Age)) + 
  geom_line(size = 0.7) + xlab("Time in years") + ylab("Discontinuation probability") + 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50,10))

plot.disc1


ggsave("figures/discontinuation1.extrapolation_no1pres.jpeg", plot.disc1)


plot.data.disc23 <- data.frame("Years" = c(disc2.extrapolated, disc3.extrapolated)/365.25,
                             "Treatment period" = c(rep("Second",10001),rep("Third",10001)),
                             "Discontinuation probability" = rep(seq(0,1,0.0001),2))

plot.data.disc23$Treatment.period <- factor(plot.data.disc23$Treatment.period, levels = c("Second","Third"))

plot.disc23 <- ggplot(plot.data.disc23, aes(x = Years, y = Discontinuation.probability, col = Treatment.period)) + 
  geom_line(size = 0.7) + xlab("Time in years") + ylab("Discontinuation probability") + 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50,10))

ggsave("figures/discontinuation23.extrapolation_no1pres.jpeg", plot.disc23)


plot.data.restart <- data.frame("Years" = c(restart1.extrapolated, restart2.extrapolated)/365.25,
                             "Treatment period" = c(rep("First",10001),rep("Second",10001)),
                             "Restarting probability" = rep(seq(0,1,0.0001),2))

plot.data.restart$Treatment.period <- factor(plot.data.restart$Treatment.period, levels = c("First","Second"))

plot.data.restart <- ggplot(plot.data.restart, aes(x = Years, y = Restarting.probability, col = Treatment.period)) + 
  geom_line(size = 0.7) + xlab("Time in years") + ylab("Restarting probability") + 
  scale_x_continuous(limits = c(0,50), breaks = seq(0,50,10))

ggsave("figures/restarting.extrapolation_no1pres.jpeg", plot.data.restart)