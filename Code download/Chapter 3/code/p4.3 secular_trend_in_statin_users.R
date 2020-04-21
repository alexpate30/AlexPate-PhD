## This will take the statin users cohort, and calculate the incidence of cvd events each year (used in Figure 4)

## Read in the statin users cohort
anal.dat.all <- read.table("D/statin_users_cohort.csv", sep="," , header=TRUE)

### They key variables for this are:
# risk10y is the 10 year risk score for patients, calculated according to same models from p1.1.2: generate_risks_female_model_B
# first_cvd_all_r: this is a number which represents the date of the patients first cvd event (0 = 1st Jan 1970)
# dtvalid_r: this is a number which represents the date of the patients start of follow up (0 = 1st Jan 1970)
# dtcens_r: this is a number which represents the date of the patients end of follow up (0 = 1st Jan 1970)
# stat_init_r: this is a number which represents the date of the start of the period of statin treatment (0 = 1st Jan 1970)
# stat_end_r: this is a number which represents the date of the end of the period of statin treatment (0 = 1st Jan 1970)
# cvd event = 1 if when a patient is censored, it is because they had a cvd event


## Females first

## Subset to females only
anal.dat<-anal.dat.all[anal.dat.all$gender == 1,]


## Want to do the same as I did for my original cohort
## For each row (stint of statin treatment), calculate the time at risk for each year, and whether they had an event (censored at point of event)

# First create the values for the cut-offs
d<-rep(0,19)
for (i in 1:19){d[i]<-round(365.25*(i-1))}
d<-d+10227
d<-c(d,d[19]+90)

## Do first follow times
ftime.list<-vector("list",19)

for (j in 1:19){
  ftime.list[[j]] <- pmax(rep(0,nrow(anal.dat)), 
                          pmin(rep(d[(j+1)],nrow(anal.dat)),pmin(anal.dat$stat_end_r,anal.dat$dtcens_r,anal.dat$first_cvd_all_r)) - 
                            pmax(rep(d[j],nrow(anal.dat)),anal.dat$dtvalid_r,anal.dat$stat_init_r)
  )
}

head(ftime.list[[1]])
head(ftime.list[[2]])
head(ftime.list[[3]])
head(ftime.list[[4]])
head(ftime.list[[5]])
head(ftime.list[[6]])
head(ftime.list[[7]])
head(ftime.list[[8]])
head(ftime.list[[9]])
head(ftime.list[[10]])
head(ftime.list[[11]])
head(ftime.list[[12]])
head(ftime.list[[13]])

## Now do wether a patient had an event in each year
event.list<-vector("list",19)
for (j in 1:19){
  event.list[[j]]<-pmin(anal.dat$cvd_event,as.numeric( (d[j] < anal.dat$first_cvd_all_r & 
                                                         anal.dat$first_cvd_all_r <= d[(j+1)]) ) )
}

## Create empty vectors for follow up and events each year
t.followup<-rep(0,19)
t.events<-rep(0,19)

## Assign elements
for (j in 1:19){t.followup[j]<-sum(ftime.list[[j]])/365.25}
for (j in 1:19){t.events[j]<-sum(event.list[[j]])}

## Empty vector for incidence rates
incidence<-rep(0,19)
for (j in 1:19){incidence[j]<-t.events[j]/t.followup[j]}

## Turn into table
inc.tab<-cbind(t.followup,t.events,1000*incidence)
rownames(inc.tab)<-1998:2016
colnames(inc.tab)<-c("followup","t.events","incidence")

inc.tab<-data.frame(inc.tab)
total<-c(sum(inc.tab[,1]),sum(inc.tab[,2]))
total<-c(total,1000*total[2]/total[1])

inc.tab.female<-rbind(inc.tab,total)
rownames(inc.tab.female)[20]<-c("total")

setwd("B")
save.image("secular_trend_statin_users.RData")


## Repear for male cohort

anal.dat<-anal.dat.all[anal.dat.all$gender == 0,]

head(anal.dat)

## Want to do the same as I did for my original cohort
## For each row (stint of statin treatment), calculate the time at risk for each year, and whether they had an event (censored at point of event)

# First create the values for the cut-offs
d<-rep(0,19)
for (i in 1:19){d[i]<-round(365.25*(i-1))}
d<-d+10227
d<-c(d,d[19]+90)

## Do first follow times
ftime.list<-vector("list",19)

for (j in 1:19){
  ftime.list[[j]] <- pmax(rep(0,nrow(anal.dat)), 
                          pmin(rep(d[(j+1)],nrow(anal.dat)),pmin(anal.dat$stat_end_r,anal.dat$dtcens_r,anal.dat$first_cvd_all_r)) - 
                            pmax(rep(d[j],nrow(anal.dat)),anal.dat$dtvalid_r,anal.dat$stat_init_r)
  )
}

head(ftime.list[[1]])
head(ftime.list[[2]])
head(ftime.list[[3]])
head(ftime.list[[4]])
head(ftime.list[[5]])
head(ftime.list[[6]])
head(ftime.list[[7]])
head(ftime.list[[8]])
head(ftime.list[[9]])
head(ftime.list[[10]])
head(ftime.list[[11]])
head(ftime.list[[12]])
head(ftime.list[[13]])

## Now do wether a patient had an event in each year
event.list<-vector("list",19)
for (j in 1:19){
  event.list[[j]]<-pmin(anal.dat$cvd_event,as.numeric( (d[j] < anal.dat$first_cvd_all_r & 
                                                          anal.dat$first_cvd_all_r <= d[(j+1)]) ) )
}

## Incidence rates
t.followup<-rep(0,19)
t.events<-rep(0,19)

for (j in 1:19){t.followup[j]<-sum(ftime.list[[j]])/365.25}
for (j in 1:19){t.events[j]<-sum(event.list[[j]])}

incidence<-rep(0,19)
for (j in 1:19){incidence[j]<-t.events[j]/t.followup[j]}

inc.tab<-cbind(t.followup,t.events,1000*incidence)
rownames(inc.tab)<-1998:2016
colnames(inc.tab)<-c("followup","t.events","incidence")

inc.tab<-data.frame(inc.tab)
total<-c(sum(inc.tab[,1]),sum(inc.tab[,2]))
total<-c(total,1000*total[2]/total[1])

inc.tab.male<-rbind(inc.tab,total)
rownames(inc.tab.male)[20]<-c("total")

setwd("B")
save.image("secular_trend_statin_users.RData")

inc.tab.male
inc.tab.female
