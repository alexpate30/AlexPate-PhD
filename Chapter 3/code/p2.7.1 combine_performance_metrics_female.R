rm(list=ls())
setwd("B")

## Firstmust read in each discrim measure file and change the name to the relevant one (they all have the name .A)
load("discrim_mesaures_female_modelB.RData")
D_RS.B<-D_RS.A
R2_D_RS.B<-R2_D_RS.A
GH_C_v1.B<-GH_C_v1.A
GH_C_v2.B<-GH_C_v2.A
HarrelsC.B<-HarrelsC.A
R2_R.B<-R2_R.A
rho_k_manual.B<-rho_k_manual.A

load("discrim_mesaures_female_modelC.RData")
D_RS.C<-D_RS.A
R2_D_RS.C<-R2_D_RS.A
GH_C_v1.C<-GH_C_v1.A
GH_C_v2.C<-GH_C_v2.A
HarrelsC.C<-HarrelsC.A
R2_R.C<-R2_R.A
rho_k_manual.C<-rho_k_manual.A

load("discrim_mesaures_female_modelD.RData")
D_RS.D<-D_RS.A
R2_D_RS.D<-R2_D_RS.A
GH_C_v1.D<-GH_C_v1.A
GH_C_v2.D<-GH_C_v2.A
HarrelsC.D<-HarrelsC.A
R2_R.D<-R2_R.A
rho_k_manual.D<-rho_k_manual.A


load("discrim_mesaures_female_modelE.RData")
D_RS.E<-D_RS.A
R2_D_RS.E<-R2_D_RS.A
HarrelsC.E<-HarrelsC.A

load("discrim_mesaures_female_modelF.RData")
D_RS.F<-D_RS.A
R2_D_RS.F<-R2_D_RS.A
HarrelsC.F<-HarrelsC.A

## Last, load model A, which doesn't need any names changing
load("discrim_mesaures_female_development_cohort_modelA_Tj_split_reanalysis.RData")



## Now I still have to do Uno C, R2_PM, BS and R_S
## Now I still have to do Uno C, R2_PM, BS and R_S

## First do R2_PM
## First do R2_PM
setwd("B")
load("generate_discrim_measures_female_R2_PM.RData")
R2_PM.A<-unlist(R2_PM.A)
R2_PM.B<-unlist(R2_PM.B)
R2_PM.C<-unlist(R2_PM.C)
R2_PM.D<-unlist(R2_PM.D)

load("generate_discrim_measures_female_R2_PM_EandF.RData")
R2_PM.E<-unlist(R2_PM.E)
R2_PM.F<-unlist(R2_PM.F)



## Next I want to do UnoC
## Next I want to do UnoC
setwd("B")
load("generate_discrim_measures_female_UnoC.RData")
R2_PM.A<-unlist(R2_PM.A)
R2_PM.B<-unlist(R2_PM.B)
R2_PM.C<-unlist(R2_PM.C)
R2_PM.D<-unlist(R2_PM.D)

load("generate_discrim_measures_female_UnoC_EandF.RData")
R2_PM.E<-unlist(R2_PM.E)
R2_PM.F<-unlist(R2_PM.F)


### Next I want to do rho_w_a
### Next I want to do rho_w_a
setwd("B")
load("generate_discrim_measures_female_rho_wa.RData")
rho_wa.A<-unlist(rho_wa.A)
rho_wa.B<-unlist(rho_wa.B)
rho_wa.C<-unlist(rho_wa.C)
rho_wa.D<-unlist(rho_wa.D)

load("generate_discrim_measures_female_rho_wa_EandF.RData")
rho_wa.E<-unlist(rho_wa.E)
rho_wa.F<-unlist(rho_wa.F)



## Next I want to do BS and R2_IBS
## Next I want to do BS and R2_IBS

## Need to calculate R2_IBS, both things required (IBS of model and IBS of null model) are avialable in the IBS list elements

setwd("B")
load("generate_discrim_measures_female_IBS_modelA.RData")
R2_IBS<-vector("list",20)
for (i in 1:20){R2_IBS[[i]]<-(1-IBS[[i]][2]/IBS[[i]][1])}
for (i in 1:20){IBS[[i]]<-(IBS[[i]][2])}

R2_IBS.A<-unlist(R2_IBS)
IBS.A<-unlist(IBS)

load("generate_discrim_measures_female_IBS_modelB.RData")
R2_IBS<-vector("list",20)
for (i in 1:20){R2_IBS[[i]]<-(1-IBS[[i]][2]/IBS[[i]][1])}
for (i in 1:20){IBS[[i]]<-(IBS[[i]][2])}

R2_IBS.B<-unlist(R2_IBS)
IBS.B<-unlist(IBS)

load("generate_discrim_measures_female_IBS_modelC.RData")
R2_IBS<-vector("list",20)
for (i in 1:20){R2_IBS[[i]]<-(1-IBS[[i]][2]/IBS[[i]][1])}
for (i in 1:20){IBS[[i]]<-(IBS[[i]][2])}

R2_IBS.C<-unlist(R2_IBS)
IBS.C<-unlist(IBS)

load("generate_discrim_measures_female_IBS_modelD.RData")
R2_IBS<-vector("list",20)
for (i in 1:20){R2_IBS[[i]]<-(1-IBS[[i]][2]/IBS[[i]][1])}
for (i in 1:20){IBS[[i]]<-(IBS[[i]][2])}

R2_IBS.D<-unlist(R2_IBS)
IBS.D<-unlist(IBS)



modelA<-c(mean(IBS.A),mean(R2_IBS.A),mean(R2_PM.A),mean(rho_wa.A),mean(rho_k_manual.A),mean(R2_R.A),mean(D_RS.A),mean(R2_D_RS.A),1-mean(HarrelsC.A),mean(UnoC.A),mean(GH_C_v1.A))
modelB<-c(mean(IBS.B),mean(R2_IBS.B),mean(R2_PM.B),mean(rho_wa.B),mean(rho_k_manual.B),mean(R2_R.B),mean(D_RS.B),mean(R2_D_RS.B),1-mean(HarrelsC.B),mean(UnoC.B),mean(GH_C_v1.B))
modelC<-c(mean(IBS.C),mean(R2_IBS.C),mean(R2_PM.C),mean(rho_wa.C),mean(rho_k_manual.C),mean(R2_R.C),mean(D_RS.C),mean(R2_D_RS.C),1-mean(HarrelsC.C),mean(UnoC.C),mean(GH_C_v1.C))
modelD<-c(mean(IBS.D),mean(R2_IBS.D),mean(R2_PM.D),mean(rho_wa.D),mean(rho_k_manual.D),mean(R2_R.D),mean(D_RS.D),mean(R2_D_RS.D),1-mean(HarrelsC.D),mean(UnoC.D),mean(GH_C_v1.D))
modelE<-c(NA         ,NA            ,mean(R2_PM.E),mean(rho_wa.E),NA                  ,NA          ,mean(D_RS.E),mean(R2_D_RS.E),1-mean(HarrelsC.E),mean(UnoC.E),NA)
modelF<-c(NA         ,NA            ,mean(R2_PM.F),mean(rho_wa.F),NA                  ,NA          ,mean(D_RS.F),mean(R2_D_RS.F),1-mean(HarrelsC.F),mean(UnoC.F),NA)


Table3<-data.frame("model A" = modelA,"model B" = modelB,"model C" = modelC,"model D" = modelD, "model E" = modelE, "model F" = modelF)
rownames(Table3)<-c("IBS","R2_IBS","R2_PM","rho_wa","rho_k_manual", "R2_R", "D_RS", "R2_D_RS", "HarrelsC", "UnoC", "GH_C")
print('Table done') 

rm(fit.BMI,fit.Cholesterol_HDL_Ratio,fit.SBP,fit.SBP_std,fit_data_parallel.1.m200000,fit_data_parallel.2.m200000,long_data_parallel_devel,long_data_parallel_test,
pred.BMI,pred.Cholesterol_HDL_Ratio,pred.SBP,pred.SBP_std,pred.model.1,pred.model.1.devel,pred.model.2,pred.model.2.devel,
anal.dat.devel,anal.dat.test,mean.dat.devel,mean.dat.test)

rm(anal.dat,mean.dat)

save.image("performance_metrics_female.RData")
