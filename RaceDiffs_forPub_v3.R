library(grid)
library(gridExtra)
library(ggplot2)
library(ppcor)
library(dplyr)
library(plyr)
library(ggfortify)
library(broom)
library(car)
library(pwr)
library(tableone)
library(modelr)
library(purrr)
library(ggpubr)
library(UpSetR)

#######Data set up
source("W:/ances/julie/ADRC/RaceDiffs/RaceDiffs_Functions.R") #Has bootstrapping function and z scoring function
source("W:/ances/julie/Data/StarkRequest/CommonFuncs.R")
source("W:/ances/julie/ADRC/RaceDiffs/RaceDiffs_MRIcleaning_DF15.R") #Getting MRI Data

source("W:/ances/julie/ADRC/RaceDiffs/RaceDiffs_Amyloidcleaning.R") #Getting centiloid data
source("W:/ances/julie/ADRC/RaceDiffs/RaceDiffs_Taucleaning.R") #Getting tau data
source("W:/ances/julie/ADRC/RaceDiffs/RaceDiffs_rsfccleaning.R") #Getting rsfc data

##################################################
#Dropping the IDs associated with anyone who appears in the following projects: NP673, fACS, NP801, H8A-MC-LZAT, HASD_ST_Follow, CCIR0197
# MRI_hold<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_3TMR.csv")
# PIB_hold<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_PIB.csv")
# AV45_hold<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_AV45.csv")
# TAU_hold<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_TAU.csv")
# 
# MRI_hold<-MRI_hold[,c("Map", "Project")]
# PIB_hold<-PIB_hold[,c("Map", "Project")]
# AV45_hold<-AV45_hold[,c("Map", "Project")]
# TAU_hold<-TAU_hold[,c("Map", "Project")]
# 
# hold<-rbind(MRI_hold, PIB_hold, AV45_hold, TAU_hold)
# rm(MRI_hold, PIB_hold, AV45_hold, TAU_hold)
# 
# hold<-hold[hold$Project == "NP673" | hold$Project == "fACS" | hold$Project == "NP801" |
#              hold$Project == "H8A-MC-LZAT" | hold$Project == "RAPID",]
# hold<-unique(hold)
# 
# ADI<-ADI[!(ADI$ID %in% hold$Map),]
# Amyloid<-Amyloid[!(Amyloid$ID %in% hold$Map),]
# Cardio<-Cardio[!(Cardio$ID %in% hold$Map),]
# Cardio2<-Cardio2[!(Cardio2$ID %in% hold$Map),]
# demogs<-demogs[!(demogs$ID %in% hold$Map),]
# df_FC<-df_FC[!(df_FC$ID %in% hold$Map),]
# df.MRI<-df.MRI[!(df.MRI$ID %in% hold$Map),]
# df.once<-df.once[!(df.once$ID %in% hold$Map),]
# df.WMH<-df.WMH[!(df.WMH$ID %in% hold$Map),]
# MRI.once<-MRI.once[!(MRI.once$ID %in% hold$Map),]
# PIB.once<-PIB.once[!(PIB.once$ID %in% hold$Map),]
# tau.once<-tau.once[!(tau.once$ID %in% hold$Map),]
# rm(hold)

########Data set up conclusion



#Demographics of people with images
listVars<-c("Age", "GENDER", "EDUC",  "apoe4",  "SES")
catVars<-c("GENDER", "apoe4")
CreateTableOne(vars = c(listVars, "Centiloid"), data = Amyloid, factorVars = catVars, strata = c("race2", "CDRgreaterthan0"))
CreateTableOne(vars = c(listVars, "Tauopathy"), data = tau.once, factorVars = catVars, strata = c("race2", "CDRgreaterthan0"))
CreateTableOne(vars = c(listVars, "ADsig"), data = MRI.once, factorVars = catVars, strata = c("race2", "CDRgreaterthan0"))
CreateTableOne(vars = c(listVars, "IntraSig"), data = df.once, factorVars = catVars, strata = c("race2", "CDRgreaterthan0"))
CreateTableOne(vars = listVars, data = Cardio2, factorVars = catVars, strata = c("race2", "CDRgreaterthan0"))



# VENN<-data.frame("ID" = as.numeric(unique(c(unique(Amyloid$ID), unique(tau.once$ID), unique(MRI.once$ID), unique(df.once$ID), unique(Cardio2$ID)))),
#                  "Amyloid" = NA, "Tau" = NA, "MRI" = NA, "rsFC" = NA, "Mediators" = NA)
# VENN$Amyloid<-ifelse(VENN$ID %in% Amyloid$ID, 1, 0)
# VENN$Tau<-ifelse(VENN$ID %in% tau.once$ID, 1, 0)
# VENN$MRI<-ifelse(VENN$ID %in% MRI.once$ID, 1, 0)
# VENN$rsFC<-ifelse(VENN$ID %in% df.once$ID, 1, 0)
# VENN$Mediators<-ifelse(VENN$ID %in% Cardio2$ID, 1, 0)
# 
# upset(VENN, nsets = 6, number.angles = 0, point.size = 3.5, line.size = 1, 
#       mainbar.y.label = "Number of Participants", sets.x.label = "Measurement Counts", 
#       text.scale = c(1.3, 1.3, 1, 1, 2, 1.3), order.by = "freq")

###########################

# ggplot(MRI.once, aes(x = race2, y = SES,fill = GENDER, add = "jitter")) + geom_boxplot()+
#    xlab ("Race") + 
#   ylab("Area Deprivation Index\nHigh values are associated with poor healthcare access") +
#   geom_dotplot(binaxis='y', stackdir='center',
#                  position=position_dodge(), dotsize = 0.1)
# 
# ggplot(MRI.once, aes(x = EDUC, y = SES, colour = race2, shape = race2)) + geom_point() + geom_smooth(method = "lm")

#Education and SES are significantly correlated, R = -.27 overall, -.36 for AA's & -.21 for whites
# cor.test(MRI.once[complete.cases(MRI.once$SES), "EDUC"], MRI.once[complete.cases(MRI.once$SES), "SES"])
# cor.test(MRI.once[complete.cases(MRI.once$SES) & MRI.once$race2 == "Black", "EDUC"], MRI.once[complete.cases(MRI.once$SES)& MRI.once$race2 == "Black", "SES"])
# cor.test(MRI.once[complete.cases(MRI.once$SES) & MRI.once$race2 == "White", "EDUC"], MRI.once[complete.cases(MRI.once$SES) & MRI.once$race2 == "White", "SES"])

################################################
#After controlling for differences in age, sex and education
#There are significant volume differences, in both the hippocampus and signature region
#between CDR's and between races
################################################
################################################
################################################
#Amyloid Analysis
model.null<-lm(Centiloid ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = Amyloid[complete.cases(Amyloid$SES),])


ggplot(Amyloid[complete.cases(Amyloid$SES),], aes(x = SES, y = Centiloid, shape = race2, colour = race2)) + geom_point() + geom_smooth(method = "lm") +
  xlab("Area Deprivation Index\nHigh values are associated with poor healthcare access") + ylab("Centiloid") + ylim(c(-10, 130)) +
  labs(colour = "Race", shape = "Race") + theme(legend.position = "bottom")


library(sandwich)
library(lmtest)
AmyloidCoefs<-coeftest(model.null, vcov = vcovHC(model.null)) #Uses method laid out in https://www.jstatsoft.org/article/view/v011i10 to deal with heteroskedasity
#d = (m1 - m2 )/ sqrt((sd1^2 + sd2^2)/2)
#Getting effect size
7.55197 / sqrt(((3.29734*sqrt(80))^2 + (3.29734*sqrt(536))^2)/2)

pAmyloid<-ggplot(Amyloid, aes(x=CDRgreaterthan0, y=PUP_fSUVR_rsf_TOT_CORTMEAN, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Mean Cortical Amyloid Accumulation") + ggtitle("Centiloids") + guides(fill=guide_legend(title="Race")) + 
  ylim(c(0.9, 3.4)) + scale_x_discrete(labels=c("0" = "CDR = 0", "1" = "CDR > 0"))+theme(legend.position = "bottom")

#Create corrected figure
Amyloid_corrected<-Amyloid
Amyloid_corrected$apoe4<-as.numeric(Amyloid_corrected$apoe4)
Amyloid_corrected$Centiloid_corrected<-Amyloid_corrected$Centiloid -AmyloidCoefs[6, 1]*Amyloid_corrected$Age - 
  AmyloidCoefs[5,1]*Amyloid_corrected$apoe4 - AmyloidCoefs[1, 1]

pA_corrected<-ggplot(Amyloid_corrected, aes(x=CDRgreaterthan0, y=Centiloid_corrected, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Mean Cortical Amyloid Accumulation") + ggtitle("Centiloids\nCorrected for Age and APOE") + guides(fill=guide_legend(title="Race")) + 
  scale_x_discrete(labels=c("0" = "CDR = 0", "1" = "CDR > 0"))+theme(legend.position = "none") + ylim(c(-50, 110))


rm(list_mods, model, model.diag.metrics, model.null, mods_df, r.std.error, tidied, resamples, boot_education, boot_lin_reg)
################################################
#Tau Analysis 
coeftest(lm(Tauopathy ~ race2 +  CDRgreaterthan0 + GENDER + EDUC + apoe4 + Age + SES + SES:race2, data = tau.once),
         vcov = vcovHC(lm(Tauopathy ~ race2 + CDRgreaterthan0 + GENDER + EDUC + apoe4 + Age + SES + SES:race2, data = tau.once)))


tau.once_alt<-tau.once[tau.once$CDRgreaterthan0 ==0,]

model.null<-lm(Tauopathy ~ race2 +GENDER + EDUC + apoe4 + Age + SES + SES:race2, data = tau.once_alt)
TauCoefs<-coeftest(model.null, vcov = vcovHC(model.null))


pTau<-ggplot(tau.once_alt, aes(x=CDRgreaterthan0, y=Tauopathy, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Mean Cortical Tau Accumulation") + ggtitle("AV-1451") + guides(fill=guide_legend(title="Race")) + 
   scale_x_discrete(labels=c("0" = "CDR = 0"))+theme(legend.position = "bottom")



hold<-tau.once_alt[tau.once_alt$CDRgreaterthan0 == 0,]
hold<-hold[,c("race2", "Tauopathy")]

#Caculating Effect Size
CohenD<-(mean(hold[hold$race2 == "African-American", "Tauopathy"]) - 
    mean(hold[hold$race2 == "Caucasian", "Tauopathy"]))/sqrt((sd(hold[hold$race2 == "African-American", "Tauopathy"])^2 +
                                                                                 sd(hold[hold$race2 == "Caucasian", "Tauopathy"])^2)/2)
pwr.t2n.test(n1 = 34, n2= 262, sig.level = 0.05, d = abs(CohenD))        

#Create corrected figure
Tau_corrected<-tau.once_alt
Tau_corrected$apoe4<-as.numeric(Tau_corrected$apoe4)
Tau_corrected$GENDERnum<-ifelse(Tau_corrected$GENDER == "male", 1, 0)
Tau_corrected$Tau_corrected<-Tau_corrected$Tauopathy -TauCoefs[6, 1]*Tau_corrected$Age - 
  TauCoefs[5,1]*Tau_corrected$apoe4 - TauCoefs[1, 1] - TauCoefs[3, 1]*Tau_corrected$GENDERnum

pT_corrected<-ggplot(Tau_corrected, aes(x=CDRgreaterthan0, y=Tau_corrected, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Mean Cortical Tau Accumulation") + ggtitle("Tauopathy\nCorrected for Age, Sex, and APOE") + guides(fill=guide_legend(title="Race")) + 
  scale_x_discrete(labels=c("0" = "CDR = 0", "1" = "CDR > 0"))+theme(legend.position = "none") 



rm(model, model.null, hold)
################################################
###########Neurodegeneration

#Volumes
ADsigVol<-ggplot(MRI.once, aes(x=CDRgreaterthan0, y=ADsig, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("AD Signature Mean Volume") + guides(fill=guide_legend(title="Race")) +
  ylim(c(-3, 3)) + scale_x_discrete(labels=c("0" = "CDR = 0", "1" = "CDR > 0"))

model.null<-lm(ADsig ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER + EDUC + apoe4 + Age, data = MRI.once[complete.cases(MRI.once$SES),])
VolCoefs<-coeftest(model.null, vcov = vcovHC(model.null))
0.2734724/ sqrt(((0.0306634*sqrt(169))^2 + (0.0306634*sqrt(935))^2)/2) #CDR

0.1899325/ sqrt(((0.0291447*sqrt(169))^2 + (0.0291447*sqrt(935))^2)/2) #Race


#Create corrected figure
Vol_corrected<-MRI.once
Vol_corrected$GENDERnum<-ifelse(Vol_corrected$GENDER == "male", 1, 0)
Vol_corrected$Vol_corrected<-Vol_corrected$ADsig -VolCoefs[7, 1]*Vol_corrected$Age - 
  VolCoefs[1, 1] - VolCoefs[4, 1]*Vol_corrected$GENDERnum

pV_corrected<-ggplot(Vol_corrected, aes(x=CDRgreaterthan0, y=Vol_corrected, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("AD Signature Volume") + ggtitle("AD Signature Volume\nCorrected for Age and Sex") + guides(fill=guide_legend(title="Race")) + 
  scale_x_discrete(labels=c("0" = "CDR = 0", "1" = "CDR > 0"))+theme(legend.position = "bottom") 


rm(demogs.tau, lm_coefs, model, model.null, p1, p2)
#################################################################################
#Resting State

MakeBarPlots(subset(var.contrib[,1], var.contrib[,1] > 0), "Intranetwork Signature", "27% of variance")

model.null<-lm(IntraSig ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = df.once)
FCCoef<-coeftest(model.null, vcov = vcovHC(model.null))
0.01462682/ sqrt(((0.01019956*sqrt(71))^2 + (0.01019956*sqrt(433))^2)/2) #race

0.03397995/ sqrt(((0.01098822*sqrt(71))^2 + (0.01098822*sqrt(433))^2)/2) #CDR



IntraSig<-ggplot(df.once, aes(x=CDRgreaterthan0, y=IntraSig, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Intranetwork rs-fc Signature Connection") + guides(fill=guide_legend(title="Race")) +
  scale_x_discrete(labels=c("0" = "CDR = 0", "1" = "CDR > 0"))


#Create corrected figure
FC_corrected<-df.once
FC_corrected$Vol_corrected<-FC_corrected$IntraSig -FCCoef[7, 1]*FC_corrected$Age - 
  FCCoef[1, 1]

pFC_corrected<-ggplot(FC_corrected, aes(x=CDRgreaterthan0, y=Vol_corrected, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Rs-FC Intranetwork Signature") + ggtitle("Rs-FC Intranetwork Signature\nCorrected for Age") + guides(fill=guide_legend(title="Race")) + 
  scale_x_discrete(labels=c("0" = "CDR = 0", "1" = "CDR > 0"))+theme(legend.position = "bottom")

lay<-rbind(c(1, 1), c(2, 2), c(3, 4))
grid.arrange(pA_corrected, pT_corrected, pV_corrected, pFC_corrected, layout_matrix = lay)

#############################################################################################
#####################
############################################################
Amyloid$race2<-relevel(Amyloid$race2, ref = "Caucasian")
Amyloid$CDRgreaterthan0<-relevel(Amyloid$CDRgreaterthan0, ref = "1")
Amyloid$apoe4<-relevel(Amyloid$apoe4, ref = "1")
Amyloid$GENDER<-relevel(Amyloid$GENDER, ref = "male")
Amyloid$Age_centered<-Amyloid$Age - mean(Amyloid$Age)
tau.once$Age_centered<-tau.once$Age - mean(tau.once$Age)
tau.once$race2<-relevel(tau.once$race2, ref = "Caucasian")
tau.once$apoe4<-relevel(tau.once$apoe4, ref = "1")
tau.once$GENDER<-relevel(tau.once$GENDER, ref = "male")
MRI.once$Age_centered<-MRI.once$Age - mean(MRI.once$Age)
MRI.once$race2<-relevel(MRI.once$race2, ref = "White")
MRI.once$CDRgreaterthan0<-relevel(MRI.once$CDRgreaterthan0, ref = "1")
MRI.once$apoe4<-relevel(MRI.once$apoe4, ref = "1")
MRI.once$GENDER<-relevel(MRI.once$GENDER, ref = "male")
df.once$Age_centered<-df.once$Age - mean(df.once$Age)
df.once$race2<-relevel(df.once$race2, ref = "Caucasian")
df.once$CDRgreaterthan0<-relevel(df.once$CDRgreaterthan0, ref = "1")
df.once$apoe4<-relevel(df.once$apoe4, ref = "1")
df.once$GENDER<-relevel(df.once$GENDER, ref = "male")

model.null.centiloidF<-lm(Centiloid ~ race2*CDRgreaterthan0 + race2*GENDER + race2*apoe4 + race2*Age_centered +
                           CDRgreaterthan0*GENDER + CDRgreaterthan0*apoe4 + CDRgreaterthan0*Age_centered +
                           GENDER*apoe4 + GENDER*Age_centered + apoe4*Age_centered, data = Amyloid)
model.null.PETtauF<-lm(Tauopathy ~  race2*GENDER + race2*apoe4 + race2*Age_centered +
                        GENDER*apoe4 + GENDER*Age_centered + apoe4*Age_centered, data = tau.once)
model.null.volF<-lm(ADsig ~ race2*CDRgreaterthan0 + race2*GENDER + race2*apoe4 + race2*Age_centered +
                      CDRgreaterthan0*GENDER + CDRgreaterthan0*apoe4 + CDRgreaterthan0*Age_centered +
                      GENDER*apoe4 + GENDER*Age_centered + apoe4*Age_centered, data = MRI.once)
model.null.FCF<-lm(IntraSig ~  race2*CDRgreaterthan0 + race2*GENDER + race2*apoe4 + race2*Age_centered +
                     CDRgreaterthan0*GENDER + CDRgreaterthan0*apoe4 + CDRgreaterthan0*Age_centered +
                     GENDER*apoe4 + GENDER*Age_centered + apoe4*Age_centered, data = df.once)

stargazer(coeftest(model.null.centiloidF, vcov = vcovHC(model.null.centiloidF)), 
          coeftest(model.null.PETtauF, vcov = vcovHC(model.null.PETtauF)), 
          coeftest(model.null.volF, vcov = vcovHC(model.null.volF)),
          coeftest(model.null.FCF, vcov = vcovHC(model.null.FCF)),
          type = "html", out = "W:/ances/julie/ADRC/SexDiffsTable_Chengjie.htm",
          column.labels=c("Amyloid - Centiloids", "Tau - AV1451", "AD Signature Volume", "Intranetwork Rs-fc"),
          # covariate.labels = c("Race - Caucasian", "CDR - Greater than 0", "Sex - Male", "APOE4+", "Age",
          #                      "Race - Caucasian: CDR - Greater than 0", "Intercept"),
          star.cutoffs = c(0.05, 0.01, 0.001))

############################################################
############################################################
levels(MRI.once$race2)<-c("African-American", "Caucasian")
model.null.centiloid<-lm(Centiloid ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = Amyloid)
msum<-anova(model.null.centiloid)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])

coeftest(model.null.centiloid, vcov = vcovHC(model.null.centiloid))
7.65148/ sqrt(((3.28411*sqrt(80))^2 + (3.28411*sqrt(536))^2)/2) #Race
23.14374/ sqrt(((2.77910*sqrt(80))^2 + (2.77910*sqrt(542))^2)/2) #CDR
1.17172/ sqrt(((0.13679*sqrt(80))^2 + (0.13679*sqrt(542))^2)/2) #Interaction

model.null.PETtau<-lm(Tauopathy ~ race2  + GENDER +  apoe4 + Age, data = tau.once_alt)
coeftest(model.null.PETtau, vcov = vcovHC(model.null.PETtau))

model.null.PETtau<-lm(Tauopathy ~ race2  + CDRgreaterthan0 + GENDER +  apoe4 + Age, data = tau.once)

msum<-anova(model.null.PETtau)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])
coeftest(model.null.PETtau, vcov = vcovHC(model.null.PETtau))
0.4802535/ sqrt(((0.1070717*sqrt(37))^2 + (0.1070717*sqrt(304))^2)/2)
0.0213445/ sqrt(((0.0439524*sqrt(37))^2 + (0.0439524*sqrt(304))^2)/2)


model.null.vol<-lm(ADsig ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = MRI.once)
coeftest(model.null.vol, vcov = vcovHC(model.null.vol))
msum<-anova(model.null.vol)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])
msum

model.null.FC<-lm(IntraSig ~  race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = df.once)
coeftest(model.null.FC, vcov = vcovHC(model.null.FC))
msum<-anova(model.null.FC)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])

library(stargazer)
#Amyloid and tau violated homoskedacity, volumes and FC did not
stargazer(coeftest(model.null.centiloid, vcov = vcovHC(model.null.centiloid)), 
          coeftest(model.null.PETtau, vcov = vcovHC(model.null.PETtau)), 
          coeftest(model.null.vol, vcov = vcovHC(model.null.vol)),
          coeftest(model.null.FC, vcov = vcovHC(model.null.FC)),
          type = "html", out = "W:/ances/julie/ADRC/SexDiffsTable_revised.htm",
          column.labels=c("Amyloid - Centiloids", "Tau - AV1451", "AD Signature Volume", "Intranetwork Rs-fc"),
          # covariate.labels = c("Race - Caucasian", "CDR - Greater than 0", "Sex - Male", "APOE4+", "Age",
          #                      "Race - Caucasian: CDR - Greater than 0", "Intercept"),
          star.cutoffs = c(0.05, 0.01, 0.001))



#################
#Cardio Params
pBPSYS<-ggplot(Cardio2, aes(x=CDRgreaterthan0, y=BPSYS, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Systolic Blood Pressure (BP)") + guides(fill=guide_legend(title="Race")) +
  scale_x_discrete(labels=c("0" = "Cognitively Normal", "1" = "CDR > 0")) +  ylim(c(80, 185))
pBMI<-ggplot(Cardio2, aes(x=CDRgreaterthan0, y=BMI, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Body Mass Index (BMI)") + guides(fill=guide_legend(title="Race")) +
  scale_x_discrete(labels=c("0" = "Cognitively Normal", "1" = "CDR > 0"))

pWMH<-ggplot(Cardio2, aes(x=CDRgreaterthan0, y=WMH_Volume, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("White Matter Hyperintensity (WMH)") + guides(fill=guide_legend(title="Race")) +
  scale_x_discrete(labels=c("0" = "Cognitively Normal", "1" = "CDR > 0"))

pADI<-ggplot(Cardio2, aes(x=CDRgreaterthan0, y=ADI_NATRANK, fill=race2)) +  
  geom_boxplot(alpha = 0.8, outlier.shape = NA) + geom_point(aes(fill = race2), size = 1, shape = 21, position = position_jitterdodge()) +
  xlab("") + ylab("Area Deprivation Index (ADI)") + guides(fill=guide_legend(title="Race")) +
  scale_x_discrete(labels=c("0" = "Cognitively Normal", "1" = "CDR > 0"))
library(lemon)
grid_arrange_shared_legend(pADI,pWMH, pBPSYS, pBMI,  nrow = 2, ncol = 2)

model.null.ADI<-lm(ADI_NATRANK ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = Cardio2)
coeftest(model.null.ADI, vcov = vcovHC(model.null.ADI))
msum<-anova(model.null.ADI)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])
msum

29.768615/ sqrt(((2.801576*sqrt(134))^2 + (2.801576*sqrt(878))^2)/2)
model.null.BP<-lm(BPSYS ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = Cardio2)
coeftest(model.null.BP, vcov = vcovHC(model.null.BP))
msum<-anova(model.null.BP)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])
msum
5.827477/ sqrt(((1.571060*sqrt(134))^2 + (1.5710606*sqrt(878))^2)/2)

model.null.BMI<-lm(BMI ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = Cardio2)
coeftest(model.null.BMI, vcov = vcovHC(model.null.BMI))
msum<-anova(model.null.BMI)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])
msum
2.422446 / sqrt(((0.523297*sqrt(134))^2 + (0.523297*sqrt(878))^2)/2)

model.null.WMH<-lm(WMH_Volume ~ race2 + CDRgreaterthan0 + race2:CDRgreaterthan0 + GENDER +  apoe4 + Age, data = Cardio2)
coeftest(model.null.WMH, vcov = vcovHC(model.null.WMH))
msum<-anova(model.null.WMH)
msum[["Sum Sq"]]/sum(msum[["Sum Sq"]])
msum
4456.698 / sqrt(((1692.249*sqrt(134))^2 + (1692.249*sqrt(878))^2)/2)


stargazer(coeftest(model.null.ADI, vcov = vcovHC(model.null.ADI)), 
          coeftest(model.null.BP, vcov = vcovHC(model.null.BP)), 
          coeftest(model.null.BMI, vcov = vcovHC(model.null.BMI)),
          coeftest(model.null.WMH, vcov = vcovHC(model.null.WMH)),
          type = "html", out = "W:/ances/julie/ADRC/RaceDiffs_CardioSES.htm",
          column.labels=c("ADI", "BP", "BMI", "WMH"),
          covariate.labels = c("Race - Caucasian", "CDR - Greater than 0", "Sex - Male", "APOE4+", "Age",
                               "Race - Caucasian: CDR - Greater than 0", "Intercept"),
          star.cutoffs = c(0.05, 0.01, 0.001))



##########################
#Mediation analysis
library("mma")
Amyloid<-MatchbyNearestDate(Amyloid, Cardio[,c("ID", "MR_Date", "BPSYS", "BMI")], "ID", "MR_Date", "MR_Date")
colnames(Amyloid)[11]<-"MR_Date"
#Amyloid<-MatchbyNearestDate(Amyloid, df.WMH[,c("ID", "MR_Date", "WMH_Volume")], "ID", "MR_Date.x", "MR_Date")
Amyloid<-merge(Amyloid, df.WMH[,c("ID", "MR_Date", "WMH_Volume")], by = c("ID", "MR_Date"), all.x = TRUE, all.y =  FALSE)
Amyloid<-merge(Amyloid, ADI, by = "ID", all.x = TRUE, all.y =  FALSE)
Amyloid<-subset(Amyloid,!duplicated(Amyloid$ID))
Amyloid<-Amyloid[abs(as.numeric(Amyloid$MR_Date - Amyloid$MR_Date.y)/365) < 2,] #All measures taken within 2 years of each other

tau.once_alt<-MatchbyNearestDate(as.data.frame(tau.once_alt), Cardio2[,c("ID", "MR_Date", "BPSYS", "BMI", "WMH_Volume", "ADI_NATRANK")], "ID", "MR_Date", "MR_Date")
tau.once_alt<-tau.once_alt[abs(as.numeric(tau.once_alt$MR_Date.x - tau.once_alt$MR_Date.y)/365) < 2,]

df.once<-MatchbyNearestDate(as.data.frame(df.once), Cardio[,c("ID", "MR_Date", "BPSYS", "BMI")], "ID", "MR_Date", "MR_Date")
colnames(df.once)[10]<-"MR_Date"
df.once<-merge(df.once, df.WMH[,c("ID", "MR_Date", "WMH_Volume")], by = c("ID", "MR_Date"), all.x = TRUE, all.y =  FALSE)
df.once<-merge(df.once, ADI, by = "ID", all.x = TRUE, all.y =  FALSE)
df.once<-df.once[abs(as.numeric(df.once$MR_Date - df.once$MR_Date.y)/365) < 2,]


MRI.once<-MatchbyNearestDate(as.data.frame(MRI.once), Cardio[,c("ID", "MR_Date", "BPSYS", "BMI")], "ID", "MR_Date", "MR_Date")
colnames(MRI.once)[9]<-"MR_Date"
MRI.once<-merge(MRI.once, df.WMH[,c("ID", "MR_Date", "WMH_Volume")], by = c("ID", "MR_Date"), all.x = TRUE, all.y =  FALSE)
MRI.once<-merge(MRI.once, ADI, by = "ID", all.x = TRUE, all.y =  FALSE)
MRI.once<-subset(MRI.once,!duplicated(MRI.once$ID))
MRI.once<-MRI.once[abs(as.numeric(MRI.once$MR_Date - MRI.once$MR_Date.y)/365) < 2,] #All measures taken within 2 years of each other


X<-Amyloid[,c("Age", "GENDER", "apoe4", "ADI_NATRANK", "BPSYS", "BMI", "WMH_Volume", "CDRgreaterthan0")]
Y<-Amyloid[,c("PUP_fSUVR_rsf_TOT_CORTMEAN")]
pred<-Amyloid[,"race2"]

data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
                     catref="male",binmed = c(3, 8), binref = c(0, 0), jointm=list(n=1,j1=c(4:7)),
                     predref="Caucasian",alpha=0.05,alpha2=0.05)
summary(data.b.b.2)
mma_A<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
          catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
          predref="Caucasian",alpha=0.05,alpha2=0.05,n=20,
          n2=500,seed=457,nonlinear=T)


summary(mma_A, alpha = 0.05)
CreateTableOne(vars = listVars, data = Amyloid, factorVars = catVars, strata = c("race2"))


X<-tau.once_alt[,c("Age", "GENDER", "apoe4", "ADI_NATRANK", "BPSYS", "BMI", "WMH_Volume")]
Y<-tau.once_alt[,c("Tauopathy")]
pred<-tau.once_alt[,"race2"]
levels(pred)<-c(levels(pred), "")
pred[is.na(pred)]<-""
data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
                     catref="male",binmed = 3, binref = 0, jointm=list(n=1,j1=c(4:7)),
                     predref="Caucasian",alpha=0.05,alpha2=0.05)


summary(data.b.b.2)
mma_T<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
           catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
           predref="Caucasian",alpha=0.05,alpha2=0.05,n=20,
           n2=500,seed=457,nonlinear=T)


summary(mma_T, alpha = 0.05)
CreateTableOne(vars = listVars, data = tau.once_alt, factorVars = catVars, strata = c("race2"))


X<-MRI.once[,c("Age", "GENDER", "apoe4", "ADI_NATRANK", "BPSYS", "BMI", "WMH_Volume", "CDRgreaterthan0")]
Y<-as.vector(MRI.once[,c("ADsig")])
pred<-MRI.once[,"race2"]



data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
                     catref="male",binmed = c(3, 8), binref = c(0, 0), jointm=list(n=1,j1=c(4:7)),
                     predref="White",alpha=0.05,alpha2=0.05)
summary(data.b.b.2)
mma_Vol<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
          catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
          predref="Caucasian",alpha=0.05,alpha2=0.05,n=20,
          n2=500,seed=457,nonlinear=T)


summary(mma_Vol, alpha = 0.1)
CreateTableOne(vars = listVars, data = MRI.once, factorVars = catVars, strata = c("race2"))


#AA's had a significantly lower ADsig volume than Caucasians. ~2/3rds of this effect was direct. 
#The remaining 1/3 was indirectly mediating by the combined effects of SES, WMH, BP sys, and BMI
#The indirect mediation effect was about half SES and half WMH. BMI and systolic BP have a negligible contribution

X<-df.once[,c("Age", "GENDER", "apoe4", "ADI_NATRANK", "BPSYS", "BMI", "WMH_Volume", "CDRgreaterthan0")]
Y<-df.once[,c("IntraSig")]
pred<-df.once[,"race2"]
levels(pred)<-c(levels(pred), "")
pred[is.na(pred)]<-""
data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
                     catref="male",binmed = c(3, 8), binref = c(0, 0), jointm=list(n=1,j1=c(4:7)),
                     predref="Caucasian" ,alpha=0.05,alpha2=0.05)
summary(data.b.b.2)
mma_FC<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
          catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
          predref="Caucasian" ,alpha=0.05,alpha2=0.05,n=20,
          n2=500,seed=457,nonlinear=T)

summary(mma_FC, alpha = 0.05)

CreateTableOne(vars = listVars, data = df.once, factorVars = catVars, strata = c("race2"))

#Plotting results of mediation analysis

data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'Systolic BP', 'BMI', 'WMH'),
  mean = c(-0.122, -0.079, -0.043, -0.022, 0.009, -0.027, 0.001),
  CI_low = c(-0.284, -0.185, -0.139, -0.089, -0.010, -0.086, -0.010),
  CI_high = c(-0.070, -0.013, -0.016, 0.011, 0.024, -0.011, 0.007))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'WMH', 'Systolic BP', 'BMI'))

p3<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + labs(title = "A. Race - Centiloid Mediation Analysis", subtitle = "Effect Sizes", caption = "95% CI Shown") +
  theme(legend.position = "none") + ylim(c(-0.4, 0.1))


data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'Systolic BP', 'BMI', 'WMH'),
  mean = c(-0.006, -0.006, 0.000, 0.004, 0.000, -0.004, 0.000),
  CI_low = c(-0.043, -0.031, -0.022, -0.008, -0.008, -0.018, -0.007),
  CI_high = c(0.029, 0.016, 0.024, 0.025, 0.005, 0.005, 0.008))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'WMH', 'Systolic BP', 'BMI'))

p4<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + 
  labs(title = "B. Race - Tauopathy Mediation Analysis", subtitle = "Effect Sizes", caption = "95% CI Shown") +
  theme(legend.position = "none") + ylim(c(-0.1, 0.03))


data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'Systolic BP', 'BMI', 'WMH', 'Sex'),
  mean = c(-0.233, -0.210, -0.023, -0.026, -0.002, 0.011, -0.007, -0.001),
  CI_low = c(-0.389, -0.347, -0.082, -0.070, -0.012, -0.005, -0.032, -0.009),
  CI_high = c(-0.141, -0.109, 0.007, 0.005, 0.005, 0.030, 0.006, 0.003))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'WMH', 'Systolic BP', 'BMI', 'Sex'))

p1<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + labs(title = "D. Race - AD Signature Volume Mediation Analysis", subtitle = "Effect Sizes", caption = "95% CI Shown") +
  theme(legend.position = "none") + ylim(c(-0.4, 0.1))


data.estimates = data.frame(
  var   = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'Systolic BP', 'BMI', 'WMH'),
  mean = c(-0.007, 0.004, -0.011, -0.009, 0, -0.002, 0.001),
  CI_low = c(-0.033, -0.010, -0.030, -0.025, -0.005, -0.009, -0.004),
  CI_high = c(0.010, 0.018, -0.001, -0.002, 0.003, 0.003, 0.007))
data.estimates$var <- factor(data.estimates$var, levels = c('Total Effect', 'Direct Effect', 'Indirect Effect', 'ADI', 'WMH', 'Systolic BP', 'BMI'))
p2<-ggplot(data.estimates, aes(var, mean)) + 
  geom_point(aes(x = var, y = mean, size = 7)) +
  geom_errorbar(aes(x = var, ymin = CI_low, ymax = CI_high), stat = "identity") + geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("") + ylab("Estimated Effect") + 
  labs(title = "C. Race - Intranetwork rsfc Signature Mediation Analysis", subtitle = "Effect Sizes", caption = "95% CI Shown") +
  theme(legend.position = "none") + ylim(c(-0.1, 0.025))

grid.arrange(p3, p4, p2,p1, nrow = 2)

####################################################
Amyloid<-MatchbyNearestDate(Amyloid, Cardio[,c("ID", "MR_Date", "BPSYS", "BMI")], "ID", "PET_Date", "MR_Date")
Amyloid<-MatchbyNearestDate(Amyloid, df.WMH[,c("ID", "MR_Date", "WMH_Volume")], "ID", "PET_Date", "MR_Date")
Amyloid<-merge(Amyloid, ADI, by = "ID", all.x = TRUE, all.y = FALSE)
X<-Amyloid[,c("Age", "GENDER", "apoe4", "EDUC", "BPSYS.x", "BMI.x", "WMH_Volume.x", "CDRgreaterthan0")]
Y<-Amyloid[,c("PUP_fSUVR_rsf_TOT_CORTMEAN")]
pred<-Amyloid[,"race2"]

data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
                     catref="male",binmed = c(3, 8), binref = c(0, 0), jointm=list(n=1,j1=c(4:7)),
                     predref="Caucasian",alpha=0.05,alpha2=0.05)
summary(data.b.b.2)
mma_A2<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
           catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
           predref="Caucasian",alpha=0.05,alpha2=0.05,n=20,
           n2=500,seed=457,nonlinear=T)


summary(mma_A2, alpha = 0.1)
CreateTableOne(vars = listVars, data = Amyloid, factorVars = catVars, strata = c("race2"))


# X<-tau.once_alt[,c("Age", "GENDER", "apoe4", "EDUC", "BPSYS", "BMI", "WMH_Volume")]
# Y<-tau.once_alt[,c("Tauopathy")]
# pred<-tau.once_alt[,"race2"]
# levels(pred)<-c(levels(pred), "")
# pred[is.na(pred)]<-""
# data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
#                      catref="male",binmed = 3, binref = 0, jointm=list(n=1,j1=c(4:7)),
#                      predref="Caucasian",alpha=0.05,alpha2=0.05)
# 
# 
# summary(data.b.b.2)
# mma_T2<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
#            catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
#            predref="Caucasian",alpha=0.05,alpha2=0.05,n=20,
#            n2=500,seed=457,nonlinear=T)
# 
# 
# summary(mma_T2, alpha = 0.05)
# CreateTableOne(vars = listVars, data = tau.once_alt, factorVars = catVars, strata = c("race2"))


X<-MRI.once[,c("Age", "GENDER", "apoe4", "EDUC", "BPSYS", "BMI", "WMH_Volume", "CDRgreaterthan0")]
Y<-as.vector(MRI.once[,c("ADsig")])
pred<-MRI.once[,"race2"]



data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
                     catref="male",binmed = c(3, 8), binref = c(0, 0), jointm=list(n=1,j1=c(4:7)),
                     predref="White",alpha=0.05,alpha2=0.05)
summary(data.b.b.2)
mma_Vol2<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
             catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
             predref="Caucasian",alpha=0.05,alpha2=0.05,n=20,
             n2=500,seed=457,nonlinear=T)


summary(mma_Vol2, alpha = 0.1)
CreateTableOne(vars = listVars, data = MRI.once, factorVars = catVars, strata = c("race2"))


#AA's had a significantly lower ADsig volume than Caucasians. ~2/3rds of this effect was direct. 
#The remaining 1/3 was indirectly mediating by the combined effects of SES, WMH, BP sys, and BMI
#The indirect mediation effect was about half SES and half WMH. BMI and systolic BP have a negligible contribution

X<-df.once[,c("Age", "GENDER", "apoe4", "EDUC", "BPSYS", "BMI", "WMH_Volume", "CDRgreaterthan0")]
Y<-df.once[,c("IntraSig")]
pred<-df.once[,"race2"]
levels(pred)<-c(levels(pred), "")
pred[is.na(pred)]<-""
data.b.b.2<-data.org(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
                     catref="male",binmed = c(3, 8), binref = c(0, 0), jointm=list(n=1,j1=c(4:7)),
                     predref="Caucasian" ,alpha=0.05,alpha2=0.05)
summary(data.b.b.2)
mma_FC2<-mma(X,Y,pred=pred,contmed=c(1, 4:7),catmed=c(2),
            catref="male",binmed = c(3), binref = 0, jointm=list(n=1,j1=c(4:7)),
            predref="Caucasian" ,alpha=0.05,alpha2=0.05,n=20,
            n2=500,seed=457,nonlinear=T)

summary(mma_FC2, alpha = 0.05)

CreateTableOne(vars = listVars, data = df.once, factorVars = catVars, strata = c("race2"))

#run with alpha = 0.1 to get 95% Ci, alpha = 0.2 to get 90% CI
summary(mma_A)
summary(mma_A, alpha = 0.2)
summary(mma_Vol, alpha = 0.2)
summary(mma_FC, alpha = 0.2)
summary(mma_A2, alpha = 0.2)

summary(mma_Vol2, alpha = 0.2)
summary(mma_FC2, alpha = 0.2)
