################################################
demogs<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR_demographics_20190122.csv")

ADI<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR_ADI_2019_08_01.csv")
ADI<-ADI[,c("id", "ADI_NATRANK")] #1079 individuals with ADI
colnames(ADI)[1]<-"ID"

demogs<-merge(demogs, ADI, by = "ID", all.x = TRUE, all.y = FALSE)
demogs<-demogs[,c("ID", "BIRTH", "GENDER", "EDUC", "apoe", "ADI_NATRANK", "HAND", "race2")]
colnames(demogs)[6]<-"SES"
################################################
#Getting MRI data
#df.MRI<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR14/HASD_ACS_DR14_3TMR.csv")
df.MRI<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_3TMR.csv")
df.MRI<-df.MRI[!(df.MRI$Project == "NP673" | df.MRI$Project == "NP801" | df.MRI$Project == "fACS"),] #Dropping people from inappropriate projects

colnames(df.MRI)[5]<-"ID" #Only for DF15
IDs<-as.character(df.MRI[,"ID"])
df.MRI<-df.MRI[df.MRI$FS_QC_Status == "Passed" | df.MRI$FS_QC_Status == "passed" | df.MRI$FS_QC_Status == "Passed with edits",]

ScaleandNormalize<-function(COLUMN, COLNAME){
  MEAN<-mean(df.MRI$MR_TOTV_INTRACRANIAL)
  model<-lm(COLUMN ~ MR_TOTV_INTRACRANIAL, data = df.MRI)
  df.MRI[,paste("Normalized", COLNAME, sep = "")]<-scale(COLUMN - (coef(model)[2] * (df.MRI$MR_TOTV_INTRACRANIAL - MEAN)))
  return(df.MRI)}

for(i in 16:202){
  df.MRI<-ScaleandNormalize(df.MRI[,i], names(df.MRI[i]))
}

df.MRI<-df.MRI[,c(5:6, 345:531)]
df.MRI$ADsig<-(df.MRI$NormalizedMR_LV_INFRTMP + df.MRI$NormalizedMR_LV_MIDTMP + df.MRI$NormalizedMR_LV_SUPERTMP+
  df.MRI$NormalizedMR_RV_INFRTMP + df.MRI$NormalizedMR_RV_MIDTMP + df.MRI$NormalizedMR_RV_SUPERTMP+
  df.MRI$NormalizedMR_LV_INFRPRTL+df.MRI$NormalizedMR_LV_SUPERPRTL +
  df.MRI$NormalizedMR_RV_INFRPRTL+df.MRI$NormalizedMR_RV_SUPERPRTL +
  df.MRI$NormalizedMR_LV_ENTORHINAL+df.MRI$NormalizedMR_RV_ENTORHINAL +
  df.MRI$NormalizedMR_LV_PRECUNEUS+df.MRI$NormalizedMR_RV_PRECUNEUS)/14
df.MRI$HippoVol<-(df.MRI$NormalizedMR_LV_HIPPOCAMPUS + df.MRI$NormalizedMR_RV_HIPPOCAMPUS )/2
df.MRI$MR_Date<-as.Date(df.MRI$MR_Date, format = "%m/%d/%Y")


df.MRI<-df.MRI[,c("ID", "MR_Date", "ADsig", "HippoVol")]
#Just looking at most recent MRI
demogs.mri<-demogs[demogs$ID %in% df.MRI$ID,]

df.MRI<-merge( demogs.mri, df.MRI,by = "ID")
df.MRI<-df.MRI[df.MRI$race2 == "Black" | df.MRI$race2 == "White",]
df.MRI$race2<-droplevels(df.MRI$race2)

df.MRI$BIRTH<-format(as.Date(df.MRI$BIRTH, "%d-%b-%Y"), "%d-%b-19%y")
df.MRI$BIRTH<-as.Date(df.MRI$BIRTH, format = "%d-%b-%Y")

#Need to look at CDR levels
df.CDR<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR_clinical_20190122.csv")
df.CDR<-df.CDR[,c("ID", "CDR", "TESTDATE")]
df.CDR$TESTDATE<-as.Date(df.CDR$TESTDATE, format = "%Y-%m-%d")
df.CDR$ID<-as.factor(df.CDR$ID)

df.MRI<-MatchbyNearestDate(df.MRI, df.CDR, "ID", "MR_Date", "TESTDATE")

df.MRI<-df.MRI[abs(as.numeric(df.MRI$TESTDATE - df.MRI$MR_Date)/365) < 2,] #making sure test occured within 2 years of MRI

df.MRI$CDRgreaterthan0<-as.factor(ifelse(df.MRI$CDR > 0, 1, 0))

df.MRI$apoe4<-as.factor(ifelse(df.MRI$apoe == 24 | df.MRI$apoe == 34 | df.MRI$apoe == 44, 1, 0))

df.MRI$race2<-droplevels(df.MRI$race2)
df.MRI$CDR<-as.factor(df.MRI$CDR)
df.MRI$Age<-as.numeric((df.MRI$MR_Date - df.MRI$BIRTH)/365)

MRI.once<-as.data.frame(df.MRI %>% 
  group_by(ID) %>%
  slice(which.max(MR_Date)))






rm( demogs.mri, df.CDR, CDR.once)

#Cardiovascular parameters
df.clin<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR_clinical_20190122.csv")
df.clin<-df.clin[,c("ID", "CDR", "TESTDATE", "BPSYS", "HEIGHT", "WEIGHT")]
#df.WMH<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR14/HASD_ACS_DR14_WMH.csv")
df.WMH<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_WMH.csv") #829 unique individuals with WMH
df.WMH<-df.WMH[!(df.WMH$Project == "NP673" | df.WMH$Project == "NP801" | df.WMH$Project == "fACS"),] #Dropping people from inappropriate projects


colnames(df.WMH)[5]<-"ID"
df.WMH$MR_Date<-as.Date(df.WMH$MR_Date, format = "%m/%d/%Y")
df.clin$TESTDATE<-as.Date(df.clin$TESTDATE, format = "%Y-%m-%d")

Cardio<-merge(df.MRI, df.clin, by = c("ID", "TESTDATE", "CDR")) 


Cardio$BMI<-Cardio$WEIGHT*703/(Cardio$HEIGHT*Cardio$HEIGHT) #1102 individuals with BMI, 1104 with BP


################
#Trying to combine cardio markers

Cardio2<-merge(Cardio, df.WMH[,c("ID", "MR_Date", "WMH_Volume")], by = c("ID", "MR_Date"), all = FALSE)
Cardio2<-Cardio2[complete.cases(Cardio2[,c("WMH_Volume", "BMI", "BPSYS")]),]
Cardio2<-merge(Cardio2, ADI, by = "ID", all = FALSE)



rm(df.clin)


