#Doing Amyloid - first PIB, then AV45
#PIB<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR14/HASD_ACS_DR14_PIB.csv")
PIB<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_PIB.csv")
PIB<-PIB[!(PIB$Project == "NP673" | PIB$Project == "NP801" | PIB$Project == "fACS"),] #Dropping people from inappropriate projects

colnames(PIB)[5]<-"ID"
demogs.pib<-demogs[demogs$ID %in% PIB$ID,]

PIB<-merge( demogs.pib, PIB,by = "ID")
PIB$PET_Date<-as.Date(PIB$PET_Date, format = "%m/%d/%Y")

#Just using most recent visit for each participant



PIB<-PIB[PIB$race2 == "Black" | PIB$race2 == "White",]

PIB<-MatchbyNearestDate(PIB[,c("ID", "PET_Date", "PIB_fSUVR_rsf_TOT_CORTMEAN")], df.MRI, "ID", "PET_Date", "MR_Date")
PIB<-PIB[abs(as.numeric(PIB$PET_Date - PIB$MR_Date)/365) < 2,]

PIB.once<-as.data.frame(PIB %>% 
  group_by(ID) %>%
  slice(which.max(PET_Date)))
PIB.once<-PIB.once[!is.na(PIB.once$CDRgreaterthan0),]
PIB.once<-PIB.once[!is.na(PIB.once$race2),]

################################################
#Repeat for AV45.  
#AV45<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR14/HASD_ACS_DR14_AV45.csv")
AV45<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_AV45.csv")
AV45<-AV45[!(AV45$Project == "NP673" | AV45$Project == "NP801" | AV45$Project == "fACS"),] #Dropping people from inappropriate projects

colnames(AV45)[5]<-"ID"
AV45$PET_Date<-as.Date(AV45$PET_Date, format = "%m/%d/%Y")
AV45<-AV45[,c("ID", "PET_Date", "AV45_fSUVR_rsf_TOT_CORTMEAN")]
AV45<-AV45[!is.na(AV45$AV45_fSUVR_rsf_TOT_CORTMEAN),]
AV45<-MatchbyNearestDate(AV45[,c("ID", "PET_Date", "AV45_fSUVR_rsf_TOT_CORTMEAN")], df.MRI, "ID", "PET_Date", "MR_Date")
AV45<-AV45[abs(as.numeric(AV45$PET_Date - AV45$MR_Date)/365) < 2,]




AV45<-AV45[with(AV45, order(ID, PET_Date)), ]


AV45.once<-as.data.frame(AV45 %>% 
  group_by(ID) %>%
  slice(which.max(PET_Date)))


AV45.once<-AV45.once[AV45.once$race2 == "Black" | AV45.once$race2 == "White",]
AV45.once$race2<-droplevels(AV45.once$race2)
AV45.once<-AV45.once[complete.cases(AV45.once),]


#########################################################################################
#Combining AV45 and PIB into centiloids
#PiBCentiloidSUVRRSF  =  45*PIB-47.5
#AV45CentiloidSUVRrsf = 53.6*AV45-43.2
#Amyloid Centiloid cutoffs....PIB = 16.4, AV45 = 20.6

PIB.once$Centiloid<-45*PIB.once$PIB_fSUVR_rsf_TOT_CORTMEAN - 47.5
AV45.once$Centiloid<-53.6*AV45.once$AV45_fSUVR_rsf_TOT_CORTMEAN -43.2
PIB.once$Tracer<-"PIB"
AV45.once$Tracer<-"AV45"
colnames(PIB.once)[3]<-"PUP_fSUVR_rsf_TOT_CORTMEAN"
colnames(AV45.once)[3]<-"PUP_fSUVR_rsf_TOT_CORTMEAN"
Amyloid<-data.frame(rbind(PIB.once, AV45.once))
#Then have to cut it down so there's no overlap between AV45 and PIB
#Keeping PIB scan if people have two scans
Amyloid<-Amyloid[!duplicated(Amyloid[,c('ID')]),]
levels(Amyloid$race2)<-c("African American", "Caucasian")

rm( demogs.pib, PIB, AV45, AV45.once)
