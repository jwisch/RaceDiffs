#tau<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/DR14/HASD_ACS_DR14_TAU.csv")
tau<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/HASD_ACS_DR15_TAU.csv")
colnames(tau)[5]<-"ID"
demogs.tau<-demogs[demogs$ID %in% tau$ID,]


tau<-merge( demogs.tau, tau,by = "ID")
tau$PET_Date<-as.Date(tau$PET_Date, format = "%m/%d/%Y")

tau<-MatchbyNearestDate(tau[,c("ID", "PET_Date", "Tauopathy")], df.MRI, "ID", "PET_Date", "MR_Date")
tau<-tau[abs(as.numeric(tau$PET_Date - tau$MR_Date)/365) < 2,]


######Doing rate of change calculations
tau.once<-as.data.frame(tau %>% 
  group_by(ID) %>%
  slice(which.max(PET_Date)))


tau.once<-merge(tau.once[,c("ID", "PET_Date", "Tauopathy")], MRI.once, by = "ID", all.x = TRUE, all.y = FALSE)

tau.once<-tau.once[tau.once$race2 == "Black" | tau.once$race2 == "White",]
tau.once$race2<-droplevels(tau.once$race2)

tau.once$race2<-revalue(tau.once$race2, c("Black" = "African-American", "White" = "Caucasian"))

tau.once<-tau.once[complete.cases(tau.once),]

rm(tau, demogs.tau)

