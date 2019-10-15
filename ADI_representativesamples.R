library(tidyverse)
library(tidycensus)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

##############Functions for this analysis
get_state_demographic_data <- function(the_state, the_year) {
  
  options(tigris_use_cache = TRUE) #keep chached version of data
  
  # get all counties in given state
  counties <- tidycensus::fips_codes %>%
    dplyr::filter(state == the_state)
  
  # loop over counties and get tracts for each county
  purrr::map(counties$county_code, 
             ~ get_acs(
               geography = 'tract',
               table = c("B02001"),
               state = the_state,
               county = .x,
               year = the_year,
               survey = 'acs5',
               geometry = TRUE,
               cache_table = TRUE
             )
  ) %>% 
    purrr::reduce(rbind)  # bind rows of all counties
}

Get_SD<-function(RACE){
  mean_black<-sum(vt[vt$variable == RACE, "Product"], na.rm = TRUE)/sum(vt[vt$variable == RACE, "estimate"], na.rm = TRUE)
  Black<-vt[vt$variable == RACE, ]
  Black$sdcalc<-(Black$ADI_NATRANK - mean_black)^2*Black$estimate
  Black<-Black[is.na(Black$sdcalc) == FALSE,]
  SDresult<-sqrt((sum(Black$sdcalc))/sum(Black$estimate))
  return(SDresult)}

explode <- function(x, weight_var) {
  w <- rlang::eval_tidy(rlang::enquo(weight_var), x)
  x[rep(seq_len(nrow(x)), w), , drop = FALSE]
}
#########################################
#api key hidden for github
census_api_key("XXX", overwrite = FALSE, install = TRUE)

v17 <- load_variables(2015, "acs5", cache = TRUE)
v17_types<-as.data.frame(table(v17$concept))
v17[362:371,]
v17_types[c(127, 128, 336, 781, 991),]
v17[v17$concept == "EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER",]


df2 <- get_state_demographic_data('MO', 2017)
df2<-df2[df2$variable == "B02001_001" | df2$variable == "B02001_002" | df2$variable == "B02001_003",]


vt2<-as.data.frame(df2)
vt2$GEOID <- as.numeric(vt2$GEOID) 
vt2$GEOID<-as.character(vt2$GEOID)

vt1<-read.csv("C:/Users/julie.wisch/Documents/ADRC/Mo_ADI_blockgroup.csv")

vt1$fips<-str_sub(vt1$fips, 1, str_length(vt1$fips)-1)
vt1$adi_natrank<-as.numeric(as.character(vt1$adi_natrank))
vt1$adi_staternk<-as.numeric(as.character(vt1$adi_staternk))
vt1b<-aggregate(vt1[, 3:4], list(vt1$fips), mean, na.rm = TRUE)


vt<-merge(vt2, vt1b, by.x = "GEOID", by.y = "Group.1", all = FALSE)
colnames(vt)[7]<-"ADI_NATRANK"
vt<-vt[,c("GEOID", "variable", "estimate", "moe", "ADI_NATRANK")]
vt<-vt[!duplicated(vt),]

vt$Product<-vt$estimate * vt$ADI_NATRANK 
sum(vt$Product, na.rm = TRUE) / sum(vt$estimate, na.rm = TRUE)


vt_combo<-aggregate(estimate ~ variable, data=vt, FUN=sum) #number of people per group
vt_combo2<-aggregate(Product ~ variable, data = vt, FUN = sum) #Average ADI per group

vt_combo2$Product/vt_combo$estimate #Total ADI, White ADI, Black ADI


mean_black<-vt_combo2[vt_combo2$variable == "B02001_003", "Product"]/vt_combo[vt_combo$variable == "B02001_003", "estimate"]
mean_white<-vt_combo2[vt_combo2$variable == "B02001_002", "Product"]/vt_combo[vt_combo$variable == "B02001_002", "estimate"]
mean_total<-vt_combo2[vt_combo2$variable == "B02001_001", "Product"]/vt_combo[vt_combo$variable == "B02001_001", "estimate"]




sd_black<-Get_SD("B02001_003")
sd_white<-Get_SD("B02001_002")
sd_total<-Get_SD("B02001_001")

data.estimates<-data.frame(
  var = c("All", "African-American", "Caucasian"),
  mean = c(mean_total, mean_black, mean_white),
  sd = c(sd_total, sd_black, sd_white))

data.estimates$CIlow[1]<-data.estimates$mean[1] - data.estimates$sd[1]/sqrt(length(vt[vt$variable == "B02001_001", "Product"]))
data.estimates$CIlow[2]<-data.estimates$mean[2] - data.estimates$sd[2]/sqrt(length(vt[vt$variable == "B02001_003", "Product"]))
data.estimates$CIlow[3]<-data.estimates$mean[3] - data.estimates$sd[3]/sqrt(length(vt[vt$variable == "B02001_002", "Product"]))
data.estimates$CIhigh[1]<-data.estimates$mean[1] + data.estimates$sd[1]/sqrt(length(vt[vt$variable == "B02001_001", "Product"]))
data.estimates$CIhigh[2]<-data.estimates$mean[2] + data.estimates$sd[2]/sqrt(length(vt[vt$variable == "B02001_003", "Product"]))
data.estimates$CIhigh[3]<-data.estimates$mean[3] + data.estimates$sd[3]/sqrt(length(vt[vt$variable == "B02001_002", "Product"]))


ggplot(data.estimates, aes(var, mean)) + geom_point(size = 7) + geom_errorbar(aes(x = var, ymin = CIlow, ymax = CIhigh),
                                                                              stat = "identity") + ylab("ADI\nHigher indicates greater deprivation")+
  xlab("Demographic Group")


Black<-vt[vt$variable == "B02001_003",]
White<-vt[vt$variable == "B02001_002",]

Blackdf<-as.data.frame(matrix(0, ncol = 2, nrow = sum(Black$estimate)))
Whitedf<-as.data.frame(matrix(0, ncol = 2, nrow = sum(White$estimate)))

Blackdf<-explode(Black, estimate)
Whitedf<-explode(White, estimate)



wilcox.test(Blackdf$ADI_NATRANK, Whitedf$ADI_NATRANK) #p < 10^-16

ADRC<-read.csv("C:/Users/julie.wisch/Documents/ADRC/ADRC_with_ADI.csv")

wilcox.test(Blackdf$ADI_NATRANK, ADRC[ADRC$race2 == "Black", "SES"]) #p = 0.00577

wilcox.test(Whitedf$ADI_NATRANK, ADRC[ADRC$race2 == "White", "SES"]) #p < 2.2e-16


library(effsize)
#Norman Cliff (1996). Ordinal methods for behavioral data analysis. Routledge.
#For Cliff's Delta absolute value you have a small effect size  around .147, a medium effect size around .33, and a large effect size around .474.
library(coin)
# wilcoxsign_test(Blackdf$ADI_NATRANK ~ Whitedf$ADI_NATRANK, distribution="exact")
# wilcoxsign_test(Blackdf$V1 ~ Whitedf$V1, distribution="exact")


cliff.delta(Blackdf$ADI_NATRANK, Whitedf$ADI_NATRANK)
#Delta effect size = 0.4577 (95% CI 0.456, 0.459)

cliff.delta(Blackdf$ADI_NATRANK, ADRC[ADRC$race2 == "Black", "SES"])
#delta = 0.0849, 95% CI -0.0198, 0.188

cliff.delta(Whitedf$ADI_NATRANK, ADRC[ADRC$race2 == "White", "SES"])
#delta = 0.395, 95% CI 0.3599, -.4284

data.estimates2<-data.frame("var" = c("African-American, ADRC", "Caucasian, ADRC"), 
                            "mean" = c(mean(ADRC[ADRC$race2 == "Black", "SES"], na.rm = TRUE), mean(ADRC[ADRC$race2 == "White", "SES"], na.rm = TRUE)),
                            "sd" = c(sd(ADRC[ADRC$race2 == "Black", "SES"], na.rm = TRUE), sd(ADRC[ADRC$race2 == "White", "SES"], na.rm = TRUE)))

data.estimates2$CIlow[1]<-data.estimates2$mean[1] - data.estimates2$sd[1]/sqrt(length(ADRC[ADRC$race2 == "Black", "SES"]))
data.estimates2$CIlow[2]<-data.estimates2$mean[2] - data.estimates2$sd[2]/sqrt(length(ADRC[ADRC$race2 == "White", "SES"]))
data.estimates2$CIhigh[1]<-data.estimates2$mean[1] + data.estimates2$sd[1]/sqrt(length(ADRC[ADRC$race2 == "Black", "SES"]))
data.estimates2$CIhigh[2]<-data.estimates2$mean[2] + data.estimates2$sd[2]/sqrt(length(ADRC[ADRC$race2 == "White", "SES"]))

data.estimates<-data.frame(rbind(data.estimates, data.estimates2))
rm(data.estimates2)

data.estimates$race<-c("All", "African-American", "Caucasian", "African-American", "Caucasian")
data.estimates$cohort<-c("State Population", "State Population", "State Population", "ADRC Sample", "ADRC Sample")

p1<-ggplot(data.estimates[2:5,]) +
  geom_bar( aes(x=cohort, y=mean, fill = race), stat="identity", position = "dodge",  alpha=0.7) +
  xlab("Race") + ylab("Area Deprivation Index (ADI)\nLower values indicate higher socioeconomic status") + 
  theme(legend.position = "bottom") + labs(fill = "Race")
  #geom_errorbar( aes(x=race, ymin=CIlow, ymax=CIhigh, group = cohort), stat="identity",position = "dodge")

 vt$variable<-as.factor(vt$variable)
 Blackdf$race<-"African-American"
 Whitedf$race<-"Caucasian"
 plotdf<-rbind(Blackdf[,c("race", "ADI_NATRANK")], Whitedf[,c("race", "ADI_NATRANK")])


     
p2<-ggplot(plotdf, aes(ADI_NATRANK)) + 
     geom_histogram(data = plotdf[plotdf$race == "African-American",],aes(fill=race), alpha = 0.4) + 
  geom_histogram(data = plotdf[plotdf$race == "Caucasian",],aes(fill=race), alpha = 0.4) + # change binwidth
       labs(title="Area Deprivation Index (ADI)", 
            subtitle="State of Missouri Population")  +
  xlab("ADI") + ylab("") + theme(legend.position = "none",   axis.text.y = element_blank(),
                                 axis.ticks = element_blank())

p3<-ggplot(ADRC, aes(SES)) + 
  geom_histogram(data = ADRC[ADRC$race2 == "Black",], aes(fill=race2), alpha = 0.4) +  # change binwidth
  geom_histogram(data = ADRC[ADRC$race2 == "White",], aes(fill=race2), alpha = 0.4) +
  labs(title="Area Deprivation Index (ADI)", 
       subtitle="ADRC Sample")  +
  xlab("ADI") + ylab("") + theme(legend.position = "none",   axis.text.y = element_blank(),
                                 axis.ticks = element_blank())

lay<-rbind(c(1, 2), c(1, 3))

grid.arrange(p1, p2, p3, layout_matrix = lay)
