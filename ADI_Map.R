ADI<-read.csv("C:/Users/julie.wisch/Documents/ADRC/ADI.csv")
ADI<-ADI[,-1]
ADI<-ADI[ADI$X5DIGZIP > 60001 & ADI$X5DIGZIP < 65900,] #Dropping so we only have missouri and illinois
ADI<-ADI[!(ADI$ADI_STATERNK == "P"),]
ADI$ADI_NATRANK<-as.numeric(as.character(ADI$ADI_NATRANK))

ADI_Agg<-(aggregate(ADI[, c("X5DIGZIP", "ADI_NATRANK")], list(ADI$X5DIGZIP), mean))
ADI_Agg<-ADI_Agg[,-1]
colnames(ADI_Agg)<-c("zip", "ADI")

library(zipcode)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)#installed via github
#data

data(zipcode)
zipcode<-zipcode[zipcode$state == "MO",]
#size by zip
us<-map_data('state')
us<-us[us$region == "missouri",]

fm<-merge(zipcode, ADI_Agg, by = "zip", all = FALSE)

ggplot(fm,aes(longitude,latitude)) +
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(color = ADI),size=.25,alpha=.25)



library(choroplethr)
library(choroplethrZip)

data(df_pop_zip)

stlcounty = seq(from = 63101, to = 63182, by = 1)

zip_choropleth(df_pop_zip,
               state_zoom = "missouri",
               title       = "2012 Missouri Population Estimates",
               legend      = "Population")

df_pop_zip<-merge(df_pop_zip, ADI_Agg, by.x = "region", by.y = "zip", all = FALSE)
df_pop_zip<-df_pop_zip[,-2]
colnames(df_pop_zip)<-c("region", "value")
#A block group with a ranking of 1 indicates the lowest level of "disadvantage" within the nation 
#and an ADI with a ranking of 100 indicates the highest level of "disadvantage".

#https://www.neighborhoodatlas.medicine.wisc.edu/
zip_choropleth(df_pop_zip,
               state_zoom = "missouri",
               title       = "2013 Missouri ADI Estimates",
               legend      = "Area Deprivation Index")

zip_choropleth(df_pop_zip,
               county_zoom = 29189,
               title       = "St. Louis County",
               legend      = "Area Deprivation Index")


