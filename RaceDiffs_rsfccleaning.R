df_FC<-read.csv("W:/ances/julie/Data/ADRC/Aggregated/Ances_compositeBB298.csv")
colnames(df_FC)[1]<-"ID"
df_FC$DATE_SCANNED<-as.Date(df_FC$DATE_SCANNED, format = "%m/%d/%Y")
df_FC<-MatchbyNearestDate(df_FC, MRI.once,  "ID", "DATE_SCANNED", "MR_Date")
df_FC<-df_FC[abs(as.numeric(df_FC$DATE_SCANNED - df_FC$MR_Date)/365) < 2,]


df_FC<-df_FC[with(df_FC, order(ID, DATE_SCANNED)), ]

df.once<-df_FC %>% 
  group_by(ID) %>%
  slice(which.max(DATE_SCANNED))


var_coord_func <- function(loadings, comp.sdev){loadings*comp.sdev}
contrib <- function(var.cos2, comp.cos2){var.cos2/comp.cos2}

MakeBarPlots<-function(Component, ComponentTitle, SubTitle){
  theme_set(theme_bw())  
  pcaPlot<-as.data.frame(Component)
  pcaPlot<-cbind(rownames(pcaPlot), pcaPlot[,1])
  colnames(pcaPlot)<-c("region", "contribution")
  pcaPlot<-as.data.frame(pcaPlot)
  pcaPlot$contribution<-as.numeric(as.character(pcaPlot$contribution))
  pcaPlot <- pcaPlot[order(-pcaPlot$contribution), ]  # sort
  # Diverging Barcharts
  p<-ggplot(pcaPlot, aes(x=reorder(region, contribution), y = contribution, label=contribution)) + 
    geom_bar(stat='identity', width=.5)  +
    #coord_cartesian(ylim = c(-0.4, 0.4)) +
    scale_y_continuous()+ ylab("Contribution")+xlab("Intranetwork Connection")+
    labs(subtitle=SubTitle, 
         title= ComponentTitle) + 
    coord_flip() + 
    theme(legend.position = "none")
  return(p)}



df_pca <- prcomp(df.once[,c(3, 16, 28, 39, 49, 58, 66, 73, 79, 84, 88, 91, 93)], scale = TRUE, rotate = "varimax")
#Intranetwork - 28%
loadings <- df_pca$rotation
sdev <- df_pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
var.cos2 <- var.coord^2
# Compute contributions of each brain region to the component
#result is between 0 and 1...should sum to 1
comp.cos2 <- apply(var.cos2, 2, sum)
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))

df.once$IntraSig<-as.numeric(as.matrix(df.once[,c(3, 16, 28, 39, 49, 58, 66, 73, 79, 84, 88, 91, 93)])%*%df_pca$rotation[,1])

rm(df,loadings, sdev, var.coord, var.cos2, comp.cos2)

df.once<-df.once[df.once$race2 == "Black" | df.once$race2 == "White",]
df.once$race2<-revalue(df.once$race2, c("Black" = "African-American", "White" = "Caucasian"))
df.once<-df.once[,c(1:2, 94:109)]

rm( var.coord, var.cos2, model, model.null,  loadings,  comp.cos2, sdev)

