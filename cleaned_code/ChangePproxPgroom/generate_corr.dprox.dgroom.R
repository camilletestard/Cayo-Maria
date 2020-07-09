#generate_corr.dprox.dgroom
#Are the individuals who spend more time in proximity also those that change their grooming 
#freq. the most?

#Load libraries
library(ggplot2)
library(ggpubr)
library(matlab)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")

ExSubScans = calcRandomScans(allScans)
unqIDs = as.character(unique(ExSubScans$focalID))

#Compute p(groom) & p(prox) for each individual:
dpProxGroom = data.frame(matrix(NA, nrow = length(unqIDs), ncol = 6)); colnames(dpProxGroom)=c("id","dprox","dgroom","group","sex"); count = 0;
for (id in 1:length(unqIDs)){
  
  id.all.pre.groom = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  id.all.post.groom = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]
  
  id.all.pre.prox = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #get all pre-hurricane data for that individual
  id.all.post.prox = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]#get all post-hurricane data for that individual
  
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  if (length(id.all.pre.groom)>20) {
    count = count+1
    dpProxGroom$id[count] = unqIDs[id]
    
    dpProxGroom$dgroom[count] = (sum(id.all.post.groom)/length(id.all.post.groom)) - (sum(id.all.pre.groom)/length(id.all.pre.groom)) #get proportion of observations this individual was seen grooming other partners PRE-hurricane
    dpProxGroom$dprox[count] = (sum(id.all.post.prox)/length(id.all.post.prox)) - (sum(id.all.pre.prox)/length(id.all.pre.prox))
    
    dpProxGroom$group[count] = as.character(group[1]); dpProxGroom$sex[count] = as.character(sex[1])
  }
}

#Plot change in prox vs. change in grooming
ggplot(dpProxGroom,aes(dprox, dgroom)) +
  geom_point(color='blue')+
  xlab("Change in Proximity")+ylab("Change in Grooming")+
  ggtitle("Correlation between change in proximity and change in grooming")+
  geom_smooth(method='lm', formula= y~x)

cor.test(dpProxGroom$dprox,dpProxGroom$dgroom) #compute correlation
