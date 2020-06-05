#####################################################################
# Compute Social Support Metrics, per individual, per year
#based on scan data to have comparable measures to post-hurricane data
#####################################################################
library(stringr)
library(igraph)

#Load data and source local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt"); allScans$groupyear = paste(allScans$group,allScans$year,sep="")
source("Social_Network_Analysis/CalcSubsampledScans.R")
source("Social_Network_Analysis/functions_SocialSupport.R")

#List of IDs for Marina
# listID = read.csv("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Marina_data/CayoPaxgeneIDRank.txt", sep = "\t")

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
group = c("R","R","F", "F", "F", "F", "V", "V", "V","V","V", "HH", "HH", "KK","KK", "KK","S")
years = c(2015, 2016, 2014, 2015, 2016, 2017, 2015, 2016, 2017, 2018,2019, 2014, 2016, 2015, 2017,2018,2019)
groupyears = c("R2015","R2016","F2014","F2015", "F2016", "F2017", "V2015", "V2016", "V2017", "V2018", "V2019", "HH2014", "HH2016","KK2015", "KK2017", "KK2018", "S2019")
# group = c("F", "F","F", "V", "V", "V", "KK","KK")
# years = c(2015, 2016, 2017, 2015, 2016, 2017, 2015, 2017)
# groupyears = c("F2015", "F2016", "F2017", "V2015", "V2016", "V2017", "KK2015", "KK2017")
SocialSupport.ALL = data.frame()
gy=1
for (gy in 1:length(groupyears)){
  
  scans = allScans[which(allScans$groupyear == groupyears[gy]),]
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_file=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
  unqIDs = as.character(meta_file$id)
  
  #Create Social Support Data frame & add Sex, Age and Rank
  SocialSupportData= scans[match(unqIDs,scans$focalID),c("focalID","group","year","sex","age","ordrank","percentrank")]
  names(SocialSupportData)[1]="id"
  SocialSupportData$groupyear = groupyears[gy]
  
  #####################################################################
  ## For GROOMING DATA
  numscans = as.data.frame(table(as.character(scans$focalID))); names(numscans) =c("id","freq")
  
  #Output the Master Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL_groom(unqIDs)
  masterEL$focalID <-  masterEL$givingID; masterEL$partnerID <- masterEL$receivingID #set focal ID and partner ID
  
  # Output weighted edgelist from the Master Edgelist.
  options(warn = -1) #set options to ignore all warnings
  weightedEL = calcEdgeList_groom(scans,masterEL)
  weightedEL$numscans <- (numscans$freq[match(weightedEL$givingID, numscans$id)] + numscans$freq[match(weightedEL$receivingID, numscans$id)])/2
  weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 3) #add weight information by dividing by the #scans for each individual
  meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
  weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights 
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$numscans <-NULL #delete those cOlumn variables
  
  DSI = data.frame(matrix(data = NA, nrow = nrow(weightedEL), ncol = 6)); colnames(DSI) = c("focalID", "partnerID", "GroomIN", "GroomOUT", "DSI", "Equal")
  DSI[, c("focalID", "partnerID")] = weightedEL[, c("focalID", "partnerID")]
  
  #Sanity checks:
  # numGroomEvents = length(which(!is.na(scans$partner.ID)))
  # length(which(scans$isSocial == 1)); length(which(scans$isSocialGive == 1)); length(which(scans$isSocialGet == 1))
  
  for (ii in 1:nrow(DSI)){
    DSI[ii, "GroomOUT"] = sum(weightedEL$weight[which(weightedEL$givingID == as.character(DSI[ii,"focalID"]) & weightedEL$receivingID == as.character(DSI[ii,"partnerID"]))])
    DSI[ii, "GroomIN"] = sum(weightedEL$weight[which(weightedEL$givingID == as.character(DSI[ii,"partnerID"]) & weightedEL$receivingID == as.character(DSI[ii,"focalID"]))])
  }
  DSI$DSI = (DSI$GroomIN + DSI$GroomOUT); DSI$Equal = (DSI$GroomIN/DSI$GroomOUT)
  
  Groom = as.data.frame(matrix(NA, nrow = length(unqIDs), ncol = 3)); names(Groom)=c("id","numPartners","strength")
  for (id in 1:length(unqIDs)){
    Groom[id,"id"] = unqIDs[id]
    Groom[id,"numPartners"] = length(which(DSI$DSI[which(DSI$focalID == unqIDs[id])]!=0))
    Groom[id, "strength"] = sum(DSI$DSI[which(DSI$focalID == unqIDs[id])])
  }
  mean_partners = mean(Groom$numPartners,na.rm=T); mean_strength = mean(Groom$strength,na.rm=T)
  Groom$stdPartners = Groom$numPartners/mean_partners; Groom$stdStrength = Groom$strength/mean_strength
  SocialSupportData[,c("numPartners_groom","strength_groom", "std_numPartners_groom","std_strength_groom")] = Groom[match(SocialSupportData$id,Groom$id), c("numPartners","strength","stdPartners","stdStrength")]
  
  #Find Strength of connection to top 3 partners
  strengthTop3 = data.frame(matrix(data = NA, nrow = length(unqIDs), ncol = 2)); names(strengthTop3) = c("id","strength")
  for (id in 1:length(unqIDs)){
    strengthTop3[id,"id"] = unqIDs[id]
    sortedDSI = sort(DSI$DSI[which(DSI$focalID == unqIDs[id])], decreasing = T)
    strengthTop3[id, "strength"] = sum(sortedDSI[1:3])
  }
  mean_strength = mean(strengthTop3$strength,na.rm=T)
  strengthTop3$stdStrength = strengthTop3$strength/mean_strength
  
  SocialSupportData$StrengthTop3_groom = strengthTop3$strength[match(SocialSupportData$id,strengthTop3$id)]
  SocialSupportData$StrengthTop3_groom_std = strengthTop3$stdStrength[match(SocialSupportData$id,strengthTop3$id)]
  
  #####################################################################
  ## For PROXIMITY DATA
  
  numscans = as.data.frame(table(as.character(scans$focalID))); names(numscans) =c("id","freq")
  
  #Find all unique IDs
  unqIDs = unique(as.character(scans$focalID))
  
  #Output the Master Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL(unqIDs)
  
  # Output weighted edgelist from the Master Edgelist.
  options(warn = -1) #set options to ignore all warnings
  weightedEL = calcEdgeList(scans,masterEL)
  weightedEL$numscans <- numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)]
  weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 3) #add weight information by dividing by the #scans for each individual
  meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
  weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights 
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$numscans <-NULL #delete those cOlumn variables
  
  DSI = data.frame(matrix(data = NA, nrow = nrow(weightedEL), ncol = 6)); colnames(DSI) = c("focalID", "partnerID", "DSIprox")
  DSI[, c("focalID", "partnerID", "DSIprox")] = weightedEL[, c("ego", "alter","weight")]
  
  Prox = as.data.frame(matrix(NA, nrow = length(unqIDs), ncol = 3)); names(Prox)=c("id","numPartners","strength")
  for (id in 1:length(unqIDs)){
    Prox[id,"id"] = unqIDs[id]
    Prox[id,"numPartners"] = length(which(DSI$DSI[which(DSI$focalID == unqIDs[id])]!=0)) + length(which(DSI$DSI[which(DSI$partnerID == unqIDs[id])]!=0))
    Prox[id, "strength"] = sum(DSI$DSI[which(DSI$focalID == unqIDs[id])]) + sum(DSI$DSI[which(DSI$partnerID == unqIDs[id])])
  }
  mean_partners = mean(Prox$numPartners,na.rm=T); mean_strength = mean(Prox$strength,na.rm=T)
  Prox$stdPartners = Prox$numPartners/mean_partners; Prox$stdStrength = Prox$strength/mean_strength
  SocialSupportData[,c("numPartners_prox","strength_prox", "std_numPartners_prox","std_strength_prox")] = Prox[match(SocialSupportData$id,Prox$id), c("numPartners","strength","stdPartners","stdStrength")]
  
  #Find Strength of connection to top 3 partners
  strengthTop3 = data.frame(matrix(data = NA, nrow = length(unqIDs), ncol = 2)); names(strengthTop3) = c("id","strength")
  for (id in 1:length(unqIDs)){
    strengthTop3[id,"id"] = unqIDs[id]
    sortedDSI = sort(DSI$DSI[which(DSI$focalID == unqIDs[id])], decreasing = T)
    strengthTop3[id, "strength"] = sum(sortedDSI[1:3])
  }
  mean_strength = mean(strengthTop3$strength,na.rm=T)
  strengthTop3$stdStrength = strengthTop3$strength/mean_strength
  
  SocialSupportData$StrengthTop3_prox = strengthTop3$strength[match(SocialSupportData$id,strengthTop3$id)]
  SocialSupportData$StrengthTop3_prox_std = strengthTop3$stdStrength[match(SocialSupportData$id,strengthTop3$id)]

  SocialSupport.ALL = rbind(SocialSupport.ALL, SocialSupportData)
  rm(SocialSupportData)
  
}
save(SocialSupport.ALL,file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/SocialSupport.RData")

