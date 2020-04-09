
#####################################################################
# Compute Social Support Metrics, per individual, per year
#####################################################################

library(stringr)
library(igraph)

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
source("Social_Network_Analysis/CalcSubsampledScans.R")
source("Social_Network_Analysis/functions_SocialSupport.R")

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
group = c("HH","KK")
years = c(2016,2018)
groupyears = c("HH2016", "KK2018")

SocialCapital.ALL = data.frame()

for (i in 1:length(groupyears)){
  
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  groom_data = read.csv(paste("Group",groupyears[i],"_GroomingEvents.txt", sep = ""))
  agg_data = read.csv(paste("Group",groupyears[i],"_AgonsiticActions.txt", sep = ""))
  focal_data = read.csv(paste("Group",groupyears[i],"_FocalData.txt", sep = ""))
  meta_data = read.csv(paste("Group",groupyears[i],"_GroupByYear.txt", sep = ""))
  prox_data = read.csv(paste("Group",groupyears[i],"_ProximityGroups.txt", sep = ""))
  
  #Create Social Capital Data frame & add Sex, Age and Rank
  SocialCapitalData= meta_data[,c("id","sex","age","ordinal.rank","percofsex.dominanted")]
  names(SocialCapitalData)=c("id","sex","age","ordrank","percentrank")
  SocialCapitalData$groupyear = groupyears[i]
  
  #####################################################################
  ## For GROOMING DATA

  #####################################################################
  ## Strength connection to top 3 partners - proximity
  
  rscans = prox_data
  # 2. Find the number of unique IDs.
  #Find all unique IDs
  unqIDs = unique(as.character(rscans$focal.monkey))
  
  # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL_SS(unqIDs)
  
  # 4. Output weighted edgelist from the Master Edgelist.
  options(warn = -1) #set options to ignore all warnings
  action = "prox"; proxdata =1
  weightedEL = calcEdgeList_SS(rscans, masterEL, action, proxdata)
  hrs = meta_data$hrs.focalfollowed[match(weightedEL$alter, meta_data$id)]
  weightedEL$weight <- round(weightedEL$count / hrs, 5) #add weight information by dividing by the #hrs spent observing
  weightedEL$count <- NULL;weightedEL$conc <- NULL #delete those calumn variables
  
  DSIprox = data.frame(matrix(data = NA, nrow = nrow(weightedEL), ncol = 6)); colnames(DSIprox) = c("focalID", "partnerID", "OUT", "IN", "DSI", "Equal")
  DSIprox[, c("focalID", "partnerID", "OUT")] = weightedEL[, c("alter", "ego","weight")]
  count = 0;
  for (ii in 1:nrow(DSIprox)){
    if (length(weightedEL$weight[which(as.character(weightedEL$alter) == as.character(DSIprox[ii,"partnerID"]) 
                                       & as.character(weightedEL$ego) == as.character(DSIprox[ii,"focalID"]))]) ==0)
    { DSIprox[ii, "IN"] = 0 ; count = count + 1}
    else { DSIprox[ii, "IN"] = weightedEL$weight[which(weightedEL$alter == as.character(DSIprox[ii,"partnerID"]) 
                                                       & weightedEL$ego == as.character(DSIprox[ii,"focalID"]))]}
  }
  DSIprox$DSI = (DSIprox$IN + DSIprox$OUT);
  
  #Find Strength of connection to top 3 partners
  strengthTop3 = data.frame(matrix(data = NA, nrow = length(unqIDs), ncol = 2)); names(strengthTop3) = c("id","strength")
  for (id in 1:length(unqIDs)){
    strengthTop3[id,"id"] = unqIDs[id]
    sortedDSI = sort(DSIprox$DSI[which(DSIprox$focalID == unqIDs[id])], decreasing = T)
    strengthTop3[id, "strength"] = sum(sortedDSI[1:3])
  }
  mean_strength = mean(strengthTop3$strength)
  strengthTop3$stdStrength = strengthTop3$strength/mean_strength
  
  SocialCapitalData$StrengthTop3 = strengthTop3$strength[match(meta_data$id,strengthTop3$id)]

  #####################################################################
  ## Strength connection to top 3 partners - grooming
  
}