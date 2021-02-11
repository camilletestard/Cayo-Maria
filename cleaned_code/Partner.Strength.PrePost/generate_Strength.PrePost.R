# Investigate Change in strength of bond pre-to-post disaster (regardless of who the partners are)
# This script computes the distribution of grooming & proximity strength before and after the disaster 
# over multiple iterations (running through sub-samples). This script was meant to investigate wether individuals interact with 
# more partners but more weakly (i.e. spreading their grooming efforts.

library(stringr)
library(igraph)

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code") 
source("Functions/CalcSubsampledScans.R")
source("Functions/functions_GlobalNetworkMetrics.R")

num_iter = 500 #number of iterations
actions = c("groom", "prox"); a=1; iter=1
Networks = data.frame(); 
for (a in 1:length(actions)) { #for both proximity and grooming 
  
  for (iter in 1:num_iter){ #for all iterations
    
    print(paste("%%%%%%%%%%%%%%%%%%",paste(actions[a],iter), "%%%%%%%%%%%%%%%%%%"))
    
    # 1. Calculate random subsamples
    randomScans = calcRandomScans(allScans)
    
    # 2. For each group, each year and pre-/post-hurr separately, compute weighted edge list: 
    group = c("V","KK"); g=1; 
    for (g in 1:length(group)){ #For each group
      randscansG = randomScans[which(randomScans$group==group[g]),] #subselect scans of group G
      
      years = unique(randscansG$year); y=1
      for (y in 1:length(years)){ #For each year in that group
        randscansY = randscansG[which(randscansG$year==years[y]),] #subselect scans of group G and year Y
        year = years[y]
        
        isPost = c(0,1); h=1
        for (h in 1:length(isPost)){ #pre- and post-hurricane 
          
          rscans = randscansY[which(randscansY$isPost==isPost[h]),] #subselect scans of group G, year Y and hurricane status H
          numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
          
          #Find all unique IDs
          unqIDs = unique(as.character(rscans$focalID))
          
          # Output the Master Edgelist of all possible pairs given the unique IDs.
          if (actions[a] == "prox") {masterEL = calcMasterEL(unqIDs)}
          if (actions[a] == "groom") {masterEL = calcMasterEL_groom(unqIDs)}
          
          # Output weighted edgelist from the Master Edgelist.
          options(warn = -1) #set options to ignore all warnings
          if (actions[a] == "prox") {weightedEL = calcEdgeList(rscans,masterEL)}
          if (actions[a] == "groom") {weightedEL = calcEdgeList_groom(rscans,masterEL)
          weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
          #IMPORTANT NOTE: alter = giving ID and ego = receiving ego. Important for later.
          weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
          weightedEL = weightedEL[,c("alter","ego","count")]
          weightedEL$conc = paste(weightedEL$alter, weightedEL$ego, sep=".")
          }
          ### Only used to exclude individuals with too low #scans
          weightedEL$numscans.alter <-numscans$freq[match(weightedEL$alter, numscans$id)]
          weightedEL$numscans.ego <-numscans$freq[match(weightedEL$ego, numscans$id)]
          ###
          weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2 
          weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by avg #observations for each ID pair
          #IMPORTANT NOTE: i do NOT standardize weights per group and year, toehrwise i cannot see whether strength has icnreased or not
          weightedEL = weightedEL[which(weightedEL$weight != 0),]
          #IMPORTANT NOTE 2: i only keep nonzero weighted edges
          weightedEL$group = group[g]; weightedEL$year = years[y]; weightedEL$isPost = isPost[h]; 
          weightedEL$iter = iter; weightedEL$action=actions[a]
          
          Networks = rbind(Networks, weightedEL)# row bind
          
        } #end of isPost for loop
      } #end of Year for loop
    } #end of group for loop
  } #End of iteration loop
} #End of action loop

#Remove edges including individuals with too few scans 
idx_to_remove = which(Networks$numscans.alter<40 | Networks$numscans.ego<40)
Networks = Networks[-idx_to_remove,]

save(Networks, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/Networks_minObs.RData")
