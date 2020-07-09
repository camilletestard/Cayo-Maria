# Investigate Change in strength of bond pre-to-post disaster (regardless of who the partners are)
# This script computes the distribution of grooming & proximity strength before and after the disaster 
# over multiple iterations (running through sub-samples). This script was meant to investigate wether individuals interact with 
# more partners but more weakly (i.e. spreading their grooming efforts.

library(stringr)
library(igraph)

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code") 
source("Functions/CalcSubsampledScans.R")
source("Functions/functions_GlobalNetworkMetrics.R")

#Initilize data frames
population.strength.All=data.frame(); ID.strength.data.All=data.frame();

num_iter = 250 #number of iterations
actions = c("groom", "prox"); a=1; iter=1

for (a in 1:length(actions)) { #for both proximity and grooming 
  
  for (iter in 1:num_iter){ #for all iterations
    
    print(paste("%%%%%%%%%%%%%%%%%%",paste(actions[a],iter), "%%%%%%%%%%%%%%%%%%"))
    
    # 1. Calculate random subsamples
    randomScans = calcRandomScans(allScans)
    
    # 2. For each group, each year and pre-/post-hurr separately, compute weighted edge list: 
    group = c("V","KK"); g=1; Networks = data.frame(); #Make sure to re-initilize network at every iteration!!
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
          weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2 
          weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by avg #observations for each ID pair
          #IMPORTANT NOTE: i do NOT standardize weights per group and year, toehrwise i cannot see whether strength has icnreased or not
          weightedEL = weightedEL[which(weightedEL$weight != 0),]#only keep nonzero weighted edges
          weightedEL$group = group[g]; weightedEL$year = years[y]; weightedEL$isPost = isPost[h]; 
          
          Networks = rbind(Networks, weightedEL)# row bind
        } #end of isPost for loop
      } #end of Year for loop
    } #end of group for loop
    
    # 3. Find mean strength of relationship for each group/year
    strength = as.data.frame(matrix(NA, nrow = 1, ncol= 13)); 
    names(strength)=c("iter","action", "group", "year","Pre.overall", "Post.overall","numdata","Pre.data", 
                      "Post.data") #Initialize population-level interaction dataframe
    g=1; y=1
    for (g in 1:length(group)){ #For each group
      data.group= Networks[which(Networks$group == group[g]),]
      years = unique(data.group$year)
      for (y in 1:length(years)){ #For each year
        data = data.group[which(data.group$year == years[y]),]
        IDs = as.character(unique(c(as.character(data$alter), as.character(data$ego))))
       
        strength.per.ID = as.data.frame(matrix(NA, nrow = length(IDs), ncol= 9)); #Initialize ID-level stable partner interaction dataframe
        names(strength.per.ID)=c("iter","action","id", "group", "year","strength.all","strength.give","strength.get","isPost")
        h=1; id=1
        for (h in 1:2){
          for (id in 1:length(IDs)){
            strength.per.ID[id, "iter"] = iter; strength.per.ID[id, "action"] = actions[a] #keep track of iteraction and "Action" used to compute strength of connection
            strength.per.ID[id, "id"] = IDs[id]; strength.per.ID[id, "group"] = group[g]; strength.per.ID[id, "year"] = years[y] #keep track of ID, year and group
            strength.per.ID[id, "strength.all"] = sum(data$weight[which((data$ego == IDs[id] | data$alter == IDs[id]) & data$isPost == isPost[h])]) #Find strength of IN or OUT for hurr status "h"
            #Separate by groom give and groom get only for GROOMING (not proximity since there is no directionality)
            strength.per.ID[id, "strength.give"] = ifelse(actions[a] == "groom", sum(data$weight[which(data$alter == IDs[id] & data$isPost == isPost[h])]), NA)
            strength.per.ID[id, "strength.get"] = ifelse(actions[a] == "groom", sum(data$weight[which(data$ego == IDs[id] & data$isPost == isPost[h])]), NA)
            strength.per.ID[id, "isPost"] = isPost[h] #keep track of hurricane status
          }
          ID.strength.data.All=rbind(ID.strength.data.All, strength.per.ID)
        }
        
        #Record overall strength to stable partners pre- vs. post hurricane.
        strength$action = actions[a]; strength$iter =iter; strength$group = group[g]; strength$year = years[y] #keep track of action/iter/group/year
        strength$Pre.overall = sum(data$weight[data$isPost==0])
        strength$Post.overall = sum(data$weight[data$isPost==1])
        strength$numdata = nrow(data)
        strength$Pre.data = sum(data$weight[data$isPost==0], na.rm=T)
        strength$Post.data = sum(data$weight[data$isPost==1], na.rm=T)
        
        
        #Keep track of overall change through multiple iterations
        population.strength.All=rbind(population.strength.All, strength)
        
        #Save all
        save(population.strength.All, ID.strength.data.All, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/StrengthPrePost.RData")
        
        #IMPORTANT NOTE: strength give/get only makes sense for grooming, not for proximity. I am keeping it constant across both to keep is simple
        #and have data frames align. But you should NOT use "proximity strength give", since there is no directionaility to proximity.
      } #End of year loop
    } #End of group loop
  } #End of iteration loop
} #End of action loop
