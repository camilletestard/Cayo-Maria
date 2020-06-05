# Change in strength of bond with existing partners.

library(stringr)
library(igraph)

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")
source("Social_Network_Analysis/CalcSubsampledScans.R")
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")

#Initilize data frames
population.strength.All=data.frame(); ID.strength.stableP.All=data.frame(); ID.strength.newP.All=data.frame(); ID.strength.oldP.All=data.frame()

num_iter = 500 #number of iterations
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
    
    # 3. Find strength of relationship to stable/new/old partners for each group/year
    strength = as.data.frame(matrix(NA, nrow = 1, ncol= 13)); 
    names(strength)=c("iter","action", "group", "year","Pre.overall", "Post.overall","numStablePairs","Pre.stableP", 
                      "Post.stableP", "numNewPairs", "Post.newP", "numOldPairs", "Pre.oldP") #Initialize population-level interaction dataframe
    g=1; y=1
    for (g in 1:length(group)){ #For each group
      data.group= Networks[which(Networks$group == group[g]),]
      years = unique(data.group$year)
      for (y in 1:length(years)){ #For each year
        data = data.group[which(data.group$year == years[y]),]
        # IDs = as.character(unique(c(as.character(data$alter), as.character(data$ego))))
        data.0 = data[which(data$isPost == 0),]; data.1 = data[which(data$isPost == 1),] #create two structures containing pre-hurricane data (data.0) and post-hurricane data
        
        ######################################################################
        #Find strength to stable partners for each ID who has a stable partner --> IDs who were interacting both before and after the hurricane
        stableP.1 = data.1[which(!is.na(match(data.1$conc, data.0$conc))),] #Find POST-hurr weights of pairs who were also interacting pre-hurr
        stableP.0 = data.0[which(!is.na(match(data.0$conc, data.1$conc))),] #Find weights of the SAME pairs PRE-hurr
        stableP = rbind(stableP.0, stableP.1) #Combine. Note: output is twice each pair with difference weights: one pre-hurr and one post-hurr, for comparison 
        
        IDs = unique(c(as.character(stableP$alter), as.character(stableP$ego))) #find unique IDs that have a stable partner.
        #Note: ID remains the same for newP and oldP. This is because we want to COMPARE strength to stable/old/new partners/pairs. Thus this must be done between the same IDs.
        
        stableP_strength = as.data.frame(matrix(NA, nrow = length(IDs), ncol= 9)); #Initialize ID-level stable partner interaction dataframe
        names(stableP_strength)=c("iter","action","id", "group", "year","strength.all","strength.give","strength.get","isPost")
        h=1; id=1
        for (h in 1:2){
          for (id in 1:length(IDs)){
            stableP_strength[id, "iter"] = iter; stableP_strength[id, "action"] = actions[a] #keep track of iteraction and "Action" used to compute strength of connection
            stableP_strength[id, "id"] = IDs[id]; stableP_strength[id, "group"] = group[g]; stableP_strength[id, "year"] = years[y] #keep track of ID, year and group
            stableP_strength[id, "strength.all"] = sum(stableP$weight[which((stableP$ego == IDs[id] | stableP$alter == IDs[id]) & stableP$isPost == isPost[h])]) #Find strength of IN or OUT for hurr status "h"
            #Separate by groom give and groom get only for GROOMING (not proximity since there is no directionality)
            stableP_strength[id, "strength.give"] = ifelse(actions[a] == "groom", sum(stableP$weight[which(stableP$alter == IDs[id] & stableP$isPost == isPost[h])]), NA)
            stableP_strength[id, "strength.get"] = ifelse(actions[a] == "groom", sum(stableP$weight[which(stableP$ego == IDs[id] & stableP$isPost == isPost[h])]), NA)
            stableP_strength[id, "isPost"] = isPost[h] #keep track of hurricane status
          }
          ID.strength.stableP.All=rbind(ID.strength.stableP.All, stableP_strength)
        }
        
        #Record overall strength to stable partners pre- vs. post hurricane.
        strength$action = actions[a]; strength$iter =iter; strength$group = group[g]; strength$year = years[y] #keep track of action/iter/group/year
        strength$Pre.overall = sum(data.0$weight)
        strength$Post.overall = sum(data.1$weight)
        strength$numStablePairs = nrow(stableP.0)
        strength$Pre.stableP = sum(stableP.0$weight, na.rm=T)
        strength$Post.stableP = sum(stableP.1$weight, na.rm=T)
        
        ######################################################################
        #Find strength to new partners for each ID
        newP = data.1[which(is.na(match(data.1$conc, data.0$conc))),] #new pairs
        # IDs = unique(c(as.character(newP$alter), as.character(newP$ego)))
        newP_strength = as.data.frame(matrix(NA, nrow = length(IDs), ncol= 9)); names(newP_strength)=c("iter","action","id", "group", "year","strength.all","strength.give","strength.get","isPost")
        for (id in 1:length(IDs)){
          newP_strength[id, "iter"] = iter; newP_strength[id, "action"] = actions[a]
          newP_strength[id, "id"] = IDs[id]; newP_strength[id, "group"] = group[g]; newP_strength[id, "year"] = years[y]
          newP_strength[id, "strength.all"] = sum(newP$weight[which(newP$ego == IDs[id] | newP$alter == IDs[id])])
          newP_strength[id, "strength.give"] = ifelse(actions[a] == "groom", sum(newP$weight[which(newP$alter == IDs[id])]), NA)
          newP_strength[id, "strength.get"] = ifelse(actions[a] == "groom", sum(newP$weight[which(newP$ego == IDs[id])]), NA)
          newP_strength[id, "isPost"] = 1
        }
        ID.strength.newP.All=rbind(ID.strength.newP.All, newP_strength)
        
        #Record overall strength of new pairs
        strength$numNewPairs = nrow(newP)
        strength$Post.newP = sum(newP$weight, na.rm=T)
        
        ######################################################################
        #Find strength to old partners (partners they don't interact with anymore post-hurricane) for each ID
        oldP = data.0[which(is.na(match(data.0$conc, data.1$conc))),] #old partners they don't interact with anymore post-hurricane
        # IDs = unique(c(as.character(oldP$alter), as.character(oldP$ego)))
        oldP_strength = as.data.frame(matrix(NA, nrow = length(IDs), ncol= 9)); names(oldP_strength)=c("iter","action","id", "group", "year","strength.all","strength.give","strength.get","isPost")
        for (id in 1:length(IDs)){
          oldP_strength[id, "iter"] = iter; oldP_strength[id, "action"] = actions[a]
          oldP_strength[id, "id"] = IDs[id]; oldP_strength[id, "group"] = group[g]; oldP_strength[id, "year"] = years[y]
          oldP_strength[id, "strength.all"] = sum(oldP$weight[which(oldP$ego == IDs[id] | oldP$alter == IDs[id])])
          oldP_strength[id, "strength.give"] = ifelse(actions[a] == "groom", sum(oldP$weight[which(oldP$alter == IDs[id])]), NA)
          oldP_strength[id, "strength.get"] = ifelse(actions[a] == "groom", sum(oldP$weight[which(oldP$ego == IDs[id])]), NA)
          oldP_strength[id, "isPost"] = 0
        }
        ID.strength.oldP.All=rbind(ID.strength.oldP.All, oldP_strength)
        
        #Record overall strength to old partners
        strength$numOldPairs = nrow(oldP)
        strength$Pre.oldP = sum(oldP$weight, na.rm=T)
        
        #Keep track of overall change through multiple iterations
        population.strength.All=rbind(population.strength.All, strength)
        
        #Save all
        save(population.strength.All, ID.strength.stableP.All, ID.strength.newP.All, ID.strength.oldP.All, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/Strength.Stable.New.Old.Partners.RData")
        
        #IMPORTANT NOTE: strength give/get only makes sense for grooming, not for proximity. I am keeping it constant across both to keep is simple
        #and have data frames align. But you should NOT use "proximity strength give", since there is no directionaility to proximity.
      } #End of year loop
    } #End of group loop
  } #End of iteration loop
} #End of action loop

population.strength.All = population.strength.All[,c("iter","action", "group", "year","Pre.overall", "Post.overall","numStablePairs","Pre.stableP", "Post.stableP", "numNewPairs", "Post.newP", "numOldPairs", "Pre.oldP")]
save(population.strength.All, ID.strength.stableP.All, ID.strength.newP.All, ID.strength.oldP.All, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/Strength.Stable.New.Old.Partners.RData")

