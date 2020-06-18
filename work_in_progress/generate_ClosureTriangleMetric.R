# Change in strength of bond with existing partners.

library(stringr)
library(igraph)

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code/Functions") 
source("CalcSubsampledScans.R")
source("functions_GlobalNetworkMetrics.R")

#Initilize data frames

num_iter = 500 #number of iterations
actions = c("groom", "prox"); a=1; iter=1
prop.triangle.close.All = as.data.frame(matrix(NA, nrow=num_iter*length(actions), ncol= 5)); names(prop.triangle.close.All)=c("iter","action","group", "year","propClosedTriangle")
closure.triangle.All = as.data.frame(matrix(NA, nrow=1, ncol= 7)); names(closure.triangle.All)=c("iter","action","group", "year", "id","id.partner","isClosedTriangle")
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
    prop.triangle.close = as.data.frame(matrix(NA, nrow=1, ncol= 5)); names(prop.triangle.close)=c("iter","group", "year","action","propClosedTriangle")
    g=1; y=1
    for (g in 1:length(group)){ #For each group
      data.group= Networks[which(Networks$group == group[g]),]
      years = unique(data.group$year)
      for (y in 1:length(years)){ #For each year
        data = data.group[which(data.group$year == years[y]),]
        # IDs = as.character(unique(c(as.character(data$alter), as.character(data$ego))))
        data.0 = data[which(data$isPost == 0),]; data.1 = data[which(data$isPost == 1),] #create two structures containing pre-hurricane data (data.0) and post-hurricane data
        
        ######################################################################
        #Find strength to new partners for each ID
        newP = data.1[which(is.na(match(data.1$conc, data.0$conc))),] #new pairs
        IDs = unique(c(as.character(newP$alter), as.character(newP$ego))); id =1
        closure.triangle = as.data.frame(matrix(NA, nrow=1, ncol= 7)); names(closure.triangle)=c("iter","action","group", "year", "id","id.partner","isClosedTriangle")
        for (id in 1:length(IDs)){
          id.prePartners = c(as.character(data.0$alter[data.0$ego == IDs[id]]), as.character(data.0$ego[newP$alter == IDs[id]]))
          new.partners = c(as.character(newP$alter[newP$ego == IDs[id]]), as.character(newP$ego[newP$alter == IDs[id]]))
          id.p=1
          for (id.p in 1:length(new.partners)){
            newP.prePartners = c(as.character(data.0$alter[data.0$ego == new.partners[id.p]]), as.character(data.0$ego[data.0$alter == new.partners[id.p]]))
            closure.triangle$id = IDs[id]; closure.triangle$id.partner = new.partners[id.p]; 
            closure.triangle$iter=iter; closure.triangle$action = actions[a]; closure.triangle$group = group[g]; closure.triangle$year = years[y]
            matching.partners = any(!is.na(match(newP.prePartners, id.prePartners)))
            closure.triangle$isClosedTriangle = ifelse(matching.partners, 1, 0)
            closure.triangle.All=rbind(closure.triangle.All, closure.triangle)
          }
        }
        prop.triangle.close$propClosedTriangle = length(which(closure.triangle.All$isClosedTriangle == 1))/length(closure.triangle.All$isClosedTriangle)
        prop.triangle.close$iter = iter; prop.triangle.close$action = actions[a]; prop.triangle.close$group = group[g]; prop.triangle.close$year = years[y]
        prop.triangle.close.All=rbind(prop.triangle.close.All, prop.triangle.close)
      } #End of year loop
    } #End of group loop
    
    #Save all
    save(closure.triangle.All, prop.triangle.close.All, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ClosedTriangles.RData")
  } #End of iteration loop
} #End of action loop


