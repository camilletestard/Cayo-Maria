#GENERATE GLOBAL NETWORK METRICS
# This script generates social networks based on proximity or grooming data.
# It also generates network metrics- density, gini coeff, sex/rank/kinship proportions
# Finally, it runs over all groups and years seperately, after sub-sampling.
# Note: edge weights are calculated by counting the frequency of proximity or grooming interactions between a pair
# of individuals (frequency rather than time becuase we are dealing with scan data, not focal). We then divide by the 
# average number of scans where each member of the dyad was seen.
# Note2: this assumes a linear relationship between number of scans and #observations in behavior X. (i.e. the more you
# observe an individual, the more likely he will be seen grooming or in proximity).
# Functions called: CalcSubsampledScans, functions_GlobalNetworkMetrics, KinshipPedigree
# Input: allScans.txt, PEDIGREE.txt
# Output: global network stats (density, community size, clustering coeff), partner preference stats (ratio obs/exp
# for sex, kin, rank). Output file: "AllStats.RData".
# IMPORTANT NOTE: I standardize weights by dividing by the mean for the group/year. 

# load libraries
library(dplyr)
library(igraph)
library(tnet)
library(ineq)
library(stringr)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
source("cleaned_code/Functions/functions_GlobalNetworkMetrics.R")
source("cleaned_code/Functions/KinshipPedigree.R")

#Load scan data and population info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")

#Compute pedigree for all IDs in this group
IDs = allScans$focalID[which(allScans$group == "KK"|allScans$group == "V")]; #only select group V and groupKK individuals
groupIDs = as.character(unique(IDs))
IDmatch = match(groupIDs, as.character(bigped$ID)); discard.na = which(is.na(IDmatch)) #find IDs in pedigree, discard NAs
if(length(discard.na)!=0) #if their are unknown pedigree for certain idnvidiuals
{pedigree = bigped[IDmatch[-discard.na],c("ID","DAM","SIRE")] #find pedigree only for known individuals
} else {pedigree = bigped[IDmatch,c("ID","DAM","SIRE")]}
ped <- KinshipPedigree(pedigree)

#Load Dominance info
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

num_iter = 500 #set number of iterations
AllStats = list() #initialize output
actions = c("groom","prox") #set beahviors on which social network can be based: either proximity or grooming

start_time <- Sys.time(); #start timer
a=1
for (a in 1:length(actions)){ #for all actions
  action = actions[a]
  
  iter=1
  for (iter in 1:num_iter){ #for all iterations
    print(paste("%%%%%%%%%%%%%%%%%%", actions[a], "iter",iter, "%%%%%%%%%%%%%%%%%%"))
    
    # 1. Calculate random subsamples
    # allScans$isPost[which(allScans$year>2018)] = 0 #make 2019 a "pre-hurricane" equivalent to compute equivalent 2018-2019 sub-samples. 
    #This will allow us to see whether density goes back down to normal.
    randomScans = calcRandomScans(allScans)
    
    #For each group, each year separately: 
    group = c("V","KK")
    g=1; y=1; h=1
    for (g in 1:length(group)){ #For each group
      randscansG = randomScans[which(randomScans$group==group[g]),] #subsampled scans for group g
      
      years = unique(randscansG$year)
      for (y in 1:length(years)){ #For each year in that group
        randscansY = randscansG[which(randscansG$year==years[y]),] #subsampled scans for group g & year y
        year = years[y]
        
        isPost = c(0,1)
        for (h in 1:length(isPost)){ #pre- and post-hurricane 
          
          print(paste("%%%%%%%%%%%%%%%%%%",paste(group[g],years[y],isPost[h],sep="."), "%%%%%%%%%%%%%%%%%%"))
          
          rscans = randscansY[which(randscansY$isPost==isPost[h]),] #subsampled scans for group g & year y & hurricane status h
          
          # 2. Find the number of scans per individual (to correct for unequal representation later)
          numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
          
          # 3. Find the number of unique IDs.
          unqIDs = unique(c(as.character(rscans$focalID)))
          
          # 4. Output the Master Edgelist of all possible pairs given the unique IDs.
          if (action == "prox") {masterEL = calcMasterEL(unqIDs)}
          if (action == "groom") {masterEL = calcMasterEL_groom(unqIDs)}
          
          # 5. Output weighted edgelist from the Master Edgelist.
          options(warn = -1) #set options to ignore all warnings
          if (action == "prox") {weightedEL = calcEdgeList(rscans,masterEL)}
          if (action == "groom") {weightedEL = calcEdgeList_groom(rscans,masterEL)
          weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
          weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
          weightedEL = weightedEL[,c("alter","ego","count")] #transform output to equal proximity data output.
          }
          #set num scans between each pair to be the average of the number of scans of the two individuals
          weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2 
          weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by #observations
          meanWeight = mean(weightedEL$weight[weightedEL$weight!=0])
          weightedEL$weight = weightedEL$weight/meanWeight
          weightedEL$count <- NULL;weightedEL$conc <- NULL #delete those calumn variables
          
          # 6. Generate graphs from proximity scans (has sex and age vertices attributes)
          sexage= data.frame()
          for (i in 1:length(unqIDs)){
            idx = which(rscans$focalID == unqIDs[i])
            sexage[i,c("id","sex","age")] <- c(unqIDs[i],as.character(rscans$sex[idx[1]]),rscans$age[idx[1]])
          }
          sexage$isFemale = 0; sexage$isFemale[which(sexage$sex == "F")]=1;
          options(warn = -1)
          if (action == "prox") {netList <- createIG(weightedEL, unqIDs, sexage)}
          if (action == "groom") {netList <- createIG_groom(weightedEL, unqIDs, sexage)}
          
          # 7. Calculate global network statistics, for whole network, male only and female only.
          GlobalStats <- calcGenStats(netList)
          
          # 8. Calculate partner sex preference, for whole network
          if (length(netList)>4){
            SexPairStats = calcSexProps(netList)}
          
          # 9. Calculate partner kin preference, for whole network
          KinPairStats <- calcKinProps(netList, ped) 
          
          # 10. Calculate partner rank preference, for whole network
          RankPairStats <- calcRankProps(netList, dominance_info, year)
          
          # 11. Combine all stats and save
          AllStatsDF <- bind_cols(data_frame(actions[a]),GlobalStats, SexPairStats, KinPairStats, RankPairStats)
          name = paste(group[g],years[y],isPost[h],sep=".")
          
          AllStats[[name]] = rbind(AllStats[[name]], AllStatsDF)  
          save(AllStats,file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/AllStats.RData")
          
        } #end of of pre-/post-hurricane loop
      } #end of year for loop
    } #end of group for loop
  } #end of iter for loop
} #end of actions for loop


end_time <- Sys.time()
end_time - start_time
