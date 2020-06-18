# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code/functions") 
source("CalcSubsampledScans.R")
source("functions_Homophily.R")
source("KinshipPedigree.R")

#Load scan data, population and dominance info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("R.Data/SocialCapital.RData")

#Compute pedigree for all IDs in this group
allIDs= allScans$focalID[which(allScans$group == "KK"|allScans$group == "V")];
groupIDs = as.character(unique(allIDs))
IDmatch = match(groupIDs, as.character(bigped$ID)); discard.na = which(is.na(IDmatch))
if(length(discard.na)!=0) 
{pedigree = bigped[IDmatch[-discard.na],c("ID","DAM","SIRE")]
} else {pedigree = bigped[IDmatch,c("ID","DAM","SIRE")]}
ped <- KinshipPedigree(pedigree)

actions = c("groom", "prox")
num_iter = 500

newP.homophily.all=data.frame()
start_time <- Sys.time(); iter=1; a=1
for (a in 1:length(actions)) { #for both proximity and grooming 
  
  for (iter in 1:num_iter){ #for all iterations
    
    print(paste("%%%%%%%%%%%%%%%%%%",paste(actions[a],iter), "%%%%%%%%%%%%%%%%%%"))
    
    # 1. Calculate random subsamples
    randomScans = calcRandomScans(allScans)
    
    #Create age category for later
    age.thresh = as.numeric(quantile(randomScans$age,0.7))
    randomScans$age.cat="Y"; randomScans$age.cat[randomScans$age >= age.thresh]="O"
    
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
          #IMPORTANT NOTE: i do NOT standardize weights per group and year, otherwise i cannot see whether strength has increased or not
          weightedEL = weightedEL[which(weightedEL$weight != 0),]#only keep nonzero weighted edges
          weightedEL$iter = iter; weightedEL$group = group[g]; weightedEL$year = years[y]; weightedEL$isPost = isPost[h]; weightedEL$action = actions[a]
          
          #Save network
          Networks = rbind(Networks, weightedEL)# row bind
          
        } #end of isPost for loop
        data = Networks[which(Networks$group == group[g] & Networks$year == years[y]),]
        data.0 = data[which(data$isPost == 0),]; data.1 = data[which(data$isPost == 1),] #create two structures containing pre-hurricane data (data.0) and post-hurricane data
        #Find new partners
        newP = data.1[which(is.na(match(data.1$conc, data.0$conc))),] #new pairs = pairs that were not in pre-hurricane network
        
        el=newP;
        # 5. Find Kin relationship
        KinPairStats <- calcKinProps(el,ped)
        
        # 6. Find sex of givingID & receivingID
        SexPairStats <- calcSexProps(el,rscans)
        
        # 7. Find age category of givingID & receivingID
        AgePairStats <- calcAgeProps(el,rscans)

        # 8. Find dominance
        RankPairStats <- calcRankProps(el, dominance_info, year, rscans)

        SocialCapital = SocialCapital.ALL[which(SocialCapital.ALL$group==group[g] & SocialCapital.ALL$year==year),]
        # 9. Find PRE-HURRICANE standard Groom Strength
        threshold=as.numeric(quantile(SocialCapital.ALL$std.DSIgroom, probs = 0.70))
        GroomPairStats <- calcGroomProps(el, SocialCapital)

        # 10. Find PRE-HURRICANE standard number of Partner
        threshold=as.numeric(quantile(SocialCapital.ALL$std.numPartnersGroom, probs = 0.70))
        NumpPairStats <- calcNumpProps(el, SocialCapital, threshold)

        # 12. Find PRE-HURRICANE eigenvector centrality
        threshold=as.numeric(quantile(SocialCapital.ALL$eig.cent.groom, probs = 0.70))
        EigcentPairStats <- calcEigcentProps(el, SocialCapital, threshold)

        #Save new P homophily
        newP.homophily=data.frame(matrix(NA,ncol=4)); names(newP.homophily)=c("iter","action","group","year")
        newP.homophily$iter = iter; newP.homophily$action = actions[a]; newP.homophily$group = group[g]; newP.homophily$year = years[y]
        # newP.homophily = cbind(SexPairStats)
        newP.homophily = cbind(newP.homophily, KinPairStats, SexPairStats, AgePairStats, RankPairStats, GroomPairStats)#, NumpPairStats, EigcentPairStats)
        newP.homophily.all = rbind(newP.homophily.all,newP.homophily)# row bind
      } #end of Year for loop
    } #end of group for loop
    
    #Save all
    save(newP.homophily.all, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/newP.Homophily2.RData")
  } #End of iteration loop
} #End of action loop

