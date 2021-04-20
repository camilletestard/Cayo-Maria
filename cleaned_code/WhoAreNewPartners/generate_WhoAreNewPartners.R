# generate_WhoAreTheNewPartners: 
# This script computes the proportion of time spent grooming 
# between each pair categories relative to total grooming; for each group/year/hurricane status separately. 
# Dyadic categories are based on: 
#   - Social status: Low->Low; Low->High; High->Low; High->High. Note: Low rank < top 80%; High rank = top 80%
#   - Sex: M->M; F->M; M->F; F->F
#   - Pre-hurr grooming strength: greg->greg; greg->shy; shy->greg; shy->shy. Note: threshold for shy < 80% (or prctile); greg > 80%
#   - Kinship: related (rel>0.125) and unrelated (unrel <0.125)
# This allows us to assess the difference in "relationship distribution" pre-to-post hurricane. E.g. is there relatively more grooming 
# happening from fem->Male post-disaster? The output unit is in % of edge weights.
# Comparisons are done in a controlled fashion: we use subsampled scans, controlling for data collection biases. The same individuals are present in
# both pre and post-hurricane networks, and the same number of obsevrations per ID are considered pre/post. The proportions 
# are computed overall multiple subsampling iterations to make sure we cover all post-hurricane data.
# NOTE: Proportion takes into account overall change in density. It allows to assess relative changes (instead of absolute)
# Functions Called: CalcSubsampledScans, functions_SocialSupport, KinshipPedigree
# Input: allScans.txt, SocialCapital.RData
# Ouput: PartnerAttributes.RData.
# Camille Testard - 2020

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
source("functions_GlobalNetworkMetrics.R")
source("KinshipPedigree.R")

#Load scan data, population and dominance info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE_2021.txt", sep="\t")
#IMPORTANT NOTE: this input file belongs to CPRC and has restricted access.
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("R.Data/SocialCapital.RData")

#Compute pedigree for all IDs
pedigree = bigped[,c("ID","DAM","SIRE")]
ped <- KinshipPedigree(pedigree)

action = c("groom", "prox")
num_iter = 500
PartnerAttr = data.frame(matrix(ncol = 31, nrow = 0)); 
colnames(PartnerAttr)= c("action","iter","group","year","isPost","LowToHigh",
                         "HighToLow","HighToHigh","LowToLow", "Kin","Unrel", "MM", "MF", "FM","FF",
                         "SocialHomophily.shy","SocialHomophily.greg","SocialOpposite.shygreg","SocialOpposite.gregshy")

start_time <- Sys.time(); iter=1; a=1

# for (a in 1:length(action)){
for (iter in 1:num_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% ",action[a], " iter",iter, " %%%%%%%%%%%%%%%%%%"))
  
  # 1. Calculate random subsamples
  randomScans = calcRandomScans(allScans)
  
  #Create age category for later
  age.thresh = as.numeric(quantile(randomScans$age,0.8))
  randomScans$age.cat="Y"; randomScans$age.cat[randomScans$age >= age.thresh]="O"
  
  #For each group, each year separately: 
  group = c("V","KK")
  
  g=1; y=1; h=1
  for (g in 1:length(group)){ #For each group
    randscansG = randomScans[which(randomScans$group==group[g]),] 
    
    years = unique(randscansG$year)
    for (y in 1:length(years)){ #For each year in that group
      randscansY = randscansG[which(randscansG$year==years[y]),] 
      year = years[y]
      
      isPost = c(0,1)
      for (h in 1:length(isPost)){ #pre- and post-hurricane 
        
        print(paste("%%%%%%%%%%%%%%%%%%",paste(group[g],years[y],isPost[h],sep="."), "%%%%%%%%%%%%%%%%%%"))
        
        rscans = randscansY[which(randscansY$isPost==isPost[h]),] 
        numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
        
        # 2. Find all unique IDs
        unqIDs = unique(c(as.character(rscans$focalID)))
        
        # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
        if (action[a] == "prox") {masterEL = calcMasterEL(unqIDs)}
        if (action[a] == "groom") {masterEL = calcMasterEL_groom(unqIDs)}
        
        # 4. Output weighted edgelist from the Master Edgelist.
        options(warn = -1) #set options to ignore all warnings
        if (action[a] == "prox") {weightedEL = calcEdgeList(rscans,masterEL)}
        if (action[a] == "groom") {weightedEL = calcEdgeList_groom(rscans,masterEL)
        weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
        weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
        weightedEL = weightedEL[,c("alter","ego","count")]
        }
        weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2 #take the average number of scans for each grooming partner
        weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by the #scans for each individual
        meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
        weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights 
        weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$numscans <-NULL #delete those calumn variables
        
        #Only keep non-zero weight interactions
        weightedEL = weightedEL[which(weightedEL$weight>0),]
        
        # 5. Find Kin relationship
        el=weightedEL
        KC      <- NULL; for(i in 1:length(el[,1])){ 
          KC[i] <-  ped[which(rownames(ped)==as.character(el$alter[i])) , which(colnames(ped)==as.character(el$ego[i]))]
        }
        el$KC   <- round(KC, 4)
        el$KinPairClass <- "unrelated"
        el$KinPairClass[which(el$KC >= .125)] <- "rel" #In this package: kinship will be 0.5 for an individual with themselves, .25 between mother and child, .125 between an uncle and neice
        
        # 6. Find sex of givingID & receivingID
        el$sexGivingID   <- rscans$sex[match(as.character(el$alter), as.character(rscans$focalID))]
        el$sexReceivingID <- rscans$sex[match(as.character(el$ego), rscans$focalID)]
        # el$pairClass  <- "opp"; el$pairClass[which(el$sexGivingID == "F" & el$sexreceivingID == "F")] <- "bothFem"; el$pairClass[which(el$sexGivingID == "M" & el$sexreceivingID == "M")] <- "bothMal"
        el$SexPairClass  <- paste(el$sexGivingID,el$sexReceivingID,sep=".")
        
        # 7. Find age category of givingID & receivingID
        el$ageGivingID   <- rscans$age.cat[match(as.character(el$alter), as.character(rscans$focalID))]
        el$ageReceivingID <- rscans$age.cat[match(as.character(el$ego), rscans$focalID)]
        # el$pairClass  <- "opp"; el$pairClass[which(el$sexGivingID == "F" & el$sexreceivingID == "F")] <- "bothFem"; el$pairClass[which(el$sexGivingID == "M" & el$sexreceivingID == "M")] <- "bothMal"
        el$AgePairClass  <- paste(el$ageGivingID,el$ageReceivingID,sep=".")
        
        # 8. Find dominance
        # Set high rank vs low rank
        dominance_info$ORD_RANK2 = "L"
        dominance_info$ORD_RANK2[which(dominance_info$X.DOMINATED>=80)]="H"
        # Classifying pairs as HH, LL or Opposite
        el$RankGivingID   <- dominance_info$ORD_RANK2[match(paste(as.character(el$alter),year,sep=""), as.character(dominance_info$IDyear))]
        el$RankReceivingID <- dominance_info$ORD_RANK2[match(paste(as.character(el$ego),year,sep=""), as.character(dominance_info$IDyear))]
        # el$pairClass  <- "opp"; el$pairClass[which(el$Rankalter == "H" & el$RankreceivingID == "H")] <- "bothH"; el$pairClass[which(el$Rankalter == "L" & el$RankreceivingID == "L")] <- "bothL"
        el$RankPairClass  <- paste(el$RankGivingID,el$RankReceivingID,sep=".")
        
        SocialCapital = SocialCapital.ALL[which(SocialCapital.ALL$group==group[g] & SocialCapital.ALL$year==year),]
        # 9. Find PRE-HURRICANE standard Groom Strength
        threshold=as.numeric(quantile(SocialCapital.ALL$std.DSIgroom, probs = 0.80))
        groom.strength.GiveID = SocialCapital$std.DSIgroom[match(as.character(el$alter),as.character(SocialCapital$id))]
        el$GroomGivingID <- ifelse(groom.strength.GiveID<threshold,"shy","greg")
        groom.strength.GetID = SocialCapital$std.DSIgroom[match(as.character(el$ego),as.character(SocialCapital$id))]
        el$GroomReceivingID <- ifelse(groom.strength.GetID<threshold,"shy","greg")
        el$GroomPairClass  <- paste(el$GroomGivingID,el$GroomReceivingID,sep=".")
        
        # 10. Find PRE-HURRICANE standard number of Partner
        threshold=as.numeric(quantile(SocialCapital.ALL$std.numPartnersGroom, probs = 0.80))
        numP.strength.GiveID = SocialCapital$std.numPartnersGroom[match(as.character(el$alter),as.character(SocialCapital$id))]
        el$numPGivingID <- ifelse(numP.strength.GiveID<threshold,"shyP","gregP")
        numP.strength.GetID = SocialCapital$std.numPartnersGroom[match(as.character(el$ego),as.character(SocialCapital$id))]
        el$numPReceivingID <- ifelse(numP.strength.GetID<threshold,"shyP","gregP")
        el$numPPairClass  <- paste(el$numPGivingID,el$numPReceivingID,sep=".")
        
        # 12. Find PRE-HURRICANE eigenvector centrality
        threshold=as.numeric(quantile(SocialCapital.ALL$eig.cent.groom, probs = 0.80))
        EigCent.strength.GiveID = SocialCapital$eig.cent.groom[match(as.character(el$alter),as.character(SocialCapital$id))]
        el$EigCentGivingID <- ifelse(EigCent.strength.GiveID<threshold,"ecL","ecH")
        EigCent.strength.GetID = SocialCapital$eig.cent.groom[match(as.character(el$ego),as.character(SocialCapital$id))]
        el$EigCentReceivingID <- ifelse(EigCent.strength.GetID<threshold,"ecL","ecH")
        el$EigCentPairClass  <- paste(el$EigCentGivingID,el$EigCentReceivingID,sep=".")
        
        #Compute proportion of time spent between categories of interest. Weights are indicative of time spent.
        #Total pairs
        total.pair.weights=sum(el$weight)
        total.pairs=length(el$weight[el$weight!=0])
        #Rank category
        LowToHigh = sum(el$weight[which(el$RankPairClass == "L.H")])/sum(el$weight)#length(which(el$RankPairClass == "L.H"))/nrow(el)
        HighToLow = sum(el$weight[which(el$RankPairClass == "H.L")])/sum(el$weight)#length(which(el$RankPairClass == "H.L"))/nrow(el)
        HighToHigh = sum(el$weight[which(el$RankPairClass == "H.H")])/sum(el$weight)#length(which(el$RankPairClass == "H.H"))/nrow(el)
        LowToLow = sum(el$weight[which(el$RankPairClass == "L.L")])/sum(el$weight)#length(which(el$RankPairClass == "L.L"))/nrow(el)
        #Kin category
        Kin = sum(el$weight[which(el$KinPairClass == "rel")])/sum(el$weight) #length(which(el$KinPairClass == "rel"))/nrow(el)
        Unrel = sum(el$weight[which(el$KinPairClass == "unrelated")])/sum(el$weight)
        #sex category
        MM = sum(el$weight[which(el$SexPairClass == "M.M")])/sum(el$weight)#length(which(el$SexPairClass == "M.M"))/nrow(el)
        MF = sum(el$weight[which(el$SexPairClass == "M.F")])/sum(el$weight)#length(which(el$SexPairClass == "M.F"))/nrow(el)
        FM = sum(el$weight[which(el$SexPairClass == "F.M")])/sum(el$weight)#length(which(el$SexPairClass == "F.M"))/nrow(el)
        FF = sum(el$weight[which(el$SexPairClass == "F.F")])/sum(el$weight)
        #Grooming category
        SocialHomophily.shy = sum(el$weight[which(el$GroomPairClass == "shy.shy")])/sum(el$weight)
        SocialHomophily.greg = sum(el$weight[which(el$GroomPairClass == "greg.greg")])/sum(el$weight)#(length(which(el$SocialPairClass == "shy.shy")) + length(which(el$SocialPairClass == "greg.greg")))/nrow(el)
        socialOpposite.shygreg = sum(el$weight[which(el$GroomPairClass == "shy.greg")])/sum(el$weight)
        socialOpposite.gregshy = sum(el$weight[which(el$GroomPairClass == "greg.shy")])/sum(el$weight)
        
        PartnerAttrDF = data.frame(matrix(nrow=1,ncol=33)); 
        names(PartnerAttrDF)= c("action","iter","group","year","isPost","total.pairs","total.pairs.weights","LowToHigh",
                                "HighToLow","HighToHigh","LowToLow", "Kin","Unrel", "MM", "MF", "FM","FF",
                                "SocialHomophily.shy","SocialHomophily.greg","SocialOpposite.shygreg","SocialOpposite.gregshy")
        PartnerAttrDF[1,] <- c(action[a],iter,group[g],years[y],isPost[h],total.pairs,total.pair.weights, as.numeric(LowToHigh), as.numeric(HighToLow), as.numeric(HighToHigh),
                               as.numeric(LowToLow), as.numeric(Kin),as.numeric(Unrel), as.numeric(MM), as.numeric(MF), as.numeric(FM), 
                               as.numeric(SocialHomophily.shy), as.numeric(SocialHomophily.greg), as.numeric(socialOpposite.shygreg), as.numeric(socialOpposite.gregshy),)
        
        PartnerAttr= rbind(PartnerAttr, PartnerAttrDF)  
        save(list="PartnerAttr",file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/PartnerAttributes.RData")
        
      }
    }
  }
}
# }
