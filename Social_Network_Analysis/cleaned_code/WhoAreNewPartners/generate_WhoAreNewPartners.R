#Generate_WhoAreTheNewPartners
# This script computes the proportion of pair categories for each group/year/hurricane
# status separately. Pair categories based on: 
#   - Social status: Low->Low; Low->High; High->Low; High->High. Note: Low rank <70%; High rank >70%
#   - Sex: M->M; F->M; M->F; F->F
#   - Pre-hurr grooming strength: greg->greg; greg->shy; shy->greg; shy->shy. Note: threshold for shy/greg is 75% (or prctile)
#   - Kinship: close kin (ck, >0.25); distant kin (dk) and unrelated (unrel <0.125)
# This allows us to assess the difference in relationship distribution pre-to-post hurricane. E.g. are there more
# F->M relationships occuring post-disaster? The output unit is in weighted % (or weighted proportion of all 
#                                                                              edges/relationships, i.e. % of grooming time occurs for pairs of category X). 
# Comparisons are done in a fair way: we use subsampled scans, controlling for data collection biases. The proportions 
# are computed overall multiple iterations to make sure we cover all post-hurricane data.
# NOTE: Proportion takes into account overall change in density. It allowws to asses relative changed (instead of absolute)
# Functions Called: CalcSubsampledScans, functions_SocialSupport, KinshipPedigree
# Input: allScans2019.txt, SocialCapital.RData
# Ouput: PartnerAttributes.RData. Proportions of each pair category specified above pre-/post-hurricane.

# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
source("Social_Network_Analysis/functions_SocialSupport.R")
source("Social_Network_Analysis/KinshipPedigree.R")

#Load scan data, population and dominance info
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")
load("Social_Network_Analysis/SocialCapital.RData")

#Compute pedigree for all IDs in this group
allIDs= allScans$focalID[which(allScans$group == "KK"|allScans$group == "V")];
groupIDs = as.character(unique(allIDs))
IDmatch = match(groupIDs, as.character(bigped$ID)); discard.na = which(is.na(IDmatch))
if(length(discard.na)!=0) 
{pedigree = bigped[IDmatch[-discard.na],c("ID","DAM","SIRE")]
} else {pedigree = bigped[IDmatch,c("ID","DAM","SIRE")]}
ped <- KinshipPedigree(pedigree)

action = c("groom", "prox")
num_iter = 500
PartnerAttr = data.frame(matrix(ncol = 19, nrow = 0)); 
colnames(PartnerAttr)= c("action","iter","group","year","isPost","LowToHigh",
                       "HighToLow","HighToHigh","LowToLow", "Kin","Unrel", "MM", "MF", "FM","FF", 
                       "SocialHomophily.shy","SocialHomophily.greg","SocialOpposite.shygreg","SocialOpposite.gregshy")

start_time <- Sys.time(); iter=1; a=1
for (a in 1:length(action)){
  for (iter in 1:num_iter){
    
    print(paste("%%%%%%%%%%%%%%%%%% ",action[a], " iter",iter, " %%%%%%%%%%%%%%%%%%"))
    
    # 1. Calculate random subsamples
    randomScans = calcRandomScans(allScans)
    
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
          el$KinPairClass[which(el$KC >= .125 & el$KC < .25)] <- "dRel"
          el$KinPairClass[which(el$KC >= .25)] <- "rel"
          
          # 6. Find sex of givingID & receivingID
          el$sexGivingID   <- rscans$sex[match(as.character(el$alter), as.character(rscans$focalID))]
          el$sexReceivingID <- rscans$sex[match(as.character(el$ego), rscans$focalID)]
          # el$pairClass  <- "opp"; el$pairClass[which(el$sexGivingID == "F" & el$sexreceivingID == "F")] <- "bothFem"; el$pairClass[which(el$sexGivingID == "M" & el$sexreceivingID == "M")] <- "bothMal"
          el$SexPairClass  <- paste(el$sexGivingID,el$sexReceivingID,sep=".")
          
          # 7. Find dominance
          # Set high rank vs low rank
          dominance_info$ORD_RANK2 = "L"
          dominance_info$ORD_RANK2[which(dominance_info$X.DOMINATED>=70)]="H"
          # Classifying pairs as HH, LL or Opposite
          el$RankGivingID   <- dominance_info$ORD_RANK2[match(paste(as.character(el$alter),year,sep=""), as.character(dominance_info$IDyear))]
          el$RankReceivingID <- dominance_info$ORD_RANK2[match(paste(as.character(el$ego),year,sep=""), as.character(dominance_info$IDyear))]
          # el$pairClass  <- "opp"; el$pairClass[which(el$Rankalter == "H" & el$RankreceivingID == "H")] <- "bothH"; el$pairClass[which(el$Rankalter == "L" & el$RankreceivingID == "L")] <- "bothL"
          el$RankPairClass  <- paste(el$RankGivingID,el$RankReceivingID,sep=".")
          
          # 8. Find PRE-HURRICANE standard Groom Strength
          threshold=as.numeric(quantile(SocialCapital.ALL$std.DSIgroom, probs = 0.75))
          SocialCapital = SocialCapital.ALL[which(SocialCapital.ALL$group==group[g] & SocialCapital.ALL$year==year),]
          groom.strength.GiveID = SocialCapital$std.DSIgroom[match(as.character(el$alter),as.character(SocialCapital$id))]
          el$SocialGivingID <- ifelse(groom.strength.GiveID<threshold,"shy","greg")
          groom.strength.GetID = SocialCapital$std.DSIgroom[match(as.character(el$ego),as.character(SocialCapital$id))]
          el$SocialReceivingID <- ifelse(groom.strength.GetID<threshold,"shy","greg")
          el$SocialPairClass  <- paste(el$SocialGivingID,el$SocialReceivingID,sep=".")
          
          #Compute proportion of time spent between categories of interest. Weights are indivicative of time spent.
          LowToHigh = sum(el$weight[which(el$RankPairClass == "L.H")])/sum(el$weight)#length(which(el$RankPairClass == "L.H"))/nrow(el)
          HighToLow = sum(el$weight[which(el$RankPairClass == "H.L")])/sum(el$weight)#length(which(el$RankPairClass == "H.L"))/nrow(el)
          HighToHigh = sum(el$weight[which(el$RankPairClass == "H.H")])/sum(el$weight)#length(which(el$RankPairClass == "H.H"))/nrow(el)
          LowToLow = sum(el$weight[which(el$RankPairClass == "L.L")])/sum(el$weight)#length(which(el$RankPairClass == "L.L"))/nrow(el)
          Kin = sum(el$weight[which(el$KinPairClass == "rel")])/sum(el$weight) #length(which(el$KinPairClass == "rel"))/nrow(el)
          Unrel = sum(el$weight[which(el$KinPairClass == "unrelated")])/sum(el$weight)
          MM = sum(el$weight[which(el$SexPairClass == "M.M")])/sum(el$weight)#length(which(el$SexPairClass == "M.M"))/nrow(el)
          MF = sum(el$weight[which(el$SexPairClass == "M.F")])/sum(el$weight)#length(which(el$SexPairClass == "M.F"))/nrow(el)
          FM = sum(el$weight[which(el$SexPairClass == "F.M")])/sum(el$weight)#length(which(el$SexPairClass == "F.M"))/nrow(el)
          FF = sum(el$weight[which(el$SexPairClass == "F.F")])/sum(el$weight)
          SocialHomophily.shy = sum(el$weight[which(el$SocialPairClass == "shy.shy")])/sum(el$weight)
          SocialHomophily.greg = sum(el$weight[which(el$SocialPairClass == "greg.greg")])/sum(el$weight)#(length(which(el$SocialPairClass == "shy.shy")) + length(which(el$SocialPairClass == "greg.greg")))/nrow(el)
          socialOpposite.shygreg = sum(el$weight[which(el$SocialPairClass == "shy.greg")])/sum(el$weight)
          socialOpposite.gregshy = sum(el$weight[which(el$SocialPairClass == "greg.shy")])/sum(el$weight)
          
          PartnerAttrDF = data.frame(matrix(nrow=1,ncol=19)); 
          names(PartnerAttrDF)= c("action","iter","group","year","isPost","LowToHigh",
                                  "HighToLow","HighToHigh","LowToLow", "Kin","Unrel", "MM", "MF", "FM","FF", 
                                  "SocialHomophily.shy","SocialHomophily.greg","SocialOpposite.shygreg","SocialOpposite.gregshy")
          PartnerAttrDF[1,] <- c(action[a],iter,group[g],years[y],isPost[h],as.numeric(LowToHigh), as.numeric(HighToLow), as.numeric(HighToHigh),
                                 as.numeric(LowToLow), as.numeric(Kin),as.numeric(Unrel), as.numeric(MM), as.numeric(MF), as.numeric(FM), 
                                 as.numeric(FF), as.numeric(SocialHomophily.shy), as.numeric(SocialHomophily.greg), as.numeric(socialOpposite.shygreg), as.numeric(socialOpposite.gregshy))
          
          PartnerAttr= rbind(PartnerAttr, PartnerAttrDF)  
          save(list="PartnerAttr",file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/PartnerAttributes.RData")
          
        }
      }
    }
  }
}
