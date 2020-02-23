######################################################
# Estimate ranking and rank certainty using Perc package
######################################################
#Note: this script is not flexible yet. Variables need to be changed if interested in different groups/years

library(Perc)
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria")# set working directory


#Load dyadic win-loss matrix for HH males 2016:
dominance_adjacency <- read.csv("Behavioral_Data/GroupHH2016_files/GroupHH2016.agg_adjacencyF.csv")
#IMPORTANT NOTE: for Lauren's dominance, male dominance matrix should be flipped, NOT female dominance matrix.
row.names(dominance_adjacency) = dominance_adjacency[,1] #assign correct row names
dominance_adjacency[,1] = NULL
colnames(dominance_adjacency) = row.names(dominance_adjacency)
dominance_adjacency[is.na(dominance_adjacency)] = 0 #replace NA values with 0
dominance_adjacency = as.matrix(dominance_adjacency) #make sure the input matrix is a data frame

#Preprocess data for Perc:
confmatrix <- dominance_adjacency #winners need to be in rows and losers in column. This is the case for the 
#agonistic actions matrices. but NOT for the male dominance matrix from lauren, which is flipped.
#If flipped: t(dominance_adjacency)
row.names(confmatrix) = row.names(dominance_adjacency)
sorted_subjects = unique(sort(colnames(confmatrix))) #sort matrix
confmatrix = confmatrix[sorted_subjects, sorted_subjects]

#Testing the transitivity of the network:
conftrans <- transitivity(confmatrix)
conftrans$transitive; conftrans$intransitive; conftrans$transitivity

#Computing dominance probability:
DominanceProbability <-conductance(confmatrix,maxLength =2)

##Examining the information gained to determine maxLength value:
#informationGain <- confmatrix - DominanceProbability$imputed.conf # substracting the original conflict matrix from imputed conflict matrix
## generating a heatmap representing information gained by using information from indirect pathways.
#plotConfmat(informationGain, ordering = NA, labels = TRUE)
##Based on this graph, use maxLength =2

#Converting to long format win-loss probability. This outputs one value of rank certainty for each dyad.
dyadicLongConverter(DominanceProbability$p.hat)

# find ranking order from simulations
s.rank <- simRankOrder(DominanceProbability$p.hat, num = 10, kmax = 100)
s.rank$BestSimulatedRankOrder # displaying the best simulated rank order
#s.rank$Costs # displaying cost for each simulated annealing run
#s.rank$AllSimulatedRankOrder
plotConfmat(DominanceProbability$p.hat, ordering = s.rank[[1]]$ID, labels = TRUE) #shows the degree of certainty in the network in a glance

#Compute rank%
ranks = 1:nrow(s.rank$BestSimulatedRankOrder)
s.rank$BestPercentRankOrder= ((nrow(s.rank$BestSimulatedRankOrder) - ranks +1)
                              /nrow(s.rank$BestSimulatedRankOrder))*100

#Compute rank certainty for each individual
rc <- rowMeans(DominanceProbability$p.hat)
s.rank$rcBestRankOrder = rc[s.rank$BestSimulatedRankOrder[,'ID']]

#Summarize rank information from Perc package
rank_info_perc = cbind(s.rank$BestSimulatedRankOrder, s.rank$BestPercentRankOrder, s.rank$rcBestRankOrder)
names(rank_info_perc)=c('id','rank','percentrank','rc')
rank_info_perc$sex = 'M'
rank_info_perc$year = '2018'
write.csv(rank_info_perc,'Behavioral_Data/GroupHH2018.rankInfoPercM.csv')