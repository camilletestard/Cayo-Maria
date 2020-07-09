#generate Aggressive Network Metrics
# This script computes aggression network density and mean weight to compare across years.
# It uses all types of aggression interactions (contact/non-contact/submission etc.). Because aggression data was not
# collected  during proximity scans in "normal data collection" years, I cannot use the sub-sampling approach we 
# developed for affiliative behaviors (proximity and grooming). Pre-hurricane: aggression is collected during focals 
# and rates should be computed by dividing counts by the number of hours. Post-hurricane (2018): aggression is collected 
# during scans. Thus, to compute weights I divide counts by the number of scan observations per IDs. This approach assumes
# a linear relationship whereby the more I observe an ID, the more counts of aggressive interactions I will get.
# So far, I have not implemented any sub-sampling in terms of number of observations per ID. However, I only use 
# "common IDs" across years - that is IDs that are involved in aggression throughout all years of observations (longitudinal).
# 
# IMPORTANT NOTE #1: aggression data post-hurricane is collected using a pseudo-systematic approach (almost ad-lib?) which
# may have introduced biases in our observations. However, if anything, we should see an artificial INCREASE in 
# #aggressive interactions (aggression is salient, easily observable during non-systematic sampling). However we rather 
# see a *decrease* in aggressive interactions.
# IMPORTANT NOTE #2: This could be simply due to the difference in feeding protocol. I should re-run the analysis only
# with PM data (not trivial to produce!).
# 
# Additionally, I use a bootstrap approach to compute a distribution of density and mean weight value for each network, 
# in each year. The bootstrap simply samples randomly, with replacement, rows from the edgelist and computes density 
# and mean weights n_boot times.

# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)
library(lubridate)
library(resample)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria")
load("R.Data/commonIDs.Rdata")
source("cleaned_code/Functions/functions_GlobalNetworkMetrics.R")

#Set parameters
network_mode = "directed" #"undirected" (if prox) or "directed" (if groom)
network_weighted = T
onlyPM=F

#For each group, each year separately: 
group = c("V","V","V","KK","KK","KK") #c("V","V","V","V","V","KK","KK","KK","S")
years = c(2016,2017,2018, 2015, 2017, 2018)#c(2015,2016,2017,2018, 2019, 2015, 2017, 2018, 2019)
groupyears =c("V2016","V2017","V2018","KK2015","KK2017","KK2018") #c("V2015","V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018", "S2019") 

density.all = data.frame(matrix(nrow=0,ncol=4));colnames(density.all)=c("dens","groupyear","group","year"); 
NetworkMetrics.all = data.frame(matrix(nrow=0,ncol=12));names(NetworkMetrics.all)=c("iter","id","deg","strength","hrs","groupyear","year","group","age","sex","percentrank","ordrank")
gy=5
for (gy in 1:length(groupyears)){ #For each group
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  #Load data
  setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
  data = read.csv(paste("Group",groupyears[gy],"_AgonsiticActions.txt",sep=""))
  # if (years[gy]==2018){meta_data$hrs.focalfollowed=meta_data$numObs} #if 2018, convert column #obs in #hrs to simplify coding after
  
  #create date of observation information: 
  data$semester = semester(data$date)
  data$quarter = quarter(data$date)
  if (length(which(data$timeblock=="AM"))==0) {data$timeBlock = "AM"; data$timeBlock[data$timeblock>3]="PM"
  } else {data$timeBlock=data$timeblock}
  
  if (onlyPM == TRUE) {data=data[data$timeBlock=="PM",]}
  
  #Output the Master Edgelist of all possible pairs given the unique IDs.
  #Load unique IDs common across years. This allows to make sure we compare the same indidivuals across years.
  if (group[gy]=="V") {unqIDs = commonIDsV} else if(group[gy]=="KK") {unqIDs = commonIDsKK} else {unqIDs = as.character(unique(meta_data$id))}
  masterEL = calcMasterEL_groom(unqIDs) #edge list taking direction into account
  
  # 4. Output weighted edgelist from the Master Edgelist.
  data$conc <- paste(data[,1],data[,2],sep="."); weightedEL = masterEL
  count = data.frame(table(data$conc))
  weightedEL$count = count$Freq[match(weightedEL$conc,count$Var1)]
  weightedEL$count[which(is.na(weightedEL$count))] = 0
  weightedEL$hrs <- (meta_data$hrs.focalfollowed[match(weightedEL$givingID, meta_data$id)] + 
                       meta_data$hrs.focalfollowed[match(weightedEL$receivingID, meta_data$id)])/2
  weightedEL$weight <- round(weightedEL$count / weightedEL$hrs, 5) #add weight information by dividing by the #scans for each individual
  meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
  weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights by group mean
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$hrs <-NULL; #delete those calumn variables
  
  length(which(weightedEL$weight>0)) #check density
  
  ########################################
  #Bootstrap Procedure; Get network metrics
  ########################################
  nboot = 100
  density = data.frame(matrix(NA, nrow=nboot, ncol=2)); names(density)=c("dens","groupyear")
  mean.weight = data.frame(matrix(NA, nrow=nboot, ncol=2)); names(mean.weight)=c("weight","groupyear")
  for (boot in 1:nboot) {
    
    print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy]," boot# ",boot, " %%%%%%%%%%%%%%%%%%"))
    
    idx = sample(seq(1:nrow(weightedEL)),size=nrow(weightedEL), replace=T)
    weightedEl.res = weightedEL[idx,] #resampled weightedEL
    
    # generate density and mean weight
    density$dens[boot] = length(which(weightedEl.res$weight!=0))/nrow(weightedEl.res)
    # mean.weight$weight[boot] = mean(weightedEl.res$weight!=0)
    
    #Transform EL into matrix 
    adjMat = dils::AdjacencyFromEdgelist(weightedEl.res)
    data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
    
    #read adjacency matrix
    m=as.matrix(data) # coerces the data set as a matrix
    am.g=graph.adjacency(m,mode="undirected",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    graph = am.g 
    
    #Get the network measures
    NetworkMetrics = data.frame(matrix(NA, nrow = length(V(graph)), ncol = 8)); 
    names(NetworkMetrics)=c("iter","id","deg","strength","hrs","groupyear","year","group")#,"INdeg","OUTdeg","between","eig.cent", "clusterCoeff")
    NetworkMetrics$id = as_ids(V(graph))
    #Weighted degree (Strength, undirected)
    NetworkMetrics$deg<-igraph::degree(graph)
    NetworkMetrics$strength<-igraph::strength(graph,vids = V(graph),mode="all")
    NetworkMetrics$hrs<-meta_data$hrs.focalfollowed[match(NetworkMetrics$id, meta_data$id)]
    NetworkMetrics$iter = boot

    NetworkMetrics$groupyear = groupyears[gy]; NetworkMetrics$year=years[gy]; NetworkMetrics$group=group[gy]
    NetworkMetrics[,c("age", "sex","percentrank", "ordrank")]=meta_data[match(NetworkMetrics$id, meta_data$id), c("age", "sex","percofsex.dominanted", "ordinal.rank")]
    
    NetworkMetrics.all=rbind(NetworkMetrics.all,NetworkMetrics)
    
  }
  density$groupyear = groupyears[gy]; density$year=years[gy]; density$group=group[gy]
  density.all=rbind(density.all,density)

}

if (onlyPM==T){save(density.all,NetworkMetrics.all, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/AggNetMetrics.onlyPM.RData")
} else {
    save(density.all,NetworkMetrics.all, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/AggNetMetrics.RData")}

