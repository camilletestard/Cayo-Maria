#generate Aggressive Network Metrics
# This script plots the output from generate_AggNetworkMetrics, i.e. density and mean weights.
# Inputs: "AggNetMetrics.RData".
# Outputs: Boxplots of aggression network metrics separated by year and group.

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
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("commonIDs.Rdata")
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")
source("Social_Network_Analysis/KinshipPedigree.R")

#Set parameters
network_mode = "directed" #"undirected" (if prox) or "directed" (if groom)
network_weighted = T

#For each group, each year separately: 
group = c("V","V","V","V","KK","KK","KK","S") #c("V","V","V","V","V","KK","KK","KK","S")
years = c(2016,2017,2018, 2019, 2015, 2017, 2018, 2019)#c(2015,2016,2017,2018, 2019, 2015, 2017, 2018, 2019)
groupyears =c("V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018","S2019") #c("V2015","V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018", "S2019") 

density.all = data.frame(matrix(nrow=0,ncol=4));colnames(density.all)=c("dens","groupyear","group","year"); 
mean.weight.all = data.frame(matrix(nrow=0,ncol=4));colnames(mean.weight.all)=c("weight","groupyear","group","year"); 
gy=1
for (gy in 1:length(groupyears)){ #For each group
  
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
  data = read.csv(paste("Group",groupyears[gy],"_AgonsiticActions.txt",sep=""))
  if (years[gy]==2018){meta_data$hrs.focalfollowed=meta_data$numObs} #if 2018, convert column #obs in #hrs to simplify coding after
  
  #create date of observation information: 
  data$semester = semester(data$date)
  data$quarter = quarter(data$date)
  
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
  nboot = 1000
  density = data.frame(matrix(NA, nrow=nboot, ncol=2)); names(density)=c("dens","groupyear")
  mean.weight = data.frame(matrix(NA, nrow=nboot, ncol=2)); names(mean.weight)=c("weight","groupyear")
  for (boot in 1:nboot) {
    idx = sample(seq(1:nrow(weightedEL)),size=nrow(weightedEL), replace=T)
    weightedEl.res = weightedEL[idx,] #resampled weightedEL
    
    density$dens[boot] = length(which(weightedEl.res$weight!=0))/nrow(weightedEl.res)
    mean.weight$weight[boot] = mean(weightedEl.res$weight!=0)
    
    # #Create adjacency matrix
    # adjMat = dils::AdjacencyFromEdgelist(weightedEL.res)
    # adjData = adjMat[["adjacency"]]; rownames(adjData) = adjMat[["nodelist"]]; colnames(adjData) = adjMat[["nodelist"]]
    # 
    # #read adjacency matrix
    # m=as.matrix(adjData) # coerces the data set as a matrix
    # graph=graph.adjacency(m,mode= network_mode,weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
    
    # #Get the network measures
    # NetworkMetrics = data.frame(matrix(NA, nrow = length(V(graph)), ncol = 7)); names(NetworkMetrics)=c("id","deg","INdeg","OUTdeg","between","eig.cent", "clusterCoeff")
    # NetworkMetrics$id = as_ids(V(graph))
    # #Weighted degree (Strength, undirected)
    # NetworkMetrics$deg<-degree(graph)
    # #weighted indegree
    # NetworkMetrics$INdeg <-degree(graph,v=V(graph), mode = "in", loops=F)
    # #weighted outdegree
    # NetworkMetrics$OUTdeg <-degree(graph,v=V(graph), mode = "out", loops=F)
    # #Weighted betweenness
    # NetworkMetrics$between<-betweenness(graph, v=V(graph), directed=F, normalized=T)
    # #Weighted eigenvector centrality
    # A <-eigen_centrality(graph, directed=F, scale=T)
    # eig.cen = as.data.frame(A["vector"])
    # NetworkMetrics$eig.cent = eig.cen$vector
    # #Weighted clustering coeff
    # NetworkMetrics$clusterCoeff = transitivity(graph, type = "localundirected")
    
  }
  density$groupyear = groupyears[gy]; density$year=years[gy]; density$group=group[gy]
  mean.weight$groupyear = groupyears[gy]; mean.weight$year=years[gy]; mean.weight$group=group[gy]
  density.all=rbind(density.all,density)
  mean.weight.all=rbind(mean.weight.all,mean.weight)
}

save(density.all,mean.weight.all, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AggNetMetrics.RData")
