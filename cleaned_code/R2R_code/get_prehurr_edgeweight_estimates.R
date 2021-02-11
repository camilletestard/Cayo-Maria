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
library(sna)
library(asnipe)
library(vegan)
library(MASS)
library(bbmle)
library(binom)
library(abind)
library(ggplot2)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/functions_GlobalNetworkMetrics.R")

## FUNCTIONS
# Computes the log likelihood of a given combination of parameters {a,b} given the data in vectors si and di
#Inputs:
#	a, b: parameters of the beta distribution (to be used as the prior)
#	si: number of observations for each dyad (vector)
#	di: number of positive observations for each dyad (vector) - i.e. when they were observed together
#Output:
#	likeli <- log likelihood P(di | a, b, si )
#		given by SUM_i log (si choose di) * Z(a+di,b+si-di) / Z(a,b) ]
#		where Z(a,b) is the beta function
ml2.likeli <- function(a,b,si,di){
  s.choose.d <- apply(cbind(si,di),1,function(x){return(choose(x[1],x[2]))})
  beta1 <- apply(cbind(si,di),1,function(x){return(beta(a+x[2],b+x[1]-x[2]))})
  likeli <- sum(log(s.choose.d) + log(beta1) - log(rep(beta(a,b),length(si))))
  return(likeli) 
}

#Load scan data and population info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/") 
allScans = read.csv("Data All Cleaned/allScans.txt")
dominance_info =read.table("Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Get pre-hurricane scans
PrePostScans =  allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
SubScans=PrePostScans[-which(PrePostScans$year==2019),] #remove 2019
PreScans = SubScans[which(SubScans$isPost==0),]
PreScans$groupyear = paste(PreScans$group, PreScans$year,sep="")

#For each group, each year separately: 
groupyears = c("V2015", "V2016", "V2017","KK2015", "KK2017"); gy=1
for (gy in 1:length(groupyears)){
  
  
  #### PART A Generate complete observation data #####
  
  #Get full network for that group & year
  rscans =  PreScans[PreScans$groupyear == groupyears[gy],]#subsampled scans for group g & year y & hurricane status h
  
  #Find the number of scans per individual (to correct for unequal representation later)
  numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
  
  #Find the number of unique IDs.
  unqIDs = unique(c(as.character(rscans$focalID)))
  
  #Get Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL_groom(unqIDs)
  
  #Get count of grooming occurrences
  options(warn = -1) #set options to ignore all warnings
  weightedEL = calcEdgeList_groom(rscans,masterEL)
  weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
  weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
  weightedEL = weightedEL[,c("alter","ego","count")] #transform output to equal proximity data output.
  countEL = weightedEL
  
  #set num scans between each pair to be the average of the number of scans of the two individuals
  weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])
  weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by the #observed scans
  # Standardize weights by dividing by the mean weight. Thus we plot relative weights, which take into account group/year differences.
  meanweight = mean(weightedEL$weight[weightedEL$weight>0]) #compute nonzero mean weight
  weightedEL$weight <- weightedEL$weight/meanweight
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$numscans<-NULL #delete
  
  #Get number of scans either individual was observed (sum of scans)
  numscansEL = countEL; numscansEL$count <-NULL
  numscansEL$numscans <- round((numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)]))
  
  #Get observed grooming network graph
  adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
  observed.net = adjMat[["adjacency"]]; diag(observed.net) <- 0
  rownames(observed.net) = adjMat[["nodelist"]]; colnames(observed.net) = adjMat[["nodelist"]]
  observed.net.g=graph.adjacency(observed.net,mode= 'directed',weighted=T)
  degree.observed = igraph::strength(observed.net.g,loops =F)
  
  #Plot observed network
  V(observed.net.g)$label.cex <- 0.8
  V(observed.net.g)$sex=as.factor(dominance_info$SEX[match(V(observed.net.g)$name,dominance_info$ID)]) 
  #set colour of sexes
  V(observed.net.g)$color=V(observed.net.g)$sex #assign the "Sex" attribute as the vertex color
  V(observed.net.g)$color=gsub("1","plum1",V(observed.net.g)$color) #Females will be orange
  V(observed.net.g)$color=gsub("2","seagreen2",V(observed.net.g)$color) #Males will be lightblue
  V(observed.net.g)$color=gsub("0","white",V(observed.net.g)$color) #unknown sex will be white
  #Set color of vertex labels
  color_vector=V(observed.net.g)$sex #assign the "Sex" attribute as the vertex color
  color_vector=gsub("1","purple",color_vector) #Females will be orange
  color_vector=gsub("2","darkblue",color_vector) #Males will be lightblue
  color_vector=gsub("0","grey",color_vector) #unknown sex will be white
  #set degree attribute
  V(observed.net.g)$degree=igraph::degree(observed.net.g)
  #Set layout
  l <- layout.fruchterman.reingold(observed.net.g, repulserad=vcount(observed.net.g)^5, area=vcount(observed.net.g)^3)
  #Plot
  igraph::plot.igraph(observed.net.g, layout=l, vertex.label=V(observed.net.g)$name, vertex.color=V(observed.net.g)$color, vertex.label.color=color_vector, vertex.label.font=2,
              vertex.size=2*V(observed.net.g)$degree+2,edge.color="grey20", 
              edge.width=E(observed.net.g)$weight*1.5,edge.arrow.size = 0.5, edge.curved=0.5,
              main = paste("Grooming Network", groupyears[gy], sep= " "))
  
  #Get both groom and either observed matrices
  adjMat2 = dils::AdjacencyFromEdgelist(countEL)# create adjacency matrix based on edge list.
  both.groom = adjMat2[["adjacency"]]; diag(both.groom) <- 0
  
  adjMat3 = dils::AdjacencyFromEdgelist(numscansEL)# create adjacency matrix based on edge list.
  either.observed = adjMat3[["adjacency"]];
  
  # Parameters
  n.samples <- 1000 # Number of networks to generate from Bayesian intervals
  n.bootstraps <- 1000 # Number of bootstrap replicates
  N <- ncol(observed.net)
  
  # METHOD 1: Clopper-Pearson confidence intervals on edge weights
  observed.network.cp.lower <- apply(abind(both.groom,either.observed,along=3),c(1,2),function(x){if(x[2]==0){return(0)} else{return(binom.test(x[1],x[2])$conf.int[1])}})
  rownames(observed.network.cp.lower) = adjMat[["nodelist"]]; colnames(observed.network.cp.lower) = adjMat[["nodelist"]]; diag(observed.network.cp.lower) <- 0
  observed.network.cp.lower.g=graph.adjacency(observed.network.cp.lower,mode= 'directed',weighted=T)
  degree.cp.lower = igraph::strength(observed.network.cp.lower.g, loops =F)
  
  observed.network.cp.upper <- apply(abind(both.groom,either.observed,along=3),c(1,2),function(x){if(x[2]==0){return(1)} else{return(binom.test(x[1],x[2])$conf.int[2])}})
  rownames(observed.network.cp.upper) = adjMat[["nodelist"]]; colnames(observed.network.cp.upper) = adjMat[["nodelist"]]; diag(observed.network.cp.upper) <- 0
  observed.network.cp.upper.g=graph.adjacency(observed.network.cp.upper,mode= 'directed',weighted=T)
  degree.cp.upper = igraph::strength(observed.network.cp.upper.g, loops =F)
  
   
  
  # # METHOD 2: Bayesian estimate
  # # Get prior
  # si <- either.observed[upper.tri(either.observed)]
  # di <- both.groom[upper.tri(both.groom)]
  # prior <- optim(par=c(1,1),fn=function(x){return(-ml2.likeli(x[1],x[2],si,di))})
  # 
  # # Storage parameters
  # observed.network.bayes.lower <- matrix(NA,nrow=N,ncol=N)
  # observed.network.bayes.upper <- matrix(NA,nrow=N,ncol=N)
  # observed.network.bayes <- matrix(NA,nrow=N,ncol=N)
  # observed.network.bayes.d <- array(NA,dim=c(N,N,n.samples))
  # 
  # # Loop through each dyad
  # ij.pairs <- cbind(rep(1:N,each=N),rep(1:N))
  # ij.pairs <- ij.pairs[which(ij.pairs[,1]<ij.pairs[,2]),]
  # for (j in 1:nrow(ij.pairs)) {
  #   out <- binom.bayes(both.groom[cbind(ij.pairs[j,1],ij.pairs[j,2])],either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])],conf.level=0.95,type="central",prior.shape1=prior$par[1],prior.shape2=prior$par[2])
  #   observed.network.bayes.lower[ij.pairs[j,1],ij.pairs[j,2]] <- out$lower
  #   observed.network.bayes.upper[ij.pairs[j,1],ij.pairs[j,2]] <- out$upper
  #   observed.network.bayes.lower[ij.pairs[j,2],ij.pairs[j,1]] <- out$lower
  #   observed.network.bayes.upper[ij.pairs[j,2],ij.pairs[j,1]] <- out$upper
  #   out <- binom.bayes(both.groom[cbind(ij.pairs[j,1],ij.pairs[j,2])],either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])],conf.level=0.00001,type="highest",prior.shape1=prior$par[1],prior.shape2=prior$par[2])
  #   observed.network.bayes[ij.pairs[j,1],ij.pairs[j,2]] <- mean(c(out$upper,out$lower))
  #   observed.network.bayes[ij.pairs[j,2],ij.pairs[j,1]] <- mean(c(out$upper,out$lower))
  # 
  #   # sample from edge weight distributions to generate boostrapped networks (from Bayesian method)
  #   tmp <- rbeta(n.samples,out$shape1,out$shape2)
  #   observed.network.bayes.d[ij.pairs[j,1],ij.pairs[j,2],] <- tmp
  #   observed.network.bayes.d[ij.pairs[j,2],ij.pairs[j,1],] <- tmp
  # }
  
  # METHOD #3: Bootstrapped errors
  observed.network.boot <- array(NA,c(N,N,n.bootstraps))
  for (j in 1:n.bootstraps) {
    
    print(paste("%%%%%%%%%%%% Boostrap",j,"%%%%%%%%%%%%%"))
    
    #Get bootstrapped network for that group & year
    rscans.boot <- rscans[sample(1:nrow(rscans),nrow(rscans),replace=TRUE),]
    
    #Find the number of scans per individual (to correct for unequal representation later)
    numscans = as.data.frame(table(as.character(rscans.boot$focalID))); names(numscans) =c("id","freq")
    
    #Get Edgelist of all possible pairs given the unique IDs.
    masterEL = calcMasterEL_groom(unqIDs)
    
    #Get count of grooming occurrences
    options(warn = -1) #set options to ignore all warnings
    weightedEL = calcEdgeList_groom(rscans.boot,masterEL)
    weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
    weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
    weightedEL = weightedEL[,c("alter","ego","count")] #transform output to equal proximity data output.

    #Get weights (divided by number of scans)
    weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2
    weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by the #observed scans
    weightedEL$weight[is.na(weightedEL$weight)]=0 #If individual is absent (NA), just set his weights to 0
    
    # Standardize weights by dividing by the mean weight. Thus we plot relative weights, which take into account group/year differences.
    meanweight = mean(weightedEL$weight[weightedEL$weight>0]) #compute nonzero mean weight
    weightedEL$weight <- weightedEL$weight/meanweight
    weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$numscans<-NULL
    
    #Get adjacency matrix
    adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
    observed.network.boot[,,j] = adjMat[["adjacency"]]
    diag(observed.network.boot[,,j]) <- 0

  }
  #Get 95% confidence interval for weighted degree for each ID in the network
  observed.network.boot.upper <- apply(observed.network.boot,c(1,2),quantile,0.975,na.rm=T)
  rownames(observed.network.boot.upper) = adjMat[["nodelist"]]; colnames(observed.network.boot.upper) = adjMat[["nodelist"]]; diag(observed.network.boot.upper) <- 0
  observed.network.boot.upper.g=graph.adjacency(observed.network.boot.upper,mode= 'directed',weighted=T)
  degree.boot.upper = igraph::strength(observed.network.boot.upper.g,loops =F)
  
  observed.network.boot.lower <- apply(observed.network.boot,c(1,2),quantile,0.025,na.rm=T)
  rownames(observed.network.boot.lower) = adjMat[["nodelist"]]; colnames(observed.network.boot.lower) = adjMat[["nodelist"]]; diag(observed.network.boot.lower) <- 0
  observed.network.boot.lower.g=graph.adjacency(observed.network.boot.lower,mode= 'directed',weighted=T)
  degree.boot.lower = igraph::strength(observed.network.boot.lower.g,loops =F)
  
  collated.estimates.boot = data.frame(matrix(ncol=4, nrow=length(degree.observed))); names(collated.estimates.boot)=c("id","Estimate","lower","upper")
  collated.estimates.boot$id = adjMat[["nodelist"]]
  collated.estimates.boot$Estimate = degree.observed
  collated.estimates.boot$lower = degree.boot.lower
  collated.estimates.boot$upper = degree.boot.upper
  
  p<- ggplot(collated.estimates.boot, aes(x=id, y=Estimate)) + 
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                  position=position_dodge(.9))
  
  #Get distribution of density in the network
  mean_degree=vector(); mean_wdegree=vector(); net.dens=vector() 
  for (j in 1:n.bootstraps) {
    graph=graph.adjacency(observed.network.boot[,,j],mode= 'directed',weighted=T)
    mean_degree[j] = mean(igraph::degree(graph))
    net.dens[j] = igraph::edge_density(graph)
  }
  hist(net.dens); hist(mean_degree); hist(mean_wdegree)
  
}


