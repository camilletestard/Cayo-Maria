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
library(reshape2)
library(netdiffuseR)
library(matrixStats)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/functions_bayesian_networks.R")

#################################################################################################
#################################################################################################

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
  
  ######################################################
  #### PART A Generate complete observation data #####
  ######################################################
  
  #Get scan data for that group & year
  rscans =  PreScans[PreScans$groupyear == groupyears[gy],]#subsampled scans for group g & year y & hurricane status h
  unqIDs = unique(c(as.character(rscans$focalID)))
  
  # #Get full network for that group & year
  # observed.net= adjacency.from.scans(rscans, unqIDs)
  # diag(observed.net) <- 0
  # observed.net.g=graph.adjacency(observed.net,mode= 'directed',weighted=T)
  # degree.observed = igraph::strength(observed.net.g,loops =F)
  # 
  # #Plot observed network
  # V(observed.net.g)$label.cex <- 0.8
  # V(observed.net.g)$sex=as.factor(dominance_info$SEX[match(V(observed.net.g)$name,dominance_info$ID)]) 
  # #set colour of sexes
  # V(observed.net.g)$color=V(observed.net.g)$sex #assign the "Sex" attribute as the vertex color
  # V(observed.net.g)$color=gsub("1","plum1",V(observed.net.g)$color) #Females will be orange
  # V(observed.net.g)$color=gsub("2","seagreen2",V(observed.net.g)$color) #Males will be lightblue
  # V(observed.net.g)$color=gsub("0","white",V(observed.net.g)$color) #unknown sex will be white
  # #Set color of vertex labels
  # color_vector=V(observed.net.g)$sex #assign the "Sex" attribute as the vertex color
  # color_vector=gsub("1","purple",color_vector) #Females will be orange
  # color_vector=gsub("2","darkblue",color_vector) #Males will be lightblue
  # color_vector=gsub("0","grey",color_vector) #unknown sex will be white
  # #set degree attribute
  # V(observed.net.g)$degree=igraph::degree(observed.net.g)
  # #Set layout
  # l <- layout.fruchterman.reingold(observed.net.g, repulserad=vcount(observed.net.g)^5, area=vcount(observed.net.g)^3)
  # #Plot
  # igraph::plot.igraph(observed.net.g, layout=l, vertex.label=V(observed.net.g)$name, vertex.color=V(observed.net.g)$color, vertex.label.color=color_vector, vertex.label.font=2,
  #                     vertex.size=2*V(observed.net.g)$degree+2,edge.color="grey20", 
  #                     edge.width=E(observed.net.g)$weight*1.5,edge.arrow.size = 0.5, edge.curved=0.5,
  #                     main = paste("Grooming Network", groupyears[gy], sep= " "))
  
  # Parameters
  # n.bootstraps <- 1000 # Number of bootstrap replicates
  # N <- ncol(observed.net)
  
  # # METHOD 1: Clopper-Pearson confidence intervals on edge weights
  # observed.network.cp.lower <- apply(abind(both.groom,either.observed,along=3),c(1,2),function(x){if(x[2]==0){return(0)} else{return(binom.test(x[1],x[2])$conf.int[1])}})
  # rownames(observed.network.cp.lower) = adjMat[["nodelist"]]; colnames(observed.network.cp.lower) = adjMat[["nodelist"]]; diag(observed.network.cp.lower) <- 0
  # observed.network.cp.lower.g=graph.adjacency(observed.network.cp.lower,mode= 'directed',weighted=T)
  # degree.cp.lower = igraph::strength(observed.network.cp.lower.g, loops =F)
  # 
  # observed.network.cp.upper <- apply(abind(both.groom,either.observed,along=3),c(1,2),function(x){if(x[2]==0){return(1)} else{return(binom.test(x[1],x[2])$conf.int[2])}})
  # rownames(observed.network.cp.upper) = adjMat[["nodelist"]]; colnames(observed.network.cp.upper) = adjMat[["nodelist"]]; diag(observed.network.cp.upper) <- 0
  # observed.network.cp.upper.g=graph.adjacency(observed.network.cp.upper,mode= 'directed',weighted=T)
  # degree.cp.upper = igraph::strength(observed.network.cp.upper.g, loops =F)
  
  
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
  
  # # METHOD #3: Bootstrapped errors
  # observed.network.boot <- array(NA,c(N,N,n.bootstraps))
  # for (j in 1:n.bootstraps) {
  #   
  #   print(paste("%%%%%%%%%%%% Boostrap",j,"%%%%%%%%%%%%%"))
  #   
  #   #Get bootstrapped network for that group & year
  #   rscans.boot <- rscans[sample(1:nrow(rscans),nrow(rscans),replace=TRUE),]
  #   
  #   #Get adjacency matrix
  #   observed.network.boot[,,j] = adjacency.from.scans(rscans.boot, unqIDs)
  #   diag(observed.network.boot[,,j]) <- 0
  #   
  # }
  # #save(observed.network.boot, file="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/full_network_boot.RData")
  # #Get 95% confidence interval for weighted degree for each ID in the network
  # observed.network.boot.upper <- apply(observed.network.boot,c(1,2),quantile,0.975,na.rm=T)
  # observed.network.boot.upper.g=graph.adjacency(observed.network.boot.upper,mode= 'directed',weighted=T)
  # degree.boot.upper = igraph::strength(observed.network.boot.upper.g,loops =F)
  # 
  # observed.network.boot.lower <- apply(observed.network.boot,c(1,2),quantile,0.025,na.rm=T)
  # observed.network.boot.lower.g=graph.adjacency(observed.network.boot.lower,mode= 'directed',weighted=T)
  # degree.boot.lower = igraph::strength(observed.network.boot.lower.g,loops =F)
  # 
  # collated.estimates.boot = data.frame(matrix(ncol=4, nrow=length(degree.observed))); names(collated.estimates.boot)=c("id","Estimate","lower","upper")
  # collated.estimates.boot$id = as.character(V(observed.net.g))
  # collated.estimates.boot$Estimate = degree.observed
  # collated.estimates.boot$lower = degree.boot.lower
  # collated.estimates.boot$upper = degree.boot.upper
  # 
  # ggplot(collated.estimates.boot, aes(x=id, y=Estimate)) + 
  #   geom_point() +
  #   geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
  #                 position=position_dodge(.9))
  # 
  # #Get distribution of mean degree and density of bootstrapped network
  # mean_degree=vector(); net.dens=vector() 
  # for (j in 1:n.bootstraps) {
  #   graph=graph.adjacency(observed.network.boot[,,j],mode= 'directed',weighted=T)
  #   mean_degree[j] = mean(igraph::degree(graph))
  #   net.dens[j] = igraph::edge_density(graph)
  # }
  # hist(net.dens); hist(mean_degree); 
  
  
  ######################################################
  #### PART B Generate subsampled observation data #####
  ######################################################
  
  # Parameters
  n.bootstraps <- 100
  subsamples <- c(seq(500,nrow(rscans),500),nrow(rscans))
  
  # Storage parameters
  N <- length(unqIDs)
  observed.network.s <- array(NA,c(N,N,length(subsamples)))
  observed.network.boot <- array(NA,c(N,N,n.bootstraps,length(subsamples)))
  observed.network.boot.upper.s <- array(NA,c(N,N,length(subsamples)))
  observed.network.boot.lower.s <- array(NA,c(N,N,length(subsamples)))
  
  # Network metrics
  observed.network.mean.degree.s <- rep(NA,length(subsamples))
  observed.network.boot.mean.degree.s <- array(NA,c(n.bootstraps,length(subsamples)))
  observed.network.boot.density.s <- array(NA,c(n.bootstraps,length(subsamples)))
  
  # Increase in increments of 20 groups observed
  observed.network.boot <- array(NA,c(N,N,n.bootstraps,length(subsamples))); i=1; j=1
  for (i in 1:length(subsamples)) {
    print(paste("%%%%%%%%%%%% Sample",i,"%%%%%%%%%%%%%"))

    
    for (j in 1:n.bootstraps) {
      #print(paste("%%%%%%%%%%%% Bootstrap",j,"%%%%%%%%%%%%%"))
      
      #Get bootstrapped network for that group & year
      rscans.boot <- rscans[sample(1:nrow(rscans),subsamples[i],replace=TRUE),]

      #Get adjacency matrix
      obj.list = adjacency.from.scans(rscans.boot, unqIDs)
      observed.network.boot[,,j,i]<- obj.list[[1]]
      
    }
    observed.network.boot.upper.s[,,i] <- apply(observed.network.boot[,,,i],c(1,2),quantile,0.975,na.rm=T)
    observed.network.boot.lower.s[,,i] <- apply(observed.network.boot[,,,i],c(1,2),quantile,0.025,na.rm=T)
    
    # Calculate degree & density
    # observed.network.mean.degree.s[i] <- mean(degree(observed.network.s[,,i],gmode="graph"))
    # observed.network.density.s[i] <-network.density(as.network(observed.network.s[,,i]))
    observed.network.boot.mean.degree.s[,i] <- colMeans(apply(observed.network.boot[,,,i],3,degree,gmode="graph"))
    observed.network.boot.density.s[,i] <- apply(observed.network.boot[,,,i],3,gden, mode="graph")
    
  }
  # save(observed.network.boot, observed.network.boot.upper.s, observed.network.boot.lower.s,
  #      observed.network.boot.mean.degree.s, observed.network.boot.density.s,
  #      file = paste("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/BootstrapResults.", groupyears[gy], ".RData",sep=""))
  # 
  # load(paste("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/BootstrapResults.", groupyears[gy], ".RData",sep=""))
  
  par(mfrow=c(1,1))
  long.mean.degree.s = melt(observed.network.boot.mean.degree.s); 
  long.mean.degree.s$Var2 = subsamples[long.mean.degree.s$Var2]
  long.mean.degree.s$Var2 = as.factor(long.mean.degree.s$Var2)
  meandeg.p<-ggplot(long.mean.degree.s, aes(x=Var2, y=value))+
    geom_point(alpha=0.2)+ 
    ylab("Mean degree")+ xlab("# Observations")+ 
    labs(title = paste("Mean degree for increasing #observations,",groupyears[gy]))+
    geom_violin(alpha=0.2, fill='blue')+ theme_classic(base_size =20)+
    geom_boxplot(alpha=0.5, outlier.shape = NA, width=0.2)+
    theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8)) 
  ggsave(paste('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SamplingEffort/meanDegree_Boot_', groupyears[gy], ".png",sep=""), plot=meandeg.p)
  
  long.density.s = melt(observed.network.boot.density.s); 
  long.density.s$Var2 = subsamples[long.density.s$Var2]
  long.density.s$Var2 = as.factor(long.density.s$Var2)
  meandens.p<-ggplot(long.density.s, aes(x=Var2, y=value))+
    geom_point(alpha=0.2)+ 
    ylab("Density")+ xlab("# Observations")+
    labs(title = paste("Density for increasing #observations,",groupyears[gy]))+
    geom_violin(alpha=0.2, fill='orange')+ theme_classic(base_size =20)+
    geom_boxplot(alpha=0.5, outlier.shape = NA, width=0.2)+
    theme(axis.text.x=element_text(color = "black", size=11, angle=30, vjust=.8, hjust=0.8))
  ggsave(paste('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SamplingEffort/meanDens_Boot_', groupyears[gy], ".png",sep=""), plot=meandens.p)
  
  # #How do the estimates of 15 random dyads change with sample size
  # dyads <- cbind(c(1,1,1,11,11,6,6,6,8,8,8,4,32,4,5),c(9,12,20,8,36,41,18,57,28,42,5,62,34,6,6))
  # 
  # par(mfrow=c(3,5),mar=c(4,4,2.5,1),mgp=c(2.5,0.8,0),oma=c(3,3,0,0))
  # for (i in 1:nrow(dyads)) {
  #   plot(NULL,type="l",ylim=c(0,1),xlim=c(0,6000),xlab="",ylab="",cex.lab=1.6,axes=FALSE,xaxs="i")
  #   axis(2,cex.axis=1.4)
  #   axis(1,at=subsamples,cex.axis=1.4)
  #   box()
  #   lines(subsamples,observed.network.boot.lower.s[dyads[i,1],dyads[i,2],],col="blue")
  #   lines(subsamples,observed.network.boot.upper.s[dyads[i,1],dyads[i,2],],col="blue")
  #   lines(subsamples,observed.network.s[dyads[i,1],dyads[i,2],],col="black",lwd=2)
  # }
  # ggsave(paste('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SamplingEffort/edgeweight_Boot_', groupyears[gy], ".png",sep=""))
  # 
}