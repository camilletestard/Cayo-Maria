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
library(data.table)

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
groupyears = c("V2015", "V2016", "V2017","KK2015", "KK2017"); gy=1; 
bayes.estimates.wdegree.full=list(); boot.estimates.wdegree.full=list()
bayes.estimates.edgew.full=list(); 

for (gy in 1:length(groupyears)){
  
  #Get full raw data for that group & year
  rscans =  PreScans[PreScans$groupyear == groupyears[gy],]#subsampled scans for group g & year
  unqIDs = unique(c(as.character(rscans$focalID)))
  
  
  #### PART A Generate complete observation data #####
  setwd('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/GroomNetworks/Sampling_R2R')
  
  # Parameters
  n.samples <- 500 # Number of networks to generate from Bayesian intervals
  n.bootstraps <- 500 # Number of bootstrap replicates

  # Collate information on observations of dyads
  N <- length(unqIDs)
  obj.list = adjacency.from.scans(rscans, unqIDs)
  observed.network<- obj.list[[1]];
  both.seen <- obj.list[[2]]; both.seen[is.na(both.seen)]=0; diag(both.seen)=0
  either.observed <- obj.list[[3]]; either.observed[is.na(either.observed)]=0; diag(either.observed)=0

  obs.graph = graph.adjacency(observed.network,mode= 'undirected',weighted=T)
  degree.observed= igraph::strength(obs.graph,loops =F)
  
  nonzero_dyads = which(observed.network!=0)
  zero_dyads = which(observed.network==0)

  #METHOD #2: Bayesian estimate

  # Get prior
  si <- either.observed[upper.tri(either.observed)]
  di <- both.seen[upper.tri(both.seen)]
  prior <- optim(par=c(1,1),fn=function(x){return(-ml2.likeli(x[1],x[2],si,di))})
  #prior$par[1]<-1/3; prior$par[2]<-1/3

  # Storage parameters
  observed.network.bayes.lower <- matrix(NA,nrow=N,ncol=N)
  observed.network.bayes.upper <- matrix(NA,nrow=N,ncol=N)
  observed.network.bayes <- matrix(NA,nrow=N,ncol=N)
  observed.network.bayes.d <- array(NA,dim=c(N,N,n.samples))

  # Loop through each dyad
  ij.pairs <- cbind(rep(1:N,each=N),rep(1:N))
  ij.pairs <- ij.pairs[which(ij.pairs[,1]<ij.pairs[,2]),]
  for (j in 1:nrow(ij.pairs)) {
    out <- binom.bayes(both.seen[cbind(ij.pairs[j,1],ij.pairs[j,2])],either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])],conf.level=0.95,type="central",prior.shape1=prior$par[1],prior.shape2=prior$par[2])
    observed.network.bayes.lower[ij.pairs[j,1],ij.pairs[j,2]] <- out$lower
    observed.network.bayes.upper[ij.pairs[j,1],ij.pairs[j,2]] <- out$upper
    observed.network.bayes.lower[ij.pairs[j,2],ij.pairs[j,1]] <- out$lower
    observed.network.bayes.upper[ij.pairs[j,2],ij.pairs[j,1]] <- out$upper

    out <- binom.bayes(both.seen[cbind(ij.pairs[j,1],ij.pairs[j,2])],either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])],conf.level=0.00001,type="highest",prior.shape1=prior$par[1],prior.shape2=prior$par[2])
    observed.network.bayes[ij.pairs[j,1],ij.pairs[j,2]] <- mean(c(out$upper,out$lower))
    observed.network.bayes[ij.pairs[j,2],ij.pairs[j,1]] <- mean(c(out$upper,out$lower))

    # sample from edge weight distributions to generate boostrapped networks (from Bayesian method)
    tmp <- rbeta(n.samples,out$shape1,out$shape2)
    observed.network.bayes.d[ij.pairs[j,1],ij.pairs[j,2],] <- tmp
    observed.network.bayes.d[ij.pairs[j,2],ij.pairs[j,1],] <- tmp
    # observed.network.bayes.d[ij.pairs[j,1],ij.pairs[j,2],] <- round(tmp*either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])])
    # observed.network.bayes.d[ij.pairs[j,2],ij.pairs[j,1],] <- round(tmp*either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])])
  }

  observed.network.bayes.sd <- apply(observed.network.bayes.d,c(1,2),sd,na.rm=T)#/sqrt(n.samples)
  observed.network.bayes.mean <- apply(observed.network.bayes.d,c(1,2),mean,na.rm=T)
  observed.network.bayes.cv <- observed.network.bayes.sd/observed.network.bayes.mean
  
  #Get distribution of weighted degree for each pair by sampling through simulated datasets
  degree.allIDs <- array(NA,c(N,n.samples)); rownames(degree.allIDs)=V(obs.graph)$name
  for (s in 1:n.samples) {
    #Get degree per individual
    graph=graph.adjacency(observed.network.bayes.d[,,s],mode= 'undirected',weighted=T)
    degree.allIDs[,s] = igraph::strength(graph,loops =F)
  }

  # a <- observed.network.bayes.upper-observed.network.bayes.lower
  # #par(mar=c(4,4,2.5,1),mgp=c(2.5,0.8,0),xaxs="i",yaxs="i")
  # hist(a,breaks=200,col="black",main="",xlab="uncertainty range",ylab="frequency", cex.lab=1.6, cex.axis=1.4,ylim=c(0,4000),xlim=c(-0.005,1))
  # box()

  observed.network.bayes.upper.g=graph.adjacency(observed.network.bayes.upper,mode= 'undirected',weighted=T)
  degree.bayes.upper = igraph::strength(observed.network.bayes.upper.g,loops =F)

  observed.network.bayes.lower.g=graph.adjacency(observed.network.bayes.lower,mode= 'undirected',weighted=T)
  degree.bayes.lower = igraph::strength(observed.network.bayes.lower.g,loops =F)

  sd.degree = rowSds(degree.allIDs)
  mean.degree = rowMeans(degree.allIDs)
  cv.degree = sd.degree/mean.degree

  collated.estimates.bayes = data.frame(matrix(ncol=4, nrow=length(degree.observed))); names(collated.estimates.bayes)=c("id","Estimate","lower","upper")
  collated.estimates.bayes$id = V(obs.graph)$name
  collated.estimates.bayes$Estimate = mean.degree#degree.observed
  collated.estimates.bayes$sd = sd.degree; collated.estimates.bayes$sem = sd.degree/sqrt(length(degree.observed));
  collated.estimates.bayes$cv = cv.degree
  collated.estimates.bayes$ymin = collated.estimates.bayes$Estimate-collated.estimates.bayes$sd; collated.estimates.bayes$ymin[collated.estimates.bayes$ymin<0]=0
  collated.estimates.bayes$lower = degree.bayes.lower
  collated.estimates.bayes$upper = degree.bayes.upper

  bayes<-ggplot(collated.estimates.bayes, aes(x=reorder(id, Estimate), y=Estimate)) +
    geom_point(size=2) + theme_classic(base_size = 15)+ #ylim(0,0.3)+
    geom_errorbar(aes(ymin= ymin, ymax=Estimate+sd), width=.2,
                  position=position_dodge(.9), color="blue", size=1.25)+
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                  position=position_dodge(.9), color="red")+
    theme(axis.text.x = element_text(angle = 45, hjust=1.1))+
    xlab('ID')+ylab('Weighted Degree')+ggtitle(paste("Bayesian Uncertainty Estimate", groupyears[gy],sep=""))
  ggsave(paste("Bayesian_Uncertainty_", groupyears[gy], ".tiff",sep=""),bayes)
  
  bayes.estimates.wdegree.full[[groupyears[gy]]] = collated.estimates.bayes
  bayes.estimates.edgew.full[[groupyears[gy]]] = list(observed.network.bayes.sd, observed.network.bayes.mean, observed.network.bayes.cv, zero_dyads, nonzero_dyads)

  # ########################################################
  # #METHOD #3: Bootstrapped errors
  # observed.network.boot <- array(NA,c(N,N,n.bootstraps))
  # degree.allIDs <- array(NA,c(N,n.bootstraps)); rownames(degree.allIDs)=V(obs.graph)$name
  # mean_degree=vector(); net.dens=vector()
  # for (j in 1:n.bootstraps) {
  # 
  #   #print(paste("%%%%%%%%%%%% Boostrap",j,"%%%%%%%%%%%%%"))
  # 
  #   #Get bootstrapped network for that group & year
  #   rscans.boot <- rscans[sample(1:nrow(rscans),nrow(rscans),replace=TRUE),]
  # 
  #   #Get adjacency matrix
  #   obj.list = adjacency.from.scans(rscans.boot, unqIDs)
  #   observed.network.boot[,,j]<- obj.list[[1]]
  # 
  #   #Get degree per individual
  #   graph=graph.adjacency(observed.network.boot[,,j],mode= 'undirected',weighted=T)
  #   degree.allIDs[,j] = igraph::strength(graph,loops =F)
  #   mean_degree[j] = mean(igraph::degree(graph))
  #   net.dens[j] = igraph::edge_density(graph)
  # }
  # 
  # observed.network.boot.sd <- apply(observed.network.boot,c(1,2),sd,na.rm=T)#/sqrt(n.bootstraps)
  # 
  # observed.network.boot.upper <- apply(observed.network.boot,c(1,2),quantile,0.975,na.rm=T)
  # observed.network.boot.upper.g=graph.adjacency(observed.network.boot.upper,mode= 'undirected',weighted=T)
  # degree.boot.upper = igraph::strength(observed.network.boot.upper.g,loops =F)
  # 
  # observed.network.boot.lower <- apply(observed.network.boot,c(1,2),quantile,0.025,na.rm=T)
  # observed.network.boot.lower.g=graph.adjacency(observed.network.boot.lower,mode= 'undirected',weighted=T)
  # degree.boot.lower = igraph::strength(observed.network.boot.lower.g,loops =F)
  # 
  # sd.degree = rowSds(degree.allIDs)
  # 
  # collated.estimates.boot = data.frame(matrix(ncol=4, nrow=length(degree.observed))); names(collated.estimates.boot)=c("id","Estimate","lower","upper")
  # collated.estimates.boot$id = V(obs.graph)$name
  # collated.estimates.boot$Estimate = degree.observed
  # collated.estimates.boot$sd = sd.degree; collated.estimates.boot$sem = sd.degree/sqrt(length(degree.observed));
  # collated.estimates.boot$ymin = degree.observed-sd.degree; collated.estimates.boot$ymin[collated.estimates.bayes$ymin<0]=0
  # collated.estimates.boot$lower = degree.boot.lower
  # collated.estimates.boot$upper = degree.boot.upper
  # 
  # boot.estimates.wdegree.full[[groupyears[gy]]] = collated.estimates.boot
  # boot.estimates.edgew.full[[groupyears[gy]]] = observed.network.boot.sd

  # ggplot(collated.estimates.boot, aes(x=reorder(id, Estimate), y=Estimate)) +
  #   geom_point() + theme_classic(base_size = 15)+
  #   geom_errorbar(aes(ymin= ymin, ymax=Estimate+sd), width=.2,
  #                 position=position_dodge(.9), color="blue", size=1.25)+
  #   geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
  #                 position=position_dodge(.9), color="red")+
  #   theme(axis.text.x = element_text(angle = 45, hjust=1.1))+
  #   xlab('ID')+ylab('Weighted Degree')+ggtitle(paste("Bootstrapped Uncertainty Estimate", groupyears[gy],sep=""))
  # ggsave(paste("Bootstrapping_Uncertainty_", groupyears[gy], ".tiff",paste=""),bayes)
  # 
  # #Get distribution of mean degree and density of bootstrapped network
  # mean_degree=vector(); net.dens=vector()
  # for (j in 1:n.bootstraps) {
  #   graph=graph.adjacency(observed.network.boot[,,j],mode= 'directed',weighted=T)
  #   mean_degree[j] = mean(igraph::degree(graph))
  #   net.dens[j] = igraph::edge_density(graph)
  # }
  # 
  # tiff(paste("Boot_density_meandeg_", groupyears[gy], ".tiff",paste=""))
  # par(mfrow=c(1,2))
  # hist(net.dens, xlab = 'network density'); hist(mean_degree, xlab = 'mean degree');
  # dev.off()
  # 
  # a <- observed.network.boot.upper-observed.network.boot.lower
  # #par(mar=c(4,4,2.5,1),mgp=c(2.5,0.8,0),xaxs="i",yaxs="i")
  # hist(a,breaks=200,col="black",main="",xlab="uncertainty range",ylab="frequency", cex.lab=1.6, cex.axis=1.4,ylim=c(0,4000),xlim=c(-0.005,1))
  # median(a);
  # box()
  
  
  ######################################################
  #### PART B Generate subsampled observation data #####
  ######################################################

  # # Permute rscans
  # rscans<-rscans[sample(1:nrow(rscans),nrow(rscans),replace=F),]
  # 
  # # Parameters
  # n.samples <- 100
  # n.bootstraps <- 100
  # subsamples <- c(seq(500,nrow(rscans),500),nrow(rscans))
  # 
  # # Storage parameters
  # N <- length(unqIDs)
  # observed.network.s <- array(NA,c(N,N,length(subsamples)))
  # observed.network.bayes.s <- array(NA,c(N,N,length(subsamples)))
  # observed.network.bayes.upper.s <- array(NA,c(N,N,length(subsamples)))
  # observed.network.bayes.lower.s <- array(NA,c(N,N,length(subsamples)))
  # 
  # # Network metrics
  # observed.network.mean.degree.s <- rep(NA,length(subsamples))
  # observed.network.bayes.mean.degree.s <- array(NA,c(n.samples,length(subsamples)))
  # observed.network.density.s <- rep(NA,length(subsamples))
  # observed.network.bayes.density.s <- array(NA,c(n.samples,length(subsamples)))
  # 
  #   # Increase in increments of 200 observations
  # i=1
  # for (i in 1:length(subsamples)) {
  #   print(paste("%%%%%%%%%%%% Sample",i,"%%%%%%%%%%%%%"))
  # 
  #   # Subsample observations
  #   rscans.s <- rscans[1:subsamples[i],]
  # 
  #   # Generate subsampled network
  #   obj.list = adjacency.from.scans(rscans.s, unqIDs)
  #   observed.network.s[,,i] <- obj.list[[1]];
  #   both.seen <- obj.list[[2]]; both.seen[is.na(both.seen)]=0; diag(both.seen)=0
  #   either.observed <- obj.list[[3]]; either.observed[is.na(either.observed)]=0; diag(either.observed)=0
  # 
  #   # Bayesian estimate
  # 
  #   # Get prior
  #   si <- either.observed[lower.tri(either.observed)]
  #   di <- both.seen[lower.tri(both.seen)]
  #   prior <- optim(par=c(1,1),fn=function(x){return(-ml2.likeli(x[1],x[2],si,di))})
  # 
  #   # Storage parameters
  #   observed.network.bayes.d <- array(NA,dim=c(N,N,n.samples))
  # 
  #   # Loop through each dyad
  #   ij.pairs <- cbind(rep(1:N,each=N),rep(1:N))
  #   ij.pairs <- ij.pairs[which(ij.pairs[,1]<ij.pairs[,2]),]
  #   j=8
  #   for (j in 1:nrow(ij.pairs)) {
  #     out <- binom.bayes(both.seen[cbind(ij.pairs[j,1],ij.pairs[j,2])],either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])],conf.level=0.95,type="central",prior.shape1=prior$par[1],prior.shape2=prior$par[2])
  #     observed.network.bayes.lower.s[ij.pairs[j,1],ij.pairs[j,2],i] <- out$lower
  #     observed.network.bayes.upper.s[ij.pairs[j,1],ij.pairs[j,2],i] <- out$upper
  #     observed.network.bayes.lower.s[ij.pairs[j,2],ij.pairs[j,1],i] <- out$lower
  #     observed.network.bayes.upper.s[ij.pairs[j,2],ij.pairs[j,1],i] <- out$upper
  #     out <- binom.bayes(both.seen[cbind(ij.pairs[j,1],ij.pairs[j,2])],either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])],conf.level=0.00001,type="highest",prior.shape1=prior$par[1],prior.shape2=prior$par[2])
  #     observed.network.bayes.s[ij.pairs[j,1],ij.pairs[j,2],i] <- mean(c(out$upper,out$lower))
  #     observed.network.bayes.s[ij.pairs[j,2],ij.pairs[j,1],i] <- mean(c(out$upper,out$lower))
  # 
  #     # sample from edge weight distributions to generate boostrapped networks (from Bayesian method)
  #     tmp <- rbeta(n.samples,out$shape1,out$shape2)
  #     observed.network.bayes.d[ij.pairs[j,1],ij.pairs[j,2],] <- tmp
  #     observed.network.bayes.d[ij.pairs[j,2],ij.pairs[j,1],] <- tmp
  #     # observed.network.bayes.d[ij.pairs[j,1],ij.pairs[j,2],] <- floor(tmp)*either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])]
  #     # observed.network.bayes.d[ij.pairs[j,2],ij.pairs[j,1],] <- floor(tmp)*either.observed[cbind(ij.pairs[j,1],ij.pairs[j,2])]
  #   }
  # 
  #   # Calculate degree and density for increasing numbers
  #   observed.network.mean.degree.s[i] <- mean(degree(observed.network.s[,,i],gmode="graph"))
  #   observed.network.density.s[i] <-network.density(as.network(observed.network.s[,,i]))
  #   observed.network.bayes.mean.degree.s[,i] <- colMeans(apply(observed.network.bayes.d,3,degree,gmode="graph"))
  #   observed.network.bayes.density.s[,i] <- apply(observed.network.bayes.d,3,gden, mode="graph")
  # }
  # 
  # tiff(paste("Bayes_meandeg_", groupyears[gy], ".tiff",paste=""))
  # par(mfrow=c(1,1))
  # plot(subsamples,observed.network.mean.degree.s, xlab="total number of observations", ylab="mean degree",
  #      type="l", lwd=2.5,col='black', cex.lab=1.6, cex.axis=1.4, ylim=c(0.01, 0.06))
  # a <- apply(observed.network.bayes.mean.degree.s,2,quantile,c(0.025,0.5,0.975))
  # polygon(c(subsamples,rev(subsamples)),c(a[3,],rev(a[1,])),col='#FF000044',border=NA)
  # lines(subsamples,a[2,],lwd=2.5,col='red')
  # lines(subsamples,a[1,],lwd=2.5,col='grey')
  # lines(subsamples,a[3,],lwd=2.5,col='grey')
  # dev.off()
  
  # par(mfrow=c(1,1))
  # plot(subsamples,observed.network.density.s, xlab="total number of observations", ylab="density",
  #      type="l", lwd=2.5,col='black', cex.lab=1.6, cex.axis=1.4, ylim=c(0.01, 0.06))
  # a <- apply(observed.network.bayes.density.s,2,quantile,c(0.025,0.5,0.975))
  # polygon(c(subsamples,rev(subsamples)),c(a[3,],rev(a[1,])),col='#FF000044',border=NA)
  # lines(subsamples,a[2,],lwd=2.5,col='red')
  # lines(subsamples,a[1,],lwd=2.5,col='grey')
  # lines(subsamples,a[3,],lwd=2.5,col='grey')
  
  # par(mfrow=c(1,1))
  # a <- observed.network.bayes.upper.s[,,length(subsamples)]-observed.network.bayes.lower.s[,,length(subsamples)]
  # hist(a,breaks=500,col="red", border="red",main="",xlab="uncertainty range",ylab="frequency", cex.lab=1.6, cex.axis=1.4,ylim=c(0,2000),xlim=c(0,0.1))
  # max(a, na.rm=T)
  # box()

  # #How do the estimates of 15 random dyads change with sample size
  # dyads <- cbind(c(1,1,1,11,11,6,6,6,8,8,8,4,32,4,5),c(9,12,20,8,36,41,18,57,28,42,5,62,34,6,6))
  # 
  # par(mfrow=c(3,5),mar=c(4,4,2.5,1),mgp=c(2.5,0.8,0),oma=c(3,3,0,0))
  # for (dy in 1:nrow(dyads)) {
  #   plot(NULL,type="l",ylim=c(0,0.2),xlim=c(200,nrow(rscans)),xlab="#obs",ylab="edge weight",cex.lab=1.6,axes=FALSE,xaxs="i")
  #   axis(2,cex.axis=1.4)
  #   axis(1,at=subsamples,cex.axis=1.4)
  #   box()
  #   lines(subsamples,observed.network.bayes.lower.s[dyads[dy,1],dyads[dy,2],],col="red",lwd=1.2)
  #   lines(subsamples,observed.network.bayes.upper.s[dyads[dy,1],dyads[dy,2],],col="red",lwd=1.2)
  #   lines(subsamples,observed.network.s[dyads[dy,1],dyads[dy,2],],col="black",lwd=2)
  # }
  # ggsave(paste('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SamplingEffort/edgeweight_Boot_', groupyears[gy], ".png",sep=""))


}

wdegree.bayes = rbindlist(bayes.estimates.wdegree.full); 
mean(wdegree.bayes$sd); mean(wdegree.bayes$cv); 
#wdegree.boot = rbindlist(boot.estimates.wdegree.full); mean(wdegree.boot$sd)

full.coeff.var.nonzero=vector();full.std.dev.nonzero=vector(); full.std.dev.zero=vector() ;gy=1
for (gy in 1:length(groupyears)){
  nonzero = bayes.estimates.edgew.full[[groupyears[gy]]][[5]]
  zero = bayes.estimates.edgew.full[[groupyears[gy]]][[4]]
  
  coeff.var = bayes.estimates.edgew.full[[groupyears[gy]]][[3]]
  coeff.var.nonzero = coeff.var[nonzero]
  
  std.dev = bayes.estimates.edgew.full[[groupyears[gy]]][[1]]
  std.dev.nonzero = std.dev[nonzero]
  std.dev.zero = std.dev[zero]
  
  full.coeff.var.nonzero = rbind(full.coeff.var.nonzero, coeff.var.nonzero)
  full.std.dev.nonzero = rbind(full.std.dev.nonzero, std.dev.nonzero)
  full.std.dev.zero = rbind(full.std.dev.zero, std.dev.zero)
}
mean(full.std.dev.nonzero, na.rm=T)
mean(full.std.dev.zero, na.rm=T)

