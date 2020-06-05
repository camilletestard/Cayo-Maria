library(ggplot2)
library(gplots)
library(igraph)

#Load data
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("allEL.Focal.RData"); load("allEL.Scan.RData")

#Delete 2018 networks that don't have focal data
allEL.Scan[[4]] <-NULL; allEL.Scan[[7]] <-NULL

#Initalize difference vectors
abs.diff.weighted = matrix(nrow=length(allEL.Focal), ncol=length(allEL.Focal))
abs.diff.unweighted = matrix(nrow=length(allEL.Scan), ncol=length(allEL.Scan))
abs.diff.density = matrix(nrow=length(allEL.Scan), ncol=length(allEL.Scan))
abs.diff.between = matrix(nrow=length(allEL.Scan), ncol=length(allEL.Scan))
abs.diff.cliqueNum = matrix(nrow=length(allEL.Scan), ncol=length(allEL.Scan))
abs.diff.closeness = matrix(nrow=length(allEL.Scan), ncol=length(allEL.Scan))
abs.diff.degree = matrix(nrow=length(allEL.Scan), ncol=length(allEL.Scan))
abs.diff.eigencent = matrix(nrow=length(allEL.Scan), ncol=length(allEL.Scan))

unweighted = T; gy=3
for (gy in 1:length(allEL.Scan)){
  
  #################################################
  # Compute difference in all weights across graphs
  
  #find absolute difference in weights
  abs.diff.weighted[gy]= sum(abs(allEL.Scan[[gy]]$weight - allEL.Focal[[gy]]$weight))
  
  #Find absolute difference for unweighted graph
  allEL.Scan.Unweighted = rep(0,length(allEL.Scan[[gy]]$weight)) #Create unweighted (binary) version of graphs
  allEL.Scan.Unweighted[which(allEL.Scan[[gy]]$weight!=0)]=1
  
  allEL.Focal.Unweighted = rep(0,length(allEL.Focal[[gy]]$weight))
  allEL.Focal.Unweighted[which(allEL.Focal[[gy]]$weight!=0)]=1
  
  abs.diff.unweighted[gy]=sum(abs(allEL.Scan.Unweighted - allEL.Focal.Unweighted))
  
  #################################################
  # Compute difference in #edges (unweighted)
  abs.diff.density[gy]= sum(abs(length(which(allEL.Scan[[gy]]$weight>0)) - length(which(allEL.Focal[[gy]]$weight>0))))
  
  #################################################
  # Compute differences network properties
  
  if(unweighted){allEL.Scan[[gy]]$weight = allEL.Scan.Unweighted}
  if(unweighted){allEL.Focal[[gy]]$weight = allEL.Focal.Unweighted}
  
  #For weighted Graphs:
  # create igraph object from Scan data
  ig1 <- simplify(graph.data.frame(d=allEL.Scan[[gy]], directed = F), #create undirected igraph object 
                  remove.loops = T) #simple graph with no loop
  ig1 <- delete.edges(ig1, which(E(ig1)$weight == 0)) #delete edges that have a weight of 0
  
  # create igraph object from Focal data
  ig2 <- simplify(graph.data.frame(d=allEL.Focal[[gy]], directed = F), #create undirected igraph object 
                  remove.loops = T) #simple graph with no loop
  ig2 <- delete.edges(ig2, which(E(ig2)$weight == 0)) #delete edges that have a weight of 0
  
  #Compute betweenness distribution
  ig1.between = data.frame(betweenness(ig1, v = V(ig1), directed = TRUE)); names(ig1.between)="betweenness"
  ig2.between = data.frame(betweenness(ig2, v = V(ig2), directed = TRUE)); names(ig2.between)="betweenness"
  #Compare betweenness scores
  abs.diff.between[gy]= sum(abs(ig1.between$betweenness - ig2.between$betweenness))
  plot(ig1.between$betweenness/mean.between.ig1,ig2.between$betweenness/mean.between.ig2, xlab="scan-based betweenness", ylab="focal-based betweenness", 
       main="Betweenness from focal vs scan-based SN")
  # plot(scale(ig1.between$betweenness),scale(ig2.between$betweenness), xlab="scan-based betweenness", ylab="focal-based betweenness", 
  #      main="Betweenness from focal vs scan-based SN")
  abline(0,1)
  
  #Compare number of cliques
  abs.diff.cliqueNum[gy]=abs(length(cliques(ig1))-length(cliques(ig2)))
  
  # #Compare closeness
  # ig1.closeness=data.frame(closeness(ig1, vids=V(ig1), mode="all")); names(ig1.closeness)="closeness"
  # ig2.closeness=data.frame(closeness(ig2, vids=V(ig2), mode="all")); names(ig2.closeness)="closeness"
  # #Compare closeness scores
  # abs.diff.closeness[gy]= sum(abs(ig1.closeness$closeness - ig2.closeness$closeness))
  # plot(ig1.closeness$closeness,ig2.closeness$closeness)
  # abline(0,1,add=T)
  
  #Compute degree centrality
  ig1.degree=data.frame(degree(ig1, v=V(ig1), mode="all")); names(ig1.degree)="degree"
  ig2.degree=data.frame(degree(ig2, v=V(ig2), mode="all")); names(ig2.degree)="degree"
  #Compare degree scores
  abs.diff.degree[gy]= sum(abs(ig1.degree$degree - ig2.degree$degree))
  plot(ig1.degree$degree/mean(ig1.degree$degree),ig2.degree$degree/mean(ig2.degree$degree), xlab="scan-based degree", ylab="focal-based degree", main="Degree from focal vs scan-based SN")
  # plot(scale(ig1.degree$degree),scale(ig2.degree$degree), xlab="scan-based degree", ylab="focal-based degree", main="Degree from focal vs scan-based SN")
  abline(0,1,add=T)
  
  #Compute eigenvector centrality
  ig1.eigencent=eigen_centrality(ig1, directed=T)
  ig2.eigencent=eigen_centrality(ig2, directed=T)
  mean.eigencent.ig1 = mean(ig1.eigencent$vector)
  mean.eigencent.ig2 = mean(ig2.eigencent$vector)
  #Compare eigenvector centrality scores
  abs.diff.eigencent[gy]= sum(abs(ig1.eigencent$vector - ig2.eigencent$vector))
  plot(ig1.eigencent$vector/mean.eigencent.ig1,ig2.eigencent$vector/mean.eigencent.ig2, ylim=c(0,2.5))
  abline(0,1,add=T)
  
  #Clustering organization
  # cluster_louvain(ig1)
  
}

