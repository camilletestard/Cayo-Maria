library(ggplot2)
library(gplots)
library(igraph)

#Load data
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/ComparingScanFocal/weighted")
load("weighted.allEL.Focal.RData"); load("weighted.allEL.Scan.RData")

# #Delete 2018 networks that don't have focal data
# allEL.Scan[[4]] <-NULL; allEL.Scan[[7]] <-NULL

#Initalize difference vectors
abs.diff.weighted = matrix(nrow=length(allEL.Focal), ncol=1)
abs.diff.unweighted = matrix(nrow=length(allEL.Scan), ncol=1)
abs.diff.degree = matrix(nrow=length(allEL.Scan), ncol=1)
abs.diff.density = matrix(nrow=length(allEL.Scan), ncol=1)
ig1.degree = list(); ig2.degree = list()

unweighted = F; gy=3
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
  
  
  #Compute degree centrality
  ig1.degree[[gy]]=degree(ig1, v=V(ig1), mode="all"); #names(ig1.degree[[gy]])="degree"
  ig2.degree[[gy]]=degree(ig2, v=V(ig2), mode="all"); #names(ig2.degree[[gy]])="degree"
  #Compare degree scores
  # abs.diff.degree[gy]= sum(abs(ig1.degree[[gy]]$degree - ig2.degree[[gy]]$degree))
  plot(ig1.degree[[gy]],ig2.degree[[gy]], xlab="scan-based degree", ylab="focal-based degree", main="Degree from focal vs scan-based SN")
  # plot(scale(ig1.degree$degree),scale(ig2.degree$degree), xlab="scan-based degree", ylab="focal-based degree", main="Degree from focal vs scan-based SN")
  abline(0,1,add=T)

}

data = data.frame(scan=unlist(ig1.degree), focal = unlist(ig2.degree))
ggplot(data, aes(x=scan, y=focal))+
  geom_jitter(size = 2,alpha=0.5, col="blue")+
  # geom_abline()+
  theme_classic(base_size = 20)+
  xlab("degree in scan networks")+ylab("degree in focal networks")

cor.test(data$scan, data$focal)
