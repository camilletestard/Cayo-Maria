#Visualize social network graph
#This script aims at visualizing social networks and how they change over time 
#(especially pre to post hurr). Social networks can be based on proximity or grooming.

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
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")

#Load scan data, population and dominance info
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")
# bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Set parameters
actions = c("prox","groom") 
group = c("V","KK")

# 1. Calculate random subsamples
# allScans$isPost[which(allScans$year==2019)] = 1 #make 2019 a "pre-hurricane" equivalent to compute equivalent 2018-2019 sub-samples. 
# #This will allow us to see whether density goes back down to normal.
# allScans$isPost[which(allScans$year==2018)] = 0 
randomScans = calcRandomScans(allScans)
a=1
for (a in 1:2){
  network_action = actions[a]
  if (network_action == "prox") {network_mode = "undirected"}
  if (network_action == "groom") {network_mode = "directed"}
  
  #For each group, each year separately: 
  g=1;y=1;h=1
  for (g in 1:length(group)){ #For each group
    randscansG = randomScans[which(randomScans$group==group[g]),] 
    
    years = unique(randscansG$year)
    for (y in 1:length(years)){ #For each year in that group
      randscansY = randscansG[which(randscansG$year==years[y]),] 
      year = years[y]
      
      isPost = c(0,1)
      for (h in 1:length(isPost)){ #pre- and post-hurricane 
        
        rscans = randscansY[which(randscansY$isPost==isPost[h]),] 
        numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
        
        # 2. Find the number of unique IDs.
        #Find all unique IDs
        unqIDs = unique(c(as.character(rscans$focalID)))
        
        # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
        if (network_action == "prox") {masterEL = calcMasterEL(unqIDs)}
        if (network_action == "groom") {masterEL = calcMasterEL_groom(unqIDs)}
        
        # 4. Output weighted edgelist from the Master Edgelist.
        options(warn = -1) #set options to ignore all warnings
        if (network_action == "prox") {weightedEL = calcEdgeList(rscans,masterEL)}
        if (network_action == "groom") {weightedEL = calcEdgeList_groom(rscans,masterEL)
        weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
        weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
        weightedEL = weightedEL[,c("alter","ego","count")]
        }
        #set num scans between each pair to be the average of the number of scans of the two individuals
        weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2
        weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by the #observed scans
        # Standardize weights by dividing by the mean weight. Thus we plot relative weights, which take into account group/year differences.
        meanweight = mean(weightedEL$weight[weightedEL$weight>0]) #compute nonzero mean weight
        weightedEL$weight <- weightedEL$weight/meanweight
        weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$numscans<-NULL #delete unused column variables
        
        #Need to upload an adjacency matrix to social network graph
        adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
        data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
        
        #read adjacency matrix
        m=as.matrix(data) # coerces the data set as a matrix
        am.g=graph.adjacency(m,mode= network_mode,weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
        am.g #igraph object properties The letters on the first line (there can be up to 4) indicates some basic information about the graph. 
        #The first letter indicates whether this is a directed ('D') or undirected ('U') graph. 
        #The 2nd letter tells you if  this is a named ('N') graph--i.e., whether or not the vertex set has a 'name' attribute. 
        #The 3rd letter tells you if this graph is weighted ('W'). 
        #The fourth letter is 'B' for bipartite graphs. These letter codes are followed by two numbers: the first is the number of vertices and the second is the number of edges.
        
        #increase space between nodes if overlapping. Choose graph layout.
        if (isPost[h] ==0) {l <- layout.spring(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)} #layout_in_circle}#
        #Node: I added the "if isPost =0" clause to make sure the node position is the same when comparing pre- and post graphs.
        #changes size of labels of vertices
        V(am.g)$label.cex <- 0.8
        
        #link up attributes file with network
        V(am.g)$sex=as.factor(dominance_info$SEX[match(V(am.g)$name,dominance_info$ID)]) #sex attribute
        # V(am.g)$agec=as.factor(att$agec[match(V(am.g)$name,attr$id)]) #age attribute
        # 
        # #set size of nodes by animal age
        # V(am.g)$size=V(am.g)$agec*6 #multiplied by 5 because nodes too small otherwise
        
        #set colour of sexes
        V(am.g)$color=V(am.g)$sex #assign the "Sex" attribute as the vertex color
        V(am.g)$color=gsub("1","darkorange",V(am.g)$color) #Females will be orange
        V(am.g)$color=gsub("2","cyan",V(am.g)$color) #Males will be lightblue
        V(am.g)$color=gsub("0","white",V(am.g)$color) #unknown sex will be white
        
        #set path for saving and plot graph
        if (network_action == "groom"){setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/SocialNetworkGraph/GroomNetworks")} 
        if (network_action == "prox"){setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/SocialNetworkGraph/ProxNetworks") }
        tiff(paste("Social Network ",group[g],years[y],".",isPost[h],".tiff",sep=""), 
             units="in", width=10, height=8, res=300, compression = 'lzw')
        plot.igraph(am.g,layout=l, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color="grey20", 
                    edge.width=E(am.g)$weight*2,edge.arrow.size = 0.5, main = paste("Social Network ",group[g],years[y],".",isPost[h],sep=""))
        dev.off()
        
        
        # plot.igraph(am.g,layout=l, vertex.color="CYAN1", vertex.size=7,edge.color="grey20", 
        #             edge.width=E(am.g)$weight*2,edge.arrow.size = 0.5)
        # 
        # 
        # #spring embedded layout
        # s <- layout.spring(am.g, spring.length=1000,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)
        # 
        # #REMOVE NODE LABEL
        # plot.igraph(am.g,layout=l, vertex.label=NA, vertex.color="orange", vertex.size=6,edge.color="grey20",edge.width=E(am.g)$weight*0.01)
        # 
        # #plot spring embedded graph
        # plot.igraph(am.g,layout=s, vertex.size=6, vertex.label.cex=0.5, edge.color="grey20",edge.width=E(am.g)$weight*0.01)
        
        
        #Import an attribute file for labelling nodes
        # attr<-read.csv("ATTRIBUTES_allsubjects.csv")
        # str(attr)
        
        
        
      }
    }
  }  
}