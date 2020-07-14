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
library(matlab)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code") 
source("Functions/CalcSubsampledScans.R")
source("Functions/functions_GlobalNetworkMetrics.R")

#Load scan data, population and dominance info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/") 
allScans = read.csv("Data All Cleaned/allScans2019.txt")
# bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")
dominance_info =read.table("Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Set parameters
actions = c("prox","groom") 
group = c("V","KK")

# 1. Calculate random subsamples
# allScans$isPost[which(allScans$year==2019)] = 1 #make 2019 a "pre-hurricane" equivalent to compute equivalent 2018-2019 sub-samples. 
# #This will allow us to see whether density goes back down to normal.
# allScans$isPost[which(allScans$year==2018)] = 0 
randomScans = calcRandomScans(allScans)
a=2
for (a in 1:2){
  network_action = actions[a]
  if (network_action == "prox") {network_mode = "undirected"}
  if (network_action == "groom") {network_mode = "directed"}
  
  #For each group, each year separately:
  all.connections=0;familiar.connections=0
  g=1;y=1;h=1
  for (g in 1:length(group)){ #For each group
    randscansG = randomScans[which(randomScans$group==group[g]),] 
    
    years = unique(randscansG$year)
    for (y in 1:length(years)){ #For each year in that group
      randscansY = randscansG[which(randscansG$year==years[y]),] 
      year = years[y]
      
      isPost = c(0,1); 
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
        weightedEL = weightedEL[,c("alter","ego","count","conc")]
        }
        #set num scans between each pair to be the average of the number of scans of the two individuals
        weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2
        weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by the #observed scans
        # Standardize weights by dividing by the mean weight. Thus we plot relative weights, which take into account group/year differences.
        meanweight = mean(weightedEL$weight[weightedEL$weight>0]) #compute nonzero mean weight
        weightedEL$weight <- weightedEL$weight/meanweight
        weightedEL$count <- NULL; weightedEL$numscans<-NULL #delete unused column variables
        
        if(isPost[h]==0){net.pre=weightedEL} else {net.post=weightedEL}
        
      }
      #sort in alphabetical order:
      net.pre=net.pre[order(net.pre$conc),]; net.post=net.post[order(net.post$conc),]
      #set familiarity vector to black when pair present in only one netowrk, and red when present in both networks.
      familiarity = ifelse(net.pre$weight!=0 & net.post$weight!=0,'red','black'); length(which(familiarity=="red"))
      test=data.frame(cbind(net.pre,net.post))
      test$familiarity = ifelse(test$weight!=0 & test$weight.1!=0,'red','black');test$familiarity[is.na(test$familiarity)]=0
      test.nonzero.weight = filter(test,test$weight!=0|test$weight.1!=0)
      # familiarity=test$familiarity;
      
      isPost=c(0,1,2); h=1
      for (h in 1:length(isPost)){ #pre- and post-hurricane 
        
        if(isPost[h]==0){weightedEL=net.pre} else {weightedEL=net.post}
        if(isPost[h]==2){weightedEL=net.pre; weightedEL$weight[familiarity=="black"]=0}
        weightedEL$weight[familiarity=="red"]
        weightedEL$conc <- NULL;
        #Need to upload an adjacency matrix to social network graph
        adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
        data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
        
        #read adjacency matrix
        m=as.matrix(data) # coerces the data set as a matrix
        am.g=graph.adjacency(m,mode= network_mode,weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
        
        #increase space between nodes if overlapping. Choose graph layout.
        if (isPost[h] ==0) {l <- layout_on_grid(am.g)};#layout_in_circle(am.g, order = V(am.g))}#layout.spring(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)
        familiarity.color = familiarity
        all.connections=cbind(all.connections,length(which(weightedEL$weight!=0)))
        familiar.connections=cbind(familiar.connections,length(which(familiarity.color=="red")))
        #Node: I added the "if isPost =0" clause to make sure the node position is the same when comparing pre- and post graphs.
        #changes size of labels of vertices
        V(am.g)$label.cex <- 0.8
        
        #link up attributes file with network
        V(am.g)$sex=as.factor(dominance_info$SEX[match(V(am.g)$name,dominance_info$ID)]) #sex attribute
        
        E(am.g)$fam = familiarity[weightedEL$weight!=0]
        # if (isPost[h] ==2){familiarity[familiarity=="red"]}
        
        #set colour of sexes
        V(am.g)$color=V(am.g)$sex #assign the "Sex" attribute as the vertex color
        V(am.g)$color=gsub("1","darkorange",V(am.g)$color) #Females will be orange
        V(am.g)$color=gsub("2","cyan",V(am.g)$color) #Males will be lightblue
        V(am.g)$color=gsub("0","white",V(am.g)$color) #unknown sex will be white
        
        #set path for saving and plot graph
        if (network_action == "groom"){setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/GroomNetworks")}
        if (network_action == "prox"){setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/ProxNetworks") }
        tiff(paste("Color-coded Social Network ",group[g],years[y],".",isPost[h],".tiff",sep=""),
        units="in", width=10, height=8, res=300, compression = 'lzw')
        plot.igraph(am.g,layout=l, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color=E(am.g)$fam,
                    edge.width=1,edge.arrow.size = 1, main = paste("Social Network ",group[g],years[y],".",isPost[h],sep=""))
        # E(am.g)$weight*2
        dev.off()
      }
    }
  }
}  

rbind(all.connections,familiar.connections)
