#VisualizeSocialNetworks_ScanData
#VisualizeSocialNetworks_ScanData & VisualizeSocialNetworks_FocalData: These script aims at comparing 
#social networks computed using scan data versus focal data. The goal is to check whether scan (or proximity data)
#pre-hurricane recapitulates focal data well

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
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")

#Load scan data, population and dominance info
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Set parameters
network_action = "groom" #"prox" or "groom" 
network_mode = "directed" #"undirected" (if prox) or "directed" (if groom)
network_weighted = "weighted" # "unweighted"

#For each group, each year separately: 
group = c("V","V","V","V","KK","KK")
years = c(2015,2016,2017,2019, 2015, 2017)
groupyears =c("V2015","V2016","V2017","V2019","KK2015","KK2017")

allEL.Scan = list()
for (gy in 1:length(groupyears)){ #For each group
  
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
  
  rscans = allScans[which(allScans$group == group[gy] & allScans$year==years[gy]),]
  
  # 2. Find the number of unique IDs.
  unqIDs = unique(as.character(unique(meta_data$id)))
  
  # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL_groom(unqIDs)
  
  # 4. Output weighted edgelist from the Master Edgelist.
  options(warn = -1) #set options to ignore all warnings
  weightedEL = calcEdgeList_groom(rscans,masterEL)
  weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
  weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
  weightedEL = weightedEL[,c("alter","ego","count")]
  #set num hrs observed between each pair to be the average #hrs observed by the two individuals
  weightedEL$hrs <- (meta_data$hrs.focalfollowed[match(weightedEL$alter, meta_data$id)] + 
                       meta_data$hrs.focalfollowed[match(weightedEL$ego, meta_data$id)])/2
  weightedEL$weight <- round(weightedEL$count / weightedEL$hrs, 5) #add weight information by dividing by the #hrs observed for each individual
  # Standardize weights by dividing by the mean weight. Thus we plot relative weights, which take into account group/year differences.
  meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
  weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights 
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$hrs <-NULL #delete those calumn variables
  
  #Save weighted EL
  allEL.Scan[[gy]]=weightedEL
  
  ##############################
  #Plot Social network
  
  #Need to upload an adjacency matrix
  adjMat = dils::AdjacencyFromEdgelist(weightedEL)
  data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
  
  #read adjacency matrix
  m=as.matrix(data) # coerces the data set as a matrix
  am.g=graph.adjacency(m,mode= network_mode,weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
  am.g #igraph object properties The letters on the first line (there can be up to 4) indicates some basic information about the graph. 
  
  
  #increase space between nodes if overlapping. Choose graph layout.
  # if (isPost[h] ==0) {l <- layout_in_circle}#layout.spring(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)}
  #changes size of labels of vertices
  V(am.g)$label.cex <- 0.5
  
  #link up attributes file with network
  V(am.g)$sex=as.factor(meta_data$sex[match(V(am.g)$name,meta_data$id)])
  # V(am.g)$agec=as.factor(att$agec[match(V(am.g)$name,attr$id)])
  # V(am.g)$agec
  # 
  # #set size of nodes by animal age
  # V(am.g)$size=V(am.g)$agec*6 #multiplied by 5 because nodes too small otherwise
  
  #set colour of sexes
  V(am.g)$color=V(am.g)$sex #assign the "Sex" attribute as the vertex color
  V(am.g)$color=gsub("1","darkorange",V(am.g)$color) #Females will be orange
  V(am.g)$color=gsub("2","cyan",V(am.g)$color) #Males will be lightblue
  V(am.g)$color=gsub("0","white",V(am.g)$color) #unknown sex will be white
  
  #plot graph
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/SocialNetworkGraph/ComparingScanFocal")
  tiff(paste("Scan-based Grooming Network",network_weighted,groupyears[gy],".tiff",sep="."),
       units="in", width=10, height=8, res=300, compression = 'lzw')
  if (network_weighted=="weighted") {plot.igraph(am.g,layout=layout_in_circle, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color="grey20",
              edge.width=E(am.g)$weight*2,edge.arrow.size = 0.3, main = paste("Scan-Based Grooming Network ",groupyears[gy],sep=""))
    } else {plot.igraph(am.g,layout=layout_in_circle, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color="grey20",
                    edge.width=2,edge.arrow.size = 0.3, main = paste("Scan-Based Grooming Network ",groupyears[gy],sep=""))}
  dev.off()
  
}  
save(allEL.Scan, file=paste(network_weighted,"allEL.Scan.RData",sep="."))
