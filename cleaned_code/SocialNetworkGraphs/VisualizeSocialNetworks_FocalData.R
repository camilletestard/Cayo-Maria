# VisualizeSocialNetworks_FocalData: 
# This script aims at comparing grooming networks computed using scan data versus focal data (only for years we have focal data!). 
# The goal is to check whether scan (or proximity data) pre-hurricane recapitulates grooming focal data well.

# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)
library(lubridate)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/functions_SocialSupport.R")

#Load scan data, population and dominance info
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Set parameters
network_action = "groom" 
network_mode = "directed"
network_weighted = "weighted" # "unweighted"

#For each group, each year separately: 
group = c("V","V","V","V","KK","KK")
years = c(2015,2016,2017,2019, 2015, 2017)
groupyears =c("V2015","V2016","V2017","V2019","KK2015","KK2017"); gy=1

allEL.Focal = list()

for (gy in 1:length(groupyears)){ #For each group
  
  #####################################################
  #FOR GROOMING
  #####################################################

  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
  data = data=read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt",sep=""))
  
  #create date of observation information: 
  data$date = mdy(gsub(".","-",substr(data$observation.session,1,8),fixed=T))
  data$semester = semester(data$date)
  data$quarter = quarter(data$date)
  
  #Output the Master Edgelist of all possible pairs given the unique IDs.
  unqIDs = as.character(unique(meta_data$id))
  masterEL = calcMasterEL_groom(unqIDs)
  
  # 4. Output weighted edgelist from the Master Edgelist.
  data$conc <- paste(data[,1],data[,2],sep="."); weightedEL = masterEL
  # Transform edgelist name for later coding
  weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
  weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
  weightedEL = weightedEL[,c("alter","ego","conc","count")]
  
  #count the duration of pair grooming
  count = data.frame(table(data$conc))
  for (i in 1:nrow(weightedEL)){
    weightedEL$count[i] = sum(data$duration[which(data$conc == weightedEL$conc[i])]) #find the time spent grooming for each pair
  }
  weightedEL$count[which(is.na(weightedEL$count))] = 0
  weightedEL$hrs <- (meta_data$hrs.focalfollowed[match(weightedEL$alter, meta_data$id)] + 
                       meta_data$hrs.focalfollowed[match(weightedEL$ego, meta_data$id)])/2
  weightedEL$weight <- round(weightedEL$count / weightedEL$hrs, 5) #add weight information by dividing by the #scans for each individual
  #check: which(weightedEL$weight>0); which(weightedEL$count>0)
  meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
  weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights 
  #Save weighted EL
  allEL.Focal[[gy]]=weightedEL
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$hrs <-NULL #delete those calumn variables
  
  #Create adjacency matrix
  adjMat = dils::AdjacencyFromEdgelist(weightedEL)
  data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
  
  #read adjacency matrix
  m=as.matrix(data) # coerces the data set as a matrix
  am.g=graph.adjacency(m,mode= network_mode,weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
  
  #increase space between nodes if overlapping. Choose layout.
  # if (groupyears[gy] =="V2015") {l <- layout_in_circle}#layout.spring(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)
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
  setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/ComparingScanFocal")
  tiff(paste("Focal-based Grooming Network",network_weighted,groupyears[gy],".tiff",sep="."),
       units="in", width=10, height=8, res=300, compression = 'lzw')
  if (network_weighted=="weighted") {plot.igraph(am.g,layout=layout_in_circle, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color="grey20",
                                     edge.width=E(am.g)$weight*2,edge.arrow.size = 0.3, main = paste("Focal-Based Grooming Network ",groupyears[gy],sep=""))}
  else {plot.igraph(am.g,layout=layout_in_circle, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color="grey20",
                    edge.width=2,edge.arrow.size = 0.3, main = paste("Focal-Based Grooming Network ",groupyears[gy],sep=""))}
  dev.off()

}  
save(allEL.Focal, file=paste(network_weighted,"allEL.Focal.RData",sep="."))