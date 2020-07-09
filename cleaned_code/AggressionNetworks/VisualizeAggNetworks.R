# Visualize Aggression Networks
# This script compute agression-based edgelist and weights, and plots aggression-based networks.
# It uses all types of aggression interactions (contact/non-contact/submission etc.). Because aggression data was not
# collected  during proximity scans in "normal data collection" years, I cannot use the sub-sampling approach we 
# developed for affiliative behaviors (proximity and grooming). Pre-hurricane: aggression is collected during focals 
# and rates should be computed by dividing counts by the number of hours. Post-hurricane (2018): aggression is collected 
# during scans. Thus, to compute weights I divide counts by the number of scan observations per IDs. This approach assumes
# a linear relationship whereby the more I observe an ID, the more counts of aggressive interactions I will get.
# So far, I have not implemented any sub-sampling in terms of number of observations per ID. However, I only use 
# "common IDs" across years - that is IDs that are involved in aggression throughout all years of observations (longitudinal).
# IMPORTANT NOTE: aggression data post-hurricane is collected using a pseudo-systematic approach (almost ad-lib?) which
# may have introduced biases in our observations. However, if anything, we should see an artificial INCREASE in 
# number of aggressive interactions (aggression is salient, easily observable during non-systematic sampling). However we rather 
# see a *decrease* in aggressive interactions.
# IMPORTANT NOTE #2: This could be simply due to the difference in feeding protocol. I should re-run the analysis only
# with PM data (not trivial to produce!).
# Functions Called: functions_GlobalNetworkMetrics
# Inputs: GroupByYear.txt, AgonsiticActions.txt, commonIDs.Rdata
# Outputs: Aggression network graphs

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
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria")
load("R.Data/commonIDs.Rdata")
source("cleaned_code/Functions/functions_GlobalNetworkMetrics.R")

#Set parameters
network_action = "groom" #"prox" or "groom" or "
network_mode = "directed" #"undirected" (if prox) or "directed" (if groom)
network_weighted = T

#For each group, each year separately: 
group = c("V","V","V","V","KK","KK","KK","S") #c("V","V","V","V","V","KK","KK","KK","S")
years = c(2016,2017,2018, 2019, 2015, 2017, 2018, 2019)#c(2015,2016,2017,2018, 2019, 2015, 2017, 2018, 2019)
groupyears =c("V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018","S2019") #c("V2015","V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018", "S2019") 

gy=2
for (gy in 1:length(groupyears)){ #For each group
  
  #Load data
  setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
  data = read.csv(paste("Group",groupyears[gy],"_AgonsiticActions.txt",sep=""))
  # if (years[gy]==2018){meta_data$hrs.focalfollowed=meta_data$numObs} #if 2018, convert column #obs in #hrs to simplify coding after
  
  #create date of observation information: 
  data$semester = semester(data$date)
  data$quarter = quarter(data$date)
  data$timeBlock = "AM"; data$timeBlock[data$timeblock>3]="PM"
  
  #Output the Master Edgelist of all possible pairs given the unique IDs.
  #Load unique IDs common across years. This allows to make sure we compare the same indidivuals across years.
  if (group[gy]=="V") {unqIDs = commonIDsV} else if(group[gy]=="KK") {unqIDs = commonIDsKK} else {unqIDs = as.character(unique(meta_data$id))}
  masterEL = calcMasterEL_groom(unqIDs) #directional edgelist
  
  # 4. Output weighted edgelist from the Master Edgelist.
  data$conc <- paste(data[,1],data[,2],sep="."); weightedEL = masterEL
  count = data.frame(table(data$conc)) #find the number of occurrence of each pair agonistic interaction 
  weightedEL$count = count$Freq[match(weightedEL$conc,count$Var1)] #Assign count to edgelist
  weightedEL$count[which(is.na(weightedEL$count))] = 0 #NAs should be set to weight=0
  weightedEL$hrs <- (meta_data$hrs.focalfollowed[match(weightedEL$givingID, meta_data$id)] +  #number of hrs = avg between the pair of IDs
                       meta_data$hrs.focalfollowed[match(weightedEL$receivingID, meta_data$id)])/2
  weightedEL$weight <- round(weightedEL$count / weightedEL$hrs, 5) #add weight information by dividing by the #hrs observed (or #scans for 2018 year) for each individual
  meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)]) #nonzero group mean
  weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights by group mean
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$hrs <-NULL #delete those calumn variables
  
  #Checks
  # length(which(weightedEL$weight>0)) #number of agonistic interactions 
  # length(which(weightedEL$weight!=0))/nrow(weightedEL) #density of agonistic network
  
  #Create adjacency matrix
  adjMat = dils::AdjacencyFromEdgelist(weightedEL)
  data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
  
  #read adjacency matrix
  m=as.matrix(data) # coerces the data set as a matrix
  am.g=graph.adjacency(m,mode= network_mode,weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
  
  ###########################################
  #PLOT GRAPH
  ###########################################
  
  #increase space between nodes if overlapping
  #fruchterman reingold layout
  # if (groupyears[gy] =="V2015") {l <- layout_in_circle}#layout.spring(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)
  #changes size of labels of vertices
  V(am.g)$label.cex <- 0.7
  
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
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
  tiff(paste("Agg.SN.",groupyears[gy],".tiff",sep=""), units="in", width=10, height=8, res=300, compression = 'lzw')
  if (network_weighted) {plot.igraph(am.g,layout=layout_in_circle, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color="grey20",
                                     edge.width=E(am.g)$weight*2,edge.arrow.size = 0.5, main = paste("Agg.SN.",groupyears[gy],sep=""))}
  else {plot.igraph(am.g,layout=layout_in_circle, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.size=13,edge.color="grey20",
                    edge.width=2,edge.arrow.size = 0.3, main = paste("Agg.SN.",groupyears[gy],sep=""))}
  dev.off()
  
}  
