#Generate pre-hurricane social capital metrics

library(stringr)
library(igraph)

#Load scan data and population info
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")

group = c("KK","KK","V", "V", "V")
years = c(2015,2017,2015,2016,2017)
groupyears = c("KK2015", "KK2017","V2015", "V2016", "V2017")
# group = c("KK","V")
# years = c(2017,)
# groupyears = c("KK2017", "V2017")
SocialCapital.ALL = data.frame()
gy = 3

#####################################################################
# Compute Social Capital Metrics, per individual, per year
#####################################################################

for (gy in 1:length(groupyears)){
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
  
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  groom_data = read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt", sep = ""))
  agg_data = read.csv(paste("Group",groupyears[gy],"_AgonsiticActions.txt", sep = ""))
  focal_data = read.csv(paste("Group",groupyears[gy],"_FocalData.txt", sep = ""))
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
  prox_data = read.csv(paste("Group",groupyears[gy],"_ProximityGroups.txt", sep = ""))
  
  #Make sure all IDs are in character
  groom_data$groom.giver = as.character(groom_data$groom.giver)
  groom_data$groom.reciever = as.character(groom_data$groom.reciever)
  # agg_data$agonsim.loser = as.character(agg_data$agonsim.loser)
  # agg_data$agonsim.winner = as.character(agg_data$agonsim.winner)
  
  #Create Social Capital Data frame & add Sex, Age, Rank, Group and Year
  SocialCapitalData= meta_data[,c("id","sex","age","ordinal.rank","percofsex.dominanted")]
  names(SocialCapitalData)=c("id","sex","age","ordrank","percentrank")
  SocialCapitalData$group = group[gy]
  SocialCapitalData$year = years[gy]
  
  #####################################################################
  ## For GROOMING DATA
  #####################################################################
  
  ##############################
  #Using a non-network approach
  
  # 1. Output weighted edgelist from the groom data.
  x = as.character(groom_data$observation.session)
  groom_data$focalID = toupper(as.character(substr(x,10, 12))) #find focal ID from observation session name
  if (gy == 3) {}
  groom.ID = as.character(meta_data$id)
  groom.give = data.frame(); groom.receive = data.frame(); id=1 #Initialize
  for (id in 1:length(groom.ID)){ #For all IDs
    groom.give[id,"id"] = groom.ID[id]; groom.receive[id,"id"] = groom.ID[id]; #Initialize groom give and groom receive df for ID "id"
    groom.give[id,"duration"] = sum(groom_data$duration[which(groom_data$focalID == groom.ID[id] #add groom.give duration if ID is focal
                                                              & groom_data$groom.giver == groom.ID[id])], na.rm =T) # & is a groom giver
    groom.receive[id,"duration"] = sum(groom_data$duration[which(groom_data$focalID == groom.ID[id] #add groom.give duration if ID is focal
                                                                 & groom_data$groom.reciever == groom.ID[id])], na.rm =T)# & is a groom receiver
  }#Outputs 2 structures: groom.give and groom receive - all IDs and duration engaged in both states.
  
  #GROOM GIVE
  hrs.followed.giver = meta_data$hrs.focalfollowed[match(groom.give$id, meta_data$id)] #find the number of hours followed for each groom giver ID
  groom.give$weight <- round(groom.give$duration / hrs.followed.giver, 5) #add weight information by dividing by the #hrs spent observing --> this yields rate
  
  #GROOM RECEIVE
  hrs.followed.reciever = meta_data$hrs.focalfollowed[match(groom.receive$id, meta_data$id)]
  groom.receive$weight <- round(groom.receive$duration / hrs.followed.reciever, 5) #add weight information by dividing by the #hrs spent observing
  
  # 2. Add Groom weighted in-degree and out-degree (weighted)
  SocialCapitalData$GroomIN = groom.receive$weight[match(meta_data$id,groom.receive$id)]
  SocialCapitalData$GroomOUT = groom.give$weight[match(meta_data$id,groom.give$id)]
  TotalGroom = SocialCapitalData$GroomIN + SocialCapitalData$GroomOUT
  meanGroomRate = mean(TotalGroom, na.rm = T) #for standardization. If we wish to!
  SocialCapitalData$DSIgroom = TotalGroom
  SocialCapitalData$std.DSIgroom = TotalGroom/meanGroomRate
  
  # 3. Find the number of grooming partners 
  partners=data.frame(); id=1
  for (id in 1:length(groom.ID)){
    partners[id,"id"] = groom.ID[id] #focal ID
    partners_rec = unique(as.character(groom_data$groom.reciever[which(groom_data$groom.giver == groom.ID[id])])) #find all partners when focal ID was giver
    partners_give = unique(as.character(groom_data$groom.giver[which(groom_data$groom.reciever == groom.ID[id])]))#find all partners when focal ID was receiver
    unique_partners = unique(c(partners_give, partners_rec)) #combine groom give and groom receive partners
    numchar = nchar(unique_partners) #find the number of characters for each partner to make sure I only include partners who we know about
    partners[id,"numPartners"] = length(unique_partners[which(numchar==3)])
  }
  mean_numP = mean(partners$numPartners, na.rm=T)
  partners$std.numPartners = partners$numPartners/mean_numP #standardize partner number
  SocialCapitalData$numPartnersGroom = partners$numPartners[match(meta_data$id,partners$id)]
  SocialCapitalData$std.numPartnersGroom = partners$std.numPartners[match(meta_data$id,partners$id)]
  
  ####################################  
  # 4. Create grooming network metrics. This uses a network approach.
  #Find all unique IDs
  unqIDs = groom.ID
  
  # Output the Master Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL_groom(unqIDs)
  groom_data$conc = paste(groom_data$groom.giver,groom_data$groom.reciever,sep=".")
  # table(groom_data$conc)
  for (ii in 1:length(masterEL$conc)){
    masterEL$count[ii] = sum(groom_data$duration[which(groom_data$conc == masterEL$conc[ii])], na.rm=T)
  }
  #Compute grooming rates using hours followed
  masterEL$hrs.followed = rowSums(cbind(meta_data$hrs.focalfollowed[match(masterEL$givingID, meta_data$id)], #hours followed is the mean num hrs 
                             meta_data$hrs.focalfollowed[match(masterEL$receivingID, meta_data$id)]))/2 #followed between the pair of each edge
  masterEL$weight =  masterEL$count/masterEL$hrs.followed #groom rate
  weightedEL = masterEL[,c("givingID","receivingID","weight")] #only keep columns of interest
  mean_weight = mean(weightedEL$weight[which(weightedEL$weight != 0)])
  weightedEL$stdWeight = weightedEL$weight/mean_weight; weightedEL$weight=NULL
  NAidx = which(is.na(weightedEL$stdWeight)); if(length(NAidx)!=0){stop()} #Check to make sure we don't have NAs
  
  #Create an adjacency matrix to generate igraph object
  adjMat = dils::AdjacencyFromEdgelist(weightedEL)
  data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
  
  #read adjacency matrix
  m=as.matrix(data) # coerces the data set as a matrix
  am.g=graph.adjacency(m,mode="directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
  graph = am.g 
  
  #Get the network measures
  NetworkMetrics = data.frame(matrix(NA, nrow = length(V(graph)), ncol = 7)); names(NetworkMetrics)=c("id","deg","INdeg","OUTdeg","between","eig.cent", "clusterCoeff")
  NetworkMetrics$id = as_ids(V(graph))
  #Weighted degree (Strength, undirected)
  NetworkMetrics$deg<-igraph::degree(graph)
  #weighted indegree
  NetworkMetrics$INdeg <-igraph::degree(graph,v=V(graph), mode = "in", loops=F)
  #weighted outdegree
  NetworkMetrics$OUTdeg <-igraph::degree(graph,v=V(graph), mode = "out", loops=F)
  #Weighted betweenness
  NetworkMetrics$between<-igraph::betweenness(graph, v=V(graph), directed=T, normalized=T)
  #Weighted eigenvector centrality
  A <-igraph::eigen_centrality(graph, directed=F, scale=T)
  eig.cen = as.data.frame(A["vector"])
  NetworkMetrics$eig.cent = eig.cen$vector
  #Weighted clustering coeff
  NetworkMetrics$clusterCoeff = transitivity(graph, type = "localundirected")

  SocialCapitalData[,c("between.groom","eig.cent.groom","clusterCoeff.groom")] = NetworkMetrics[match(meta_data$id, NetworkMetrics$id), c("between","eig.cent","clusterCoeff")]

  #####################################################################
  ## For PROXIMITY DATA
  #####################################################################
  
  ##############################
  #Using a non-network approach
  
  prox_partners = as.data.frame(str_split(prox_data$in.proximity, c(","), simplify = TRUE))
  colnames(prox_partners)[1]="focalID"
  
  unqIDs = as.character(meta_data$id)
  #Find the number of scans per focal ID
  proxRate = data.frame()
  for (id in 1:length(unqIDs)){
    proxRate[id, "id"]= unqIDs[id]
    scans = which(prox_partners$focalID == unqIDs[id]) #find the number of scans where focal ID = id
    proxRate[id, "numScans"] = length(scans) #num scans
    proxRate[id, "numPartners"] = 0
    #Find the number of partners
    for (i in 1:length(scans)){ #for all scans of id
      numProxPartners = length(which(prox_partners[scans[i],2:length(prox_partners)] != "")) #find the number of partners at that scan
      proxRate[id, "numPartners"] = proxRate[id, "numPartners"] + numProxPartners #add #partners through scans
    }
    proxRate[id, "proxRate"] = proxRate$numPartners[id]/proxRate$numScans[id] #rate is the average number of partner per proximity scan
  }
  meanProxRate = mean(proxRate$proxRate, na.rm = T)#to standardize proximity rate later if we wish to.
  proxRate$DSIprox = proxRate$proxRate; proxRate$std.DSIprox = proxRate$proxRate/meanProxRate
  mean_numP = mean(proxRate$numPartners, na.rm=T)#to standardize num prox partner later if we wish to.
  proxRate$std.numPartners = proxRate$numPartners/mean_numP
  
  SocialCapitalData$numPartnersProx = proxRate$numPartners[match(meta_data$id,proxRate$id)]
  SocialCapitalData$std.numPartnersProx = proxRate$std.numPartners[match(meta_data$id,proxRate$id)]
  SocialCapitalData$DSIprox = proxRate$DSIprox[match(meta_data$id,proxRate$id)]
  SocialCapitalData$std.DSIprox = proxRate$std.DSIprox[match(meta_data$id,proxRate$id)]
  
  ####################################  
  # Create grooming network metrics. This uses a network approach.
  rscans = prox_data
  
  # Output the Master Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL(unqIDs)
  
  # 4. Output weighted edgelist from the Master Edgelist.
  options(warn = -1) #set options to ignore all warnings
  weightedEL = calcEdgeList(rscans, masterEL)
  hrs = meta_data$hrs.focalfollowed[match(weightedEL$alter, meta_data$id)]
  weightedEL$weight <- round(weightedEL$count / hrs, 5) #add weight information by dividing by the #hrs spent observing
  weightedEL$count <- NULL;weightedEL$conc <- NULL #delete those calumn variables
  
  #Need to upload an adjacency matrix, rather than socprog style data...
  adjMat = dils::AdjacencyFromEdgelist(weightedEL)
  data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
  
  #read adjacency matrix
  m=as.matrix(data) # coerces the data set as a matrix
  am.g=graph.adjacency(m,mode="undirected",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
  graph = am.g 
  
  #Get the network measures
  NetworkMetrics = data.frame(matrix(NA, nrow = length(V(graph)), ncol = 7)); names(NetworkMetrics)=c("id","deg","INdeg","OUTdeg","between","eig.cent", "clusterCoeff")
  NetworkMetrics$id = as_ids(V(graph))
  #Weighted degree (Strength, undirected)
  NetworkMetrics$deg<-igraph::degree(graph)
  #weighted indegree
  NetworkMetrics$INdeg <-igraph::degree(graph,v=V(graph), mode = "in", loops=F)
  #weighted outdegree
  NetworkMetrics$OUTdeg <-igraph::degree(graph,v=V(graph), mode = "out", loops=F)
  #Weighted betweenness
  NetworkMetrics$between<-igraph::betweenness(graph, v=V(graph), directed=F, normalized=T)
  #Weighted eigenvector centrality
  A <-igraph::eigen_centrality(graph, directed=F, scale=T)
  eig.cen = as.data.frame(A["vector"])
  NetworkMetrics$eig.cent = eig.cen$vector
  #Weighted clustering coeff
  NetworkMetrics$clusterCoeff = igraph::transitivity(graph, type = "localundirected")
  
  SocialCapitalData[,c("between.prox","eig.cent.prox","clusterCoeff.prox")] = NetworkMetrics[match(meta_data$id, NetworkMetrics$id), c("between","eig.cent","clusterCoeff")]
  
  #####################################################################
  ## Combining proximity and grooming data
  
  SocialCapitalData$numPartners = (SocialCapitalData$numPartnersProx + SocialCapitalData$numPartnersGroom)/2
  SocialCapitalData$std.numPartners = (SocialCapitalData$std.numPartnersProx + SocialCapitalData$std.numPartnersGroom)/2
  SocialCapitalData$DSI = (SocialCapitalData$DSIprox + SocialCapitalData$DSIgroom)/2
  SocialCapitalData$std.DSI = (SocialCapitalData$std.DSIprox + SocialCapitalData$std.DSIgroom)/2
  
  #####################################################################
  ## For AGGRESSION DATA
  #####################################################################
  #I am including all aggression types and only focal data to get accurate weights

  # 1. Output weighted edgelist from the aggression data.
  #AGGRESSION GIVE = aggression "winner"
  agg.give = as.data.frame(table(agg_data$agonsim.winner[which(agg_data$focal.individual=="agonsim.winner")]));names(agg.give)[1]='id'
  agg.give = agg.give[-which(nchar(as.character(agg.give$id))>3),] #only include known IDs (i.e. 3 characters)
  hrs.followed.giver = meta_data$hrs.focalfollowed[match(agg.give$id, meta_data$id)] #find #hours followed
  #Exclude individuals not in meta data file
  if (length(which(is.na(hrs.followed.giver))) !=0) {
  agg.give = agg.give[-which(is.na(hrs.followed.giver)),]
  hrs.followed.giver = hrs.followed.giver[-which(is.na(hrs.followed.giver))]
  }
  #Add weights
  agg.give$weight = agg.give$Freq/hrs.followed.giver
  
  #AGGRESSION RECEIVE = aggression "loser"
  agg.receive = as.data.frame(table(agg_data$agonsim.loser[which(agg_data$focal.individual=="agonsim.loser")]));names(agg.receive)[1]='id'
  agg.receive = agg.receive[-which(nchar(as.character(agg.receive$id))>3),]
  hrs.followed.receive= meta_data$hrs.focalfollowed[match(agg.receive$id, meta_data$id)]
  #Exclude individuals not in meta data file
  if (length(which(is.na(hrs.followed.receive))) !=0) {
    agg.receive = agg.receive[-which(is.na(hrs.followed.receive)),]
    hrs.followed.receive = hrs.followed.receive[-which(is.na(hrs.followed.receive))]
  }
  #Add weights
  agg.receive$weight = agg.receive$Freq/hrs.followed.receive

  # 2. Add Aggression weighted in-degree and out-degree (weighted)
  SocialCapitalData$AggOUT = agg.give$weight[match(meta_data$id,agg.give$id)]
  SocialCapitalData$AggIN = agg.receive$weight[match(meta_data$id,agg.receive$id)]
  TotalAgg = SocialCapitalData$AggIN + SocialCapitalData$AggOUT
  meanAggRate = mean(TotalAgg, na.rm = T)
  SocialCapitalData$DSIAgg = TotalAgg
  SocialCapitalData$std.DSIAgg = TotalAgg/meanAggRate
  
  #####################################################################
  ## For vigilance and sdb rates
  #####################################################################
  
  unqIDs = as.character(meta_data$id); vig.sdb=data.frame(matrix(NA,length(unqIDs), 3)); colnames(vig.sdb)=c("id","vig.freq","sdb.freq")
  for (id in 1:length(unqIDs)){
    vig.sdb$id[id] = unqIDs[id]
    vig.sdb$vig.freq[id] = length(which(focal_data$focal.id == unqIDs[id] & (focal_data$behaviour == "Vigilnce" | focal_data$behaviour == "Vigilnce_overtime")))
    vig.sdb$sdb.freq[id] = length(which(focal_data$focal.id == unqIDs[id] & (focal_data$behaviour == "SelfGrm" | focal_data$behaviour == "SelfGrm_overtime" | focal_data$behaviour == "Scratch" | focal_data$behaviour == "Scratch_overtime")))
  }
  hrs.followed = meta_data$hrs.focalfollowed[match(unqIDs, meta_data$id)]
  vig.sdb$vig.ra = vig.sdb$vig.freq/hrs.followed
  vig.sdb$vig.I = vig.sdb$vig.ra/mean(vig.sdb$vig.ra)
  vig.sdb$sdb.ra = vig.sdb$sdb.freq/hrs.followed
  vig.sdb$sdb.I = vig.sdb$sdb.ra/mean(vig.sdb$sdb.ra)
  
  SocialCapitalData$vig.ra = vig.sdb$vig.ra[match(meta_data$id,vig.sdb$id)]
  SocialCapitalData$sdb.ra = vig.sdb$sdb.ra[match(meta_data$id,vig.sdb$id)]
  SocialCapitalData$std.vig.ra = vig.sdb$vig.I[match(meta_data$id,vig.sdb$id)]
  SocialCapitalData$std.sdb.ra = vig.sdb$sdb.I[match(meta_data$id,vig.sdb$id)]
  
  ###################################################################
  # Merge and save data
  SocialCapital.ALL = rbind(SocialCapital.ALL, SocialCapitalData)
}
save( SocialCapital.ALL,file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/SocialCapital.RData")
