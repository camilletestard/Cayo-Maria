#Generate pre-hurricane social capital metrics

library(stringr)
library(igraph)

#Load scan data and population info
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
source("Social_Network_Analysis/CalcSubsampledScans.R")
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
# group = c("KK","KK","V", "V", "V")
# years = c(2015,2017,2015,2016,2017)
#groupyears = c("KK2015", "KK2017","V2015", "V2016", "V2017")
group = c("KK","V")
years = 2017
groupyears = c("KK2017", "V2017")
SocialCapital.ALL = data.frame()

for (i in 1:length(groupyears)){
  
  print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[i], "%%%%%%%%%%%%%%%%%%"))
  
  #####################################################################
  # 1. Compute change in p(Acc) and p(Social), per individual, per year
  #####################################################################
  num_iter = 50; dprob.ALL = data.frame()
  #Calculate random subsamples
  for (iter in 1:num_iter){
    print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
    
    randomScans = calcRandomScans(allScans)
    rscans = randomScans[which(randomScans$year == 2017 & randomScans$group == group[i]),]
    # rscans = randomScans[which(randomScans$year == years[i] & randomScans$group == group[i]),]
    
    unqIDs = as.character(unique(rscans$focalID))
    dprob=data.frame(matrix(NA, nrow=length(unqIDs),2)); colnames(dprob)=c("dpAcc","dpSocial")
    for (id in 1:length(unqIDs)){ #For all individuals
      isProx.pre = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isProx.post = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      isSocial.pre = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isSocial.post = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      dpAcc=NA; dpSocial=NA
      if (length(isProx.pre)>=10) { #If there are more than 10 observations for that individual 
        pACC.pre = sum(isProx.pre)/length(isProx.pre)
        pACC.post = sum(isProx.post)/length(isProx.post)
        dpAcc = pACC.post - pACC.pre
        pSocial.pre = sum(isSocial.pre)/length(isSocial.pre)
        pSocial.post = sum(isSocial.post)/length(isSocial.post)
        dpSocial = pSocial.post - pSocial.pre
      }
      dprob[id,]=c(dpAcc,dpSocial)
    }
    dprob.ALL = rbind(dprob.ALL, dprob)
  }
  dprob.ALL = rbind(data.frame(rep(unqIDs,num_iter),dprob.ALL)); names(dprob.ALL)[1]="id"
  dprob.ALL$groupyear = groupyears[i]
  
  # #####################################################################
  # # 2. Add Sex, Age and Rank
  # #####################################################################
  # 
  # demoInfo= data.frame()
  # for (iii in 1:length(unqIDs)){
  #   idx = which(rscans$focalID == unqIDs[iii])
  #   demoInfo[iii,c("id","sex","age","ordrank","percentrank")] <- c(unqIDs[iii],as.character(rscans$sex[idx[1]]),rscans$age[idx[1]],
  #                                                                  as.character(rscans$ordrank[idx[1]]),rscans$percentrank[idx[1]])
  # }
  # dprob.ALL[,c("sex","age","ordrank","percentrank")] = demoInfo[match(dprob.ALL$id,demoInfo$id),c("sex","age","ordrank","percentrank")]
  # col.order = c("id","groupyear","sex","age","ordrank","percentrank","dpAcc","dpSocial")
  # dprob.ALL = dprob.ALL[,col.order]
  # 
  #####################################################################
  # 3. Compute Social Capital Metrics, per individual, per year
  #####################################################################
  
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  groom_data = read.csv(paste("Group",groupyears[i],"_GroomingEvents.txt", sep = ""))
  agg_data = read.csv(paste("Group",groupyears[i],"_AgonsiticActions.txt", sep = ""))
  focal_data = read.csv(paste("Group",groupyears[i],"_FocalData.txt", sep = ""))
  meta_data = read.csv(paste("Group",groupyears[i],"_GroupByYear.txt", sep = ""))
  prox_data = read.csv(paste("Group",groupyears[i],"_ProximityGroups.txt", sep = ""))
  
  #Create Social Capital Data frame & add Sex, Age and Rank
  SocialCapitalData= meta_data[,c("id","sex","age","ordinal.rank","percofsex.dominanted")]
  names(SocialCapitalData)=c("id","sex","age","ordrank","percentrank")
  
  #####################################################################
  ## For GROOMING DATA
  
  # 1. Output weighted edgelist from the groom data.
  x = as.character(groom_data$observation.session)
  groom_data$focalID = substr(x,nchar(x)-7, nchar(x)-5)
  groom.ID = unique(groom_data$focalID); groom.give = data.frame(); groom.receive = data.frame()
  for (id in 1:length(groom.ID)){
    groom.give[id,"id"] = groom.ID[id]; groom.receive[id,"id"] = groom.ID[id]; 
    groom.give[id,"duration"] = sum(groom_data$duration[which(groom_data$focalID == groom.ID[id] & groom_data$groom.giver == groom.ID[id])], na.rm =T)
    groom.receive[id,"duration"] = sum(groom_data$duration[which(groom_data$focalID == groom.ID[id] & groom_data$groom.reciever == groom.ID[id])], na.rm =T)
  }
  #GROOM GIVE
  hrs.followed.giver = meta_data$hrs.focalfollowed[match(groom.give$id, meta_data$id)]
  groom.give$weight <- round(groom.give$duration / hrs.followed.giver, 5) #add weight information by dividing by the #hrs spent observing --> this yields rate
  groom.give$duration <- NULL
  
  #GROOM RECEIVE
  hrs.followed.reciever = meta_data$hrs.focalfollowed[match(groom.receive$id, meta_data$id)]
  #Compute weight for each receiver
  groom.receive$weight <- round(groom.receive$duration / hrs.followed.reciever, 5) #add weight information by dividing by the #hrs spent observing
  groom.receive$duration <- NULL
  
  # 2. Add Groom weighted in-degree and out-degree (weighted)
  SocialCapitalData$group = group[i]; SocialCapitalData$year = 2017 #years[i]
  SocialCapitalData$GroomIN = groom.receive$weight[match(meta_data$id,groom.receive$id)]
  SocialCapitalData$GroomOUT = groom.give$weight[match(meta_data$id,groom.give$id)]
  TotalGroom = SocialCapitalData$GroomIN + SocialCapitalData$GroomOUT
  meanGroomRate = mean(TotalGroom, na.rm = T)
  SocialCapitalData$DSIgroom = TotalGroom/meanGroomRate
  
  # 3. Find the number of grooming partners 
  partners=data.frame()
  for (id in 1:length(groom.ID)){
    partners[id,"id"] = groom.ID[id]
    partners_rec = unique(as.character(groom_data$groom.reciever[which(groom_data$groom.giver == groom.ID[id])]))
    partners_give = unique(as.character(groom_data$groom.giver[which(groom_data$groom.reciever == groom.ID[id])]))
    unique_partners = unique(c(partners_give, partners_rec)); numchar = nchar(unique_partners)
    partners[id,"numPartners"] = length(unique_partners[which(numchar==3)])
  }
  SocialCapitalData$numPartnersGroom = partners$numPartners[match(meta_data$id,partners$id)]
    
  #####################################################################
  ## For PROXIMITY DATA
  prox_partners = as.data.frame(str_split(prox_data$in.proximity, c(","), simplify = TRUE))
  colnames(prox_partners)[1]="focalID"
  
  unqIDs = unique(prox_partners$focalID)
  #Find the number of scans per focal ID
  proxRate = data.frame()
  for (id in 1:length(unqIDs)){
    proxRate[id, "id"]= unqIDs[id]
    scans = which(prox_partners$focalID == unqIDs[id])
    proxRate[id, "numScans"] = length(scans)
    proxRate[id, "numPartners"] = 0
    for (i in 1:length(scans)){
      numProxPartners = length(which(prox_partners[scans[i],2:length(prox_partners)] != ""))
      proxRate[id, "numPartners"] = proxRate[id, "numPartners"] + numProxPartners
    }
    proxRate[id, "proxRate"] = proxRate$numPartners[id]/proxRate$numScans[id]
  }
  meanProxRate = mean(proxRate$proxRate, na.rm = T)
  proxRate$DSIprox = proxRate$proxRate/meanProxRate
  
  SocialCapitalData$numPartnersProx = proxRate$numPartners[match(meta_data$id,proxRate$id)]
  SocialCapitalData$DSIprox = proxRate$DSIprox[match(meta_data$id,proxRate$id)]
  
  #####################################################################
  ## Combining proximity and grooming data
  
  SocialCapitalData$numPartners = (SocialCapitalData$numPartnersProx + SocialCapitalData$numPartnersGroom)/2
  SocialCapitalData$DSI = (SocialCapitalData$DSIprox + SocialCapitalData$DSIgroom)/2
  
  #####################################################################
  ## For AGGRESSION DATA
  #I am including all aggression types and only focal data to get accurate weights

  # 1. Output weighted edgelist from the aggression data.
  #AGGRESSION GIVE
  agg.give = as.data.frame(table(agg_data$agonsim.winner[which(agg_data$focal.individual=="agonsim.winner")]));names(agg.give)[1]='id'
  agg.give = agg.give[-which(nchar(as.character(agg.give$id))>3),]
  hrs.followed.giver = meta_data$hrs.focalfollowed[match(agg.give$id, meta_data$id)]
  #Exclude individuals not in meta data file
  if (length(which(is.na(hrs.followed.giver))) !=0) {
  agg.give = agg.give[-which(is.na(hrs.followed.giver)),]
  hrs.followed.giver = hrs.followed.giver[-which(is.na(hrs.followed.giver))]
  }
  #Add weights
  agg.give$weight = agg.give$Freq/hrs.followed.giver
  
  #AGGRESSION RECEIVE
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
  SocialCapitalData$DSIAgg = TotalAgg/meanAggRate
  
  #####################################################################
  ## For vigilance and sdb rates
  unqIDs = as.character(unique(focal_data$focal.id)); vig.sdb=data.frame(matrix(NA,length(unqIDs), 3)); colnames(vig.sdb)=c("id","vig.freq","sdb.freq")
  for (id in 1:length(unqIDs)){
    vig.sdb$id[id] = unqIDs[id]
    vig.sdb$vig.freq[id] = length(which(focal_data$focal.id == unqIDs[id] & (focal_data$behaviour == "Vigilnce" | focal_data$behaviour == "Vigilnce_overtime")))
    vig.sdb$sdb.freq[id] = length(which(focal_data$focal.id == unqIDs[id] & (focal_data$behaviour == "SelfGrm" | focal_data$behaviour == "SelfGrm_overtime" | focal_data$behaviour == "Scratch" | focal_data$behaviour == "Scratch_overtime")))
  }
  hrs.followed = meta_data$hrs.focalfollowed[match(unqIDs, meta_data$id)]
  if(length(which(is.na(hrs.followed)))!=0) {vig.sdb = vig.sdb[-which(is.na(hrs.followed))]
  hrs.followed = hrs.followed[which(is.na(hrs.followed))]}
  vig.sdb$vig.ra = vig.sdb$vig.freq/hrs.followed; vig.sdb$vig.freq=NULL
  vig.sdb$vig.I = vig.sdb$vig.ra/mean(vig.sdb$vig.ra)
  vig.sdb$sdb.ra = vig.sdb$sdb.freq/hrs.followed; vig.sdb$sdb.freq=NULL
  vig.sdb$sdb.I = vig.sdb$sdb.ra/mean(vig.sdb$sdb.ra)
  
  SocialCapitalData$vig.I = vig.sdb$vig.I[match(meta_data$id,vig.sdb$id)]
  SocialCapitalData$sdb.I = vig.sdb$sdb.I[match(meta_data$id,vig.sdb$id)]
  
  #####################################################################
  ## For Social Network Metrics
  
  rscans = prox_data
  # 2. Find the number of unique IDs.
  #Find all unique IDs
  unqIDs = unique(as.character(rscans$focal.monkey))
  
  # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL(unqIDs)
  
  # 4. Output weighted edgelist from the Master Edgelist.
  options(warn = -1) #set options to ignore all warnings
  action = "prox"; proxdata =1
  weightedEL = calcEdgeList(rscans, masterEL, action, proxdata)
  hrs = meta_data$hrs.focalfollowed[match(weightedEL$alter, meta_data$id)]
  weightedEL$weight <- round(weightedEL$count / hrs, 5) #add weight information by dividing by the #hrs spent observing
  weightedEL$count <- NULL;weightedEL$conc <- NULL #delete those calumn variables
  
  #Need to upload an adjacency matrix, rather than socprog style data...
  adjMat = dils::AdjacencyFromEdgelist(weightedEL)
  data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
  
  #read adjacency matrix
  m=as.matrix(data) # coerces the data set as a matrix
  am.g=graph.adjacency(m,mode="directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
  graph = am.g 
  
  #Get the network measures
  NetworkMetrics = data.frame(matrix(NA, nrow = length(V(graph)), ncol = 6)); names(NetworkMetrics)=c("id","deg","INdeg","OUTdeg","between","eig.cent")
  NetworkMetrics$id = as_ids(V(graph))
  #Weighted degree (Strength, undirected)
  NetworkMetrics$deg<-degree(graph)
  #weighted indegree
  NetworkMetrics$INdeg <-degree(graph,v=V(graph), mode = "in", loops=F)
  #weighted outdegree
  NetworkMetrics$OUTdeg <-degree(graph,v=V(graph), mode = "out", loops=F)
  #Weighted betweenness
  NetworkMetrics$between<-betweenness(graph, v=V(graph), directed=T, normalized=T)
  #Weighted eigenvector centrality
  A <-eigen_centrality(graph, directed=F, scale=T)
  eig.cen = as.data.frame(A["vector"])
  NetworkMetrics$eig.cent = eig.cen$vector

  SocialCapitalData[,c("between","eig.cent")] = NetworkMetrics[match(meta_data$id, NetworkMetrics$id), c("between","eig.cent")]

  ###################################################################
  # Merge and save data
  dprob.ALL[,colnames(SocialCapitalData)[-1]]=NA
  dprob.ALL[,colnames(SocialCapitalData)[-1]] = SocialCapitalData[match(dprob.ALL$id,SocialCapitalData$id),colnames(SocialCapitalData)[-1]]
  
  
  SocialCapital.ALL = rbind(SocialCapital.ALL, dprob.ALL)
}
save( SocialCapital.ALL,file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/SocialCapital.dSocialRates6.RData")
#.2 50 obs per individual, 10obs pre and 10obs post
#.3 One obervation per individual
#.4 10 observation per individual but only include individuals with at least 20 observations.
#.5 50 obserbation pre & post per individual, only include individuals with at least 10 observations