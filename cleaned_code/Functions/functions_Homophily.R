
###############################################
#Functions to generate networks from unique IDs
###############################################


#1. Output the NON-DIRECTIONAL Master Edgelist of all possible pairs given the unique IDs
calcMasterEL    <- function(unqIDs){ #unq IDs only include focal individuals. I.e. We will only take into account focal individuals in our social networks.
  ego <- NULL; alter <- NULL #Initialize ego and alter
  for(i in 1:length(unqIDs)){ #for all unique IDs
    #Create a list of pairsfor each individual and all other IDs, directionality does not matter
    ego <- append(ego, rep(unqIDs[i], length(unqIDs) - i)) #append: add to the variable "ego"
    alter   <- append(alter  , unqIDs[(i+1):length(unqIDs)])
  }
  alter <- alter[1:length(ego)]#Make sure ego and alter are the same length
  
  masterEL <- data.frame(ego, alter) #combine ego and alter
  masterEL$conc <- paste(masterEL[,1],masterEL[,2],sep=".") #create "pair" or "edge" column 
  masterEL$count <- 0 #initialize count for each pair
  
  return(masterEL)
}

#2. Calculate NON-DIRECTIONAL Edgelist (for proximity data)
calcEdgeList    <- function(rscans, masterEL){
  
  partners = str_split(rscans$in.proximity, c(","), simplify = TRUE) #split proximity partner by ","
  focalID = as.character(rscans$focalID)
  a = cbind(focalID,partners) #bind focal ID and partner together
  
  PP <- NULL  
  for(ii in 1:nrow(a)){ #for all observations
    for(p in 2:ncol(a)){ #for all proximity partners (not counting the first column, which is the focal ID column)
      if(!is.na(a[ii,p])) { #if not NA
        if(a[ii,p] != "" ) { #if not empty
          S1 <- data.frame(ego = as.character(a[ii, 1]), #ego is the focal ID
                           alter = as.character(a[ii, p])) #alter for proximity partners. There will be a separate row for each partner. 
          PP <- dplyr::bind_rows(PP, S1) #bind rows, adds values to PP
        }
      }
    }
  }
  #create an edge in each direction, since the relationship is NOT directional. Both "directions" should count.
  PP$conc1 <- paste(str_trim(PP$alter), str_trim(PP$ego), sep=".") 
  PP$conc2 <- paste(str_trim(PP$ego), str_trim(PP$alter), sep=".")
  head(PP)
  head(masterEL)  
  
  for(i in 1:nrow(PP)){ #for all pairs
    if(PP$conc1[i] %in% masterEL$conc){ #If this edge exists in the master list
      masterEL$count[which(masterEL$conc == PP$conc1[i])] <- masterEL$count[which(masterEL$conc == PP$conc1[i])] +1 #Find the index and add counts
    }
    
    if(PP$conc2[i] %in% masterEL$conc){
      masterEL$count[which(masterEL$conc == PP$conc2[i])] <- masterEL$count[which(masterEL$conc == PP$conc2[i])] +1
    }
  }
  
  return(masterEL)
}

#3. Calculate weights for non-directional network from edgelist
calcWeights    <- function(PP, masterEL){
  PP$conc1 <- paste(str_trim(PP$alter), str_trim(PP$ego), sep=".") 
  PP$conc2 <- paste(str_trim(PP$ego), str_trim(PP$alter), sep=".")
  for(i in 1:nrow(PP)){ #for all pairs
    if(PP$conc1[i] %in% masterEL$conc){ #If this edge exists in the master list
      masterEL$count[which(masterEL$conc == PP$conc1[i])] <- masterEL$count[which(masterEL$conc == PP$conc1[i])] +1 #Find the index and add counts
    }
    
    if(PP$conc2[i] %in% masterEL$conc){
      masterEL$count[which(masterEL$conc == PP$conc2[i])] <- masterEL$count[which(masterEL$conc == PP$conc2[i])] +1
    }
  }
  
  return(masterEL)
}
###############################################
#For directed  graph (grooming data)

#1. Output the Master Edgelist of all possible pairs given the unique IDs for grooming data (directional)
calcMasterEL_groom    <- function(unqIDs){ #unq IDs only include focal individuals. I.e. We will take into account focal individuals in our social networks.
  givingID <- NULL; receivingID <- NULL
  for(i in 1:length(unqIDs)){
    givingID <- append(givingID, rep(unqIDs[i], length(unqIDs) - 1)) #append: add to the variable "givingID"
    if (i==1) {receivingID   <- append(receivingID  , unqIDs[2:length(unqIDs)]) }
    else {receivingID   <- append(receivingID  , unqIDs[c(1:i-1,(i+1):length(unqIDs))])  }
  }
  receivingID <- receivingID[1:length(givingID)]
  
  masterEL <- data.frame(givingID,receivingID)
  masterEL$conc <- paste(masterEL[,1],masterEL[,2],sep=".")
  masterEL$count <- 0
  
  return(masterEL)
}

#2. Calculate DIRECTIONAL Edgelist
calcEdgeList_groom    <- function(rscans, masterEL){
  
  partners = str_split(rscans$partner.ID, c(","), simplify = TRUE)
  focalID = as.character(rscans$focalID)
  a = cbind(focalID,partners,rscans$isSocialGive, rscans$isSocialGet,rscans$isSocial)
  
  PP <- NULL;count1=0; count2=0; count3=0  
  for(ii in 1:nrow(a)){ #for all observations
    if(!is.na(a[ii,2])) { #if not NA
      if(a[ii,ncol(a)] == 1 ) { #if isSocial
        if(a[ii,ncol(partners)+2] == 1){count1=count1+1; S1 <- data.frame(givingID = as.character(a[ii, 1]), receivingID = as.character(a[ii, 2]))}
        else if (a[ii,ncol(partners)+3] == 1) {count2=count2+1; S1 <- data.frame(givingID = as.character(a[ii, 2]), receivingID = as.character(a[ii, 1]))}
        else {count3=count3+1}#; S1 <- data.frame(givingID = as.character(a[ii, 2]), receivingID = as.character(a[ii, 1]))}
        PP <- dplyr::bind_rows(PP, S1) #bind rows, adds values to PP
      }
    }
  }
  PP$conc <- paste(str_trim(PP$givingID), str_trim(PP$receivingID), sep=".")#creates directed edges
  head(PP)
  head(masterEL)  
  
  for(i in 1:nrow(PP)){
    if(PP$conc[i] %in% masterEL$conc){ #If this edge exists in the master list
      masterEL$count[which(masterEL$conc == PP$conc[i])] <- masterEL$count[which(masterEL$conc == PP$conc[i])] +1 #Find the index and add counts
    }
  }
  
  return(masterEL)
}

#3. Calculate weights for non-directional network from edgelist
calcWeights_groom    <- function(PP, masterEL){
  for(i in 1:nrow(PP)){
    if(PP$conc[i] %in% masterEL$conc){ #If this edge exists in the master list
      masterEL$count[which(masterEL$conc == PP$conc[i])] <- masterEL$count[which(masterEL$conc == PP$conc[i])] +1 #Find the index and add counts
    }
  }
  
  return(masterEL)
}
###############################################
#Caculate partner preference
###############################################

# Calculate sex-based joint-counts
calcSexProps <- function(el, rscans){

  # Classifying pairs as FF, MM or Opposite
  el$sexGivingID   <- rscans$sex[match(as.character(el$alter), as.character(rscans$focalID))]
  el$sexReceivingID <- rscans$sex[match(as.character(el$ego), rscans$focalID)]
  # el$pairClass  <- "opp"; el$pairClass[which(el$sexGivingID == "F" & el$sexReceivingID == "F")] <- "bothFem"; el$pairClass[which(el$sexGivingID == "M" & el$sexReceivingID == "M")] <- "bothMal"
  el$pairClass  <- ifelse(el$sexGivingID != el$sexReceivingID, "opp","same")
    
  # Calculating Observed Sex Pair weights
  # weight.FF     <- sum(el$weight[el$pairClass=="bothFem"]) #sum of all the weights for class FF
  # weight.MM     <- sum(el$weight[el$pairClass=="bothMal"]) #sum of all the weights for class MM                       
  # weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class MF
  weight.opp  <- sum(el$weight[el$pairClass=="opp"])
  weight.same  <- sum(el$weight[el$pairClass=="same"])
  
  # FF     <- length(which(el$pairClass=="bothFem")) #sum of all the weights for class FF
  # MM     <- length(which(el$pairClass=="bothMal")) #sum of all the weights for class MM                       
  # cross  <- length(which(el$pairClass=="opp")) #sum of all the weights for class MF 
  opp  <- length(which(el$pairClass=="opp"))
  same  <- length(which(el$pairClass=="same"))
  
  allIDs = unique(c(as.character(el$alter),as.character(el$ego)))
  allIDs.sex = as.character(rscans$sex[match(as.character(allIDs), as.character(rscans$focalID))])
  dens = nrow(el)/ (length(allIDs.sex)^2- length(allIDs.sex))
  #Generate random graph with equal density and distribution of attributes
  # rand.el = as.data.frame(as_edgelist(erdos.renyi.game(length(allIDs), nrow(el), type="gnm",directed=T))) #generate random graph given number of edges
  rand.el = as.data.frame(as_edgelist(erdos.renyi.game(length(allIDs), dens, type="gnp",directed=T))) #generate random graph given density (or p(edge) between any two vertices)
  names(rand.el)=c("alter","ego"); rand.el$weight = el$weight[sample(1:nrow(el), nrow(rand.el), replace=T)] #add weights
  rand.el$sexGivingID   <- allIDs.sex[rand.el$alter]
  rand.el$sexReceivingID <- allIDs.sex[rand.el$ego]
  # rand.el$pairClass  <- "opp";  rand.el$pairClass[which( rand.el$sexGivingID == "F" &  rand.el$sexReceivingID == "F")] <- "bothFem";  rand.el$pairClass[which( rand.el$sexGivingID == "M" &  rand.el$sexReceivingID == "M")] <- "bothMal"
  rand.el$pairClass  <- ifelse(rand.el$sexGivingID != rand.el$sexReceivingID, "opp","same")
  
  # Calculating Sex Pair weights from random
  # rand.weight.FF     <- sum(rand.el$weight[rand.el$pairClass=="bothFem"]) #sum of all the weights for class FF
  # rand.weight.MM     <- sum(rand.el$weight[rand.el$pairClass=="bothMal"]) #sum of all the weights for class MM                       
  # rand.weight.cross  <- sum(rand.el$weight[rand.el$pairClass=="opp"]) #sum of all the weights for class MF 
  rand.weight.opp  <- sum(rand.el$weight[el$pairClass=="opp"])
  rand.weight.same  <- sum(rand.el$weight[el$pairClass=="same"])
  
  # rand.FF     <- length(which(rand.el$pairClass=="bothFem")) #sum of all the weights for class FF
  # rand.MM     <- length(which(rand.el$pairClass=="bothMal")) #sum of all the weights for class MM                       
  # rand.cross  <- length(which(rand.el$pairClass=="opp")) #sum of all the weights for class MF
  rand.opp  <- length(which(rand.el$pairClass=="opp"))
  rand.same  <- length(which(rand.el$pairClass=="same"))
  
  #Compute expected proportions of pair classes if the network was completely random
  possFemPairs        <-    (length(which(allIDs.sex=="F"))^2)      - length(which(allIDs.sex=="F")) #all possible FF pairs
  possMalPairs        <-    (length(which(allIDs.sex=="M"))^2)      - length(which(allIDs.sex=="M")) #all possible MM pairs
  allPossiblePairs    <-    (length(allIDs.sex)^2)      - length(allIDs.sex) #all possible pairs
  possSamePairs       <-    (possFemPairs + possMalPairs)
  possCrossPairs      <-    allPossiblePairs - (possFemPairs + possMalPairs) #all possible MF pairs

  #Weighted expected proportions
  # exp.FF      <- (possFemPairs/allPossiblePairs)    * sum(el$weight)
  # exp.MM      <- (possMalPairs/allPossiblePairs)    * sum(el$weight)
  # exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.opp   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.same   <- (possSamePairs/allPossiblePairs)  * sum(el$weight)

  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  # eo.FF     <- weight.FF/exp.FF
  # eo.MM     <- weight.MM/exp.MM
  # eo.sexCross  <- weight.cross/exp.cross
  eo.same.sex  <- weight.same/exp.same
  eo.opp.sex   <- weight.opp/exp.opp

  #Compute assortativity index
  # create igraph object
  ig <- simplify(graph.data.frame(d=el, directed = T), #create UNDIRECTED igraph object 
                 remove.loops = T) #simple graph with no loop
  ig <- delete_edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0 [delete.edges]
  # Add Sex to IG
  ig <-set_vertex_attr(ig, name = "isFemale", value = as.character(rscans$sex[match(as.character(V(ig)$name), as.character(rscans$focalID))]))#set.vertex.attribute
  assort.index.sex = assortativity(ig, as.factor(vertex_attr(ig,"isFemale")), types2 = NULL, directed = TRUE)
  
  # sexPairStats <- data.frame(eo.FF, eo.MM, eo.sexCross)
  # sexPairStats <- data.frame(FF, MM, cross, rand.FF, rand.MM, rand.cross,
  #                            weight.FF, weight.MM, weight.cross, rand.weight.FF, rand.weight.MM, rand.weight.cross, 
  #                            exp.FF, exp.MM, exp.cross, eo.FF, eo.MM, eo.sexCross)
  # sexPairStats <- data.frame(opp, same, rand.opp, rand.same, weight.opp, weight.same, 
  #                            rand.weight.opp, rand.weight.same, 
  #                            exp.opp, exp.same, eo.opp, eo.same, assort.index)
  sexPairStats <- data.frame(eo.same.sex, eo.opp.sex, assort.index.sex)

  return(sexPairStats)        
}

# Calculate sex-based joint-counts
calcAgeProps <- function(el, rscans){
  # Classifying pairs as OO, YY or Opposite
  allIDs = unique(c(as.character(el$alter),as.character(el$ego)))
  allIDs.age = as.character(rscans$age.cat[match(as.character(allIDs), as.character(rscans$focalID))])
  el$ageGivingID   <- rscans$age.cat[match(as.character(el$alter), as.character(rscans$focalID))]
  el$ageReceivingID <- rscans$age.cat[match(as.character(el$ego), rscans$focalID)]
  # el$pairClass  <- "opp"; el$pairClass[which(el$ageGivingID == "Y" & el$ageReceivingID == "Y")] <- "bothYoung"; el$pairClass[which(el$ageGivingID == "O" & el$ageReceivingID == "O")] <- "bothOld"
  el$pairClass  <- ifelse(el$ageGivingID != el$ageReceivingID, "opp","same")
  
  # Calculating Sex Pair weights
  # weight.YY     <- sum(el$weight[el$pairClass=="bothYoung"]) #sum of all the weights for class FF
  # weight.OO     <- sum(el$weight[el$pairClass=="bothOld"]) #sum of all the weights for class MM                       
  # weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class MF 
  weight.opp  <- sum(el$weight[el$pairClass=="opp"])
  weight.same  <- sum(el$weight[el$pairClass=="same"])
  
  #Compute expected proportions of pair classes if the network was completely random
  possOldPairs        <-    (length(which(allIDs.age=="O"))^2)      - length(which(allIDs.age=="O")) #all possible OO pairs
  possYoungPairs      <-    (length(which(allIDs.age=="Y"))^2)      - length(which(allIDs.age=="Y")) #all possible YY pairs
  allPossiblePairs    <-    (length(allIDs.age)^2)      - length(allIDs.age) #all possible pairs
  possSamePairs       <-    (possOldPairs + possYoungPairs)
  possCrossPairs      <-    allPossiblePairs - (possOldPairs + possYoungPairs) #all possible OY pairs
  
  #Weighted expected proportions
  # exp.YY      <- (possYoungPairs/allPossiblePairs)    * sum(el$weight)
  # exp.OO      <- (possOldPairs/allPossiblePairs)    * sum(el$weight)
  # exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.opp   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.same   <- (possSamePairs/allPossiblePairs)  * sum(el$weight)
  
  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  # eo.YY     <- weight.YY/exp.YY
  # eo.OO     <- weight.OO/exp.OO
  # eo.ageCross  <- weight.cross/exp.cross
  eo.same.age <- weight.same/exp.same
  eo.opp.age   <- weight.opp/exp.opp
  
  #Compute assortativity index
  # create igraph object
  ig <- simplify(graph.data.frame(d=el, directed = T), #create UNDIRECTED igraph object 
                 remove.loops = T) #simple graph with no loop
  ig <- delete_edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0 [delete.edges]
  # Add Sex to IG
  ig <-set_vertex_attr(ig, name = "Age", value = as.numeric(rscans$age[match(as.character(V(ig)$name), as.character(rscans$focalID))]))#set.vertex.attribute
  assort.index.age = assortativity(ig, as.factor(vertex_attr(ig,"Age")), types2 = NULL, directed = TRUE)
  
  # agePairStats <- data.frame(eo.YY, eo.OO, eo.ageCross)
  agePairStats <- data.frame(eo.same.age, eo.opp.age, assort.index.age)
  
  return(agePairStats)        
}

# Calculate kin-based joint-counts
calcKinProps <- function(el, ped){
  
  KC      <- NULL; for(i in 1:length(el[,1])){ 
    KC[i] <-  ped[which(rownames(ped)==as.character(el$ego[i])) , which(colnames(ped)==as.character(el$alter[i]))]
  }
  el$KC   <- round(KC, 4)
  el$pairClass <- "unrelated"
  # el$pairClass[which(el$KC >= .125 & el$KC < .25)] <- "dRel"
  el$pairClass[which(el$KC >= .25)] <- "rel"
  
  #Compute observed weight of related pair classes
  obs.ck     <- sum( el$weight[el$pairClass =="rel"])
  # obs.dk     <- sum( el$weight[el$pairClass =="dRel"])
  obs.u      <- sum( el$weight[el$pairClass =="unrelated"])
  
  #Computer expected weight of related pair classes based on the distribution of kinship relationship in the whole population
  ckPairs    <- length(which(ped  >= .25))               ; exp.ck      <- (ckPairs   / length(ped))   * sum(el$weight)
  # dkPairs    <- length(which(ped  >= .125 & ped <.25))   ; exp.dk      <- (dkPairs   / length(ped))   * sum(el$weight)
  uPairs     <- length(which(ped  < .25))              ; exp.u       <- (uPairs    / length(ped))   * sum(el$weight)
  
  #Compute observed/expected ratio
  eo.ck      <- obs.ck    /   exp.ck
  # eo.dk      <- obs.dk    /   exp.dk
  eo.u       <- obs.u     /   exp.u
  
  el$weightKC    <- el$weight * el$KC
  kinDegree      <- sum(el$weightKC) / sum(el$weight)
  
  kinPairStats <- data.frame(eo.ck, eo.u, kinDegree)
  
  return(kinPairStats)
}

# Calculate Rank-based joint-counts
calcRankProps <- function(el, dominance_info, year, rscans){
  
  #Set high rank vs low rank
  dominance_info$ORD_RANK2 = "L"
  dominance_info$ORD_RANK2[which(dominance_info$X.DOMINATED>=70)]="H"
  # Classifying pairs as HH, LL or Opposite
  el$RankEgo   <- dominance_info$ORD_RANK2[match(paste(as.character(el$ego),year,sep=""), as.character(dominance_info$IDyear))]
  el$RankAlter <- dominance_info$ORD_RANK2[match(paste(as.character(el$alter),year,sep=""), as.character(dominance_info$IDyear))]
  # el$pairClass  <- "opp"; el$pairClass[which(el$RankEgo == "H" & el$RankAlter == "H")] <- "bothH"; el$pairClass[which(el$RankEgo == "L" & el$RankAlter == "L")] <- "bothL"
  el$pairClass  <- ifelse(el$RankEgo != el$RankAlter, "opp","same")
  
  # Calculating Rank Pair weights
  # weight.HH     <- sum(el$weight[el$pairClass=="bothH"]) #sum of all the weights for class FF
  # weight.LL     <- sum(el$weight[el$pairClass=="bothL"]) #sum of all the weights for class MM                       
  # weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class MF  
  weight.opp  <- sum(el$weight[el$pairClass=="opp"])
  weight.same  <- sum(el$weight[el$pairClass=="same"])
  
  #Compute expected proportions of pair classes if the network was completely random
  allIDs = unique(c(as.character(el$alter),as.character(el$ego)))
  nodes = data.frame(matrix(0, nrow = length(allIDs), ncol = 2)); names(nodes)=c("id","rank")
  nodes$id =  allIDs
  nodes$rank = dominance_info$ORD_RANK2[match(paste(nodes$id,year,sep=""), as.character(dominance_info$IDyear))]
  
  possHHPairs        <-    (length(which(nodes$rank=="H"))^2) - length(which(nodes$rank=="H")) #all possible HH pairs
  possLLPairs        <-    (length(which(nodes$rank=="L"))^2) - length(which(nodes$rank=="L"))  #all possible LL pairs
  allPossiblePairs   <-    (length(allIDs)^2)      - length(allIDs) #all possible pairs
  possSamePairs       <-   (possHHPairs + possLLPairs)
  possCrossPairs     <-    allPossiblePairs - (possHHPairs + possLLPairs) #all possible HL pairs
  
  #Weighted expected proportions
  # exp.HH      <- (possHHPairs/allPossiblePairs)    * sum(el$weight)
  # exp.LL      <- (possLLPairs/allPossiblePairs)    * sum(el$weight)
  # exp.cross  <- (possCrossPairs/allPossiblePairs) * sum(el$weight)
  exp.opp   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.same   <- (possSamePairs/allPossiblePairs)  * sum(el$weight)
  
  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  # eo.HH     <- weight.HH/exp.HH
  # eo.LL     <- weight.LL/exp.LL
  # eo.rankCross <- weight.cross/exp.cross
  eo.same.rank  <- weight.same/exp.same
  eo.opp.rank   <- weight.opp/exp.opp
  
  #Compute assortativity index
  # create igraph object
  ig <- simplify(graph.data.frame(d=el, directed = T), #create UNDIRECTED igraph object 
                 remove.loops = T) #simple graph with no loop
  ig <- delete_edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0 [delete.edges]
  # Add Rank to IG
  ig <-set_vertex_attr(ig, name = "Rank", value = as.numeric(rscans$percentrank[match(as.character(V(ig)$name), as.character(nodes$id))]))#set.vertex.attribute
  assort.index.rank = assortativity(ig, as.factor(vertex_attr(ig,"Rank")), types2 = NULL, directed = TRUE)
  
  # RankPairStats <- data.frame(eo.HH, eo.LL, eo.rankCross)
  RankPairStats <- data.frame(eo.same.rank, eo.opp.rank, assort.index.rank)
  
  return(RankPairStats)
}

# Calculate Groom-based joint-counts
calcGroomProps <- function(el, SocialCapital, threshold){
  
  threshold=as.numeric(quantile(SocialCapital.ALL$std.DSIgroom, probs = 0.70))
  groom.strength.GiveID = SocialCapital$std.DSIgroom[match(as.character(el$alter),as.character(SocialCapital$id))]
  el$GroomGivingID <- ifelse(groom.strength.GiveID<threshold,"shy","greg")
  groom.strength.GetID = SocialCapital$std.DSIgroom[match(as.character(el$ego),as.character(SocialCapital$id))]
  el$GroomReceivingID <- ifelse(groom.strength.GetID<threshold,"shy","greg")
  # el$pairClass  <- "opp"; el$pairClass[which(el$GroomGivingID == "shy" & el$GroomReceivingID == "shy")] <- "bothShy"; el$pairClass[which(el$GroomGivingID == "greg" & el$GroomReceivingID == "greg")] <- "bothGreg"
  el$pairClass  <- ifelse(el$GroomGivingID != el$GroomReceivingID, "opp","same")
  
  # Calculating Groom Pair weights
  # weight.SS     <- sum(el$weight[el$pairClass=="bothShy"]) #sum of all the weights for class SS
  # weight.GG     <- sum(el$weight[el$pairClass=="bothGreg"]) #sum of all the weights for class GG                       
  # weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class SG 
  weight.opp  <- sum(el$weight[el$pairClass=="opp"])
  weight.same  <- sum(el$weight[el$pairClass=="same"])
  
  allIDs = unique(c(as.character(el$alter),as.character(el$ego))); allIDs.groom="greg"
  allIDs.groom[SocialCapital$std.DSIgroom[match(as.character(allIDs),as.character(SocialCapital$id))]>=threshold]="greg"
  allIDs.groom[SocialCapital$std.DSIgroom[match(as.character(allIDs),as.character(SocialCapital$id))]<threshold]="shy"
  #Compute expected proportions of pair classes if the network was completely random
  possShyPairs        <-    (length(which(allIDs.groom=="shy"))^2)      - length(which(allIDs.groom=="shy")) #all possible OO pairs
  possGregPairs        <-    (length(which(allIDs.groom=="greg"))^2)      - length(which(allIDs.groom=="greg")) #all possible YY pairs
  allPossiblePairs    <-    (length(allIDs.groom)^2)      - length(allIDs.groom) #all possible pairs
  possSamePairs       <-    (possShyPairs + possGregPairs)
  possCrossPairs      <-    allPossiblePairs - (possShyPairs + possGregPairs) #all possible OY pairs
  
  #Weighted expected proportions
  # exp.SS      <- (possShyPairs/allPossiblePairs)    * sum(el$weight)
  # exp.GG      <- (possGregPairs/allPossiblePairs)    * sum(el$weight)
  # exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.opp   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.same   <- (possSamePairs/allPossiblePairs)  * sum(el$weight)
  
  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  # eo.SS     <- weight.SS/exp.SS
  # eo.GG     <- weight.GG/exp.GG
  # eo.groomCross  <- weight.cross/exp.cross
  eo.same.groom  <- weight.same/exp.same
  eo.opp.groom   <- weight.opp/exp.opp
  
  #Compute assortativity index
  # create igraph object
  ig <- simplify(graph.data.frame(d=el, directed = F), #create UNDIRECTED igraph object 
                 remove.loops = T) #simple graph with no loop
  ig <- delete_edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0 [delete.edges]
  # Add Sex to IG
  ig <-set_vertex_attr(ig, name = "Groom", value = as.numeric(SocialCapital$std.DSIgroom[match(as.character(V(ig)$name), as.character(SocialCapital$id))]))#set.vertex.attribute
  assort.index.groom = assortativity(ig, as.factor(vertex_attr(ig,"Groom")), types2 = NULL, directed = TRUE)
  
  groomPairStats <- data.frame(eo.same.groom, eo.opp.groom, assort.index.groom)
}

# Calculate numP-based joint-counts
calcNumpProps <- function(el, SocialCapital, threshold){
  
  numP.strength.GiveID = SocialCapital$std.numPartnersGroom[match(as.character(el$alter),as.character(SocialCapital$id))]
  el$numPGivingID <- ifelse(numP.strength.GiveID<threshold,"shyP","gregP")
  numP.strength.GetID = SocialCapital$std.numPartnersGroom[match(as.character(el$ego),as.character(SocialCapital$id))]
  el$numPReceivingID <- ifelse(numP.strength.GetID<threshold,"shyP","gregP")
  # el$pairClass  <- "opp"; el$pairClass[which(el$numPGivingID == "shyP" & el$numPReceivingID == "shyP")] <- "bothLowP"; el$pairClass[which(el$numPGivingID == "gregP" & el$numPReceivingID == "gregP")] <- "bothHighP"
  el$pairClass  <- ifelse(el$GroomGivingID != el$GroomReceivingID, "opp","same")
  
  # Calculating num partners Pair weights
  # weight.SS     <- sum(el$weight[el$pairClass=="bothLowP"]) #sum of all the weights for class SS
  # weight.GG     <- sum(el$weight[el$pairClass=="bothHighP"]) #sum of all the weights for class GG                       
  # weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class SG 
  weight.opp  <- sum(el$weight[el$pairClass=="opp"])
  weight.same  <- sum(el$weight[el$pairClass=="same"])
  
  allIDs = unique(c(as.character(el$alter),as.character(el$ego)));allIDs.numP="gregP"
  allIDs.numP[SocialCapital$std.numPartnersGroom[match(as.character(allIDs),as.character(SocialCapital$id))]>=threshold]="gregP"
  allIDs.numP[SocialCapital$std.numPartnersGroom[match(as.character(allIDs),as.character(SocialCapital$id))]<threshold]="shyP"

  #Compute expected proportions of pair classes if the network was completely random
  possShyPairs        <-    (length(which(allIDs.numP=="shyP"))^2)      - length(which(allIDs.numP=="shyP")) #all possible OO pairs
  possGregPairs        <-    (length(which(allIDs.numP=="gregP"))^2)      - length(which(allIDs.numP=="gregP")) #all possible YY pairs
  allPossiblePairs    <-    (length(allIDs.numP)^2)      - length(allIDs.numP) #all possible pairs
  possSamePaire       <-    (possShyPairs + possGregPairs)
  possCrossPairs      <-    allPossiblePairs - (possShyPairs + possGregPairs) #all possible OY pairs
  
  #Weighted expected proportions
  # exp.SS      <- (possShyPairs/allPossiblePairs)    * sum(el$weight)
  # exp.GG      <- (possGregPairs/allPossiblePairs)    * sum(el$weight)
  # exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.opp   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.same   <- (possSamePairs/allPossiblePairs)  * sum(el$weight)
  
  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  # eo.SSpartners     <- weight.SS/exp.SS
  # eo.GGpartners     <- weight.GG/exp.GG
  # eo.NumpCross  <- weight.cross/exp.cross
  eo.same.numP  <- weight.same/exp.same
  eo.opp.numP   <- weight.opp/exp.opp
  
  #Compute assortativity index
  # create igraph object
  ig <- simplify(graph.data.frame(d=el, directed = T), #create DIRECTED igraph object 
                 remove.loops = T) #simple graph with no loop
  ig <- delete_edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0 [delete.edges]
  # Add numP to IG
  ig <-set_vertex_attr(ig, name = "numP", value = as.character(SocialCapital$std.numPartnersGroom[match(as.character(V(ig)$name), as.character(SocialCapital$id))]))#set.vertex.attribute
  assort.index.numP = assortativity(ig, as.factor(vertex_attr(ig,"numP")), types2 = NULL, directed = TRUE)
  
  # NumpPairStats <- data.frame(eo.SSpartners, eo.GGpartners, eo.NumpCross)
  NumpPairStats <- data.frame(eo.same.numP, eo.opp.numP, assort.index.numP)
}

# Calculate eigenvector centrality-based joint-counts
calcEigcentProps <- function(el, SocialCapital, threshold){
  
  EigCent.strength.GiveID = SocialCapital$eig.cent.groom[match(as.character(el$alter),as.character(SocialCapital$id))]
  el$EigCentGivingID <- ifelse(EigCent.strength.GiveID<threshold,"ecL","ecH")
  EigCent.strength.GetID = SocialCapital$eig.cent.groom[match(as.character(el$ego),as.character(SocialCapital$id))]
  el$EigCentReceivingID <- ifelse(EigCent.strength.GetID<threshold,"ecL","ecH")
  # el$pairClass  <- "opp"; el$pairClass[which(el$EigCentGivingID == "ecL" & el$EigCentReceivingID == "ecL")] <- "bothLowEC"; el$pairClass[which(el$EigCentGivingID == "ecH" & el$EigCentReceivingID == "ecH")] <- "bothHighEC"
  el$pairClass  <- ifelse(el$GroomGivingID != el$GroomReceivingID, "opp","same")
  
  # Calculating num partners Pair weights
  # weight.SS     <- sum(el$weight[el$pairClass=="bothLowEC"]) #sum of all the weights for class SS
  # weight.GG     <- sum(el$weight[el$pairClass=="bothHighEC"]) #sum of all the weights for class GG                       
  # weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class SG 
  weight.opp  <- sum(el$weight[el$pairClass=="opp"])
  weight.same  <- sum(el$weight[el$pairClass=="same"])
  
  allIDs = unique(c(as.character(el$alter),as.character(el$ego)));allIDs.eigcent="ecH"
  allIDs.eigcent[SocialCapital$eig.cent.groom[match(as.character(allIDs),as.character(SocialCapital$id))]>=threshold]="ecH"
  allIDs.eigcent[SocialCapital$eig.cent.groom[match(as.character(allIDs),as.character(SocialCapital$id))]<threshold]="ecL"
  #Compute expected proportions of pair classes if the network was completely random
  possShyPairs        <-    (length(which(allIDs.eigcent=="ecL"))^2)      - length(which(allIDs.eigcent=="ecL")) #all possible SS pairs
  possGregPairs        <-    (length(which(allIDs.eigcent=="ecH"))^2)      - length(which(allIDs.eigcent=="ecH")) #all possible GG pairs
  allPossiblePairs    <-    (length(allIDs.eigcent)^2)      - length(allIDs.eigcent) #all possible pairs
  possSamePairs       <-    (possShyPairs + possGregPairs)
  possCrossPairs      <-    allPossiblePairs - (possShyPairs + possGregPairs) #all possible GS pairs
  
  #Weighted expected proportions
  # exp.SS      <- (possShyPairs/allPossiblePairs)    * sum(el$weight)
  # exp.GG      <- (possGregPairs/allPossiblePairs)    * sum(el$weight)
  # exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.opp   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
  exp.same   <- (possSamePairs/allPossiblePairs)  * sum(el$weight)
  
  #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
  # eo.SSeigcent     <- weight.SS/exp.SS
  # eo.GGeigcent     <- weight.GG/exp.GG
  # eo.EigcentCross  <- weight.cross/exp.cross
  eo.same.eigCent  <- weight.same/exp.same
  eo.opp.eigCent   <- weight.opp/exp.opp
  
  #Compute assortativity index
  # create igraph object
  ig <- simplify(graph.data.frame(d=el, directed = F), #create UNDIRECTED igraph object 
                 remove.loops = T) #simple graph with no loop
  ig <- delete_edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0 [delete.edges]
  # Add Sex to IG
  ig <-set_vertex_attr(ig, name = "EigCentGroom", value = as.character(SocialCapital$eig.cent.groom[match(as.character(V(ig)$name), as.character(SocialCapital$id))]))#set.vertex.attribute
  assort.index.eigCent = assortativity(ig, as.factor(vertex_attr(ig,"EigCentGroom")), types2 = NULL, directed = TRUE)
  
  # EigcentPairStats <- data.frame(eo.SSeigcent, eo.GGeigcent, eo.EigcentCross)
  EigcentPairStats <- data.frame(eo.same.eigCent, eo.opp.eigCent, assort.index.eigCent)
}