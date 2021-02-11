########## FUNCTIONS ##########

#FUNCTION 1
# Computes the log likelihood of a given combination of parameters {a,b} given the data in vectors si and di
#Inputs:
#	a, b: parameters of the beta distribution (to be used as the prior)
#	si: number of observations for each dyad (vector)
#	di: number of positive observations for each dyad (vector) - i.e. when they were observed together
#Output:
#	likeli <- log likelihood P(di | a, b, si )
#		given by SUM_i log (si choose di) * Z(a+di,b+si-di) / Z(a,b) ]
#		where Z(a,b) is the beta function
ml2.likeli <- function(a,b,si,di){
  s.choose.d <- apply(cbind(si,di),1,function(x){return(choose(x[1],x[2]))})
  beta1 <- apply(cbind(si,di),1,function(x){return(beta(a+x[2],b+x[1]-x[2]))})
  likeli <- sum(log(s.choose.d) + log(beta1) - log(rep(beta(a,b),length(si))))
  return(likeli) 
}

#FUNCTION 2
adjacency.from.scans <- function(rscans, unqIDs){
  
  #Find the number of scans per individual (to correct for unequal representation later)
  numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
  
  #Get Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL(unqIDs)
  
  #Get count of grooming occurrences
  options(warn = -1) #set options to ignore all warnings
  weightedEL = calcEdgeList(rscans,masterEL)
  # weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
  # weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
  weightedEL = weightedEL[,c("alter","ego","count")] #transform output to equal proximity data output.
  
  #Get weights (divided by number of scans)
  weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])
  weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by the #observed scans
  weightedEL$weight[is.na(weightedEL$weight)]=0 #If individual is absent (NA), just set his weights to 0
  weightedEL$numscans[is.na(weightedEL$numscans)]=0
  # # Standardize weights by dividing by the mean weight. Thus we get relative weights, which take into account group/year differences.
  # meanweight = mean(weightedEL$weight[weightedEL$weight>0]) #compute nonzero mean weight
  # weightedEL$weight <- weightedEL$weight/meanweight
  
  #Get observed graph
  obs.graph = as.matrix(netdiffuseR::edgelist_to_adjmat(weightedEL[,c("alter","ego")],w=weightedEL$weight, undirected=T))# create adjacency matrix based on edge list.
  # = adjMat[["adjacency"]]
  
  both.groom = as.matrix(netdiffuseR::edgelist_to_adjmat(weightedEL[,c("alter","ego")],w=weightedEL$count, undirected=T))# create adjacency matrix based on edge list.
  # adjMat2 = dils::AdjacencyFromEdgelist(weightedEL[,c("alter","ego","count")])# create adjacency matrix based on edge list.
  # both.groom = adjMat2[["adjacency"]]; diag(both.groom) <- 0
  
  either.observed = as.matrix(netdiffuseR::edgelist_to_adjmat(weightedEL[,c("alter","ego")],w=weightedEL$numscans, undirected=T))# create adjacency matrix based on edge list.
  # adjMat3 = dils::AdjacencyFromEdgelist(weightedEL[,c("alter","ego","numscans")])# create adjacency matrix based on edge list.
  # either.observed = adjMat3[["adjacency"]];
  
  return(list(obs.graph, both.groom, either.observed))
}

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
  a = cbind(focalID, partners, rscans$isSocialGive, rscans$isSocialGet, rscans$isSocial)
  
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
#####################################################################

# FUNCTION 3
#Output the NON-DIRECTIONAL Master Edgelist of all possible pairs given the unique IDs
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

# FUNCTION 4
calcEdgeList    <- function(rscans, masterEL){
  
  partners = str_split(rscans$partner.ID, c(","), simplify = TRUE) #split proximity partner by ","
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
