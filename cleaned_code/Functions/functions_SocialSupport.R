###############################################
#Functions to generate networks from unique IDs
###############################################

###############################################
# For undirected graph (proximity data)


#1. Output the NON-DIRECTIONAL Master Edgelist of all possible pairs given the unique IDs
calcMasterEL    <- function(unqIDs){ #unq IDs only include focal individuals. I.e. We will take into account focal individuals in our social networks.
  ego <- NULL; alter <- NULL
  for(i in 1:length(unqIDs)){
    ego <- append(ego, rep(unqIDs[i], length(unqIDs) - i)) #append: add to the variable "ego"
    alter   <- append(alter  , unqIDs[(i+1):length(unqIDs)])
  }
  alter <- alter[1:length(ego)]
  
  masterEL <- data.frame(ego, alter)
  masterEL$conc <- paste(masterEL[,1],masterEL[,2],sep=".")
  masterEL$count <- 0
  
  return(masterEL)
}

#2. Calculate NON-DIRECTIONAL Edgelist
calcEdgeList    <- function(rscans, masterEL){
  
  partners = str_split(rscans$in.proximity, c(","), simplify = TRUE)
  focalID = as.character(rscans$focalID)
  a = cbind(focalID,partners)
  
  PP <- NULL  
  for(ii in 1:nrow(a)){ #for all observations
    for(p in 2:ncol(a)){ #for all proximity partners (not counting the first column, which is the focal ID column)
      if(!is.na(a[ii,p])) { #if not NA
        if(a[ii,p] != "" ) { #if not empty
          S1 <- data.frame(ego = as.character(a[ii, 1]), alter = as.character(a[ii, p]))
          PP <- dplyr::bind_rows(PP, S1) #bind rows, adds values to PP
        }
      }
    }
  }
  PP$conc1 <- paste(str_trim(PP$alter), str_trim(PP$ego), sep=".")
  PP$conc2 <- paste(str_trim(PP$ego), str_trim(PP$alter), sep=".")
  head(PP)
  head(masterEL)  
  
  for(i in 1:nrow(PP)){
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
  masterEL$count <- 0; masterEL$focalID <-  masterEL$givingID; masterEL$partnerID <- masterEL$receivingID
  
  return(masterEL)
}

#2. Calculate DIRECTIONAL Edgelist
calcEdgeList_groom    <- function(rscans, masterEL){
  
  partners = str_split(rscans$partner.ID, c(","), simplify = TRUE)
  focalID = as.character(rscans$focalID)
  a = cbind(focalID,partners,rscans$isSocialGive, rscans$isSocialGet,rscans$isSocial)
  
  PP <- NULL
  for(ii in 1:nrow(a)){ #for all observations
    if(!is.na(a[ii,2])) { #if not NA
      if(a[ii,ncol(a)] == 1 ) { #if is in a grooming state
        if(a[ii,ncol(partners)+2] == 1){S1 <- data.frame(givingID = as.character(a[ii, 1]), receivingID = as.character(a[ii, 2]), focalID = as.character(a[ii, 1]), partnerID = as.character(a[ii, 2]))}
        else if (a[ii,ncol(partners)+3] == 1) {S1 <- data.frame(givingID = as.character(a[ii, 2]), receivingID = as.character(a[ii, 1]), focalID = as.character(a[ii, 1]), partnerID = as.character(a[ii, 2]))}
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
      masterEL$focalID[which(masterEL$conc == PP$conc[i])] <- PP$focalID[i]
      masterEL$partnerID[which(masterEL$conc == PP$conc[i])] <- PP$partnerID[i]
    }
  }
  
  return(masterEL)
}

