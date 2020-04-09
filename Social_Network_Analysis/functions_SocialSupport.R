###############################################
#Functions to generate networks from unique IDs
###############################################

#1. Output the Master Edgelist of all possible pairs given the unique IDs for grooming data (directional)
calcMasterEL_groom    <- function(unqIDs){ #unq IDs only include focal individuals. I.e. We will take into account focal individuals in our social networks.
  alter <- NULL; ego <- NULL
  for(i in 1:length(unqIDs)){
    alter <- append(alter, rep(unqIDs[i], length(unqIDs) - 1)) #append: add to the variable "alter"
    if (i==1) {ego   <- append(ego  , unqIDs[2:length(unqIDs)]) }
    else {ego   <- append(ego  , unqIDs[c(1:i-1,(i+1):length(unqIDs))])  }
  }
  ego <- ego[1:length(alter)]
  
  masterEL <- data.frame(alter, ego)
  masterEL$conc <- paste(masterEL[,1],masterEL[,2],sep=".")
  masterEL$count <- 0
  
  return(masterEL)
}

#2. Calculate Edgelist
calcEdgeList_SS    <- function(rscans, masterEL, action, proxdata){
  
  if (action=="groom") {partners = str_split(rscans$partner.ID, c(","), simplify = TRUE)}
  if (action=="prox") {partners = str_split(rscans$in.proximity, c(","), simplify = TRUE)}
  
  if (proxdata != 1){ # If computing edgelist from scan data, add focalID column
    focalID = as.character(rscans$focalID)
    a = cbind(focalID,partners)}
  
  if (proxdata == 1){ a = partners } # If computing edgelist from proximity data, focalID = 1st column
  
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
  PP$conc1 <- paste(str_trim(PP$ego), str_trim(PP$alter), sep=".")
  PP$conc2 <- paste(str_trim(PP$alter), str_trim(PP$ego), sep=".")
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

