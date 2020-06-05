
# load libraries
library(dplyr)
library(igraph)
library(tnet)
library(ineq)

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/vCT_Sam_Rscripts/")
# load in pedigree
cheetah.spkin <- function(pedigree) #Load function
{
  require(kinship2)
  
  if (all(c("id","sire","dam") %in% names(pedigree))) #if all column names = id, sire, dam
    # %in% identify if an element belogns to a vector
  {
    pedigree <- as(2*kinship(pedigree$id,dadid=pedigree$sire,momid=pedigree$dam),"dsCMatrix") #
    #kinship function to determine kinship relationship from indiivdual, mom and dad IDs
    #as() coerce an object to a given class, here dsCMatrix = class of symmetric, sparse numeric matrices in the compressed form 
  } else
    pedigree <- as(2*kinship(pedigree[,1],dadid=pedigree[,2],momid=pedigree[,3]),"dsCMatrix")
  
  return(pedigree)
}

bigped <- cheetah.spkin(read.csv("bigpedv4.csv"))

# load in data
P1 <- A1

# Calculate Date Information
P1 <- calcDates(P1)   

A1$dateL   <- lubridate::mdy(A1$date)
A1$quarter <- lubridate::quarter(A1$dateL)
P1$quarter <- A1$quarter

# Create dataframe
netIDs <- data.frame(groups = c(rep("groupV", 8), rep("groupKK", 8)), postH = c(rep(1, 4), rep(0,4)), quarter = rep(seq(1:4), 4)) #create a dataframe for with an entry for each group, each quarter, pre- and post-hurricane
netIDs <- netIDs[-c(8, 13, 16),] #remove quarter 4 in V pre-hurricane and quarter 1&4 pre-hurricane in KK


fullFileByQuarter <- NULL   
for(p in 1:nrow(netIDs)){ #for each group/Q/hurricane status bins     
  # Reduce file to only what is needed
  scans2 <- createScans(P1, as.character(netIDs[p,1]), netIDs[p,2], netIDs[p,3]) #Select scans of interest
  
  # Get obervation table
  obsTable <- calcObsTable(scans) #Count the number of ID appearance in each time block, for each ID
  
  # Get unique IDs which appear in all 5 time blocks
  unqIDs <- calcUnqIDs(scans)
  
  # Get Master Edgelist: create list of all possible edges between nodes 
  EL   <- calcMasterEL(unqIDs)
  
  # Reduce Pedigree
  ped <- reducePed(bigped, unqIDs) #only look at kinship relationships between individuals present in the same time block/Q/group etc.
  
  # Permute 20 Networks
  metrics <- NULL
  runningEL <- NULL
  
  for(k in 1:500){
    
    # calculate random scans
    minTimeBlock <-2; maxTimeBlock <- 6; hrs <- sample(1:10, 1) #Sample: takes a sample of the specified size from the elements of x using either with or without replacement.   
    P2 <- calcRandomScans(scans, minTimeBlock, maxTimeBlock, hrs, unqIDs)
    
    # Generate edgelist
    options(warn = -1) #set options to ignore all warnings
    P3 <- calcEdgeList(randomScans = P2, masterEdgeList = EL) #create edgelist with count for all IDs in this random scan, in this bin
    options(warn = 0, digits = 5)
    
    P3$weight <- round(P3$count / hrs, 5) #add weight information by dividing by the #hrs spent observing
    P3$count <- NULL; P3$conc <- NULL #delete those calumn variables
    
    # Update Running edgelist (for drawing the figure)
    # runningEL <- calcRunningEdgeList(runningEL = runningEL, newEL = P3)
    
    # Generate undirected graphs from proximity scans (has sex and age vertices attributes)
    options(warn = -1) #set options to ignore all warnings
    netList <- createIG(P3, unqIDs)
    
    # Calculate global network statistics
    dfG <- calcGenStats(netList) 
    
    # Generate Sex-based joint-counts
    dfS <- calcSexProps(netList)
    
    # Generate kin-based joint counts
    dfK <- calcKinProps(netList, ped)    
    
    # Combine metrics   
    df <- bind_cols(dfG, dfS, dfK)
    
    # Load into running dataframe
    metrics <- bind_rows(metrics, df)
  }
  
  metrics$group    <- netIDs[p,1]
  metrics$isPost   <- netIDs[p,2]
  metrics$quarter  <- netIDs[p,3]
  
  metrics$netID <- paste(metrics$group, metrics$isPost, metrics$quarter, sep=".")   
  
  fullFileByQuarter <- bind_rows(fullFileByQuarter, metrics)
}    

write.csv(file = "permutedNetsQ.csv", fullFileByQuarter)      
















