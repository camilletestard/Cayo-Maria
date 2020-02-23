# Generating the Network Functions
library(dplyr)

    # reduce the pedigree to only what is needed
    reducePed       <- function(matrix, unqIDs){
      reduce  <- match(as.character(unqIDs), colnames(matrix)) #find character ID in the pedigree (output indices)
      matrix2 <- matrix[reduce, reduce]
      return(matrix2)
    }
    
    # calculate date information
    calcDates       <- function(dataframe){
      dataframe$date     <- lubridate::mdy(dataframe$date)
      
      dataframe$month    <- lubridate::month(dataframe$date[i])
      dataframe$day      <- lubridate::day(dataframe$date[i])
      dataframe$julDay   <- lubridate::yday(dataframe$date[i])
      dataframe$wDay     <- lubridate::wday(dataframe$date[i])
      dataframe$quarter  <- lubridate::quarter(dataframe$date[i]) 
      dataframe$week     <- lubridate::week(dataframe$date[i])
      
      return(dataframe)
    }
  
    # Reduce Dataframe to create scans file, use only what's needed
    createScans     <- function(dataframe, group, isPost, quarter){
    HLP1 <- group
    HLP2 <- isPost 
    HLP3 <- quarter
      
    scans <-
      dataframe %>% 
      filter(dataframe$group == HLP1 & dataframe$isPost == HLP2 & dataframe$quarter == HLP3) #Only select the rows (or cases) of interest
      #filter: find rows/cases where conditions are true
      

    scans  <- scans[c("date", "focalID", "timeBlock", "quarter", "activity2", "adult1",  #only select specified variables)
                      "adult2", "adult3",  "adult4",  "adult5",    "adult6", 
                      "adult7", "adult8",  "adult9")]
        
    return(scans)
    }
    
    # calc Observation Table
    calcObsTable <- function(dataframe){
      unqIDs <-
        dataframe %>%
        group_by(focalID, timeBlock) %>% #group focal IDs by time blocks
        summarise(count = length(focalID)) %>% #count the number of scans for each ID in each time block
        as.data.frame()
      
      return(unqIDs)
    }
    
    # Calculate Unique IDs
    calcUnqIDs      <- function(dataframe){
      
      unqIDs <-
        dataframe %>%
        group_by(focalID, timeBlock) %>% 
        summarise(count = length(focalID)) %>% 
        as.data.frame()
      
      unqIDs$hlp <- 1
      
      unqIDs <-
        unqIDs %>% 
        group_by(focalID) %>% 
        summarise(hlp = sum(hlp)) %>%
        filter(!hlp <= 4) %>% #Only consider individuals which appear in all 5 blocks
        as.data.frame()
      
      unqIDs <- as.character(unqIDs$focalID) #Only keep the IDs
      
      return(unqIDs)
      
    }
    
    # Calculate Random Scans
    calcRandomScans <- function(scans ,minTimeBlock, maxTimeBlock, hrs, unqIDs){
        P2 <- NULL
        for(i in 1:length(unqIDs)){
          for(tb in 2:6){
            S1 <- scans %>%  filter(focalID == unqIDs[i] & timeBlock == tb) #only select rows of focalID i & in timeblock tb
            
            if(nrow(S1) < 6){S1 <- S1[sample(1:nrow(S1), hrs , replace=T),]} #if the number of appearances of ID i <6, then randomly re-select 'hrs' samples with replacement
            #Why choose randomly hrs [1 10]?
            #This adds NA when there is no ID i in time block tb. Why is he doing that?
            else { S1 <- S1[sample(1:nrow(S1), 6 , replace=F),]} #if number of appearances is >=6, select 6 rows without replacement 
          
            P2 <- bind_rows(P2, S1) #concatenate (or bind) all focal ID scans 
          }
        }
        return(P2)
    }
    
    
      # Calculate Master Edgelist
      calcMasterEL    <- function(unqIDs){
        alter <- NULL; ego <- NULL
        for(i in 1:length(unqIDs)){
          alter <- append(alter, rep(unqIDs[i], length(unqIDs) - i)) #append: add to the variable "alter"
          ego   <- append(ego  , unqIDs[(i+1):length(unqIDs)])
        }
        ego <- ego[1:length(alter)]
        
        mastEL <- data.frame(alter, ego)
        
        return(mastEL)
      }
      
      # Calculate Edgelist
      calcEdgeList    <- function(randomScans, masterEdgeList){
        
        P2       <- randomScans
        masterEL <- masterEdgeList
        
        P3 <- NULL
        for(i in 1:nrow(P2)){
          for(p in 6:14){      #for all "adult" variables (CT modified 5-13 to 6-14)
            if(!is.na(P2[i,p])) { #CT added this NA if condition otherwise there was an error.
              if(P2[i,p] != "" ) { #find all non-empty columns
                S1 <- data.frame(ego = as.character(P2[i, 2]), alter = as.character(P2[i, p]))
                P3 <- dplyr::bind_rows(P3, S1) #bind rows adds values to P3 in the row dimension (creates new rows, not columns)
              }
            }
          }
        }
        P3$conc1 <- paste(P3$ego,   P3$alter, sep=".") #concatenate multiple characters together with specifed separtion "sep"
        P3$conc2 <- paste(P3$alter, P3$ego  , sep=".")
        masterEL$conc  <- paste(masterEL$alter, masterEL$ego, sep=".")
        masterEL$count <- 0
        head(P3)
        
        head(masterEL)  
        
        for(i in 1:nrow(P3)){
          if(P3$conc1[i] %in% masterEL$conc){ #If this edge exists in the master list
            masterEL$count[which(masterEL$conc == P3$conc1[i])] <- masterEL$count[which(masterEL$conc == P3$conc1[i])] +1 #Find the index and add counts
          }
          
          if(P3$conc2[i] %in% masterEL$conc){
            masterEL$count[which(masterEL$conc == P3$conc2[i])] <- masterEL$count[which(masterEL$conc == P3$conc2[i])] +1
          }
        }
        
        return(masterEL)
      }
      
      # Calculate Running Edgelist (For creating summary networks)
      calcRunningEdgeList <- function(runningEL, newEL){
        
        if(k == 1){
          runningEL <- newEL
        } else{
          for(i in 1:nrow(runningEL)){
            runningEL$weight[i] <- round((runningEL$weight[i] + newEL$weight[i]) / 2, 3)
          }
        }
        
        return(runningEL)
      }
      
      # Create igraph and tnet objects
      createIG        <- function(edgelist, unqIDs){
        
        # create igraph object
        ig <- simplify(graph.data.frame(d=edgelist, directed = F), #create undirected igraph object 
                       remove.loops = T) #simple graph with no loop
        ig <- delete.edges(ig, which(E(ig)$weight == 0)) #delete edges that have a weight of 0
        
        # Add in isolated individuals
        ig <- add.vertices(ig, length(unqIDs[which(!unqIDs %in% V(ig)$name)]), #find unique IDs that are not in ig vertices (i.e. isolated individuals whose edge weight are all 0)
                           attr = list(name = as.character(unqIDs[which(!unqIDs %in% V(ig)$name)]))) #add names of those isolated indiividuals
        
        # Add Sex to IG
        ig <- set.vertex.attribute(ig, name = "isFemale", value = sexage$isFemale[match(V(ig)$name, as.character(sexage$focalID))])
        
        # Add Age to IG
        sexage$age <- round(as.numeric(lubridate::mdy("6/1/2018") - lubridate::mdy(sexage$dob)) / 365, 2)
        ig <- set.vertex.attribute(ig, name = "age", value = sexage$age[match(V(ig)$name, as.character(sexage$focalID))])
        
        # Create tnet object
        tnet <- cbind(get.edgelist(ig, names=FALSE), #get the list of edges from object ig
                      E(ig)$weight)
        if(!is.directed(ig)){ #if ig is not directed
          tnet <- symmetrise_w(tnet)} #make sure the network is symmetrical (i.e. undirected)
        tnet  <- as.tnet(tnet, type="weighted one-mode tnet") 
        
        # create female only networks
        igFem <-  igraph::delete.vertices(ig, which(V(ig)$isFemale==0))    
        igFem <-  delete_vertex_attr(igFem, name="isFemale")
        
        tnetFem <- cbind(get.edgelist(igFem, names=FALSE), E(igFem)$weight)
        if(!is.directed(igFem)){tnetFem <- symmetrise_w(tnetFem)}
        tnetFem  <- as.tnet(tnetFem, type="weighted one-mode tnet") 
        
        # create male only networks: 
        igMal <-  igraph::delete.vertices(ig, which(V(ig)$isFemale==1))    
        igMal <-  delete_vertex_attr(igMal, name="isFemale")
        
        tnetMal <- cbind(get.edgelist(igMal, names=FALSE), E(igMal)$weight)
        #if(!is.directed(igMal)){tnetMal <- symmetrise_w(tnetMal)} #CT commented for now as this causes an error when there are no edges between males
        tnetMal  <- as.tnet(tnetMal, type="weighted one-mode tnet") 
        
        netList <- list(ig, tnet, igFem, tnetFem, igMal, tnetMal) 
        return(netList)
      }
      
      # Calculate generic network values
      calcGenStats <- function(netList){
        # total network
        dens    <- length(E(netList[[1]])) / (length(V(netList[[1]]))^2 - length(V(netList[[1]]))) # #edges / all possible edges
        dens.w  <- sum(E(netList[[1]])$weight) * (length(E(netList[[1]])) / (length(V(netList[[1]]))^2 - length(V(netList[[1]])))) #network density * sum of all weights
        gini    <- ineq::ineq(as.numeric(degree(netList[[1]]))  , "gini") #Compute gini coefficient as a measure of equality   
        gini.w  <- ineq::ineq(as.numeric(strength(netList[[1]])), "gini") 
        kcomm   <- length(fastgreedy.community( #Find dense subgraph, also called communicites in graphs
          delete.vertices(as.undirected(netList[[1]]), degree(netList[[1]]) == 0))[]) #delete vertices that have no edges
        clust.w <- as.numeric(tnet::clustering_w(netList[[2]])) #clustering coefficient
        
        # female network
        dens.f    <- length(E(netList[[3]])) / (length(V(netList[[3]]))^2 - length(V(netList[[3]])))
        dens.f.w  <- sum(E(netList[[3]])$weight) * (length(E(netList[[3]])) / (length(V(netList[[3]]))^2 - length(V(netList[[3]]))))
        gini.f    <- ineq::ineq(as.numeric(degree(netList[[3]]))  , "gini")     
        gini.f.w  <- ineq::ineq(as.numeric(strength(netList[[3]])), "gini") 
        kcomm.f   <- length(fastgreedy.community(delete.vertices(as.undirected(netList[[3]]), degree(netList[[3]]) == 0))[])
        clust.f.w <- as.numeric(tnet::clustering_w(netList[[4]]))
        
        # male network
        dens.m    <- length(E(netList[[5]])) / (length(V(netList[[5]]))^2 - length(V(netList[[5]])))
        dens.m.w  <- sum(E(netList[[5]])$weight) * (length(E(netList[[5]])) / (length(V(netList[[5]]))^2 - length(V(netList[[5]]))))
        gini.m    <- ineq::ineq(as.numeric(degree(netList[[5]]))  , "gini")     
        gini.m.w  <- ineq::ineq(as.numeric(strength(netList[[5]])), "gini") 
        #kcomm.m   <- length(fastgreedy.community(delete.vertices(as.undirected(netList[[5]]), degree(netList[[5]]) == 0))[]) #CT commented because there is not enough vertices to compute
        #clust.m.w <- as.numeric(tnet::clustering_w(netList[[6]]))
        
        df <- data.frame(dens, dens.w, gini, gini.w, kcomm, clust.w,
                         dens.f, dens.f.w, gini.f, gini.f.w, kcomm.f, clust.f.w,
                         dens.m, dens.m.w, gini.m, gini.m.w) #, kcomm.m, clust.m.w) # removed because of errors. CT added
        
        return(df)
      }
      
      # Calculate sex-based joint-counts
      calcSexProps <- function(netList){
        # Calculating Sex Pairs
        el            <- data.frame(get.edgelist(netList[[1]]), E(netList[[1]])$weight); colnames(el) <- c("ego", "alter", "weight") #tranform tnet object to dataframe
        el$isFemEgo   <- sexage$isFemale[match(as.character(el$ego),   sexage$focalID)]
        el$isFemAlter <- sexage$isFemale[match(as.character(el$alter), sexage$focalID)]
        el$pairClass  <- "opp"; el$pairClass[which(el$isFemEgo == 1 & el$isFemAlter == 1)] <- "bothFem"; el$pairClass[which(el$isFemEgo == 0 & el$isFemAlter == 0)] <- "bothMal"
        
        weight.FF     <- sum(el$weight[el$pairClass=="bothFem"]) #sum of all the weights for class FF
        weight.MM     <- sum(el$weight[el$pairClass=="bothMal"]) #sum of all the weights for class MM                       
        weight.cross  <- sum(el$weight[el$pairClass=="opp"]) #sum of all the weights for class MF                           
        
        #Compute expected proportions of pair classes if the network was completely random
        possFemPairs        <-    (length(V(netList[[3]]))^2)      - length(V(netList[[3]])) #all possible FF pairs
        possMalPairs        <-    (length(V(netList[[5]]))^2)      - length(V(netList[[5]])) #all possible MM pairs
        allPossiblePairs    <-    (length(V(netList[[1]]))^2)    - length(V(netList[[1]])) #all possible pairs
        possCrossPairs      <-    allPossiblePairs - (possFemPairs + possMalPairs) #all possible MF pairs
        #Weighted expectetd proportions
        exp.FF      <- (possFemPairs/allPossiblePairs)    * sum(el$weight)
        exp.MM      <- (possMalPairs/allPossiblePairs)    * sum(el$weight)
        exp.cross   <- (possCrossPairs/allPossiblePairs)  * sum(el$weight)
        
        #Compute ratio actual/expected. If ratio = 1 then proportion of FF pairs is exactly as expected if network was random
        eo.FF     <- weight.FF/exp.FF
        eo.MM     <- weight.MM/exp.MM
        eo.cross  <- weight.cross/exp.cross
        
        sexPairStats <- data.frame(eo.FF, eo.MM, eo.cross) #tranform tnet object to dataframe
        return(sexPairStats)        
      }
      
      # Calculate kin-based joint-counts for female network only
      calcKinProps <- function(netList, pedigree){
        
        el <- data.frame(get.edgelist(netList[[3]]), E(netList[[3]])$weight); colnames(el) <- c("ego", "alter", "weight") #tranform tnet object to dataframe
        
        KC      <- NULL; for(i in 1:length(el[,1])){ KC[i] <-  ped[which(rownames(ped)==as.character(el$ego[i])) , which(colnames(ped)==as.character(el$alter[i]))]}
        el$KC   <- round(KC, 4)
        el$pairClass <- "unrelated"
        el$pairClass[which(el$KC >= .125 & el$KC < .25)] <- "dRel"
        el$pairClass[which(el$KC >= .25)] <- "rel"
        
        #Compute observed weight of related pair classes
        obs.ck     <- sum( el$weight[el$pairClass =="rel"])
        obs.dk     <- sum( el$weight[el$pairClass =="dRel"])
        obs.u      <- sum( el$weight[el$pairClass =="unrelated"])
        
        #Computer expected weight of related pair classes if network was completely random
        ckPairs    <- length(which(ped  >= .25)) - length(V(netList[[3]]))  ; exp.ck      <- (ckPairs   / length(ped))   * sum(el$weight)
        dkPairs    <- length(which(ped  >= .125 & ped <.25))                ; exp.dk      <- (dkPairs   / length(ped))   * sum(el$weight)
        uPairs     <- length(which(ped  <= .125))                           ; exp.u       <- (uPairs    / length(ped))   * sum(el$weight)
        
        #Compute observed/expected ratio
        eo.ck      <- obs.ck    /   exp.ck  
        eo.dk      <- obs.dk    /   exp.dk  
        eo.u       <- obs.u     /   exp.u   
        
        el$weightKC    <- el$weight * el$KC
        kinDegree      <- sum(el$weightKC) / sum(el$weight)
        
        kinPairStats <- data.frame(eo.ck, eo.dk, eo.u, kinDegree)
        
        return(kinPairStats)
      }
      
      
       