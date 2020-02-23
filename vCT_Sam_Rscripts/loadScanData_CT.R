# LoadScanData      
#This script takes allScans as input and cleans it up, re-organizes it, to have outputs (A2, A3) usable in other scripts. 

#rm(list = ls())# clear all variables

      # set working directory
      setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria") #Change to my working directory 
      options(digits=10, round=5) #Allows the user to set and examine a variety of global options 
      #which affect the way in which R computes and displays its results."Digits": vontrols the number of significant
      # digits to print when printing number values. Default is 7. "'Round": round to 5 digits after "." 
      
      # Load libraries
      library(dplyr) #Grammar for data manipulation. Tool for wroking with data frames.
      library(glmmTMB) #fit linear and generlaized linear mixed models
      library(missForest) #non-parametric missing value imputation using random forest

      # 1. Load and clean data
          # Scan data
          allScans <- read.csv("allScans.csv")
          A1 <- allScans 
          
          allScans    <- allScans[-which(grepl("adult", colnames(allScans)))] #delete all "adult" columns from allScans
          # colnames() gives the name of columns of data frame
          # grepl("adult", colnames(x)) finds the string "adults" in the column names of allScans. grepl searches for matches to the first argument 
          # (here "adult") within each element of a character vector x. Note: The output of grepl is boolean (i.e. true/false)."which" allows to find true values in the boolean vector.
          # which = find (matlab)
          # to use all indices EXCEPT a subcategory use "-". allScans[-x] will output all columns of allScans except x.
          # If you want to select column (or variable) x, use allScans[x]. To select row1, column2: allScans[1,2]
          A1 <- A1[,which(grepl("adult", colnames(A1)))] #select only "adult" columns from A1
          #To ONLY select column 'x' use: A1[,x]. Same as matlab but don't need to use ':'
          column_names = names(A1)
          
          
          # 2. Re-format the data of proximity co-occurrences.Remove Duplicates in scans, recombine data, remove no longer needed data
          
          #For loop version
          #for(i in 1:ncol(A1)){ #for loop, same as for i =1:#columns of A1. use {} to mark beginning and end of for loop.
            #A1[,i] <- as.character(A1[,i]) #transform column i in A 'A[,i]' in charachter form
            #} 
          
              #for(i in 1:nrow(A1)){ # for all rows in A1
                #partnerIDs <-    as.character(A1[i, which(!A1[i,] == "")]) # create variable partnerIDs for each row containing all partners (i.e. all non empty entries of that row)
                ## which(!A1[i,] == ""): find non-empty characters in A1 row i. ! = ~ 
                ## as.character: give character (or string) format 
                #unqPartIDs <-    sort(unique(partnerIDs)) #find and order unique partner IDs 
                #length     <-    length(unqPartIDs)
                #A1[i,]     <-    "" # empty row i
                
                #if(length > 0){A1[i,1:length(unqPartIDs)]    <-    unqPartIDs} #if there are partners, fill in row i with unique partner IDs in rown (1:length)
                ##if loop: if(x) do {y}
                
              #} #end of for loop 

          #lapply version (much more efficient)
          cleanup <- function(row_i){
            d <- as.character(A1[row_i, which(!A1[row_i,] == "")]) #find all entries that are NOT ""
            d <- sort(unique(d)) #eliminate duplicates and order name IDs alphabetically
            d <- c(d, rep("",ncol(A1)-length(d))) #concatenate d with a replication of "" of the correct length
            return(d)
          }
          A1 <- lapply(1:nrow(A1), FUN=cleanup) #functions as a for loop but is much more efficient. Use it when computing i does not depend on i-1
          A1 <- do.call(rbind.data.frame, A1 ) #reorganize list into dataframe form
          names(A1)=column_names #rename columns
          
              A1 <- dplyr::bind_cols(allScans, A1) #bind together allScans and A1
        
              A2 <- A1
          
              save.image("Hurricane Workspace.RData")
              #load(file="Hurricane Workspace.RData") #CT added
              
              # Add total, alone, and social counts, both including and excluding their grooming partners
              A2 <- 
                A2 %>% #comes from dplyr package, passes the left hand side of the operator to the first argument of the right hand side of the operator.
                mutate(isSocial = ifelse(activity2 == "Social", 1, 0)) %>% #add variable "isSocial"
                #mutate: adds new variables and preserves existing ones
                #ifelse: if activity2 = "Social", then isSocial=1, else isSocial =0;
                mutate(isRest   = ifelse(activity2 == "Rest"  , 1, 0)) %>% #add variale "isRest" to A2+isSocial
                mutate(isTravel = ifelse(activity2 == "Travel", 1, 0)) %>% 
                mutate(isFeed   = ifelse(activity2 == "Feed"  , 1, 0))  
                   
              countI <- numeric(nrow(A2)) #create numeric variable of length(nrow(A2)) = array of zeros
              for(i in 1:nrow(A2)){
                countI[i] <- length(which(A2[i,13:21] != "")) #find and save the number of partners in countI 
                }
              
              A2 <-
                A2 %>% 
                mutate(countI = countI) %>% #social count including grooming partner
                mutate(countE = ifelse(activity2 == "Social", countI - 1, countI)) #social count excluding grooming partner
              
              A2 <-
                A2 %>% 
                mutate(isAloneI    = ifelse(countI == 0, 1 , 0))  %>% #alone count 
                mutate(isAloneE    = ifelse(countE == 0, 1 , 0))  %>% 
                mutate(isNotAloneI = ifelse(isAloneI == 1, 0 ,1)) %>%  #not alone count (does not take into acount the number of partners)
                mutate(isNotAloneE = ifelse(isAloneE == 1, 0 ,1))
              
              A2 <-
                A2 %>%
                mutate(isFeedTime = ifelse(timeBlock == 1, 1, 0))
              
              # Remove white space for focal ID; make sure they are all uppercase
              A2$focalID <- gsub(" ", "", A2$focalID , fixed = TRUE) #"gsub" perform replacement of all matches respectively
              A2$focalID <- toupper(A2$focalID) #make sure all letters are uppercase
              
              # Add date information
              A2$date  <- lubridate::mdy(as.character(A2$date)) #Use lubridate to decompose and standardize the date info
              A2$month <- lubridate::month(A2$date)
              A2$year  <- lubridate::year(A2$date)
              A2$week  <- lubridate::week(A2$date)
              A2$jDay  <- lubridate::yday(A2$date)
              A2$Q     <- lubridate::quarter(A2$date)
              A2$maria <- lubridate::mdy("9/20/2017")
              
              ## Create unique IDs for binning data
              A2$bin    <- paste(A2$focalID, A2$group, A2$Q, A2$isFeedTime, A2$isPost, sep=".")
              #paste: concatenate vectors after converting to character
                
        # Load demographic data
        sexage      <- read.csv("sexage.csv")
        sexage$dob  <- lubridate::mdy(sexage$dob)
        
        # Load and impute behavioral data
        behav       <- read.csv("behav.txt")
        vars <- c("Year", "sdb.ra", "vig.ra", "age", "sex", "rank", "rank2", "rc")
        behavForIMP <- behav[,vars]
        
        behavIMP <- missForest(behavForIMP) #used to impute missing values. missForest uses a random forest trained on the observed values of
        #a data matrix to predict the missing values. 
        
        behav$sdb.ra <- behavIMP$ximp$sdb.ra
        behav$vig.ra <- behavIMP$ximp$vig.ra
        behav$rank   <- behavIMP$ximp$rank
        behav$rank2  <- behavIMP$ximp$rank2
        behav$rc     <- behavIMP$ximp$rc
        
        
            # add sex and age information
            A2$isFemale <- sexage$isFemale[match(as.character(A2$focalID), sexage$focalID)]
            #match(X,Y): X = values to be matched and Y = values to be matched against. Output will be of length X, with indices from Y.  
            A2$age      <- round(as.numeric(A2$date - sexage$dob[match(as.character(A2$focalID), sexage$focalID)])/365, 2) #gets age from dob and current date
            
            # add behavioral information
            A2$Find <- paste(A2$focalID, "|" , A2$year, sep="")
            
            A2$rank2 <- behav$rank2[match(A2$Find, behav$Find)] 
            A2$rc    <- behav$rc[match(A2$Find, behav$Find)]
          
            head(A2) #returns the first part of a matrix
            
        # summarizing bins (creating the A3 file). Here Sam is taking a representative sub-sample of the whole sample. 
            A3 <-
              A2 %>% 
              dplyr::group_by(bin) %>% # This allows to re-order matrix by ID then group then...bin: focalID, group, Q, isFeedTime, isPost
              dplyr::summarise(focalID    = first(focalID)    , #summarise: gives given output separately for each bin (i.e. id/group/Q...); first: extract the first value from a vector
                               group      = first(group)      , #this procedure subsamples one data point per individual, per each category.
                               year       = first(year)       , #the output will be only a subsample of all data sets 
                               observer   = first(observer)   ,
                               isFemale   = first(isFemale)   ,
                               age        = median(age)       ,
                               rank       = first(rank2)      , 
                               rc         = first(rc)         ,
                               Q          = first(Q)          ,
                               isFeed     = first(isFeedTime) ,
                               isPost     = first(isPost)     ,
                               count      = length(bin)       ,
                               isNotAlone = sum(isNotAloneE)  ,
                               isSocial   = sum(isSocial)     ,
                               pNotAlone  = isNotAlone / count,
                               pSocial    = isSocial   / count) %>%
              as.data.frame() %>% #re-format into a data frame
              dplyr::select(c(-bin)) #remove the bin column from the data frame.
              
              head(A3)

             # save.image("Hurricane Workspace.RData")
          