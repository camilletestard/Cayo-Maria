# generate_FocalBasedNetworks: This script generates social networks for pre-hurricane years, based on focal data.
# Similarly as above, to compute weights I divide time spent grooming by the average number of hours observed between the dyad.
# Weights are further standardized by dividing by the mean for the group/year.
# Input: DOMINANCE.txt; _GroupByYear.txt & _GroomingEvents.txt for all groups and years
# Output: allEL.Focal.RData

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code") 
source("Functions/functions_GlobalNetworkMetrics.R")

#Load scan data, population and dominance info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Set parameters
network_action = "groom" 
network_mode = "directed"
network_weighted = "weighted" # "unweighted"

#For each group, each year separately: 
group = c("V","V","V","KK","KK")
years = c(2015,2016,2017,2015, 2017)
groupyears =c("V2015","V2016","V2017","KK2015","KK2017"); gy=1

allEL.Focal = list(); ID.list=list()

for (gy in 1:length(groupyears)){ #For each group
  
  #####################################################
  #FOR GROOMING
  #####################################################
  
  #Load data
  setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
  data = data=read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt",sep=""))
  
  # #create date of observation information: 
  # data$date = mdy(gsub(".","-",substr(data$observation.session,1,8),fixed=T))
  # data$semester = semester(data$date)
  # data$quarter = quarter(data$date)
  
  #Output the Master Edgelist of all possible pairs given the unique IDs.
  unqIDs = as.character(unique(meta_data$id))
  masterEL = calcMasterEL_groom(unqIDs)
  
  # 4. Output weighted edgelist from the Master Edgelist.
  data$conc <- paste(data[,1],data[,2],sep="."); weightedEL = masterEL
  # Transform edgelist name for later coding
  weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
  weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
  weightedEL = weightedEL[,c("alter","ego","conc","count")]
  
  #count the duration of pair grooming
  count = data.frame(table(data$conc))
  for (i in 1:nrow(weightedEL)){
    weightedEL$count[i] = sum(data$constrained_duration[which(data$conc == weightedEL$conc[i])]) #find the time spent grooming for each pair
  }
  weightedEL$count[which(is.na(weightedEL$count))] = 0
  weightedEL$hrs <- (meta_data$hrs.focalfollowed[match(weightedEL$alter, meta_data$id)] + 
                       meta_data$hrs.focalfollowed[match(weightedEL$ego, meta_data$id)])/2
  weightedEL$weight <- round(weightedEL$count / weightedEL$hrs, 5) #add weight information by dividing by the #scans for each individual
  #check: which(weightedEL$weight>0); which(weightedEL$count>0)
  meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
  weightedEL$std.weight <- weightedEL$weight/meanWeight #Normalize weights 
  #Save weighted EL
  allEL.Focal[[gy]]=weightedEL
  weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$hrs <-NULL #delete those calumn variables
  ID.list[[gy]]=as.character(meta_data$id)
  
}
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data")
save(allEL.Focal,ID.list, file=paste(network_weighted,"allEL.Focal.RData",sep="."))