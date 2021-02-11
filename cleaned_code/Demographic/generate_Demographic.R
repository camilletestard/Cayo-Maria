#Load AllScans file
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

# WITHOUT SUBSAMPLING
#Select groups of interest
PrePostScans =  allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
SubScans=PrePostScans[-which(PrePostScans$year==2019),] #remove 2019
unqIDs = as.character(unique(SubScans$focalID))

#Exclude individuals that are not present either pre-hurricane or post-hurricane
for (id in 1:length(unqIDs)){ #For all individuals
  
  if (length(which(SubScans$focalID == unqIDs[id] & SubScans$isPost == 0))==0 
      | length(which(SubScans$focalID == unqIDs[id] & SubScans$isPost == 1))==0) {
    
    SubScans=SubScans[-which(SubScans$focalID == unqIDs[id]),]
    
  }
}
ExSubScans=SubScans
unqIDs = as.character(unique(ExSubScans$focalID))

#Get sex and age: sample = allScans[match(unqIDs, allScans$focalID),]

#WITH SUBSAMPLING
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
ExSubScans = calcRandomScans(allScans);
unqIDs = as.character(unique(ExSubScans$focalID))

#Demographics
demographics = data.frame(); id=1
for (id in 1:length(unqIDs)){
  demographics[id,"id"]=unqIDs[id]
  idx=which(!is.na(match(ExSubScans$focalID,unqIDs[id])))
  demographics[id,"sex"]=ExSubScans$sex[idx[1]]
  demographics[id,"group"]=ExSubScans$group[idx[1]]
}
table(demographics$sex,demographics$group)

