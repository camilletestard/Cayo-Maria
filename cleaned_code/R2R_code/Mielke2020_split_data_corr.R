#Load scan data, population and dominance info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#Get pre-hurricane scans
PrePostScans =  allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
SubScans=PrePostScans[-which(PrePostScans$year==2019),] #remove 2019
PreScans = SubScans[which(SubScans$isPost==0),]
PreScans$groupyear = paste(PreScans$group, PreScans$year,sep="")

#Get unique IDs from subsample
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
source("cleaned_code/Functions/functions_bayesian_networks.R")
ExSubScans = calcRandomScans(allScans);
unqIDs = as.character(unique(ExSubScans$focalID))

n_iter = 500; iter =1
for (iter in 1:n_iter){
  
  id=1
  for (id in 1:length(unqIDs)){
    idx = PreScans[PreScans$focalID == unqIDs[id]]
    idx_dataset1 = sample(1:length(idx), round(length(idx)))
      
      
    dataset1 = 
  }
}