#Loading regular agonistic action file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned/") 
allScans = read.csv("allScans.txt"); unique(allScans$focal.activity)

group = c("KK","V"); g=1

for (g in 1:length(group)){
  
  #Load GroupByYear
  meta_file = read.csv(paste("Group",group[g],"2018_GroupByYear.txt",sep=""));
  
  #Number observations in scan for each focal in scans
  allScans2018 = allScans[which(allScans$group ==group[g] & allScans$year ==2018),];
  countID = as.data.frame(table(droplevels(as.factor(allScans2018$focalID))))
  
  meta_file$numObs = countID$Freq[match(as.character(meta_file$id),as.character(countID$Var1))]
  
  write.csv(meta_file,file=paste("Group",group[g],"2018_GroupByYear.txt",sep=""),row.names = FALSE)
}

