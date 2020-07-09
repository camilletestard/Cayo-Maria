#Find number of observations and proxy for number of hours followed for 2018 data.
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned/") 

group = c("KK","V"); g=2

for (g in 1:length(group)){
  
  #Load Data: Scan mega file and GroupByYear
  data = read.csv(paste("Group",group[g],"2018_scansamples_FULL_CLEANED.csv", sep=""))
  meta_file = read.csv(paste("Group",group[g],"2018_GroupByYear.txt",sep=""));
  
  #Make appropriate ID corrections (i.e. check for 1.00E+02)
  data$subject.ID = as.character(data$subject.ID)
  data$subject.ID[which(data$subject.ID == "1.00E+02")] = "1E2"
  data$subject.ID[which(data$subject.ID == "3.00E+04")] = "3E4"
  
  #Number observations in scan for each focal in scans
  countID = as.data.frame(table(droplevels(as.factor(data$subject.ID)))) #Output frequency table for number of observations
  meta_file$numObs = countID$Freq[match(as.character(meta_file$id),as.character(countID$Var1))] #add to meta-data file (GrouByYear)
  meta_file$numObs[is.na(meta_file$numObs)]=0
  meta_file$propObs = meta_file$numObs/nrow(data) #add proportion of all observations for each ID in GrouByYear
  
  #Find total time spent observing in the field
  data$date <- lubridate::mdy(as.character(data$date))
  all.dates= unique(data$date); d=1; time.obs.total = 0;
  for (d in 1:length(all.dates)){
    num.scans = max(data$scan.num[data$date == all.dates[d]])
    time.obs = 15*num.scans #15=length in minutes of 1 scan
    time.obs.total = time.obs.total + time.obs
  }
  hrs.obs.total = time.obs.total/60
  
  #Add proxy for number of hrs followed for each ID
  meta_file$hrs.focalfollowed = meta_file$propObs*hrs.obs.total # Proportion of total observations * total number of hours spent in the field
  meta_file$hrs_focalfollowed = NULL
  write.csv(meta_file,file=paste("Group",group[g],"2018_GroupByYear.txt",sep=""),row.names = FALSE)
}

