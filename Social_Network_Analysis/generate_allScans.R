#Create allScans file

library(dplyr)
library(lubridate)
library(hms)
library(varhandle)
library(stringr)
library(schoolmath)

# # set working directory
# setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/vCT_Sam_Rscripts") #Change to my working directory
# #Load allScans
# allScans <- read.csv("allScans.csv")
# allScans_from_focal <- allScans[-which(allScans$group=="groupKK" & allScans$year==2018 | 
#                                          allScans$group=="groupV" & allScans$year==2018 ),]

######################################################
#PRE-HURRICANE data
######################################################

#Load proximity scans from groups and years of interest in the normal format: 
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
group = c("F", "F", "F", "V", "V", "V", "HH", "HH", "KK","KK", "KK")
years = c(2015, 2016, 2017, 2015, 2016, 2017, 2014, 2016, 2013, 2015, 2017)
groupyears = c("F2015", "F2016", "F2017", "V2015", "V2016", "V2017", "HH2014", "HH2016", "KK2013","KK2015", "KK2017")
allScans2 = data.frame()
for (i in 1:length(groupyears)){
  prox_data = read.csv(paste("Group",groupyears[i],"_ProximityGroups.txt", sep = ""))
  
  #Add group info
  prox_data$group =group[i]
  
  #Format date and add year + quarter info
  prox_data$date <- lubridate::dmy(as.character(prox_data$date))
  prox_data$year <- lubridate::year(prox_data$date)
  prox_data$Q    <- lubridate::quarter(prox_data$date)
  prox_data$date <- as.character(prox_data$date) #re-format to character after finding year and quarter
  
  #Add hurricane info
  prox_data$isPost = 0
  
  #Format time and add timeBlock info
  prox_data$time = as_hms(as.character(prox_data$time))
  prox_data$timeBlock = NA
  prox_data$timeBlock[which(prox_data$time <= as_hms("11:00:00"))] = "AM";
  prox_data$timeBlock[which(prox_data$time > as_hms("11:00:00"))] = "PM";
  
  #Format activity
  prox_data$focal.activity = as.character(prox_data$focal.activity)
  prox_data$focal.activity[which(prox_data$focal.activity=="feedplant"|prox_data$focal.activity=="feedchow")]="feed"
  prox_data$focal.activity[which(prox_data$focal.activity=="feedwater")]="drink"
  #To be consistent with post-hurricane data
  prox_data$focal.activity.isPost=NA
  prox_data$partner.ID=NA
  
  #Format name if needed
  prox_data$focal.monkey=sub("'E","E",as.character(prox_data$focal.monkey)) #Replace 'XEX byXEX names if needed
  
  #Clean up: rename and delete unused columns
  names(prox_data)[4] = "focalID"
  
  #Exclude focal from in.proximity, count number of prox partners
  partners = as.data.frame(str_split_fixed(prox_data$in.proximity,",",2))
  prox_data$in.proximity = partners[,2]
  prox_data <- prox_data %>% mutate_all(na_if,"");
  prox_data[] = lapply(prox_data,str_trim)
  prox_data$num.prox = str_count(as.character(prox_data$in.proximity),",")+1
  prox_data$num.prox[is.na(prox_data$num.prox)]=0

  #Add social information
  prox_data$isProx=1; prox_data$isProx[which(prox_data$num.prox==0)]=0
  prox_data$isSocial=0; prox_data$isSocial[which(prox_data$focal.activity=="social")]=1
  
  #Add grooming partner.ID information from focal data
  focal_data = read.csv(paste("Group",groupyears[i],"_FocalData.txt", sep = ""))
  scans_grooming = which(prox_data$isSocial==1)
  count = 0
  for (ii in 1:length(scans_grooming)){
    obs_name = prox_data$observation.name[scans_grooming[ii]]
    idx_obs_name = which(!is.na(match(focal_data$observation.name, obs_name)))
    groomBehav = which(focal_data$behaviour[idx_obs_name] == "GroomGET" | focal_data$behaviour[idx_obs_name] == "GroomGIVE")
    if (length(groomBehav)>1){count = count +1}
    prox_data$partner.ID[scans_grooming[ii]] = as.character(focal_data$partner.id[idx_obs_name[groomBehav[1]]])
    
    # #using time technique - I have issues figuring out which indiivdual was groomed right before scan...
    # scantime = as_hms(as.character(prox_data$time[scans_grooming[ii]]));
    # focaltime = as_hms(as.character(focal_data$behaviour.starttime[idx_obs_name]))
    # timepoint = max(which(is.negative(as.numeric(focaltime -scantime))))
    # prox_data$partner.ID[scans_grooming[ii]] = as.character(focal_data$partner.id[idx_obs_name[timepoint]])
  }
  
  prox_data[,c("observation.name","time","partners.activity..sequential.")] = NULL;
  
  #Order columns
  col_order <- c("date","focalID","group","year","scan.number","focal.activity","focal.activity.isPost","partner.ID","in.proximity","num.prox","isProx","isSocial","Q","isPost","timeBlock")
  prox_data <- prox_data[, col_order]
  
  allScans2= rbind(allScans2, prox_data)
}

######################################################
#Post-HURRICANE data
######################################################

#Load scans from 2018 (weird format)
allScans3=data.frame()
V2018 = read.csv("GroupV2018_scansamples_FULL_CLEANED.csv"); V2018[,"partner.activity"]=NULL #Because we don't have partner activity in post-hurricane KK
KK2018 = read.csv("GroupKK2018_scansamples_FULL_CLEANED.csv"); names(KK2018)[1]="date"
allScans3=rbind(V2018,KK2018)

allScans3$date <- lubridate::mdy(as.character(allScans3$date))
allScans3$year <- lubridate::year(allScans3$date)
allScans3$Q    <- lubridate::quarter(allScans3$date)
allScans3$date <- as.character(allScans3$date) #re-format to character after finding year and quarter

allScans3$isPost = 1

#Format time and create timeBlock column
#IMPORTANT: MAKE SURE TIME COLUMNS ARE FORMATTED IN EXCEL IN FORMAT "13:55:00"
allScans3$start.time = as_hms(as.character(allScans3$start.time))
allScans3$stop.time = as_hms(as.character(allScans3$stop.time))

allScans3$timeBlock = NA
allScans3$timeBlock[which(allScans3$start.time <= as_hms("11:00:00"))] = "AM";
allScans3$timeBlock[which(allScans3$start.time > as_hms("11:00:00"))] = "PM";

#Format XEX names
allScans3$subject.ID=sub("'","",as.character(allScans3$subject.ID)) #Replace 'XEX byXEX names if needed
allScans3$prox.adult.IDs=sub("'","",as.character(allScans3$prox.adult.IDs))

allScans3[,c("stop.time","observer.initials","cayo.map.code","nearest.adult.neighbour.ID","distance.nearest.neighbour","start.time")]=NULL
names(allScans3)[3]="scan.number"; names(allScans3)[4]="focalID"; names(allScans3)[5]="focal.activity"; names(allScans3)[7]="in.proximity"

#create column with equivalent activity to pre-hurricane
allScans3[] = lapply(allScans3,str_trim)
allScans3$focal.activity.isPost = as.character(allScans3$focal.activity) #preserve post-hurricane activity code
#re-code activity to pre-hurricane for comparison
allScans3$focal.activity = as.character(allScans3$focal.activity)
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='G'),which(allScans3$focal.activity=='E'),which(allScans3$focal.activity=='E,P'),which(allScans3$focal.activity=='G,E')))]="social"
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='R'),which(allScans3$focal.activity=='P')))]="rest"
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='AG'),which(allScans3$focal.activity=='AR')))]="aggression"
allScans3$focal.activity[unique(c(which(allScans3$focal.activity=='SR'),which(allScans3$focal.activity=='SG')))]="submit"
allScans3$focal.activity[grep('T',allScans3$focal.activity)]="travel"
allScans3$focal.activity[grep('F',allScans3$focal.activity)]="feed"
allScans3$focal.activity[grep('D',allScans3$focal.activity)]="drink"
allScans3$focal.activity[grep('SD',allScans3$focal.activity)]="sdb"
allScans3$focal.activity[grep('N/A',allScans3$focal.activity)]="UNK"
#unique(allScans3$focal.activity) #Check correct activity categories

#Exclude focal from in.proximity, count number of prox partners
allScans3$partner.ID = as.character(allScans3$partner.ID); allScans3$partner.ID[which(allScans3$partner.ID=="N/A")]=NA
allScans3$in.proximity = as.character(allScans3$in.proximity); allScans3$in.proximity[which(allScans3$in.proximity=="N/A")]=NA
allScans3$num.prox = str_count(as.character(allScans3$in.proximity),",")+1
allScans3$num.prox[is.na(allScans3$num.prox)]=0

#Add social information
allScans3$isProx=1; allScans3$isProx[which(allScans3$num.prox==0)]=0
allScans3$isSocial=0; allScans3$isSocial[which(allScans3$focal.activity=="social")]=1

#Order columns
col_order <- c("date","focalID","group","year","scan.number","focal.activity","focal.activity.isPost","partner.ID","in.proximity","num.prox","isProx","isSocial","Q","isPost","timeBlock")
allScans3 <- allScans3[, col_order]

######################################################
#COMBINE PRE-/POST-HURRICANE DATA
######################################################

rm.all.but(c("allScans2","allScans3"))
allScans= rbind(allScans2,allScans3)

#Find all unique IDs
a = str_split(allScans$in.proximity, c(","), simplify = TRUE)
proxIDs = str_trim(c(a[,1],a[,2],a[,3],a[,4],a[,5],a[,6],a[,7],a[,8],a[,9]))
allIDs = unique(c(as.character(allScans$focalID),proxIDs))
allIDs[which(nchar(allIDs)!=3)]
#Make appropriate ID corrections
allScans[which(allScans == "5.00E+06",arr.ind = TRUE)] = "5E6"
allScans[which(allScans == "2.00E+04",arr.ind = TRUE)] = "2E4"
allScans[which(allScans == "4.00E+02",arr.ind = TRUE)] = "4E2"
allScans[which(allScans == "9.00E+03",arr.ind = TRUE)] = "9E3"
allScans[which(allScans == "6.00E+06",arr.ind = TRUE)] = "6E6"
allScans[which(allScans == "1.00E+05",arr.ind = TRUE)] = "1E5"
allScans[which(allScans == "1.00E+02",arr.ind = TRUE)] = "1E2"
allScans[which(allScans == "3.00E+04",arr.ind = TRUE)] = "3E4"

# #Check again
# a = str_split(allScans$in.proximity, c(","), simplify = TRUE)
# proxIDs = str_trim(c(a[,1],a[,2],a[,3],a[,4],a[,5],a[,6],a[,7],a[,8],a[,9]))
# allIDs = unique(c(as.character(allScans$focalID),proxIDs))
# allIDs[which(nchar(allIDs)!=3)]


######################################################
#ADD SEX, AGE, RANK DATA
######################################################

#Load proximity scans from groups and years of interest (after running geneate_allScans): 
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data") 
population_info = read.csv("SubjectInfo_2010-2017/Population details_Allgroups.allyears.txt")
dominance_info =read.table("Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Find all unique IDs
a = str_split(allScans$in.proximity, c(","), simplify = TRUE)
proxIDs = str_trim(c(a[,1],a[,2],a[,3],a[,4],a[,5],a[,6],a[,7],a[,8],a[,9]))
allIDs = unique(c(as.character(allScans$focalID),proxIDs))

#get sex and year of birth from "Population details_Allgroups.allyears.txt"
ID_info= data.frame(); count = 0
for (i in 1:length(allIDs)){
  idx = which(as.character(population_info$id) == allIDs[i])
  if (length(idx)!=0){
    count = count +1
    ID_info[count,c("id","sex","yob")]<-population_info[idx,c("id","sex","yob")]
  }
}

#Combine allScans with ID_info
allScans$sex=NA; allScans$yob=NA
for (i in 1:nrow(ID_info)){
  idx = which(as.character(ID_info$id[i]) == as.character(allScans$focalID))
  allScans$sex[idx] = as.character(ID_info$sex[i])
  allScans$yob[idx] = ID_info$yob[i]
}
allScans$age = 2017-as.numeric(allScans$yob) # Set age at the time of hurricane 2017
allScans$yob = NULL

#Add rank info from "DOMINANCE.txt" file
allScans$rankFind = paste(as.character(allScans$focalID),2017, sep="") #Set rank at the time of hurricane 2017
allScans$ordrank=NA; allScans$percentrank=NA; allScans$sexrank=NA
for (i in 1:nrow(dominance_info)){
  idx = which(as.character(dominance_info$IDyear[i]) == as.character(allScans$rankFind))
  if (length(idx)!=0){
    allScans$ordrank[idx] = as.character(dominance_info$ORD_RANK[i])
    allScans$percentrank[idx] = as.character(dominance_info$X.DOMINATED[i])
  }
}
allScans$rankFind = NULL

col_order <- c("date","focalID","group","year","scan.number","sex","age","ordrank","percentrank","focal.activity","focal.activity.isPost","partner.ID","in.proximity","num.prox","isProx","isSocial","Q","isPost","timeBlock")
allScans <- allScans[, col_order]

######################################################
#sAVE ALLSCANS.TXT
######################################################

write.csv(allScans,"Data All Cleaned/allScans.txt", row.names = F)