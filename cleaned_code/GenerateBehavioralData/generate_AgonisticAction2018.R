library(stringr)
library(lubridate)

#Loading regular agonistic action file
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/") 
preHurr.format = read.csv("Data All Cleaned/GroupV2017_AgonsiticActions.txt");

#load scan data
allScans = read.csv("Data All Cleaned/allScans.txt"); unique(allScans$focal.activity)
group = c("KK","V"); g=1

for (g in 1:length(group)){
  
  #####################################
  #SCAN DATA
  #####################################
  
  #Select agonistic scan data for group[g]
  agonistic.data = allScans[which(allScans$focal.activity =="aggression" | allScans$focal.activity =="submit"
                                  & allScans$group ==group[g] & allScans$isPost ==1),];
  #initialize agonistic dataframe
  agonistic.actions = as.data.frame(matrix(nrow = nrow(agonistic.data), ncol = ncol(preHurr.format)))
  names(agonistic.actions)=names(preHurr.format) 
  
  #Find indices of action given/received and submit/aggression for FOCAL ID
  idxR = which(str_detect(agonistic.data$focal.activity.isPost,"R"))#find index of agonistic action RECEIVED
  idxG = which(str_detect(agonistic.data$focal.activity.isPost,"G"))#find index of agonistic action GIVEN
  idxAgg = which(agonistic.data$focal.activity =="aggression")#find index of aggression
  idxSub = which(agonistic.data$focal.activity =="submit")#find index of submission
  
  #Fill in agonism winner and loser
  #For focal ID
  agonistic.actions$agonsim.loser[c(intersect(idxR, idxAgg), intersect(idxG, idxSub))] = as.character(agonistic.data$focalID[c(intersect(idxR, idxAgg), intersect(idxG, idxSub))]) #Loser = Aggression received or submission given
  agonistic.actions$agonsim.winner[c(intersect(idxG, idxAgg), intersect(idxR, idxSub))] = as.character(agonistic.data$focalID[c(intersect(idxG, idxAgg), intersect(idxR, idxSub))])#Winner = Aggression given or submission received
  #For partner ID (when focalID looses, partner wins!)
  agonistic.actions$agonsim.winner[c(intersect(idxR, idxAgg), intersect(idxG, idxSub))] = as.character(agonistic.data$partner.ID[c(intersect(idxR, idxAgg), intersect(idxG, idxSub))])#Partner Winner when Focal ID aggression received or submission given
  agonistic.actions$agonsim.loser[c(intersect(idxG, idxAgg), intersect(idxR, idxSub))] = as.character(agonistic.data$partner.ID[c(intersect(idxG, idxAgg), intersect(idxR, idxSub))])#Partner Loser when Focal ID aggression given or submission reveived
  
  #Fill in date
  agonistic.actions$date[c(intersect(idxR, idxAgg), intersect(idxG, idxSub))] = as.character(ymd(agonistic.data$date[c(intersect(idxR, idxAgg), intersect(idxG, idxSub))]))
  agonistic.actions$date[c(intersect(idxG, idxAgg), intersect(idxR, idxSub))] = as.character(ymd(agonistic.data$date[c(intersect(idxG, idxAgg), intersect(idxR, idxSub))]))
  
  #Fill in focalID
  agonistic.actions$focal.individual[c(intersect(idxR, idxAgg), intersect(idxG, idxSub))] = "agonsim.loser"
  agonistic.actions$focal.individual[c(intersect(idxG, idxAgg), intersect(idxR, idxSub))] = "agonsim.winner"
  
  #Fill in agonism action type (only two possibility)
  agonistic.actions$agonsim.type[idxAgg]="aggression"
  agonistic.actions$agonsim.type[idxSub]="submit"
  
  #Fill in sex
  agonistic.actions$winner.sex = agonistic.data$sex[match(agonistic.data$focalID, agonistic.actions$agonsim.winner)]
  agonistic.actions$loser.sex = agonistic.data$sex[match(agonistic.data$focalID, agonistic.actions$agonsim.loser)]
  
  #Fill in observer & source
  agonistic.actions$observer=ifelse(group[g]=="V","DP","JN")
  agonistic.actions$data.source="scan"
  
  #Fill in timeblock
  agonistic.actions$timeblock = agonistic.data$timeBlock
  
  #####################################
  #ADLIB DATA
  #####################################
  
  #Load adlib data
  adlib = read.csv(paste("Group",group[g],"2018_files/Group",group[g],"2018_AggData.csv",sep=""))
  #initialize
  agonistic.actions.adlib = as.data.frame(matrix(nrow = nrow(adlib), ncol = ncol(preHurr.format)))
  names(agonistic.actions.adlib)=names(preHurr.format) 
  
  agonistic.actions.adlib$agonsim.loser = adlib$ID.of.Winner
  agonistic.actions.adlib$agonsim.winner = adlib$ID.of..Loser
  agonistic.actions.adlib$agonsim.type=adlib$Type.of..interaction..code.
  agonistic.actions.adlib$observer = adlib$Observer
  agonistic.actions.adlib$date = as.character(ymd(mdy(adlib$ï..Date..mm.dd.yy.)))
  agonistic.actions.adlib$data.source="adlib"
  agonistic.actions.adlib$timeblock=NA
  
  #####################################
  #Merge & save
  agonistic.actions.full = rbind(agonistic.actions,agonistic.actions.adlib)
  write.csv(agonistic.actions.full,file=paste("Data All Cleaned/Group",group[g],"2018_AgonsiticActions.txt",sep=""),row.names = FALSE)
  
}

