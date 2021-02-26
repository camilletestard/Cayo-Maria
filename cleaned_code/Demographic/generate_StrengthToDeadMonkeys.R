# generate_StrengthToDeadMonkeys: The goal of this script is to find whether individuals who lost partners are the most 
# sensitive to the hurricane. It will find the strength of relationship between each monkey to dead partners 
# (considering up to one year after the hurricane).
# Input: CPRCdemographicfile_acquired_03.2020.csv; allScans.txt; allEL.Focal.RData
# Output: strength.to.deceased.RData
# Camille Testard - 2020

library(lubridate)
library(dplyr)

#Load data
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
Demographics = read.csv("Behavioral_Data/CPRCdemographicfile_acquired_03.2020.csv"); names(Demographics)[1]="id"
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt"); 
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data")
load("allEL.Focal.RData")

#Find study population
study.pop = unique(unlist(ID.list)) #all individuals in group by year files from pre-hurricane years in group V and KK

#Format date, letters etc.
Demographics$Sex = toupper(Demographics$Sex); 
Demographics$DateTransfer = NULL; Demographics$EventDate = NULL; Demographics$Last.Event = NULL; 
Demographics$DOD <- mdy(as.character(Demographics$DOD))
Demographics$YOD <- year(as.character(Demographics$DOD))
Demographics$MOD <- month(as.character(Demographics$DOD))
Demographics$DOB <- mdy(as.character(Demographics$DOB))
Demographics$AgeAtDeath = Demographics$YOD - Demographics$BirthSeason 
Demographics$AgeAtDeath[Demographics$AgeAtDeath <0]=0 #Note: there are discrepancies between birth season and death date
# boxplot(Demographics$AgeAtDeath[Demographics$AgeAtDeath >0])

####################################################
# Find who died in V and KK in the year following the hurricane
####################################################

Deaths.postHurr =  filter(Demographics,Demographics$DOD >= as.Date("2017-09-17") & Demographics$DOD < as.Date("2018-10-01"))#Find deaths before the hurricane
#only onsider V and KK deaths
Deaths.study.pop = filter(Deaths.postHurr, Deaths.postHurr$LastGroup=="V"|Deaths.postHurr$LastGroup=="KK")
#Add old age factor:
Deaths.study.pop$isOld=0; Deaths.study.pop$isOld[Deaths.study.pop$AgeAtDeath>17]=1
#only consider deaths in the study population
id.matches=match(study.pop,as.character(Deaths.study.pop$id))
Deaths.study.pop = Deaths.study.pop[id.matches[!is.na(id.matches)],]
Deaths.study.ID = as.character(Deaths.study.pop$id)

# table(as.character(Deaths.study.pop$LastGroup))
# table(as.character(Deaths.study.pop$LastGroup), as.character(Deaths.study.pop$Sex))

##########################################################################
# Find pre-hurricane strength of bond from survivors to the deceased
##########################################################################
group = c("V","V","V","KK","KK")
years = c(2015,2016,2017,2015, 2017)
groupyears =c("V2015","V2016","V2017","KK2015","KK2017"); gy=1;id=1;id.dead=1
strength.to.deceased=data.frame()

for (gy in 1:length(groupyears)){
  IDs = ID.list[[gy]]
  EL = allEL.Focal[[gy]]
  
  strength = as.data.frame(matrix(0,length(IDs),ncol= 7)); #Initialize ID-level stable partner interaction dataframe
  names(strength)=c("id", "group","year","dead.give","dead.get","std.dead.give","std.dead.get")
  for (id in 1:length(IDs)){ #for all monkeys
    for (id.dead in 1:length(Deaths.study.ID)){ #for all dead monkeys
      if (IDs[id] != Deaths.study.ID[id.dead]){ #only if monkey is not dead
        strength[id,"id"]=IDs[id]; strength[id,"group"]=group[gy]; strength[id,"year"]=years[gy]; 
        strength[id, "dead.give"] = strength$dead.give[id] + sum(EL$weight[which(EL$alter == IDs[id] & EL$ego == Deaths.study.ID[id.dead])])
        strength[id, "std.dead.give"] = strength$std.dead.give[id] + sum(EL$std.weight[which(EL$alter == IDs[id] & EL$ego == Deaths.study.ID[id.dead])])
        strength[id, "dead.get"] = strength$dead.get[id] + sum(EL$weight[which(EL$ego == IDs[id] & EL$alter == Deaths.study.ID[id.dead])])
        strength[id, "std.dead.get"] = strength$std.dead.get[id] + sum(EL$std.weight[which(EL$ego == IDs[id] & EL$alter == Deaths.study.ID[id.dead])])
      }
    }
  }
  strength.to.deceased=rbind(strength.to.deceased,strength)
}
strength.to.deceased$dead.all=strength.to.deceased$dead.give+strength.to.deceased$dead.get
strength.to.deceased$std.dead.all=strength.to.deceased$std.dead.give+strength.to.deceased$std.dead.get
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data")
save(strength.to.deceased,file="strength.to.deceased.RData")