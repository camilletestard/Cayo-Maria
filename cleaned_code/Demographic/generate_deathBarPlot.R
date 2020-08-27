# generate_deathBarPlot: This script takes in CPRC demographic file and implements two analyses: 
# (1) It finds who died in the year following the hurricane
# (2) It computes the number of deaths per 100 adults, for every month from one year before until one year after the hurricane.
# Input: CPRCdemographicfile_acquired_03.2020.csv; allScans.txt; DOMINANCE.txt
# Output: Longitudinal deaths/100 adults bar plot. 

library(lme4)
library(lubridate)
library(dplyr)
library(grr)
library(pracma)
library(data.table)


#load local functions
setwd("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
load("R.Data/SocialCapital.RData")

#Load data
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
dominance = read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt", header = T); 
Demographics = read.csv("Behavioral_Data/CPRCdemographicfile_acquired_03.2020.csv"); names(Demographics)[1]="id"
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt"); 

#Only select study population
study.pop = filter(allScans,allScans$group=="V"|allScans$group=="KK")
study.pop.IDs.V = as.character(unique(study.pop$focalID[study.pop$group=="V"]))
study.pop.IDs.KK = as.character(unique(study.pop$focalID[study.pop$group=="KK"]))


#Format date, letters etc.
Demographics$Sex = toupper(Demographics$Sex); 
Demographics$Status = toupper(Demographics$Status); 
Demographics$DateTransfer = NULL; Demographics$EventDate = NULL; Demographics$Last.Event = NULL; 

Demographics$DOD <- mdy(as.character(Demographics$DOD))
Demographics$YOD <- year(as.character(Demographics$DOD))
Demographics$MOD <- month(as.character(Demographics$DOD))
Demographics$DOB <- mdy(as.character(Demographics$DOB))
Demographics$YOB <- year(as.character(Demographics$DOB))
Demographics$AgeAtDeath = Demographics$YOD - Demographics$BirthSeason 
Demographics$AgeAtDeath[Demographics$AgeAtDeath <0]=0 #Note: there are discrepancies between birth season and death date
# boxplot(Demographics$AgeAtDeath[Demographics$AgeAtDeath >0])

####################################################
# Find who died in V and KK post hurricane
####################################################

Deaths.postHurr =  filter(Demographics,Demographics$DOD >= as.Date("2017-09-01") & Demographics$DOD < as.Date("2018-10-01"))#Find deaths before the hurricane
#only onsider V and KK deaths
Deaths.study.pop = filter(Deaths.postHurr, Deaths.postHurr$LastGroup=="V"|Deaths.postHurr$LastGroup=="KK")
#Add old age factor:
Deaths.study.pop$isOld=0; Deaths.study.pop$isOld[Deaths.study.pop$AgeAtDeath>17]=1
#only consider deaths of juveniles +
Deaths.study.pop = filter(Deaths.study.pop, Deaths.study.pop$AgeAtDeath>2)
#get age category of ID who died
Deaths.study.pop$AgeCat = "Adults"; Deaths.study.pop$AgeCat[Deaths.study.pop$AgeAtDeath<6]="Juv"; Deaths.study.pop$AgeCat[Deaths.study.pop$AgeAtDeath>17]="Old"
#Get dominance
dominance2017=filter(dominance,dominance$YEAR>=2015)
# Deaths.study.pop$ord.rank=dominance$ORD_RANK[match(Deaths.study.pop$id,dominance$ID)]

#get number of focal death in each group
table(droplevels(Deaths.study.pop$LastGroup))
table(droplevels(Deaths.study.pop$LastGroup), Deaths.study.pop$AgeCat)
table(droplevels(Deaths.study.pop$LastGroup), Deaths.study.pop$Sex)
# table(droplevels(Deaths.study.pop$LastGroup), Deaths.study.pop$ordinal.rank)

################################################
# Compute moving average deaths (bar plot)
################################################

years=c(2016,2016,2016,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,
        2018,2018,2018,2018,2018,2018,2018,2018,2018,2018)
months=c(10,11,12,01,02,03,04,05,06,07,08,10,11,12,01,02,03,04,05,06,07,08,09,10)
#Important note: I skip sept 2017 because there are no deaths on records, probably because the survey was done end of sept (after 28th sept)
#I'll consider death in october to be deaths related to the hurricane.
monthYears=paste(months,years,sep=".")
death.month.adults=c(); my=1
for (my in 1:length(monthYears)){
  Demographics.temp =Demographics
  Demographics.temp$age=years[my]-Demographics.temp$YOB
  # Demographics.temp=filter(Demographics.temp,Demographics.temp$age<4)
  change.status = which(Demographics.temp$DOD > as.Date(paste(years[my],months[my],"28",sep="-")))
  Demographics.temp$Status[change.status] = "IN CS"
  
  alive = length(which(Demographics.temp$Status=="IN CS"))
  deaths = length(which(Demographics.temp$DOD >= as.Date(paste(years[my],months[my],"01",sep="-")) & Demographics.temp$DOD <= as.Date(paste(years[my],months[my],"28",sep="-"))))
  death.month.adults[my]= deaths/alive*100
}
# months.plot=c("oct","nov","dec","jan","fev","mar","avr","mai","jun","jul","aug","sep","oct","nov","dec","jan","fev","mar","avr","mai","jun","jul","aug","sep")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Demography")

tiff("Deaths_barplot_all.tiff",units="in", width=8, height=5, res=300, compression = 'lzw')
barplot(death.month.adults,ylab="deaths per 100 monkeys",main="Deaths per 100 monkeys", ylim=c(0,4))
segments(14.5,0,14.5,4,col="red",lwd=4, lty=2)
dev.off()

