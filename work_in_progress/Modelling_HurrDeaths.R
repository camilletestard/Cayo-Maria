#Hurricane Deaths
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
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt"); 

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
# Compute moving average deaths
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
  Demographics.temp=filter(Demographics.temp,Demographics.temp$age>=4)
  change.status = which(Demographics.temp$DOD > as.Date(paste(years[my],months[my],"28",sep="-")))
  Demographics.temp$Status[change.status] = "IN CS"
  
  alive = length(which(Demographics.temp$Status=="IN CS"))
  deaths = length(which(Demographics.temp$DOD >= as.Date(paste(years[my],months[my],"01",sep="-")) & Demographics.temp$DOD <= as.Date(paste(years[my],months[my],"28",sep="-"))))
  death.month.adults[my]= deaths/alive*100
}
# months.plot=c("oct","nov","dec","jan","fev","mar","avr","mai","jun","jul","aug","sep","oct","nov","dec","jan","fev","mar","avr","mai","jun","jul","aug","sep")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Demography")

tiff("Deaths_barplot.tiff",units="in", width=8, height=5, res=300, compression = 'lzw')
barplot(death.month.adults,ylab="deaths per 100 adults",main="Deaths per 100 adults (age>=4)")
segments(13.5,0,13.5,3,col="red",lwd=4)
dev.off()



####################################################
# Model death and births based on social parameters
####################################################

num_iter = 25; dprob.ALL = data.frame(); iter =1
#Calculate random subsamples
for (iter in 1:num_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  randomScans = calcRandomScans(allScans)
  
  ########################################################################################################
  # 1. Find birth and death status of focal IDs
  data = randomScans
  allIDs = as.character(unique(data$focalID))
  
  #Find birth and deaths post-hurricane
  postHurrDeaths = filter(Demographics, Demographics$DOD >= as.Date("2017-09-16"))#Find deaths after hurricane
  # hist(postHurrDeaths$AgeAtDeath)
  postHurrBirths = filter(Demographics, Demographics$DOB >= as.Date("2017-09-16"))#Find births after hurricane
  
  DeathsBirths = data.frame(matrix(ncol=7, nrow=length(allIDs))); names(DeathsBirths)=c("id","sex","age","percentrank","group","isDead","numBirths")
  for (id in 1:length(allIDs)){
    DeathsBirths$id[id] = allIDs[id]
    DeathsBirths$sex[id] = as.character(unique(data$sex[which(data$focalID==allIDs[id])]))
    DeathsBirths$age[id] = as.character(unique(data$age[which(data$focalID==allIDs[id])]))
    DeathsBirths$percentrank[id] = as.character(unique(data$percentrank[which(data$focalID==allIDs[id])]))
    DeathsBirths$group[id] = as.character(unique(data$group[which(data$focalID==allIDs[id])]))
    DeathsBirths$isDead[id] = ifelse(allIDs[id] %in% postHurrDeaths$id, 1, 0)
    DeathsBirths$numBirths[id] = ifelse(DeathsBirths$sex[id]=="F",length(which(postHurrBirths$BehaviorMom == allIDs[id])),
                                        length(which(postHurrBirths$SireGen == allIDs[id])))
    DeathsBirths$AgeAtDeath[id]=ifelse(DeathsBirths$isDead[id]==1,postHurrDeaths$AgeAtDeath[which(postHurrDeaths$id==allIDs[id])],NA)
  }
  
  length(which(DeathsBirths$isDead==1))
  boxplot(DeathsBirths$AgeAtDeath[which(DeathsBirths$isDead==1)])
  DeathsBirths[(which(DeathsBirths$isDead==1)),]
  
  ########################################################################################################
  # 1. Compute change in p(Acc) and p(Social), per individual, per year
  
  group = c("KK","KK","V", "V", "V")
  years = c(2015,2017,2015,2016,2017)
  groupyears = c("KK2015", "KK2017","V2015", "V2016", "V2017")
  SocialChange.ALL = data.frame()
  gy = 1
  
  for (gy in 1:length(groupyears)){
    
    print(paste("%%%%%%%%%%%%%%%%%% ",groupyears[gy], "%%%%%%%%%%%%%%%%%%"))
    
    rscans = randomScans[which(randomScans$year == years[gy] & randomScans$group == group[gy]),]
    # rscans = randomScans[which(randomScans$year == years[gy] & randomScans$group == group[gy]),]
    
    unqIDs = as.character(unique(rscans$focalID))
    dprob=data.frame(matrix(NA, nrow=length(unqIDs),ncol=4)); colnames(dprob)=c("id","dpAcc","dpSocial","num_obs")
    for (id in 1:length(unqIDs)){ #For all individuals
      isProx.pre = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isProx.post = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      isSocial.pre = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isSocial.post = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      dpAcc=NA; dpSocial=NA; num_obs = length(isProx.pre)
      if (length(isProx.pre)>=20) { #If there are more than 10 observations for that individual
        pACC.pre = sum(isProx.pre)/length(isProx.pre)
        pACC.post = sum(isProx.post)/length(isProx.post)
        dpAcc = pACC.post - pACC.pre
        pSocial.pre = sum(isSocial.pre)/length(isSocial.pre)
        pSocial.post = sum(isSocial.post)/length(isSocial.post)
        dpSocial = pSocial.post - pSocial.pre
      } #end of #observation if
      dprob[id,]=c(unqIDs[id],dpAcc,dpSocial,num_obs)
    } #end of all IDs for loop
    dprob$groupyear = groupyears[gy]
    dprob$year = years[gy]
    dprob.ALL = rbind(dprob.ALL, dprob)
  } #end of all group years for loop

  changeSocialRates = dprob.ALL[-which(is.na(dprob.ALL$dpAcc)),] #remove NA
  #combine change in social rates and death + numBirths
  changeSocialRates[,colnames(DeathsBirths)[-1]]=DeathsBirths[match(changeSocialRates$id,DeathsBirths$id),colnames(DeathsBirths)[-1]]
  
  
  ################################################
  # Model odds of deaths given change in social rates. 
  ################################################
  #Scale parameters: 
  changeSocialRates$sex = as.factor(changeSocialRates$sex); 
  changeSocialRates$isDead = as.factor(changeSocialRates$isDead); 
  changeSocialRates$age = as.numeric(changeSocialRates$age)
  changeSocialRates$percentrank = as.numeric(changeSocialRates$percentrank)/100
  changeSocialRates$dpAcc = as.numeric(changeSocialRates$dpAcc)
  changeSocialRates$dpSocial = as.numeric(changeSocialRates$dpSocial)
  changeSocialRates$num_obs = as.numeric(changeSocialRates$num_obs)
  changeSocialRates$group = as.factor(changeSocialRates$group)
  changeSocialRates$year = as.factor(changeSocialRates$year)
  changeSocialRates$groupyear = as.factor(changeSocialRates$groupyear)
  changeSocialRates[,c("age")] <- scale(changeSocialRates[,c("age")])
  
  predictDeath.prox <- glmer(isDead~ dpAcc + sex + age + percentrank + group +(1|id) +(1|year), data = changeSocialRates, family = binomial(link = "logit"))
  summary(predictDeath.prox)
  #Note: including groupyear or year, variance does no deviate from zero
  
  predictDeath.groom <- glmer(isDead~ dpSocial + sex + age + percentrank + group +(1|id), data = changeSocialRates, family = binomial(link = "logit"))
  summary(predictDeath.groom)
  
  performance::check_model(predictDeath)
  
} #end of all iterations for loop 