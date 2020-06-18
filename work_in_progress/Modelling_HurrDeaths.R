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
Demographics = read.csv("Behavioral_Data/CPRCdemographicfile_acquired_03.2020.csv"); names(Demographics)[1]="id"
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt"); 


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

################################################
# Compute moving average deaths
################################################

#Find pre-hurricane deaths 
preHurrDeaths = filter(Demographics,Demographics$DOD >= as.Date("2014-09-16") & Demographics$DOD < as.Date("2017-09-16"))#Find deaths before the hurricane
preYears = 2014:2017
# Structure with deaths per months of each pre-hurricane year of interest
preDeaths=matrix(NA, ncol=12, nrow=length(preYears))
for (y in 1:length(preYears)){ #for all pre-hurricane years we consider
  for (m in 1:12){ #for all months of the year
    preDeaths[y,m]=length(which(preHurrDeaths$YOD == preYears[y] & preHurrDeaths$MOD == m))
  }
}
preDeaths.long=Reshape(t(preDeaths),1)[9:44]

#Find post-hurricane deaths 
postHurrDeaths = filter(Demographics, Demographics$DOD >= as.Date("2017-09-16"))#Find deaths after hurricane
postYears = 2017:2020
# Structure with deaths per months of each post-hurricane year of interest
postDeaths=matrix(NA, ncol=12, nrow=length(postYears))
for (y in 1:length(postYears)){ #for all years
  for (m in 1:12){ #for all months of the year
    postDeaths[y,m]=length(which(postHurrDeaths$YOD == postYears[y] & postHurrDeaths$MOD == m))
  }
}
postDeaths.long=Reshape(t(postDeaths),1)[10:38]

#Plot moving average deaths from 2014 to 2020
all.Deaths = c(preDeaths.long,postDeaths.long)
all.Deaths.smooth = frollmean(all.Deaths,3)
# plot(all.Deaths, type="l");
plot(all.Deaths.smooth, type="l", lwd=2, col="blue"); abline(v=length(preDeaths.long), lwd=2, lty=2,col="red")


################################################
# 
################################################

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