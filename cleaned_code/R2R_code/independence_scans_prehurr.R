#Check distribution of scan observations pre-hurricane
library(ggplot2)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

sub = 1
for (sub in 2:10){
  
  print(paste('%%%%%%%%',sub,'%%%%%%%%'))
  
  #Subsample data
  ExSubScans = calcRandomScans(allScans)
  
  # PRE-HURRICANE

  #Take the pre-hurricane observations
  pre_obs = ExSubScans[ExSubScans$isPost==0,]
  #pre_obs$unq.scan.id=as.numeric(pre_obs$unq.scan.id)
  
  #Compare observed and theoretical distribution of rates 
  unq_obs = unique(droplevels(pre_obs$observation.name))
  obs=1; prop.groom.observed=vector(); prop.groom.theoretical = vector()
  for (obs in 1:length(unq_obs)){

    #Get observed distribution of probability of grooming
    sample = pre_obs[droplevels(pre_obs$observation.name) == unq_obs[obs],]
    #prop.prox.observed[obs] = length(which(sample$isProx ==1))/length(sample)
    prop.groom.observed[obs] = length(which(sample$isSocial ==1))/length(sample)
    
    #Get theoretical distribution given independence of observations
    num_obs = nrow(sample)
    scans = sample(1:1000, num_obs, replace=TRUE)
    isGrooming = rep(0, num_obs)
    isGrooming[which(scans>995)] = 1
    
    prop.groom.theoretical[obs]=length(which(isGrooming==1))/num_obs
  }
  length(which(prop.groom.observed>0.4)); length(which(prop.groom.theoretical>0.4)); 
  mean(prop.groom.observed); mean(prop.groom.theoretical)
  
  setwd('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results')
  
  tiff(paste('Test independence of obs. pre-hurricane ',sub,'.tiff',sep=""))
  hist(prop.groom.observed, breaks = 5,col=rgb(0,1,0,1), cex.lab=1.5,main = "",
       xlab ="proportion of grooming event in a scan", xlim=c(0,0.2))
  hist(prop.groom.theoretical,breaks = 20,add=T, col=rgb(0,0,1,0.5))#[prop.groom.theoretical!=0], 20)
  legend("topright", c("Observed distribution", "Theoretical distribution \n(assuming independence of obs.)"),
         fill=c("green", "blue"), cex=1.2)
  box()
  dev.off()
  
  # setwd('C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data')
  # save(mean_num_obs_pre, mean_num_obs_pre, sd_num_obs_pre,
  #      sd_num_obs_pre,num_obs_per_scan_pre, num_obs_per_scan_pre,file="Check_Indep_Obs_v2.RData")
}

#Check 
setwd('C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data')
load("Check_Indep_Obs.RData")
mean(mean_num_obs_pre); mean(sd_num_obs_pre)
hist(mean_num_obs_pre, xlim=c(2,12))
hist(mean_num_obs_pre, add=T)


ggplot(pre_obs, aes(x=observation.name))+
  geom_bar()
