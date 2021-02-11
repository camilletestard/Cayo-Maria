#Check distribution of scan observations post-hurricane
library(ggplot2)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

# mean_num_obs_pre = vector(); sd_num_obs_pre = vector(); num_obs_per_scan_pre = list()
# mean_num_obs_post = vector(); sd_num_obs_post = vector(); num_obs_per_scan_post = list()
sub = 1
for (sub in 2:10){
  
  print(paste('%%%%%%%%',sub,'%%%%%%%%'))
  
  #Subsample data
  ExSubScans = calcRandomScans(allScans)
  
  # PRE-HURRICANE
  #Take the post-hurricane observations
  pre_obs = ExSubScans[ExSubScans$isPost==0,]
  #post_obs$unq.scan.id=as.numeric(post_obs$unq.scan.id)

  #Get distribution of observations per scan
  num_obs_per_scan_pre[[sub]] = as.data.frame(table(droplevels(pre_obs$observation.name)))
  mean_num_obs_pre[sub] = mean(num_obs_per_scan_pre[[sub]]$Freq);
  sd_num_obs_pre[sub] = sd(num_obs_per_scan_pre[[sub]]$Freq)
  
  
  # POST-HURRICANE
  #Take the post-hurricane observations
  post_obs = ExSubScans[ExSubScans$isPost==1,]
  #post_obs$unq.scan.id=as.numeric(post_obs$unq.scan.id)
  
  #Get distribution of observations per scan
  num_obs_per_scan_post[[sub]] = as.data.frame(table(droplevels(post_obs$observation.name)))
  mean_num_obs_post[sub] = mean(num_obs_per_scan_post[[sub]]$Freq); 
  sd_num_obs_post[sub] = sd(num_obs_per_scan_post[[sub]]$Freq)

  setwd('C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data')
  save(mean_num_obs_post, mean_num_obs_pre, sd_num_obs_post,
       sd_num_obs_pre,num_obs_per_scan_post, num_obs_per_scan_pre,file="Check_Indep_Obs_v2.RData")
}

#Check 
setwd('C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data')
load("Check_Indep_Obs.RData")
hist(mean_num_obs_pre, xlim=c(2,12))
hist(mean_num_obs_post, add=T)


ggplot(post_obs, aes(x=observation.name))+
  geom_bar()
