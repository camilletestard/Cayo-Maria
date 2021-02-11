#Check distribution of scan observations post-hurricane
library(ggplot2)
library(stats)
library(dplyr)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

sub = 1
for (sub in 1:10){ #For different sub-sampling iterations
  
  print(paste('%%%%%%%%',sub,'%%%%%%%%'))
  
  #Subsample data
  ExSubScans = calcRandomScans(allScans)
  
  # POST-HURRICANE
  #Take the post-hurricane observations
  post_obs = ExSubScans[ExSubScans$isPost==1,]
  #post_obs$unq.scan.id=as.numeric(post_obs$unq.scan.id)
  
  #Compare observed and theoretical distribution of rates 
  unq_obs = unique(droplevels(post_obs$observation.name))
  obs=1; prop.groom.observed=vector(); prop.groom.theoretical = vector()
  for (obs in 1:length(unq_obs)){

    #Get observed distribution of probability of grooming
    sample = post_obs[droplevels(post_obs$observation.name) == unq_obs[obs],]
    prop.groom.observed[obs] = length(which(sample$isSocial ==1))/length(sample)

    #Get theoretical distribution given independence of observations
    num_obs = nrow(sample)
    scans = sample(1:100, num_obs, replace=TRUE)
    isGrooming = rep(0, num_obs)
    isGrooming[which(scans>97)] = 1
    
    prop.groom.theoretical[obs]=length(which(isGrooming==1))/num_obs
  }
  #Get number of extreme values to evaluate differences in the tail
  length(which(prop.groom.observed>0.4)); length(which(prop.groom.theoretical>0.4)); 
  # #Discard extreme values:
  # prop.groom.observed = prop.groom.observed[-which(prop.groom.observed>0.4)]
  # prop.groom.theoretical = prop.groom.theoretical[-which(prop.groom.theoretical>0.4)]
  #Compute mean to ensure they are similar
  mean(prop.groom.observed); mean(prop.groom.theoretical)
  
  #Chi-Square test
  # categories = c("(-0.05,0]","(0,0.05]","(0.05,0.1]","(0.1,0.15]","(0.15,0.2]","(0.2,0.25]","(0.25,0.3]",
  #                "(0.3,0.35]","(0.35,0.4]","(0.4,0.45]","(0.45,0.5]","(0.5,0.55]","(0.55,0.6]","(0.6,0.65]",
  #                "(0.65,0.7]","(0.7,0.75]","(0.75,0.8]","(0.8,0.85]","(0.85,0.9]","(0.9,0.95]","(0.95,1]")
  categories = c("(-0.05,0]","(0,0.05]","(0.05,0.1]","(0.1,0.15]","(0.15,0.2]","(0.2,0.25]","(0.25,0.3]",
                 "(0.3,0.35]",NA)
  dat.binned.theoretical = theoretical.df %>% count(Marks=cut(prop,seq(-0.05,0.35,0.05))) %>%
    mutate(pct = n/sum(n))
  observed.df = as.data.frame(prop.groom.observed); names(observed.df)="prop"
  dat.binned.observed = observed.df %>% count(Marks=cut(prop,seq(-0.05,0.35,0.05))) %>%
    mutate(pct = n/sum(n))
  
  data.test = data.frame(matrix(ncol=3, nrow=length(categories)))
  names(data.test)=c("Marks","observed","theoretical.prob")
  data.test$Marks=categories; data.test$Marks[9]="(0.35,1]"
  data.test$observed = 0; data.test$observed[match(dat.binned.observed$Marks,data.test$Marks)] =dat.binned.observed$n
  data.test$theoretical.prob = 0; data.test$theoretical.prob[match(dat.binned.theoretical$Marks,data.test$Marks)]=dat.binned.theoretical$pct
  
  chisq.test(data.test$observed, data.test$theoretical.prob)
  
  #Plots
  setwd('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results')
  
  tiff(paste('Test independence of obs. post-hurricane ',sub,'.tiff',sep=""))
  hist(prop.groom.observed, breaks = 20,col=rgb(0,1,0,1), cex.lab=1.5,main = "",
       xlab ="proportion of grooming event in a scan", xlim=c(0,0.4))
  hist(prop.groom.theoretical,breaks = 20,add=T, col=rgb(0,0,1,0.5))#[prop.groom.theoretical!=0], 20)
  legend("topright", c("Observed distribution", "Theoretical distribution \n(assuming independence of obs.)"),
         fill=c("green", "blue"), cex=1.2)
  box()
  dev.off()
  
  tiff(paste('Test independence of obs. post-hurricane cumul. ',sub,'.tiff',sep=""))
  plot(ecdf(prop.groom.observed), col = 'green', cex=0, lwd=4, xlab ="proportion of grooming event in a scan",
       ylab = "Cumulative distribution", main="",cex.lab=1.5)
  plot(ecdf(prop.groom.theoretical), col=rgb(0,0,1,0.5), cex=0, lwd=4, add=T)
  legend("bottomright", c("Observed distribution", "Theoretical distribution \n(assuming independence of obs.)"),
         fill=c("green", "blue"), cex=1.2)
  box()
  dev.off()
  # setwd('C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data')
  # save(mean_num_obs_post, mean_num_obs_pre, sd_num_obs_post,
  #      sd_num_obs_pre,num_obs_per_scan_post, num_obs_per_scan_pre,file="Check_Indep_Obs_v2.RData")
}

#Check 
setwd('C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data')
load("Check_Indep_Obs.RData")
hist(mean_num_obs_pre, xlim=c(2,12))
hist(mean_num_obs_post, add=T)


ggplot(post_obs, aes(x=observation.name))+
  geom_bar()
