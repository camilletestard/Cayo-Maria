#Modeling Global Network Metrics
# Test the difference in proximity and grooming network density distributions 
# pre-to-post hurricane, for each group and year seperately.
# Compute the difference between pre and post-hurricane network density for each subsampling iterations. We will get a 
# distribution of that difference. I then compute and save the mean difference and 95% confidence interval.
# Output: .csv files with mean difference and CI, for grooming and proximity and each group and year separately (total = 10 estimates).  

library(data.table)
library(overlapping)
library(matrixStats)
library(gridExtra) 
library(graphics)


load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/AllStats.RData")

##########################################################
#Pooling all data together
##########################################################
group = c("V", "V", "V", "KK", "KK")
years = c(2015, 2016, 2017, 2015, 2017)
groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017"); gy=1
for (gy in 1:length(groupyear)){ #For each group
  name.0 = paste(groupyear[gy],".0",sep="")
  AllStats[[name.0]]$isPost = 0
  AllStats[[name.0]]$group = group[gy]
  AllStats[[name.0]]$year = years[gy]
  name.1 = paste(groupyear[gy],".1",sep="")
  AllStats[[name.1]]$isPost = 1
  AllStats[[name.1]]$group = group[gy]
  AllStats[[name.1]]$year = years[gy]
}

#Pooling data from multiple years: 
PooledData = rbindlist(AllStats); PooledData$groupyear = paste(PooledData$group,PooledData$year,sep=".")
data.groom = as.data.frame(PooledData[PooledData$`actions[a]`=="groom",c(2,17,18,19,23,24,25,30,31,32,33,36)]) #Select columns of interest for grooming networks
data.prox = as.data.frame(PooledData[PooledData$`actions[a]`=="prox",c(2,17,18,19,23,24,25,30,31,32,33,36)]) #Select columns of interest for proximity networks

##########################################################
#GROOMING
##########################################################
pref.stats.ALL = list()
for (gy in 1:length(groupyear)){
  
  data.pre = data.groom[which(data.groom$groupyear==groupyear[gy] & data.groom$isPost==0),(1:10)]
  data.post = data.groom[which(data.groom$groupyear==groupyear[gy] & data.groom$isPost==1),(1:10)]
  data.post_pre = data.post - data.pre
  
  Means = colMeans2(as.matrix(data.post_pre)); Means = round(Means,3)
  CI = colQuantiles(as.matrix(data.post_pre), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
  Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
  
  pref.stats.ALL[[gy]] = Estimates
}
# pref.stats.ALL[[3]]

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/GlobalNetworkMetrics/") 

for (gy in 1:length(groupyear)){
write.csv(pref.stats.ALL[[gy]],paste("Groom.density.diff",groupyear[gy],"minObs.csv",sep="_"))
}

##########################################################
#PROXIMITY
##########################################################
pref.stats.ALL = list()
for (gy in 1:length(groupyear)){
  
  data.pre = data.prox[which(data.prox$groupyear==groupyear[gy] & data.prox$isPost==0),(1:10)]
  data.post = data.prox[which(data.prox$groupyear==groupyear[gy] & data.prox$isPost==1),(1:10)]
  data.post_pre = data.post - data.pre
  
  Means = colMeans2(as.matrix(data.post_pre)); Means = round(Means,3)
  CI = colQuantiles(as.matrix(data.post_pre), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
  Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
  
  pref.stats.ALL[[gy]] = Estimates
}

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/GlobalNetworkMetrics/") 

for (gy in 1:length(groupyear)){
  write.csv(pref.stats.ALL[[gy]],paste("Prox.density.diff",groupyear[gy],"csv",sep="."))
}

