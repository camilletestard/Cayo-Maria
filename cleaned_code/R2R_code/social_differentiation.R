# load libraries
library(dplyr)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(sna)
library(asnipe)
library(vegan)
library(MASS)
library(bbmle)
library(binom)
library(abind)
library(ggplot2)
library(reshape2)
library(netdiffuseR)
library(matrixStats)
library(data.table)


#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/functions_social_differentiation.R")

#Load scan data and population info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/") 
allScans = read.csv("Data All Cleaned/allScans.txt")

#Get pre-hurricane scans
PrePostScans =  allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
SubScans=PrePostScans[-which(PrePostScans$year==2019),] #remove 2019
PreScans = SubScans[which(SubScans$isPost==0),]
PreScans$groupyear = paste(PreScans$group, PreScans$year,sep="")

#For each group, each year separately: 
groupyears = c("V2015", "V2016", "V2017","KK2015", "KK2017"); gy=2; 
social.diff.beta = list(); social.diff.whitehead = list(); numscans.dyad = list(); numscans.per.id = list()
for (gy in 1:length(groupyears)){
  
  #Get full raw data for that group & year
  rscans =  PreScans[PreScans$groupyear == groupyears[gy],]#subsampled scans for group g & year
  unqIDs = unique(c(as.character(rscans$focalID)))
  
  input.data = get_inputdata(rscans, unqIDs)
  numscans.dyad[[gy]] = input.data[,2]
  numscans.per.id[[gy]] = rbind(input.data[,3], input.data[,4])
  
  social.diff.beta[[gy]] = social_differentiation(input.data[,1], input.data[,2], method = "Beta-binomial")
  df=as.data.frame(social.diff.beta[[gy]])
  # social.diff.whitehead[[gy]] = social_differentiation(input.data[,1], input.data[,2], method = "Whitehead")
  # write.csv(df, file = paste("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/precision_net_", groupyears[gy],"_minObs.csv", sep=""))
}
mean(unlist(numscans.dyad))
sd(unlist(numscans.dyad))

mean(unlist(numscans.per.id))
sd(unlist(numscans.per.id))
 