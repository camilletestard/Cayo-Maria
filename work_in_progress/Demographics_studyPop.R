#Load data
setwd("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/")
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt"); 
randomScans = calcRandomScans(allScans)

allIDs = as.character(unique(randomScans$focalID))
age = as.numeric(randomScans$age[match(allIDs,randomScans$focalID)])
sex = as.character(randomScans$sex[match(allIDs,randomScans$focalID)])
group = as.character(randomScans$group[match(allIDs,randomScans$focalID)])

Demographics =data.frame(allIDs,age,sex, group)

table(Demographics$group, Demographics$sex)
range(Demographics$age[Demographics$group=="KK"])
mean(Demographics$age[Demographics$group=="KK"])
sd(Demographics$age[Demographics$group=="KK"])