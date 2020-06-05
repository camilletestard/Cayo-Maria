library(ggplot2)

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")

allScans$groupyear = paste(allScans$group, allScans$year,sep="")
obs.table.Q = as.data.frame(table(droplevels(as.factor(allScans$groupyear)), as.factor(allScans$Q))); obs.table.Q$sampling = "Quarter"
obs.table.TB = as.data.frame(table(droplevels(as.factor(allScans$groupyear)), as.factor(allScans$timeBlock))); obs.table.TB$sampling = "Time Block"
# obs.table.S = as.data.frame(table(droplevels(as.factor(allScans$groupyear)), as.factor(allScans$sex))); obs.table.S$sampling = "Quarter"
obs.table = rbind(obs.table.Q,obs.table.TB)

ggplot(data = obs.table, aes(x = Var1, y= Freq, fill = Var2)) +
  geom_bar(position="stack", stat="identity")+
  ggtitle("Sampling Effort by Quarter and Time Block")+
  labs(fill = "Time", x="Group Year",y="#Obs")+
  facet_grid(~sampling)

# scans = allScans[which(allScans$groupyear=="HH2016" | allScans$groupyear =="KK2018"),]
# Qfreq = table(scans$isPost, scans$Q)
# TBfreq = table(scans$isPost, scans$timeBlock)