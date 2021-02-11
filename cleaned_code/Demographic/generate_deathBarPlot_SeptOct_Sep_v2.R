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
library(ggplot2)
library(gmodels)


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


################################################
# Compute moving average deaths (bar plot)
################################################

years=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
months=c(09,09,09,09,09,09,09,09,09,09,09,09,09,09,09,09,09,09,09,09)

#I'll consider death in october to be deaths related to the hurricane.
monthYears=paste(months,years,sep=".")
alive=c(); deaths=c();death.month.adults=c(); my=2
for (my in 1:length(monthYears)){
  Demographics.temp =Demographics
  Demographics.temp$age=years[my]-Demographics.temp$YOB
  # Demographics.temp=filter(Demographics.temp,Demographics.temp$age>=4)
  change.status = which(Demographics.temp$DOD > as.Date(paste(years[my],months[my],"28",sep="-")))
  Demographics.temp$Status[change.status] = "IN CS"
  
  alive[my] = length(which(Demographics.temp$Status=="IN CS"))
  deaths[my] = length(which(Demographics.temp$DOD >= as.Date(paste(years[my],months[my],"01",sep="-")) & Demographics.temp$DOD <= as.Date(paste(years[my],months[my]+1,"28",sep="-"))))
  death.month.adults[my]= deaths[my]/alive[my]*100
}

ci(deaths, confidence=0.95)
hist(deaths,10)

death.df=data.frame(month=c("sept/oct.07","sept/oct.08","sept/oct.09",
                            "sept/oct.10","sept/oct.11","sept/oct.12",
                            "sept/oct.13","sept/oct.14","sept/oct.15",
                            "sept/oct.16","sept/oct.17","sept/oct.18","sept/oct.19"),
                    death.per.alive=death.month.adults, deaths = deaths, alive = alive)
death.df$month <- factor(death.df$month, levels = c("sept/oct.07","sept/oct.08","sept/oct.09",
                                                    "sept/oct.10","sept/oct.11","sept/oct.12",
                                                    "sept/oct.13","sept/oct.14","sept/oct.15",
                                                    "sept/oct.16","sept/oct.17","sept/oct.18","sept/oct.19"))
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Demography")

death.plot<-ggplot(data=death.df, aes(x=month, y=deaths)) +
  geom_bar(stat="identity")+theme_classic(base_size = 30)+ ylim(0,50)+
  ylab('# Deaths')+xlab('')+
  geom_vline(xintercept=11, linetype=1, color='red', size=2)+
  # theme(axis.title.y = element_text(size = 20))+
  # theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels= c("sept/oct.07","","sept/oct.09",
                             "","sept/oct.11","",
                             "sept/oct.13","","sept/oct.15",
                             "","sept/oct.17","","sept/oct.19")) +
  #c("october","november","december","january","february","march","april","may","june","july","august",
  #"sept./oct.","november","december","january","february","march","april","may","june","july","august","september","october"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1.1))

ggsave('absdeathplot.png')
ggsave('absdeathplot.eps', death.plot)

