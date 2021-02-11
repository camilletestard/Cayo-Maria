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
library(reshape2)

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

years=c(1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,
        2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018)
months=c(01,02,03,04,05,06,07,08,09,10,11,12)

#I'll consider death in october to be deaths related to the hurricane.
monthYears=paste(months,years,sep=".")
alive=matrix(nrow=length(years), ncol=length(months)); deaths=matrix(nrow=length(years), ncol=length(months));death.rate=matrix(nrow=length(years), ncol=length(months)); m=1; y=1
for (y in 1:length(years)){
  for (m in 1:length(months)){
    Demographics.temp =Demographics
    Demographics.temp$age=years[y]-Demographics.temp$YOB
    # Demographics.temp=filter(Demographics.temp,Demographics.temp$age>=4)
    change.status = which(Demographics.temp$DOD > as.Date(paste(years[y],months[m],"28",sep="-")))
    Demographics.temp$Status[change.status] = "IN CS"
    
    alive[y,m] = length(which(Demographics.temp$Status=="IN CS"))
    deaths[y,m] = length(which(Demographics.temp$DOD >= as.Date(paste(years[y],months[m],"01",sep="-")) 
                               & Demographics.temp$DOD <= as.Date(paste(years[y],months[m],"28",sep="-"))))
    death.rate[y,m]= deaths[y,m]/alive[y,m]*100
  }
}

data = death.rate
colnames(data)=c("jan.","fev.","mar.","apr.","may","june","jul.","aug.",
                       "sept.","oct.","nov.","dec.")
rownames(data)=c("1998","1999","2000","2001","2002","2003",
                       "2004","2005","2006","2007","2008","2009",
                       "2010","2011","2012",
                       "2013","2014","2015","2016","2017","2018")

data = as.data.frame(melt(data))
data$Var1=as.factor(data$Var1)
data$Var3=1; data$Var3[data$Var2=="fev."]=2; data$Var3[data$Var2=="mar."]=3
data$Var3[data$Var2=="apr."]=4; data$Var3[data$Var2=="may"]=5; data$Var3[data$Var2=="june"]=6; data$Var3[data$Var2=="jul."]=7
data$Var3[data$Var2=="aug."]=8; data$Var3[data$Var2=="sept."]=9; data$Var3[data$Var2=="oct."]=10; data$Var3[data$Var2=="nov."]=11; data$Var3[data$Var2=="dec."]=12
#data$Var2=factor(data$Var2, levels = c("june","jul.","aug.","sept.","oct.","nov.","dec.","jan.","fev.","mar.","apr.","may"))

deaths_hurr = data[data$Var1==2017 | data$Var1==2018,]
death_nothurr = data[data$Var1!=2017 & data$Var1!=2018,]

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Demography")

death.plot<-ggplot(data=death_nothurr, aes(x=Var2, y=value)) +
  geom_boxplot(outlier.shape =NA)+theme_classic(base_size = 20)+ #ylim(0.2)+
  geom_point(data=deaths_hurr,aes(x=Var3, y=value, colour=Var1), size=3)+
  geom_line(data=deaths_hurr, aes(x=Var3, y=value, colour=Var1), size=1)+
  #geom_smooth(data=deaths_hurr, aes(x=Var3, y=value, colour=Var1), size=1, se=FALSE)+
  geom_vline(xintercept=9.5, linetype=1, color='red', size=2)+
  ylab('Deaths per 100 individuals')+xlab('')+
  # ylab('Number of Deaths')+xlab('')+
  theme(axis.text.x = element_text(angle = 45, hjust=1.1))

ggsave('deathrateplot.png')
ggsave('deathrateplot.eps', death.plot)



