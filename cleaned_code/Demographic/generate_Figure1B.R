# generate_deathBarPlot: This script takes in CPRC demographic file and computes the number of deaths per 100 adults, 
# for every month from 1998 to 2018 (one year after the hurricane).
# Input: CPRCdemographicfile_acquired_03.2020.csv; allScans.txt; DOMINANCE.txt
# NOTE: The .csv input file to generate this figure is owned by the CPRC and has restricted access. 
# Output: Longitudinal deaths/100 adults plot. 
# Camille Testard - 2020

library(lme4)
library(lubridate)
library(dplyr)
library(grr)
library(pracma)
library(data.table)
library(ggplot2)
library(reshape2)
library(gmodels)

#load local functions
setwd("~/Documents/Github/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
load("R.Data/SocialCapital.RData")

#Load data
setwd("~/Desktop/Desktop-Cayo-Maria/") 
dominance = read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt", header = T); 
Demographics = read.csv("Behavioral_Data/CPRCdemographicfile_acquired_03.2020.csv"); names(Demographics)[1]="id" #This document is owned by CPRC and has restricted access.
Demographics_v2 = read.csv("Behavioral_Data/Dead_missing_LDS.csv"); names(Demographics_v2)[1]="id"#This document is owned by CPRC and has restricted access.
Demographics_v3 = merge(Demographics,Demographics_v2, by="id")
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
alive=matrix(nrow=length(years), ncol=length(months)); deaths=matrix(nrow=length(years), ncol=length(months));
death.rate=matrix(nrow=length(years), ncol=length(months)); m=1; y=1
age.at.death = list(); mean_ageatdeath=matrix(nrow=length(years), ncol=length(months)); monthyear = 1

for (y in 1:length(years)){
  for (m in 1:length(months)){
    Demographics.temp =Demographics
    Demographics.temp$age=years[y]-Demographics.temp$YOB
    Demographics.temp=filter(Demographics.temp,Demographics.temp$age>=6)
    change.status = which(Demographics.temp$DOD > as.Date(paste(years[y],months[m],"28",sep="-")))
    Demographics.temp$Status[change.status] = "IN CS"
    
    alive[y,m] = length(which(Demographics.temp$Status=="IN CS"))
    deaths[y,m] = length(which(Demographics.temp$DOD >= as.Date(paste(years[y],months[m],"01",sep="-")) 
                               & Demographics.temp$DOD <= as.Date(paste(years[y],months[m],"28",sep="-"))))
    age.at.death[[monthyear]] = Demographics$AgeAtDeath[which(Demographics.temp$DOD >= as.Date(paste(years[y],months[m],"01",sep="-")) 
                                                      & Demographics.temp$DOD <= as.Date(paste(years[y],months[m],"28",sep="-")))]
    mean_ageatdeath[y,m] = mean(age.at.death[[monthyear]])
    death.rate[y,m]= deaths[y,m]/alive[y,m]*100
    monthyear=monthyear+1
  }
}

#Find mortality over the year following the hurricane
sum(c(deaths[20,9:12], deaths[21,1:3]))/alive[20,9]

#Plot mortality post-hurricane compared to before.
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

death.plot<-ggplot(data=death_nothurr, aes(x=Var3, y=value, color=Var1)) +
  geom_line(size=0.5)+theme_classic(base_size = 20)+
  geom_point(data=deaths_hurr,aes(x=Var3, y=value, colour=Var1), size=3)+
  geom_line(data=deaths_hurr, aes(x=Var3, y=value, colour=Var1), size=1.5)+
  #geom_smooth(data=deaths_hurr, aes(x=Var3, y=value, colour=Var1), size=1, se=FALSE)+
  geom_vline(xintercept=9.5, linetype=1, color='red', size=2)+
  ylab('Deaths per 100 adults')+xlab('')+
  # ylab('Number of Deaths')+xlab('')+
  scale_x_discrete(limits=c(1:12),
                    labels= c("jan.","","mar.","","may","","jul.","",
                             "sept.","","nov.","")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1.1))

ggsave('deathrateplot_v4.png')
ggsave('deathrateplot_v4.eps', death.plot)

#Test statistically difference in death rate in october 2017
data_oct = data[data$Var2=="oct.",]
oct2017_value = data_oct$value[data_oct$Var1==2017]
ci(data_oct$value)

data_nov = data[data$Var2=="nov.",]
nov2017_value = data_nov$value[data_nov$Var1==2017]
ci(data_nov$value)