#Visualize Temperature Data
#This script will allow us to 

library(ggplot2)
setwd("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/")
source("cleaned_code/Functions/functions_summarySE.R")

#Load temp data from 2018-2019
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
tempData = read.csv("ThermochronData/cayo_therm_data_2018-2020.csv")

#Load thermochron location
# thermoInfo = read.csv("ThermochronData/Thermochron_deployment1and2_locations.csv")

#Format date and add year + quarter info
tempData$date  <- lubridate::ymd(as.character(tempData$date))
tempData$year  <- lubridate::year(tempData$date)
tempData$month <- lubridate::month(tempData$date)
tempData$day   <- lubridate::day(tempData$date)
tempData$Q     <- lubridate::quarter(tempData$date)
tempData$dateCode    <- paste(tempData$year,tempData$month, sep=".")

#Plot temperature across time for different temp sensors
table(tempData$dateCode)
countData=data.frame(matrix(NA, nrow=17, ncol=3)); names(countData)=c("month", "year", "count")
countData$month = c("06jun","07jul","08aug","04apr","05may","06jun","07jul","08aug","09sep","10oct","11nov","12dec","01jan","02feb","03march","04apr","05may")#countData$month = c(6,7,8,4,5,6,7,8,9,10,11)
countData$year = 2019; countData$year[1:3]=2018; countData$year[13:17]=2020
countData$count = c(table(as.factor(tempData$month[tempData$year == 2018])), 
                    table(as.factor(tempData$month[tempData$year == 2019])),
                    table(as.factor(tempData$month[tempData$year == 2020])))

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Temperature") 

tiff("temp.data.distr.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
ggplot(countData, aes(x=as.factor(countData$month), y=count))+
  geom_point(aes(col=as.factor(year)),size = 4)+
  ggtitle("Count of temperature data")+
  labs(x="Month", col="year")
dev.off()

#############

#plot boxplots of temperature throughout the year
ggplot(tempData, aes(x=as.factor(month), y=Value))+
  geom_boxplot()+
  facet_grid(~state)

#plot boxplots of temperature across vegetation states
ggplot(tempData, aes(x=as.factor(state), y=Value))+
  geom_boxplot()+

#Combine exposed and de-vegetated
tempData$state2=ifelse(tempData$state=="Vegetated", "Vegetated","Exposed")
tempData$state=tempData$state2
tempData$gen.location="Big Cayo"; tempData$gen.location[tempData$Location=="SCL"|tempData$Location=="SCU"]="Small Cayo"

#Split months by "hot" and "cool"
tempData$season="cool"; tempData$season[tempData$month>3 & tempData$month<10]="hot"

tempSumm.month <- summarySE(tempData, measurevar="Value", groupvars=c("month"))
tempSumm.quarter <- summarySE(tempData, measurevar="Value", groupvars=c("Q"))
tempSumm.time <- summarySE(tempData, measurevar="Value", groupvars=c("year","month")); tempSumm.time$time = paste(tempSumm.time$year,tempSumm.time$month, sep=".")
tempSumm.state <- summarySE(tempData, measurevar="Value", groupvars=c("state"))
tempSumm.season <-summarySE(tempData, measurevar="Value", groupvars=c("season"))

tempSumm.month.state <- summarySE(tempData, measurevar="Value", groupvars=c("month","state"))
ggplot(tempSumm.month.state, aes(x=month, y=Value))+
  # geom_boxplot()+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci, col=state), width=.1)+
  geom_line(aes(col=state))+
  geom_point(aes(col=state))+
  ylim(25,40)+
  ggtitle("Temperature split by month and vegetation state")+
  facet_grid(~gen.location)

tiff("temp.by.month.state.location.tiff",units="in", width=7, height=4, res=300, compression = 'lzw')
tempSumm.month.state.location <- summarySE(tempData, measurevar="Value", groupvars=c("month","state","gen.location"))
ggplot(tempSumm.month.state.location, aes(x=month, y=Value))+
  # geom_boxplot()+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci, col=state), width=.1)+
  geom_line(aes(col=state))+
  geom_point(aes(col=state))+
  ylim(25,40)+
  ggtitle("Temperature split by month, location and vegetation state")+
  facet_grid(~gen.location)
dev.off()

tempSumm.q.state.location <- summarySE(tempData, measurevar="Value", groupvars=c("Q","state","gen.location"))
tiff("temp.by.q.state.location.tiff",units="in", width=7, height=4, res=300, compression = 'lzw')
ggplot(tempSumm.q.state.location, aes(x=Q, y=Value))+
  # geom_boxplot()+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci, col=state), width=.1)+
  geom_line(aes(col=state))+
  geom_point(aes(col=state))+
  ylim(25,40)+
  ggtitle("Temperature split by month")+
  facet_grid(~gen.location)
dev.off()

#Show mean and sem temperature divided by months
# tempSumm.month$month<-c("04april","05may","06june","07jul","08aug","09sept","10oct","11nov")
ggplot(tempSumm.month, aes(x=month, y=Value))+
  # geom_boxplot()+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci), width=.1)+
  geom_line()+
  geom_point()+
  ylim(30,37)+
  ggtitle("Temperature split by month")

#Show mean and sem temperature divided by quarter
ggplot(tempSumm.quarter, aes(x=Q, y=Value))+
  # geom_boxplot()+
  geom_errorbar(aes(ymin=Value-se, ymax=Value+se), width=.05)+
  geom_line()+
  geom_point()+
  ylim(30,37)+
  ggtitle("Temperature split by quarter")

#Show mean and ci temperature divided by state
tiff("temp.by.state.location.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
ggplot(tempSumm.state, aes(x=state, y=Value))+
  # geom_boxplot()+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci), width=.05)+
  geom_point(aes(col=state))+
  ylim(30,40)+
  ggtitle("Temperature split by state")
dev.off()

#Difference between the hot and cool months
tiff("temp.by.season.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
ggplot(tempSumm.season, aes(x=season, y=Value))+
  # geom_boxplot()+
  geom_errorbar(aes(ymin=Value-ci, ymax=Value+ci), width=.05)+
  geom_point(aes(col=season))+
  ylim(30,40)+
  ggtitle("Temperature split by season")
dev.off()
  
