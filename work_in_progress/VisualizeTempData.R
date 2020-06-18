#Visualize Temperature Data
#This script will allow us to 

library(ggplot2)

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
tempData = read.csv("Behavioral_Data/cayo_therm_data.csv")

#Format date and add year + quarter info
tempData$date <- lubridate::ymd(as.character(tempData$date))
tempData$year <- lubridate::year(tempData$date)
tempData$month<- lubridate::month(tempData$date)
tempData$day  <- lubridate::day(tempData$date)
tempData$Q    <- lubridate::quarter(tempData$date)
tempData$dateCode    <- paste(tempData$year,tempData$Q, sep=".")

#Plot temperature across time for different temp sensors
table(tempData$dateCode)
countData=data.frame(matrix(NA, nrow=11, ncol=3)); names(countData)=c("month", "year", "count")
countData$month = c("06jun","07jul","08aug","04apr","05may","06jun","07jul","08aug","09sep","10oct","11nov")#countData$month = c(6,7,8,4,5,6,7,8,9,10,11)
countData$year = 2019; countData$year[1:3]=2018
countData$count = c(table(as.factor(tempData$month[tempData$year == 2018])), 
                         table(as.factor(tempData$month[tempData$year == 2019])))

ggplot(countData, aes(x=as.factor(countData$month), y=count))+
  geom_point(size = 4)+
  facet_grid(~year)

ggplot(tempData, aes(x=as.factor(tempData$month), y=Value))+
  geom_boxplot()+
  facet_grid(~state)
