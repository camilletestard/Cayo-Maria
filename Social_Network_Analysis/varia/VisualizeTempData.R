setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
tempData = read.csv("Behavioral_Data/cayo_therm_data.csv")

#Format date and add year + quarter info
prox_data$date <- lubridate::dmy(as.character(prox_data$date))
prox_data$year <- lubridate::year(prox_data$date)
prox_data$Q    <- lubridate::quarter(prox_data$date)
prox_data$date <- as.character(prox_data$date) #re-format to character after finding year and quarter