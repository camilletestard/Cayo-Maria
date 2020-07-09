#Plot greenery data for big cayo and small cayo separately

library(lubridate)

#load data
data=read.csv("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/GreeneryData/EOB_raw_data.csv")
names(data)[1]="date"
data$date = mdy(data$date)

#plot change in greenery (NDVI values)
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Temperature")
tiff("Greenery.by.location.tiff", units="in", width=10, height=8, res=300, compression = 'lzw')
ggplot(data, aes(x=date, y=Mean))+
  # geom_errorbar(aes(ymin=Mean-sd, ymax=Mean+sd, col=location), width=.1)+
  geom_line(aes(col=location))+
  geom_point(aes(col=location))+
  ylim(0,0.5)+
  ylab("Mean Greenery index (NDVI)")+
  ggtitle("Greenery index split by location")
dev.off()