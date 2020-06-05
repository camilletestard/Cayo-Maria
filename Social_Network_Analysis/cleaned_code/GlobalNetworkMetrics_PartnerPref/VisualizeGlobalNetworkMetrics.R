#Visualize Global Network Metrics
#This script aims at visualizing change in grooming and proximity network densities

library(ggplot2)
library(gridExtra)
library(data.table)

#Load data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AllStats.RData")

###################################################
# plot change in proximity and grooming density
###################################################

# groupyear = c("V.2015","V.2016","V.2017","V.2019","KK.2015","KK.2017"); gy=1
groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017"); gy=1
actions = c("groom","prox"); a=1
for (gy in 1:length(groupyear)){ #For each groupyear
  #for pre-hurricane data
  name.0 = paste(groupyear[gy],".0",sep="") #create name groupyear.HurrStatus (0/1)
  data.0 = AllStats[[name.0]]; data.0$isPost = 0 #pull out data and add hurricane status info
  #for post-hurricane data
  name.1 = paste(groupyear[gy],".1",sep="")
  data.1 = AllStats[[name.1]]; data.1$isPost = 1
  
  for (a in 1:2){ #For grooming and proximity
  data= rbind(data.0, data.1) #pool data from that group & year together
  data= data[data$`actions[a]`==actions[a],] #Select data points with action of interest (groom or proximity)
  
  #######################################
  # DENSITY
  
  #Plot density change (over multiple iterations)
  name <-paste(actions[a],"density",gy,sep=".")
  plot <-ggplot(data, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Density ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y=paste("Density of", actions[a],"network", sep=" "))
    #ylim(0, 0.1)
  assign(name, plot)
  
  #######################################
  # CLSUTER COEFF
  
  #Plot clustering change (over multiple iterations)
  name <-paste(actions[a],"clust",gy,sep=".")
  plot <-ggplot(data, aes(x= as.factor(isPost), y=clust.w, fill=as.factor(isPost) ))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle(paste("Cluster.Coeff ",groupyear[gy],sep=""))+
    labs(fill = "Hurricane Status",x="Hurricane Status",y=paste("Clust.Coeff of", actions[a],"network", sep=" "))
  #ylim(0, 0.1)
  assign(name, plot)
  
 } 
}
#combine plots for global network metrics
FullPlot.prox.dens = grid.arrange(prox.density.1, prox.density.2, prox.density.3, prox.density.4, prox.density.5, ncol=3, nrow=2)
FullPlot.prox.clust = grid.arrange(prox.clust.1, prox.clust.2, prox.clust.3, prox.clust.4, prox.clust.5, ncol=3, nrow=2)
FullPlot.groom.dens = grid.arrange(groom.density.1, groom.density.2, groom.density.3,groom.density.4, groom.density.5, ncol=3, nrow=2)
FullPlot.groom.clust = grid.arrange(groom.clust.1, groom.clust.2, groom.clust.3, groom.clust.4, groom.clust.5, ncol=3, nrow=2)

#Save Plots:
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/GlobalNetworkMetrics") 
ggsave(FullPlot.prox.dens, file = paste("Proximity/DensityPlotsProx.png",sep=""))
ggsave(FullPlot.groom.dens, file = paste("Groom/DensityPlotsGroom.png",sep=""))
ggsave(FullPlot.prox.clust, file = paste("Proximity/ClustPlotsProx.png",sep=""))
ggsave(FullPlot.groom.clust, file = paste("Groom/ClustPlotsGroom.png",sep=""))

# ##########################################################
# #Pooling all data together
# #The problem is that each year/group is not on the same scale. This is not very useful.
# 
# groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017")
# for (gy in 1:length(groupyear)){ #For each group
#   name.0 = paste(groupyear[gy],".0",sep="")
#   AllStats[[name.0]]$isPost = 0
#   name.1 = paste(groupyear[gy],".1",sep="")
#   AllStats[[name.1]]$isPost = 1
# }
# 
# #Pooling data from multiple years: 
# PooledData = rbindlist(AllStats)
# 
# ggplot(PooledData, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
#   geom_boxplot()+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
#   ggtitle(paste("Density All GroupYears"))+
#   labs(fill = "Hurricane Status",x="Hurricane Status",y="Density of social network")
