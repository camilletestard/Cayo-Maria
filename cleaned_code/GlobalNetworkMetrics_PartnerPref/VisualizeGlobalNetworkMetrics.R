#Visualize Global Network Metrics
# This script aims at visualizing change in grooming and proximity network densities, 
# divided by group and year.
# Input: "AllStats.RData" (output of generate_GlobalNetworkMetrics.R)
# Output: proximity-based and groom-based network density distribution from  multiple iterations,
# for all groups and years.

library(ggplot2)
library(gridExtra)
library(data.table)

#Load data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/AllStats.RData")

###################################################
# plot change in proximity and grooming density
###################################################

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/GlobalNetworkMetrics")

group=c("V","V","V","KK","KK")
year=c("2015","2016","2017","2015","2017")
groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017")
for (gy in 1:length(groupyear)){ #For each group
  name.0 = paste(groupyear[gy],".0",sep="")
  AllStats[[name.0]]$isPost = 0; AllStats[[name.0]]$group=group[gy]; AllStats[[name.0]]$year=year[gy]
  name.1 = paste(groupyear[gy],".1",sep="")
  AllStats[[name.1]]$isPost = 1; AllStats[[name.1]]$group=group[gy]; AllStats[[name.1]]$year=year[gy]
}

#Pooling data from multiple years:
PooledData = rbindlist(AllStats)
PooledData.groom= PooledData[PooledData$`actions[a]`=="groom"]
PooledData.prox= PooledData[PooledData$`actions[a]`=="prox"]

groom.dens<-ggplot(PooledData.groom, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
  geom_boxplot()+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle(paste("Density All GroupYears"))+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Density of social network")+
  facet_grid(~group)
# facet_grid(group~year)
ggsave("groom.dens.tiff",prox.dens)
ggsave("groom.dens.eps",prox.dens)

###################################################
# Plot change in grooming density divided by group
###################################################

PooledData.groom.V=PooledData.groom[PooledData.groom$group=="V",]
PooledData.groom.KK=PooledData.groom[PooledData.groom$group=="KK",]

groom.dens.V<-ggplot(PooledData.groom.V, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
  geom_violin()+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle(paste("Density All GroupYears"))+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Density of social network")+
  facet_grid(~year)
ggsave("groom.dens.V.tiff",groom.dens.V)
ggsave("groom.dens.V.eps",groom.dens.V)

groom.dens.KK<-ggplot(PooledData.groom.KK, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
  geom_violin()+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle(paste("Density All GroupYears"))+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Density of social network")+
  facet_grid(~year)
ggsave("groom.dens.KK.tiff",groom.dens.KK)
ggsave("groom.dens.KK.eps",groom.dens.KK)

###################################################
# Plot change in proximity density divided by group
###################################################
PooledData.prox.V=PooledData.prox[PooledData.prox$group=="V",]
PooledData.prox.KK=PooledData.prox[PooledData.prox$group=="KK",]

prox.dens.V<-ggplot(PooledData.prox.V, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
  geom_violin()+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle(paste("Density All GroupYears"))+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Density of social network")+
  facet_grid(~year)
ggsave("prox.dens.V.tiff",prox.dens.V)
ggsave("prox.dens.V.eps",prox.dens.V)

prox.dens.KK<-ggplot(PooledData.prox.KK, aes(x= as.factor(isPost), y=dens, fill=as.factor(isPost) ))+
  geom_violin()+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle(paste("Density All GroupYears"))+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Density of social network")+
  facet_grid(~year)
ggsave("prox.dens.KK.tiff",prox.dens.KK)
ggsave("prox.dens.KK.eps",prox.dens.KK)