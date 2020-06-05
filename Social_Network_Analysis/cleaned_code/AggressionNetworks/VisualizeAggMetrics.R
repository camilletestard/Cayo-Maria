# VisualizeAggMetrics: 
# This script plots the output from generate_AggNetworkMetrics, i.e. density and mean weights from aggression networks.
# Inputs: "AggNetMetrics.RData".
# Outputs: Boxplots of aggression network metrics separated by year and group.

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AggNetMetrics.RData")
density.all$isPost = 0; density.all$isPost[which(density.all$year>2017)] = 1
mean.weight.all$isPost = 0; mean.weight.all$isPost[which(mean.weight.all$year>2017)] = 1

ggplot(density.all, aes(x= as.factor(year), y=dens, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  facet_grid(~group)+
  ggtitle("Aggression Network Density")+
  labs(fill = "Hurricane Status", x="year",y="Density")


ggplot(mean.weight.all, aes(x= as.factor(year), y=weight, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  facet_grid(~group)+
  ggtitle("Aggression Network Mean Weight")+
  labs(fill = "Hurricane Status", x="year",y="Mean Weight Interaction")