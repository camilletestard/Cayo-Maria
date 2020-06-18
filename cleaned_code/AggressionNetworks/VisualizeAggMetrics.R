# VisualizeAggMetrics: 
# This script plots the output from generate_AggNetworkMetrics, i.e. density and mean weights from aggression networks.
# Inputs: "AggNetMetrics.RData".
# Outputs: Boxplots of aggression network metrics separated by year and group.

library(glmmTMB)
library(jtools)

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/AggNetMetrics.RData")
density.all=density.all[density.all$group!="S",]
density.all$isPost = 0; density.all$isPost[which(density.all$year>2017)] = 1
mean.weight.all$isPost = 0; mean.weight.all$isPost[which(mean.weight.all$year>2017)] = 1

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Aggression")

tiff("AggressionDensity.tiff", units="in", width=10, height=8, res=300, compression = 'lzw')
ggplot(density.all, aes(x= as.factor(year), y=dens, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  facet_grid(~group)+
  ggtitle("Aggression Network Density")+
  labs(fill = "Hurricane Status", x="year",y="Density")
dev.off()

agg.dens <- glmmTMB(as.numeric(dens) ~ isPost*group, data=density.all, family = beta_family(link="logit"))
summary(agg.dens, digits = 3)
export_summs(agg.dens, model.names = "Agg.dens",to.file = "docx", file.name = "Modeling.AggDens.ALL.docx")

density.KK=density.all[density.all$group=="KK",]
agg.dens <- glmmTMB(as.numeric(dens) ~ isPost, data=density.KK, family = beta_family(link="logit"))
summary(agg.dens, digits = 3)
export_summs(agg.dens, model.names = "Agg.dens",to.file = "docx", file.name = "Modeling.Agg.Dens.KK.docx")

density.V=density.all[density.all$group=="V",]
agg.dens <- glmmTMB(as.numeric(dens) ~ isPost +(1|year), data=density.V, family = beta_family(link="logit"))
summary(agg.dens, digits = 3)
export_summs(agg.dens, model.names = "Agg.dens",to.file = "docx", file.name = "Modeling.Agg.Dens.V.docx")

# #mean weight
# ggplot(mean.weight.all, aes(x= as.factor(year), y=weight, fill=as.factor(isPost)))+
#   geom_boxplot()+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
#   facet_grid(~group)+
#   ggtitle("Aggression Network Mean Weight")+
#   labs(fill = "Hurricane Status", x="year",y="Mean Weight Interaction")