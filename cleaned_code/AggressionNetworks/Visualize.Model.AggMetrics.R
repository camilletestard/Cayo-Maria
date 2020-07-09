# VisualizeAggMetrics: 
# This script plots the output from generate_AggNetworkMetrics, i.e. density and mean weights from aggression networks.
# Inputs: "AggNetMetrics.RData".
# Outputs: Boxplots of aggression network metrics separated by year and group.

library(glmmTMB)
library(jtools)

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/AggNetMetrics.RData")
# density.all=density.all[density.all$group!="S",]
density.all$isPost = 0; density.all$isPost[which(density.all$year>2017)] = 1
NetworkMetrics.all$isPost = 0; NetworkMetrics.all$isPost[which(NetworkMetrics.all$year>2017)] = 1

#Plot aggression network density 
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Aggression")

tiff("AggressionDensity.tiff", units="in", width=10, height=8, res=300, compression = 'lzw')
ggplot(density.all, aes(x= as.factor(year), y=dens, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  facet_grid(~group)+
  ggtitle("Aggression Network Density")+
  labs(fill = "Hurricane Status", x="year",y="Density")
dev.off()

####################################
#Model change in aggression density
####################################
density.all$dens[density.all$dens==0]=0.000001 #to avoid errors
agg.dens <- glmmTMB(as.numeric(dens) ~ isPost*group +(1|year), data=density.all, family = beta_family(link="logit"))
summary(agg.dens, digits = 3)
export_summs(agg.dens, model.names = "Agg.dens",to.file = "docx", file.name = "Modeling.AggDens.ALL.docx")

density.KK=density.all[density.all$group=="KK",]
agg.dens <- glmmTMB(as.numeric(dens) ~ isPost +(1|year), data=density.KK, family = beta_family(link="logit"))
summary(agg.dens, digits = 3)
export_summs(agg.dens, model.names = "Agg.dens",to.file = "docx", file.name = "Modeling.Agg.Dens.KK.onlyPM.docx")

density.V=density.all[density.all$group=="V",]
density.V$dens[density.V$dens==0]=0.000001 #to avoid errors
agg.dens <- glmmTMB(as.numeric(dens) ~ isPost +(1|year), data=density.V, family = beta_family(link="logit"))
summary(agg.dens, digits = 3)
export_summs(agg.dens, model.names = "Agg.dens",to.file = "docx", file.name = "Modeling.Agg.Dens.V.onlyPM.docx")

####################################
#Model change in aggression strength
####################################

#Check correlation with number of hours
plot(NetworkMetrics.all$deg,NetworkMetrics.all$hrs)
cor.test(NetworkMetrics.all$deg,NetworkMetrics.all$hrs)
plot(NetworkMetrics.all$strength,NetworkMetrics.all$hrs)
cor.test(NetworkMetrics.all$strength,NetworkMetrics.all$hrs)
cor.test(NetworkMetrics.all$strength,NetworkMetrics.all$deg)

#Visualize
Agg.data.pre= filter(NetworkMetrics.all, NetworkMetrics.all$isPost==0)
Agg.data.post= filter(NetworkMetrics.all, NetworkMetrics.all$isPost==1)
#group KK
hist(Agg.data.pre$strength[which(Agg.data.pre$group=="KK")],breaks=30, col=rgb(0,1,1,0.5)); 
hist(Agg.data.post$strength[which(Agg.data.post$group=="KK")],add=T, breaks=30, col=rgb(1,1,0,0.5))

#group V
hist(Agg.data.pre$strength[which(Agg.data.pre$group=="V")],breaks=40, col=rgb(0,1,1,0.5)); 
hist(Agg.data.post$strength[which(Agg.data.post$group=="V")],add=T, breaks=30, col=rgb(1,1,0,0.5))


#Model
NetworkMetrics.all$percentrank=NetworkMetrics.all$percentrank/100
NetworkMetrics.all$age=scale(NetworkMetrics.all$age)

#Full Model
Agg.BM <- glmmTMB(strength~ isPost + group + sex + age + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.all,
                  family =gaussian)
# performance::check_model(Agg.BM)
summary(Agg.BM, digits = 3)
export_summs(Agg.BM, model.names = "Agg.BaseModel", to.file = "docx", file.name = "Modeling.Agg.Strength.BM.docx")

#################
#GROUP
Agg.BM.G <- glmmTMB(strength~ isPost*group + sex + age + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.all,
                  family =gaussian)
# performance::check_model(Agg.BM.G)
summary(Agg.BM.G, digits = 3)
export_summs(Agg.BM.G, model.names = "Agg.GroupModel",to.file = "docx", file.name = "Modeling.Agg.Strength.G.docx")

NetworkMetrics.KK = NetworkMetrics.all[NetworkMetrics.all$group=="KK",]
Agg.BM.KK <- glmmTMB(strength~ isPost*sex + age + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.KK,
                    family =gaussian)
# performance::check_model(Agg.BM.G)
summary(Agg.BM.KK, digits = 3)
export_summs(Agg.BM.KK, model.names = "Agg.KKModel",to.file = "docx", file.name = "Modeling.Agg.Strength.KK.docx")

NetworkMetrics.V = NetworkMetrics.all[NetworkMetrics.all$group=="V",]
Agg.BM.V <- glmmTMB(strength~ isPost*sex + age + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.V,
                     family =gaussian)
# performance::check_model(Agg.BM.G)
summary(Agg.BM.V, digits = 3)
export_summs(Agg.BM.V, model.names = "Agg.VModel",to.file = "docx", file.name = "Modeling.Agg.Strength.V.docx")

#################
#SEX
Agg.BM.S <- glmmTMB(strength~ isPost*sex + group + age + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.all,
                    family =gaussian)
# performance::check_model(Agg.BM.G)
summary(Agg.BM.S, digits = 3)
export_summs(Agg.BM.S, model.names = "Agg.SexModel",to.file = "docx", file.name = "Modeling.Agg.Strength.S.docx")

NetworkMetrics.M = NetworkMetrics.all[NetworkMetrics.all$sex=="M",]
Agg.BM.M <- glmmTMB(strength~ isPost + group + age + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.M,
                    family =gaussian)
# performance::check_model(Agg.BM.G)
summary(Agg.BM.M, digits = 3)
export_summs(Agg.BM.M, model.names = "Agg.MModel",to.file = "docx", file.name = "Modeling.Agg.Strength.M.docx")

NetworkMetrics.F = NetworkMetrics.all[NetworkMetrics.all$sex=="F",]
Agg.BM.F <- glmmTMB(strength~ isPost + group + age + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.F,
                    family =gaussian)
# performance::check_model(Agg.BM.G)
summary(Agg.BM.F, digits = 3)
export_summs(Agg.BM.F, model.names = "Agg.FModel",to.file = "docx", file.name = "Modeling.Agg.Strength.F.docx")

##################
#Age
Agg.A <- glmmTMB(strength~ isPost*age + group + sex + percentrank +hrs + (1|id) + (1|year), data = NetworkMetrics.all,
                  family =gaussian)
# performance::check_model(Agg.BM)
summary(Agg.A, digits = 3)
export_summs(Agg.A, model.names = "Agg.AgeModel",to.file = "docx", file.name = "Modeling.Agg.Strength.A.docx")

#Rank
Agg.R <- glmmTMB(strength~ isPost*percentrank + group + sex + age +hrs + (1|id) + (1|year), data = NetworkMetrics.all,
                 family =gaussian)
# performance::check_model(Agg.BM)
summary(Agg.R, digits = 3)
export_summs(Agg.R, model.names = "Agg.RankModel",to.file = "docx", file.name = "Modeling.Agg.Strength.R.docx")