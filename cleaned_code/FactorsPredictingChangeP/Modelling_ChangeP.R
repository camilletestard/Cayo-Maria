# Modelling_ChangeP: The goal is to model change p(groom) and p(prox) post-hurricane. what factor pre-hurricane
# predicts the change in social behavior post-hurricane? 
#   Modelling happens over multiple iterations because we compute dpgroom and dpprox over sub-sampled scans. At each
# iteration, I compute the change in pProx and pGroom (only considering IDs with at least 20obs), the script then
# combines change in social rates info with pre-hurricane social capital factors in one big data frame for modelling.
# Functions called: CalcSubsampledScans
# Input: "SocialCapital.RData" (output from generate_SocialCapitalMetrics), allScans.txt (to compute change in social rates)
# Output Models (for each group separately): "ChangePModelEffects.RData"
# - Groom Model: sex + age + group + rank + dead.all + std.DSIGroom + dpProx + (1|ID) + (1|year)
# - Prox Model: sex + age + group + rank + dead.all + std.DSIProx + (1|ID) + (1|year)
# NOTES:
#   -dead.all = strength of bond to partners who died in the year following the hurricane
# -dProx = change in proximity
# -std.X = standardized versions (divided by mean)
# In sections 3. I compile model outputs from all iterations by computing mean estimates and 95% CI. 
# in section 4. I visualize individual effects

library(lme4)# Generalized Linear Mixed Models
library(lmerTest)
library(performance)
library(sjPlot)
library(jtools)
library(ggplot2)
library(dplyr)
library(lmerTest)
library(matrixStats)
library(gridExtra)
library(graphics)

# Load required data
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP.RData")
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/SocialCapital.RData")
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/strength.to.deceased.RData")

#####################################################################
# 1. Merge dprob, social Capital and strength to dead IDs data &clean
#####################################################################
data.combined=merge.data.frame(dprob.ALL,strength.to.deceased, by=intersect(c("id","year"),c("id","year")))
full.data=merge.data.frame(data.combined,SocialCapital.ALL,by=intersect(c("id","year"),c("id","year")))
full.data$group.x=NULL; full.data$group.y=NULL

###########################################################
# 2. Modelling change p(groom), p(prox)
###########################################################

#Scale parameters: 
full.data[,c("age","DSIgroom","numPartnersGroom","dead.all")] <- scale(full.data[,c("age","DSIgroom","numPartnersGroom","dead.all")])

n_iter = max(full.data$iter); iter=1
dpAcc.Effects.V = data.frame(); dpSocial.Effects.V = data.frame();
dpAcc.Effects.KK = data.frame(); dpSocial.Effects.KK = data.frame();


for (iter in 1:n_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  
  data.V= full.data[which(full.data$iter==iter&full.data$group=="V"),]
  data.KK= full.data[which(full.data$iter==iter&full.data$group=="KK"),]
  
  ###########################################################
  ## Model Social Capital effect on change in proximity rates 
  ###########################################################
  
  dpAcc.V <- lmer(dpAcc~ sex + age + percentrank + std.dead.all + std.DSIprox  +  (1|id) +(1|year), data = data.V, na.action=na.omit)
  summary(dpAcc.V)
  # performance::check_model(dpAcc.V) #check model assumptions
  dpAcc.Effects.V[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIprox")] <- getME(dpAcc.V, "beta")
  dpAcc.Effects.V[iter,c("(focalID)","(year)")] <- getME(dpAcc.V, "theta")
  
  dpAcc.KK <- lmer(dpAcc~ sex + age + percentrank + std.dead.all + std.DSIprox  +  (1|id) +(1|year), data = data.KK, na.action=na.omit)
  summary(dpAcc.KK)
  # performance::check_model(dpAcc.KK) #check model assumptions
  dpAcc.Effects.KK[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIprox")] <- getME(dpAcc.KK, "beta")
  dpAcc.Effects.KK[iter,c("(focalID)","(year)")] <- getME(dpAcc.KK, "theta")
  
  ###########################################################
  ## Model Social Capital effect on change in grooming rates
  ###########################################################

  dpSocial.V <- lmer(dpSocial~ sex + age + percentrank + dead.all + std.DSIgroom + dpAcc + (1|id)+  (1|year), data = data.V, na.action=na.omit)
  summary(dpSocial.V)
  # performance::check_model(dpSocial.V)
  # plot(effects::allEffects(dpSocial.V))
  dpSocial.Effects.V[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIgroom","dpAcc")] <- getME(dpSocial.V, "beta")
  dpSocial.Effects.V[iter,c("(focalID)","(year)")] <- getME(dpSocial.V, "theta")
  
  dpSocial.KK <- lmer(dpSocial~ sex + age + percentrank + dead.all + std.DSIgroom + dpAcc + (1|id)+  (1|year), data = data.KK, na.action=na.omit)
  summary(dpSocial.KK)
  # performance::check_model(dpSocial.KK)
  # plot(effects::allEffects(dpSocial.KK))
  dpSocial.Effects.KK[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIgroom","dpAcc")] <- getME(dpSocial.KK, "beta")
  dpSocial.Effects.KK[iter,c("(focalID)","(year)")] <- getME(dpSocial.KK, "theta")
  
  
  
  save(dpSocial.Effects.KK,dpAcc.Effects.KK,dpSocial.Effects.V,dpAcc.Effects.V,
       file="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ChangeP.ModelEffects.RData")
}

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/PreHurricaneFactors-BehavioralFlex")

#####################################################################
# 4. Compile model effects
#####################################################################

#Proximity
Means = colMeans2(as.matrix(dpAcc.Effects.V)); Means = round(Means,3)
CI = colQuantiles(as.matrix(dpAcc.Effects.V), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc<-tableGrob(Estimates); t.Acc<-grid.arrange(t.Acc, top="Base Model: Change p(Prox); group V"); #create table, arrange table
write.csv(Estimates,"PredictProx.V.csv")

Means = colMeans2(as.matrix(dpAcc.Effects.KK)); Means = round(Means,3)
CI = colQuantiles(as.matrix(dpAcc.Effects.KK), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc<-tableGrob(Estimates); t.Acc<-grid.arrange(t.Acc, top="Base Model: Change p(Prox); group KK"); #create table, arrange table
write.csv(Estimates,"PredictProx.KK.csv")

#Grooming
Means = colMeans2(as.matrix(dpSocial.Effects.V)); Means = round(Means,3)
CI = colQuantiles(as.matrix(dpSocial.Effects.V), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc<-tableGrob(Estimates); t.Soc<-grid.arrange(t.Soc, top="Base Model: Change p(Groom); group V"); #create table, arrange table
write.csv(Estimates,"PredictGroom.V.csv")

Means = colMeans2(as.matrix(dpSocial.Effects.KK)); Means = round(Means,3)
CI = colQuantiles(as.matrix(dpSocial.Effects.KK), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc<-tableGrob(Estimates); t.Soc<-grid.arrange(t.Soc, top="Base Model: Change p(Groom); group KK"); #create table, arrange table
write.csv(Estimates,"PredictGroom.KK.csv")
#end of script

# #####################################################################
# # 4. Visualizations
# #####################################################################
# 
# setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/PreHurricaneFactors-BehavioralFlex/Plots")
# 
# #check distribution of independent variables
# tiff("ChangeProbProx.tiff",units="in", width=7, height=4, res=300, compression = 'lzw')
# hist(full.data$dpAcc,col=rgb(1,0,0,0.5), breaks=20,main="Change p(proximity) pre- to post-hurricane",xlab="Change p(Proximity)", xlim=c(-0.5,1))
# segments(x0=0,y0=0,x1=0,y1=20000,col="red",lwd=4, lty=2)
# box()
# dev.off()
# 
# # postscript("ChangeProbProx.eps")
# # hist(full.data$dpAcc,col=rgb(1,0,0,0.5), breaks=20,main="Change p(proximity) pre- to post-hurricane",xlab="Change p(Proximity)", xlim=c(-0.5,1))
# # segments(x0=0,y0=0,x1=0,y1=20000,col="red",lwd=4, lty=2)
# # box()
# # dev.off()
# 
# tiff("ChangeProbGroom.tiff",units="in", width=7, height=4, res=300, compression = 'lzw')
# hist(full.data$dpSocial,col=rgb(0,1,1,0.5), breaks=20,main="Change p(grooming) pre- to post-hurricane",xlab="Change p(Grooming)", xlim=c(-0.4,0.4))
# segments(x0=0,y0=0,x1=0,y1=25000,col="red",lwd=4, lty=2)
# box()
# dev.off()
# 
# # postscript("ChangeProbGroom.eps")
# # hist(full.data$dpSocial,col=rgb(0,1,1,0.5), breaks=20,main="Change p(grooming) pre- to post-hurricane",xlab="Change p(Grooming)", xlim=c(-0.4,0.4))
# # segments(x0=0,y0=0,x1=0,y1=25000,col="red",lwd=4, lty=2)
# # box()
# # dev.off()
# 
# #Visualize how strength of bond to dead individuals (from the hurricane) affects change in grooming/proximity 
# dead<-ggplot(data, aes(x=dead.all, y=dpSocial))+
#   geom_point(alpha = 0.25, size = 3) + 
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster strength of bond to deceased IDs")+
#   xlab("Strength of bond to deceased IDs pre-disaster (standardized)")+
#   ylab("Change in Grooming")
# cor.test(data$dpSocial,data$dead.all) #test correlation
# tiff("dead.tiff",units="in", width=8.5, height=4, res=300, compression = 'lzw'); dead; dev.off() #save plot
# 
# ggplot(data, aes(x=dead.all, y=dpAcc))+
#   geom_point(alpha = 0.25, size = 3) + 
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in proximity as a function of pre-disaster strength of bond to deceased IDs")+
#   xlab("Strength of bond to deceased IDs pre-disaster (standardized)")+
#   ylab("Change in proximity")
# cor.test(data$dpAcc,data$dead.all) #test correlation
# 
# 
# #Visualize how baseline p(groom) is related to post-disaster change in p(groom)
# preGroom <- ggplot(data, aes(x=std.DSIgroom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
#   geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
#   geom_point(alpha = 0.25, size = 3) + 
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming level")+
#   xlab("Pre-disaster Grooming (standardized)")+
#   ylab("Change in Grooming")+
#   facet_grid(~sex)
# cor.test(data$dpSocial,data$DSIgroom) #test correlation
# tiff("preGroom.tiff",units="in", width=7, height=4, res=300, compression = 'lzw'); preGroom; dev.off() #save plot
# 
# #Visualize how age is related to post-disaster change in p(groom)
# ggplot(data, aes(x=age, y=dpSocial))+
#   geom_point(alpha = 0.25) + 
#   geom_smooth(method=lm, color='#2C3E50')+
#   ggtitle("Post-disaster change in grooming as a function of age")
# cor.test(data$dpSocial,data$age) #test correlation
# 
# #Visualize how sex is related to post-disaster change in p(groom)
# ggplot(data, aes(x=sex, y=dpSocial,fill=sex))+
#   geom_boxplot(aes(group=sex),alpha = 0.75, col="grey")+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.2)
# 
#Visualize how group is related to post-disaster change in p(groom)
# ggplot(data, aes(x=group, y=dpAcc,fill=group))+
#   geom_boxplot(aes(group=group),alpha = 0.75, col="grey")+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.2)
# table(data$id,data$group)

# #Visualize how number of partner is related to post-disaster change in p(groom)
# numP.groom <- ggplot(data, aes(x=numPartnersGroom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
#   geom_point(alpha = 0.25, size = 3) + 
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming level")+
#   xlab("Pre-disaster number of grooming partners (standardized)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(data$dpSocial,data$numPartnersGroom) #test correlation
# # tiff("numP.groom.tiff",units="in", width=7, height=4, res=300, compression = 'lzw'); numP.groom; dev.off()
# 
# #Visualize how eigenvector centrality is related to post-disaster change in p(groom)
# eigCent.groom <- ggplot(data, aes(x=eig.cent.groom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
#   # geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
#   geom_point(alpha = 0.25, size = 3) +
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming network position")+
#   xlab("Pre-disaster index of how well-connected your friends are (eigenvector centrality)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(data$dpAcc,data$eig.cent.groom)
# #tiff("eigCent.prox.tiff",units="in", width=9, height=4, res=300, compression = 'lzw'); eigCent.prox; dev.off()
# 
# #Visualize how clustering coeff is related to post-disaster change in p(groom)
# ClusterCoeff.groom <- ggplot(data, aes(x=clusterCoeff.groom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
#   geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
#   geom_smooth(method=lm, color='#2C3E50')+
#   ggtitle("Post-disaster change in proximity as a function of pre-disaster social network position")+
#   xlab("Pre-disaster index of position in network (Clustering Coefficient)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(data$dpSocial,data$clusterCoeff.groom)
# # tiff("eigCent.groom.tiff",units="in", width=9, height=4, res=300, compression = 'lzw'); eigCent.prox; dev.off()
# 
# ggplot(data, aes(x=between.groom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
#   geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
#   geom_smooth(method=lm, color='#2C3E50')+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster social network position")+
#   xlab("Pre-disaster index of position in network (betweenness)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(data$dpSocial,data$between.groom)