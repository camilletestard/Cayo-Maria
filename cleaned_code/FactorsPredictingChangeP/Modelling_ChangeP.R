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
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP_min20.RData")
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/SocialCapital.RData")
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/strength.to.deceased.RData")

#####################################################################
# 1. Merge dprob, social Capital and strength to dead IDs data &clean
#####################################################################
data.combined=merge.data.frame(dprob.ALL,strength.to.deceased, by=intersect(c("id","year"),c("id","year")))
full.data=merge.data.frame(data.combined,SocialCapital.ALL,by=intersect(c("id","year"),c("id","year")))
full.data$group.x=NULL; full.data$group.y=NULL
full.data$CSI = (full.data$std.DSIgroom + full.data$std.DSIprox)/2
full.data$pSocial.pre.adj = as.numeric(full.data$pSocial.pre) - mean(as.numeric(full.data$pSocial.pre))
df1 = full.data[,c("dpSocial","pSocial.pre.adj","pSocial.pre","pSocial.post","percentrank","std.dead.all","std.DSIgroom","dpAcc")]
full.data[,c("dpSocial","pSocial.pre.adj","pSocial.pre","pSocial.post","percentrank","std.dead.all","std.DSIgroom","dpAcc")] =lapply(df1,as.numeric)


###########################################################
# 2. Modelling change p(groom), p(prox)
###########################################################

#Scale parameters: 
full.data[,c("age","DSIgroom","numPartnersGroom","dead.all")] <- scale(full.data[,c("age","DSIgroom","numPartnersGroom","dead.all")])

#Exclude individuals with extreme values
full.data=full.data[full.data$DSIgroom<=0.5 & full.data$DSIgroom>=-0.5,]


n_iter = max(full.data$iter); iter=1
dpAcc.Effects.V = data.frame(); dpSocial.Effects.V = data.frame();
dpAcc.Effects.KK = data.frame(); dpSocial.Effects.KK = data.frame();

iter=sample(n_iter,1)
for (iter in 1:n_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  
  data.V= full.data[which(full.data$iter==iter&full.data$group=="V"),]
  data.KK= full.data[which(full.data$iter==iter&full.data$group=="KK"),]
  
  ###########################################################
  ## Model Social Capital effect on change in proximity rates 
  ###########################################################
  
  # dpAcc.V <- lmer(dpAcc~ age + percentrank + std.dead.all + sex:std.DSIprox  +  (1|id) +(1|year), data = data.V, na.action=na.omit)
  # summary(dpAcc.V)
  # # performance::check_model(dpAcc.V) #check model assumptions
  # dpAcc.Effects.V[iter,c("(Intercept)","age","rank","dead.all","sexM","DSIprox","sexM:DSIprox")] <- getME(dpAcc.V, "beta")
  # dpAcc.Effects.V[iter,c("(focalID)","(year)")] <- getME(dpAcc.V, "theta")
  # 
  # dpAcc.KK <- lmer(dpAcc~ age + percentrank + std.dead.all + sex:std.DSIprox  +  (1|id) +(1|year), data = data.KK, na.action=na.omit)
  # summary(dpAcc.KK)
  # # performance::check_model(dpAcc.KK) #check model assumptions
  # dpAcc.Effects.KK[iter,c("(Intercept)","age","rank","dead.all","sexM","DSIprox","sexM:DSIprox")] <- getME(dpAcc.KK, "beta")
  # dpAcc.Effects.KK[iter,c("(focalID)","(year)")] <- getME(dpAcc.KK, "theta")
  
  ###########################################################
  ## Model Social Capital effect on change in grooming rates
  ###########################################################

  dpSocial.V <- lmer(dpSocial~ sex + age + percentrank + std.dead.all + std.DSIgroom + dpAcc + (1|id)+  (1|year), data = data.V, na.action=na.omit)
  summary(dpSocial.V)
  # performance::check_model(dpSocial.V)
  # plot(effects::allEffects(dpSocial.V))
  dpSocial.Effects.V[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIgroom","dpAcc")] <- getME(dpSocial.V, "beta")
  dpSocial.Effects.V[iter,c("(focalID)","(year)")] <- getME(dpSocial.V, "theta")
  
  dpSocial.KK <- lmer(dpSocial~ sex + age + percentrank + std.dead.all + std.DSIgroom + dpAcc + (1|id)+  (1|year), data = data.KK, na.action=na.omit)
  summary(dpSocial.KK)
  # performance::check_model(dpSocial.KK)
  # plot(effects::allEffects(dpSocial.KK))
  dpSocial.Effects.KK[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIgroom","dpAcc")] <- getME(dpSocial.KK, "beta")
  dpSocial.Effects.KK[iter,c("(focalID)","(year)")] <- getME(dpSocial.KK, "theta")
  
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

#####################################################################
# 5. Visualizations
#####################################################################

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/PreHurricaneFactors-BehavioralFlex/Plots")

full.data.plot=full.data

#check distribution of independent variables
dpProx<-ggplot(full.data.plot, aes(x=dpAcc))+
  geom_histogram(fill="darkgoldenrod", color="#e9ecef", alpha=0.8)+
  geom_vline(xintercept=0, linetype="longdash", lwd=1.5)+
  # facet_grid(~group)+
  #   ggtitle("Post-disaster change in grooming as a function of pre-disaster strength of bond to deceased IDs")+
  xlab("Post-hurr. change in proximity rates")+ theme_classic(base_size=20)
ggsave("ChangeProbProx.tiff", dpProx)

dpGroom<-ggplot(full.data.plot, aes(x=dpSocial))+
  geom_histogram(fill="darksalmon", color="#e9ecef", alpha=0.9)+
  geom_vline(xintercept=0, linetype="longdash", lwd=1.5)+
  # facet_grid(~group)+
  #   ggtitle("Post-disaster change in grooming as a function of pre-disaster strength of bond to deceased IDs")+
  xlab("Post-hurr. change in grooming rates")+ theme_classic(base_size=20)
ggsave("ChangeProbGroom.tiff", dpGroom)
mean(full.data.plot$dpSocial); sd(full.data.plot$dpSocial)


# #Visualize how strength of bond to dead individuals (from the hurricane) affects change in grooming/proximity
# dead<-ggplot(full.data.plot, aes(x=dead.all, y=dpSocial))+
#   geom_point(alpha = 0.25, size = 3) +
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster strength of bond to deceased IDs")+
#   xlab("Strength of bond to deceased IDs pre-disaster (standardized)")+
#   ylab("Change in Grooming")
# cor.test(full.data.plot$dpSocial,full.data.plot$dead.all) #test correlation
# tiff("dead.tiff",units="in", width=8.5, height=4, res=300, compression = 'lzw'); dead; dev.off() #save plot
# 
# ggplot(full.data.plot, aes(x=dead.all, y=dpAcc))+
#   geom_point(alpha = 0.25, size = 3) +
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in proximity as a function of pre-disaster strength of bond to deceased IDs")+
#   xlab("Strength of bond to deceased IDs pre-disaster (standardized)")+
#   ylab("Change in proximity")
# cor.test(full.data.plot$dpAcc,full.data.plot$dead.all) #test correlation

#If want to plot only one iteration
full.data.plot=full.data[full.data$iter==sample(500,1),]
# full.data.plot=full.data.plot[full.data.plot$DSIgroom<=1.5 & full.data.plot$DSIgroom>=-0.5,]

#Distribution of change in grooming
dpGroom<-ggplot(full.data.plot, aes(x=dpSocial))+
  geom_histogram(fill="darksalmon", color="#e9ecef")+
  geom_vline(xintercept=0, linetype="longdash", lwd=1.5)+
  # facet_grid(~group)+
  #   ggtitle("Post-disaster change in grooming as a function of pre-disaster strength of bond to deceased IDs")+
  xlab("Post-to-post hurricane change in p(grooming)")+ theme_classic(base_size=20)
  mean(full.data.plot$dpSocial); sd(full.data.plot$dpSocial)
ggsave("ChangeProbGroom.tiff", dpGroom)
ggsave("ChangeProbGroom.eps", dpGroom)

#Visualize how baseline grooming is related to post-disaster change in p(groom)
preGroom <- ggplot(full.data.plot, aes(x=DSIgroom, y=dpSocial))+
  geom_point(aes(colour = "darksalmon"), size = 3) +
  geom_smooth(method=lm, color=rgb(1,0,0))+
  geom_hline(yintercept=0,linetype="longdash",lwd=0.5)+
  # ggtitle("Post-disaster change in grooming rate\nas a function of pre-disaster sociality")+
  xlab("Pre-disaster social integration (standardized)")+
  ylab("Change in p(grooming)")+ #ylim(-0.1, 0.1)+ xlim(1,3)+
  facet_grid(~group)+ theme_classic(base_size=20)
  # scale_color_manual(values=c("plum1","seagreen2"))
cor.test(full.data.plot$dpSocial[full.data.plot$group=="KK"],full.data.plot$std.DSIgroom[full.data.plot$group=="KK"]) #test correlation
cor.test(full.data.plot$dpSocial[full.data.plot$group=="V"],full.data.plot$std.DSIgroom[full.data.plot$group=="V"]) #test correlation
ggsave("preGroom.tiff",preGroom)
ggsave("preGroom.eps",preGroom)

# preProx <- ggplot(full.data.plot, aes(x=std.DSIprox, y=dpAcc))+
#   geom_point(aes(colour = sex),alpha = 0.5, size = 3) +
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   # ggtitle("Post-disaster change in proximity rate\nas a function of pre-disaster sociality")+
#   xlab("Pre-disaster Proximity (standardized)")+
#   ylab("Change in p(proximity)")+ #ylim(-0.3, 0.3)+
#   facet_grid(sex~group)+ theme_classic(base_size=20)+
#   scale_color_manual(values=c("plum1","seagreen2"))
# cor.test(full.data.plot$dpAcc,full.data.plot$std.DSIprox) #test correlation
# ggsave("preProx.tiff",preProx)

preDeath <- ggplot(full.data.plot, aes(x=std.dead.all, y=dpSocial))+
  geom_point(aes(colour = "darksalmon"),size = 3) +
  geom_smooth(method=lm, color=rgb(1,0,0))+
  geom_hline(yintercept=0,linetype="longdash",lwd=0.5)+
  # ggtitle("Post-disaster change in grooming rate\nas a function of pre-disaster sociality")+
  xlab("Loss of partner (standardized)")+
  ylab("Change in p(grooming)")+ ylim(-0.3, 0.3)+ xlim(0,4)+
  facet_grid(~group)+ theme_classic(base_size=20)
  # scale_color_manual(values=c("plum1","seagreen2"))
cor.test(full.data.plot$dpSocial[full.data.plot$group=="KK"],full.data.plot$std.dead.all[full.data.plot$group=="KK"]) #test correlation
cor.test(full.data.plot$dpSocial[full.data.plot$group=="V"],full.data.plot$std.dead.all[full.data.plot$group=="V"])
ggsave("preDeath.tiff",preDeath)
ggsave("preDeath.eps",preDeath)

dAcc.dSoc <- ggplot(full.data.plot, aes(x=dpAcc, y=dpSocial))+
  geom_point(aes(colour = "darksalmon"), size = 3) +
  geom_smooth(method=lm, color=rgb(1,0,0))+
  geom_hline(yintercept=0,linetype="longdash",lwd=0.5)+
  # ggtitle("Post-disaster change in grooming rate\nas a function of pre-disaster sociality")+
  xlab("Change in p(proximity)")+
  ylab("Change in p(grooming)")+
  facet_grid(~group)+ theme_classic(base_size=20)
  # scale_color_manual(values=c("plum1","seagreen2"))
cor.test(full.data.plot$dpSocial[full.data.plot$group=="KK"],full.data.plot$dpAcc[full.data.plot$group=="KK"]) #test correlation
cor.test(full.data.plot$dpSocial[full.data.plot$group=="V"],full.data.plot$dpAcc[full.data.plot$group=="V"])
ggsave("dpAcc.dpSoc.tiff",dAcc.dSoc)
ggsave("dpAcc.dpSoc.eps",dAcc.dSoc)

# #Visualize how age is related to post-disaster change in p(groom)
# ggplot(full.data.plot, aes(x=age, y=dpSocial))+
#   geom_point(alpha = 0.25) +
#   geom_smooth(method=lm, color='#2C3E50')+
#   ggtitle("Post-disaster change in grooming as a function of age")
# cor.test(full.data.plot$dpSocial,full.data.plot$age) #test correlation
# 
# #Visualize how sex is related to post-disaster change in p(groom)
# ggplot(full.data.plot, aes(x=sex, y=dpSocial,fill=sex))+
#   geom_boxplot(aes(group=sex),alpha = 0.75, col="grey")+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.2)
# 
# # Visualize how group is related to post-disaster change in p(groom)
# ggplot(full.data.plot, aes(x=group, y=dpAcc,fill=group))+
#   geom_boxplot(aes(group=group),alpha = 0.75, col="grey")+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.2)
# table(full.data.plot$id,full.data.plot$group)

# #Visualize how number of partner is related to post-disaster change in p(groom)
# numP.groom <- ggplot(full.data.plot, aes(x=numPartnersGroom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) +
#   geom_point(alpha = 0.25, size = 3) +
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming level")+
#   xlab("Pre-disaster number of grooming partners (standardized)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(full.data.plot$dpSocial,full.data.plot$numPartnersGroom) #test correlation
# # tiff("numP.groom.tiff",units="in", width=7, height=4, res=300, compression = 'lzw'); numP.groom; dev.off()
# 
# #Visualize how eigenvector centrality is related to post-disaster change in p(groom)
# eigCent.groom <- ggplot(full.data.plot, aes(x=eig.cent.groom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) +
#   # geom_point(aes(colour = sex),alpha = 0.25, size = 3) +
#   geom_point(alpha = 0.25, size = 3) +
#   geom_smooth(method=lm, color=rgb(1,0,0))+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming network position")+
#   xlab("Pre-disaster index of how well-connected your friends are (eigenvector centrality)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(full.data.plot$dpAcc,full.data.plot$eig.cent.groom)
# #tiff("eigCent.prox.tiff",units="in", width=9, height=4, res=300, compression = 'lzw'); eigCent.prox; dev.off()
# 
# #Visualize how clustering coeff is related to post-disaster change in p(groom)
# ClusterCoeff.groom <- ggplot(full.data.plot, aes(x=clusterCoeff.groom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) +
#   geom_point(aes(colour = sex),alpha = 0.25, size = 3) +
#   geom_smooth(method=lm, color='#2C3E50')+
#   ggtitle("Post-disaster change in proximity as a function of pre-disaster social network position")+
#   xlab("Pre-disaster index of position in network (Clustering Coefficient)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(full.data.plot$dpSocial,full.data.plot$clusterCoeff.groom)
# # tiff("eigCent.groom.tiff",units="in", width=9, height=4, res=300, compression = 'lzw'); eigCent.prox; dev.off()
# 
# ggplot(full.data.plot, aes(x=between.groom, y=dpSocial))+
#   # geom_point(aes(colour = sex, size = age),alpha = 0.25) +
#   geom_point(aes(colour = sex),alpha = 0.25, size = 3) +
#   geom_smooth(method=lm, color='#2C3E50')+
#   ggtitle("Post-disaster change in grooming as a function of pre-disaster social network position")+
#   xlab("Pre-disaster index of position in network (betweenness)")+
#   ylab("Change in Grooming post-disaster")+
#   facet_grid(~sex)
# cor.test(full.data.plot$dpSocial,full.data.plot$between.groom)