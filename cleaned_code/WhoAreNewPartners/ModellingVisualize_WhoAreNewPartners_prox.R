# ModellingVisualize_WhoAreNewPartners: 
# This script visualizes and runs statistics on the output of "generate_WhoAreTheNewPartners".
# = proportion of proxing between pair categories for each group/year/hurricane status separately. Pair categories based on: 
#   - Social status: Low->Low; Low->High; High->Low; High->High. Note: Low rank <80%; High rank >80%
#   - Sex: M->M; F->M; M->F; F->F
#   - Pre-hurr proxing strength: greg->greg; greg->shy; shy->greg; shy->shy. Note: threshold for shy/greg is 80% (or prctile)
#   - Kinship: related (rel>0.125) and unrelated (unrel <0.125)
# This script will allows us to assess the difference in relationship distribution pre-to-post hurricane. 
# E.g. are there more F->M relationships occuring post-disaster? 
# Input: PartnerAttributes.RData
# (1) Compute difference in proportions between pre- and post- for each subsampling iteration. We will get distributions of differences.
# (2) Compute mean difference and 95% confidence interval for each dyadic category.
# (3) Compute one-sided p-value (=proportion of difference distribution above or below zero. It depends on which side we're testing).
# Output: .csv files with estimates, 95%CI and one-sided p-vals.
# Visulizations: Violin plots of difference in proportions pre/post in each dyadic category, separated by group.


library(ggplot2)
library(lme4)# Generalized Linear Mixed Models
library(glmmTMB)
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)
library(gridExtra)
library(matrixStats)

#load & format data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/PartnerAttributes_prox.RData")
PartnerAttr$groupyear = paste(PartnerAttr$group, PartnerAttr$year, sep="")
PartnerAttr$isPost = as.factor(PartnerAttr$isPost)
for (col in seq(6,33)) {PartnerAttr[,col] = as.numeric(PartnerAttr[,col])}

#Separate data by proxing and proximity
data.prox = PartnerAttr[which(PartnerAttr$action=="prox"),]
data.prox.V=data.prox[data.prox$group=="V",];data.prox.KK=data.prox[data.prox$group=="KK",]
#Select columns of interest
data.V.pre = data.prox.V[data.prox.V$isPost==0,8:33]; data.V.post = data.prox.V[data.prox.V$isPost==1,8:33]; 
data.KK.pre = data.prox.KK[data.prox.KK$isPost==0,8:33]; data.KK.post = data.prox.KK[data.prox.KK$isPost==1,8:33]; 

##############################################
# TEST DIFFERENCE IN PROPORTIONS:

#GROUP V
data.V.diff = data.V.post-data.V.pre#Compute difference in proportions between pre- and post- for each subsampling iteration
Means = colMeans2(as.matrix(data.V.diff )); Means = round(Means,3) #Comput mean estimates
CI = colQuantiles(as.matrix(data.V.diff ), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute 95% CI (2.5 and 97.5 percentiles)
#Note: 95%CI is the 2-sided hypothesis testing approach and will be used for statistical significance
#To define a pre-/post- difference as trending, I compute the one-sided p-value
# = the proportion of difference distribution above or below zero.
binary_diff = data.V.diff<0; binary_diff=ifelse(binary_diff==T,1,0)
pval_neg=as.data.frame(colSums(binary_diff)/nrow(data.V.diff)) #proportion of distribution below zero
binary_diff = data.V.diff>0; binary_diff=ifelse(binary_diff==T,1,0)
pval_pos=as.data.frame(colSums(binary_diff)/nrow(data.V.diff))#proportion of distribution above zero
pval=apply(cbind(pval_neg,pval_pos), 1, FUN=min)#Use the smallest proportion as p-value (allows us to see which side is tested)
Estimates.V = cbind(Means, CI, pval); Estimates.V = as.data.frame(Estimates.V); names(Estimates.V) = c("Estimate","2.5%","97.5%","one-sided pval")

#GROUP KK
data.KK.diff = data.KK.post-data.KK.pre
Means = colMeans2(as.matrix(data.KK.diff )); Means = round(Means,3)
CI = colQuantiles(as.matrix(data.KK.diff ), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
binary_diff = data.KK.diff<0; binary_diff=ifelse(binary_diff==T,1,0)
pval_neg=as.data.frame(colSums(binary_diff)/nrow(data.V.diff))
binary_diff = data.KK.diff>0; binary_diff=ifelse(binary_diff==T,1,0)
pval_pos=as.data.frame(colSums(binary_diff)/nrow(data.V.diff))
pval=apply(cbind(pval_neg,pval_pos), 1, FUN=min)
Estimates.KK = cbind(Means,CI, pval); Estimates.KK = as.data.frame(Estimates.KK); names(Estimates.KK) = c("Estimate","2.5%","97.5%","one-sided pval")

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/WhoAreNewPartners/") 

write.csv(Estimates.V,"proxPartnerPref.Stats.V .csv")
write.csv(Estimates.KK,"proxPartnerPref.Stats.KK.csv")

##############################################
#PLOTS/visualizations

data.KK.diff$group="KK"; data.V.diff$group="V"
data.diff.full =rbind(data.V.diff, data.KK.diff)

##############################################
#SOCIAL RANK : plot %change HH/LH/HL/LL interactions pre-to-post hurr.
##############################################

## prox ##

# #prox Low to High
# tiff("L2H.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
l2h<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(LowToHigh), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Low|High")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
  # facet_grid(~group)
# dev.off()

#prox High to Low
h2l<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(HighToLow), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("HighR|LowR ")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
  # facet_grid(~group)


#prox Low to Low
# tiff("L2L.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
l2l<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(LowToLow), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("LowR|LowR ")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
  # facet_grid(~group)
# dev.off()


#prox High to high
# tiff("H2H.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
h2h<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(HighToHigh), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("HighR|HighR ")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
  # facet_grid(~group)
# dev.off()


FullPlot.prox = grid.arrange(h2h,h2l,l2h,l2l, ncol=4, nrow=1)
ggsave(FullPlot.prox, file ="changeRankPref_prox.png")
ggsave(FullPlot.prox, file ="changeRankPref_prox.eps")


##################################################
# KINSHIP : plot %change ck/dk/unrel interactions pre-to-post hurr.
##################################################

## prox #

#prox Kin
# kin<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(Kin), fill=as.factor(group) ))+
#   geom_violin()+
#   geom_hline(yintercept=0, color = "red", linetype = "dashed")+
#   ggtitle("Kin")+
#   labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proximity pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
# # dev.off()
# ggsave(kin, file ="changeKinPref_prox.png")
# ggsave(kin, file ="changeKinPref_prox.eps")


##################################################
# SOCIAL HOMOPHILY : plot %change shy.shy/greg.greg/shy.greg/greg/shy interactions pre-to-post hurr.
##################################################

## prox #

#prox shy.shy
# tiff("shy2shy.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
shy2shy<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialHomophily.shy), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Shy|Shy")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
# dev.off()

#prox greg.greg
# tiff("greg2greg.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
greg2greg<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialHomophily.greg), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Greg|Greg")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
# dev.off()

# #prox shy.greg
shy2greg<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialOpposite.shygreg), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Shy|Greg")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)


# #prox greg.shy
# tiff("greg2shy.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
greg2shy<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialOpposite.gregshy), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Greg|Shy")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
# dev.off()

FullPlot.prox = grid.arrange(shy2shy,greg2greg,shy2greg,greg2shy, ncol=4, nrow=1)
ggsave(FullPlot.prox, file ="changeSocialPref_prox.png")
ggsave(FullPlot.prox, file ="changeSocialPref_prox.eps")

########################################
# SEX: plot %change MM/MF/FM/FF interactions pre-to-post hurr.
########################################

## prox #

# # prox Male --> Male
M2M<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(MM), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Male|Male")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)


# # prox Male --> Female
M2F<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(MF), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Male|Female")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)


# prox Female --> Male
# tiff("F2M.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
F2M<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(FM), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Female|Male")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
# dev.off()

# prox Female --> Female
F2F<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(FF), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Female|Female")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of proxing pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)

FullPlot.prox = grid.arrange(M2M,M2F,F2M,F2F, ncol=4, nrow=1)
ggsave(FullPlot.prox, file ="changeSexPref_prox.png")
ggsave(FullPlot.prox, file ="changeSexPref_prox.eps")

