# ModellingVisualize_WhoAreNewPartners: 
# This script visualizes and runs statistics on the output of "generate_WhoAreTheNewPartners".
# = proportion of grooming between pair categories for each group/year/hurricane status separately. Pair categories based on: 
#   - Social status: Low->Low; Low->High; High->Low; High->High. Note: Low rank <80%; High rank >80%
#   - Sex: M->M; F->M; M->F; F->F
#   - Pre-hurr grooming strength: greg->greg; greg->Solitary; Solitary->greg; Solitary->Solitary. Note: threshold for Solitary/greg is 80% (or prctile)
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
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/PartnerAttributes_minObs.RData")
PartnerAttr$groupyear = paste(PartnerAttr$group, PartnerAttr$year, sep="")
PartnerAttr$isPost = as.factor(PartnerAttr$isPost)
for (col in seq(6,33)) {PartnerAttr[,col] = as.numeric(PartnerAttr[,col])}

#Separate data by grooming and groomimity
data.groom = PartnerAttr[which(PartnerAttr$action=="groom"),]
data.groom.V=data.groom[data.groom$group=="V",];data.groom.KK=data.groom[data.groom$group=="KK",]
#Select columns of interest
data.V.pre = data.groom.V[data.groom.V$isPost==0,8:33]; data.V.post = data.groom.V[data.groom.V$isPost==1,8:33]; 
data.KK.pre = data.groom.KK[data.groom.KK$isPost==0,8:33]; data.KK.post = data.groom.KK[data.groom.KK$isPost==1,8:33]; 

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

write.csv(Estimates.V,"groomPartnerPref.Stats.V_minObs.csv")
write.csv(Estimates.KK,"groomPartnerPref.Stats.KK_minObs.csv")

##############################################
#PLOTS/visualizations

data.KK.diff$group="KK"; data.V.diff$group="V"
data.diff.full =rbind(data.V.diff, data.KK.diff)

##############################################
#SOCIAL RANK : plot %change HH/LH/HL/LL interactions pre-to-post hurr.
##############################################

## groom ##

# #groom Low to High
# tiff("L2H.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
l2h<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(LowToHigh), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Low-->High")+
  labs(fill = "Group",x="Group",y=NULL)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
  # facet_grid(~group)
# dev.off()

#groom High to Low
h2l<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(HighToLow), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("HighR-->LowR ")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
# facet_grid(~group)


#groom Low to Low
# tiff("L2L.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
l2l<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(LowToLow), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("LowR-->LowR ")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
  # facet_grid(~group)
# dev.off()


#groom High to high
# tiff("H2H.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
h2h<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(HighToHigh), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("HighR-->HighR ")+
  labs(fill = "Group",x="Group",y="Change in prop. of grooming pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
  # facet_grid(~group)
# dev.off()


FullPlot.groom = grid.arrange(h2h,h2l,l2h,l2l, ncol=4, nrow=1)
ggsave(FullPlot.groom, file ="changeRankPref_groom_minObs.png")
ggsave(FullPlot.groom, file ="changeRankPref_groom_minObs.eps")


##################################################
# KINSHIP : plot %change ck/dk/unrel interactions pre-to-post hurr.
##################################################

## groom #

#groom Kin
kin<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(Kin), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Kin")+
  labs(fill = "Group",x="Group",y="Change in prop. of grooming pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
# dev.off()
ggsave(kin, file ="changeKinPref_groom_minObs.png")
ggsave(kin, file ="changeKinPref_groom_minObs.eps")


##################################################
# SOCIAL HOMOPHILY : plot %change Solitary.Solitary/greg.greg/Solitary.greg/greg/Solitary interactions pre-to-post hurr.
##################################################

## groom #

#groom Solitary.Solitary
# tiff("Solitary2Solitary.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
Solitary2Solitary<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialHomophily.shy), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Solitary-->Solitary")+
  labs(fill = "Group",x="Group",y="Change in prop. of grooming pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
# dev.off()

#groom greg.greg
# tiff("greg2greg.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
greg2greg<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialHomophily.greg), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Greg-->Greg")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
# dev.off()

# #groom Solitary.greg
Solitary2greg<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialOpposite.shygreg), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Solitary-->Greg")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")


# #groom greg.Solitary
# tiff("greg2Solitary.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
greg2Solitary<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(SocialOpposite.gregshy), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Greg-->Solitary")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
# dev.off()

FullPlot.groom = grid.arrange(Solitary2Solitary,greg2greg,Solitary2greg,greg2Solitary, ncol=4, nrow=1)
ggsave(FullPlot.groom, file ="changeSocialPref_groom_minObs.png")
ggsave(FullPlot.groom, file ="changeSocialPref_groom_minObs.eps")

########################################
# SEX: plot %change MM/MF/FM/FF interactions pre-to-post hurr.
########################################

## groom #

# # groom Male --> Male
M2M<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(MM), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Male-->Male")+
  labs(fill = "Group",x="Group",y="Change in prop. of grooming pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")

# # groom Male --> Female
M2F<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(MF), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Male-->Female")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")

# groom Female --> Male
# tiff("F2M.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
F2M<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(FM), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Female-->Male")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")
# dev.off()

# groom Female --> Female
F2F<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(FF), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Female-->Female")+
  labs(fill = "Group",x="Group",y=NULL)+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")

FullPlot.groom = grid.arrange(M2M,M2F,F2M,F2F, ncol=4, nrow=1)
ggsave(FullPlot.groom, file ="changeSexPref_groom_minObs.png")
ggsave(FullPlot.groom, file ="changeSexPref_groom_minObs.eps")

