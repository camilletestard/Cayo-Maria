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
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/PartnerAttributes_groom.RData")
PartnerAttr$groupyear = paste(PartnerAttr$group, PartnerAttr$year, sep="")
PartnerAttr$isPost = as.factor(PartnerAttr$isPost)
for (col in seq(6,33)) {PartnerAttr[,col] = as.numeric(PartnerAttr[,col])}

#Separate data by grooming and groomimity
data.groom = PartnerAttr[which(PartnerAttr$action=="groom"),]
data.groom.V=data.groom[data.groom$group=="V",];data.groom.KK=data.groom[data.groom$group=="KK",]
#Select columns of interest
data.V.pre = data.groom.V[data.groom.V$isPost==0,c(4,8:33)]; data.V.post = data.groom.V[data.groom.V$isPost==1,c(4,8:33)]; 
data.KK.pre = data.groom.KK[data.groom.KK$isPost==0,c(4,8:33)]; data.KK.post = data.groom.KK[data.groom.KK$isPost==1,c(4,8:33)]; 

##############################################
#PLOTS/visualizations

#GROUP V
data.V.diff = cbind(data.V.post[,1],data.V.post[,2:27]-data.V.pre[,2:27])#Compute difference in proportions between pre- and post- for each subsampling iteration
names(data.V.diff)[1]="year"
#GROUP KK
data.KK.diff = cbind(data.KK.post[,1],data.KK.post[,2:27]-data.KK.pre[,2:27])
names(data.KK.diff)[1]="year"


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
  theme(legend.position = "none")+
  facet_grid(~year)
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
ggsave(FullPlot.groom, file ="changeRankPref_groom.png")
ggsave(FullPlot.groom, file ="changeRankPref_groom.eps")


##################################################
# KINSHIP : plot %change ck/dk/unrel interactions pre-to-post hurr.
##################################################

## groom #
setwd('C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/PartnerPreference/groomPartnerPref')

#groom Kin
kin<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(Kin), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed", lwd=2)+
  ggtitle("Kin --> Kin")+
  labs(fill = "Group",x="Group",y="Change in prop. of grooming pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=20)+
  theme(legend.position = "none")+
  facet_grid(~year)
# dev.off()
ggsave(kin, file ="changeKinPref_groom.png")
ggsave(kin, file ="changeKinPref_groom.eps")


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
ggsave(FullPlot.groom, file ="changeSocialPref_groom.png")
ggsave(FullPlot.groom, file ="changeSocialPref_groom.eps")

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
ggsave(FullPlot.groom, file ="changeSexPref_groom.png")
ggsave(FullPlot.groom, file ="changeSexPref_groom.eps")

