#Modelling_WhoAreNewPartners
# This script visualizes and runs statistics on the output of "generate_WhoAreTheNewPartners".
# = proportions of pair categories for each group/year/hurricane status separately. Pair categories based on: 
#   - Social status: Low->Low; Low->High; High->Low; High->High. Note: Low rank <70%; High rank >70%
#   - Sex: M->M; F->M; M->F; F->F
#   - Pre-hurr grooming strength: greg->greg; greg->shy; shy->greg; shy->shy. Note: threshold for shy/greg is 75% (or prctile)
#   - Kinship: close kin (ck, >0.25); distant kin (dk) and unrelated (unrel <0.125)
# This script will allows us to assess the difference in relationship distribution pre-to-post hurricane. 
# E.g. are there more F->M relationships occuring post-disaster? 
#   The input unit is in weighted % (or weighted proportion of all edges/relationships, i.e. % of grooming time occurs for pairs of category X). 
# Input: PartnerAttributes.RData
# Visulizations: Box plots separated by group, year and hurricane status of the proportons in each category.
# Models: beta family models (appropriate for proportions). isPost as fixed; groupyear as random effect. There is one 
# model per pair category (total of 15)

library(ggplot2)
library(lme4)# Generalized Linear Mixed Models
library(glmmTMB)
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)

#load & format data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/PartnerAttributes.RData")
PartnerAttr$groupyear = paste(PartnerAttr$group, PartnerAttr$year, sep="")
PartnerAttr$isPost = as.factor(PartnerAttr$isPost)
for (col in seq(6,19)) {PartnerAttr[,col] = as.numeric(PartnerAttr[,col])}

#Separate data by grooming and proximity
data.groom = PartnerAttr[which(PartnerAttr$action=="groom"),]
data.prox = PartnerAttr[which(PartnerAttr$action=="prox"),]

##############################################
#SOCIAL RANK : plot % HH/LH/HL/LL interactions
##############################################
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/WhoAreNewPartners") #set saving directory
## GROOM ##

#Groom Low to High
tiff("L2H.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(LowToHigh), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom LowR->HighR ")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of L2H interactions")+
  facet_grid(~groupyear)
dev.off()

data.groom$LowToHigh[data.groom$LowToHigh==0] = 0.00001 #Avoid error when running beta family model
l2h.groom <- glmmTMB(as.numeric(LowToHigh) ~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(l2h.groom, digits = 3)
# performance::check_model(l2h.groom)

#Groom High to Low
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(HighToLow), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom HighR->LowR ")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of H2L interactions")+
  facet_grid(~groupyear)

data.groom$HighToLow[data.groom$HighToLow==0] = 0.00001 #Avoid error when running beta family model
h2l.groom <- glmmTMB(as.numeric(HighToLow) ~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(h2l.groom, digits = 3)
# performance::check_model(h2l.groom)

#Groom Low to Low
tiff("L2L.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(LowToLow), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom LowR to LowR ")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of L2L interactions")+
  facet_grid(~groupyear)
dev.off()

l2l.groom <- glmmTMB(as.numeric(LowToLow) ~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(l2l.groom, digits = 3)
# performance::check_model(l2l.groom)

#Groom High to high
tiff("H2H.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(HighToHigh), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom HighR to HighR ")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of H2H interactions")+
  facet_grid(~groupyear)
dev.off()

h2h.groom <- glmmTMB(as.numeric(HighToHigh) ~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(h2h.groom, digits = 3)
# performance::check_model(h2h.groom)

## PRXOIMITY ##

# #Prox Low to High
# ggplot(data.prox, aes(x= as.factor(isPost), y=as.numeric(LowToHigh), fill=as.factor(isPost) ))+
#   geom_boxplot()+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
#   ggtitle("Prox LowR->HighR ")+
#   labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of L2H interactions")+
#   facet_grid(~groupyear)
# 
# l2h.prox <- glmmTMB(as.numeric(LowToHigh)~ isPost*group +(1|year), data=data.prox, family = beta_family(link="logit"))
# summary(l2h.prox, digits = 3)
# performance::check_model(l2h.prox)

# #Prox Low to Low
# ggplot(data.prox, aes(x= as.factor(isPost), y=as.numeric(LowToLow), fill=as.factor(isPost) ))+
#   geom_boxplot()+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
#   ggtitle("Prox LowR to LowR ")+
#   labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of L2L interactions")+
#   facet_grid(~groupyear)

##################################################
# KINSHIP : plot % ck/dk/unrel interactions
##################################################

## GROOM #

#Groom Kin
tiff("Kin.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(Kin), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Kin")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Kin interactions")+
  facet_grid(~groupyear)
dev.off()

data.groom$Kin[data.groom$Kin==0] = 0.00001 #Avoid error when running beta family model
kin.groom <- glmmTMB(as.numeric(Kin) ~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(kin.groom, digits = 3)
# performance::check_model(kin.groom)

#Groom unrelated
tiff("Unrel.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(Unrel), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Unrelated")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Unrelated interactions")+
  facet_grid(~groupyear)
dev.off()

data.groom$Unrel[data.groom$Unrel==0] = 0.00001 #Avoid error when running beta family model
data.groom$Unrel[data.groom$Unrel==1] = 0.99999 #Avoid error when running beta family model
unrel.groom <- glmmTMB(as.numeric(Unrel) ~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(unrel.groom, digits = 3)
# performance::check_model(unrel.groom)

## PROXIMITY ##

# ggplot(data.prox, aes(x= as.factor(isPost), y=as.numeric(Kin), fill=as.factor(isPost) ))+
#   geom_boxplot()+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
#   ggtitle("Prox Kin")+
#   labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Kin interactions")+
#   facet_grid(~groupyear)
# 
# kin.prox <- glmmTMB(as.numeric(Kin)~ isPost*group +(1|year), data=data.prox, family = beta_family(link="logit"))
# summary(kin.prox, digits = 3)
# performance::check_model(kin.groom)

##################################################
# SOCIAL HOMOPHILY : plot % shy.shy/greg.greg/shy.greg/greg/shy interactions
##################################################

## GROOM #

#Groom shy.shy
tiff("shy2shy.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(SocialHomophily.shy), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Social Homophily shy2shy")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Homophilic interactions")+
  facet_grid(~groupyear)
dev.off()

data.groom$SocialHomophily.shy[data.groom$SocialHomophily.shy==0] = 0.00001 #Avoid error when running beta family model
socialHomShy.groom <- glmmTMB(as.numeric(SocialHomophily.shy)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(socialHomShy.groom, digits = 3)
# performance::check_model(socialHomShy.groom)

#Groom greg.greg
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(SocialHomophily.greg), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Social Homophily greg2greg")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Homophilic interactions")+
  facet_grid(~groupyear)

data.groom$SocialHomophily.greg[data.groom$SocialHomophily.greg==0] = 0.00001 #Avoid error when running beta family model
socialHomGreg.groom <- glmmTMB(as.numeric(SocialHomophily.greg)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(socialHomGreg.groom, digits = 3)
# performance::check_model(socialHomGreg.groom)

#Groom shy.greg
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(SocialOpposite.shygreg), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Social Opposite shy2greg")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Opposite interactions")+
  facet_grid(~groupyear)

socialOppShy2Greg.groom <- glmmTMB(as.numeric(SocialOpposite.shygreg)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(socialOppShy2Greg.groom, digits = 3)
# performance::check_model(socialOppShy2Greg.groom)

#Groom greg.shy
tiff("greg2shy.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(SocialOpposite.gregshy), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Social Opposite greg2shy")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Opposite interactions")+
  facet_grid(~groupyear)
dev.off()

socialOppGreg2Shy.groom <- glmmTMB(as.numeric(SocialOpposite.gregshy)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(socialOppGreg2Shy.groom, digits = 3)
# performance::check_model(socialOppGreg2Shy.groom)

## PROXIMITY ##

# ggplot(data.prox, aes(x= as.factor(isPost), y=as.numeric(SocialHomopholy), fill=as.factor(isPost) ))+
#   geom_boxplot()+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
#   ggtitle("Prox Social Homophily")+
#   labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of Homophilic interactions")+
#   facet_grid(~groupyear)
# 
# socialHom.prox <- glmmTMB(as.numeric(SocialHomophily)~ isPost*group +(1|year), data=data.prox, family = beta_family(link="logit"))
# summary(socialHom.prox, digits = 3)
# performance::check_model(socialHom.prox)

########################################
# SEX: plot % MM/MF/FM/FF interactions
########################################

## GROOM #

# Groom Male --> Male
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(MM), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Male->Male")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of M->M interactions")+
  facet_grid(~groupyear)

data.groom$MM[data.groom$MM==0] = 0.00001 #Avoid error when running beta family model
MM.groom <- glmmTMB(as.numeric(MM)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(MM.groom, digits = 3)
# performance::check_model(MM.groom)

# Groom Male --> Female
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(MF), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Male->Female")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of M->F interactions")+
  facet_grid(~groupyear)

MF.groom <- glmmTMB(as.numeric(MF)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(MF.groom, digits = 3)
# performance::check_model(MF.groom)

# Groom Female --> Male
tiff("F2M.change.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(FM), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Female->Male")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of F->M interactions")+
  facet_grid(~groupyear)
dev.off()

FM.groom <- glmmTMB(as.numeric(FM)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(FM.groom, digits = 3)
# performance::check_model(FM.groom)

# Groom Female --> Female
ggplot(data.groom, aes(x= as.factor(isPost), y=as.numeric(FF), fill=as.factor(isPost) ))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Groom Female->Female")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of F->F interactions")+
  facet_grid(~groupyear)

FF.groom <- glmmTMB(as.numeric(FF)~ isPost*group +(1|year), data=data.groom, family = beta_family(link="logit"))
summary(FF.groom, digits = 3)
# performance::check_model(FF.groom)

## PROXIMITY ##

# # Prox Female --> Male
# ggplot(data.prox, aes(x= as.factor(isPost), y=as.numeric(FM), fill=as.factor(isPost)))+
#   geom_boxplot()+
#   geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
#   ggtitle("Prox Female->Male")+
#   labs(fill = "Hurricane Status",x="Hurricane Status",y="Proportion of F->M interactions")+
#   facet_grid(~groupyear)
# 
# FM.prox <- glmmTMB(as.numeric(FM)~ isPost*group +(1|year), data=data.prox, family = beta_family(link="logit"))
# summary(FM.groom, digits = 3)
# performance::check_model

#############################################
#Save and export models

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/WhoAreNewPartners")
export_summs(l2h.groom, h2l.groom, h2h.groom, l2l.groom, model.names = c("LR.HR", "HR.LR", "HR.HR", "LR.LR"),
             to.file = "docx", file.name = "Modeling.Rank.NewPartners.docx")
export_summs(kin.groom, unrel.groom, model.names = c("Kin","Unrel"),to.file = "docx", file.name = "Modeling.Kin.NewPartners.docx")
export_summs(socialHomShy.groom, socialHomGreg.groom, socialOppGreg2Shy.groom, socialOppShy2Greg.groom, 
             model.names=c("shy.shy","greg.greg", "greg.shy", "shy.greg"),
             to.file = "docx", file.name = "Modeling.Social.NewPartners.docx")
export_summs(MM.groom, MF.groom, FM.groom, FF.groom, 
             model.names = c("M.M", "M.F", "F.M", "F.F"),to.file = "docx", file.name = "Modeling.Sex.NewPartners.docx")