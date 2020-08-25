# VisualizeModel_StrengthPrePost_StableP: Statistically test the difference in grooming strength to "stable" partners pre-to-post hurricane.
# Question: How much do IDs interact with partners they had before the hurricane?
#   Input:Strength.StablePartners.RData
# Note: bond strengths are log-transformed before being used as dependent variables in a linear mixed model to follow normal distribution assumption.
# Model: lmer(log(weight) ~ isPost + sex + age + percentrank+ (1|alter)+(1|year), data=data.groom.iter.V or KK)

library(lme4)# Generalized Linear Mixed Models
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)
library(matrixStats)
library(gridExtra) 
library(graphics)

#Load data
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("R.Data/Strength.StablePartners.RData")

##########################################
#Add sex, age and rank info
##########################################
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/")
population_info = read.csv("Behavioral_Data/SubjectInfo_2010-2017/Population details_Allgroups.allyears.txt")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Find all unique IDs
allIDs = unique(ID.strength.stableP.All$id)

#get sex and year of birth from "Population details_Allgroups.allyears.txt"
ID_info= data.frame(); count = 0
for (id in 1:length(allIDs)){
  idx = which(as.character(population_info$id) == allIDs[id])
  if (length(idx)!=0){
    count = count +1
    ID_info[count,c("id","sex","yob")]<-population_info[idx,c("id","sex","yob")]
  }
}
ID_info$age = 2017-ID_info$yob #age in 2017
ID.strength.stableP.All[,c("sex","age")] = ID_info[match(ID.strength.stableP.All$id,ID_info$id),c("sex","age")]

#Add rank info from "DOMINANCE.txt" file
ID.strength.stableP.All$rankFind = paste(ID.strength.stableP.All$id,ID.strength.stableP.All$year,sep="")
ID.strength.stableP.All$ordrank=NA; ID.strength.stableP.All$percentrank=NA; ID.strength.stableP.Allsexrank=NA
for (ii in 1:nrow(dominance_info)){
  idx = which(as.character(dominance_info$IDyear[ii]) == as.character(ID.strength.stableP.All$rankFind))
  if (length(idx)!=0){
    ID.strength.stableP.All$ordrank[idx] = as.character(dominance_info$ORD_RANK[ii])
    ID.strength.stableP.All$percentrank[idx] = as.character(dominance_info$X.DOMINATED[ii])
  }
}
ID.strength.stableP.All$percentrank = as.numeric(ID.strength.stableP.All$percentrank)/100

#Scale parameters
data.groom = ID.strength.stableP.All[which(ID.strength.stableP.All$action == "groom"),]
data.groom[,"age"] <- scale(data.groom[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

##########################################################
## MODELLING
##########################################################

#Note: for now I implement one model over multiple iterations.
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Partner.Strength.PrePost/New.vs.Stable.Partner.Strength") #set saving directory

# Separate model for each iteration and keep track of parameter estimates each time.
StableP.groom.V =data.frame(); StableP.groom.KK =data.frame();
for (iter in 1:max(ID.strength.stableP.All$iter)) {

  print(paste("%%%%%%%%%%%%%%%%%%",iter, "%%%%%%%%%%%%%%%%%%"))

  data.groom.iter.V = data.groom[which(data.groom$iter == iter& data.groom$group=="V"),]
  data.groom.iter.KK = data.groom[which(data.groom$iter == iter& data.groom$group=="KK"),]
  
  # ## VISUALIZATION
  # ##########################################################
  # data.groom.iter = data.groom[which(data.groom$iter == iter),]
  # #GROOMING
  # strength.stable.prepost<-ggplot(data.groom.iter, aes(x= as.factor(isPost), y=strength.all, fill=as.factor(isPost) ))+
  #   geom_violin()+
  #   # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  #   ggtitle(paste("Srength Bond Stable Partners"))+
  #   labs(fill = "Hurricane Status",x="Hurricane Status",y="Bond Strength")+
  #   facet_grid(~group)
  #   # facet_grid(group~year)
  # ggsave("Strength.stableP.prepost.tiff",strength.stable.prepost)
  # ggsave("Strength.stableP.prepost.eps",strength.stable.prepost)
  
  ## MODELLING
  ##########################################################
  
  #GROOMING
  groomStableP.V <- lmer(log(strength.all)~ isPost + sex + age + percentrank + (1|id) + (1|year), data = data.groom.iter.V)
  summary(groomStableP.V, digits = 3)
  # performance::check_model(groomStableP)
  StableP.groom.V[iter,c("(Intercept)","isPost","sexM","age","rank")] <- getME(groomStableP.V, "beta")
  StableP.groom.V[iter,c("(focalID)","(year)")] <- getME(groomStableP.V, "theta")
  
  groomStableP.KK <- lmer(log(strength.all)~ isPost + sex + age + percentrank + (1|id) + (1|year), data = data.groom.iter.KK)
  summary(groomStableP.KK, digits = 3)
  # performance::check_model(groomStableP)
  StableP.groom.KK[iter,c("(Intercept)","isPost","sexM","age","rank")] <- getME(groomStableP.KK, "beta")
  StableP.groom.KK[iter,c("(focalID)","(year)")] <- getME(groomStableP.KK, "theta")

}

# Pool results from all iterations - Groom
Means = colMeans2(as.matrix(StableP.groom.V)); Means = round(Means,3)
CI = colQuantiles(as.matrix(StableP.groom.V), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.groom.BM<-tableGrob(Estimates); t.groom.BM<-grid.arrange(t.groom.BM, top="Stable Partners Groom Model (Group V)"); #create table, arrange table
write.csv(Estimates, file="StrengthStableP.V.csv")

Means = colMeans2(as.matrix(StableP.groom.KK)); Means = round(Means,3)
CI = colQuantiles(as.matrix(StableP.groom.KK), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.groom.BM<-tableGrob(Estimates); t.groom.BM<-grid.arrange(t.groom.BM, top="Stable Partners Groom Model (Group KK)"); #create table, arrange table
write.csv(Estimates, file="StrengthStableP.KK.csv")
