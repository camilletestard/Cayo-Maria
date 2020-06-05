######## Modelling strength to stable partners pre-to-post

library(lme4)# Generalized Linear Mixed Models
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)

#Load data
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("Social_Network_Analysis/Strength.Stable.New.Old.Partners.RData")

##########################################
#Add sex, age and rank info
##########################################
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
data.prox = ID.strength.stableP.All[which(ID.strength.stableP.All$action == "prox"),]
data.groom[,"age"] <- scale(data.groom[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
data.prox[,"age"] <- scale(data.prox[,"age"])

##########################################################
## MODELLING
##########################################################
#Note: for now I implement one model over multiple iterations.
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/New.vs.Stable.Partner.Strength") #set saving directory

proxStableP <- glmer(strength.all~ isPost + sex + age + percentrank + group + (1|id) + (1|year), data = data.prox, family = Gamma(link=log))
summ(proxStableP, digits = 3)
# performance::check_model(proxStableP)
export_summs(proxStableP, model.names = c("StableP_Prox"), digits=3, to.file = "docx", file.name = "StableP.Prox.BM.docx")

groomStableP  <- lmer(strength.all~ isPost + sex + age + percentrank + group + (1|id) + (1|year), data = data.groom)
summ(groomStableP, digits = 3)
# performance::check_model(groomStableP)
export_summs(groomStableP, model.names = c("StableP_Groom"), digits=3, to.file = "docx", file.name = "StableP.Groom.BM.docx")

# # Separate model for each iteration and keep track of values each time.
# StableP.prox = data.frame(); StableP.groom=data.frame()
# for (iter in 1:max(ID.strength.stableP.All$iter)) {
#   
#   print(paste("%%%%%%%%%%%%%%%%%%",iter, "%%%%%%%%%%%%%%%%%%"))
#   
#   data.prox.iter = data.prox[which(data.prox$iter == iter),]
#   data.groom.iter = data.groom[which(data.groom$iter == iter),]
# 
#   proxStableP <- glmer(strength.all~ isPost + sex + age + percentrank + group + (1|id) + (1|year), data = data.prox.iter, family = Gamma(link=log))
#   summ(proxStableP, digits = 3)
#   # performance::check_model(proxStableP)
#   StableP.prox[iter,c("(Intercept)","isPost","sexM","age","rank","groupV")] <- getME(proxStableP, "beta")
#   StableP.prox[iter,c("(focalID)","(year)")] <- getME(proxStableP, "theta")
# 
#   groomStableP  <- lmer(strength.all~ isPost + sex + age + percentrank + group + (1|id) + (1|year), data = data.groom.iter)
#   summ(groomStableP, digits = 3)
#   # performance::check_model(groomStableP)
#   StableP.groom[iter,c("(Intercept)","isPost","sexM","age","rank","groupV")] <- getME(groomStableP, "beta")
#   StableP.groom[iter,c("(focalID)","(year)")] <- getME(groomStableP, "theta")
# 
# }
# 
# Means = colMeans2(as.matrix(StableP.prox)); Means = round(Means,2)
# CI = colQuantiles(as.matrix(StableP.prox), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
# Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
# t.prox.BM<-tableGrob(Estimates); t.prox.BM<-grid.arrange(t.prox.BM, top="Model All Effects: Prox Model Parameter Estimates"); #create table, arrange table
# 
# Means = colMeans2(as.matrix(StableP.groom)); Means = round(Means,2)
# CI = colQuantiles(as.matrix(StableP.groom), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
# Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
# t.groom.BM<-tableGrob(Estimates); t.groom.BM<-grid.arrange(t.groom.BM, top="Model All Effects: Groom Model Parameter Estimates"); #create table, arrange table
# 
