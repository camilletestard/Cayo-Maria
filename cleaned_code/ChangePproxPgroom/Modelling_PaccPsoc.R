######## Modelling Logistic Regressions
# Modelling_PaccPsoc: Binomial Mixed Models of p(proximity) & p(grooming). Has p(groom) & p(prox) changed post-hurricane?
#   Functions called: CalcSubsampledScans
# Input: allScans.txt
# Output: Model parameters over 500 iterations (multiple iterations of sub-sampling to make sure we actually take all 
# post-hurricane data into account). Models: 
# - p(prox) or p(groom) ~ isPost*quarter + sex + age + rank + group + timeBlock + (1|year) + (1|ID)
# - Models are run separately for each group
# Model outputs are saved as "ModelEffectsFinal.RData"
# The second part of the script takes in "ModelEffectsFinal.RData" as input, averages across iterations, finds the 95% CI of parameter
# estimates and saves a .csv file for each group.

library(lme4)# Generalized Linear Mixed Models
library(glmmTMB)
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)
library(ggpubr)
library(dplyr)
library(fitdistrplus)
library(lmtest)

#Load AllScans file
setwd("~/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
allScans = read.csv("Data All Cleaned/allScans.txt")

###############################################################################################
#Run glmm on social rates
###############################################################################################
#Note: If the 500 iterations of the model were already run, skip to the next section: "visualize model effects"

iter = 500;  start_time <- Sys.time()
NotAloneEffects = data.frame(); SocialEffects = data.frame();
NotAlone.PM.Effects = data.frame(); Social.PM.Effects = data.frame();
NotAlone.G.Effects = data.frame(); Social.G.Effects = data.frame();
NotAlone.V.Effects = data.frame(); Social.V.Effects = data.frame();
NotAlone.KK.Effects = data.frame(); Social.KK.Effects = data.frame();

#Load previous 
# load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ModelEffectsFinal.RData")
i=1

for (i in 1:iter) {
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",i, "%%%%%%%%%%%%%%%%%%"))
  
  ExSubScans = calcRandomScans(allScans)
  #Scale parameters
  ExSubScans[,"age"] <- scale(ExSubScans[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
  ExSubScans$percentrank <- ExSubScans$percentrank/100
  ExSubScans$year <- as.factor(ExSubScans$year); ExSubScans$isPost <- as.factor(ExSubScans$isPost)
  ExSubScans$isProx <- as.factor(ExSubScans$isProx); ExSubScans$isSocial <- as.factor(ExSubScans$isSocial)
  
  #Group V
  ExSubScansV = ExSubScans[which(ExSubScans$group=='V'),]
  
  #PROXIMITY MODEL
  isNotAloneV <- glmer(isProx~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID) + (1|year), data = ExSubScansV, family = binomial) #Note: might want to try MCMCglmm?
  # performance::check_model(isNotAloneV)
  print(summary(isNotAloneV))
  #Plot effects seperately
  # plot(effects::allEffects(isNotAloneV))
  # sjPlot::plot_model(isNotAloneV, type="re", vline.color = "black")
  NotAlone.V.Effects[i,c("(Intercept)","isPost","Q","sexM","age","rank","timeBlockPM","isPost1:Q")] <- getME(isNotAloneV, "beta")
  NotAlone.V.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneV, "theta")
  
  #GROOMING MODEL
  isSocialV <- glmer(isSocial~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID) + (1|year), data = ExSubScansV, family = binomial)
  print(summary(isSocialV))
  # performance::check_collinearity(isSocialV)
  # performance::check_model(isSocialV)
  Social.V.Effects[i,c("(Intercept)","isPost","Q","sexM","age","rank","timeBlockPM","isPost1:Q")] <- getME(isSocialV, "beta")
  Social.V.Effects[i,c("(focalID)","(year)")] <- getME(isSocialV, "theta")
  
  #Group KK
  ExSubScansKK = ExSubScans[which(ExSubScans$group=='KK'),]
  
  #PROXIMITY MODEL
  isNotAloneKK <- glmer(isProx~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID) + (1|year), data = ExSubScansKK, family = binomial) #Note: might want to try MCMCglmm?
  print(summary(isNotAloneKK))
  #plot(effects::allEffects(isNotAloneKK))
  # performance::check_model(isNotAloneKK)
  NotAlone.KK.Effects[i,c("(Intercept)","isPost","Q","sexM","age","rank","timeBlockPM","isPost1:Q")] <- getME(isNotAloneKK, "beta")
  NotAlone.KK.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneKK, "theta")
  
  #GROOMING MODEL
  isSocialKK <- glmer(isSocial~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID) + (1|year), data = ExSubScansKK, family = binomial)
  print(summary(isSocialKK))
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocialKK)
  Social.KK.Effects[i,c("(Intercept)","isPost","Q","sexM","age","rank","timeBlockPM","isPost1:Q")] <- getME(isSocialKK, "beta")
  Social.KK.Effects[i,c("(focalID)","(year)")] <- getME(isSocialKK, "theta")
  
  save.image("~/Documents/GitHub/Cayo-Maria/R.Data/ModelEffectsFinal.RData") #Save output
  
}
end_time <- Sys.time()
end_time - start_time

###############################################################################################
# Save model effects into .csv tables
###############################################################################################

#Load data
load ("~/Documents/GitHub/Cayo-Maria/R.Data/ModelEffectsFinal.RData")

library(ggplot2)
library(matrixStats)
library(gridExtra) 
library(graphics)

setwd("~/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPSoc/")
#GROUP V MODEL EFFECTS
Means = colMeans2(as.matrix(NotAlone.V.Effects)); Means = round(Means,3)
CI = colQuantiles(as.matrix(NotAlone.V.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.V<-tableGrob(Estimates);t.Acc.V<-grid.arrange(t.Acc.V, top="p(Prox) Model (Group V)");
write.csv(Estimates,"pprox.V.csv")

Means = colMeans2(as.matrix(Social.V.Effects)); Means = round(Means,3)
CI = colQuantiles(as.matrix(Social.V.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.V<-tableGrob(Estimates);t.Soc.V<-grid.arrange(t.Soc.V, top="p(Groom) Model (Group V)");
write.csv(Estimates,"psoc.V.csv")

#GROUP KK MODEL EFFECTS
Means = colMeans2(as.matrix(NotAlone.KK.Effects)); Means = round(Means,3)
CI = colQuantiles(as.matrix(NotAlone.KK.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.KK<-tableGrob(Estimates);t.Acc.KK<-grid.arrange(t.Acc.KK, top="p(Prox) Model (Group KK)");
write.csv(Estimates,"pprox.KK.csv")

Means = colMeans2(as.matrix(Social.KK.Effects)); Means = round(Means,3)
CI = colQuantiles(as.matrix(Social.KK.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.KK<-tableGrob(Estimates);t.Soc.KK<-grid.arrange(t.Soc.KK, top="p(Groom) Model (Group KK)");
write.csv(Estimates,"psoc.KK.csv")

