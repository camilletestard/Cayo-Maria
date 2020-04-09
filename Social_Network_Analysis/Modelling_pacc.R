######## Modelling Logistic Regressions on A2

library(lme4)# Generalized Linear Mixed Models
library(MCMCglmm)# Generalized Linear Mixed Models, other package
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#Run glmm on social rates
iter = 100;  start_time <- Sys.time()
NotAloneEffects = data.frame(); SocialEffects = data.frame();
NotAlone.PM.Effects = data.frame(); Social.PM.Effects = data.frame();
NotAlone.G.Effects = data.frame(); Social.G.Effects = data.frame();
NotAlone.V.Effects = data.frame(); Social.V.Effects = data.frame();
NotAlone.KK.Effects = data.frame(); Social.KK.Effects = data.frame();
NotAlone.S.Effects = data.frame(); Social.S.Effects = data.frame();
NotAlone.Q.Effects = data.frame(); Social.Q.Effects = data.frame();
NotAlone.F.Effects = data.frame(); Social.F.Effects = data.frame();
NotAlone.M.Effects = data.frame(); Social.M.Effects = data.frame();
NotAlone.R.Effects = data.frame(); Social.R.Effects = data.frame();
NotAlone.A.Effects = data.frame(); Social.A.Effects = data.frame();

#load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/ModelEffects.RData")

for (i in 1:iter) {
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",i, "%%%%%%%%%%%%%%%%%%"))
  
  ExSubScans = calcRandomScans(allScans)
  #Scale parameters
  ExSubScans[,"age"] <- scale(ExSubScans[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
  ExSubScans[,"percentrank"] <- ExSubScans[,"percentrank"]/10
  ExSubScans$year <- as.factor(ExSubScans$year); ExSubScans$isPost <- as.factor(ExSubScans$isPost)
  ExSubScans$isProx <- as.factor(ExSubScans$isProx); ExSubScans$isSocial <- as.factor(ExSubScans$isSocial)
  
  ##########################################################
  ##BASE MODEL
  ##########################################################
  
  isNotAlone <- glmer(isProx~ isPost + sex + age + percentrank + group + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAlone)
  # simres1 <- simulateResiduals(isNotAlone, n = 1000)
  # testResiduals(simres1)
  # performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
  NotAloneEffects[i,c("(Intercept)","isPost","sexM","age","rank","groupV")] <- getME(isNotAlone, "beta")
  NotAloneEffects[i,c("(focalID)","(year)")] <- getME(isNotAlone, "theta")
  
  isSocial <- glmer(isSocial~ isPost + sex + age + percentrank + group + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
  summary(isSocial)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  SocialEffects[i,c("(Intercept)","isPost","sexM","age","rank","groupV")] <- getME(isSocial, "beta")
  SocialEffects[i,c("(focalID)","(year)")] <- getME(isSocial, "theta")

  ##########################################################
  ##ONLY AFTERNOON MODEL
  ##########################################################
  
  ExSubScansPM = ExSubScans[which(ExSubScans$timeBlock=='PM'),]
  
  isNotAlonePM <- glmer(isProx~ isPost + sex + age + percentrank + group + (1|focalID) + (1|year), data = ExSubScansPM, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAlonePM)
  NotAlone.PM.Effects[i,c("(Intercept)","isPost","sexM","age","rank","groupV")] <- getME(isNotAlonePM, "beta")
  NotAlone.PM.Effects[i,c("(focalID)","(year)")] <- getME(isNotAlonePM, "theta")
  
  isSocialPM <- glmer(isSocial~ isPost + sex + age + percentrank + group+ (1|focalID) + (1|year), data = ExSubScansPM, family = binomial)
  summary(isSocialPM)
  Social.PM.Effects[i,c("(Intercept)","isPost","sexM","age","rank","groupV")] <- getME(isSocialPM, "beta")
  Social.PM.Effects[i,c("(focalID)","(year)")] <- getME(isSocialPM, "theta")
  
  ##########################################################
  ##See differences between groups
  ##########################################################
  
  isNotAloneG <- glmer(isProx~ isPost*group + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneG)
  NotAlone.G.Effects[i,c("(Intercept)","isPost","groupV","sexM","age","rank","isPost:groupV")] <- getME(isNotAloneG, "beta")
  NotAlone.G.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneG, "theta")
  
  isSocialG <- glmer(isSocial~ isPost*group + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
  summary(isSocialG)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.G.Effects[i,c("(Intercept)","isPost","groupV","sexM","age","rank","isPost:groupV")] <- getME(isSocialG, "beta")
  Social.G.Effects[i,c("(focalID)","(year)")] <- getME(isSocialG, "theta")
  
  ##################################################################
  
  #Group V
  ExSubScansV = ExSubScans[which(ExSubScans$group=='V'),]
  
  isNotAloneV <- glmer(isProx~ isPost + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScansV, family = binomial) #Note: might want to try MCMCglmm?
  #isNotAloneV <- glmer(isProx~ isPost + sex + age + percentrank + (1|year), data = ExSubScansV, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneV)
  #Plot effects seperately
  plot(effects::allEffects(isNotAloneV))
  sjPlot::plot_model(isNotAloneV, type="re", vline.color = "black")
  NotAlone.V.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isNotAloneV, "beta")
  NotAlone.V.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneV, "theta")
  
  isSocialV <- glmer(isSocial~ isPost + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScansV, family = binomial)
  summary(isSocialV)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.V.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isSocialV, "beta")
  Social.V.Effects[i,c("(focalID)","(year)")] <- getME(isSocialV, "theta")
  
  #Group KK
  ExSubScansKK = ExSubScans[which(ExSubScans$group=='KK'),]
  
  isNotAloneKK <- glmer(isProx~ isPost + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScansKK, family = binomial) #Note: might want to try MCMCglmm?
  #isNotAloneKK <- glmer(isProx~ isPost + sex + age + percentrank + year + (1|focalID), data = ExSubScansKK, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneKK)
  #plot(effects::allEffects(isNotAloneKK))
  NotAlone.KK.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isNotAloneKK, "beta")
  NotAlone.KK.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneKK, "theta")
  
  isSocialKK <- glmer(isSocial~ isPost + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScansKK, family = binomial)
  summary(isSocialKK)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.KK.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isSocialKK, "beta")
  Social.KK.Effects[i,c("(focalID)","(year)")] <- getME(isSocialKK, "theta")
  
  ##########################################################
  ##See differences between sex
  ##########################################################  
  
  # Males
  ExSubScansM = ExSubScans[which(ExSubScans$sex=="M"),]
  
  isNotAloneM <- glmer(isProx~ isPost + group + age + percentrank+ (1|focalID) + (1|year), data = ExSubScansM, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneM)
  # simres1 <- simulateResiduals(isNotAlone, n = 1000)
  # testResiduals(simres1)
  # performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
  NotAlone.M.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isNotAloneM, "beta")
  NotAlone.M.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneM, "theta")
  
  isSocialM <- glmer(isSocial~ isPost + group + age + percentrank+ (1|focalID) + (1|year), data = ExSubScansM, family = binomial)
  summary(isSocialM)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.M.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isSocialM, "beta")
  Social.M.Effects[i,c("(focalID)","(year)")] <- getME(isSocialM, "theta")
  
  ##########################################################
  
  # Females
  ExSubScansF = ExSubScans[which(ExSubScans$sex=="F"),]
  
  isNotAloneF <- glmer(isProx~ isPost + group + age + percentrank+ (1|focalID) + (1|year), data = ExSubScansF, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneF)
  # simres1 <- simulateResiduals(isNotAlone, n = 1000)
  # testResiduals(simres1)
  # performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
  NotAlone.F.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isNotAloneF, "beta")
  NotAlone.F.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneF, "theta")
  
  isSocialF <- glmer(isSocial~ isPost + group + age + percentrank + (1|focalID) + (1|year), data = ExSubScansF, family = binomial)
  summary(isSocialF)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.F.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isSocialF, "beta")
  Social.F.Effects[i,c("(focalID)","(year)")] <- getME(isSocialF, "theta")
  
  ##########################################################
  
  #For Both Sex
  isNotAloneS <- glmer(isProx~ isPost*sex + group + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneS)
  # simres1 <- simulateResiduals(isNotAlone, n = 1000)
  # testResiduals(simres1)
  # performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
  NotAlone.S.Effects[i,c("(Intercept)","isPost","sexM","groupV","age","rank","isPost:sexM")] <- getME(isNotAloneS, "beta")
  NotAlone.S.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneS, "theta")
  
  isSocialS <- glmer(isSocial~ isPost*sex + group + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
  summary(isSocialS)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.S.Effects[i,c("(Intercept)","isPost","sexM","groupV","age","rank","isPost:sexM")] <- getME(isSocialS, "beta")
  Social.S.Effects[i,c("(focalID)","(year)")] <- getME(isSocialS, "theta")
  
  ##########################################################
  ##See differences between quarter
  ##########################################################  
  
  isNotAloneQ <- glmer(isProx~ isPost*Q + sex + group + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneQ)
  # simres1 <- simulateResiduals(isNotAlone, n = 1000)
  # testResiduals(simres1)
  # performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
  NotAlone.Q.Effects[i,c("(Intercept)","isPost","Q","sexM","groupV","age","rank","isPost:Q")] <- getME(isNotAloneQ, "beta")
  NotAlone.Q.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneQ, "theta")
  
  isSocialQ <- glmer(isSocial~ isPost*Q + sex + group + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
  summary(isSocialQ)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.Q.Effects[i,c("(Intercept)","isPost","Q","sexM","groupV","age","rank","isPost:Q")] <- getME(isSocialQ, "beta")
  Social.Q.Effects[i,c("(focalID)","(year)")] <- getME(isSocialQ, "theta")
  
  ##########################################################
  ##See differences between rank
  ##########################################################  
  
  isNotAloneR <- glmer(isProx~ isPost*percentrank + sex + group + age + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneR)
  # simres1 <- simulateResiduals(isNotAlone, n = 1000)
  # testResiduals(simres1)
  # performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
  NotAlone.R.Effects[i,c("(Intercept)","isPost","rank","sexM","groupV","age","isPost:Rank")] <- getME(isNotAloneR, "beta")
  NotAlone.R.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneR, "theta")
  
  isSocialR <- glmer(isSocial~ isPost*percentrank + sex + group + age + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
  summary(isSocialR)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.R.Effects[i,c("(Intercept)","isPost","rank","sexM","groupV","age","isPost:Rank")] <- getME(isSocialR, "beta")
  Social.R.Effects[i,c("(focalID)","(year)")] <- getME(isSocialR, "theta")
 
  ##########################################################
  ##See differences between age
  ##########################################################
  
  isNotAloneA <- glmer(isProx~ isPost*age + sex + group + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
  summary(isNotAloneA)
  # simres1 <- simulateResiduals(isNotAlone, n = 1000)
  # testResiduals(simres1)
  # performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
  NotAlone.A.Effects[i,c("(Intercept)","isPost","age","sexM","groupV","rank","isPost:age")] <- getME(isNotAloneA, "beta")
  NotAlone.A.Effects[i,c("(focalID)","(year)")] <- getME(isNotAloneA, "theta")
  
  isSocialA <- glmer(isSocial~ isPost*age + sex + group + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
  summary(isSocialA)
  # simres2 <- simulateResiduals(isSocial, n = 1000)
  # testResiduals(simres2)
  # performance::check_model(isSocial)
  Social.A.Effects[i,c("(Intercept)","isPost","age","sexM","groupV","rank","isPost:age")] <- getME(isSocialA, "beta")
  Social.A.Effects[i,c("(focalID)","(year)")] <- getME(isSocialA, "theta")
  
  save.image("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/ModelEffectsFinal.RData")
  
}
end_time <- Sys.time()
end_time - start_time
