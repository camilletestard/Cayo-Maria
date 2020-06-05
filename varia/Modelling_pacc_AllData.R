######## Modelling Logistic Regressions on ALl Data

#Note: All data include all observations from individuals that were present both before and after the hurricane. 
# Excludes individuals that are not present either pre-hurricane or post-hurricane

library(lme4)# Generalized Linear Mixed Models
library(MCMCglmm)# Generalized Linear Mixed Models, other package
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")

#Run glmm on social rates

SubScans =  allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
unqIDs = as.character(unique(SubScans$focalID))

#Exclude individuals that are not present either pre-hurricane or post-hurricane
for (id in 1:length(unqIDs)){ #For all individuals
  
  if (length(which(SubScans$focalID == unqIDs[id] & SubScans$isPost == 0))==0 
      | length(which(SubScans$focalID == unqIDs[id] & SubScans$isPost == 1))==0) {
    
    SubScans=SubScans[-which(SubScans$focalID == unqIDs[id]),]
    i=i+1
    
  }
}
ExSubScans=SubScans
unqIDs = as.character(unique(ExSubScans$focalID))

#Scale parameters
ExSubScans[,"age"] <- scale(ExSubScans[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
ExSubScans[,"percentrank"] <- ExSubScans[,"percentrank"]/100

##########################################################
##BASE MODEL
##########################################################

#Check there i ony one age per individual (at time of hurricane)
age= table(as.character(ExSubScans$focalID), ExSubScans$age)

glmer(SocialDiscounting ~ daysSocialDist + age + sex + daysSocialDist*age + (1|Geographic), data =MTurk )

isNotAlone <- glmer(isProx~ isPost + sex + age + percentrank + group + (1|focalID), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summ(isNotAlone, digits = 3)
# performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
export_summs(isNotAlone, model.names = c("BaseModel_Prox"), digits=3,
             to.file = "docx", file.name = "isNotAloneBM.docx")

isSocial <- glmer(isSocial~ isPost + sex + age + percentrank + group + (1|focalID), data = ExSubScans, family = binomial)
summ(isSocial)
# performance::check_model(isSocial)
export_summs(isSocial, model.names = c("BaseModel_Groom"), digits=3,
             to.file = "docx", file.name = "isSocialBM.docx")

##########################################################
##ONLY AFTERNOON MODEL
##########################################################

ExSubScansPM = ExSubScans[which(ExSubScans$timeBlock=='PM'),]

isNotAlonePM <- glmer(isProx~ isPost + sex + age + percentrank + group + (1|focalID), data = ExSubScansPM, family = binomial) #Note: might want to try MCMCglmm?
summ(isNotAlonePM)
export_summs(isNotAlonePM, model.names = c("PMmodel_Prox"), digits=3,
             to.file = "docx", file.name = "isNotAlonePM.docx")


isSocialPM <- glmer(isSocial~ isPost + sex + age + percentrank + group+ (1|focalID), data = ExSubScansPM, family = binomial)
summary(isSocialPM)
Social.PM.Effects[i,c("(Intercept)","isPost","sexM","age","rank","groupV")] <- getME(isSocialPM, "beta")
Social.PM.Effects[i,c("(focalID)")] <- getME(isSocialPM, "theta")

##########################################################
##See differences between groups
##########################################################

isNotAloneG <- glmer(isProx~ isPost*group + sex + age + percentrank + (1|focalID), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneG)
NotAlone.G.Effects[i,c("(Intercept)","isPost","groupV","sexM","age","rank","isPost:groupV")] <- getME(isNotAloneG, "beta")
NotAlone.G.Effects[i,c("(focalID)")] <- getME(isNotAloneG, "theta")

isSocialG <- glmer(isSocial~ isPost*group + sex + age + percentrank + (1|focalID), data = ExSubScans, family = binomial)
summary(isSocialG)
Social.G.Effects[i,c("(Intercept)","isPost","groupV","sexM","age","rank","isPost:groupV")] <- getME(isSocialG, "beta")
Social.G.Effects[i,c("(focalID)")] <- getME(isSocialG, "theta")

##################################################################

#Group V
ExSubScansV = ExSubScans[which(ExSubScans$group=='V'),]

isNotAloneV <- glmer(isProx~ isPost + sex + age + percentrank + (1|focalID), data = ExSubScansV, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneV)
NotAlone.V.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isNotAloneV, "beta")
NotAlone.V.Effects[i,c("(focalID)")] <- getME(isNotAloneV, "theta")

isSocialV <- glmer(isSocial~ isPost + sex + age + percentrank + (1|focalID), data = ExSubScansV, family = binomial)
summary(isSocialV)
Social.V.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isSocialV, "beta")
Social.V.Effects[i,c("(focalID)")] <- getME(isSocialV, "theta")

#Group KK
ExSubScansKK = ExSubScans[which(ExSubScans$group=='KK'),]

isNotAloneKK <- glmer(isProx~ isPost + sex + age + percentrank + (1|focalID), data = ExSubScansKK, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneKK)
NotAlone.KK.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isNotAloneKK, "beta")
NotAlone.KK.Effects[i,c("(focalID)")] <- getME(isNotAloneKK, "theta")

isSocialKK <- glmer(isSocial~ isPost + sex + age + percentrank + (1|focalID) , data = ExSubScansKK, family = binomial)
summary(isSocialKK)
Social.KK.Effects[i,c("(Intercept)","isPost","sexM","age","rank")] <- getME(isSocialKK, "beta")
Social.KK.Effects[i,c("(focalID)")] <- getME(isSocialKK, "theta")

##########################################################
##See differences between sex
##########################################################  

# Males
ExSubScansM = ExSubScans[which(ExSubScans$sex=="M"),]

isNotAloneM <- glmer(isProx~ isPost + group + age + percentrank+ (1|focalID) , data = ExSubScansM, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneM)
# simres1 <- simulateResiduals(isNotAlone, n = 1000)
# testResiduals(simres1)
# performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
NotAlone.M.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isNotAloneM, "beta")
NotAlone.M.Effects[i,c("(focalID)")] <- getME(isNotAloneM, "theta")

isSocialM <- glmer(isSocial~ isPost + group + age + percentrank+ (1|focalID) , data = ExSubScansM, family = binomial)
summary(isSocialM)
# simres2 <- simulateResiduals(isSocial, n = 1000)
# testResiduals(simres2)
# performance::check_model(isSocial)
Social.M.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isSocialM, "beta")
Social.M.Effects[i,c("(focalID)")] <- getME(isSocialM, "theta")

##########################################################

# Females
ExSubScansF = ExSubScans[which(ExSubScans$sex=="F"),]

isNotAloneF <- glmer(isProx~ isPost + group + age + percentrank+ (1|focalID) , data = ExSubScansF, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneF)
# simres1 <- simulateResiduals(isNotAlone, n = 1000)
# testResiduals(simres1)
# performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
NotAlone.F.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isNotAloneF, "beta")
NotAlone.F.Effects[i,c("(focalID)")] <- getME(isNotAloneF, "theta")

isSocialF <- glmer(isSocial~ isPost + group + age + percentrank + (1|focalID) , data = ExSubScansF, family = binomial)
summary(isSocialF)
# simres2 <- simulateResiduals(isSocial, n = 1000)
# testResiduals(simres2)
# performance::check_model(isSocial)
Social.F.Effects[i,c("(Intercept)","isPost","groupV","age","rank")] <- getME(isSocialF, "beta")
Social.F.Effects[i,c("(focalID)")] <- getME(isSocialF, "theta")

##########################################################

#For Both Sex
isNotAloneS <- glmer(isProx~ isPost*sex + group + age + percentrank + (1|focalID) , data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneS)
# simres1 <- simulateResiduals(isNotAlone, n = 1000)
# testResiduals(simres1)
# performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
NotAlone.S.Effects[i,c("(Intercept)","isPost","sexM","groupV","age","rank","isPost:sexM")] <- getME(isNotAloneS, "beta")
NotAlone.S.Effects[i,c("(focalID)")] <- getME(isNotAloneS, "theta")

isSocialS <- glmer(isSocial~ isPost*sex + group + age + percentrank + (1|focalID) , data = ExSubScans, family = binomial)
summary(isSocialS)
# simres2 <- simulateResiduals(isSocial, n = 1000)
# testResiduals(simres2)
# performance::check_model(isSocial)
Social.S.Effects[i,c("(Intercept)","isPost","sexM","groupV","age","rank","isPost:sexM")] <- getME(isSocialS, "beta")
Social.S.Effects[i,c("(focalID)")] <- getME(isSocialS, "theta")

##########################################################
##See differences between quarter
##########################################################  

isNotAloneQ <- glmer(isProx~ isPost*Q + sex + group + age + percentrank + (1|focalID) , data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneQ)
# simres1 <- simulateResiduals(isNotAlone, n = 1000)
# testResiduals(simres1)
# performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
NotAlone.Q.Effects[i,c("(Intercept)","isPost","Q","sexM","groupV","age","rank","isPost:Q")] <- getME(isNotAloneQ, "beta")
NotAlone.Q.Effects[i,c("(focalID)")] <- getME(isNotAloneQ, "theta")

isSocialQ <- glmer(isSocial~ isPost*Q + sex + group + age + percentrank + (1|focalID) , data = ExSubScans, family = binomial)
summary(isSocialQ)
# simres2 <- simulateResiduals(isSocial, n = 1000)
# testResiduals(simres2)
# performance::check_model(isSocial)
Social.Q.Effects[i,c("(Intercept)","isPost","Q","sexM","groupV","age","rank","isPost:Q")] <- getME(isSocialQ, "beta")
Social.Q.Effects[i,c("(focalID)")] <- getME(isSocialQ, "theta")

##########################################################
##See differences between rank
##########################################################  

isNotAloneR <- glmer(isProx~ isPost*percentrank + sex + group + age + (1|focalID) , data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneR)
# simres1 <- simulateResiduals(isNotAlone, n = 1000)
# testResiduals(simres1)
# performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
NotAlone.R.Effects[i,c("(Intercept)","isPost","rank","sexM","groupV","age","isPost:Rank")] <- getME(isNotAloneR, "beta")
NotAlone.R.Effects[i,c("(focalID)")] <- getME(isNotAloneR, "theta")

isSocialR <- glmer(isSocial~ isPost*percentrank + sex + group + age + (1|focalID) , data = ExSubScans, family = binomial)
summary(isSocialR)
# simres2 <- simulateResiduals(isSocial, n = 1000)
# testResiduals(simres2)
# performance::check_model(isSocial)
Social.R.Effects[i,c("(Intercept)","isPost","rank","sexM","groupV","age","isPost:Rank")] <- getME(isSocialR, "beta")
Social.R.Effects[i,c("(focalID)")] <- getME(isSocialR, "theta")

##########################################################
##See differences between age
##########################################################

isNotAloneA <- glmer(isProx~ isPost*age + sex + group + percentrank + (1|focalID) , data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneA)
# simres1 <- simulateResiduals(isNotAlone, n = 1000)
# testResiduals(simres1)
# performance::check_model(isNotAlone) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
NotAlone.A.Effects[i,c("(Intercept)","isPost","age","sexM","groupV","rank","isPost:age")] <- getME(isNotAloneA, "beta")
NotAlone.A.Effects[i,c("(focalID)")] <- getME(isNotAloneA, "theta")

isSocialA <- glmer(isSocial~ isPost*age + sex + group + percentrank + (1|focalID) , data = ExSubScans, family = binomial)
summary(isSocialA)
# simres2 <- simulateResiduals(isSocial, n = 1000)
# testResiduals(simres2)
# performance::check_model(isSocial)
Social.A.Effects[i,c("(Intercept)","isPost","age","sexM","groupV","rank","isPost:age")] <- getME(isSocialA, "beta")
Social.A.Effects[i,c("(focalID)")] <- getME(isSocialA, "theta") 


save.image("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/ModelEffects_AllData.RData")


end_time <- Sys.time()
end_time - start_time
