######## Modelling Logistic Regressions on ALl Data

#Note: All data include all observations from individuals that were present both before and after the hurricane. 
# Excludes individuals that are not present either pre-hurricane or post-hurricane

library(lme4)# Generalized Linear Mixed Models
library(MCMCglmm)# Generalized Linear Mixed Models, other package
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)
library(effects)
library(sjPlot)
library(sjlabelled)
library(sjmisc)

#Load AllScans file
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#Select groups of interest
PrePostScans =  allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
SubScans=PrePostScans[-which(PrePostScans$year==2019),] #remove 2019
unqIDs = as.character(unique(SubScans$focalID))

#Exclude individuals that are not present either pre-hurricane or post-hurricane
for (id in 1:length(unqIDs)){ #For all individuals
  
  if (length(which(SubScans$focalID == unqIDs[id] & SubScans$isPost == 0))==0 
      | length(which(SubScans$focalID == unqIDs[id] & SubScans$isPost == 1))==0) {
    
    SubScans=SubScans[-which(SubScans$focalID == unqIDs[id]),]
    
  }
}
ExSubScans=SubScans
unqIDs = as.character(unique(ExSubScans$focalID))

#Demographics
demographics = data.frame(); id=1
for (id in 1:length(unqIDs)){
  demographics[id,"id"]=unqIDs[id]
  idx=which(!is.na(match(ExSubScans$focalID,unqIDs[id])))
  demographics[id,"sex"]=ExSubScans$sex[idx[1]]
  demographics[id,"group"]=ExSubScans$group[idx[1]]
}
table(demographics$sex,demographics$group)

# full.sample.table = table(as.character(ExSubScans$focalID),ExSubScans$isPost)

#Scale parameters
ExSubScans[,"age"] <- scale(ExSubScans[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
ExSubScans[,"percentrank"] <- ExSubScans[,"percentrank"]/100
ExSubScans$isSocial=as.factor(ExSubScans$isSocial)
ExSubScans$isProx=as.factor(ExSubScans$isProx)
ExSubScans$isPost=as.factor(ExSubScans$isPost)
ExSubScans$year = as.factor(ExSubScans$year)

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPSoc/AllData")

#################
#All Groups

#Proximity Model
isNotAlone <- glmer(isProx~ isPost*Q + sex + age + percentrank + timeBlock + group + isPost:group + (1|focalID), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
# check_model(isNotAlone)
summary(isNotAlone)
export_summs(isNotAlone, model.names = c("Full Model"), digits=3,error_format="[{conf.low}, {conf.high}]", error_pos="right",
             to.file = "docx", file.name = "isNotAloneFULL.docx")

#Grooming model
isSocial <- glmer(isSocial~ isPost*Q + sex + age + percentrank + timeBlock + group + isPost:group + (1|focalID), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isSocial)
export_summs(isSocial, model.names = c("Full Model"), digits=3,error_format="[{conf.low}, {conf.high}]", error_pos="right",
             to.file = "docx", file.name = "isSocialFULL.docx")

#################
#Group V
ExSubScansV = ExSubScans[which(ExSubScans$group=='V'),]

#Proximity Model
isNotAloneV <- glmer(isProx~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID), data = ExSubScansV, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneV)
export_summs(isNotAloneV, model.names = c("V.Model"), digits=3,error_format="[{conf.low}, {conf.high}]", error_pos="right",
             to.file = "docx", file.name = "isNotAloneV.docx")

# plot(predictorEffect("isPost", isNotAloneV))
# effect("isPost",isNotAloneV)

#Grooming model
isSocialV <- glmer(isSocial~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID), data = ExSubScansV, family = binomial)
summary(isSocialV)

# plot(predictorEffect("isPost", isSocialV))
# effect("isPost",isSocialV)
# plot_model(isSocialV)

# #############
# #Simone's code:
# as.data.frame(effects::allEffects(isSocialV))$isPost
# 
# df2 = expand.grid(isPost = unique(ExSubScansV$isPost),
#                   focalID = unique(ExSubScansV$focalID),
#                   timeBLock=unique(ExSubScansV$timeBlock),
#                   Q=unique(ExSubScansV$Q),
#                   age = mean(ExSubScansV$age),
#                   percentrank = mean(ExSubScansV$percentrank),
# )
# df1$fit = predict(fit1, type = "response", newdata = df1)
# df1 <- df1 %>% 
#   group_by(RV) %>% 
#   summarise(fit=mean(fit))
# 
# data_regression %>% 
#   Rmisc::summarySEwithin(., measurevar = "response", withinvars = "RV") %>% 
#   mutate(RV=as.numeric(as.character(RV))) %>% 
#   ggplot() +
#   geom_line(data = df1, aes(RV, fit), size = 1, color="navy") +
#   geom_pointrange(aes(RV, response, ymin=response-se, ymax=response+se), color="navy", size=1) +
#   labs(x="Item Difference Subjective Value", y="P(Right Chosen)") +
#   scale_x_continuous(breaks = -3:3) +
#   theme_pubr() +
#   theme
# #############

export_summs(isSocialV, model.names = c("V.Model"), digits=3,error_format="[{conf.low}, {conf.high}]", error_pos="right",
             to.file = "docx", file.name = "isSocialV.docx")

#################
#Group KK
ExSubScansKK = ExSubScans[which(ExSubScans$group=='KK'),]

#Proximity Model
isNotAloneKK <- glmer(isProx~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID), data = ExSubScansKK, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneKK)
export_summs(isNotAloneKK, model.names = c("KK.Model"), digits=3,error_format="[{conf.low}, {conf.high}]", error_pos="right",
             to.file = "docx", file.name = "isNotAloneKK.docx")

#Grooming Model
isSocialKK <- glmer(isSocial~ isPost*Q + sex + age + percentrank + timeBlock + (1|focalID) , data = ExSubScansKK, family = binomial)
summary(isSocialKK)
export_summs(isSocialKK, model.names = c("KK.Model"), digits=3,error_format="[{conf.low}, {conf.high}]", error_pos="right",
             to.file = "docx", file.name = "isSocialKK.docx")

save.image("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ModelEffects_AllData.RData")


end_time <- Sys.time()
end_time - start_time
