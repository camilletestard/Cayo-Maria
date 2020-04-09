#Modeling Global Network Metrics


library(lme4)# Generalized Linear Mixed Models
library(MCMCglmm)# Generalized Linear Mixed Models, other package
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(jtools)
library(data.table)

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AllStatsGroom.RData")
AllStats[["KK.2013.1"]]= NULL
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 

##########################################################
#Pooling all data together
##########################################################

groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017")
for (gy in 1:length(groupyear)){ #For each group
  name.0 = paste(groupyear[gy],".0",sep="")
  AllStats[[name.0]]$isPost = 0
  AllStats[[name.0]]$groupyear = groupyear[gy]
  name.1 = paste(groupyear[gy],".1",sep="")
  AllStats[[name.1]]$isPost = 1
  AllStats[[name.1]]$groupyear = groupyear[gy]
}

#Pooling data from multiple years: 
PooledData = rbindlist(AllStats)
hist(PooledData$dens, breaks = 20)
hist(PooledData$eo.FF, breaks = 20)

#Testing density preferences

densityModel <- lmer(dens~ isPost + (1|groupyear), data = PooledData) 
summ(densityModel, digits = 3)
performance::check_model(densityModel) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
plot(effects::allEffects(densityModel))
sjPlot::plot_model(densityModel, type="re", vline.color = "black")

export_summs(densityModel, model.names = c("Density"), digits=3,
             to.file = "docx", file.name = "densityModel.docx")

#Testing sex preferences
eo.FF.model <-  lmer(eo.FF~ isPost + (1|groupyear), data = PooledData) 
summ(eo.FF.model)
performance::check_model(eo.FF.model)

eo.MM.model <-  lmer(eo.MM~ isPost + (1|groupyear), data = PooledData) 
summary(eo.MM.model)
performance::check_model(eo.MM.model)

eo.cross.model <-  lmer(eo.cross~ isPost + (1|groupyear), data = PooledData) 
summary(eo.cross.model)
performance::check_model(eo.cross.model)

export_summs(eo.FF.model, eo.MM.model, eo.cross.model, model.names = c("eo.FF", "eo.MM", "eo.cross"),
             to.file = "docx", file.name = "SexPrefModel.docx")

#Testing kin preferences
eo.ck.model <-  lmer(eo.ck~ isPost + (1|groupyear), data = PooledData) 
summ(eo.ck.model)
performance::check_model(eo.ck.model)

eo.dk.model <-  lmer(eo.dk~ isPost + (1|groupyear), data = PooledData) 
summary(eo.dkmodel)
performance::check_model(eo.dkmodel)

eo.u.model <-  lmer(eo.u~ isPost + (1|groupyear), data = PooledData) 
summary(eo.u.model)
performance::check_model(eo.u.model)

export_summs(eo.ck.model, eo.dk.model, eo.u.model, model.names = c("eo.ck", "eo.dk", "eo.u"),
             to.file = "docx", file.name = "KinPrefModel.docx")

#Testing rank preferences
eo.HH.model <-  lmer(eo.HH~ isPost + (1|groupyear), data = PooledData) 
summ(eo.HH.model)
performance::check_model(eo.HH.model)

eo.LL.model <-  lmer(eo.LL~ isPost + (1|groupyear), data = PooledData) 
summary(eo.LLmodel)
performance::check_model(eo.LLmodel)

eo.crossR.model <-  lmer(eo.crossR~ isPost + (1|groupyear), data = PooledData) 
summary(eo.crossR.model)
performance::check_model(eo.crossR.model)

export_summs(eo.HH.model, eo.LL.model, eo.crossR.model, model.names = c("eo.HH", "eo.LL", "eo.crossR"),
             to.file = "docx", file.name = "RankPrefModel.docx")
