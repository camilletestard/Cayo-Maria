#Modeling Global Network Metrics


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

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/AllStats.RData")

##########################################################
#Pooling all data together
##########################################################
group = c("V", "V", "V", "K", "K")
years = c(2015, 2016, 2017, 2015, 2017)
groupyear = c("V.2015","V.2016","V.2017","KK.2015","KK.2017"); gy=1
for (gy in 1:length(groupyear)){ #For each group
  name.0 = paste(groupyear[gy],".0",sep="")
  AllStats[[name.0]]$isPost = 0
  AllStats[[name.0]]$group = group[gy]
  AllStats[[name.0]]$year = years[gy]
  name.1 = paste(groupyear[gy],".1",sep="")
  AllStats[[name.1]]$isPost = 1
  AllStats[[name.1]]$group = group[gy]
  AllStats[[name.1]]$year = years[gy]
}

#Pooling data from multiple years: 
PooledData = rbindlist(AllStats); PooledData$groupyear = paste(PooledData$group,PooledData$year,sep="")
data.groom = PooledData[PooledData$`actions[a]`=="groom",]
data.prox = PooledData[PooledData$`actions[a]`=="prox",]

# #Check distribution of dependent variables.
# hist(data.groom$dens, breaks = 40); #Histogram
# ggdensity(data.groom$dens, main = "Distribution of social network density", xlab = "Social Network Density"); #density plot to show distribution
# ggqqplot(data.groom$dens); #qqplot to check how much DV deviates from the  normal assumptions
# shapiro.test(data.groom$dens) #Normality test of the DV.
# descdist(data.groom$dens) #Function to determine the most appropriate GLMM to model this DV
# hist(PooledData$eo.FF, breaks = 40); ggdensity(log(PooledData$eo.FF), main = "Distribution of social network density", xlab = "O/E female pairs");
# ggqqplot(PooledData$eo.FF); shapiro.test(PooledData$eo.FF)
# descdist(PooledData$dens)

###########################################################
#Model GROOMING Network Properties
###########################################################

#Testing density preferences
densityModel <- glmmTMB(dens~ isPost + (1|groupyear), data = data.groom, beta_family(link = "logit")) #Using beta family model because density = proportion
summary(densityModel) #summary of model
performance::check_model(densityModel) #Check model assumptions visually (method 1)
simres <- simulateResiduals(fittedModel = densityModel, n = 250) #another method to check for model assumptions (normality of residuals, outlier test)
testResiduals(simres) #statistical tests of model assumptions for method 2
plot(effects::allEffects(densityModel)) #plot fixed effects
sjPlot::plot_model(densityModel, type="re", vline.color = "black") # plot random effects

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/GlobalNetworkMetrics/Groom") 
export_summs(densityModel, model.names = c("Groom.Density"), digits=3, #Save & export model summary into a nice format
             to.file = "docx", file.name = "Groom.DensityModel.docx")

##########################
#Testing sex preferences
eo.FF.model <-  glmmTMB(eo.FF~ isPost + group + (1|year), data = data.groom, family = Gamma(link = "log")) 
summary(eo.FF.model)
# performance::check_model(eo.FF.model)
            

eo.MM.model <-  glmmTMB(eo.MM~ isPost + group + (1|year), data = data.groom, family = Gamma(link = "log")) 
summary(eo.MM.model)
# performance::check_model(eo.MM.model)
# simres <- simulateResiduals(fittedModel = eo.MM.model, n = 250)
# testResiduals(simres)

eo.cross.model <-  glmmTMB(eo.cross~ isPost + group + (1|year), data = data.groom, family = Gamma(link = "log")) 
summary(eo.cross.model)
# performance::check_model(eo.cross.model)
# simres <- simulateResiduals(fittedModel = eo.cross.model, n = 250)
# testResiduals(simres)

export_summs(eo.FF.model, eo.MM.model, eo.cross.model, model.names = c("eo.FF", "eo.MM", "eo.cross"),
             to.file = "docx", file.name = "Groom.SexPrefModel.docx")

##########################
#Testing kin preferences
eo.ck.model <-  glmmTMB(eo.ck~ isPost + (1|groupyear), data = data.groom, family = Gamma(link = "log")) 
summary(eo.ck.model)
# performance::check_model(eo.ck.model)

# eo.dk.model <-  glmmTMB(eo.dk~ isPost + (1|groupyear), data = data.groom, family = Gamma(link = "log")) 
# summary(eo.dkmodel)
# performance::check_model(eo.dkmodel)
#Note: Comment for now because we get negative values, which is not normal...(ratio of two positive values)

eo.u.model <-  glmmTMB(eo.u~ isPost + (1|groupyear), data = data.groom, family = Gamma(link = "log")) 
summary(eo.u.model)
# performance::check_model(eo.u.model)

export_summs(eo.ck.model, eo.u.model, model.names = c("eo.ck", "eo.u"),
             to.file = "docx", file.name = "Groom.KinPrefModel.docx")

##########################
#Testing rank preferences
eo.HH.model <-  glmmTMB(eo.HH~ isPost + group + (1|year), data = data.groom, family = Gamma(link = "log")) 
# eo.HH.model <-  lmer(eo.HH~ isPost + group + (1|year), data = data.groom) 
summary(eo.HH.model)
# performance::check_model(eo.HH.model)

eo.LL.model <-  glmmTMB(eo.LL~ isPost + group + (1|year), data = data.groom, family = Gamma(link = "log")) 
# eo.LL.model <-  lmer(eo.LL~ isPost + group + (1|year), data = data.groom) 
summary(eo.LL.model)
performance::check_model(eo.LL.model)

eo.crossR.model <-  glmmTMB(eo.crossR~ isPost + group + (1|year), data = data.groom, family = Gamma(link = "log")) 
summary(eo.crossR.model)
performance::check_model(eo.crossR.model)

export_summs(eo.HH.model, eo.LL.model, eo.crossR.model, model.names = c("eo.HH", "eo.LL", "eo.crossR"),
             to.file = "docx", file.name = "Groom.RankPrefModel.docx")

###########################################################
#Model PROXIMITY Network Properties
###########################################################

#Testing density preferences
densityModel <- glmmTMB(dens~ isPost + (1|groupyear), data = data.prox, beta_family(link = "logit")) #Using beta family model because density = proportion
summary(densityModel) #summary of model
performance::check_model(densityModel) #Check model assumptions visually (method 1)
simres <- simulateResiduals(fittedModel = densityModel, n = 250) #another method to check for model assumptions (normality of residuals, outlier test)
testResiduals(simres) #statistical tests of model assumptions for method 2
plot(effects::allEffects(densityModel)) #plot fixed effects
sjPlot::plot_model(densityModel, type="re", vline.color = "black") # plot random effects

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/GlobalNetworkMetrics/Proximity") 
export_summs(densityModel, model.names = c("Prox.Density"), digits=3, #Save & export model summary into a nice format
             to.file = "docx", file.name = "Prox.DensityModel.docx")

##########################
#Testing sex preferences
eo.FF.model <-  glmmTMB(eo.FF~ isPost + group + (1|year), data = data.prox, family = Gamma(link = "log")) 
summary(eo.FF.model)
# performance::check_model(eo.FF.model)


eo.MM.model <-  glmmTMB(eo.MM~ isPost + group + (1|year), data = data.prox, family = Gamma(link = "log")) 
summary(eo.MM.model)
# performance::check_model(eo.MM.model)
# simres <- simulateResiduals(fittedModel = eo.MM.model, n = 250)
# testResiduals(simres)

eo.cross.model <-  glmmTMB(eo.cross~ isPost + group + (1|year), data = data.prox, family = Gamma(link = "log")) 
summary(eo.cross.model)
# performance::check_model(eo.cross.model)
# simres <- simulateResiduals(fittedModel = eo.cross.model, n = 250)
# testResiduals(simres)

export_summs(eo.FF.model, eo.MM.model, eo.cross.model, model.names = c("eo.FF", "eo.MM", "eo.cross"),
             to.file = "docx", file.name = "Prox.SexPrefModel.docx")

##########################
#Testing kin preferences
eo.ck.model <-  glmmTMB(eo.ck~ isPost + (1|groupyear), data = data.prox, family = Gamma(link = "log")) 
summary(eo.ck.model)
# performance::check_model(eo.ck.model)

# eo.dk.model <-  glmmTMB(eo.dk~ isPost + (1|groupyear), data = data.prox, family = Gamma(link = "log")) 
# summary(eo.dkmodel)
# performance::check_model(eo.dkmodel)
#Note: Comment for now because we get negative values, which is not normal...(ratio of two positive values)

eo.u.model <-  glmmTMB(eo.u~ isPost + (1|groupyear), data = data.prox, family = Gamma(link = "log")) 
summary(eo.u.model)
# performance::check_model(eo.u.model)

export_summs(eo.ck.model, eo.u.model, model.names = c("eo.ck", "eo.u"),
             to.file = "docx", file.name = "Prox.KinPrefModel.docx")

##########################
#Testing rank preferences
eo.HH.model <-  glmmTMB(eo.HH~ isPost + group + (1|year), data = data.prox, family = Gamma(link = "log")) 
# eo.HH.model <-  lmer(eo.HH~ isPost + group + (1|year), data = data.prox) 
summary(eo.HH.model)
# performance::check_model(eo.HH.model)

eo.LL.model <-  glmmTMB(eo.LL~ isPost + group + (1|year), data = data.prox, family = Gamma(link = "log")) 
# eo.LL.model <-  lmer(eo.LL~ isPost + group + (1|year), data = data.prox) 
summary(eo.LL.model)
# performance::check_model(eo.LL.model)

eo.crossR.model <-  glmmTMB(eo.crossR~ isPost + group + (1|year), data = data.prox, family = Gamma(link = "log")) 
summary(eo.crossR.model)
# performance::check_model(eo.crossR.model)

export_summs(eo.HH.model, eo.LL.model, eo.crossR.model, model.names = c("eo.HH", "eo.LL", "eo.crossR"),
             to.file = "docx", file.name = "Prox.RankPrefModel.docx")
