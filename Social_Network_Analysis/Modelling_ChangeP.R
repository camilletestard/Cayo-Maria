######## Modelling Logistic Regressions

library(lme4)# Generalized Linear Mixed Models
#library(lmerTest)
library(performance)
library(sjPlot)
library(jtools)
#library(glmmTMB)# Generalized Linear Mixed Models, other package
#library(MCMCglmm)# Generalized Linear Mixed Models, other package
#library(bbmle)#Tools for General Maximum Likelihood Estimation
#library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models

#Load data
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("Social_Network_Analysis/SocialCapital.dSocialRates6.RData")
#Format data correclty
SocialCapital.ALL$sex = as.factor(SocialCapital.ALL$sex); SocialCapital.ALL$age = as.numeric(SocialCapital.ALL$age)
SocialCapital.ALL$group = as.factor(SocialCapital.ALL$group);SocialCapital.ALL$percentrank = as.numeric(SocialCapital.ALL$percentrank)/10
SocialCapital.ALL$year = as.factor(SocialCapital.ALL$year);
data = SocialCapital.ALL[-which(is.na(SocialCapital.ALL$dpAcc)),] #remove NA
#data$ordrank2 = "L"; data$ordrank2[which(data$percentrank > 70)]="H"
data$isSocialIncrease = 0; data$isSocialIncrease[which(data$dpSocial>0)]=1
data$isSocialIncrease = as.factor(data$isSocialIncrease)

#check distribution of independent variables
hist(data$dpSoc,col=rgb(0,1,1,0.5), breaks=20,main="Change p(Acc/Soc) pre- to post-hurricane",xlab="Change p(Acc/Soc)", xlim=c(-0.5,1))
hist(data$dpAcc,col=rgb(1,0,0,0.5), breaks=40,add=T)
abline(v=0, col="red", lwd=4, lty=2)
box()
legend("topright", c("dp(Acc)", "dp(Soc)"), fill=c("red", "cyan"))

#Scale parameters: 
data[,c("age","numPartners")] <- scale(data[,c("age","numPartners")])

###########################################################
## Model Social Capital effect on change in proximity rates
###########################################################

#Modelling change in p(Acc)
dpAcc1 <- lmer(dpAcc~ sex + age + group + percentrank + (1|id), data = data)
summary(dpAcc1)
performance::check_model(dpAcc1)

dpAcc2 <- lmer(dpAcc~ sex + age + group + percentrank + DSI + numPartners + (1|id), data = data, na.action=na.omit)
summary(dpAcc2)
performance::check_model(dpAcc2)

dpAcc3 <- lmer(dpAcc~ sex + age + group + percentrank + vig.I + sdb.I + (1|id), data = data, na.action=na.omit)
summary(dpAcc3)
performance::check_model(dpAcc3)

dpAcc4 <- lmer(dpAcc~ sex + age + group + percentrank + eig.cent + between + (1|id), data = data, na.action=na.omit)
summary(dpAcc4)
performance::check_model(dpAcc4)

dpAcc5 <- lmer(dpAcc~ sex + age + group + percentrank + DSI + numPartners + vig.I + sdb.I + eig.cent + between + (1|id), data = data, na.action=na.omit)
summary(dpAcc5)
performance::check_model(dpAcc5)
export_summs(dpAcc5, digits = 3, to.file = "docx", file.name = "ModelingChangePAccAll.docx")

export_summs(dpAcc1, dpAcc2, dpAcc3, dpAcc4, model.names = c("Base Model", "NeedForChange", "NeedForChange2", "CapacityForChange"),
             to.file = "docx", file.name = "ModelingChangePAcc.docx")

# Combining all paramters, the model fails to converge. So I will run 3 models separately
# dpAcc4 <- lmer(dpAcc~ sex + age + group + percentrank + GroomIN + GroomOUT + AggIN + AggOUT + vig.ra + sdb.ra + (1|id) + (1|year), data = data, na.action=na.omit)
# summary(dpAcc4)
# performance::check_model(dpAcc4)
# tab_model(dpAcc3, digits = 3)

###########################################################
## Model Social Capital effect on change in grooming rates
###########################################################

#Modelling change in p(Social)
dpSocial1 <- lmer(dpSocial~ sex + age +group + percentrank + (1|id), data = data, na.action=na.omit)
summary(dpSocial1)
#Plot effects seperately
plot(effects::allEffects(dpSocial1))
sjPlot::plot_model(dpSocial1, type="re", vline.color = "black")
#Test assumption and output table3
performance::check_model(dpSocial1)

dpSocial2 <- lmer(dpSocial~ sex + age + group + percentrank + DSI + numPartners + (1|id), data = data, na.action=na.omit)
summary(dpSocial2)
performance::check_model(dpSocial2)

dpSocial3 <- lmer(dpSocial~ sex + age + group + percentrank + vig.I + sdb.I + (1|id), data = data, na.action=na.omit)
summary(dpSocial3)
performance::check_model(dpSocial3)

dpSocial4 <- lmer(dpSocial~ sex + age + group + percentrank + eig.cent + between + (1|id), data = data, na.action=na.omit)
summary(dpSocial4)
performance::check_model(dpSocial4)

dpSocial5 <- lmer(dpSocial~ sex + age + group + percentrank + DSI + numPartners + vig.I + sdb.I + eig.cent + between + (1|id), data = data, na.action=na.omit)
summary(dpSocial5)
performance::check_model(dpSocial5)
export_summs(dpSocial5, digits = 3, to.file = "docx", file.name = "ModelingChangePSocAll.docx")

export_summs(dpSocial1, dpSocial2, dpSocial3, dpSocial4, model.names = c("Base Model", "NeedForChange", "NeedForChange2", "CapacityForChange"),
             to.file = "docx", file.name = "ModelingChangePSoc.docx")


#Model Whether there was an increase or a decrease in grooming rates
isSocialIncrease <- glmer(isSocialIncrease ~ sex + age + group + percentrank + (1|id) + (1|year), data = data, na.action=na.omit, family = binomial)
summary(isSocialIncrease)
performance::check_model(isSocialIncrease)
tab_model(isSocialIncrease, digits = 3, transform = NULL)

