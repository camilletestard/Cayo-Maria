######## Modelling Logistic Regressions

library(lme4)# Generalized Linear Mixed Models
#library(lmerTest)
library(performance)
library(sjPlot)
library(jtools)
library(ggplot2)
library(dplyr)
#library(glmmTMB)# Generalized Linear Mixed Models, other package
#library(MCMCglmm)# Generalized Linear Mixed Models, other package
#library(bbmle)#Tools for General Maximum Likelihood Estimation
#library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models

#Load data
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
source("Social_Network_Analysis/CalcSubsampledScans.R")
load("Social_Network_Analysis/SocialCapital.RData")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")

#Initilize structures:
ChangeProx.BM = data.frame(); ChangeGroom.BM = data.frame()
ChangeProx.NFC = data.frame(); ChangeGroom.NFC = data.frame()
ChangeProx.CFC = data.frame(); ChangeGroom.CFC = data.frame()
ChangeProx.ALL = data.frame(); ChangeGroom.ALL = data.frame()


#Format data
SocialCapital.ALL$id = as.character(SocialCapital.ALL$id); 
SocialCapital.ALL$sex = as.factor(SocialCapital.ALL$sex); 
SocialCapital.ALL$age = as.numeric(SocialCapital.ALL$age)
SocialCapital.ALL$group = as.factor(SocialCapital.ALL$group)
SocialCapital.ALL$percentrank = as.numeric(SocialCapital.ALL$percentrank)/100
SocialCapital.ALL$year = as.factor(SocialCapital.ALL$year)

#Set parameters: 
num_iter = 50; iter =1
only2017=F; #if only considering 2017 (year just prior hurricane). Note: there is  no V2017 valid dp(groom) & dp(prox) because they all have less than 20 obs.
group = c("KK","KK","V", "V", "V")
years = c(2015,2017,2015,2016,2017)
groupyears = c("KK2015", "KK2017","V2015", "V2016", "V2017")

for (iter in 1:num_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  #####################################################################
  # 1. Compute change in p(Acc) and p(Social), per individual, per year
  #####################################################################

  #Calculate random subsamples
  randomScans = calcRandomScans(allScans)
  dprob.ALL = data.frame(); gy=1
  for (gy in 1:length(groupyears)){
    
    rscans = randomScans[which(randomScans$year == years[gy] & randomScans$group == group[gy]),]
    #Load data
    setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned")
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
    
    unqIDs = as.character(meta_data$id)
    dprob=data.frame(matrix(NA, nrow=length(unqIDs),ncol=4)); colnames(dprob)=c("id","dpAcc","dpSocial","num_obs")
    for (id in 1:length(unqIDs)){ #For all individuals
      isProx.pre = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isProx.post = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      isSocial.pre = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isSocial.post = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      dpAcc=NA; dpSocial=NA; num_obs = length(isProx.pre)
      if (length(isProx.pre)>=20) { #If there are more than 10 observations for that individual
        pACC.pre = sum(isProx.pre)/length(isProx.pre)
        pACC.post = sum(isProx.post)/length(isProx.post)
        dpAcc = pACC.post - pACC.pre
        pSocial.pre = sum(isSocial.pre)/length(isSocial.pre)
        pSocial.post = sum(isSocial.post)/length(isSocial.post)
        dpSocial = pSocial.post - pSocial.pre
      } #end of min obs clause
      dprob[id,]=c(unqIDs[id],dpAcc,dpSocial,num_obs)
    } #end of id for loop
    dprob$group = group[gy]; dprob$year = years[gy]
    dprob.ALL = rbind(dprob.ALL, dprob)
  } #end of groupyear for loop

  dprob.ALL$dpAcc=as.numeric(dprob.ALL$dpAcc)
  dprob.ALL$dpSocial=as.numeric(dprob.ALL$dpSocial)
  
  #####################################################################
  # 2. Merge dprob and social Capital data &clean
  #####################################################################
  data=bind_cols(SocialCapital.ALL, dprob.ALL) #combine dprob and social capital (should be in exactly the same row order (by id))
  if(all_equal(data$id,data$id1)){data$id1=NULL} #remove second id column (redundant)
  
  data = data[-which(is.na(data$dpAcc)),] #remove NA
  #data$ordrank2 = "L"; data$ordrank2[which(data$percentrank > 70)]="H" #change ord rank to have two levels only
  #Add "isIncrease" factor for grooming. If we simply want to model whether an indvidual increased its grooming or not (binary).
  data$isSocialIncrease = 0; data$isSocialIncrease[which(data$dpSocial>0)]=1
  data$isSocialIncrease = as.factor(data$isSocialIncrease)
  
  #Scale parameters: 
  data[,c("age")] <- scale(data[,c("age")])
  
  #####################################################################
  # 3. Visualizations
  #####################################################################
  
  #check distribution of independent variables
  # tiff("ChangeProb.tiff",units="in", width=7, height=4, res=300, compression = 'lzw')
  hist(data$dpSocial,col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox/Groom) pre- to post-hurricane",xlab="Change p(Prox/Groom)", xlim=c(-0.5,1))
  hist(data$dpAcc,col=rgb(1,0,0,0.5), breaks=30,add=T)
  segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
  box()
  legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
  # dev.off()
  
  #Visualize how baseline p(groom) is related to post-disaster change in p(groom)
  preGroom <- ggplot(data, aes(x=DSIgroom, y=dpSocial))+
    # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
    # geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
    geom_point(alpha = 0.25, size = 3) + 
    geom_smooth(method=lm, color=rgb(1,0,0))+
    ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming level")+
    xlab("Pre-disaster Grooming")+
    ylab("Change in Grooming post-disaster (standardized)")+
    facet_grid(~sex)
  cor.test(data$dpSocial,data$DSIgroom) #test correlation
  # tiff("preGroom.tiff",units="in", width=7, height=4, res=300, compression = 'lzw'); preGroom; dev.off() #save plot
  
  #Visualize how age is related to post-disaster change in p(groom)
  ggplot(data, aes(x=age, y=dpSocial))+
    geom_point(alpha = 0.25) + 
    geom_smooth(method=lm, color='#2C3E50')+
    ggtitle("Post-disaster change in grooming as a function of age")
  cor.test(data$dpSocial,data$age) #test correlation
  
  #Visualize how sex is related to post-disaster change in p(groom)
  ggplot(data, aes(x=sex, y=dpSocial,fill=sex))+
    geom_boxplot(aes(group=sex),alpha = 0.75, col="grey")+
    geom_jitter(position = position_jitter(0.2), alpha = 0.2)
  
  #Visualize how number of partner is related to post-disaster change in p(groom)
  numP.groom <- ggplot(data, aes(x=numPartnersGroom, y=dpSocial))+
    # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
    geom_point(alpha = 0.25, size = 3) + 
    geom_smooth(method=lm, color=rgb(1,0,0))+
    ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming level")+
    xlab("Pre-disaster number of grooming partners (standardized)")+
    ylab("Change in Grooming post-disaster")+
    facet_grid(~sex)
  cor.test(data$dpSocial,data$numPartnersGroom) #test correlation
  # tiff("numP.groom.tiff",units="in", width=7, height=4, res=300, compression = 'lzw'); numP.groom; dev.off()
  
  #Visualize how eigenvector centrality is related to post-disaster change in p(groom)
  eigCent.groom <- ggplot(data, aes(x=eig.cent.groom, y=dpSocial))+
    # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
    # geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
    geom_point(alpha = 0.25, size = 3) +
    geom_smooth(method=lm, color=rgb(1,0,0))+
    ggtitle("Post-disaster change in grooming as a function of pre-disaster grooming network position")+
    xlab("Pre-disaster index of how well-connected your friends are (eigenvector centrality)")+
    ylab("Change in Grooming post-disaster")+
    facet_grid(~sex)
  cor.test(data$dpAcc,data$eig.cent.groom)
  #tiff("eigCent.prox.tiff",units="in", width=9, height=4, res=300, compression = 'lzw'); eigCent.prox; dev.off()
  
  #Visualize how clustering coeff is related to post-disaster change in p(groom)
  ClusterCoeff.groom <- ggplot(data, aes(x=clusterCoeff.groom, y=dpSocial))+
    # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
    geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
    geom_smooth(method=lm, color='#2C3E50')+
    ggtitle("Post-disaster change in proximity as a function of pre-disaster social network position")+
    xlab("Probability that your friends are friends with each other (Clustering Coefficient)")+
    ylab("Change in Proximity post-disaster")+
    facet_grid(~sex)
  cor.test(data$dpSocial,data$clusterCoeff.groom)
  # tiff("eigCent.groom.tiff",units="in", width=9, height=4, res=300, compression = 'lzw'); eigCent.prox; dev.off()
  
  ggplot(data, aes(x=between.groom, y=dpSocial))+
    # geom_point(aes(colour = sex, size = age),alpha = 0.25) + 
    geom_point(aes(colour = sex),alpha = 0.25, size = 3) + 
    geom_smooth(method=lm, color='#2C3E50')+
    ggtitle("Post-disaster change in grooming as a function of pre-disaster social network position")+
    xlab("Pre-disaster index of how well-connected your friends are (eigenvector centrality)")+
    ylab("Change in Proximity post-disaster")+
    facet_grid(~sex)
  
  ###########################################################
  # 4. Modelling change p(groom), p(prox)
  ###########################################################
  
  if (only2017){data = data[which(data$year == 2017),]}
  
  ###########################################################
  ## Model Social Capital effect on change in proximity rates 
  
  #Base Model
  dpAcc1 <- lmer(dpAcc~ sex + age + group + percentrank +  (1|year), data = data)
  summary(dpAcc1)
  # performance::check_model(dpAcc1) #test model assumptions visually
  ChangeProx.BM[iter,c("(Intercept)","sexM","age","groupV","rank")] <- getME(dpAcc1, "beta")
  ChangeProx.BM[iter,c("(focalID)","(year)")] <- getME(dpAcc1, "theta")
  
  #"Need for change" model
  dpAcc2 <- lmer(dpAcc~ sex + age + group + percentrank + DSI + numPartners +  (1|year), data = data, na.action=na.omit)
  summary(dpAcc2)
  # performance::check_model(dpAcc2)
  ChangeProx.NFC[iter,c("(Intercept)","sexM","age","groupV","rank","DSI","numP")] <- getME(dpAcc2, "beta")
  ChangeProx.NFC[iter,c("(focalID)","(year)")] <- getME(dpAcc2, "theta")
  
  # dpAcc3 <- lmer(dpAcc~ sex + age + group + percentrank + std.vig.ra +  (1|year), data = data, na.action=na.omit)
  # summary(dpAcc3)
  # performance::check_model(dpAcc3)
  
  dpAcc4 <- lmer(dpAcc~ sex + age + group + percentrank + eig.cent.prox + between.prox + clusterCoeff.prox +  (1|year), data = data, na.action=na.omit)
  summary(dpAcc4)
  # performance::check_model(dpAcc4)
  ChangeProx.CFC[iter,c("(Intercept)","sexM","age","groupV","rank","eig.cent","between","clustcoeff")] <- getME(dpAcc4, "beta")
  ChangeProx.CFC[iter,c("(focalID)","(year)")] <- getME(dpAcc4, "theta")
  
  dpAcc5 <- lmer(dpAcc~ sex + age + group + percentrank + DSI + numPartners + eig.cent.prox + between.prox + clusterCoeff.prox +  (1|year), data = data, na.action=na.omit)
  summary(dpAcc5)
  # performance::check_model(dpAcc5)
  ChangeProx.ALL[iter,c("(Intercept)","sexM","age","groupV","rank","DSI","numP","eig.cent","between","clustcoeff")] <- getME(dpAcc5, "beta")
  ChangeProx.ALL[iter,c("(focalID)","(year)")] <- getME(dpAcc5, "theta")
  
  # #Save & export  models
  # export_summs(dpAcc5, digits = 3, to.file = "docx", file.name = "ModelingChangePAccAll.AllYears.docx")
  # export_summs(dpAcc1, dpAcc2, dpAcc4, model.names = c("Base Model", "NeedForChange", "CapacityForChange"),
  #              to.file = "docx", file.name = "Modeling.ChangePAcc.AllYears.docx")
  # compare_performance(dpAcc1, dpAcc2, dpAcc4, dpAcc5)
  
  ###########################################################
  ## Model Social Capital effect on change in grooming rates
  
  #Modelling change in p(Social)
  dpSocial1 <- lmer(dpSocial~ sex + age +group + percentrank +  (1|year), data = data, na.action=na.omit)
  summary(dpSocial1)
  # #Plot effects seperately
  # plot(effects::allEffects(dpSocial1))
  # sjPlot::plot_model(dpSocial1, type="re", vline.color = "black")
  # performance::check_model(dpSocial1) #check model assumptions
  ChangeGroom.BM[iter,c("(Intercept)","sexM","age","groupV","rank")] <- getME(dpSocial1, "beta")
  ChangeGroom.BM[iter,c("(focalID)","(year)")] <- getME(dpSocial1, "theta")
  
  dpSocial2 <- lmer(dpSocial~ sex + age + group + percentrank + std.DSIgroom + std.numPartnersGroom +  (1|year), data = data, na.action=na.omit)
  summary(dpSocial2)
  # performance::check_model(dpSocial2)
  ChangeGroom.NFC[iter,c("(Intercept)","sexM","age","groupV","rank","DSI","numP")] <- getME(dpSocial2, "beta")
  ChangeGroom.NFC[iter,c("(focalID)","(year)")] <- getME(dpSocial2, "theta")
  
  # dpSocial3 <- lmer(dpSocial~ sex + age + group + percentrank + vig.iter +  (1|year), data = data, na.action=na.omit)
  # summary(dpSocial3)
  # performance::check_model(dpSocial3)
  
  dpSocial4 <- lmer(dpSocial~ sex + age + group + percentrank + eig.cent.groom + between.groom + clusterCoeff.groom +  (1|year), data = data, na.action=na.omit)
  summary(dpSocial4)
  # performance::check_model(dpSocial4)
  ChangeGroom.CFC[iter,c("(Intercept)","sexM","age","groupV","rank","eig.cent","between","clustcoeff")] <- getME(dpSocial4, "beta")
  ChangeGroom.CFC[iter,c("(focalID)","(year)")] <- getME(dpSocial4, "theta")
  
  dpSocial5 <- lmer(dpSocial~ sex + age + group + percentrank + DSI + numPartners + eig.cent.groom + between.groom + + clusterCoeff.groom+  (1|year), data = data, na.action=na.omit)
  summary(dpSocial5)
  # performance::check_model(dpSocial5)
  ChangeGroom.ALL[iter,c("(Intercept)","sexM","age","groupV","rank","DSI","numP","eig.cent","between","clustcoeff")] <- getME(dpSocial5, "beta")
  ChangeGroom.ALL[iter,c("(focalID)","(year)")] <- getME(dpSocial5, "theta")
  
  # #Save & export  models
  # export_summs(dpSocial5, digits = 4, to.file = "docx", file.name = "ModelingChangePSocAll.AllYears.docx")
  # export_summs(dpSocial1, dpSocial2, dpSocial4, model.names  = c("Base Model", "NeedForChange", "CapacityForChange"),
  #              to.file = "docx", digits = 3, file.name = "Modeling.ChangePSoc.AllYears.docx") 
  # compare_performance(dpSocial1, dpSocial2, dpSocial4, dpSocial5)
  
  #Model Whether there was an increase or a decrease in grooming rates
  isSocialIncrease <- glmer(isSocialIncrease ~ sex + age + group + percentrank +  (1|year), data = data, na.action=na.omit, family = binomial)
  summary(isSocialIncrease)
  # performance::check_model(isSocialIncrease)
  
  save(ChangeProx.BM,ChangeProx.NFC,ChangeProx.CFC,ChangeProx.ALL,ChangeGroom.BM,ChangeGroom.NFC,ChangeGroom.CFC,ChangeGroom.ALL,
       file = "C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/ChangePModelEffects.RData")
  
}