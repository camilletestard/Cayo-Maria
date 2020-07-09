#generate_Hist_dpGroomProx 

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
setwd("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/")
source("cleaned_code/Functions/CalcSubsampledScans.R")
load("R.Data/SocialCapital.RData")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")

#Set parameters: 
num_iter = 50; iter =1
only2017=F; #if only considering 2017 (year just prior hurricane). Note: there is  no V2017 valid dp(groom) & dp(prox) because they all have less than 20 obs.
group = c("KK","KK","V", "V", "V")
years = c(2015,2017,2015,2016,2017)
groupyears = c("KK2015", "KK2017","V2015", "V2016", "V2017")

dprob.ALL = data.frame();
for (iter in 1:num_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  #####################################################################
  # 1. Compute change in p(Acc) and p(Social), per individual, per year
  #####################################################################
  
  #Calculate random subsamples
  randomScans = calcRandomScans(allScans)
   gy=1
  for (gy in 1:length(groupyears)){
    
    rscans = randomScans[which(randomScans$year == years[gy] & randomScans$group == group[gy]),]
    #Load data
    setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned")
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
    
    unqIDs = as.character(meta_data$id)
    dprob=data.frame(matrix(NA, nrow=length(unqIDs),ncol=5)); colnames(dprob)=c("iter","id","dpAcc","dpSocial","num_obs")
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
      dprob[id,]=c(iter,unqIDs[id],dpAcc,dpSocial,num_obs)
    } #end of id for loop
    dprob$group = group[gy]; dprob$year = years[gy]; dprob$iter=iter
    dprob.ALL = rbind(dprob.ALL, dprob)
  } #end of groupyear for loop
  
  dprob.ALL$dpAcc=as.numeric(dprob.ALL$dpAcc)
  dprob.ALL$dpSocial=as.numeric(dprob.ALL$dpSocial)
}
save(dprob.ALL,file="C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/dprob.prox.groom.RData")

#####################################################################
# 2. Merge dprob and social Capital data &clean
####################################################################
setwd("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/")
load("R.Data/SocialCapital.RData")
load("R.Data/dprob.prox.groom.RData")

data=dprob.ALL
data[,c("sex","age","percentrank")]= SocialCapital.ALL[match(dprob.ALL$id,SocialCapital.ALL$id),c("sex","age","percentrank")]#combine dprob and social capital (should be in exactly the same row order (by id))

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

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPSoc")

#check distribution of independent variables
# tiff("ChangeProb.tiff",units="in", width=7, height=4, res=300, compression = 'lzw')
tiff("Change.pProx.pGroom.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial,col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom)",xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.25,1))
hist(data$dpAcc,col=rgb(1,0,0,0.5), breaks=40,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

tiff("Change.pGroom.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial,col=rgb(0,1,1,0.5), breaks=20,main="Change p(Groom)",xlab="Change p(Groom) pre-to-post hurricane", xlim=c(-0.25,0.25))
segments(x0=0,y0=0,x1=0,y1=1500,col="red",lwd=4, lty=2)
box()
dev.off()

#Seperated by sex
#Males
tiff("Change.pProx.pGroom.M.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$sex=="M")],col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - Males",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,600))
hist(data$dpAcc[which(data$sex=="M")],col=rgb(1,0,0,0.5), breaks=40,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

#Females
tiff("Change.pProx.pGroom.F.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$sex=="F")],col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - Females",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,900))
hist(data$dpAcc[which(data$sex=="F")],col=rgb(1,0,0,0.5), breaks=40,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

#Males vs. Females
tiff("Change.pGroom.MvsF.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$sex=="M")],col=rgb(0,0,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - Males",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.25,0.25), ylim=c(0,1000))
hist(data$dpSocial[which(data$sex=="F")],col=rgb(1,0,0,0.5), breaks=20,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Males", "Females"), fill=c("red", "cyan"))
dev.off()

#Separated by group
#KK
tiff("Change.pProx.pGroom.KK.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$group=="KK")],col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - group KK",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,900))
hist(data$dpAcc[which(data$group=="KK")],col=rgb(1,0,0,0.5), breaks=30,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

tiff("Change.pProx.pGroom.KK.F.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$group=="KK" & data$sex=="F")],col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - group KK, Females",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,400))
hist(data$dpAcc[which(data$group=="KK" & data$sex=="F")],col=rgb(1,0,0,0.5), breaks=30,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

tiff("Change.pProx.pGroom.KK.M.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$group=="KK" & data$sex=="M")],col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - group KK, Males",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,400))
hist(data$dpAcc[which(data$group=="KK" & data$sex=="M")],col=rgb(1,0,0,0.5), breaks=30,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

#V
tiff("Change.pProx.pGroom.V.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$group=="V")],col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - group V",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,1000))
hist(data$dpAcc[which(data$group=="V")],col=rgb(1,0,0,0.5), breaks=30,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

tiff("Change.pProx.pGroom.V.F.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$group=="V" & data$sex=="F")],col=rgb(0,1,1,0.5), breaks=20,main="Change p(Prox) or p(Groom) - group V, Females",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,600))
hist(data$dpAcc[which(data$group=="V" & data$sex=="F")],col=rgb(1,0,0,0.5), breaks=30,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()

tiff("Change.pProx.pGroom.V.M.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
hist(data$dpSocial[which(data$group=="V" & data$sex=="M")],col=rgb(0,1,1,0.5), breaks=10,main="Change p(Prox) or p(Groom) - group V, Males",
     xlab="Change p(Prox) or p(Groom) pre-to-post hurricane", xlim=c(-0.5,1), ylim=c(0,600))
hist(data$dpAcc[which(data$group=="V" & data$sex=="M")],col=rgb(1,0,0,0.5), breaks=40,add=T)
segments(x0=0,y0=0,x1=0,y1=3000,col="red",lwd=4, lty=2)
box()
legend("topright", c("Change p(Prox)", "Change p(Groom)"), fill=c("red", "cyan"))
dev.off()