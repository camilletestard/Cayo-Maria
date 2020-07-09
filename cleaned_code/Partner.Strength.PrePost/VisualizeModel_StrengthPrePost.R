#Compare strength of bonds pre-to-post hurricane. Are individuals spreading their grooming investment over more 
#weak partners or do they have fewer but stronger bonds?
#This script visualizes and models the change in grooming and proximity bond strength post-disaster.
# Overlapping histograms.

library(lme4)
library(jtools)
#Load data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/StrengthPrePost.RData")

##########################################################
#Compare distribution of grooming strength pre- to post- hurricane
#Separate data by groom/prox
data.groom = ID.strength.data.All[which(ID.strength.data.All$action == "groom"),]
data.prox = ID.strength.data.All[which(ID.strength.data.All$action == "prox"),]

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Partner.Strength.PrePost/")

#both groups
tiff("StrengthBond.PrePost.tiff", 
     units="in", width=10, height=10, res=300, compression = 'lzw')

hist(log(data.groom$strength.all[data.groom$isPost==0]), col=rgb(1,0,0,0.5), xlim=c(-5, 0),
     xlab="Strength of grooming bond (log)",
     main="Strength of grooming bond pre- to post-hurricane")
hist(log(data.groom$strength.all[data.groom$isPost==1]), col=rgb(0,1,1,0.5), breaks=30, add=T)
box()
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))

dev.off()

data.groom$strength.all[data.groom$strength.all==0]=NA
strength.BM = lmer(log(strength.all)~isPost*group+(1|id)+(1|year),data=data.groom)
# performance::check_model(strengh.BM)
export_summs(strength.BM, model.names = "change.Groom.Strength", to.file = "docx", file.name = "Modeling.GroomStrength.PrePost.docx")

#group V
data.groom.V=data.groom[data.groom$group=="V",]
hist(log(data.groom.V$strength.all[data.groom.V$isPost==0]), col=rgb(1,0,0,0.5), xlim=c(-5, 0))
hist(log(data.groom.V$strength.all[data.groom.V$isPost==1]), col=rgb(0,1,1,0.5), breaks=30, add=T)

data.groom.V$strength.all[data.groom.V$strength.all==0]=NA
strength.V = lmer(log(strength.all)~isPost +(1|id)+(1|year),data=data.groom.V)
# performance::check_model(strengh.V)
export_summs(strength.V, model.names = "change.Groom.Strength", to.file = "docx", file.name = "Modeling.GroomStrength.PrePost.V.docx")

#group KK
data.groom.KK=data.groom[data.groom$group=="KK",]
hist(log(data.groom.KK$strength.all[data.groom.KK$isPost==0]), col=rgb(1,0,0,0.5), xlim=c(-5, 0), ylim=c(0,3500))
hist(log(data.groom.KK$strength.all[data.groom.KK$isPost==1]), col=rgb(0,1,1,0.5), breaks=20, add=T)

data.groom.KK$strength.all[data.groom.KK$strength.all==0]=NA
strength.KK = lmer(log(strength.all)~isPost +(1|id)+(1|year),data=data.groom.KK)
# performance::check_model(strengh.KK)
export_summs(strength.KK, model.names = "change.Groom.Strength", to.file = "docx", file.name = "Modeling.GroomStrength.PrePost.KK.docx")
