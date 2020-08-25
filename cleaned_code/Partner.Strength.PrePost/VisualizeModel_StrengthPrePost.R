#Compare strength of bonds pre-to-post hurricane. Are individuals spreading their grooming investment over more 
#weak partners or do they have fewer but stronger bonds?
#This script visualizes and tests the differene in grooming and proximity bond strength pre-to-post disaster.
# Overlapping histograms.

library(lme4)
library(jtools)
library(matrixStats)
library(gridExtra) 
library(graphics)
library(fitdistrplus)

#Load data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/Networks.RData")

##########################################
#Add sex, age and rank info
##########################################
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/")
population_info = read.csv("Behavioral_Data/SubjectInfo_2010-2017/Population details_Allgroups.allyears.txt")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

#Find all unique IDs
allIDs = unique(Networks$alter)

#get sex and year of birth from "Population details_Allgroups.allyears.txt"
ID_info= data.frame(); count = 0
for (id in 1:length(allIDs)){
  idx = which(as.character(population_info$id) == allIDs[id])
  if (length(idx)!=0){
    count = count +1
    ID_info[count,c("id","sex","yob")]<-population_info[idx,c("id","sex","yob")]
  }
}
ID_info$age = 2017-ID_info$yob #age in 2017
Networks[,c("sex","age")] = ID_info[match(Networks$alter,ID_info$id),c("sex","age")]

#Add rank info from "DOMINANCE.txt" file
Networks$rankFind = paste(Networks$alter,Networks$year,sep="")
Networks$ordrank=NA; Networks$percentrank=NA; Networkssexrank=NA
for (ii in 1:nrow(dominance_info)){
  idx = which(as.character(dominance_info$IDyear[ii]) == as.character(Networks$rankFind))
  if (length(idx)!=0){
    Networks$ordrank[idx] = as.character(dominance_info$ORD_RANK[ii])
    Networks$percentrank[idx] = as.character(dominance_info$X.DOMINATED[ii])
  }
}
Networks$percentrank = as.numeric(Networks$percentrank)/100

#Separate data by groom/prox & Scale parameters
data.groom = Networks[which(Networks$action == "groom"),]
data.groom[,"age"] <- scale(data.groom[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html



##########################################
#Model edge weight change pre-/post-
##########################################
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Partner.Strength.PrePost/")

AllP.groom.V=data.frame(); AllP.groom.KK=data.frame(); 
AllP.prox=data.frame(); iter=1

for (iter in 1:500){
  print(paste("%%%%%%%%%%%%%%%%%%",iter, "%%%%%%%%%%%%%%%%%%"))
  
  data.groom.iter=data.groom[data.groom$iter==iter,]
  data.prox.iter=data.prox[data.prox$iter==iter,]
  
  ## VISUALIZATION for 1 iteration ##
  # #GROOMING
  # strength.all.prepost<-ggplot(data.groom.iter, aes(x= as.factor(isPost), y=weight, fill=as.factor(isPost) ))+
  #   geom_violin()+
  #   # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  #   ggtitle(paste("Srength Bond Stable Partners"))+
  #   labs(fill = "Hurricane Status",x="Hurricane Status",y="Bond Strength")+
  #   facet_grid(~group)
  #   # facet_grid(group~year)
  # ggsave("Strength.AllP.prepost.tiff",strength.all.prepost)
  # ggsave("Strength.AllP.prepost.eps",strength.all.prepost)

  ## Separate data by groom and action (groom or prox) ##
  data.prox.iter.V = data.prox[which(data.prox$iter == iter & data.prox$group=="V"),]
  data.prox.iter.KK = data.prox[which(data.prox$iter == iter & data.prox$group=="KK"),]
  data.groom.iter.V = data.groom[which(data.groom$iter == iter& data.groom$group=="V"),]
  data.groom.iter.KK = data.groom[which(data.groom$iter == iter& data.groom$group=="KK"),]
  
  #GROOMING
  strength.groom.V = lmer(log(weight) ~ isPost + sex + age + percentrank+ (1|alter)+(1|year), data=data.groom.iter.V)
  # performance::check_model(strength.groom.V)
  # summary(strength.groom.V)
  AllP.groom.V[iter,c("(Intercept)","isPost","sexM","age","rank")] <- getME(strength.groom.V, "beta")
  AllP.groom.V[iter,c("id", "year")] <- getME(strength.groom.V, "theta")
  
  strength.groom.KK = lmer(log(weight) ~ isPost + sex + age + percentrank+ (1|alter)+(1|year), data=data.groom.iter.KK)
  # performance::check_model(strength.groom.KK)
  AllP.groom.KK[iter,c("(Intercept)","isPost","sexM","age","rank")] <- getME(strength.groom.KK, "beta")
  AllP.groom.KK[iter,c("id", "year")] <- getME(strength.groom.KK, "theta")

}

# Pool results from all iterations - Groom
Means = colMeans2(as.matrix(AllP.groom.V),na.rm = T); Means = round(Means,3)
CI = colQuantiles(as.matrix(AllP.groom.V,na.rm = T), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.groom.groom<-tableGrob(Estimates); t.groom.groom<-grid.arrange(t.groom.groom, top="Model All Effects: Groom Model Parameter Estimates"); #create table, arrange table
write.csv(Estimates, file="C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Partner.Strength.PrePost/GroomStrengthPrePost.V.csv")

Means = colMeans2(as.matrix(AllP.groom.KK)); Means = round(Means,3)
CI = colQuantiles(as.matrix(AllP.groom.KK), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.groom.groom<-tableGrob(Estimates); t.groom.groom<-grid.arrange(t.groom.groom, top="Model All Effects: Groom Model Parameter Estimates"); #create table, arrange table
write.csv(Estimates, file="C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/Partner.Strength.PrePost/GroomStrengthPrePost.KK.csv")
