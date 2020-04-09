######## Modelling Logistic Regressions on A2

library(lme4)# Generalized Linear Mixed Models
library(MCMCglmm)# Generalized Linear Mixed Models, other package
library(bbmle)#Tools for General Maximum Likelihood Estimation
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")


ExSubScans = calcRandomScans(allScans) #Load sub-sampled scans
ExSubScans$focalID = as.character(ExSubScans$focalID)
age = as.data.frame(table(ExSubScans$age, ExSubScans$focalID))
rank = as.data.frame(table(ExSubScans$percentrank, ExSubScans$focalID))

#remove individuals with too few observations
idFreq = as.data.frame(table(ExSubScans$focalID)) #Find frequency
id_not_sampled_enough = as.character(idFreq$Var1[which(idFreq$Freq < 40)]) # find individuals with less than 20 observations pre or post
ExSubScans = ExSubScans[which(is.na(match(ExSubScans$focalID, id_not_sampled_enough))),] # remove scans from those individuals

# Make sure variables are coded correctly
ExSubScans$focalID = as.character(ExSubScans$focalID)
ExSubScans$year <- as.factor(ExSubScans$year); ExSubScans$isPost <- as.integer(as.character(ExSubScans$isPost))
ExSubScans$isProx <- as.integer(as.character(ExSubScans$isProx)); ExSubScans$isSocial <- as.integer(as.character(ExSubScans$isSocial))
#Scale parameters
ExSubScans[,"age"] <- scale(ExSubScans[,"age"]) #helps avoid convergence issues: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
ExSubScans[,"percentrank"] <- as.numeric(ExSubScans[,"percentrank"])/10


##########################################################
#Compute p(prox) for each individual in each group separately
##########################################################

#NOTE: p(prox) here is agnostic to the number of observations per individual or the year

#### p(prox) for group V individuals ####
ExSubScansV = ExSubScans[which(ExSubScans$group=='V'),] #Only select group V

unqIDs = as.character(unique(ExSubScansV$focalID))

for (i in 1:length(unqIDs)){
  isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 7)); colnames(isProx)=c("id","prob","isPost","group","sex","age","percentrank"); count = 0;
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansV$isProx[which(ExSubScansV$focalID == unqIDs[id] & ExSubScansV$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansV$isProx[which(ExSubScansV$focalID == unqIDs[id] & ExSubScansV$isPost == 1)]#get all post-re-hurricane data for that individuals
    group = ExSubScansV$group[which(ExSubScansV$focalID == unqIDs[id])]#get group info for that individual
    sex =  ExSubScansV$sex[which(ExSubScansV$focalID == unqIDs[id])]#get sex info for that individual
    percentrank = ExSubScansV$percentrank[which(ExSubScansV$focalID == unqIDs[id])]#get rank info for that individual
    age = ExSubScansV$age[which(ExSubScansV$focalID == unqIDs[id])]#get age info for that individual
    # for pre-hurricane scans
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.pre)/length(id.all.pre) #compute probability to be scanned in proximity
    isProx$isPost[count] = 0; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1]) #add sex and group info
    isProx$percentrank[count] = percentrank[1]; isProx$age[count] = age[1] #add rank and age info
    # for post-hurricane scans
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.post)/length(id.all.post)
    isProx$isPost[count] = 1; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    isProx$percentrank[count] = percentrank[1]; isProx$age[count] = age[1]
  }
}
isProxV = isProx
hist(isProxV$prob, breaks = 20)

# Find the ID of individuals with p(prox) = 1 (which doesnt seem too biological...)
# maxOut_ID = isProxV$id[which(isProxV$prob == 1)]
# Check whther these are individuals with low number of observations
# numObs_maxOut_ID = V_id_freq$Freq[match(maxed_out_ID, V_id_freq$Var1)]
# Yes it is the case! So from now on I will include a minimum of 20 observations pre and post-hurricane.

#### p(prox) for Group KK individuals####
ExSubScansKK = ExSubScans[which(ExSubScans$group=='KK'),]

unqIDs = unique(ExSubScansKK$focalID)

for (i in 1:length(unqIDs)){
  isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 7)); colnames(isProx)=c("id","prob","isPost","group","sex","age","percentrank"); count = 0;
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansKK$isProx[which(ExSubScansKK$focalID == unqIDs[id] & ExSubScansKK$isPost == 0)] #get all pre-hurricane data for that indiKKiduals
    id.all.post = ExSubScansKK$isProx[which(ExSubScansKK$focalID == unqIDs[id] & ExSubScansKK$isPost == 1)]#get all post-re-hurricane data for that indiKKiduals
    group = ExSubScansKK$group[which(ExSubScansKK$focalID == unqIDs[id])]#get group info for that individual
    sex =  ExSubScansKK$sex[which(ExSubScansKK$focalID == unqIDs[id])]#get sex info for that individual
    percentrank = ExSubScansKK$percentrank[which(ExSubScansKK$focalID == unqIDs[id])]#get rank info for that individual
    age = ExSubScansKK$age[which(ExSubScansKK$focalID == unqIDs[id])]#get age info for that individual
    #if (length(id.all.pre)>=10) { #If there are more than 20 obserKKations for that individual 
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
    isProx$isPost[count] = 0; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    isProx$percentrank[count] = percentrank[1]; isProx$age[count] = age[1]
    
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.post)/length(id.all.post)
    isProx$isPost[count] = 1; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    isProx$percentrank[count] = percentrank[1]; isProx$age[count] = age[1]
    #}
  }
}
isProxKK = isProx
hist(isProxKK$prob, breaks = 20)
# maxOut_ID = isProxKK$id[which(isProxKK$prob == 0)]; isProxKK$isPost[which(isProxKK$prob == 0)]
# numObs_maxOut_ID = V_id_freq$Freq[match(maxed_out_ID, V_id_freq$Var1)]

##########################################################
#Run model on probabilities directly (like in graph)
##########################################################

#Only for group V
hist(isProxV$prob[which(isProxV$isPost == 0)], col=rgb(1,0,0,0.5), breaks = 20, xlim=c(0, 0.6), main = "Overlapping Histogram", xlab="p(prox)")
hist(isProxV$prob[which(isProxV$isPost == 1)], col=rgb(0,0,1,0.5), add = T, breaks = 30)
#the histograms show a clear increase in p(prox) post-hurricane

isNotAloneV <- lm(prob~ isPost + sex + age + percentrank, data = isProxV) #Note: might want to try MCMCglmm?
summary(isNotAloneV)
performance::check_model(isNotAloneV)
#When running model on p(prox) instead of binomial, I do get a positive isPost parameter (which fits with the historgram)

##############################################################################
#Run binomial model without individuals with low #obs 
##############################################################################

#### BASE MODEL ####

#For p(prox), all groups
isNotAlone <- glmer(isProx~ isPost + sex + age + percentrank + group + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAlone)
# performance::check_model(isNotAlone) #Check that model assumptions were respected
#Note: results fit the plotted probabilities

#For p(groom), all groups
isSocial <- glmer(isSocial~ isPost + sex + age + percentrank + group + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
summary(isSocial)
# performance::check_model(isSocial)

#Note: results for Base Model matched plots of p(prox) and p(groom) pre and post-hurricane

#################################
##See differences between groups
#################################

#Incldue interaction paramater with group
isNotAloneG <- glmer(isProx~ isPost*group + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneG)

isSocialG <- glmer(isSocial~ isPost*group + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScans, family = binomial)
summary(isSocialG)

##################################################################

##### Group V ####
ExSubScansV = ExSubScans[which(ExSubScans$group=='V'),]
table(ExSubScansV$isPost,ExSubScansV$isProx)
#Check change in overall population probability (different way than table, but I could've used that)
V_id_freq = as.data.frame(table(ExSubScansV$focalID)); V_isPost_freq = as.data.frame(table(ExSubScansV$isPost));
probPre = sum(ExSubScansV$isProx[which(ExSubScansV$isPost==0)])/V_isPost_freq$Freq[1]
probPost = sum(ExSubScansV$isProx[which(ExSubScansV$isPost==1)])/V_isPost_freq$Freq[2]
probPost - probPre # NO THERE IS MORE THAN DOUBLING OF PROB

#Check which individual probability decreased.
unqIDs = unique(ExSubScansV$focalID)
for (i in 1:length(unqIDs)){
  isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isProx)=c("id","prob","isPost","group","sex"); count = 0;
  dprob = data.frame(matrix(NA, nrow = length(unqIDs), ncol = 2)); colnames(dprob)=c("id","prob"); count2 = 0;
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansV$isProx[which(ExSubScansV$focalID == unqIDs[id] & ExSubScansV$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansV$isProx[which(ExSubScansV$focalID == unqIDs[id] & ExSubScansV$isPost == 1)]#get all post-re-hurricane data for that individuals
    group = ExSubScansV$group[which(ExSubScansV$focalID == unqIDs[id])]#get group info for that individual
    sex =  ExSubScansV$sex[which(ExSubScansV$focalID == unqIDs[id])]#get sex info for that individual
    #if (length(id.all.pre)>=10) { #If there are more than 20 observations for that individual 
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
    isProx$isPost[count] = 0; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.post)/length(id.all.post)
    isProx$isPost[count] = 1; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    
    count2 = count2+1
    dprob$id[count2]= unqIDs[id]
    dprob$prob[count2]= isProx$prob[count] - isProx$prob[count-1]
    #}
  }
}
hist(dprob$prob); neg_prob_id = dprob$id[which(dprob$prob<0)] #Check who deceased p(prox) post-hurricane
V_id_freq$Freq[match(neg_prob_id, V_id_freq$Var1)] #check the number of observations for these individuals

# #If I remove IDs with decrease post-hurricane
# ExSubScansV= ExSubScansV[is.na(match(ExSubScansV$focalID, neg_prob_id)),]

isNotAloneV <- glmer(isProx~ isPost + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScansV, family = binomial) #Note: might want to try MCMCglmm?
summary(isNotAloneV)
#Plot effects seperately
plot(effects::allEffects(isNotAloneV))
sjPlot::plot_model(isNotAloneV, type="re", vline.color = "black")

#Kenny code!
#ggplot(ExSubScansV,aes(isProx,isPost)) + geom_bin2d(binwidth=1) + stat_bin2d(geom='text',aes(label = ..count..),bins=2) + facet_wrap(~focalID) + scale_x_continuous(limits=c(-2,2)) + scale_y_continuous(limits=c(-2,2)) + theme_classic() + scale_fill_gradient(low = "white", high = "red")

#Note: Results DO NOT match the p(prox) change per individual
# If i only include individuals with at least 40 observations total, i still get a negative isPost parameter
# If I exclude individuals who decreased their p(prox) I STILL GET A NEGATIVE PARAMATER
# But, if I remove the random effect (1|ID) than the paramater becomes positive. 

isSocialV <- glmer(isSocial~ isPost + sex + age + percentrank + (1|focalID) + (1|year), data = ExSubScansV, family = binomial)
summary(isSocialV)

#Group KK
ExSubScansKK = ExSubScans[which(ExSubScans$group=='KK'),]
KK_id_freq = as.data.frame(table(ExSubScansV$focalID)); KK_isPost_freq = as.data.frame(table(ExSubScansKK$isPost));
probPre = sum(ExSubScansKK$isProx[which(ExSubScansKK$isPost==0)])/KK_isPost_freq$Freq[1]
probPost = sum(ExSubScansKK$isProx[which(ExSubScansKK$isPost==1)])/KK_isPost_freq$Freq[2]
probPost - probPre

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

