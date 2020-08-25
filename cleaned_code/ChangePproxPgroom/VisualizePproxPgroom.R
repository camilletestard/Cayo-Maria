#Visualize pProx and pGroom
# This scripts aims to visualize how p(proximity) and p(grooming) changed pre-to-post huricane 
# and whether there are inter-individual differences in how much individuals changed their affiliative behaviors. 
# Note: visualization happen after 1 iteration of sub-sampling and therefore is not a representation of our full
# dataset. However, after mutliple output iterations, all plots look similar, which makes me think it is a good 
# representaiton of our full dataset. 
# Functions called: CalcSubsampledScans
# Input: allScans.txt
# Outputs (visuals):
#   - paired box plots pProx/pGroom pre- post-hurricane
# - Violin plots and histograms of pProx/pGroom pre- post-hurricane
# - Separated by group and quarter
# - Correlation plot dpGroom vs. dpProx.

#Load libraries
library(ggplot2)
library(ggpubr)
library(matlab)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#Optional: Load mutiple iterations of subsampled data. This will only be used if one wishes to plot data coming from 
#multiple iterations instead on just one.
# load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP.RData")

#Subsample data
ExSubScans = calcRandomScans(allScans)
unqIDs = as.character(unique(ExSubScans$focalID))

###################################################################
#For probability of PROXIMITY:
###################################################################

#Set saving directory
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPsoc/") 

#Compute p(prox) for each individual:
isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isProx)=c("id","prob","isPost","group","sex"); count = 0; #initialize
for (id in 1:length(unqIDs)){ #For all individuals
  id.all.pre = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #get all pre-hurricane data for that individual
  id.all.post = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]#get all post-hurricane data for that individual
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id])]#get sex info for that individual
  if (length(id.all.pre)>=10) { #If there are at least 20 observations for that individual both pre-hurricane and post-hurricane
    count = count+1
    isProx$id[count] = unqIDs[id] #record ID
    isProx$prob[count] = sum(id.all.pre)/length(id.all.pre) #get proportion of observations this individual was seen in proximity to other partners PRE-hurricane
    isProx$isPost[count] = "pre"; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1]) #keep track of hurricane status, sex and group info
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.post)/length(id.all.post) #get proportion of observations this individual was seen in proximity to other partners POST-hurricane
    isProx$isPost[count] = "post"; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
  }
}
isProx$groupsex = paste(isProx$group,isProx$sex,sep=".") #create group.sex column for later plotting
isProx=isProx[-which(is.na(isProx$prob)),] #remove all individuals with no proximity info (<20 observations)
#For later plotting keep track of increases and decreases in p(prox)
is_increasing = zeros(c(length(which(isProx$isPost=="post")),1)); is_increasing[which((isProx$prob[which(isProx$isPost=="post")] - isProx$prob[which(isProx$isPost=="pre")])>0)]=1
is_increasing = rep(is_increasing,times=ones(c(length(which(isProx$isPost=="post")),1))*2) 

#For later plotting change order of factor
isProx$isPost <- factor(isProx$isPost, levels = c("pre","post"))
isProx$isPost  # notice the changed order of factor levels

#Plot paired prox data pre- post:
ggplot(isProx, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_violin(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.5)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+#,position = position_jitter(0.1))+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Proximity) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="p(Proximity)")

#Violin plots of p(prox) pre/post-hurricane (not paired)
pAcc<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("p(Proximity) pre & post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Proximity)")
ggsave(file="Change.pProx.eps", plot=pAcc)
ggsave(file="Change.pProx.tiff", plot=pAcc)


#Plot histogram of p(prox) pre/post-hurricane
hist(isProx$prob[which(isProx$isPost=="pre")],col=rgb(1,0,0,0.5), breaks=10, main="Individual p(Proximity) pre- to post-hurricane",xlab="p(Proximity)", xlim=c(0, 1))
hist(isProx$prob[which(isProx$isPost=="post")],col=rgb(0,1,1,0.5), breaks=30, add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#########################
#Quarter differences 

#isProx per Quarter
quarter = unique(ExSubScans$Q); isProxQ = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isProxQ)=c("prob","isPost","quarter"); count = 0;
for (q in 1:length(quarter)){
  
  ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
    group = ExSubScansQ$group[which(ExSubScansQ$focalID == unqIDs[id])]#get group info for that individual
    sex =  ExSubScansQ$sex[which(ExSubScansQ$focalID == unqIDs[id])]#get sex in
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isProxQ$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProxQ$isPost[count] = "pre"; isProxQ$quarter[count] = paste("Q",quarter[q],sep="")
      isProxQ$group[count] = as.character(group[1]); isProxQ$sex[count] = as.character(sex[1])
      count = count+1
      isProxQ$prob[count] = sum(id.all.post)/length(id.all.post)
      isProxQ$isPost[count] = "post"; isProxQ$quarter[count] = paste("Q",quarter[q],sep="")
      isProxQ$group[count] = as.character(group[1]); isProxQ$sex[count] = as.character(sex[1])
    }
  }
}
isProxQ=isProxQ[-which(is.na(isProxQ$prob)),]
#For later plotting change order of factor
isProxQ$isPost <- factor(isProxQ$isPost, levels = c("pre","post"))
isProxQ$isPost  # notice the changed order of factor levels

#Plot change in p(Proximity), divided by quarter & group
pAccQ<-ggplot(isProxQ, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin()+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Proximity) pre- to post-hurricane, divided by quarter")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(proximity)")+
  facet_grid(group~quarter)
#Save plot
ggsave(file="Change.pProxQ.eps", plot=pAccQ)
ggsave(file="Change.pProxQ.tiff", plot=pAccQ)


###################################################################
#For probability of GROOMING:
###################################################################

#Compute p(groom) for each individual:
isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isSocial)=c("id","prob","isPost","group","sex"); count = 0;
for (id in 1:length(unqIDs)){
  id.all.pre = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  id.all.post = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  if (length(id.all.pre)>20) {
    count = count+1
    isSocial$id[count] = unqIDs[id]
    isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre) #get proportion of observations this individual was seen grooming other partners PRE-hurricane
    isSocial$isPost[count] = "pre"; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
    count = count+1
    isSocial$id[count] = unqIDs[id]
    isSocial$prob[count] = sum(id.all.post)/length(id.all.post) #get proportion of observations this individual was seen grooming other partners POST-hurricane
    isSocial$isPost[count] = "post"; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
  }
}
isSocial$groupsex = paste(isSocial$group,isSocial$sex,sep=".")
isSocial=isSocial[-which(is.na(isSocial$prob)),]

#For later plotting keep track of increases and decreases.
is_increasing = zeros(c(length(which(isSocial$isPost=="post")),1)); 
is_increasing[which((isSocial$prob[which(isSocial$isPost=="post")] - isSocial$prob[which(isSocial$isPost=="pre")])>0)]=1
is_increasing = rep(is_increasing,times=ones(c(length(which(isSocial$isPost=="post")),1))*2) 

#For later plotting change order of factor
isSocial$isPost <- factor(isSocial$isPost, levels = c("pre","post"))
isSocial$isPost  # notice the changed order of factor levels

#Plot paired data pre- post:
ggplot(isSocial, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_violin(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="p(Grooming)")

#Violin plots of p(groom) pre/post-hurricane
pSoc<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("p(Grooming) pre & post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="p(Grooming)")
ggsave(file="Change.pSocial.eps", plot=pSoc)
ggsave(file="Change.pSocial.tiff", plot=pSoc)

#Plot histogram of p(groom) pre/post-hurricane
hist(isSocial$prob[which(isSocial$isPost=="pre")],col=rgb(1,0,0,0.5), breaks=20,main="Individual p(Grooming) pre- to post-hurricane",xlab="p(Grooming)")
hist(isSocial$prob[which(isSocial$isPost=="post")],col=rgb(0,1,1,0.5), breaks=20,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#########################
#Quarter differences 

#isSocial per Quarter
quarter = unique(ExSubScans$Q); isSocialQ = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isSocialQ)=c("prob","isPost","quarter"); count = 0;
for (q in 1:length(quarter)){
  
  ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
    group = ExSubScansQ$group[which(ExSubScansQ$focalID == unqIDs[id])]#get group info for that individual
    sex =  ExSubScansQ$sex[which(ExSubScansQ$focalID == unqIDs[id])]#get sex in
    if (length(id.all.pre)>=10) { #If there are more than 20 observations for that individual 
      count = count+1
      isSocialQ$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isSocialQ$isPost[count] = "pre"; isSocialQ$quarter[count] = paste("Q",quarter[q],sep="")
      isSocialQ$group[count] = as.character(group[1]); isSocialQ$sex[count] = as.character(sex[1])
      count = count+1
      isSocialQ$prob[count] = sum(id.all.post)/length(id.all.post)
      isSocialQ$isPost[count] = "post"; isSocialQ$quarter[count] = paste("Q",quarter[q],sep="")
      isSocialQ$group[count] = as.character(group[1]); isSocialQ$sex[count] = as.character(sex[1])
    }
  }
}
isSocialQ=isSocialQ[-which(is.na(isSocialQ$prob)),]
#For later plotting change order of factor
isSocialQ$isPost <- factor(isSocialQ$isPost, levels = c("pre","post"))
isSocialQ$isPost  # notice the changed order of factor levels

#Plot change in p(Grooming), divided by quarter and group
pSocQ <-ggplot(isSocialQ, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin()+
  # geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Grooming) pre- to post-hurricane, divided by quarter")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(grooming)")+
  facet_grid(group~quarter)
ggsave(file="Change.pSocQ.eps", plot=pSocQ)
ggsave(file="Change.pSocQ.tiff", plot=pSocQ)


##############################################################################
#Plot correlation between change in prob proximity and change in prob grooming
##############################################################################

changeSocial = data.frame(matrix(NA, nrow = length(unqIDs), ncol = 3)); names(changeSocial)=c("id","dProx","dGroom")
for (id in 1:length(unqIDs)){
  changeSocial$id[id]=unqIDs[id] #id
  dProx = isProx$prob[intersect(which(!is.na(match(isProx$id, unqIDs[id]))), which(isProx$isPost==1))] - #find change in pProx
    isProx$prob[intersect(which(!is.na(match(isProx$id, unqIDs[id]))), which(isProx$isPost==0))]
  dGroom = isSocial$prob[intersect(which(!is.na(match(isSocial$id, unqIDs[id]))), which(isSocial$isPost==1))] - #find change in pGroom
    isSocial$prob[intersect(which(!is.na(match(isSocial$id, unqIDs[id]))), which(isSocial$isPost==0))]
  if(length(dProx) + length(dGroom) == 2){ #if there are enough obsevrations to plot pProx and pGroom
    changeSocial$dProx[id] = dProx
    changeSocial$dGroom[id] = dGroom 
  }
}
#Scatter plot to visualize whether there is a correlation between the change in pProx and change in pGroom
ggscatter(changeSocial, x = "dProx", y = "dGroom", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Change in Proximity", ylab = "Change in Grooming",
          title = "Correlation between dp(Prox) and dp(Groom)")

