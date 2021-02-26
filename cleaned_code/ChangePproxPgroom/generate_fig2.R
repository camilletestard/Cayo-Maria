#Generate figure 2
#Compute p(grooming) and p(proximity) for all individuals over 500 iterations and plot pre-/post-differences.The resulting figure is Figure 2 in the manuscript.
#Functions called:CalcSubsampledScans.R
#Input: allScans.txt
#Output: ChangeP_figure2.RData and Figure 2 from manuscript.
# Camille Testard - 2021

#Load AllScans file
setwd("/Users/camilletestard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
allScans = read.csv("Data All Cleaned/allScans.txt")

# n_iter=500; iter = 1
# isProx.all = data.frame(matrix(NA, nrow = 0, ncol = 7)); colnames(isProx.all)=c("id","prob","isPost","group","sex","groupsex","iter"); count = 0; #initialize
# isSocial.all = data.frame(matrix(NA, nrow = 0, ncol = 7)); colnames(isSocial.all)=c("id","prob","isPost","group","sex","groupsex","iter"); count = 0; #initialize
# 
# for (iter in 1:n_iter){
#   
#   print(paste("%%%%%%%%%%%%%%%%%%%%%%% iter ", iter," %%%%%%%%%%%%%%%%%%%%%%%"))
#   #Subsample data
#   ExSubScans = calcRandomScans(allScans)
#   unqIDs = as.character(unique(ExSubScans$focalID))
#   
#   ###################################################
#   ## Proximity for full year
#   
#   #Compute p(prox) for each individual:
#   isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isProx)=c("id","prob","isPost","group","sex"); count = 0; #initialize
#   for (id in 1:length(unqIDs)){ #For all individuals
#     id.all.pre = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #get all pre-hurricane data for that individual
#     id.all.post = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]#get all post-hurricane data for that individual
#     group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
#     sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id])]#get sex info for that individual
#     count = count+1
#     isProx$id[count] = unqIDs[id] #record ID
#     isProx$prob[count] = sum(id.all.pre)/length(id.all.pre) #get proportion of observations this individual was seen in proximity to other partners PRE-hurricane
#     isProx$isPost[count] = "pre"; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1]) #keep track of hurricane status, sex and group info
#     count = count+1
#     isProx$id[count] = unqIDs[id]
#     isProx$prob[count] = sum(id.all.post)/length(id.all.post) #get proportion of observations this individual was seen in proximity to other partners POST-hurricane
#     isProx$isPost[count] = "post"; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
#   }
#   isProx$groupsex = paste(isProx$group,isProx$sex,sep=".") #create group.sex column for later plotting
#   isProx$iter=iter
#   isProx.all=rbind(isProx.all,isProx)
#   
#   
#   ###################################################
#   ## Grooming for full year
#   
#   #Compute p(groom) for each individual:
#   isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isSocial)=c("id","prob","isPost","group","sex"); count = 0;
#   for (id in 1:length(unqIDs)){
#     id.all.pre = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
#     id.all.post = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]
#     group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
#     sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
#     count = count+1
#     isSocial$id[count] = unqIDs[id]
#     isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre) #get proportion of observations this individual was seen grooming other partners PRE-hurricane
#     isSocial$isPost[count] = "pre"; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
#     count = count+1
#     isSocial$id[count] = unqIDs[id]
#     isSocial$prob[count] = sum(id.all.post)/length(id.all.post) #get proportion of observations this individual was seen grooming other partners POST-hurricane
#     isSocial$isPost[count] = "post"; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1]);
#   }
#   isSocial$groupsex = paste(isSocial$group,isSocial$sex,sep=".")
#   isSocial$iter=iter
#   isSocial.all=rbind(isSocial.all,isSocial)
#   
#   save(isProx.all, isSocial.all, file="RData/ChangeP_figure2.RData")
#   
# }


### PLOT RESULTS ###
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPsoc/") 
load("ChangeP_figure1.RData")

#Load libraries
library(ggplot2)
library(ggpubr)
library(matlab)
library(beeswarm)

#For later plotting change order of factor
isProx.all$isPost <- factor(isProx.all$isPost, levels = c("pre","post"))
isSocial.all$isPost <- factor(isSocial.all$isPost, levels = c("pre","post"))

# #If only want to use one iteration
# iter = sample(500,1)
# isProx.all.plot=isProx.all[isProx.all$iter==iter,]
# isSocial.all.plot=isSocial.all[isSocial.all$iter==iter,]

isProx.all.plot=isProx.all
isSocial.all.plot=isSocial.all

###################################################################
#For probability of PROXIMITY:
###################################################################

#Our transformation function
scaleFUN <- function(x) sprintf("%.2f", x)

#Violin plots of p(prox) pre/post-hurricane (not paired)
pAcc<-
  ggplot(isProx.all.plot, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin(alpha=0.5, colour=NA)+
  geom_boxplot(width=.1, outlier.shape = NA)+
  # ggtitle("Change in proximity rates\nfollowing Hurricane Maria")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(fill = "Hurricane \n Status",x="Hurricane Status",y="p(Proximity)")+
  theme(legend.position = "none")+
  theme_classic(base_size=20)+
  facet_grid(~group)
ggsave(file="Change.pProx.eps", plot=pAcc)
ggsave(file="Change.pProx.tiff", plot=pAcc)


#Plot histogram of p(prox) pre/post-hurricane
hist(isProx.all.plot$prob[which(isProx.all.plot$isPost=="pre")],col=rgb(1,0,0,0.5), breaks=10, main="Individual p(Proximity) pre- to post-hurricane",xlab="p(Proximity)", xlim=c(0, 1))
hist(isProx.all.plot$prob[which(isProx.all.plot$isPost=="post")],col=rgb(0,1,1,0.5), breaks=30, add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()


###################################################################
#For probability of GROOMING:
###################################################################

#Violin plots of p(groom) pre/post-hurricane
pSoc<-
  ggplot(isSocial.all.plot, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin(alpha=0.5, colour=NA)+ ylim(0,1)+
  geom_boxplot(width=.1, outlier.shape = NA)+
  # ggtitle("Change in grooming rates\nfollowing Hurricane Maria")+
  labs(fill = "Hurricane\nStatus",x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~group)+theme_classic(base_size=20)
  #+scale_y_continuous(labels = scaleFUN)
ggsave(file="Change.pSocial_v2.eps", plot=pSoc)
ggsave(file="Change.pSocial_v2.tiff", plot=pSoc)

#Plot histogram of p(groom) pre/post-hurricane
hist(isSocial.all.plot$prob[which(isSocial.all.plot$isPost=="pre")],col=rgb(1,0,0,0.5), breaks=20,main="Individual p(Grooming) pre- to post-hurricane",xlab="p(Grooming)")
hist(isSocial.all.plot$prob[which(isSocial.all.plot$isPost=="post")],col=rgb(0,1,1,0.5), breaks=30,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

