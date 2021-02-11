#Generate figure 1

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


#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#Optional: Load mutiple iterations of subsampled data. This will only be used if one wishes to plot data coming from 
#multiple iterations instead on just one.
# load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP.RData")


#Set saving directory
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPsoc/") 

# 
# ###################################################################
# #For probability of PROXIMITY:
# ###################################################################
# 
# n_iter=500; iter = 1
# isProx.all = data.frame(matrix(NA, nrow = 0, ncol = 7)); colnames(isProx.all)=c("id","prob","isPost","group","sex","groupsex","iter"); count = 0; #initialize
# isProxQ.all = data.frame(matrix(NA, nrow = 0, ncol = 5)); colnames(isProx.all)=c("id","prob","isPost","group","sex"); count = 0; #initialize
# isSocial.all = data.frame(matrix(NA, nrow = 0, ncol = 7)); colnames(isSocial.all)=c("id","prob","isPost","group","sex","groupsex","iter"); count = 0; #initialize
# isSocialQ.all = data.frame(matrix(NA, nrow = 0, ncol = 5)); colnames(isSocial.all)=c("id","prob","isPost","group","sex"); count = 0; #initialize
# 
# for (iter in 2:n_iter){
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
#   ###################################################
#   ## Proximity per quarter
#   
#   quarter = unique(ExSubScans$Q); isProxQ = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 4)); colnames(isProxQ)=c("id","prob","isPost","quarter"); count = 0;
#   for (q in 1:length(quarter)){
#     
#     ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
#     
#     for (id in 1:length(unqIDs)){ #For all individuals
#       id.all.pre = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
#       id.all.post = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
#       group = ExSubScansQ$group[which(ExSubScansQ$focalID == unqIDs[id])]#get group info for that individual
#       sex =  ExSubScansQ$sex[which(ExSubScansQ$focalID == unqIDs[id])]#get sex in
#       count = count+1
#       isProxQ$id[count] = unqIDs[id] #record ID
#       isProxQ$prob[count] = sum(id.all.pre)/length(id.all.pre)
#       isProxQ$isPost[count] = "pre"; isProxQ$quarter[count] = paste("Q",quarter[q],sep="")
#       isProxQ$group[count] = as.character(group[1]); isProxQ$sex[count] = as.character(sex[1])
#       count = count+1
#       isProxQ$id[count] = unqIDs[id] #record ID
#       isProxQ$prob[count] = sum(id.all.post)/length(id.all.post)
#       isProxQ$isPost[count] = "post"; isProxQ$quarter[count] = paste("Q",quarter[q],sep="")
#       isProxQ$group[count] = as.character(group[1]); isProxQ$sex[count] = as.character(sex[1])
#     }
#   }
#   isProxQ$iter=iter
#   isProxQ.all=rbind(isProxQ.all,isProxQ)
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
#   ###################################################
#   ## Grooming per quarter
#   
#   quarter = unique(ExSubScans$Q); isSocialQ = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 4)); colnames(isSocialQ)=c("id","prob","isPost","quarter"); count = 0;
#   for (q in 1:length(quarter)){
#     
#     ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
#     
#     for (id in 1:length(unqIDs)){ #For all individuals
#       id.all.pre = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
#       id.all.post = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
#       group = ExSubScansQ$group[which(ExSubScansQ$focalID == unqIDs[id])]#get group info for that individual
#       sex =  ExSubScansQ$sex[which(ExSubScansQ$focalID == unqIDs[id])]#get sex in
#       count = count+1
#       isSocialQ$id[count] = unqIDs[id] #record ID
#       isSocialQ$prob[count] = sum(id.all.pre)/length(id.all.pre)
#       isSocialQ$isPost[count] = "pre"; isSocialQ$quarter[count] = paste("Q",quarter[q],sep="")
#       isSocialQ$group[count] = as.character(group[1]); isSocialQ$sex[count] = as.character(sex[1])
#       count = count+1
#       isSocialQ$id[count] = unqIDs[id] #record ID
#       isSocialQ$prob[count] = sum(id.all.post)/length(id.all.post)
#       isSocialQ$isPost[count] = "post"; isSocialQ$quarter[count] = paste("Q",quarter[q],sep="")
#       isSocialQ$group[count] = as.character(group[1]); isSocialQ$sex[count] = as.character(sex[1])
#     }
#   }
#   isSocialQ$iter=iter
#   isSocialQ.all=rbind(isSocialQ.all,isSocialQ)
#   
#   save(isProx.all, isProxQ.all,isSocial.all,isSocialQ.all,file="ChangeP_figure1.RData")
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
isProxQ.all$isPost <- factor(isProxQ.all$isPost, levels = c("pre","post"))
isSocial.all$isPost <- factor(isSocial.all$isPost, levels = c("pre","post"))
isSocialQ.all$isPost <- factor(isSocialQ.all$isPost, levels = c("pre","post"))

#Remove NAs
isProxQ.all=isProxQ.all[!is.nan(isProxQ.all$prob),]
isSocialQ.all=isSocialQ.all[!is.nan(isSocialQ.all$prob),]

# #If only want to use one iteration
# iter = sample(500,1)
# isProx.all.plot=isProx.all[isProx.all$iter==iter,]
# isProxQ.all.plot=isProxQ.all[isProxQ.all$iter==iter,]
# isSocial.all.plot=isSocial.all[isSocial.all$iter==iter,]
# isSocialQ.all.plot=isSocialQ.all[isSocialQ.all$iter==iter,]

isProx.all.plot=isProx.all
isProxQ.all.plot=isProxQ.all
isSocial.all.plot=isSocial.all
isSocialQ.all.plot=isSocialQ.all

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

#########################
#Quarter differences 

#Plot change in p(Proximity), divided by quarter & group
pAccQ<-ggplot(isProxQ.all.plot, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin()+
  geom_boxplot(width=.1, outlier.shape = NA)+
  ggtitle("Change in proximity rates following\nHurricane Maria")+
  labs(fill = "Hurricane\nStatus",x="Hurricane Status",y="P(proximity)")+
  facet_grid(group~quarter)+theme_classic(base_size=20)
#Save plot
ggsave(file="Change.pProxQ.eps", plot=pAccQ)
ggsave(file="Change.pProxQ.tiff", plot=pAccQ)


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

#########################
#Quarter differences 

#isSocial per Quarter

#Plot change in p(Grooming), divided by quarter and group
pSocQ <- ggplot(isSocialQ.all.plot, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_violin()+
  geom_boxplot(width=.1, outlier.shape = NA)+
  ggtitle("Change in grooming rates following the hurricane")+
  labs(fill = "Hurricane\nStatus",x="Hurricane Status",y="P(grooming)")+
  facet_grid(group~quarter)+ylim(0,0.35)+theme_classic(base_size=20)
ggsave(file="Change.pSocQ.eps", plot=pSocQ)
ggsave(file="Change.pSocQ.tiff", plot=pSocQ)

beeswarm(prob ~ isPost + quarter, data = isSocialQ.all.plot, 
         log = TRUE, pch = 16, col = rainbow(2),
         main = 'beeswarm')

##############################################################################
#Plot longitudinal change in p(prox), p(groom)
##############################################################################

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPsoc/") 


# Change quarter to reflect passage of time
isProxQ.all.plot$quarter[which(isProxQ.all.plot$quarter=="Q1" & 
                                 isProxQ.all.plot$isPost=="post")]="Q5"
isProxQ.all.plot$quarter[which(isProxQ.all.plot$quarter=="Q2" & 
                                 isProxQ.all.plot$isPost=="post")]="Q6"
isProxQ.all.plot$quarter[which(isProxQ.all.plot$quarter=="Q3" & 
                                 isProxQ.all.plot$isPost=="post")]="Q7"
isProxQ.all.plot$quarter[which(isProxQ.all.plot$quarter=="Q4" & 
                                 isProxQ.all.plot$isPost=="post")]="Q8"

isSocialQ.all.plot$quarter[which(isSocialQ.all.plot$quarter=="Q1" & 
                                 isSocialQ.all.plot$isPost=="post")]="Q5"
isSocialQ.all.plot$quarter[which(isSocialQ.all.plot$quarter=="Q2" & 
                                 isSocialQ.all.plot$isPost=="post")]="Q6"
isSocialQ.all.plot$quarter[which(isSocialQ.all.plot$quarter=="Q3" & 
                                 isSocialQ.all.plot$isPost=="post")]="Q7"
isSocialQ.all.plot$quarter[which(isSocialQ.all.plot$quarter=="Q4" & 
                                 isSocialQ.all.plot$isPost=="post")]="Q8"

# plot.line.prox = isProxQ.all.plot %>%
#   group_by(quarter) %>%
#   summarize(median = median(prob, na.rm = TRUE))
# plot.line.prox$isPost[c(1:4)]="pre"; plot.line.prox$isPost[c(5:8)]="post";

#plot longitudinal change in social rates
long.prox<-ggplot(isProxQ.all.plot, aes(x=as.factor(quarter), y=prob, fill=as.factor(isPost)))+
  geom_violin(alpha=0.5, colour=NA)+
  geom_boxplot(width=.1, outlier.shape = NA)+
  geom_vline(xintercept = 4.5, colour="darkgreen", linetype="longdash",lwd=1)+
  ggtitle("Timecourse of changes in proximity rates")+
  labs(fill = "Hurricane\nStatus",x="Hurricane Status",y="P(proximity)")+
  facet_grid(~group)+theme_classic(base_size=20)+xlab('')+
  scale_x_discrete(labels= c("jan-mar.","apr-jun.","jul-sept.","oct-dec.",
                             "jan-mar.","apr-jun.","jul-sept.","oct-dec.")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1.1))+
  annotate("text",x=2, y=0.9, label="2015-2017",size=5,color='black')+
  annotate("text",x=5.2, y=0.9, label="2018",size=5,color='black')+
  annotate("text",x=5.5, y=1, label="Hurricane",size=5,color='darkgreen')
ggsave(file="long.pProx.jpg", plot=long.prox)
ggsave(file="long.pProx.svg", plot=long.prox)

lomg.groom<-ggplot(isSocialQ.all.plot, aes(x=as.factor(quarter), y=prob, fill=as.factor(isPost)))+
  geom_violin(alpha=0.5, colour=NA)+
  geom_boxplot(width=.1, outlier.shape = NA)+
  geom_vline(xintercept = 4.5, colour="darkgreen", linetype="longdash",lwd=1)+
  ggtitle("Timecourse of changes in grooming rates")+
  labs(fill = "Hurricane\nStatus",x="Hurricane Status",y="P(grooming)")+
  facet_grid(~group)+ylim(0,0.35)+theme_classic(base_size=20)+xlab('')+
  scale_x_discrete(labels= c("jan-mar.","apr-jun.","jul-sept.","oct-dec.",
                             "jan-mar.","apr-jun.","jul-sept.","oct-dec.")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1.1))+
  annotate("text",x=2, y=0.32, label="2015-2017",size=5,color='black')+
  annotate("text",x=5.2, y=0.32, label="2018",size=5,color='black')+
  annotate("text",x=5.5, y=0.35, label="Hurricane",size=5,color='darkgreen')
ggsave(file="long.pSoc.jpg", plot=lomg.groom)
ggsave(file="long.pSoc.svg", plot=lomg.groom)

# ##############################################################################
# #Plot correlation between change in prob proximity and change in prob grooming
# ##############################################################################
# 
# changeSocial = data.frame(matrix(NA, nrow = length(unqIDs), ncol = 3)); names(changeSocial)=c("id","dProx","dGroom")
# for (id in 1:length(unqIDs)){
#   changeSocial$id[id]=unqIDs[id] #id
#   dProx = isProx$prob[intersect(which(!is.na(match(isProx$id, unqIDs[id]))), which(isProx$isPost==1))] - #find change in pProx
#     isProx$prob[intersect(which(!is.na(match(isProx$id, unqIDs[id]))), which(isProx$isPost==0))]
#   dGroom = isSocial$prob[intersect(which(!is.na(match(isSocial$id, unqIDs[id]))), which(isSocial$isPost==1))] - #find change in pGroom
#     isSocial$prob[intersect(which(!is.na(match(isSocial$id, unqIDs[id]))), which(isSocial$isPost==0))]
#   if(length(dProx) + length(dGroom) == 2){ #if there are enough obsevrations to plot pProx and pGroom
#     changeSocial$dProx[id] = dProx
#     changeSocial$dGroom[id] = dGroom 
#   }
# }
# #Scatter plot to visualize whether there is a correlation between the change in pProx and change in pGroom
# ggscatter(changeSocial, x = "dProx", y = "dGroom", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Change in Proximity", ylab = "Change in Grooming",
#           title = "Correlation between dp(Prox) and dp(Groom)")
# 
