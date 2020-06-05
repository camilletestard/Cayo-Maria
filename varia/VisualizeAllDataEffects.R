#Vizualize P(Acc) & P(Social) effects
library(sjPlot)

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/ModelEffects_AllData.RData")

#print model table
tab_model(isNotAlone, transform=NULL)
tab_model(isNotAlonePM, transform=NULL)
tab_model(isNotAloneG, transform=NULL)
tab_model(isNotAloneS, transform=NULL)
tab_model(isNotAloneQ, transform=NULL)
tab_model(isSocial, transform=NULL)
tab_model(isSocialPM, transform=NULL)
tab_model(isSocialG, transform=NULL)
tab_model(isSocialS, transform=NULL)
tab_model(isSocialQ, transform=NULL)

#Load libraries
library(ggplot2)
library(ggpubr)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

ExSubScans = allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
unqIDs = as.character(unique(ExSubScans$focalID))

###################################################################
#For probability of being accompanied:
###################################################################

isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 4)); colnames(isProx)=c("prob","isPost","group","sex"); count = 0;
for (id in 1:length(unqIDs)){ #For all individuals
  
  if (length(which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0))!=0 
      & length(which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1))!=0) {
    
    id.all.pre = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]#get all post-re-hurricane data for that individuals
    group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
    sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
    if (length(id.all.pre)>=2 & length(id.all.post)>=2) { #If there are more than 10 observations for that individual 
      count = count+1
      isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProx$isPost[count] = 0; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
      count = count+1
      isProx$prob[count] = sum(id.all.post)/length(id.all.post)
      isProx$isPost[count] = 1; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    }
  }
}
isProx$groupsex = paste(isProx$group,isProx$sex,sep=".")
isProx=isProx[-which(is.na(isProx$prob)),]

#Plot paired data pre- post:
ProbPre=isProx$prob[which(isProx$isPost == 0)]; ProbPost=isProx$prob[which(isProx$isPost == 1)]; d<- data.frame(Pre=ProbPre, Post=ProbPost)
ggpaired(d, cond1 = "Pre",cond2 = "Post",fill = "condition", palette = "jco")

pdf(file="Change_P(Acc).pdf", width=5, height=5, onefile = T) #width and height of the graphics region in inches. One file if true = multiple graphs in one file

{ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")}

#Plot change in P(Acc), divided by group
{ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~group)}

#Plot change in P(Acc), divided by sex
{ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~sex)}

#Plot change in P(Acc), divided by sex & group
{ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~groupsex)}

#isProx per year
years = unique(ExSubScans$year); isProx = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 4)); colnames(isProx)=c("prob","isPost","year"); count = 0;
for (y in 1:length(years)){
  
  ExSubScansY = ExSubScans[which(ExSubScans$year==years[y]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansY$isProx[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansY$isProx[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProx$isPost[count] = 0; isProx$year[count] = years[y]
      count = count+1
      isProx$prob[count] = sum(id.all.post)/length(id.all.post)
      isProx$isPost[count] = 1; isProx$year[count] = years[y]
    }
  }
}
isProx=isProx[-which(is.na(isProx$prob)),]

#Plot change in P(Acc), divided by year
{ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~year)}

#isProx per Quarter
quarter = unique(ExSubScans$Q); isProx = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isProx)=c("prob","isPost","quarter"); count = 0;
for (q in 1:length(quarter)){
  
  ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProx$isPost[count] = 0; isProx$quarter[count] = quarter[q]
      count = count+1
      isProx$prob[count] = sum(id.all.post)/length(id.all.post)
      isProx$isPost[count] = 1; isProx$quarter[count] = quarter[q]
    }
  }
}
isProx=isProx[-which(is.na(isProx$prob)),]

#Plot change in P(Acc), divided by quarter
{ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane, divided by quarter")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~quarter)}

#isProx per timeBlock
TB = c("AM","PM"); isProx = data.frame(matrix(NA, nrow = length(unqIDs)*4, ncol = 4)); colnames(isProx)=c("prob","isPost","timeBlock"); count = 0;
for (tb in 1:2){
  
  ExSubScansTB = ExSubScans[which(as.character(ExSubScans$timeBlock)==TB[tb]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansTB$isProx[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansTB$isProx[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProx$isPost[count] = 0; isProx$timeBlock[count] = TB[tb]
      count = count+1
      isProx$prob[count] = sum(id.all.post)/length(id.all.post)
      isProx$isPost[count] = 1; isProx$timeBlock[count] = TB[tb]
    }
  }
  #readline(prompt = "pause ")
}
isProx=isProx[-which(is.na(isProx$prob)),]

#Plot change in P(Acc), divided by timeBlock
{ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane, divided by time block")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~timeBlock)}

###################################################################
#For probability of being social:
###################################################################

isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 4)); colnames(isSocial)=c("prob","isPost","group","sex"); count = 0;
for (id in 1:length(unqIDs)){
  id.all.pre = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  id.all.post = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  if (length(id.all.pre)>10) {
    count = count+1
    isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre)
    isSocial$isPost[count] = 0; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
    count = count+1
    isSocial$prob[count] = sum(id.all.post)/length(id.all.post)
    isSocial$isPost[count] = 1; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
  }
}
isSocial$groupsex = paste(isSocial$group,isSocial$sex,sep=".")
isSocial=isSocial[-which(is.na(isSocial$prob)),]

pdf(file="Change_P(Social).pdf", width=5, height=5, onefile = T) #width and height of the graphics region in inches. One file if true = multiple graphs in one file

{ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Social) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
    facet_grid(~group)}

{ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Social) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
    facet_grid(~sex)}

{ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Social) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
    facet_grid(~groupsex)}

#isSocial per year
years = unique(ExSubScans$year); isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 4)); colnames(isSocial)=c("prob","isPost","year"); count = 0;
for (y in 1:length(years)){
  
  ExSubScansY = ExSubScans[which(ExSubScans$year==years[y]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansY$isSocial[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansY$isSocial[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isSocial$isPost[count] = 0; isSocial$year[count] = years[y]
      count = count+1
      isSocial$prob[count] = sum(id.all.post)/length(id.all.post)
      isSocial$isPost[count] = 1; isSocial$year[count] = years[y]
    }
  }
  #readline(prompt = "pause ")
}
isSocial=isSocial[-which(is.na(isSocial$prob)),]

#Plot change in P(Social), divided by year
{ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Social) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
    facet_grid(~year)}

#isSocial per Quarter
quarter = unique(ExSubScans$Q); isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isSocial)=c("prob","isPost","quarter"); count = 0;
for (q in 1:length(quarter)){
  
  ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isSocial$isPost[count] = 0; isSocial$quarter[count] = quarter[q]
      count = count+1
      isSocial$prob[count] = sum(id.all.post)/length(id.all.post)
      isSocial$isPost[count] = 1; isSocial$quarter[count] = quarter[q]
    }
  }
}
isSocial=isSocial[-which(is.na(isSocial$prob)),]

#Plot change in P(Acc), divided by quarter
{ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Social) pre- to post-hurricane, divided by quarter")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~quarter)}

#isSocial per timeBlock
TB = c("AM","PM"); isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*4, ncol = 4)); colnames(isSocial)=c("prob","isPost","timeBlock"); count = 0;
for (tb in 1:2){
  
  ExSubScansTB = ExSubScans[which(as.character(ExSubScans$timeBlock)==TB[tb]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansTB$isSocial[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansTB$isSocial[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isSocial$isPost[count] = 0; isSocial$timeBlock[count] = TB[tb]
      count = count+1
      isSocial$prob[count] = sum(id.all.post)/length(id.all.post)
      isSocial$isPost[count] = 1; isSocial$timeBlock[count] = TB[tb]
    }
  }
  #readline(prompt = "pause ")
}
isSocial=isSocial[-which(is.na(isSocial$prob)),]

#Plot change in P(Social), divided by timeBlock
{ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Social) pre- to post-hurricane")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
    facet_grid(~timeBlock)}

dev.off()