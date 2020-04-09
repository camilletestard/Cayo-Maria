#Load libraries
library(ggplot2)
library(ggpubr)
library(matlab)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

ExSubScans = calcRandomScans(allScans)
unqIDs = as.character(unique(ExSubScans$focalID))

###################################################################
#For probability of being accompanied:
###################################################################

isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isProx)=c("id","prob","isPost","group","sex"); count = 0;
for (id in 1:length(unqIDs)){ #For all individuals
  id.all.pre = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #get all pre-hurricane data for that individuals
  id.all.post = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]#get all post-re-hurricane data for that individuals
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
  if (length(id.all.pre)>=10) { #If there are more than 20 observations for that individual 
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.pre)/length(id.all.pre)
    isProx$isPost[count] = 0; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.post)/length(id.all.post)
    isProx$isPost[count] = 1; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
  }
}
isProx$groupsex = paste(isProx$group,isProx$sex,sep=".")
isProx=isProx[-which(is.na(isProx$prob)),]
#For later plotting keep track of increases and decreases.
is_increasing = zeros(c(length(which(isProx$isPost==1)),1)); is_increasing[which((isProx$prob[which(isProx$isPost==1)] - isProx$prob[which(isProx$isPost==0)])>0)]=1
is_increasing = rep(is_increasing,times=ones(c(length(which(isProx$isPost==1)),1))*2) 

#Plot paired data pre- post:
ggplot(isProx, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="P(Acc)")

pAcc<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Proximity)")

hist(isProx$prob[which(isProx$isPost==0)],col=rgb(1,0,0,0.5), breaks=30, main="Individual p(Acc) pre- to post-hurricane",xlab="p(Acc)", xlim=c(0, 1))
hist(isProx$prob[which(isProx$isPost==1)],col=rgb(0,1,1,0.5), breaks=30, add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#Plot change in P(Acc), divided by group
pAccG<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+ 
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~group)
# Plot paired data
ggplot(isProx, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="P(Acc)")+
  facet_grid(~group)

#Plot change in P(Acc), divided by sex
pAccS<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~sex)

#Plot change in P(Acc), divided by sex & group
pAccSG<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~groupsex)

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
pAccY<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Acc) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~year)

#isProx per Quarter
quarter = unique(ExSubScans$Q); isProx = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isProx)=c("prob","isPost","quarter"); count = 0;
for (q in 1:length(quarter)){
  
  ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=5) { #If there are more than 10 observations for that individual 
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
pAccQ<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
    geom_boxplot()+
    geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
    ggtitle("Individual p(Acc) pre- to post-hurricane, divided by quarter")+
    labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
    facet_grid(~quarter)

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
pAccTB<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Acc) pre- to post-hurricane, divided by time block")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~timeBlock)

###################################################################
#For probability of being social:
###################################################################

isSocial = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isSocial)=c("id","prob","isPost","group","sex"); count = 0;
for (id in 1:length(unqIDs)){
  id.all.pre = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  id.all.post = ExSubScans$isSocial[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)]
  if (length(id.all.pre)>20) {
    count = count+1
    isSocial$id[count] = unqIDs[id]
    isSocial$prob[count] = sum(id.all.pre)/length(id.all.pre)
    isSocial$isPost[count] = 0; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
    count = count+1
    isSocial$id[count] = unqIDs[id]
    isSocial$prob[count] = sum(id.all.post)/length(id.all.post)
    isSocial$isPost[count] = 1; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
  }
}
isSocial$groupsex = paste(isSocial$group,isSocial$sex,sep=".")
isSocial=isSocial[-which(is.na(isSocial$prob)),]

#For later plotting keep track of increases and decreases.
is_increasing = zeros(c(length(which(isSocial$isPost==1)),1)); 
is_increasing[which((isSocial$prob[which(isSocial$isPost==1)] - isSocial$prob[which(isSocial$isPost==0)])>0)]=1
is_increasing = rep(is_increasing,times=ones(c(length(which(isSocial$isPost==1)),1))*2) 

#Plot paired data pre- post:
ggplot(isSocial, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="P(Social)")

pSoc<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")
  
hist(isSocial$prob[which(isSocial$isPost==0)],col=rgb(1,0,0,0.5), breaks=20,main="Individual p(Soc) pre- to post-hurricane",xlab="p(Soc)")
hist(isSocial$prob[which(isSocial$isPost==1)],col=rgb(0,1,1,0.5), breaks=20,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#Separated by Group
pSocG<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
  facet_grid(~group)

ggplot(isSocial, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="P(Social)")+
  facet_grid(~group)

hist(isSocial$prob[which(isSocial$isPost==0 & isSocial$group=="V")],col=rgb(1,0,0,0.5), breaks=20,main="GROUP V p(Soc) pre- to post-hurricane",xlab="p(Soc)")
hist(isSocial$prob[which(isSocial$isPost==1 & isSocial$group=="V")],col=rgb(0,1,1,0.5), breaks=30,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()
hist(isSocial$prob[which(isSocial$isPost==0 & isSocial$group=="KK")],col=rgb(1,0,0,0.5), breaks=20,main="GROUP KK p(Soc) pre- to post-hurricane",xlab="p(Soc)")
hist(isSocial$prob[which(isSocial$isPost==1 & isSocial$group=="KK")],col=rgb(0,1,1,0.5), breaks=20,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#Separated by Sex
pSocS<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
  facet_grid(~sex)

ggplot(isSocial, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="P(Social)")+
  facet_grid(~sex)

#Separated by Sex & Group
pSocGS<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Social) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Social)")+
  facet_grid(~groupsex)

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