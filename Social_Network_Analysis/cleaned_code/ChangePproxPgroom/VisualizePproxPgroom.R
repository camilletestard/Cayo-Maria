#Visualize pProx and pGroom
#This scripts aims to visualize how proximity and grooming changed pre-to-post huricane and whether there are inter-individual differences in
#how much individuals changed their affiliative behaviors

#Load libraries
library(ggplot2)
library(ggpubr)
library(matlab)

#Load AllScans file
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")

ExSubScans = calcRandomScans(allScans)
unqIDs = as.character(unique(ExSubScans$focalID))

###################################################################
#For probability of PROXIMITY:
###################################################################

#Compute p(prox) for each individual:
isProx = data.frame(matrix(NA, nrow = length(unqIDs)*2, ncol = 5)); colnames(isProx)=c("id","prob","isPost","group","sex"); count = 0; #initialize
for (id in 1:length(unqIDs)){ #For all individuals
  id.all.pre = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 0)] #get all pre-hurricane data for that individual
  id.all.post = ExSubScans$isProx[which(ExSubScans$focalID == unqIDs[id] & ExSubScans$isPost == 1)]#get all post-hurricane data for that individual
  group = ExSubScans$group[which(ExSubScans$focalID == unqIDs[id])]#get group info for that individual
  sex =  ExSubScans$sex[which(ExSubScans$focalID == unqIDs[id])]#get sex info for that individual
  if (length(id.all.pre)>=20) { #If there are at least 20 observations for that individual both pre-hurricane and post-hurricane
    count = count+1
    isProx$id[count] = unqIDs[id] #record ID
    isProx$prob[count] = sum(id.all.pre)/length(id.all.pre) #get proportion of observations this individual was seen in proximity to other partners PRE-hurricane
    isProx$isPost[count] = 0; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1]) #keep track of hurricane status, sex and group info
    count = count+1
    isProx$id[count] = unqIDs[id]
    isProx$prob[count] = sum(id.all.post)/length(id.all.post) #get proportion of observations this individual was seen in proximity to other partners POST-hurricane
    isProx$isPost[count] = 1; isProx$group[count] = as.character(group[1]); isProx$sex[count] = as.character(sex[1])
  }
}
isProx$groupsex = paste(isProx$group,isProx$sex,sep=".") #create group.sex column for later plotting
isProx=isProx[-which(is.na(isProx$prob)),] #remove all individuals with no proximity info (<20 observations)
#For later plotting keep track of increases and decreases in p(prox)
is_increasing = zeros(c(length(which(isProx$isPost==1)),1)); is_increasing[which((isProx$prob[which(isProx$isPost==1)] - isProx$prob[which(isProx$isPost==0)])>0)]=1
is_increasing = rep(is_increasing,times=ones(c(length(which(isProx$isPost==1)),1))*2) 

#Plot paired data pre- post:
ggplot(isProx, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.5)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+#,position = position_jitter(0.1))+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Proximity) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="p(Proximity)")

#Plot box plots of p(prox) pre/post-hurricane
pAcc<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Change in p(Proximity) post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Proximity)")

tiff("Change.pProx.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
pAcc
dev.off()

#Plot histogram of p(prox) pre/post-hurricane
hist(isProx$prob[which(isProx$isPost==0)],col=rgb(1,0,0,0.5), breaks=10, main="Individual p(Proximity) pre- to post-hurricane",xlab="p(Proximity)", xlim=c(0, 1))
hist(isProx$prob[which(isProx$isPost==1)],col=rgb(0,1,1,0.5), breaks=30, add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#########################
#Group differences

#Plot change in p(Proximity), divided by group
pAccG<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+ 
  ggtitle("Individual p(Proximity) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~group)
# Plot paired data, divided by group
ggplot(isProx, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Proximity) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="p(Proximity)")+
  facet_grid(~group)

#########################
#Sex differences

#Plot change in p(Proximity), divided by sex
pAccS<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Proximity) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~sex)

#Plot change in p(Proximity), divided by sex & group
pAccSG<-ggplot(isProx, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Proximity) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~groupsex)

#########################
#Year differences 

#isProx per year
years = unique(ExSubScans$year); isProxY = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isProxY)=c("prob","isPost","year"); count = 0;
for (y in 1:length(years)){
  
  ExSubScansY = ExSubScans[which(ExSubScans$year==years[y]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansY$isProx[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansY$isProx[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=20) { #If there are more than 10 observations for that individual 
      count = count+1
      isProxY$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProxY$isPost[count] = 0; isProxY$year[count] = years[y]
      count = count+1
      isProxY$prob[count] = sum(id.all.post)/length(id.all.post)
      isProxY$isPost[count] = 1; isProxY$year[count] = years[y]
    }
  }
}
isProxY=isProxY[-which(is.na(isProxY$prob)),]

#Plot change in p(Proximity), divided by year
pAccY<-ggplot(isProxY, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Proximity) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~year)

#########################
#Year differences 

#isProx per Quarter
quarter = unique(ExSubScans$Q); isProxQ = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isProxQ)=c("prob","isPost","quarter"); count = 0;
for (q in 1:length(quarter)){
  
  ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansQ$isProx[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=20) { #If there are more than 10 observations for that individual 
      count = count+1
      isProxQ$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProxQ$isPost[count] = 0; isProxQ$quarter[count] = paste("Q",quarter[q],sep="")
      count = count+1
      isProxQ$prob[count] = sum(id.all.post)/length(id.all.post)
      isProxQ$isPost[count] = 1; isProxQ$quarter[count] = paste("Q",quarter[q],sep="")
    }
  }
}
isProxQ=isProxQ[-which(is.na(isProxQ$prob)),]

#Plot change in p(Proximity), divided by quarter
pAccQ<-ggplot(isProxQ, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Proximity) pre- to post-hurricane, divided by quarter")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(proximity)")+
  facet_grid(~quarter)

#########################
#TimeBlock differences 

#isProx per timeBlock
TB = c("AM","PM"); isProxTB = data.frame(matrix(NA, nrow = length(unqIDs)*4, ncol = 4)); colnames(isProxTB)=c("prob","isPost","timeBlock"); count = 0;
for (tb in 1:2){
  
  ExSubScansTB = ExSubScans[which(as.character(ExSubScans$timeBlock)==TB[tb]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansTB$isProx[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansTB$isProx[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=20) { #If there are more than 10 observations for that individual 
      count = count+1
      isProxTB$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isProxTB$isPost[count] = 0; isProxTB$timeBlock[count] = TB[tb]
      count = count+1
      isProxTB$prob[count] = sum(id.all.post)/length(id.all.post)
      isProxTB$isPost[count] = 1; isProxTB$timeBlock[count] = TB[tb]
    }
  }
  #readline(prompt = "pause ")
}
isProxTB=isProxTB[-which(is.na(isProxTB$prob)),]

#Plot change in p(Proximity), divided by timeBlock
pAccTB<-ggplot(isProxTB, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Proximity) pre- to post-hurricane, divided by time block")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(Accompanied)")+
  facet_grid(~timeBlock)

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
    isSocial$isPost[count] = 0; isSocial$group[count] = as.character(group[1]); isSocial$sex[count] = as.character(sex[1])
    count = count+1
    isSocial$id[count] = unqIDs[id]
    isSocial$prob[count] = sum(id.all.post)/length(id.all.post) #get proportion of observations this individual was seen grooming other partners POST-hurricane
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
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="p(Grooming)")

#Plot box plots of p(groom) pre/post-hurricane
pSoc<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Change p(Grooming) post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="p(Grooming)")

tiff("Change.pGroom.tiff",units="in", width=5, height=4, res=300, compression = 'lzw')
pSoc
dev.off()

#Plot histogram of p(groom) pre/post-hurricane
hist(isSocial$prob[which(isSocial$isPost==0)],col=rgb(1,0,0,0.5), breaks=20,main="Individual p(Grooming) pre- to post-hurricane",xlab="p(Grooming)")
hist(isSocial$prob[which(isSocial$isPost==1)],col=rgb(0,1,1,0.5), breaks=20,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#########################
#Group differences

#Box plots separated by Group
pSocG<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~group)

#Paired data separated by group
ggplot(isSocial, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~group)

#Hitogram separated by group
#GroupV
hist(isSocial$prob[which(isSocial$isPost==0 & isSocial$group=="V")],col=rgb(1,0,0,0.5), breaks=20,main="GROUP V p(Soc) pre- to post-hurricane",xlab="p(Soc)")
hist(isSocial$prob[which(isSocial$isPost==1 & isSocial$group=="V")],col=rgb(0,1,1,0.5), breaks=30,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()
#GroupKK
hist(isSocial$prob[which(isSocial$isPost==0 & isSocial$group=="KK")],col=rgb(1,0,0,0.5), breaks=20,main="GROUP KK p(Soc) pre- to post-hurricane",xlab="p(Soc)")
hist(isSocial$prob[which(isSocial$isPost==1 & isSocial$group=="KK")],col=rgb(0,1,1,0.5), breaks=20,add=T)
legend("topright", c("Pre", "Post"), fill=c("red", "cyan"))
box()

#########################
#Sex differences 

#Box plots separated by Sex
pSocS<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Males show greater increase in grooming post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~sex)

#Paired data separated by Sex
ggplot(isSocial, aes(x = as.factor(isPost), y = prob, fill=as.factor(isPost))) +
  geom_boxplot(aes(group=as.factor(isPost)),alpha = 0.5, col="grey") +
  geom_point(alpha = 0.2)+#,position = position_jitter(0.1)) +
  geom_line(aes(group = id, col =as.factor(is_increasing)), alpha = 0.5)+
  scale_colour_manual(values = c("black", "red"))+
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status", col ="Increasing (0/1)", x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~sex)

#Separated by Sex & Group
pSocGS<-ggplot(isSocial, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~groupsex)

#########################
#Year differences 

#isSocial per year
years = unique(ExSubScans$year); isSocialY = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 4)); colnames(isSocialY)=c("prob","isPost","year"); count = 0;
for (y in 1:length(years)){
  
  ExSubScansY = ExSubScans[which(ExSubScans$year==years[y]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansY$isSocial[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansY$isSocial[which(ExSubScansY$focalID == unqIDs[id] & ExSubScansY$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=20) { #If there are more than 10 observations for that individual 
      count = count+1
      isSocialY$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isSocialY$isPost[count] = 0; isSocialY$year[count] = years[y]
      count = count+1
      isSocialY$prob[count] = sum(id.all.post)/length(id.all.post)
      isSocialY$isPost[count] = 1; isSocialY$year[count] = years[y]
    }
  }
  #readline(prompt = "pause ")
}
isSocialY=isSocialY[-which(is.na(isSocialY$prob)),]

#Plot change in p(Grooming), divided by year
ggplot(isSocialY, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~year)

#########################
#Quarter differences 

#isSocial per Quarter
quarter = unique(ExSubScans$Q); isSocialQ = data.frame(matrix(NA, nrow = length(unqIDs)*8, ncol = 3)); colnames(isSocialQ)=c("prob","isPost","quarter"); count = 0;
for (q in 1:length(quarter)){
  
  ExSubScansQ = ExSubScans[which(ExSubScans$Q==quarter[q]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansQ$isSocial[which(ExSubScansQ$focalID == unqIDs[id] & ExSubScansQ$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=20) { #If there are more than 10 observations for that individual 
      count = count+1
      isSocialQ$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isSocialQ$isPost[count] = 0; isSocialQ$quarter[count] = paste("Q",quarter[q],sep="")
      count = count+1
      isSocialQ$prob[count] = sum(id.all.post)/length(id.all.post)
      isSocialQ$isPost[count] = 1; isSocialQ$quarter[count] = paste("Q",quarter[q],sep="")
    }
  }
}
isSocialQ=isSocialQ[-which(is.na(isSocialQ$prob)),]

#Plot change in p(Grooming), divided by quarter
ggplot(isSocialQ, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Grooming) pre- to post-hurricane, divided by quarter")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="P(grooming)")+
  facet_grid(~quarter)

#########################
#TimeBlock differences 

#isSocial per timeBlock
TB = c("AM","PM"); isSocialTB = data.frame(matrix(NA, nrow = length(unqIDs)*4, ncol = 4)); colnames(isSocialTB)=c("prob","isPost","timeBlock"); count = 0;
for (tb in 1:2){
  
  ExSubScansTB = ExSubScans[which(as.character(ExSubScans$timeBlock)==TB[tb]),]
  
  for (id in 1:length(unqIDs)){ #For all individuals
    id.all.pre = ExSubScansTB$isSocial[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 0)] #get all pre-hurricane data for that individuals
    id.all.post = ExSubScansTB$isSocial[which(ExSubScansTB$focalID == unqIDs[id] & ExSubScansTB$isPost == 1)]#get all post-re-hurricane data for that individuals
    if (length(id.all.pre)>=10) { #If there are more than 10 observations for that individual 
      count = count+1
      isSocialTB$prob[count] = sum(id.all.pre)/length(id.all.pre)
      isSocialTB$isPost[count] = 0; isSocialTB$timeBlock[count] = TB[tb]
      count = count+1
      isSocialTB$prob[count] = sum(id.all.post)/length(id.all.post)
      isSocialTB$isPost[count] = 1; isSocialTB$timeBlock[count] = TB[tb]
    }
  }
  #readline(prompt = "pause ")
}
isSocialTB=isSocialTB[-which(is.na(isSocialTB$prob)),]

#Plot change in p(Grooming), divided by timeBlock
ggplot(isSocialTB, aes(x=as.factor(isPost), y=prob, fill=as.factor(isPost)))+
  geom_boxplot()+
  geom_jitter(position = position_jitter(0.2), alpha = 0.5)+
  ggtitle("Individual p(Grooming) pre- to post-hurricane")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="p(Grooming)")+
  facet_grid(~timeBlock)

##############################################################################
#Plot correlation between change in prob proximity and change in prob grooming

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

