library(ggplot2)
library(scales)

#Load AllScans file
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#WITHOUT SUBSAMPLING
PrePostScans =  allScans[which(as.character(allScans$group) == "V" | as.character(allScans$group) == "KK"),]
SubScans=PrePostScans[-which(PrePostScans$year==2019),] #remove 2019
SubScans$groupyear = paste(SubScans$group, SubScans$year,sep="")

#WITH SUBSAMPLING
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("cleaned_code/Functions/CalcSubsampledScans.R")
ExSubScans = calcRandomScans(allScans);
unqIDs = as.character(unique(ExSubScans$focalID))

#GET NUMBER OF SCANS PER INDIVIDUAL (either based on sub-sampled data or not)
scans_per_id = as.data.frame(table(droplevels(SubScans$focalID), SubScans$year))
scans_per_id= scans_per_id[which(!is.na(match(scans_per_id$Var1,unqIDs))),]
scans_per_id = scans_per_id[scans_per_id$Freq!=0,]

# Get number of years per IDs:
idyear=rowSums(as.matrix(table(droplevels(scans_per_id$Var1), scans_per_id$Var2)))
mean(idyear); sd(idyear)
mean(scans_per_id$Freq[scans_per_id$Var2!=2018]); sd(scans_per_id$Freq[scans_per_id$Var2!=2018])
mean(scans_per_id$Freq[scans_per_id$Var2==2018]); sd(scans_per_id$Freq[scans_per_id$Var2==2018])

#Get number of observations by date
freqs <- aggregate(SubScans$date, by=list(SubScans$date, SubScans$group), FUN=length)
freqs$names <- as.Date(freqs$Group.1, format="%Y-%m-%d")
total_num.scans = sum(freqs$x)

ggplot(freqs, aes(x=names, y=x)) + geom_bar(stat="identity") +
  scale_x_date(breaks="4 months", labels=date_format("%Y-%b")) +
  ylab("Frequency") + xlab("Year and Month") + facet_grid(~Group.2)+
  theme_bw(base_size=20)+ theme(axis.text.x = element_text(angle = 75, hjust=1.1))

#PRE-HURRICANE NUMBER OF GROOMING BOUTS, FOCAL HOURS AND NUMBER OF SCANS PER IDYEAR IN OUR SAMPLE
#Load proximity scans from groups and years of interest in the focal format: 
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned") 
groupyears = c("V2015", "V2016", "V2017","KK2015", "KK2017")

gy=1; grooming_bouts_per_group=data.frame(matrix(data=NA, nrow=length(groupyears), ncol=0)); 
all_grooming_bouts = data.frame(matrix(data=NA, nrow=0, ncol=7)); 
names(all_grooming_bouts) = c("id","groupyear","num.groom.bouts","groom.time.in.s","unq.partners","hrs.followed","num.scans")
mean_hrs_followed = vector(); sd_hrs_followed = vector(); min_hrs_followed = vector();
mean_numScans = vector(); sd_numScans = vector(); min_numScans = vector();
mean_groomingBouts = vector(); sd_groomingBouts = vector()
for (gy in 1:length(groupyears)){ #for all groups & years
  
  meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
  groom_data = read.csv(paste("Group",groupyears[gy],"_GroomingEvents.txt", sep = "")) #load prox data from groupyear gy
  
  grooming_bouts_per_group$group[gy]=groupyears[gy]
  grooming_bouts_per_group$total_bouts[gy]= nrow(groom_data)
  grooming_bouts_per_group$num_focals[gy]=length(!is.na(match(meta_data$id,unqIDs)))
  
  id_list = as.character(meta_data$id[match(unqIDs,meta_data$id)])
  IDs = id_list[!is.na(id_list)]; grooming_bouts = data.frame(matrix(data=NA, nrow=length(IDs), ncol=7)); 
  names(grooming_bouts) = c("id","groupyear","num.groom.bouts","groom.time.in.s","unq.partners","hrs.followed","num.scans");id=1
  for (id in 1:length(IDs)){
    grooming_bouts$id[id] = IDs[id]; grooming_bouts$groupyear = groupyears[gy]
    idx=which(groom_data$groom_giver== IDs[id] | groom_data$groom_reciever== IDs[id])
    grooming_bouts$num.groom.bouts[id] = length(idx)
    grooming_bouts$groom.time.in.s[id] = sum(groom_data$constrained_duration[idx])
    grooming_bouts$unq.partners[id] = length(unique(c(as.character(groom_data$groom_giver[idx]), as.character(groom_data$groom_reciever[idx]))))
    grooming_bouts$hrs.followed[id] = meta_data$hrs.focalfollowed[meta_data$id==IDs[id]]
    grooming_bouts$num.scans[id] = length(which(SubScans$focalID == IDs[id] & SubScans$groupyear == groupyears[gy]))
  }
  
  all_grooming_bouts = rbind(all_grooming_bouts, grooming_bouts)
  mean_hrs_followed[gy]=mean(grooming_bouts$hrs.followed) #mean(meta_data$hrs.focalfollowed)
  sd_hrs_followed[gy]=sd(grooming_bouts$hrs.followed)
  min_hrs_followed[gy]=min(grooming_bouts$hrs.followed)
  
  mean_numScans[gy]=mean(grooming_bouts$num.scans) #mean(meta_data$hrs.focalfollowed)
  sd_numScans[gy]=sd(grooming_bouts$num.scans)
  min_numScans[gy]=min(grooming_bouts$num.scans)
  
  mean_groomingBouts[gy]=mean(grooming_bouts$num.groom.bouts) #mean(meta_data$hrs.focalfollowed)
  sd_groomingBouts[gy]=sd(grooming_bouts$num.groom.bouts)
}

save(mean_hrs_followed, sd_hrs_followed, mean_numScans, sd_numScans, 
     file = "C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/DataPerGroup.RData")

mean(all_grooming_bouts$num.groom.bouts); sd(all_grooming_bouts$num.groom.bouts)
mean(all_grooming_bouts$hrs.followed); sd(all_grooming_bouts$hrs.followed) ; range(all_grooming_bouts$hrs.followed)
mean(all_grooming_bouts$num.scans); sd(all_grooming_bouts$num.scans); range(all_grooming_bouts$num.scans)

#Plot distribution of samples across individuals
ggplot(all_grooming_bouts, aes(x=num.scans, fill=groupyear))+
  geom_histogram(color="#e9ecef")+facet_grid(~groupyear)+ theme_classic(base_size = 20)+ xlab('# scans')+
  theme(axis.text.x=element_text(color = "black", size=15, angle=40, vjust=.8, hjust=0.8)) 

ggplot(all_grooming_bouts, aes(x=hrs.followed, fill=groupyear))+
  geom_histogram(color="#e9ecef")+facet_grid(~groupyear)+ theme_classic(base_size = 20)+ xlab('# hours followed')+
  theme(axis.text.x=element_text(color = "black", size=15, angle=40, vjust=.8, hjust=0.8)) 

#Restricting to only individuals with a lot of data?
length(which(all_grooming_bouts$hrs.followed>4))

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results")
#Correlation between hrs followed and number of grooming bouts
plot(all_grooming_bouts$hrs.followed, all_grooming_bouts$num.groom.bouts)
cor.test(all_grooming_bouts$num.groom.bouts,all_grooming_bouts$hrs.followed)

#Correlation between #scans and number of grooming bouts
ggplot(all_grooming_bouts, aes(x=num.scans, y=num.groom.bouts))+
  geom_point()+ xlab('# of scans')+ ylab('# grooming bouts')+
geom_smooth(method = lm)+ theme_classic(base_size=20)
cor.test(all_grooming_bouts$num.scans,all_grooming_bouts$num.groom.bouts)
ggsave('NumScans_vs_GroomingBouts.tiff')

#Correlation between #scans and number of unq partners
ggplot(all_grooming_bouts, aes(x=num.scans, y=unq.partners))+
  geom_point()+ xlab('# of scans')+ ylab('# Unique partners')+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  geom_vline(xintercept=60, linetype="dashed",lwd=1.5)+
  #geom_smooth(method = lm)+ 
  theme_classic(base_size=20)
cor.test(all_grooming_bouts$unq.partners,all_grooming_bouts$num.scans)
ggsave('NumScans_vs_UnqPartners.tiff')

data_below_thresh = all_grooming_bouts[which(all_grooming_bouts$num.scans<60),]
IDs.to.exclude = unique(data_below_thresh$id)
ids_info=data_below_thresh[match(IDs.to.exclude, data_below_thresh$id),]
table(ids_info$groupyear)

data_above_thresh = all_grooming_bouts[which(all_grooming_bouts$num.scans>=60),]
ggplot(data_above_thresh, aes(x=num.scans, y=unq.partners))+
  geom_point()+ xlab('# of scans')+ ylab('# Unique partners')+
  geom_smooth(method = lm)+
  theme_classic(base_size=20)
cor.test(data_above_thresh$unq.partners,data_above_thresh$num.scans)

#Correlation between hrs followed and number of unq partners
plot(all_grooming_bouts$hrs.followed, all_grooming_bouts$unq.partners)
cor.test(all_grooming_bouts$unq.partners,all_grooming_bouts$hrs.followed)

#####################################################################
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP_min40.RData")
dprob.ALL$groupyear=paste(dprob.ALL$group, dprob.ALL$year,sep="")
dprob.ALL$idyear=paste(dprob.ALL$id, dprob.ALL$groupyear,sep="")
IDs_minObs=unique(dprob.ALL$idyear)
all_grooming_bouts$idyear = paste(all_grooming_bouts$id, all_grooming_bouts$groupyear,sep="")
IDs_to_exclude = setdiff(all_grooming_bouts$idyear, IDs_minObs)

data_minObs=all_grooming_bouts[match(IDs_minObs, all_grooming_bouts$idyear),]
mean(data_minObs$num.scans)
sd(data_minObs$num.scans)
min(data_minObs$num.scans)

for (gy in 1:length(groupyears)){ #for all groups & years
  data = data_minObs[data_minObs$groupyear == groupyears[gy],]
  
  mean_hrs_followed[gy]=mean(data$hrs.followed) #mean(meta_data$hrs.focalfollowed)
  sd_hrs_followed[gy]=sd(data$hrs.followed)
  
  mean_numScans[gy]=mean(data$num.scans) #mean(meta_data$hrs.focalfollowed)
  sd_numScans[gy]=sd(data$num.scans)
}
save(mean_hrs_followed, sd_hrs_followed, mean_numScans, sd_numScans, 
     file = "C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/DataPerGroup_minObs.RData")

######
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/Networks_minObs.RData")
Networks$groupyear=paste(Networks$group, Networks$year,sep="")
Networks$alteryear=paste(Networks$alter, Networks$groupyear,sep="")
Networks$egoyear=paste(Networks$ego, Networks$groupyear,sep="")
idyear = unique(c(Networks$alteryear, Networks$egoyear))

data_minObs=all_grooming_bouts[match(idyear, all_grooming_bouts$idyear),]
mean(data_minObs$num.scans)
min(data_minObs$num.scans)

######
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/Strength.StablePartners_minOBs.RData")
ID.strength.stableP.All$groupyear=paste(ID.strength.stableP.All$group, ID.strength.stableP.All$year,sep="")
ID.strength.stableP.All$idyear=paste(ID.strength.stableP.All$id, ID.strength.stableP.All$groupyear,sep="")
IDs_minObs=unique(ID.strength.stableP.All$idyear)
all_grooming_bouts$idyear = paste(all_grooming_bouts$id, all_grooming_bouts$groupyear,sep="")
IDs_to_exclude = setdiff(all_grooming_bouts$idyear, IDs_minObs)

data_minObs=all_grooming_bouts[match(IDs_minObs, all_grooming_bouts$idyear),]
mean(data_minObs$num.scans)
sd(data_minObs$num.scans)
min(data_minObs$num.scans)
