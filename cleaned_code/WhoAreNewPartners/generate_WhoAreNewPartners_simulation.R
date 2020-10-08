# Run simulations - power analysis for partner preference analysis: 

# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)
library(ggplot2)
library(data.table)
library(gridExtra)
library(matrixStats)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code/functions") 
source("CalcSubsampledScans.R")
source("functions_GlobalNetworkMetrics.R")
source("KinshipPedigree.R")

#Load scan data, population and dominance info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")


action = c("groom", "prox")
num_iter = 500
PartnerAttr = data.frame(matrix(ncol = 7, nrow = 0)); 
colnames(PartnerAttr)= c("action","iter","group","year","isPost", "Kin","Unrel")

start_time <- Sys.time(); iter=1; a=1

# for (a in 1:length(action)){
for (iter in 1:num_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% ",action[a], " iter",iter, " %%%%%%%%%%%%%%%%%%"))
  
  # 1. Calculate random subsamples
  randomScans = calcRandomScans(allScans)
  
  #Create age category for later
  age.thresh = as.numeric(quantile(randomScans$age,0.8))
  randomScans$age.cat="Y"; randomScans$age.cat[randomScans$age >= age.thresh]="O"
  
  #For each group, each year separately: 
  group = c("V","KK")
  
  g=1; y=1; h=1
  for (g in 1:length(group)){ #For each group
    randscansG = randomScans[which(randomScans$group==group[g]),] 
    
    years = unique(randscansG$year)
    for (y in 1:length(years)){ #For each year in that group
      randscansY = randscansG[which(randscansG$year==years[y]),] 
      year = years[y]
      
      isPost = c(0,1)
      for (h in 1:length(isPost)){ #pre- and post-hurricane 
        
        print(paste("%%%%%%%%%%%%%%%%%%",paste(group[g],years[y],isPost[h],sep="."), "%%%%%%%%%%%%%%%%%%"))
        
        rscans = randscansY[which(randscansY$isPost==isPost[h]),] 
        numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
        
        # 2. Find all unique IDs
        unqIDs = unique(c(as.character(rscans$focalID)))
        
        # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
        masterEL = calcMasterEL_groom(unqIDs)
        
        # 4. Output weighted edgelist from the Master Edgelist.
        options(warn = -1) #set options to ignore all warnings
        weightedEL = calcEdgeList_groom(rscans,masterEL)
        weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
        weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
        weightedEL = weightedEL[,c("alter","ego","count")]
        weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2 #take the average number of scans for each grooming partner
        weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by the #scans for each individual
        meanWeight = mean(weightedEL$weight[which(weightedEL$weight!=0)])
        weightedEL$weight <- weightedEL$weight/meanWeight #Normalize weights 
        weightedEL$count <- NULL;weightedEL$conc <- NULL; weightedEL$numscans <-NULL #delete those calumn variables
        
        #Only keep non-zero weight interactions
        weightedEL = weightedEL[which(weightedEL$weight>0),]

        # Set Kin relationship
        el=weightedEL
        el$KinPairClass <- "unrelated"
        for (i in 1:nrow(el)){
          number = sample(1000,1,F) #create number that we will use to artificially create networks with known effects
          # print(number)
          if (isPost[h] == 0 & number <=600) {el$KinPairClass[i] <- "rel"} #pre-hurricane 50% chance of being assigned kin
          if (isPost[h] == 1 & number <=475) {el$KinPairClass[i] <- "rel"} #post-hurricane 37.5% chance of being assigned kin
        }
        #Known effect size: change in proportion of time spent by 12.5%

        #Kin category
        Kin = length(which(el$KinPairClass == "rel"))/length(el$weight) #length(which(el$KinPairClass == "rel"))/nrow(el)
        Unrel = length(which(el$KinPairClass == "unrelated"))/length(el$weight)
        
        PartnerAttrDF = data.frame(matrix(nrow=1,ncol=7)); 
        names(PartnerAttrDF)= c("action","iter","group","year","isPost","Kin","Unrel")
        PartnerAttrDF[1,] <- c(action[a],iter,group[g],years[y],isPost[h], as.numeric(Kin),as.numeric(Unrel))
        
        PartnerAttr= rbind(PartnerAttr, PartnerAttrDF)  
        save(list="PartnerAttr",file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/PartnerAttributes_Sim.RData")
        
      }
    }
  }
}



#load & format data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/PartnerAttributes_Sim.RData")
PartnerAttr$groupyear = paste(PartnerAttr$group, PartnerAttr$year, sep="")
PartnerAttr$isPost = as.factor(PartnerAttr$isPost)
for (col in seq(6,7)) {PartnerAttr[,col] = as.numeric(PartnerAttr[,col])}

#Separate data by grooming and groomimity
data.groom = PartnerAttr[which(PartnerAttr$action=="groom"),]
data.groom.V=data.groom[data.groom$group=="V",];data.groom.KK=data.groom[data.groom$group=="KK",]
#Select columns of interest
data.V.pre = data.groom.V[data.groom.V$isPost==0,6:7]; data.V.post = data.groom.V[data.groom.V$isPost==1,6:7]; 
data.KK.pre = data.groom.KK[data.groom.KK$isPost==0,6:7]; data.KK.post = data.groom.KK[data.groom.KK$isPost==1,6:7]; 

##############################################
# TEST DIFFERENCE IN PROPORTIONS:

#GROUP V
data.V.diff = data.V.post-data.V.pre#Compute difference in proportions between pre- and post- for each subsampling iteration
Means = colMeans2(as.matrix(data.V.diff )); Means = round(Means,3) #Comput mean estimates
CI = colQuantiles(as.matrix(data.V.diff ), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute 95% CI (2.5 and 97.5 percentiles)
#Note: 95%CI is the 2-sided hypothesis testing approach and will be used for statistical significance
#To define a pre-/post- difference as trending, I compute the one-sided p-value
# = the proportion of difference distribution above or below zero.
binary_diff = data.V.diff<0; binary_diff=ifelse(binary_diff==T,1,0)
pval_neg=as.data.frame(colSums(binary_diff)/nrow(data.V.diff)) #proportion of distribution below zero
binary_diff = data.V.diff>0; binary_diff=ifelse(binary_diff==T,1,0)
pval_pos=as.data.frame(colSums(binary_diff)/nrow(data.V.diff))#proportion of distribution above zero
pval=apply(cbind(pval_neg,pval_pos), 1, FUN=min)#Use the smallest proportion as p-value (allows us to see which side is tested)
Estimates.V = cbind(Means, CI, pval); Estimates.V = as.data.frame(Estimates.V); names(Estimates.V) = c("Estimate","2.5%","97.5%","one-sided pval")
print(Estimates.V)

#GROUP KK
data.KK.diff = data.KK.post-data.KK.pre
Means = colMeans2(as.matrix(data.KK.diff )); Means = round(Means,3)
CI = colQuantiles(as.matrix(data.KK.diff ), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
binary_diff = data.KK.diff<0; binary_diff=ifelse(binary_diff==T,1,0)
pval_neg=as.data.frame(colSums(binary_diff)/nrow(data.V.diff))
binary_diff = data.KK.diff>0; binary_diff=ifelse(binary_diff==T,1,0)
pval_pos=as.data.frame(colSums(binary_diff)/nrow(data.V.diff))
pval=apply(cbind(pval_neg,pval_pos), 1, FUN=min)
Estimates.KK = cbind(Means,CI, pval); Estimates.KK = as.data.frame(Estimates.KK); names(Estimates.KK) = c("Estimate","2.5%","97.5%","one-sided pval")
print(Estimates.KK)

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/WhoAreNewPartners/") 

write.csv(Estimates.V,"groomPartnerPref.Stats.V .csv")
write.csv(Estimates.KK,"groomPartnerPref.Stats.KK.csv")

##############################################
#PLOTS/visualizations

data.KK.diff$group="KK"; data.V.diff$group="V"
data.diff.full =rbind(data.V.diff, data.KK.diff)

##################################################
# KINSHIP : plot %change ck/dk/unrel interactions pre-to-post hurr.
##################################################

## groom #

#groom Kin
tiff("Kin.change.125.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
kin<-ggplot(data.diff.full, aes(x= as.factor(group), y=as.numeric(Kin), fill=as.factor(group) ))+
  geom_violin()+
  geom_hline(yintercept=0, color = "red", linetype = "dashed")+
  ggtitle("Kin")+
  labs(fill = "Hurricane Status",x="Hurricane Status",y="Change in prop. of grooming pre/post")+scale_fill_manual(values=c("#E69F00", "#56B4E9")) + theme_classic(base_size=14)
# dev.off()
ggsave(kin, file ="changeKinPref_groom.png")
ggsave(kin, file ="changeKinPref_groom.eps")



 