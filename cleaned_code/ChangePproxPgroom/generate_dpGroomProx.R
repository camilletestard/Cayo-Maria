#Load data
setwd("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/")
source("cleaned_code/Functions/CalcSubsampledScans.R")
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/")
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")

#Set parameters:
num_iter = 500; iter =1
only2017=F; #if only considering 2017 (year just prior hurricane). Note: there is  no V2017 valid dp(groom) & dp(prox) because they all have less than 20 obs.
group = c("KK","KK","V", "V", "V")
years = c(2015,2017,2015,2016,2017)
groupyears = c("KK2015", "KK2017","V2015", "V2016", "V2017")

dprob.ALL = data.frame();
for (iter in 1:num_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  #####################################################################
  # 1. Compute change in p(Acc) and p(Social), per individual, per year
  #####################################################################
  
  #Calculate random subsamples
  randomScans = calcRandomScans(allScans)
  gy=1
  for (gy in 1:length(groupyears)){
    
    rscans = randomScans[which(randomScans$year == years[gy] & randomScans$group == group[gy]),]
    #Load data
    setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Behavioral_Data/Data All Cleaned")
    meta_data = read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt", sep = ""))
    
    unqIDs = as.character(meta_data$id)
    dprob=data.frame(matrix(NA, nrow=length(unqIDs),ncol=4)); colnames(dprob)=c("id","dpAcc","dpSocial","num_obs")
    for (id in 1:length(unqIDs)){ #For all individuals
      isProx.pre = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isProx.post = rscans$isProx[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      isSocial.pre = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 0)] #get all pre-hurricane data for that individuals
      isSocial.post = rscans$isSocial[which(as.character(rscans$focalID) == unqIDs[id] & rscans$isPost == 1)]#get all post-re-hurricane data for that individuals
      dpAcc=NA; dpSocial=NA; num_obs = length(isProx.pre)
      if (length(isProx.pre)>=20) { #If there are more than 10 observations for that individual
        pACC.pre = sum(isProx.pre)/length(isProx.pre)
        pACC.post = sum(isProx.post)/length(isProx.post)
        dpAcc = pACC.post - pACC.pre
        pSocial.pre = sum(isSocial.pre)/length(isSocial.pre)
        pSocial.post = sum(isSocial.post)/length(isSocial.post)
        dpSocial = pSocial.post - pSocial.pre
      } #end of min obs clause
      dprob[id,]=c(unqIDs[id],dpAcc,dpSocial,num_obs)
    } #end of id for loop
    dprob$group = group[gy]; dprob$year = years[gy]; dprob$iter=iter
    dprob.ALL = rbind(dprob.ALL, dprob)
  } #end of groupyear for loop
}

dprob.ALL$dpAcc=as.numeric(dprob.ALL$dpAcc)
dprob.ALL$dpSocial=as.numeric(dprob.ALL$dpSocial)
if (length(which(is.na(dprob.ALL$dpAcc)))!=0) {dprob.ALL = dprob.ALL[-which(is.na(dprob.ALL$dpAcc)),]} #remove NA
setwd("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data")
save(dprob.ALL,file="ChangeP.RData")

# load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP.RData")
# length(unique(dprob.ALL$id))
