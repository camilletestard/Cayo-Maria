#Find common IDs across years

#For group V: 
groupyears =c("V2016","V2017","V2018","V2019"); gy=1
meta_data=list()
for (gy in 1:length(groupyears)){ #For each group
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data[[gy]]=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
}
commonIDsV=intersect(intersect(intersect(meta_data[[1]]$id,meta_data[[2]]$id),meta_data[[3]]$id),meta_data[[4]]$id)

#For group KK: 
groupyears =c("KK2015","KK2017","KK2018"); gy=1
meta_data=list()
for (gy in 1:length(groupyears)){ #For each group
  #Load data
  setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Behavioral_Data/Data All Cleaned") 
  meta_data[[gy]]=read.csv(paste("Group",groupyears[gy],"_GroupByYear.txt",sep=""))
}
commonIDsKK=intersect(intersect(meta_data[[1]]$id,meta_data[[2]]$id),meta_data[[3]]$id)

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
save(commonIDsKK,commonIDsV,file="commonIDs.Rdata")
s
