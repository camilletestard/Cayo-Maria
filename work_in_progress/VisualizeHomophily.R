#Visualize Homophily
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/newP.Homophily2.RData")
newP.homophily.all$groupyear = paste(newP.homophily.all$group,newP.homophily.all$year,sep="")
groupyear = unique(newP.homophily.all$groupyear)
actions=c("groom","prox")

#Kinship
a=1; gy=1
for (a in 1:length(actions)){
  for (gy in 1:length(groupyear)){
    idx=which(newP.homophily.all$groupyear==groupyear[gy] & newP.homophily.all$action == actions[a])
    hist(newP.homophily.all$eo.ck[idx],breaks=30, col=rgb(1,0,0,0.5), xlim=c(0, 6), main=paste("Kinship",groupyear[gy],sep=" "))
    hist(newP.homophily.all$eo.u[idx],breaks=1,add=T, col=rgb(0,0,1,0.5))
  }
}

#Sex
idx.groom=which(newP.homophily.all$action == "groom")
idx.prox = which(newP.homophily.all$action == "prox")
hist(newP.homophily.all$assort.index.sex[idx.groom],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
     main=paste("Sex Homophily"), xlab="Assortativity Index")
hist(newP.homophily.all$assort.index.sex[idx.prox],breaks=10, col=rgb(0,0,1,0.5), xlim=c(-1, 1), add=T)

#Age
hist(newP.homophily.all$assort.index.age[idx.groom],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
     main=paste("Age Homophily"), xlab="Assortativity Index")
hist(newP.homophily.all$assort.index.age[idx.prox],breaks=10, col=rgb(0,0,1,0.5), xlim=c(-1, 1), add=T)

#Rank
hist(newP.homophily.all$assort.index.rank[idx.groom],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
     main=paste("Rank Homophily"), xlab="Assortativity Index")
hist(newP.homophily.all$assort.index.rank[idx.prox],breaks=10, col=rgb(0,0,1,0.5), xlim=c(-1, 1), add=T)

#Groom
hist(newP.homophily.all$assort.index.groom[idx.groom],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
     main=paste("Grooming Homophily"), xlab="Assortativity Index")
hist(newP.homophily.all$assort.index.groom[idx.prox],breaks=10, col=rgb(0,0,1,0.5), xlim=c(-1, 1), add=T)

#Num Partners
hist(newP.homophily.all$assort.index.numP[idx.groom],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
     main=paste("Number of Partners Homophily"), xlab="Assortativity Index")
hist(newP.homophily.all$assort.index.numP[idx.prox],breaks=10, col=rgb(0,0,1,0.5), xlim=c(-1, 1), add=T)


##################################################################################################
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ERGMeffects.RData")
groupyear = unique(ERGMeffects.ALL$groupyear)
actions=c("groom","prox")

gy=1
for (gy in 1:length(groupyear)){
  idx.groom.pre=which(ERGMeffects.ALL$groupyear==groupyear[gy] & ERGMeffects.ALL$action == "groom" & ERGMeffects.ALL$ispost == 0)
  idx.groom.post=which(ERGMeffects.ALL$groupyear==groupyear[gy] & ERGMeffects.ALL$action == "groom" & ERGMeffects.ALL$ispost == 1)
  idx.prox = which(ERGMeffects.ALL$groupyear==groupyear[gy] & ERGMeffects.ALL$action == "prox")
  hist(ERGMeffects.ALL$homoph.sexF[idx.groom.pre],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
       main=paste("Female Homophily ",groupyear[gy],sep=" "), xlab="Model Parameter for Female Homophily")
  hist(ERGMeffects.ALL$homoph.sexF[idx.groom.post],breaks=10, col=rgb(0,0,1,0.5), add=T)
}

#Set indices
idx.groom.pre=which(ERGMeffects.ALL$action == "groom" & ERGMeffects.ALL$ispost == 0)
idx.groom.post=which(ERGMeffects.ALL$action == "groom" & ERGMeffects.ALL$ispost == 1)

# homoph.cat = names(ERGMeffects.ALL)[6:ncol(ERGMeffects.ALL)]
hist(ERGMeffects.ALL$homoph.sexF[idx.groom.pre],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-2, 2), ylim=c(0, 150),
     main=paste("Female Homophily "), xlab="Model Parameter for Female Homophily")
hist(ERGMeffects.ALL$homoph.sexF[idx.groom.post],breaks=20, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Pre-Hurr", "Post-Hurr"), col=c("red", "blue"), lwd=10)

hist(ERGMeffects.ALL$homoph.sexM[idx.groom.pre],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-3, 2), 
     main=paste("Male Homophily"), xlab="Model Parameter for Male Homophily")
hist(ERGMeffects.ALL$homoph.sexM[idx.groom.post],breaks=30, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Pre-Hurr", "Post-Hurr"), col=c("red", "blue"), lwd=10)

hist(ERGMeffects.ALL$homoph.age[idx.groom.pre],breaks=10, col=rgb(1,0,0,0.5), xlim=c(-0.5, 0.5), 
     main=paste("Age Homophily "), xlab="Model Parameter for Age Homophily")
hist(ERGMeffects.ALL$homoph.age[idx.groom.post],breaks=10, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Pre-Hurr", "Post-Hurr"), col=c("red", "blue"), lwd=10)

hist(ERGMeffects.ALL$homoph.rank[idx.groom.pre],breaks=10, col=rgb(1,0,0,0.5), xlim=c(-0.1, 0.1), 
     main=paste("Rank Homophily "), xlab="Model Parameter for Rank Homophily")
hist(ERGMeffects.ALL$homoph.rank[idx.groom.post],breaks=10, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Pre-Hurr", "Post-Hurr"), col=c("red", "blue"), lwd=10)

hist(ERGMeffects.ALL$homoph.groom[idx.groom.pre],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
     main=paste("Groom Strength Homophily "), xlab="Model Parameter for Groom Homophily")
hist(ERGMeffects.ALL$homoph.groom[idx.groom.post],breaks=20, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Pre-Hurr", "Post-Hurr"), col=c("red", "blue"), lwd=10)

hist(ERGMeffects.ALL$homoph.numP[idx.groom.pre],breaks=20, col=rgb(1,0,0,0.5), xlim=c(-1, 1), 
     main=paste("Number Partners Homophily "), xlab="Model Parameter for num P. Homophily")
hist(ERGMeffects.ALL$homoph.numP[idx.groom.post],breaks=20, col=rgb(0,0,1,0.5), add=T)
legend("topright", c("Pre-Hurr", "Post-Hurr"), col=c("red", "blue"), lwd=10)

##################################################################################################
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/TERGMeffects.RData")
library(matrixStats)
library(gridExtra) 
library(graphics)

#Exclude outliers (due to mis-fit)
TERGMeffects.ALL=TERGMeffects.ALL[TERGMeffects.ALL$actions=="groom",]
TERGMeffects.ALL[TERGMeffects.ALL<=-5]=NA
TERGMeffects.ALL[TERGMeffects.ALL>=5]=NA
TERGMeffects.ALL[TERGMeffects.ALL==-Inf]=NA

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ERGM/")

#Plot distribution of parameters for bond formation model
tiff("TERGMcoeff.formation.tiff", 
     units="in", width=10, height=10, res=300, compression = 'lzw')

boxplot(TERGMeffects.ALL$form.triangle.close,TERGMeffects.ALL$form.mutual,
        TERGMeffects.ALL$form.homoph.sexF, TERGMeffects.ALL$form.homoph.sexM,TERGMeffects.ALL$form.homoph.age,
        TERGMeffects.ALL$form.homoph.rank, TERGMeffects.ALL$form.homoph.groom, TERGMeffects.ALL$form.homoph.numP,
        at=c(1,2,3,4,5,6,7,8),
        horizontal =T, main="Coefficient from TERGM Formation Model",
        xlab="coefficient",#range=0.9,
        names=c("triang.clos","recip.","fem.","male","age","rank","groom","num p."))
segments(0,0.2,0,8.8, col = "Red", lty=5, lwd=2, add=T)

dev.off()
# 
# boxplot(TERGMeffects.ALL$form.homoph.rank,
#         horizontal =T, main="Coefficient from TERGM Formation Model",
#         xlab="coefficient",range=0.9,
#         names=c("rank"))
# segments(0,0,0,10, col = "Red", lty=5, lwd=2, add=T)

Means = colMeans2(as.matrix(TERGMeffects.ALL[,4:12]), na.rm=T); Means = round(Means,2)
CI = colQuantiles(as.matrix(TERGMeffects.ALL[,4:12]), probs = c(0.05, 0.95), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); colnames(Estimates) = c("Estimate","2.5%","97.5%")
row.names(Estimates)=c("edges","triangle.closure","reciprocity","female","male","age","rank","groom","number.partners")
t.formation<-tableGrob(Estimates); t.formation<-grid.arrange(t.formation, top="Coefficient from TERGM Formation Model"); #create table, arrange table
write.csv(Estimates, file="TERGMcoeff.formation.csv")


#Plot distribution of parameters for bond dissolutiom model
tiff("TERGMcoeff.dissolution.tiff", 
     units="in", width=10, height=8, res=300, compression = 'lzw')

plot<-boxplot(TERGMeffects.ALL$diss.homoph.sexM,TERGMeffects.ALL$diss.homoph.age,
              TERGMeffects.ALL$diss.homoph.rank, TERGMeffects.ALL$diss.homoph.groom, TERGMeffects.ALL$diss.homoph.numP,
              at=c(1,2,3,4,5),
              horizontal =T, main="Coefficient from TERGM Dissolution Model",
              xlab="coefficient",#range=0.9,
              names=c("female","age","rank","groom","num p."))
segments(0,0.2,0,5.8, col = "Red", lty=5, lwd=2, add=T) 
#Note  homophily for males is always -Inf, which I think is because none of the bonds between male dissolve. I should Check.

dev.off()

Means = colMeans2(as.matrix(TERGMeffects.ALL[,c(13,14,16:19)]), na.rm=T); Means = round(Means,2)
CI = colQuantiles(as.matrix(TERGMeffects.ALL[,c(13,14,16:19)]), probs = c(0.05, 0.95), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); colnames(Estimates) = c("Estimate","2.5%","97.5%")
row.names(Estimates)=c("edges","female","age","rank","groom","number.partners")
t.formation<-tableGrob(Estimates); t.formation<-grid.arrange(t.formation, top="Coefficient from TERGM Formation Model"); #create table, arrange table
write.csv(Estimates, file="TERGMcoeff.dissolution.csv")