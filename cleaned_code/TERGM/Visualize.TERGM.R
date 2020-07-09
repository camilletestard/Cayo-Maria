#Visualize TERGM
# Visualize in TERGM model paramaters (for both formation and dissolution) in two ways: 
#         1. horizontal box plot
#         2. tables of coefficients with 95% confidence intervals.

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/TERGMeffects3.RData")
library(matrixStats)
library(gridExtra) 
library(graphics)

#Exclude outliers (due to mis-fit)
# TERGMeffects.ALL = TERGMeffects.ALL[which(str_detect(TERGMeffects.ALL$groupyear,"KK")),]
# TERGMeffects.ALL=TERGMeffects.ALL[TERGMeffects.ALL$actions=="groom",]
TERGMeffects.ALL[TERGMeffects.ALL<=-5]=NA
TERGMeffects.ALL[TERGMeffects.ALL>=5]=NA
TERGMeffects.ALL[TERGMeffects.ALL==-Inf]=NA

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ERGM/")

#Plot distribution of parameters for bond formation model
tiff("TERGMcoeff.formation.tiff", 
     units="in", width=10, height=10, res=300, compression = 'lzw')

boxplot(TERGMeffects.ALL$form.triangle.close,TERGMeffects.ALL$form.reciprocity,
        TERGMeffects.ALL$form.kinship,TERGMeffects.ALL$form.prox,
        TERGMeffects.ALL$form.homoph.sexF, TERGMeffects.ALL$form.homoph.sexM,TERGMeffects.ALL$form.homoph.age,
        TERGMeffects.ALL$form.homoph.rank,TERGMeffects.ALL$form.homoph.numP,
        at=c(1,2,3,4,5,6,7,8,9),
        horizontal =T, main="Coefficient from TERGM Formation Model",
        xlab="coefficient",#range=0.9,
        names=c("triad.clos","recip.","kin.","prox","fem.","male","age","rank","greg"))
segments(0,0.2,0,10.8, col = "Red", lty=5, lwd=2)

dev.off()
# 
# boxplot(TERGMeffects.ALL$form.homoph.rank,
#         horizontal =T, main="Coefficient from TERGM Formation Model",
#         xlab="coefficient",range=0.9,
#         names=c("rank"))
# segments(0,0,0,10, col = "Red", lty=5, lwd=2, add=T)

Means = colMeans2(as.matrix(TERGMeffects.ALL[,3:13]), na.rm=T); Means = round(Means,2)
CI = colQuantiles(as.matrix(TERGMeffects.ALL[,3:13]), probs = c(0.05, 0.95), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); colnames(Estimates) = c("Estimate","2.5%","97.5%")
row.names(Estimates)=c("edges","triad.closure","reciprocity","kin","prox","female","male","age","rank","greg")
t.formation<-tableGrob(Estimates); t.formation<-grid.arrange(t.formation, top="Coefficient from TERGM Formation Model"); #create table, arrange table
write.csv(Estimates, file="TERGMcoeff.formation.csv")


#Plot distribution of parameters for bond dissolutiom model
tiff("TERGMcoeff.dissolution.tiff", 
     units="in", width=10, height=10, res=300, compression = 'lzw')

boxplot(TERGMeffects.ALL$diss.kinship,TERGMeffects.ALL$diss.prox,TERGMeffects.ALL$diss.homoph.sexF,TERGMeffects.ALL$diss.homoph.sexM,
              TERGMeffects.ALL$diss.homoph.age,TERGMeffects.ALL$diss.homoph.rank,
              TERGMeffects.ALL$diss.homoph.numP,
              at=c(1,2,3,4,5,6,7),
              horizontal =T, main="Coefficient from TERGM Dissolution Model",
              xlab="coefficient",#range=0.9,
              names=c("kin.","prox","female","male","age","rank","greg"))
segments(0,0.2,0,7.8, col = "Red", lty=5, lwd=2) 
#Note  homophily for males is always -Inf, which I think is because none of the bonds between male dissolve. I should Check.

dev.off()

Means = colMeans2(as.matrix(TERGMeffects.ALL[,c(14:21)]), na.rm=T); Means = round(Means,2)
CI = colQuantiles(as.matrix(TERGMeffects.ALL[,c(14:21)]), probs = c(0.05, 0.95), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); colnames(Estimates) = c("Estimate","2.5%","97.5%")
row.names(Estimates)=c("edges","kinship","female","male","age","rank","groom","number.partners")
t.formation<-tableGrob(Estimates); t.formation<-grid.arrange(t.formation, top="Coefficient from TERGM Formation Model"); #create table, arrange table
write.csv(Estimates, file="TERGMcoeff.dissolution.csv")