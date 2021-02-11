#Visualize TERGM
# Visualize in TERGM model paramaters (for both formation and dissolution) in two ways: 
#  Remove outliers due to model misfits (coefficient is more than 3 std. deviations awat from mean or -Inf.
# (1)  horizontal violin plot
# (2)  tables of mean estimates for all coefficients & 95% confidence intervals.

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/TERGMeffects_minObs.RData")
library(matrixStats)
library(gridExtra) 
library(graphics)
library(vioplot)

#Exclude outliers in proximity column (due to misfits)
TERGMeffects.ALL$form.prox[TERGMeffects.ALL$form.prox==-Inf]=NA
outlier_threshold = mean(TERGMeffects.ALL$form.prox, na.rm=T) - sd(TERGMeffects.ALL$form.prox, na.rm=T)*3 #threshold 3 standard eviations away for the mean
TERGMeffects.ALL$form.prox[TERGMeffects.ALL$form.prox<=outlier_threshold]=NA

TERGMeffects.ALL$diss.prox[TERGMeffects.ALL$diss.prox==-Inf]=NA
outlier_threshold = mean(TERGMeffects.ALL$diss.prox, na.rm=T) - sd(TERGMeffects.ALL$diss.prox, na.rm=T)*3
TERGMeffects.ALL$diss.prox[TERGMeffects.ALL$diss.prox<=outlier_threshold]=NA

#Separate by groups
TERGMeffects.V = TERGMeffects.ALL[TERGMeffects.ALL$groupyear=="V2015"|TERGMeffects.ALL$groupyear=="V2016"|
                                          TERGMeffects.ALL$groupyear=="V2017",]
TERGMeffects.KK = TERGMeffects.ALL[TERGMeffects.ALL$groupyear=="KK2015"|
                                          TERGMeffects.ALL$groupyear=="KK2017",]

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ERGM/")

##### V #####

#Plot distribution of parameters for bond formation model
tiff("TERGMcoeff.formation.V_minObs.tiff", 
     units="in", width=10, height=10, res=300, compression = 'lzw')

vioplot(TERGMeffects.V$form.edge,TERGMeffects.V$form.triangle.close,
        TERGMeffects.V$form.reciprocity, TERGMeffects.V$form.prox,
        at=c(1,2,3,4),#outline=FALSE,
        horizontal =T, main="Coefficient from TERGM Formation Model (Group V)",
        cex.axis =2,cex.main =2,cex.names =1.75,
        names=c("density","triad clos.","recip.","proximity"))
segments(0,0.2,0,10.8, col = "Red", lty=5, lwd=2)

dev.off()

#Create table
Means = colMeans2(as.matrix(TERGMeffects.V[,3:6]), na.rm=T); Means = round(Means,2)
CI = colQuantiles(as.matrix(TERGMeffects.V[,3:6]), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); colnames(Estimates) = c("Estimate","2.5%","97.5%")
row.names(Estimates)=c("edges","triad.closure","reciprocity","prox")
t.formation<-tableGrob(Estimates); t.formation<-grid.arrange(t.formation, top="Coefficient from TERGM Formation Model"); #create table, arrange table
write.csv(Estimates, file="TERGMcoeff.formation.V_minObs.csv")

##### KK #####

#Plot distribution of parameters for bond formation model
tiff("TERGMcoeff.formation.KK.tiff", 
     units="in", width=10, height=10, res=300, compression = 'lzw')

vioplot(TERGMeffects.KK$form.edge,TERGMeffects.KK$form.triangle.close,
        TERGMeffects.KK$form.reciprocity, TERGMeffects.KK$form.prox,
        at=c(1,2,3,4),#outline=FALSE,
        horizontal =T, main="Coefficient from TERGM Formation Model (Group KK)",
        cex.axis =2,cex.main =2,cex.names =1.75,
        names=c("density","triad clos.","recip.","proximity"))
segments(0,0.2,0,10.8, col = "Red", lty=5, lwd=2)

dev.off()

#Create table
Means = colMeans2(as.matrix(TERGMeffects.KK[,3:6]), na.rm=T); Means = round(Means,2)
CI = colQuantiles(as.matrix(TERGMeffects.KK[,3:6]), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); colnames(Estimates) = c("Estimate","2.5%","97.5%")
row.names(Estimates)=c("edges","triad.closure","reciprocity","prox")
t.formation<-tableGrob(Estimates); t.formation<-grid.arrange(t.formation, top="Coefficient from TERGM Formation Model"); #create table, arrange table
write.csv(Estimates, file="TERGMcoeff.formation.KK_minObs.csv")