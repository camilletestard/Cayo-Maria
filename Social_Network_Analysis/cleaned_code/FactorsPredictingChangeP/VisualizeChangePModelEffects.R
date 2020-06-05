#Vizualize change in p(Prox) & p(Groom) effects
#This script outputs density plots, mean and 95%CI of model paramaters, over n iterations.

library(ggplot2)
library(matrixStats)
library(gridExtra)
library(graphics)

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/ChangePModelEffects.RData")

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Results/PreHurricaneFactors-BehavioralFlex/") 

######################################################
#Output table of change in PROXIMITY model parameters

#1. Compute table estimate of BASE model parameters
Means = colMeans2(as.matrix(ChangeProx.BM)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeProx.BM), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.prox.BM<-tableGrob(Estimates); t.prox.BM<-grid.arrange(t.prox.BM, top="Model All Effects: p(Prox) Model Parameter Estimates"); #create table, arrange table

#2. Compute table estimate of NEED FOR CHANGE model parameters
Means = colMeans2(as.matrix(ChangeProx.NFC)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeProx.NFC), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.prox.NFC<-tableGrob(Estimates); t.prox.NFC<-grid.arrange(t.prox.NFC, top="Model All Effects: p(Prox) Model Parameter Estimates"); #create table, arrange table

#3. Compute table estimate of CAPACITY FOR CHANGE model parameters
Means = colMeans2(as.matrix(ChangeProx.CFC)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeProx.CFC), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.prox.CFC<-tableGrob(Estimates); t.prox.CFC<-grid.arrange(t.prox.CFC, top="Model All Effects: p(Prox) Model Parameter Estimates"); #create table, arrange table

#4. Compute table estimate of ALL model parameters
Means = colMeans2(as.matrix(ChangeProx.ALL)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeProx.ALL), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.prox.ALL<-tableGrob(Estimates); t.prox.ALL<-grid.arrange(t.prox.ALL, top="Model All Effects: p(Prox) Model Parameter Estimates"); #create table, arrange table

#combine all tables
FullPlot = grid.arrange(t.prox.BM, t.prox.NFC,t.prox.CFC,t.prox.ALL, ncol=4) #combine all three plots/tables

######################################################
#Output table of change in GROOMING model parameters

#1. Compute table estimate of BASE model parameters
Means = colMeans2(as.matrix(ChangeGroom.BM)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeGroom.BM), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Groom.BM<-tableGrob(Estimates); t.Groom.BM<-grid.arrange(t.Groom.BM, top="Model All Effects: p(Groom) Model Parameter Estimates"); #create table, arrange table

#2. Compute table estimate of NEED FOR CHANGE model parameters
Means = colMeans2(as.matrix(ChangeGroom.NFC)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeGroom.NFC), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Groom.NFC<-tableGrob(Estimates); t.Groom.NFC<-grid.arrange(t.Groom.NFC, top="Model All Effects: p(Groom) Model Parameter Estimates"); #create table, arrange table

#3. Compute table estimate of CAPACITY FOR CHANGE model parameters
Means = colMeans2(as.matrix(ChangeGroom.CFC)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeGroom.CFC), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Groom.CFC<-tableGrob(Estimates); t.Groom.CFC<-grid.arrange(t.Groom.CFC, top="Model All Effects: p(Groom) Model Parameter Estimates"); #create table, arrange table

#4. Compute table estimate of ALL model parameters
Means = colMeans2(as.matrix(ChangeGroom.ALL)); Means = round(Means,2)
CI = colQuantiles(as.matrix(ChangeGroom.ALL), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,3) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Groom.ALL<-tableGrob(Estimates); t.Groom.ALL<-grid.arrange(t.Groom.ALL, top="Model All Effects: p(Groom) Model Parameter Estimates"); #create table, arrange table

#combine all tables
FullPlot = grid.arrange(t.Groom.BM, t.Groom.NFC,t.Groom.CFC,t.Groom.ALL, ncol=4) #combine all three plots/tables
