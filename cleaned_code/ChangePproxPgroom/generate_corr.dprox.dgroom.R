#generate_corr.dprox.dgroom
#Are the individuals who spend more time in proximity also those that change their grooming 
#freq. the most?

#Load libraries
library(ggplot2)
library(ggpubr)
library(matlab)

#Load data
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP.RData")

correl.coeff=vector(); iter=1
for (iter in 1:max(dprob.ALL$iter)){
  
  print(paste("%%%%%%%%%%%%%%%%%%%%%%%%%%%% iter", iter, " %%%%%%%%%%%%%%%%%%%%%%%%%%%%"))
  
  dprob.iter=dprob.ALL[dprob.ALL$iter==iter,]
  # #Plot change in prox vs. change in grooming
  # corr.plot <- ggplot(dprob.iter,aes(dpAcc, dpSocial)) +
  #   geom_point(color='blue')+
  #   xlab("Change in Proximity")+ylab("Change in Grooming")+
  #   ggtitle("Correlation between change in proximity and change in grooming")+
  #   geom_smooth(method='lm', formula= y~x)
  
  correl <- cor.test(dprob.iter$dpAcc,dprob.iter$dpSocial) #compute correlation
  correl.coeff[iter]=correl[["estimate"]]
}

#Find mean correlation coefficient and 95% CI
mean.corr = mean(correl.coeff)
CI = quantile(correl.coeff,probs=c(0.025, 0.975))
#Save plot
ggsave("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPSoc/Corr.pProx.PGroom.eps")
