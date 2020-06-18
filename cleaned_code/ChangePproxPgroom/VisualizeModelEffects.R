#Vizualize p(Prox) & p(Groom) effects
#This script outputs density plots, mean and 95%CI of model paramaters, over n iterations.
# Input generated in "Modeling_PaccPsoc".
# Input: ModelEffectsFinal.RData (output of script3)
# Output: Density plot and tables.

library(ggplot2)
library(matrixStats)
library(gridExtra) 
library(graphics)

load("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Social_Network_Analysis/ModelEffectsFinal.RData")
rm(list=setdiff(ls(),c("NotAlone.R.Effects", "NotAlone.A.Effects", "NotAlone.G.Effects","NotAlone.KK.Effects","NotAlone.V.Effects",
                       "NotAlone.S.Effects", "NotAlone.Q.Effects","NotAlone.PM.Effects","NotAloneEffects","Social.G.Effects",
                       "Social.KK.Effects", "Social.V.Effects","Social.S.Effects","Social.Q.Effects","Social.PM.Effects","SocialEffects",
                       "NotAlone.M.Effects","NotAlone.F.Effects","Social.M.Effects", "Social.F.Effects", "Social.R.Effects", "Social.A.Effects")))

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPSoc/") 

#1. Show the distribution of the Hurricane effect on p(Prox) and p(Groom) considering all data: 
isPostBM<- ggplot(NotAloneEffects,aes(x=isPost))+ #set main ggplot parameters
  geom_density(aes(fill = "p(Prox)"))+ #density plot1
  geom_density(data = SocialEffects, aes(x=isPost,fill = "p(Groom)"))+ #density plot 2
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+ #add 0-line
  ggtitle("Size of Hurricane Effect on p(Prox) & p(Groom)")+ #add title
  labs(fill="Independent Variable",x="isPost Parameter",y="Density")+ #add labels
  xlim(-1,2)+ ylim(0, 15) #add plot limits 

Means = colMeans2(as.matrix(NotAloneEffects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAloneEffects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc<-tableGrob(Estimates); t.Acc<-grid.arrange(t.Acc, top="Base Model: p(Prox) Model Parameter Estimates"); #create table, arrange table

Means = colMeans2(as.matrix(SocialEffects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(SocialEffects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc<-tableGrob(Estimates); t.Soc <- grid.arrange(t.Soc, top="Base Model: p(Groom) Parameter Estimates");

FullPlot = grid.arrange(t.Acc, t.Soc,isPostBM, ncol=3) #combine all three plots/tables

ggsave(FullPlot, file = paste("Base Model Parameter Estimates and CI.png",sep=""))
ggsave(FullPlot, file = paste("eps.format/Base Model Parameter Estimates and CI.eps",sep=""))


#2. Considering only PM data
isPostAcc<-ggplot(NotAloneEffects,aes(x=isPost))+
  geom_density(aes(fill = "All Data"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = NotAlone.PM.Effects, aes(x=isPost, fill = "PM only"))+
  ggtitle("Size of Hurricane Effect on p(Prox): AllData vs. PM only")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Dataset")+
  xlim(-1,2)+ ylim(0, 25)

isPostSoc<-ggplot(SocialEffects,aes(x=isPost))+
  geom_density(aes(fill = "All Data"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = Social.PM.Effects, aes(x=isPost, fill = "PM only"))+
  ggtitle("Size of Hurricane Effect on p(Groom): AllData vs. PM only")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Dataset")+
  xlim(-1,2)+ ylim(0, 20)

Means = colMeans2(as.matrix(NotAlone.PM.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.PM.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.PM<-tableGrob(Estimates);t.Acc.PM<-grid.arrange(t.Acc.PM, top="p(Prox) PM-only Model Parameter Estimates");

Means = colMeans2(as.matrix(Social.PM.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.PM.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.PM<-tableGrob(Estimates);t.Soc.PM<-grid.arrange(t.Soc.PM, top="p(Groom) PM-only Model Parameter Estimates");

FullPlot = grid.arrange(isPostAcc,isPostSoc, t.Acc.PM, t.Soc.PM, ncol=2, nrow=2)

ggsave(FullPlot, file = paste("PM Model Parameter Estimates and CI.png",sep=""))

#3. Does it differ between groups:
isPostGroup <- ggplot(NotAlone.G.Effects,aes(x=`isPost:groupV`))+
    geom_density(aes(fill = "p(Prox)" ))+
    geom_density(data = Social.G.Effects, aes(x=`isPost:groupV`, fill = "p(Groom)"))+
    geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
    ggtitle("Interaction Hurricane:GroupV Effect")+
    labs(x = "isPost:groupV paramater",
         y = "Density",
         fill = "Independent Variable")+
    xlim(-2.5,2)+ ylim(0, 10)

Means = colMeans2(as.matrix(NotAlone.G.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.G.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.G<-tableGrob(Estimates);t.Acc.G<-grid.arrange(t.Acc.G, top="p(Prox) Model by group Parameter Estimates");

Means = colMeans2(as.matrix(Social.G.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.G.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.G<-tableGrob(Estimates);t.Soc.G<-grid.arrange(t.Soc.G, top="p(Groom) Model by group Parameter Estimates");

FullPlot = grid.arrange(t.Acc.G, t.Soc.G, isPostGroup, ncol=3)

ggsave(FullPlot, file = paste("Group Model Parameter Estimates and CI.png",sep=""))


# KK-only & V-only Model paramter isPost comparison for proximty rate
isProxKKV <- ggplot(NotAlone.KK.Effects,aes(x=isPost))+
  geom_density(aes(fill = "KK" ))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = NotAlone.V.Effects, aes(x=isPost, fill = "V"))+
  ggtitle("Hurricane Effect on p(Prox): KK vs. V")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Group")+
  xlim(-3,3)+ ylim(0, 15)

#KK-only & V-only Model paramter isPost comparison for grooming rate
isGroomKKV <- ggplot(Social.KK.Effects,aes(x=isPost))+
  geom_density(aes(fill = "KK" ))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = Social.V.Effects, aes(x=isPost, fill = "V"))+
  ggtitle("Hurricane Effect on p(Groom): KK vs. V")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Group")+
  xlim(-1,1)+ ylim(0, 15)

Means = colMeans2(as.matrix(NotAlone.KK.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.KK.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.KK<-tableGrob(Estimates);{grid.arrange(t.Acc.KK, top="p(Prox) KK-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.KK.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.KK.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.KK<-tableGrob(Estimates);{grid.arrange(t.Soc.KK, top="p(Groom) KK-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(NotAlone.V.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.V.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.V<-tableGrob(Estimates);{grid.arrange(t.Acc.V, top="p(Prox) V-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.V.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.V.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.V<-tableGrob(Estimates);{grid.arrange(t.Soc.V, top="p(Groom) V-only Model Parameter Estimates")};

FullPlot = grid.arrange(t.Acc.KK, t.Acc.V, t.Soc.KK,t.Soc.V, ncol=2, nrow=2)
ggsave(FullPlot, file = paste("All Group Models Parameter Estimates and CI.png",sep=""))

#3. Does it differ between sex:
isPostSex <- ggplot(NotAlone.S.Effects,aes(x=`isPost:sexM`))+
    geom_density(aes(fill = "p(Prox)" ))+
    geom_density(data = Social.S.Effects, aes(x=`isPost:sexM`, fill = "p(Groom)"))+
    geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
    ggtitle("Interaction Hurricane:Sex Effect")+
    labs(x = "isPost:SexM paramater",
         y = "Density",
         fill = "Independent Variable")+
    xlim(-1,2)+ ylim(0, 10)

Means = colMeans2(as.matrix(NotAlone.S.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.S.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.S<-tableGrob(Estimates);t.Acc.S<-grid.arrange(t.Acc.S, top="p(Prox) Model by sex Parameter Estimates");

Means = colMeans2(as.matrix(Social.S.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.S.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.S<-tableGrob(Estimates);t.Soc.S<-grid.arrange(t.Soc.S, top="p(Groom) Model by sex Parameter Estimates");

FullPlot = grid.arrange(t.Acc.S, t.Soc.S, isPostSex, ncol=3)

ggsave(FullPlot, file = paste("Sex Model Parameter Estimates and CI.png",sep=""))

isProxFM <- ggplot(NotAlone.M.Effects,aes(x=isPost))+
  geom_density(aes(fill = "M" ))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = NotAlone.F.Effects, aes(x=isPost, fill = "F"))+
  ggtitle("Hurricane Effect on p(Prox): M vs. F")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Sex")+
  xlim(-1,2)+ ylim(0, 10)

isSocialFM <- ggplot(Social.M.Effects,aes(x=isPost))+
  geom_density(aes(fill = "M" ))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = Social.F.Effects, aes(x=isPost, fill = "F"))+
  ggtitle("Hurricane Effect on p(Groom): M vs. F")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Sex")+
  xlim(-1,2)+ ylim(0, 10)


Means = colMeans2(as.matrix(NotAlone.M.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.M.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.M<-tableGrob(Estimates);{grid.arrange(t.Acc.M, top="p(Prox) M-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.M.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.M.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.M<-tableGrob(Estimates);{grid.arrange(t.Soc.M, top="p(Groom) M-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(NotAlone.F.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.F.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.F<-tableGrob(Estimates);{grid.arrange(t.Acc.F, top="p(Prox) F-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.F.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.F.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.F<-tableGrob(Estimates);{grid.arrange(t.Soc.F, top="p(Groom) F-only Model Parameter Estimates")};

FullPlot = grid.arrange(t.Acc.M, t.Acc.F, t.Soc.M,t.Soc.F, ncol=2, nrow=2)
ggsave(FullPlot, file = paste("All Sex Models Parameter Estimates and CI.png",sep=""))

#3. Does it differ according to the quarter:
isPostQ <- ggplot(NotAlone.Q.Effects,aes(x=`isPost:Q`))+
  geom_density(aes(fill = "p(Prox)" ))+
  geom_density(data = Social.Q.Effects, aes(x=`isPost:Q`, fill = "p(Groom)"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  ggtitle("Interaction Hurricane:Q Effect")+
  labs(x = "isPost:Q paramater",
       y = "Density",
       fill = "Independent Variable")+
  xlim(-1,2)+ ylim(0, 10)

Means = colMeans2(as.matrix(NotAlone.Q.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.Q.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.Q<-tableGrob(Estimates);t.Acc.Q<-grid.arrange(t.Acc.Q, top="p(Prox) Model by Q Parameter Estimates");

Means = colMeans2(as.matrix(Social.Q.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.Q.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.Q<-tableGrob(Estimates);t.Soc.Q<-grid.arrange(t.Soc.Q, top="p(Groom) Model by Q Parameter Estimates");

FullPlot = grid.arrange(t.Acc.Q, t.Soc.Q, isPostQ, ncol=3)

ggsave(FullPlot, file = paste("Q Model Parameter Estimates and CI.png",sep=""))

# 4. Does it differ according to age
isPostA <- ggplot(NotAlone.A.Effects,aes(x=`isPost:age`))+
  geom_density(aes(fill = "p(Prox)" ))+
  geom_density(data = Social.A.Effects, aes(x=`isPost:age`, fill = "p(Groom)"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  ggtitle("Interaction Hurricane:age Effect")+
  labs(x = "isPost:age paramater",
       y = "Density",
       fill = "Independent Variable")+
  xlim(-0.2,0.4)+ ylim(0, 25)

Means = colMeans2(as.matrix(NotAlone.A.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.A.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.A<-tableGrob(Estimates);t.Acc.A<-grid.arrange(t.Acc.A, top="p(Prox) Model by age Parameter Estimates");

Means = colMeans2(as.matrix(Social.A.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.A.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.A<-tableGrob(Estimates);t.Soc.A<-grid.arrange(t.Soc.A, top="p(Groom) Model by age Parameter Estimates");

FullPlot = grid.arrange(t.Acc.A, t.Soc.A, isPostA, ncol=3)

ggsave(FullPlot, file = paste("Age Model Parameter Estimates and CI.png",sep=""))

# 5. Does it differ according to rank
isPostR <- ggplot(NotAlone.R.Effects,aes(x=`isPost:Rank`))+
  geom_density(aes(fill = "p(Prox)" ))+
  geom_density(data = Social.R.Effects, aes(x=`isPost:Rank`, fill = "p(Groom)"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  ggtitle("Interaction Hurricane:rank Effect")+
  labs(x = "isPost:rank paramater",
       y = "Density",
       fill = "Independent Variable")+
  xlim(-0.2,0.4)+ ylim(0, 25)

Means = colMeans2(as.matrix(NotAlone.R.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.R.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.R<-tableGrob(Estimates);t.Acc.R<-grid.arrange(t.Acc.R, top="p(Prox) Model by rank Parameter Estimates");

Means = colMeans2(as.matrix(Social.R.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.R.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.R<-tableGrob(Estimates);t.Soc.R<-grid.arrange(t.Soc.R, top="p(Groom) Model by rank Parameter Estimates");

FullPlot = grid.arrange(t.Acc.R, t.Soc.R, isPostR, ncol=3)

ggsave(FullPlot, file = paste("Rank Model Parameter Estimates and CI.png",sep=""))
