#Vizualize P(Acc) & P(Social) effects
library(ggplot2)
library(matrixStats)
library(gridExtra)
library(graphics)

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/ModelEffectsFinal.RData")
rm(list=setdiff(ls(),c("NotAlone.R.Effects", "NotAlone.A.Effects", "NotAlone.G.Effects","NotAlone.KK.Effects","NotAlone.V.Effects",
                       "NotAlone.S.Effects", "NotAlone.Q.Effects","NotAlone.PM.Effects","NotAloneEffects","Social.G.Effects",
                       "Social.KK.Effects", "Social.V.Effects","Social.S.Effects","Social.Q.Effects","Social.PM.Effects","SocialEffects",
                       "NotAlone.M.Effects","NotAlone.F.Effects","Social.M.Effects", "Social.F.Effects", "Social.R.Effects", "Social.A.Effects")))

setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 

#1. Show the distribution of the Hurricane effect on P(Acc) and P(Social) considering all data: 
isPostBM<- ggplot(NotAloneEffects,aes(x=isPost))+
  geom_density(aes(fill = "pAcc"))+
  geom_density(data = SocialEffects, aes(x=isPost,fill = "pSocial"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  ggtitle("Size of Hurricane Effect on p(Acc) & p(Social)")+
  labs(fill="Independent Variable",x="isPost Parameter",y="Density")+
  xlim(-1,2)+ ylim(0, 15)

Means = colMeans2(as.matrix(NotAloneEffects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAloneEffects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc<-tableGrob(Estimates); t.Acc<-grid.arrange(t.Acc, top="Base Model: p(Acc) Model Parameter Estimates");

Means = colMeans2(as.matrix(SocialEffects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(SocialEffects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc<-tableGrob(Estimates); t.Soc <- grid.arrange(t.Soc, top="Base Model: p(Social) Parameter Estimates");

FullPlot = grid.arrange(t.Acc, t.Soc,isPostBM, ncol=3)

ggsave(FullPlot, file = paste("Base Model Parameter Estimates and CI.png",sep=""))


#2. Considering only PM data
isPostAcc<-ggplot(NotAloneEffects,aes(x=isPost))+
  geom_density(aes(fill = "All Data"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = NotAlone.PM.Effects, aes(x=isPost, fill = "PM only"))+
  ggtitle("Size of Hurricane Effect on p(Acc): AllData vs. PM only")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Dataset")+
  xlim(-1,2)+ ylim(0, 15)

isPostSoc<-ggplot(SocialEffects,aes(x=isPost))+
  geom_density(aes(fill = "All Data"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = Social.PM.Effects, aes(x=isPost, fill = "PM only"))+
  ggtitle("Size of Hurricane Effect on p(Social): AllData vs. PM only")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Dataset")+
  xlim(-1,2)+ ylim(0, 15)

Means = colMeans2(as.matrix(NotAlone.PM.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.PM.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.PM<-tableGrob(Estimates);t.Acc.PM<-grid.arrange(t.Acc.PM, top="p(Acc) PM-only Model Parameter Estimates");

Means = colMeans2(as.matrix(Social.PM.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.PM.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.PM<-tableGrob(Estimates);t.Soc.PM<-grid.arrange(t.Soc.PM, top="p(Social) PM-only Model Parameter Estimates");

FullPlot = grid.arrange(isPostAcc,isPostSoc, t.Acc.PM, t.Soc.PM, ncol=2, nrow=2)

ggsave(FullPlot, file = paste("PM Model Parameter Estimates and CI.png",sep=""))

#3. Does it differ between groups:
isPostGroup <- ggplot(NotAlone.G.Effects,aes(x=`isPost:groupV`))+
    geom_density(aes(fill = "p(Acc)" ))+
    geom_density(data = Social.G.Effects, aes(x=`isPost:groupV`, fill = "p(Social)"))+
    geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
    ggtitle("Interaction Hurricane:GroupV Effect")+
    labs(x = "isPost:groupV paramater",
         y = "Density",
         fill = "Independent Variable")+
    xlim(-2.5,2)+ ylim(0, 10)

Means = colMeans2(as.matrix(NotAlone.G.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.G.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.G<-tableGrob(Estimates);t.Acc.G<-grid.arrange(t.Acc.G, top="p(Acc) Model by group Parameter Estimates");

Means = colMeans2(as.matrix(Social.G.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.G.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.G<-tableGrob(Estimates);t.Soc.G<-grid.arrange(t.Soc.G, top="p(Social) Model by group Parameter Estimates");

FullPlot = grid.arrange(t.Acc.G, t.Soc.G, isPostGroup, ncol=3)

ggsave(FullPlot, file = paste("Group Model Parameter Estimates and CI.png",sep=""))


# KK-only & V-only Model paramter isPost comparison for proximty rate
{ggplot(NotAlone.KK.Effects,aes(x=isPost))+
  geom_density(aes(fill = "KK" ))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = NotAlone.V.Effects, aes(x=isPost, fill = "V"))+
  ggtitle("Hurricane Effect on p(Acc): KK vs. V")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Group")+
  xlim(-3,3)+ ylim(0, 15)}

#KK-only & V-only Model paramter isPost comparison for grooming rate
{ggplot(Social.KK.Effects,aes(x=isPost))+
  geom_density(aes(fill = "KK" ))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  geom_density(data = Social.V.Effects, aes(x=isPost, fill = "V"))+
  ggtitle("Hurricane Effect on p(Social): KK vs. V")+
  labs(x = "isPost paramater",
       y = "Density",
       fill = "Group")+
  xlim(-1,1)+ ylim(0, 15)}

Means = colMeans2(as.matrix(NotAlone.KK.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.KK.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.KK<-tableGrob(Estimates);{grid.arrange(t.Acc.KK, top="p(Acc) KK-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.KK.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.KK.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.KK<-tableGrob(Estimates);{grid.arrange(t.Soc.KK, top="p(Social) KK-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(NotAlone.V.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.V.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.V<-tableGrob(Estimates);{grid.arrange(t.Acc.V, top="p(Acc) V-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.V.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.V.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.V<-tableGrob(Estimates);{grid.arrange(t.Soc.V, top="p(Social) V-only Model Parameter Estimates")};

#3. Does it differ between sex:
isPostSex <- ggplot(NotAlone.S.Effects,aes(x=`isPost:sexM`))+
    geom_density(aes(fill = "p(Acc)" ))+
    geom_density(data = Social.S.Effects, aes(x=`isPost:sexM`, fill = "p(Social)"))+
    geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
    ggtitle("Interaction Hurricane:Sex Effect")+
    labs(x = "isPost:SexM paramater",
         y = "Density",
         fill = "Independent Variable")+
    xlim(-1,2)+ ylim(0, 10)

Means = colMeans2(as.matrix(NotAlone.S.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.S.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.S<-tableGrob(Estimates);t.Acc.S<-grid.arrange(t.Acc.S, top="p(Acc) Model by sex Parameter Estimates");

Means = colMeans2(as.matrix(Social.S.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.S.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.S<-tableGrob(Estimates);t.Soc.S<-grid.arrange(t.Soc.S, top="p(Social) Model by sex Parameter Estimates");

FullPlot = grid.arrange(t.Acc.S, t.Soc.S, isPostSex, ncol=3)

ggsave(FullPlot, file = paste("Sex Model Parameter Estimates and CI.png",sep=""))

# {ggplot(NotAlone.M.Effects,aes(x=isPost))+
#   geom_density(aes(fill = "M" ))+
#   geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
#   geom_density(data = NotAlone.F.Effects, aes(x=isPost, fill = "F"))+
#   ggtitle("Hurricane Effect on p(Acc): M vs. F")+
#   labs(x = "isPost paramater",
#        y = "Density",
#        fill = "Sex")+
#   xlim(-1,2)+ ylim(0, 10)}
# 
# {hist(NotAlone.M.Effects$groupV)}
# 
# {ggplot(Social.M.Effects,aes(x=isPost))+
#   geom_density(aes(fill = "M" ))+
#   geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
#   geom_density(data = Social.F.Effects, aes(x=isPost, fill = "F"))+
#   ggtitle("Hurricane Effect on p(Social): M vs. F")+
#   labs(x = "isPost paramater",
#        y = "Density",
#        fill = "Sex")+
#   xlim(-1,2)+ ylim(0, 10)}
# 
# {hist(NotAlone.F.Effects$groupV)}
# 
# 
Means = colMeans2(as.matrix(NotAlone.M.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.M.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.M<-tableGrob(Estimates);{grid.arrange(t.Acc.M, top="p(Acc) M-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.M.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.M.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.M<-tableGrob(Estimates);{grid.arrange(t.Soc.M, top="p(Social) M-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(NotAlone.F.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.F.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.F<-tableGrob(Estimates);{grid.arrange(t.Acc.F, top="p(Acc) F-only Model Parameter Estimates")};

Means = colMeans2(as.matrix(Social.F.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.F.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.F<-tableGrob(Estimates);{grid.arrange(t.Soc.F, top="p(Social) F-only Model Parameter Estimates")};


#3. Does it differ according to the quarter:
isPostQ <- ggplot(NotAlone.Q.Effects,aes(x=`isPost:Q`))+
  geom_density(aes(fill = "p(Acc)" ))+
  geom_density(data = Social.Q.Effects, aes(x=`isPost:Q`, fill = "p(Social)"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  ggtitle("Interaction Hurricane:Q Effect")+
  labs(x = "isPost:Q paramater",
       y = "Density",
       fill = "Independent Variable")+
  xlim(-1,2)+ ylim(0, 10)

Means = colMeans2(as.matrix(NotAlone.Q.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.Q.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.Q<-tableGrob(Estimates);t.Acc.Q<-grid.arrange(t.Acc.Q, top="p(Acc) Model by Q Parameter Estimates");

Means = colMeans2(as.matrix(Social.Q.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.Q.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.Q<-tableGrob(Estimates);t.Soc.Q<-grid.arrange(t.Soc.Q, top="p(Social) Model by Q Parameter Estimates");

FullPlot = grid.arrange(t.Acc.Q, t.Soc.Q, isPostQ, ncol=3)

ggsave(FullPlot, file = paste("Q Model Parameter Estimates and CI.png",sep=""))

# 4. Does it differ according to age
isPostA <- ggplot(NotAlone.A.Effects,aes(x=`isPost:age`))+
  geom_density(aes(fill = "p(Acc)" ))+
  geom_density(data = Social.A.Effects, aes(x=`isPost:age`, fill = "p(Social)"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  ggtitle("Interaction Hurricane:age Effect")+
  labs(x = "isPost:age paramater",
       y = "Density",
       fill = "Independent Variable")+
  xlim(-0.2,0.4)+ ylim(0, 25)

Means = colMeans2(as.matrix(NotAlone.A.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.A.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.A<-tableGrob(Estimates);t.Acc.A<-grid.arrange(t.Acc.A, top="p(Acc) Model by age Parameter Estimates");

Means = colMeans2(as.matrix(Social.A.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.A.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.A<-tableGrob(Estimates);t.Soc.A<-grid.arrange(t.Soc.A, top="p(Social) Model by age Parameter Estimates");

FullPlot = grid.arrange(t.Acc.A, t.Soc.A, isPostA, ncol=3)

ggsave(FullPlot, file = paste("Age Model Parameter Estimates and CI.png",sep=""))

# 5. Does it differ according to rank
isPostR <- ggplot(NotAlone.R.Effects,aes(x=`isPost:Rank`))+
  geom_density(aes(fill = "p(Acc)" ))+
  geom_density(data = Social.R.Effects, aes(x=`isPost:Rank`, fill = "p(Social)"))+
  geom_vline(aes(xintercept=0), color="black", linetype="dashed", size=1)+
  ggtitle("Interaction Hurricane:rank Effect")+
  labs(x = "isPost:rank paramater",
       y = "Density",
       fill = "Independent Variable")+
  xlim(-0.2,0.4)+ ylim(0, 25)

Means = colMeans2(as.matrix(NotAlone.R.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(NotAlone.R.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Acc.R<-tableGrob(Estimates);t.Acc.R<-grid.arrange(t.Acc.R, top="p(Acc) Model by rank Parameter Estimates");

Means = colMeans2(as.matrix(Social.R.Effects)); Means = round(Means,2)
CI = colQuantiles(as.matrix(Social.R.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,2)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")
t.Soc.R<-tableGrob(Estimates);t.Soc.R<-grid.arrange(t.Soc.R, top="p(Social) Model by rank Parameter Estimates");

FullPlot = grid.arrange(t.Acc.R, t.Soc.R, isPostR, ncol=3)

ggsave(FullPlot, file = paste("Age Model Parameter Estimates and CI.png",sep=""))

dev.off()