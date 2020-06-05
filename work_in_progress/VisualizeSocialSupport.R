#VISUALIZATION OF SOCIAL SUPPORT

library(ggplot2)
library(gridExtra)

load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Social_Network_Analysis/SocialSupport.RData")

################################################
# For longitudinal data 
################################################
preHurricane = SocialSupport.ALL[which(SocialSupport.ALL$groupyear=="KK2015"| SocialSupport.ALL$groupyear=="KK2017"
                                       | SocialSupport.ALL$groupyear=="V2015"| SocialSupport.ALL$groupyear=="V2016" | SocialSupport.ALL$groupyear=="V2017"),]; 
postHurricane = SocialSupport.ALL[which(SocialSupport.ALL$groupyear=="KK2018"|SocialSupport.ALL$groupyear=="V2018"),]
preHurricane$isPost = 0; postHurricane$isPost = 1

data = rbind(preHurricane,postHurricane)
unqIDs = as.character(unique(data$id))


################################################
# For cross-sectional data 
################################################
HH2016 = SocialSupport.ALL[which(SocialSupport.ALL$groupyear=="HH2016"),]
KK2017 = SocialSupport.ALL[which(SocialSupport.ALL$groupyear=="KK2017"),]
KK2018 = SocialSupport.ALL[which(SocialSupport.ALL$groupyear=="KK2018"),]
data = rbind(HH2016,KK2017,KK2018)
data$percentrank[which(data$percentrank<1)] = data$percentrank[which(data$percentrank<1)]*100

#########
#GROOMING
#########

#Grooming number of partners
mean(HH2016$numPartners_groom); mean(KK2017$numPartners_groom); mean(KK2018$numPartners_groom)
t.test(HH2016$numPartners_groom,KK2017$numPartners_groom)
hist(HH2016$numPartners_groom,col=rgb(0,1,1,0.5), main="Num Groom Partners Across Groups/Years",xlab="Number Groom Partners", xlim = c(0, 50), ylim=c(0, 50))
hist(KK2017$numPartners_groom,col=rgb(1,0,0,0.5), breaks = 10, add=T)
hist(KK2018$numPartners_groom,col=rgb(1,0.5,0,0.5), breaks = 40, add=T)
legend("topright", c("HH2016", "KK2017","KK2018"), fill=c("cyan", "red", "orange"))

numPartnerGroom <- ggplot(data)+
  geom_density(aes(x =numPartners_groom, fill = groupyear), alpha =0.5)+
  ggtitle("Number of grooming partners")+
  labs(fill = "GroupYear",x="Number of Grooming Partners")+
  xlim(0,60)

#Time spent grooming
mean(HH2016$strength_groom); mean(KK2017$strength_groom);mean(KK2018$strength_groom)
t.test(HH2016$strength_groom,KK2017$strength_groom)
GroomStrength <- ggplot(data)+
  geom_density(aes(x =strength_groom, fill = groupyear), alpha =0.5)+
  ggtitle("Strength of connection (based on grooming)")+
  xlim(0, 90)+
  labs(fill = "GroupYear",x="Strength of connection",y="Frequency")

#Time spent grooming to top 3 partners
mean(HH2016$StrengthTop3_groom); mean(KK2017$StrengthTop3_groom);mean(KK2018$StrengthTop3_groom)
t.test(HH2016$StrengthTop3_groom,KK2017$StrengthTop3_groom)
GroomStrengthTop3 <- ggplot(data)+
  geom_density(aes(x =StrengthTop3_groom, fill = groupyear), alpha =0.5)+
  ggtitle("Strength of connection to top3 partners")+
  xlim(0, 50)+
  labs(fill = "GroupYear",x="Strength of connection",y="Frequency")

allPlotsDensity = grid.arrange(numPartnerGroom,GroomStrength,GroomStrengthTop3)

#How do the measures correlated with each other:
data.pre = data[which(data$groupyear == "HH2016"|data$groupyear == "KK2017"),]
numP.Strength <- ggplot(data.pre, aes(x=numPartners_groom, y=strength_groom, color=groupyear)) +
  geom_point(size=2, alpha=0.5)+
  ggtitle("Correlation between number of grooming partners & grooming strength")
numP.StrengthTop3 <-ggplot(data.pre, aes(x=numPartners_groom, y=StrengthTop3_groom, color=groupyear)) +
  geom_point(size=2, alpha=0.5)+
ggtitle("Correlation between number of grooming partners & grooming strength to top 3 partners")
Strength.StrengthTop3 <-ggplot(data.pre, aes(x=strength_groom, y=StrengthTop3_groom, color=groupyear)) +
  geom_point(size=2, alpha = 0.5)+
  ggtitle("Correlation between grooming strength & grooming strength to top3 partners")

allPlots = grid.arrange(numP.Strength, numP.StrengthTop3, Strength.StrengthTop3, nrow=3,ncol=1)

#Show how rank relates to grooming strength
mean.groom=mean(data.pre$strength_groom,na.rm=T)
ggplot(data.pre, aes(x=percentrank, y=strength_groom, color=groupyear)) +
  geom_point(size=2)+
  geom_hline(yintercept=mean.groom, linetype="dashed",size=1)+
  geom_vline(xintercept=50, linetype="dashed",size=1)+
  labs(x="Social Status", y="Grooming Strength",title = "Spread of IDs across rank and grooming strength")
cor.test(data.pre$percentrank, data.pre$strength_groom, method="pearson", use = "complete.obs")

mean.groom=mean(KK2018$strength_groom,na.rm=T)
ggplot(KK2018, aes(x=percentrank, y=strength_groom, color=groupyear)) +
  geom_point(size=2)+
  geom_hline(yintercept=mean.groom, linetype="dashed",size=1)+
  geom_vline(xintercept=50, linetype="dashed",size=1)+
  labs(x="Social Status", y="Grooming Strength",title = "Spread of IDs across rank and grooming strength - KK2018 Only")
cor.test(KK2018$percentrank, KK2018$strength_groom, method="pearson", use = "complete.obs")

##########
#PROXIMITY
##########

#Proximity number of partners
mean(HH2016$numPartners_prox); mean(KK2017$numPartners_prox); mean(KK2018$numPartners_prox);
t.test(HH2016$numPartners_prox,KK2017$numPartners_prox)
ggplot(data)+
  geom_density(aes(x =numPartners_prox, fill = groupyear), alpha =0.5)+
  ggtitle("Number of proximity partners across groups and years")+
  xlim(0, 90)+
  labs(fill = "GroupYear",x="Number of Proximity Partners",y="Frequency")

#Time spent in proximity
mean(HH2016$strength_prox); mean(KK2017$strength_prox);mean(KK2018$strength_prox)
t.test(HH2016$strength_prox,KK2017$strength_prox)
ggplot(data)+
  geom_density(aes(x =strength_prox, fill = groupyear), alpha =0.5)+
  ggtitle("Time spent in proximity across groups and years")+
  xlim(0, 150)+
  labs(fill = "GroupYear",x="Time spent in proximity",y="Frequency")

#Time spent grooming to top 3 partners
mean(HH2016$StrengthTop3_prox); mean(KK2017$StrengthTop3_prox);mean(KK2018$StrengthTop3_prox)
t.test(HH2016$StrengthTop3_prox,KK2017$StrengthTop3_prox)
ggplot(data)+
  geom_density(aes(x =StrengthTop3_prox, fill = groupyear), alpha =0.5)+
  ggtitle("Strength of connection to top3 partners across groups and years")+
  xlim(0, 40)+
  labs(fill = "GroupYear",x="Time spent grooming to top3 partners",y="Frequency")
