library(dplyr)
library(ggplot2)
library(lme4)# Generalized Linear Mixed Models
library(lmerTest)
library(performance)
library(sjPlot)
library(jtools)
library(ggplot2)
library(dplyr)
library(lmerTest)
library(matrixStats)
library(gridExtra)
library(graphics)


setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/ChangePAccPsoc/") 
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/ChangeP_min20.RData")
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/SocialCapital.RData")
load("C:/Users/Camille Testard/Documents/Github/Cayo-Maria/R.Data/strength.to.deceased.RData")
data.combined=merge.data.frame(dprob.ALL,strength.to.deceased, by=intersect(c("id","year"),c("id","year")))
full.data=merge.data.frame(data.combined,SocialCapital.ALL,by=intersect(c("id","year"),c("id","year")))
full.data$group.x=NULL; full.data$group.y=NULL
full.data$CSI = (full.data$std.DSIgroom + full.data$std.DSIprox)/2
df1 = full.data[,c("dpSocial","pSocial.pre","pSocial.post","percentrank","std.dead.all","std.DSIgroom","dpAcc")]
full.data[,c("dpSocial","pSocial.pre","pSocial.post","percentrank","std.dead.all","std.DSIgroom","dpAcc")] =lapply(df1,as.numeric)


#####################################################
#Test for regression to the mean and adjust difference value
# BASED ON KELLY AND PRICE 2005

n_iter = max(full.data$iter); 
iter=sample(n_iter,1)
dpSocial.Effects = data.frame(); p.value=vector(); t.value=vector(); df =vector(); var.change=vector()
for (iter in 1:n_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  
  # data = isSocial.all[isSocial.all$iter==iter,]
  full.data.iter = full.data[full.data$iter ==iter,]
  
  #full.data.iter$pSocial.pre[full.data.iter$pSocial.pre==0] = 0.00000001
  #x1 = log(full.data.iter$pSocial.pre); hist(x1,20)
  x1 = full.data.iter$pSocial.pre;
  mean1=mean(x1)
  var1 = var(x1)
  
  #full.data.iter$pSocial.post[full.data.iter$pSocial.post==0] = 0.00000001
  #x2 = log(full.data.iter$pSocial.post); hist(x2,20)
  x2 = full.data.iter$pSocial.post
  mean2=mean(x2)
  var2 = var(x2)
  
  D = x2-x1
  
  rho = cor(x1,x2)
  
  delta = mean2-mean1
  
  exp.D.add = (1-rho)*(x1-mean1)+delta
  
  theta = var2/var1
  exp.D.diff = (1-rho*theta)*(x1-mean1)+delta
  
  n=length(x1); 
  df[iter] = n-2
  num = sqrt(n-2)*(var1/var2 - var2/var1)
  den = 2*sqrt(1-rho^2)
  t.value[iter] = num/den
  p.value[iter] = 2*pt(-abs(t.value[iter]), df=df[iter])
  
  var.change[iter] = (var2-var1)/var1*100
  
  if (p.value[iter]<0.05){
  D.adj = rho*(x1-mean1)-(x2-mean2)
  } else {
    D.adj = (2*rho*var1*var1)/(var1^2 + var2^2)
  }
  
  full.data.iter$dpSocial.adj = D.adj
  
  # ggplot(full.data.iter, aes(y=dpSocial.adj, x=std.DSIgroom))+
  #   geom_jitter(alpha=0.5, size=2, col='blue')+
  #   xlab('Adjusted change in p(groom)')+ylab('Standardized social integration')+
  #   theme_classic(base_size = 20)
  # cor.test(full.data.iter$dpSocial.adj, full.data.iter$std.DSIgroom)
  
  # dpSocial <- lmer(dpSocial.adj~ sex + group + age + percentrank + std.dead.all + CSI + dpAcc + (1|id)+  (1|year), data = full.data.iter, na.action=na.omit)
  # summary(dpSocial)
  # 
  # dpSocial.Effects[iter,c("(Intercept)","sexM","group","age","rank","dead.all","CSI","dpAcc")] <- getME(dpSocial, "beta")
  # 
}
quantile(t.value,probs = c(0.025, 0.95))
quantile(p.value,probs = c(0.025, 0.95))
quantile(var.change,probs = c(0.025, 0.95))

Means = colMeans2(as.matrix(dpSocial.Effects)); Means = round(Means,3)
CI = colQuantiles(as.matrix(dpSocial.Effects), probs = c(0.025, 0.975), na.rm = TRUE); CI = round(CI,4) #compute mean estimte and 95% CI (2.5 and 97.5 percentiles)
Estimates = cbind(Means,CI); Estimates = as.data.frame(Estimates); names(Estimates) = c("Estimate","2.5%","97.5%")

#########################################
#Simulation to understand regression to the mean:

pre.data = sample(20, 100, replace = T)/100
post.data = sample(20, 100, T)/100#pre.data+sample(100,100)/1000#
post.min.pre = post.data-pre.data

plot(pre.data, post.data, col='blue', alpha=0.5)
rho=cor(pre.data, post.data)

plot(pre.data, post.min.pre, pch=16, col='lightblue', alpha=0.5, xlab = "pre-value", 
     ylab = "change pre-to-post", cex=1,cex.lab=1.5)
legend("topright", 
       legend = "rho = -0.65,\nvar1 = 0.0036,\nvar2 = 0.0035,\np>0.7",bty = "n")
cor.test(pre.data, post.min.pre)
# plot(pre.data, post.min.pre, col='blue', alpha=0.5, xlim=c(0.05, 0.15))

var1=var(pre.data)
var2=var(post.data)

n=length(pre.data); 
df = n-2
num = sqrt(n-2)*(var1/var2 - var2/var1)
den = 2*sqrt(1-rho^2)
t.value= num/den
p.value = 2*pt(-abs(t.value), df=df)
  
##########################################

# 1. Merge dprob, social Capital and strength to dead IDs data &clean
data.combined=merge.data.frame(dprob.ALL,strength.to.deceased, by=intersect(c("id","year"),c("id","year")))
full.data=merge.data.frame(data.combined,SocialCapital.ALL,by=intersect(c("id","year"),c("id","year")))
full.data$group.x=NULL; full.data$group.y=NULL
full.data$CSI = (full.data$std.DSIgroom + full.data$std.DSIprox)/2
full.data$pSocial.pre.adj = as.numeric(full.data$pSocial.pre) - mean(as.numeric(full.data$pSocial.pre))

data=full.data[full.data$num_obs>=40 & full.data$iter==1,]

#Visualizations
ggplot(data, aes(x=as.numeric(pSocial.pre), y=as.numeric(std.DSIgroom)))+
  geom_jitter(alpha=0.5, size=2, col='blue')+
  xlab('p(groom) pre-hurricane')+ylab('Standardized social integration')+
  theme_classic(base_size = 20)
cor.test(as.numeric(data$pSocial.pre), as.numeric(data$std.DSIgroom))

ggplot(data, aes(x=as.numeric(pSocial.pre), y=as.numeric(std.dead.all)))+
  geom_jitter(alpha=0.5, size=2, col='blue')+
  xlab('p(groom) pre-hurricane')+ylab('Strength to deceased partner')+
  theme_classic(base_size = 20)
cor.test(as.numeric(data$pSocial.pre), as.numeric(data$std.dead.all))

ggplot(data, aes(x=as.numeric(pSocial.pre), y=as.numeric(CSI)))+
  geom_jitter(alpha=0.5, size=2, col='blue')+
  xlab('p(groom) pre-hurricane')+ylab('Standardized social integration (CSI)')+
  theme_classic(base_size = 20)
cor.test(as.numeric(data$pSocial.pre), as.numeric(data$CSI))

ggplot(data, aes(x=as.numeric(pACC.pre), y=as.numeric(std.DSIprox)))+
  geom_jitter(alpha=0.5, size=2, col='blue')+
  xlab('p(prox) pre-hurricane')+ylab('Standardized social integration')+
  theme_classic(base_size = 20)
cor.test(as.numeric(data$pACC.pre), as.numeric(data$std.DSIprox))


############################################
#Linear regression
n_iter = max(full.data$iter); 
iter=sample(n_iter,1)
for (iter in 1:n_iter){
  
  print(paste("%%%%%%%%%%%%%%%%%% iter",iter, "%%%%%%%%%%%%%%%%%%"))
  
  data.V= full.data[which(full.data$iter==iter&full.data$group=="V"),]
  data.KK= full.data[which(full.data$iter==iter&full.data$group=="KK"),]
  
  ###########################################################
  ## Model Social Capital effect on change in grooming rates
  ###########################################################
  
  dpSocial.V <- lmer(pSocial.post~ sex + age + percentrank + std.dead.all + std.DSIgroom + dpAcc + (1|id)+  (1|year), data = data.V, na.action=na.omit)
  summary(dpSocial.V)
  
  dpSocial.V <- lmer(dpSocial~ sex + age + percentrank + std.dead.all + std.DSIgroom + dpAcc + (1|id)+  (1|year), data = data.V, na.action=na.omit)
  summary(dpSocial.V)
  # performance::check_model(dpSocial.V)
  # plot(effects::allEffects(dpSocial.V))
  dpSocial.Effects.V[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIgroom","dpAcc")] <- getME(dpSocial.V, "beta")
  dpSocial.Effects.V[iter,c("(focalID)","(year)")] <- getME(dpSocial.V, "theta")
  
  dpSocial.KK <- lmer(dpSocial~ sex + age + percentrank + std.dead.all + std.DSIgroom + dpAcc + (1|id)+  (1|year), data = data.KK, na.action=na.omit)
  summary(dpSocial.KK)
  # performance::check_model(dpSocial.KK)
  # plot(effects::allEffects(dpSocial.KK))
  dpSocial.Effects.KK[iter,c("(Intercept)","sexM","age","rank","dead.all","DSIgroom","dpAcc")] <- getME(dpSocial.KK, "beta")
  dpSocial.Effects.KK[iter,c("(focalID)","(year)")] <- getME(dpSocial.KK, "theta")
  

  save(dpSocial.Effects.KK,dpAcc.Effects.KK,dpSocial.Effects.V,dpAcc.Effects.V,
       file="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ChangeP.ModelEffects.RData")
}

####################################
#CODE BASED ON BARNETT ET AL. 2004

# Change these parameters depending on your data;
r<-cor(isSocial.all$prob[isSocial.all$isPost=="pre"], isSocial.all$prob[isSocial.all$isPost=="post"])
sigma<-sd(isSocial.all$prob); # total std;
mu<-mean(isSocial.all$prob); # population mean;
cut<-median(isSocial.all$prob); # cut-off;
# Loops to run through rho and m scenrarios;
sigma2_w=vector(length=11,mode="numeric")
sigma2_b=vector(length=11,mode="numeric")
Rl=vector(length=11,mode="numeric")
Rg=vector(length=11,mode="numeric")
rho=vector(length=11,mode="numeric")
for (rhox in 0:10){
  rho[rhox+1]<-rhox/10
  sigma2_w[rhox+1]<-(1-rho[rhox+1])*(sigma^2); # within-subject variance;
  sigma2_b[rhox+1]<-rho[rhox+1]*(sigma^2); # between-subject variance;
  for (m in 1:1){ # Number of baseline measurements;
    zg<-(cut-mu)/sigma; # z;
    zl<-(mu-cut)/sigma; # z;
    x1g<-dnorm(x=zg); # phi - probability density;
    x2g<-1-pnorm(q=zg); # Phi - CDF
    x1l<-dnorm(x=zl); # phi;
    x2l<-1-pnorm(q=zl); # Phi;
    czl<-x1l/x2l; # C(z) in paper;
    czg<-x1g/x2g; # C(z) in paper;
    Rl[rhox+1]<-(sigma2_w[rhox+1]/m)/sqrt(sigma2_b[rhox+1]+(sigma2_w[rhox+1]/m))*czl; # RTM effect, Equations (1) m=1 & (2) m>1;
    Rg[rhox+1]<-(sigma2_w[rhox+1]/m)/sqrt(sigma2_b[rhox+1]+(sigma2_w[rhox+1]/m))*czg; # RTM effect;
  }
}
output<-cbind(sigma2_b,sigma2_w,rho,Rl,Rg)
print("The expected RTM effect for a range of baseline samples sizes and rhos")
print(output)
print("sigma2_b=between-subject variance, sigma2_w=within-subject variance")
print("rho=within-subject correlation, Rl=RTM effect (<cut-off), Rg=RTM effect (>cut-off)");


#R code to perform an ANCOVA, using the Nambour skin cancer prevention trial as an example
data = dprob.ALL[dprob.ALL$iter==1,]
# Calculate the baseline mean
meanb<-mean(as.numeric(data$pSocial.pre))
# Difference the baseline mean from every baseline observation
data$adiff<-as.numeric(data$pSocial.pre)-meanb
#ANCOVA using lm
model<-lm(formula=as.numeric(data$pSocial.post)~adiff+group,data=data)
summary(model)
