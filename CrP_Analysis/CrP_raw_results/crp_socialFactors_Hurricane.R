
###############################################
#Q2: Do social factors affect iindividual's response to the hurricane:
##############################################

## Load libraries
library(dplyr)
library(glmmTMB)# Generalized Linear Mixed Models using Template Model Builder
library(bbmle)#Tools for General Maximum Likelihood Estimation
set.seed(20) #set the seed of R's random number generator, useful for creating simulations or random objects that an be reproduced
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(lme4) #classic glmm package
library(fitdistrplus) #package to determine which seem to be the best fit distribution given the data
library(Hmisc)
library(performance) #Check performance of model. from: https://github.com/easystats
library(sjPlot)


## Load liver crp data
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria")# set working directory
rslt <- read.csv("Liver_results/liver_results.csv")
names(rslt)=c("id","well_id","crp_level","run_num","plaque_num","crp_date") #rename properly columns
rslt$id = as.character(rslt$id)
rslt$id <- sub(" ", "", rslt$id) #eliminate spaces in names

#Load trapping data
trapping2016 <- read.csv("Biobanking_info/2016_Biobanking_metadata.csv")
names(trapping2016)[1]="id"
trapping2016$age = as.numeric(trapping2016$age)

trapping2018 <- read.csv("Biobanking_info/2018_Biobanking_metadata.csv")
names(trapping2018)[1]="id"
trapping2018$age = as.numeric(trapping2018$age)


#Combine trapping data from 2016 and 2018 years and only keep useful columns
full_trapping <- rbind(as.data.frame(trapping2016[c("id", "age", "sex", "with_infant","trapping_date","days_between")]), 
                       as.data.frame(trapping2018[c("id", "age", "sex", "with_infant","trapping_date","days_between")]))

#Combine crp and trapping data
trap_crp_rslt = merge(rslt,full_trapping, by.x = "id", by.y = "id")
length(unique(trap_crp_rslt$id))

#Load current behav file
behav  <- read.csv("CrP_Analysis/behav_KK2017_HH2016.csv")
#behav <- behav[which(behav$year_behav==2016),]

#Combine with behav.csv file
full_rslt = merge(trap_crp_rslt, behav, by.x = "id", by.y = "id")
length(unique(full_rslt$id))
names(full_rslt)[7] = "age"; names(full_rslt)[8] = "sex"; full_rslt["sex.y"]=NULL;full_rslt["age.y"]=NULL

# Creating a "hurricane" column
data = full_rslt
data$trapping_date  <- lubridate::mdy(data$trapping_date)
data$crp_date  <- lubridate::mdy(data$crp_date)
data$year <- lubridate::year(data$trapping_date)
data$isPost = 0; data$isPost[which(data$year == 2018)] =1
num_presamples = length(which(data$isPost == 0)); num_postsamples = length(which(data$isPost == 1)) #we have about half observations before and half after the hurricane
#Creating a "is female" column
data$isFemale = 0; data$isFemale[which(as.character(data$sex) == "F")] =1 

#############################################
#### Univariate and Bivariate Statistics ####

# Plot distribution of crp levels to choose link function:
hist(data$crp_level)
stripchart(data$crp_level, method="jitter")
mean(data$crp_level); var(data$crp_level)


# Plot distribution of crp pre- and post-
col1 <- "#356b86"; col2 <- "#f49634" #adjust color
#Pre-hurricane
stripchart(data$crp_level[data$year == 2016], #select data: pre-hurricne
           col=adjustcolor(col1, .3), #set color and alpha value (transparency)
           method= "jitter", jitter= .15, #Method to separate coincident point. Add jitter to overlap.
           vertical =T, #Draw plots vertically
           xlim=c(0,3), ylim=c(0,3), #plot limits
           pch = 19, #specifies a symbol for plotting points (circles, triangles....)
           add=F, #add chart to current plot
           at=1) #set location of graph. fudge parameter to make slight to the right or left of "at" location
boxplot(data$crp_level[data$year == 2016], col=adjustcolor(col1, .5), outline=F, #outline shows the outliers or not
        xlim=c(0,3), ylim=c(0,3), add=T, at=1, axes=F)
#Post-hurricane
stripchart(data$crp_level[data$year == 2018], #select data: post-hurricne
           col=adjustcolor(col2, .3), method= "jitter", jitter= .15, vertical =T,
           xlim=c(0,3), ylim=c(0,3), pch = 19, add=T, at=2)
boxplot(data$crp_level[data$year == 2018], col=adjustcolor(col2, .5), outline=F, #outline shows the outliers or not
        xlim=c(0,3), ylim=c(0,3), add=T, at=2, axes=F)
legend('topright', legend=c("Pre-Hurricane", "Post-hurricane"), fill = c(col1, col2))


#Plot distribution of crp across plaques
col1<-"dodgerblue1"; col2<-"darkorchid"; col3<-"chocolate1"; col4<-"coral1"
#Plaque 1
stripchart(data$crp_level[data$plaque_num == 1], col=adjustcolor(col1, .3), method= "jitter", jitter= .15, vertical =T,
           xlim=c(0,5), ylim=c(0,4), pch = 19, add=F, at=1)
boxplot(data$crp_level[data$plaque_num == 1], col=adjustcolor(col1, .5), outline=F, xlim=c(0,5), ylim=c(0,4), add=T, at=1, axes=F)
#Plaque 2
stripchart(data$crp_level[data$plaque_num == 2], col=adjustcolor(col2, .3), method= "jitter", jitter= .15, vertical =T,
           xlim=c(0,5), ylim=c(0,4), pch = 19, add=T, at=2)
boxplot(data$crp_level[data$plaque_num == 2], col=adjustcolor(col2,.5), outline=F, xlim=c(0,5), ylim=c(0,4), add=T, at=2, axes=F)
#Plaque 3
stripchart(data$crp_level[data$plaque_num == 3], col=adjustcolor(col3, .3), method= "jitter", jitter= .15, vertical =T,
           xlim=c(0,5), ylim=c(0,4), pch = 19, add=T, at=3)
boxplot(data$crp_level[data$plaque_num == 3], col=adjustcolor(col3, .5), outline=F, xlim=c(0,5), ylim=c(0,4), add=T, at=3, axes=F)
#Plaque 4
stripchart(data$crp_level[data$plaque_num == 4], col=adjustcolor(col4, .3), method= "jitter", jitter= .15, vertical =T,
           xlim=c(0,5), ylim=c(0,4), pch = 19, add=T, at=4)
boxplot(data$crp_level[data$plaque_num == 4], col=adjustcolor(col4, .5), outline=F, xlim=c(0,5), ylim=c(0,4), add=T, at=4, axes=F)
legend('topright', legend=c("plaque#1", "plaque#2", "plaque#3", "plaque#4"), fill = c(col1, col2, col3, col4))


#Plot distribution across numerical predictors: days_between & age
plot(data$crp_level,data$days_between)
plot(data$crp_level,data$age)
plot(data$crp_level,data$rank)
plot(data$crp_level,data$eigen.pr)
plot(data$crp_level,data$indeg.agg)


#Plot distribution across sex
col1 <- "#356b86"; col2 <- "#f49634" #adjust color
#male
stripchart(data$crp_level[data$isFemale == 0], col=adjustcolor(col1, .3), method= "jitter", jitter= .15, vertical =T,
           xlim=c(0,3), ylim=c(0,4), pch = 19, add=F, at=1)
boxplot(data$crp_level[data$isFemale == 0], col=adjustcolor(col1, .5), outline=F, xlim=c(0,3), ylim=c(0,4), add=T, at=1, axes=F)
#female
stripchart(data$crp_level[data$isFemale == 1], col=adjustcolor(col2, .3), method= "jitter", jitter= .15, vertical =T,
           xlim=c(0,3), ylim=c(0,4), pch = 19, add=T, at=2)
boxplot(data$crp_level[data$isFemale == 1], col=adjustcolor(col2, .5), outline=F, xlim=c(0,3), ylim=c(0,4), add=T, at=2, axes=F)
legend(2.25, 3.5, legend=c("M", "F"), fill = c(col1, col2))


#########################
#### Run glmm models ####

# Some variables might need to be scaled or 
data$age <- scale(data$age, center=T, scale = F) #divide by mean but don't divide by root mean square. This preserves parameter interpretatiob
data$sdb.ra <- scale(data$sdb.ra, center=T, scale = F)
data$vig.ra <- scale(data$vig.ra, center=T, scale = F)
data$Windeg.agg<- scale(data$Windeg.agg, center=T, scale = F)
data$Wdeg.pr<- scale(data$Wdeg.pr, center=T, scale = F)

# Some variables might need to be set as factors
data$isPost <- as.factor(data$isPost)
data$isFemale <- as.factor(data$isFemale)
summary(data) #check variables
descdist(data$crp_level) # function to help user determine the distribution of his data
#based on this plot, could be gamma, could be beta

###########################
#### Which predictors? ####

#Compute correlation matrix between predictors 
#corr_matrix = cor(full_rslt[,c("crp_level","days_between", "sdb.ra", "vig.ra","rank","rank2","rc","eigen.pr","Wdeg.pr","Windeg.agg")],
                  #method = "spearman", use = "complete.obs"); corr_matrix = round(corr_matrix,2)
#high_corr = which(corr_matrix >0.3 & corr_matrix <1);
#Eigen.pr is highly correlated with Wdeg.pr. I will keep Eigen.pr because it is a standardized measure.
#Rank2 & rank are highly correlated with rc (0.77, 0.43)
#rank & rc correlated with eigen.pr (0.46)

crpLevel1 <- glmmTMB(crp_level ~ isFemale + age + isPost +days_between + (1|plaque_num) + (1|id),
                            data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevel1)

crpLevel2 <- glmmTMB(crp_level ~ isFemale + age + days_between + isPost*rank2 + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevel2)

crpLevel3 <- glmmTMB(crp_level ~ isFemale + age +days_between + isPost*Windeg.agg + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevel3)

crpLevel4 <- glmmTMB(crp_level ~ isFemale + age +days_between + isPost*Wdeg.pr + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevel4)

crpLevel5 <- glmmTMB(crp_level ~ isFemale + age +days_between + isPost*sdb.ra + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevel5)

crpLevel6 <- glmmTMB(crp_level ~ isFemale + age +days_between + isPost*vig.ra + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevel6)

#Comparing model fits
AICtab(crpLevel1 , crpLevel2, crpLevel3, crpLevel4, crpLevel5, crpLevel6)

crpLevelComplete <- glmmTMB(crp_level ~ isFemale + age + days_between + sdb.ra + vig.ra + Wdeg.pr + Windeg.agg + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevelComplete) 

simres <- simulateResiduals(crpLevelComplete, n = 1000)
testResiduals(simres)
testZeroInflation(simres) #test zero-inflation
#Variance Inflation Factor to test multicollinearity
#check_collinearity(crpLevelComplete) #if VIF <5, don't need to worry about collinearity.
check_model(crpLevelComplete) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers

#print model table
tab_model(crpLevelComplete, transform = NULL) #make sure the parameters are not exp() transformed, because I used a log liink function.
