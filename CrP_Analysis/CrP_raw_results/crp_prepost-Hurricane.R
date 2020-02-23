##############################################################################
# Q1: Is there a difference between pre-hurricane and post-hurricane crp levels
##############################################################################
#Load libraries
library(dplyr)
library(glmmTMB)# Generalized Linear Mixed Models using Template Model Builder
library(bbmle)#Tools for General Maximum Likelihood Estimation
set.seed(20) #set the seed of R's random number generator, useful for creating simulations or random objects that an be reproduced
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models
library(lme4) #classic glmm package
library(fitdistrplus) #package to determine which seem to be the best fit distribution given the data
library(performance)
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

## Combine crp and trapping data
trap_crp_rslt = merge(rslt,full_trapping)

# There were ids missing between the liver results file and the trapping file. 
#however I have recovered the mis-spelled IDs
#To find IDs not in the trapping file:
#missing_IDs_trap = rslt$id[which(is.na(match(rslt$id, full_trapping$id)))]

# Creating a "hurricane" column
data = trap_crp_rslt
data$trapping_date  <- lubridate::mdy(data$trapping_date)
data$crp_date  <- lubridate::mdy(data$crp_date)
data$year <- lubridate::year(data$trapping_date)
data$isPost = 0; data$isPost[which(data$year == 2018)] =1
num_presamples = length(which(data$isPost == 0)); num_postsamples = length(which(data$isPost == 1))
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
           at=1,
           ylab="CrP Level" ) #set location of graph. fudge parameter to make slight to the right or left of "at" location
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

#Plot distribution across days_between & age
plot(data$crp_level,data$days_between)
plot(data$crp_level,data$age)

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

# Some variables might need to be scaled or set as factors
data$age <- scale(data$age)
data$isPost <- as.factor(data$isPost)
data$isFemale <- as.factor(data$isFemale)
summary(data) #check variables
descdist(data$crp_level) # function to help user determine the distribution of his data
#based on this plot, could be gamma, could be beta

# create training and test sets (Not used in analysis below)
#train  <- sample (1:nrow (data), round (.80 * nrow (data))) #select randomly 85% of sample data for training
#datatr   <- data[ train,]
#datate   <- data[-train,] #take the rest 15% for testing

#########################
#### Which family? ####

#Gaussian
crpLevel_gauss <- glmmTMB(crp_level ~ isFemale + age + isPost + days_between + (1|plaque_num) + (1|id),
                          data = data, ziformula= ~0 , family = gaussian) 
summary(crpLevel_gauss)#Zero inflation does not seem to improve model fit.
#Although gaussian model has the best fit (with no warnings), it violates model assumptions on residuals:
#Testing assumptions
simres1 <- simulateResiduals(crpLevel_gauss, n = 1000)
testResiduals(simres1) #test uniformity (KS), dispersion and outlier. You want all these to be non-significant ideally!
#Here all the assumptions are violated

#ziGamma: Gamma cannot be used with 0's (cannot include non-positive values), but ziGamma can!
crpLevel_zigamma <- glmmTMB(crp_level ~ isFemale + age + isPost + days_between + (1|plaque_num) + (1|id),
                          data = data, ziformula= ~. , family = ziGamma(link="log"))
#A zero inflation formula NEEDS to be specified here. Otherwise the model will be considered Gamma and cannot run with non-positive values (i.e. 0's)
#I get warnings() : In (function (start, objective, gradient = NULL, hessian = NULL, NA/NaN function evaluation
#if I don't specify a link funtion. With "log" link function no warnings. With "inverse", "logit" I still do get warnings.
summary(crpLevel_zigamma)
simres2 <- simulateResiduals(crpLevel_zigamma, n = 1000)
testResiduals(simres2)
testZeroInflation(simres2) #test zero-inflation
check_model(crpLevel_zigamma) #this allow to check for multicollinearity, homogeneity of variance, nromality of random effects, normality of residuals and outliers
#Here all of the assumptions are met.

#Variance Inflation Factor to test multicollinearity
check_collinearity(crpLevel_zigamma) #if VIF <5, don't need to worry about collinearity.

#Beta: can only be used with proportion y values (between 0 and 1)
#Poisson: can only be used with integers.
#Negative binomial (linear and non-linear) AND truncated negative binomial model are not converging. 
#Also negative binomial is expecting integers

###########################
#### Which predictors? ####

crpLevel1 <- glmmTMB(crp_level ~ isFemale + age + isPost + days_between + (1|plaque_num) + (1|id),
                            data = data, ziformula= ~. , family = ziGamma(link="log")); summary(crpLevel1) #Both random factor are important predictors
crpLevel2 <- glmmTMB(crp_level ~ age + isPost + days_between + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log"))
crpLevel3 <- glmmTMB(crp_level ~ isPost + days_between + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log"))
crpLevel4 <- glmmTMB(crp_level ~ days_between + (1|plaque_num) + (1|id),
                     data = data, ziformula= ~. , family = ziGamma(link="log"))

#Comparing model fits
AICtab(crpLevel1 , crpLevel2, crpLevel3, crpLevel4)

#print model table
tab_model(crpLevel1, transform = NULL)

###########################
#### 2-step modelling ####

#### Run glmm models step 1:presence/absence; step 2: Distributions of non-zero value ####
data$isCRP = 0; data$isCRP[which(data$crp_level != 0)] =1
data_nonzero = data[which(data$crp_level!=0),]
hist(data_nonzero$crp_level); stripchart(data_nonzero$crp_level, method="jitter")
descdist(data_nonzero$crp_level)

#Run presence/absence model
crpLevel_bin <- glmmTMB(isCRP ~ isFemale + age + isPost + days_between + (1|plaque_num) + (1|id),
                            data = data, family = binomial)
summary(crpLevel_bin)
simres3 <- simulateResiduals(crpLevel_bin, n = 1000)
testResiduals(simres3)

#Run conditional model on non-zero values
crpLevel_gauss <- glmmTMB(crp_level ~ isFemale + age + isPost + days_between + (1|plaque_num) + (1|id),
                          data = data_nonzero, family = gaussian(link="log"))
summary(crpLevel_gauss)
simres4 <- simulateResiduals(crpLevel_gauss, n = 1000)
testResiduals(simres4) #Passes all the residuals' tests

crpLevel_gamma <- glmmTMB(crp_level ~ isFemale + age + isPost + days_between + (1|plaque_num) + (1|id),
                        data = data_nonzero, family = Gamma(link="log"))
summary(crpLevel_gamma)
simres5 <- simulateResiduals(crpLevel_gamma, n = 1000)
testResiduals(simres5) #Fails uniformity of resiuals' test

#Negative binomial linear and non-linear fail to converge & expect integer values

