######## Modelling Logistic Regressions on A2

rm(list=ls()[! ls() %in% c("A2","A3")]) #remove all variables 

library(glmmTMB)# Generalized Linear Mixed Models using Template Model Builder
library(bbmle)#Tools for General Maximum Likelihood Estimation
set.seed(20) #set the seed of R's random number generator, useful for creating simulations or random objects that an be reproduced
library(DHARMa) #residual diagnostic fr hierarchical (multi-level/mixed) regression models

        ### Reduce File to group V and group KK, exclude 2013
        T2 <- A2
        T2 <- T2[T2$group == "groupKK" | T2$group == "groupV",]
        T2 <- T2[T2$year  != 2013,]
        
        ### some variables might need to be scaled.
        T2$age <- scale(T2$age)

        ### create training and test sets
        train  <- sample (1:nrow (T2), round (.85 * nrow (T2))) #select randomly 85% of sample data for training
        #Sample: takes a sample of a specified size (second argument) from the elements of the first 
        #argument.Can be with or without replacement and with certain proabbilities attached to each value.
        T2tr   <- T2[ train,]
        T2te   <- T2[-train,] #take the rest 15% for testing
        

        ## Combined, P(Accompanied, but not grooming), using only training data set. This does not take into account the number of partners.
        isNotAloneC    <- glmmTMB(isNotAloneE ~ isFemale + age + timeBlock + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group),
                                  data = T2tr, ziformula= ~ 0, family = binomial) #Why does he use glmmTMB? poly()?
        isNotAloneC2   <- glmmTMB(isNotAloneE ~ isFemale + age + timeBlock + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group),
                                  data = T2tr, ziformula= ~ 0, family = binomial)
        isNotAloneC3   <- glmmTMB(isNotAloneE ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group), 
                                  data = T2tr, ziformula= ~ 0, family = binomial)
        isNotAloneC4   <- glmmTMB(isNotAloneE ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group), 
                                  data = T2tr, ziformula= ~ 0, family = binomial)

        # Test the models on the testing data set
        pred1 <- predict(isNotAloneC, T2te) #predict: obtains predictions and estimates standard errors of those predictions
        #argument 1: glmm model; argument2: new data to look for variables with which to predict.
        summary(lm(T2te$isNotAloneE ~ pred1)) #test whether the predictions 
        pred2 <- predict(isNotAloneC2, T2te); summary(lm(T2te$isNotAloneE ~ pred2))
        pred3 <- predict(isNotAloneC3, T2te); summary(lm(T2te$isNotAloneE ~ pred3))
        pred4 <- predict(isNotAloneC4, T2te); summary(lm(T2te$isNotAloneE ~ pred4))
                
        # C4 performs best on the test set, rerunning models with full data
        isNotAloneC4.2   <- glmmTMB(isNotAloneE ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group),
                                   data = T2, ziformula= ~ 0, family = binomial)
                
        # Evaluate the best performing model
        summary(isNotAloneC4.2)
        simres1 <- simulateResiduals(isNotAloneC4.2, n = 1000) #creates scaled residuals by simulating from the fitted model
        testResiduals(simres1) #runs a uniformity test (Kolmogorov-Smirnov test), 
        #Dispersion test (nonparametric test via sd of residual fitted vs simulated)
        #OUtliers test based on exact binomial test
        prox <- MuMIn::r.squaredGLMM(isNotAloneC4.2) #outputs marginal R2 (r2m)= variance explained by fixed effects 
        # Conditional R2 (r2c) = variance explained by the complete model (random + fixed)
        #computes r2c and r2m using two different methods (2 rows)

      ## Combined P(Grooming), using the training data 
      isSocialC    <- glmmTMB(isSocial ~ isFemale + age + timeBlock + isPost + poly(Q, 2)*isPost + (1 | year) + (1 | focalID) + (1 | group),
                              data = T2tr, ziformula= ~ 0, family = binomial)
      isSocialC2   <- glmmTMB(isSocial ~ isFemale + age + timeBlock + isPost + poly(Q, 2):isPost + (1 | year) + (1 | focalID) + (1 | group),
                              data = T2tr, ziformula= ~ 0, family = binomial)
      isSocialC3   <- glmmTMB(isSocial ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2):isPost + (1 | year)  + (1 | focalID) + (1 | group),
                              data = T2tr, ziformula= ~ 0, family = binomial)
      isSocialC4   <- glmmTMB(isSocial ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group),
                              data = T2tr, ziformula= ~ 0, family = binomial)
      
      # Test the models on the testing data set
      pred1 <- predict(isSocialC,  T2te); summary(lm(T2te$isSocial ~ pred1))
      pred2 <- predict(isSocialC2, T2te); summary(lm(T2te$isSocial ~ pred2))
      pred3 <- predict(isSocialC3, T2te); summary(lm(T2te$isSocial ~ pred3))
      pred4 <- predict(isSocialC4, T2te); summary(lm(T2te$isSocial ~ pred4))
    
      # C4 performs best on the test set, rerunning models with full data
      isSocialC4.2   <- glmmTMB(isSocial ~ isFemale + age + poly(timeBlock, 2) + isPost + poly(Q, 2)*isPost + (1 | year)  + (1 | focalID) + (1 | group), 
                                data = T2, ziformula= ~ 0, family = binomial)
      
      AICtab(isSocialC, isSocialC2, isSocialC3, isSocialC4) # Generates a table ranking the models based on the selected
      #information criteria and also provides delta AIC and Akaike weights. 
      summary(isSocialC4)
      simres2 <- simulateResiduals(isSocialC3, n = 1000)
      testResiduals(simres2) 
      soc <- MuMIn::r.squaredGLMM(isSocialC3)
              




      