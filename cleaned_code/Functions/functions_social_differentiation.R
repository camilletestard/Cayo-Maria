#' Estimation of social differentiation
#'
#' Estimates the coefficient of variation of the underlying association probabilities using maximum likelihood, assuming underlying association probabilities follow a beta distribution.
#'
#' @param Num Numeric vector, numerator of the association indices
#' @param Den Numeric vector, denominator of association indices
#' @param method Character, indicating what likelihood function to use
#' @param res Resolution used for integration (only used for \code{method = "Whitehead"})
#' @param initial.params Initial parameters for model fitting
#'
#' @details Social differentiation is commonly defined as the coefficient of variation of the true, underlying association probabilities. This estimation procedure assumes that the underlying probabilities follow a beta distribution, and estimates the parameters of this distribution given the observed association indices.
#' The estimation of social differeniation serves as both a helpful descriptor of social structure, and a useful measure for determining the power and precision of social analyses. The estimated correlation between true and observed association indices can be derived by dividing the estimated social differentiation by the observed CV of association indices.
#' This function can perform estimation in two ways. The first is using the likelihood function given by Whitehead (2009), using \code{method = "Whitehead"}. The second uses the probability mass function of the beta-binomial distribution using \code{method = "Beta-binomial"}.
#' Because it requires numeric integration, the \code{"Whitehead"} method is much slower than the \code{"Beta-binomial"} method, however has the upside of being directly comparable to results from SOCPROG (and thus the vast majority of values reported in the literature).
#' Data should be numeric vectors of numerators and denominators, rather than square matrices. See \code{get_numerator()} and \code{get_denominator()} for easy ways to extract these from raw data.
#' This function calculates and returns the standard error and confidence interval for the estimated social differentiation and correlation. This is based on the inverse Hessian matrix from the optimization routine. Note that a bootstrap of the raw data may be more robust.
#'
#' @return A matrix containing the estimated social differentiation, the CV of the observed associations, and the estimated correlation between true and observed association indices, along with standard errors and confidence intervals.
#'
#' @examples
#' X <- get_numerator(srkw_sightings, return = "vector", data_format = "GBI")
#' D <- get_denominator(srkw_sightings, return = "vector", data_format = "GBI")
#' social_differentiation(X, D, method = "Beta-binomial")
#'
#' @export
social_differentiation <- function(Num, Den, method = c("Whitehead","Beta-binomial"), res = 0.001,  initial.params = c(0.1,0.1)){
  if(length(method) > 1) method <- method[1]
  if(!method %in% c("Whitehead","Beta-binomial")) method <- "Whitehead"
  X <- Num
  D <- Den
  #likelihood functions for social differentiation
  LL.whitehead <- function(z, X, D, delt){
    a <- exp(z[1])
    b <- exp(z[2])
    deltint <- as.vector(seq(from = delt, to = 1 - delt, by = delt))
    D <- as.matrix(D)
    X <- as.matrix(X)
    deltint <- as.vector(seq(from = delt, to = 1 - delt, by = delt)) #vector of values in [0,1]
    ndel <- length(deltint)
    nX <- length(X)
    XX <- X %*% t(rep(1, ndel))
    DD <- D %*% t(rep(1, ndel))
    deldel <- rep(1, nX) %*% t(deltint)
    rrx <- suppressWarnings(stats::dbeta(deltint, a, b)) #density for beta
    I <- ((deldel^XX)*((1-deldel)^(DD-XX))) %*% rrx
    I <- sum(log(I))
    -I
  }
  LL.betabinom <- function(z, X, D){
    a <- exp(z[1])
    b <- exp(z[2])
    ll <- VGAM::dbetabinom.ab(X, size = D, shape1 = a, shape2 = b, log = T)
    I <- sum(ll)
    -I
  }
  #get parameter estimates
  if(method == "Whitehead"){
    result <- stats::optim(initial.params, fn = LL.whitehead, X = X, D = D, delt = res, hessian = T) #MLE for all AIs
  }
  if(method == "Beta-binomial"){
    result <- stats::optim(initial.params, fn = LL.betabinom, X = X, D = D, hessian = T) #MLE for all AIs
  }
  #transform parameters
  a <- exp(result$par[1])
  b <- exp(result$par[2])
  #calculate mean and standard deviation
  mean.fit <- a/(a+b)
  sd.fit <- sqrt((a*b)/((a+b)^2*(a+b+1)))
  #estimate of social differentiation
  estimate <- sd.fit/mean.fit
  #observed CV
  observed <- stats::sd(X/D)/mean(X/D)
  #estimated correlation
  correlation <- estimate/observed
  #sample parameters based on estimates and hessian matrix
  samp <- MASS::mvrnorm(n = 100000, mu = result$par, Sigma = solve(result$hessian))
  #distribution of CVs
  cv.samp <- apply(samp,1,function(z){
      a <- exp(z[1])
      b <- exp(z[2])
      mean.fit = a/(a+b)
      sd.fit = sqrt((a*b)/((a+b)^2*(a+b+1)))
      sd.fit/mean.fit
  })
  #get SEs and CIs
  se_S <- sd(cv.samp, na.rm = T)
  se_r <- sd(cv.samp/observed, na.rm = T)
  ci_S <- quantile(cv.samp, c(0.025,0.975), na.rm = T)
  ci_r <- quantile(cv.samp/observed, c(0.025,0.975), na.rm = T)

  #make summary table
  summary <- matrix(nrow = 3, ncol = 4)
  row.names(summary) <- c("Observed CV","Social Differentiation", "Correlation")
  colnames(summary) <- c("Estimate", "SE", "Lower CI", "Upper CI")

  summary[1,1] <- observed
  summary[2,1] <- estimate
  summary[2,2] <- se_S
  summary[2,c(3,4)] <- ci_S
  summary[3,1] <- correlation
  summary[3,2] <- se_r
  summary[3,c(3,4)] <- ci_r
  return(summary)
}


#FUNCTION 2
get_inputdata <- function(rscans, unqIDs){
  
  #Find the number of scans per individual (to correct for unequal representation later)
  numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
  
  #Get Edgelist of all possible pairs given the unique IDs.
  masterEL = calcMasterEL(unqIDs)
  
  #Get count of grooming occurrences
  options(warn = -1) #set options to ignore all warnings
  weightedEL = calcEdgeList(rscans,masterEL)
  # weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
  # weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
  weightedEL = weightedEL[,c("alter","ego","count")] #transform output to equal proximity data output.
  
  #Get weights (divided by number of scans)
  weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])
  ### Only used to exclude individuals with too low #scans
  weightedEL$numscans.alter <-numscans$freq[match(weightedEL$alter, numscans$id)]
  weightedEL$numscans.ego <-numscans$freq[match(weightedEL$ego, numscans$id)]
  # weightedEL=weightedEL[-which(weightedEL$numscans.alter<60 | weightedEL$numscans.ego<60),]
  # ###
  
  output = weightedEL[,c('count','numscans','numscans.alter','numscans.ego')]
  
  return(output)
}

# FUNCTION 3
#Output the NON-DIRECTIONAL Master Edgelist of all possible pairs given the unique IDs
calcMasterEL    <- function(unqIDs){ #unq IDs only include focal individuals. I.e. We will only take into account focal individuals in our social networks.
  ego <- NULL; alter <- NULL #Initialize ego and alter
  for(i in 1:length(unqIDs)){ #for all unique IDs
    #Create a list of pairsfor each individual and all other IDs, directionality does not matter
    ego <- append(ego, rep(unqIDs[i], length(unqIDs) - i)) #append: add to the variable "ego"
    alter   <- append(alter  , unqIDs[(i+1):length(unqIDs)])
  }
  alter <- alter[1:length(ego)]#Make sure ego and alter are the same length
  
  masterEL <- data.frame(ego, alter) #combine ego and alter
  masterEL$conc <- paste(masterEL[,1],masterEL[,2],sep=".") #create "pair" or "edge" column 
  masterEL$count <- 0 #initialize count for each pair
  
  return(masterEL)
}

# FUNCTION 4
calcEdgeList    <- function(rscans, masterEL){
  
  partners = str_split(rscans$partner.ID, c(","), simplify = TRUE) #split proximity partner by ","
  focalID = as.character(rscans$focalID)
  a = cbind(focalID,partners) #bind focal ID and partner together
  
  PP <- NULL  
  for(ii in 1:nrow(a)){ #for all observations
    for(p in 2:ncol(a)){ #for all proximity partners (not counting the first column, which is the focal ID column)
      if(!is.na(a[ii,p])) { #if not NA
        if(a[ii,p] != "" ) { #if not empty
          S1 <- data.frame(ego = as.character(a[ii, 1]), #ego is the focal ID
                           alter = as.character(a[ii, p])) #alter for proximity partners. There will be a separate row for each partner. 
          PP <- dplyr::bind_rows(PP, S1) #bind rows, adds values to PP
        }
      }
    }
  }
  #create an edge in each direction, since the relationship is NOT directional. Both "directions" should count.
  PP$conc1 <- paste(str_trim(PP$alter), str_trim(PP$ego), sep=".") 
  PP$conc2 <- paste(str_trim(PP$ego), str_trim(PP$alter), sep=".")
  head(PP)
  head(masterEL)  
  
  for(i in 1:nrow(PP)){ #for all pairs
    if(PP$conc1[i] %in% masterEL$conc){ #If this edge exists in the master list
      masterEL$count[which(masterEL$conc == PP$conc1[i])] <- masterEL$count[which(masterEL$conc == PP$conc1[i])] +1 #Find the index and add counts
    }
    
    if(PP$conc2[i] %in% masterEL$conc){
      masterEL$count[which(masterEL$conc == PP$conc2[i])] <- masterEL$count[which(masterEL$conc == PP$conc2[i])] +1
    }
  }
  
  return(masterEL)
}
