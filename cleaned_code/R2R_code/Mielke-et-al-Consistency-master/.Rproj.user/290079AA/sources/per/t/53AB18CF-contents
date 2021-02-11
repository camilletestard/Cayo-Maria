##### This function creates the consistency measure as described in 'Mielke et al 2020 Consistency of social interactions in sooty mangabeys and chimpanzees'
##### for questions, contact mielke.alexand@gmail.com
##### Essentially, the script repeatedly splits the interaction distributions randomly in half and compares the halves; it does this for successively smaller subsets of the data as a standardised measure of consistency
##### Data have to be structured by interactants and day; i.e., for each individual and each partner for each day, there should be a column stating how many interactions or minutes of interactions they had, and the observation time to standardise by
##### Examples of the data structure can be found in 'prepared_data.RData'

##### The following parameters need to be defined
#### Input
## 'Individual1' is a vector with the sender of the interaction type
## 'Individual2' is a vector with the receiver of the interaction type
## 'date' is a vector with the date for which the dyadic data are included
## 'interactions' is a vector with the number of interactions or minutes of interactions for the dyad on that day
## 'undirectional' is a boolean parameter; if 'undirectional == T', sender and receiver are considered the same for each day and rates are calculated only once for the dyad
## 'observation time' is a vector with the number of hours that the dyad could have been observed on that day
## 'k.seq' is the size of the steps by which the dataset is reduced repeatedly. So, if k.seq is 0.05, the consistency is calculated for 100%, 95%, 90% etc of the data. The smaller k.seq, the longer things take, but the more accurate the result
## 'j' is the number of interations for each subset of data. So, if j = 20, then the full dataset will be split 20 times, then the next smallest subset of data will be split and compared 20 times, etc 
## 'plot.col' is the color of the plot
## 'behaviour' adds the title of the behaviour to the plot
## 'average.duration' is the average duration of an interaction of that type in case durations are used rather than occurrences, to make comparable plots for event and state interactions

#### Output
##'consistency' data frame with all iterations including the number of interactions per dyad, the size of the dataset, and the correlation between the halves
##'plot' ggplot of the consistency frame
##'standardised' data frame with every number of interactions per dyad that was observed and the median of the correlations for this number of interactions per dyad

library(ggplot2)
library(compiler)

consistency <- function(individual1, individual2, date, interactions, undirectional = FALSE, observation.time, k.seq = 0.05, j = 20, plot.col = 'black', behaviour = '', average.duration = 1){
  #vectorise
  date = as.character(date)
  
  # detect days on which nothing happened and remove them
  nonzero.days = aggregate(observation.time, by = list(date), sum)
  nonzero.days = setdiff(date, nonzero.days$Group.1[nonzero.days$x==0])
  individual1 = individual1[date%in%nonzero.days]
  individual2 = individual2[date%in%nonzero.days]
  interactions = interactions[date%in%nonzero.days]
  observation.time = observation.time[date%in%nonzero.days]
  date = date[date%in%nonzero.days]
  
  # create dyads
  dyad = interaction(individual1, individual2)
  
  # create interactions per hour
  beh.h = (interactions / observation.time)
  beh.h[is.na(beh.h)] = 0
  beh.h[is.infinite(beh.h)] = 0
  
  #number of individuals in dataset
  inds = length(unique(c(individual1, individual2)))
  
  #compile comparison function
  comparison = cmpfun(comparison)
  
  # apply comparison function (see below) for subsets of different sizes (between 100% and 2% of days, in steps determined by k.seq)
  consistency.frame = lapply(seq(1,0.02, by=-k.seq), function(k){ # select increasingly smaller subsets of the data to calculate consistency
    comparison.frame = do.call(rbind, lapply(1:j, function(x) comparison(dyad = dyad, beh.h = beh.h, date = date, k = k, behaviour = behaviour, inds = inds, interactions = interactions, undirectional = undirectional, average.duration = average.duration)))
    return(comparison.frame)
  })
  consistency.frame = do.call(rbind, consistency.frame)
  
  # create standardisation (see below)
  standard = standardisation(consistency.frame = consistency.frame)
  
  # plot results
  consistency.plot = ggplot(consistency.frame, aes(x = interactions.per.dyad, y = cor.halves)) +
    geom_point(alpha = 0.5, show.legend = F, color = plot.col, fill = plot.col) +
    ylim(min(consistency.frame$cor.halves, na.rm = T),1) +
    ylab('Correlation between Random Halves') +
    xlab('Average Interactions per Dyad') +
    geom_hline(yintercept = 0.5, linetype = 2) +
    ggtitle(behaviour) +
    theme_bw()
  consistency.plot = consistency.plot +
    geom_line(standard$ind.int, mapping = aes(x = average.interactions.per.dyad, y = average.median), color = plot.col, size = 1.5)
  
  return(list(consistency = consistency.frame, plot = consistency.plot, standardised = standard$results))
}



### sub-function that does the comparison between one random half and the other random half of the dataset
comparison <- function(dyad, beh.h, date, k, behaviour, inds, interactions, undirectional, average.duration = 1){
  
  # dates
  dates = sort(as.Date(unique(date)))
  # randomly select start day
  start.day=sample(dates[1:(length(dates)-round(length(dates)*k))], 1, FALSE, NULL)
  
  # select all days of the specified length after the start date (100% of days, 95% of days etc)
  all.days=as.character(dates[which(dates==start.day):(which(dates==start.day)+length(dates)*k)])
  
  ### Resample data within the time frame
  ran.days=sample(all.days, size=round(length(all.days)*0.5,0), replace=F, NULL) # randomly select half the days
  other.days=all.days[!all.days%in%ran.days] # select days for the other half
  
  # Aggregate data in both halves
  ran.beh.h = beh.h[date%in%ran.days]
  ran.dyad = dyad[date%in%ran.days]
  xx.set1=aggregate(ran.beh.h, by=list(ran.dyad), sum) # aggregate for both individuals
  colnames(xx.set1)=c("dyad", "behaviour.ph")
  
  ran.beh.h = beh.h[date%in%other.days]
  ran.dyad = dyad[date%in%other.days]
  xx.set2=aggregate(ran.beh.h, by=list(ran.dyad), sum) # aggregate for both individuals
  colnames(xx.set2)=c("dyad", "behaviour.ph")
  
  # select only dyads that appear in both halves
  common = intersect(xx.set1$dyad, xx.set2$dyad)
  xx.set1 = xx.set1[xx.set1$dyad %in% common,]
  xx.set2 = xx.set2[xx.set2$dyad %in% common,]
  
  # if without direction, every dyad only included once, not twice
  if(isTRUE(undirectional)){
    common = sort(unique(unlist(lapply(strsplit(common, split = '.', fixed = T), function(x) {paste(sort(x), collapse = '.')}))))
    xx.set1 = xx.set1[xx.set1$dyad %in% common,]
    xx.set2 = xx.set2[xx.set2$dyad %in% common,]
                    }
  # create results by correlating the distributions of both halves
  comparison.frame = data.frame(subset.size = k,
                                individuals = inds,
                                cor.halves = cor(xx.set1$behaviour.ph, xx.set2$behaviour.ph, method = 'spearman'),
                                interactions = sum(interactions[date %in% all.days])/average.duration,
                                days=length(all.days),
                                interactions.per.individual = (sum(interactions[date %in% all.days])/average.duration) / inds,
                                behaviour = behaviour)
  comparison.frame$interactions.per.dyad = comparison.frame$interactions / ((comparison.frame$individuals^2 - comparison.frame$individuals)/2)
  return(comparison.frame)
}

# standardisation: for every number of interactions per dyad, detect median of correlations
standardisation <- function(consistency.frame){
  # take consistency frame from previous analysis
  xx.frame = consistency.frame
  
  # calculate interactions per dyad used in the frame
  xx.frame$interactions.per.dyad = round((xx.frame$interactions * 2 / ((as.numeric(xx.frame$individuals)^2 - as.numeric(xx.frame$individuals))/2)), 0)
  xx.frame$count = 1
  
  # determine median value of all correlations for that number of interactions per dyad
  ind.int = aggregate(xx.frame$cor.halves, by = list(xx.frame$interactions.per.dyad), median, na.rm = T)
  colnames(ind.int) = c('average.interactions.per.dyad', 'average.median')
  ind.int$average.interactions.per.dyad = ind.int$average.interactions.per.dyad/2
  
  # create standard deviation for the same thing
  ind.int$sd = aggregate(xx.frame$cor.halves, by = list(xx.frame$interactions.per.dyad), sd, na.rm = T)$x
  
  # and number of times that number of interactions per dyad occurred
  ind.int$count = aggregate(xx.frame$count, by = list(xx.frame$interactions.per.dyad), sum, na.rm = T)$x
  results = data.frame(interaction.per.dyad = NA, median.correlation = NA, sd = NA)
  if(max(ind.int$average.median)>0.5){
    nr = min(which(ind.int$average.median >= 0.5))
    results=data.frame(interaction.per.dyad = ind.int$average.interactions.per.dyad[nr],
                       median.correlation = ind.int$average.median[nr],
                       sd = ind.int$sd[nr])
  }
  
  return(list(ind.int = ind.int, results = results))
}

