#### Simulations for Mielke et al 2020 'Consistency of social interactions in sooty mangabeys and chimpanzees'
######### this code creates simulated data for n number of individuals, interacting 1-10 times a day, with three different certainties of choice (strong preference for some individuals, medium preference for some individuals, egalitarian distribution)
### at the end, for each individual, there should be interactions every day that they are the focal

source('consistency_function.R')
library(compiler)
library(parallel)
library(doParallel)
consistency = cmpfun(consistency)


############## this all takes really long. To make it faster, have smaller group sizes

#nr of individuals in sample
nr.ids = c(10, 10, 10, 15, 15, 15, 20, 20, 20)

##### create behavioural datasets for one year for all individuals in the community
sim.data = lapply(nr.ids, function(k) {
  # create individuals
  subj.to.keep = as.character(1:k) 
  
  # create time frame
  dates = seq(as.Date("2019-11-01"), as.Date("2020-10-31"), "days")
  
  #create combination
  date.set = data.frame(expand.grid(subj.to.keep, dates))
  colnames(date.set) = c('focal', 'date')

  # for each day and individual, create between 1 and 10 interactions
  data.set = lapply(1:nrow(date.set), function(x) {
    xx = date.set[x, ]
    xx = xx[rep(seq_len(nrow(xx)), each = sample(1:10, 1)), ]
    xx$gr.bout.index = x + (1:nrow(xx)) - 1
    xx = xx[rep(seq_len(nrow(xx)), each = length(subj.to.keep)), ]
    xx$pot.partner = as.character(subj.to.keep)
    xx$focal = as.character(xx$focal)
    xx = subset(xx, focal != pot.partner)
    xx$gr.bout.index = sapply(xx$gr.bout.index, function(y) paste(c(x,y), collapse = '_'))
    return(xx)
  })

  data.set = do.call(rbind, data.set)

  #### create dyads
  foc = as.character(expand.grid(subj.to.keep, subj.to.keep)[, 1])
  dyads = data.frame(
    focal = foc,
    partner = sort(as.character(
      expand.grid(subj.to.keep, subj.to.keep)[, 1]
    ))
  )

  # for each interaction, randomly select only subset of 'possible' partners
  remove = unlist(lapply(1:nrow(data.set), function(i) {
    return(sample(c(0, 1), size = 1, prob = c(0.3, 0.7)))
  }))
  data.set = subset(data.set, remove == 1)
  data.set = data.set[data.set$focal!= data.set$pot.partner,]

  # remove interactions where individual is alone
  eff.gr.size = table(data.set$gr.bout.index)
  eff.gr.size = names(eff.gr.size)[eff.gr.size > 1]

  # create dyads as term in data
  data.set = subset(data.set, gr.bout.index%in%eff.gr.size)
  data.set$dyad = apply(cbind(
    as.character(data.set$focal),
    as.character(data.set$pot.partner)
  ), 1, function(x) {
    paste(sort(x), collapse = "_")
  })

  ### set grooming likelihood per dyad
  dyads$gr.llh = 0
  dyads = dyads[dyads$focal!=dyads$partner,]
  dyads$dyad = apply(cbind(as.character(dyads$focal), as.character(dyads$partner)), 1, function(x) {
    paste(sort(x), collapse = "_")
  })
  dyads = dyads[!duplicated(dyads$dyad), ]
  dyads$gr.llh = abs(rnorm(
    n = nrow(dyads),
    mean = 0.5,
    sd = 0.2
  ) ^ 2)
  dyads$gr.llh[dyads$gr.llh > 1] = 1

  #vectorise
  focal = data.set$focal
  gr.bout.index = data.set$gr.bout.index
  gr.llh = dyads$gr.llh[match(data.set$dyad, dyads$dyad)]
  llh.high = data.set$gr.llh
  llh.medium = data.set$gr.llh
  llh.low = data.set$gr.llh

  # create likelihoods of interaction under different conditions: 'high' is squared, 'medium' is squared but restricted to 0.05 - 0.95, low is normal and restricted to 0.1 - 0.9
  for(i in 1:length(unique(focal))){
    xx = gr.llh[focal == unique(focal)[i]]
    xx2 = xx ^ 2
    llh.medium[focal == unique(focal)[i]] = (xx2-min(xx2))/(max(xx2)-min(xx2)) * (0.95 - 0.05) + 0.05
    llh.low[focal == unique(focal)[i]] = (xx-min(xx))/(max(xx)-min(xx)) * (0.9 - 0.1) + 0.1
    llh.high[focal == unique(focal)[i]] = ((xx2-min(xx2))/(max(xx2)-min(xx2)))
  }


  # remove 0 and 1
  llh.high[llh.high == 1] = 0.9999
  llh.medium[llh.medium == 1] = 0.9999
  llh.low[llh.low == 1] = 0.001
  llh.high[llh.high == 0] = 0.001
  llh.medium[llh.medium == 0] = 0.001
  llh.low[llh.low == 0] = 0.001

  # create vectors for chosen as partner 1/0
  gr.initiated.low = rep(0, length(llh.low))
  gr.initiated.medium = rep(0, length(llh.low))
  gr.initiated.high = rep(0, length(llh.low))
  gr.initiated.lowin = rep(0, length(llh.low))
  gr.initiated.mediumin = rep(0, length(llh.low))
  gr.initiated.highin = rep(0, length(llh.low))
  gr.initiated.random = rep(0, length(llh.low))
  gr.initiated.randomin = rep(0, length(llh.low))

  xx = names(table(gr.bout.index)[table(gr.bout.index)==1])
  gr.bout.index = gr.bout.index[!gr.bout.index%in%xx]
  data.set = data.set[!data.set$gr.bout.index%in%xx,]

  # for each bout, under each condition, select who was chosen
  for (i in unique(gr.bout.index)) {
    xx = which(gr.bout.index == i)
    nrlow = sample(xx, size = 1, prob = (llh.low[xx]))
    nrmedium = sample(xx,
                      size = 1,
                      prob = (llh.medium[xx]))
    nrhigh = sample(xx, size = 1, prob = (llh.high[xx]))
    nrlowin = sample(xx, size = 1, prob = 1 - (llh.low[xx]))
    nrmediumin = sample(xx,
                        size = 1,
                        prob = 1 - (llh.medium[xx]))
    nrhighin = sample(xx,
                      size = 1,
                      prob = 1 - (llh.high[xx]))
    nrrandom = sample(xx, size = 1)
    nrrandomin = sample(xx, size = 1)
    gr.initiated.low[nrlow] = 1
    gr.initiated.medium[nrmedium] = 1
    gr.initiated.high[nrhigh] = 1
    gr.initiated.lowin[nrlowin] = 1
    gr.initiated.mediumin[nrmediumin] = 1
    gr.initiated.highin[nrhighin] = 1
    gr.initiated.random[nrrandom] = 1
    gr.initiated.randomin[nrrandomin] = 1
  }

  # put likelihoods into data.set
  data.set$gr.initiated.high = gr.initiated.high
  data.set$gr.initiated.medium = gr.initiated.medium
  data.set$gr.initiated.low = gr.initiated.low
  data.set$gr.initiated.highin = gr.initiated.highin
  data.set$gr.initiated.mediumin = gr.initiated.mediumin
  data.set$gr.initiated.lowin = gr.initiated.lowin
  data.set$gr.initiated.randomin = gr.initiated.randomin

  data.set = data.set[order(data.set$gr.bout.index),]
  row.nr = 1:nrow(data.set)
  subs = as.numeric(row.nr >= (nrow(data.set) / 2)) + 1
  data.list = list(
    low.certainty = data.set[data.set$gr.initiated.low == 1, ],
    med.certainty = data.set[data.set$gr.initiated.medium == 1, ],
    high.certainty = data.set[data.set$gr.initiated.high == 1, ],
    low.certainty.change = data.set[data.set$gr.initiated.low == 1 &
                                      subs == 1 | data.set$gr.initiated.lowin == 1 & subs == 2, ],
    med.certainty.change = data.set[data.set$gr.initiated.medium == 1 &
                                      subs == 1 | data.set$gr.initiated.mediumin == 1 & subs == 2, ],
    high.certainty.change = data.set[data.set$gr.initiated.high == 1 &
                                       subs == 1 | data.set$gr.initiated.highin == 1 & subs == 2, ],
    random.certainty = data.set[data.set$gr.initiated.random == 1, ]
  )

  return(data.list)
})

names(sim.data) = nr.ids

# now create simulations for each simulated dataset: select one focal per day, choose whether all days are included or just 2/3 or 1/3, and do consistency test

sim.results = lapply(sim.data, function(n){
  # parallelize
  mycluster <- makeCluster(11, type = "PSOCK")
  # export the relevant information to each core
  clusterExport(cl = mycluster,
                c("sim.data",
                  "n",
                  "consistency",
                  'nr.ids',
                  'cmpfun',
                  'comparison',
                  'standardisation'),
                envir = environment())
  registerDoParallel(mycluster)
  
  part.results = parLapply(cl = mycluster, X = 1:length(n), function(m){
    library(ggplot2)
    # select dataset
    dsi.data = n[[m]]
    
    #create durations for chosen individuals (randomly)
    dsi.data$Duration = 1
    for (i in 1:nrow(dsi.data)) {
      xx = rnorm(1000, mean = 60, sd = 50)
      xx = xx[xx > 20]
      dsi.data$Duration[i] = sample(xx, 1)
    }
    dsi.data$Sender = dsi.data$focal
    dsi.data$Receiver = dsi.data$pot.partner

    # get only one focal per day, only include 
    rand.dsi.data=dsi.data
    rand.dsi.data$chosen = 1
    rand.dsi.data=rand.dsi.data[complete.cases(rand.dsi.data),]
    rand.dsi.data$day.focal = unlist(lapply(unique(rand.dsi.data$date), function(i){
      xx = sample(unique(rand.dsi.data$focal[rand.dsi.data$date == i]), 1)
      xx = rep(xx, nrow(rand.dsi.data[rand.dsi.data$date == i,]))
      return(xx)
    }))
    rand.dsi.data=subset(rand.dsi.data, Sender == day.focal | Receiver == day.focal)

    # create for all focals all possible interaction partners per day and how many interactions they had
    subj.to.keep = unique(c(rand.dsi.data$Receiver, rand.dsi.data$Sender))
    dates = unique(rand.dsi.data$date)
    foc=sort(rep(subj.to.keep, times=length(subj.to.keep)))
    data.set=data.frame(individual1=foc, individual2=rep(subj.to.keep, times=length(subj.to.keep)), grooming.sent=0, observation.time=0)
    foc.dates=sort(rep(dates, times=nrow(data.set)))
    data.set=data.frame(individual1=rep(foc, times=length(dates)), individual2=rep(subj.to.keep, times=length(subj.to.keep)), date=as.Date(foc.dates), grooming.sent=0, grooming.interactions.sent=0, observation.time=0)

    data.set=subset(data.set, individual1!=individual2)
    data.set$dyad=apply(cbind(as.character(data.set$individual1), as.character(data.set$individual2)), 1, function(x){paste(sort(x), collapse="_")})

    # collate interactions
    for(i in 1:nrow(rand.dsi.data)){
      nr=which(data.set$individual1==rand.dsi.data$Sender[i] & data.set$individual2==rand.dsi.data$Receiver[i] & data.set$date==rand.dsi.data$date[i])
      nr2=which(data.set$individual2==rand.dsi.data$Sender[i] & data.set$individual1==rand.dsi.data$Receiver[i] & data.set$date==rand.dsi.data$date[i])
      data.set$grooming.sent[nr]=data.set$grooming.sent[nr]+rand.dsi.data$Duration[i]
      data.set$grooming.interactions.sent[nr]=data.set$grooming.interactions.sent[nr]+1
    }

    # create observation times of 12h per focal day
    data.set$individual1=as.character(data.set$individual1)
    data.set$individual2=as.character(data.set$individual2)

    for(i in 1:length(unique(data.set$individual1))){
      ind1=unique(data.set$individual1)[i]
      ind.dates=unique(rand.dsi.data$date[rand.dsi.data$foc==ind1])
      data.set$observation.time[(data.set$individual1==ind1|data.set$individual2==ind1) & data.set$date%in%ind.dates]=data.set$observation.time[(data.set$individual1==ind1|data.set$individual2==ind1) & data.set$date%in%ind.dates]+12
    }

    # randomly select full dataset, 2/3 or 1/3 of data days
    observation.effort = c(1, 0.66, 0.33)
    observation.data = lapply(observation.effort, function(y){
      yy=sample(unique(data.set$date), size = length(unique(data.set$date)) * y)
      rand.data.set=subset(data.set, date%in%yy)
      return(rand.data.set)
    })

    names(observation.data) = c(1, 0.66, 0.33)

    # for all datasets, create consistency measure
    consistency.frame = lapply(1:length(observation.data), function(x){
      xx = consistency(individual1 = observation.data[[x]]$individual1, individual2 = observation.data[[x]]$individual2, date = observation.data[[x]]$date, interactions = observation.data[[x]]$grooming.sent, observation.time = observation.data[[x]]$observation.time, k.seq = 0.02, j = 100, plot.col = 'black', behaviour = paste(c(names(n)[[m]], names(observation.data)[x]), collapse = ' '), average.duration = sum(observation.data[[x]]$grooming.sent)/sum(observation.data[[x]]$grooming.interactions.sent))
      xx.cons = xx$consistency
      xx.cons$observation.time = as.numeric(names(observation.data)[x])
      return(list(consist = xx.cons, plot = xx$plot))
    })
    
    #store data
    consistency.plot = lapply(consistency.frame, function(x) x$plot)
    consistency.frame = lapply(consistency.frame, function(x) x$consist)
    consistency.frame = do.call(rbind, consistency.frame)
    consistency.frame = consistency.frame[complete.cases(consistency.frame),]

    consistency.frame$inverted = as.numeric(grepl(names(n)[[m]], pattern = 'change', fixed = T))
    consistency.frame$certainty = unlist(strsplit(names(n)[[m]], split = '.', fixed = T))[1]
    return(list(consistency.frame = consistency.frame, plot = consistency.plot))
  })
  stopCluster(mycluster)
  part.plot = lapply(part.results, function(x) x$plot)
  part.results = lapply(part.results, function(x) x$consistency.frame)
  part.results = do.call(rbind, part.results)
  return(list(part.results = part.results, plot = part.plot))
})

sim.plot = lapply(sim.results, function(x) x$plot)

# collate all results
sim.results = lapply(sim.results, function(x) x$part.results)
all.results = lapply(1:length(sim.results), function(x){
  xx = sim.results[[x]]
  xx$dyads = (as.numeric(xx$individuals)^2 - as.numeric(xx$individuals))/2
  return(xx)
})

all.results = do.call(rbind, all.results)

all.results$interactions.dyad = all.results$interactions/all.results$dyads


#show that number of individuals is linear

#### impact of number of individuals
standardisation.individuals10 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1 & individuals == 10)))$ind.int
standardisation.individuals10$individuals = 10
standardisation.individuals15 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1 & individuals == 15)))$ind.int
standardisation.individuals15$individuals = 15
standardisation.individuals20 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1 & individuals == 20)))$ind.int
standardisation.individuals20$individuals = 20
standardisation.individuals = rbind(standardisation.individuals10, standardisation.individuals15, standardisation.individuals20)

individual.impact = ggplot(filter(all.results, inverted == 0 & certainty == 'high' & observation.time == 1) %>%
                             sample_frac(0.3, replace = F),
                           aes(x = interactions.dyad, y = cor.halves, color = as.factor(individuals))) +
  geom_point(alpha = 0.6, size = 2) + ylim(0,1) + xlim(0,20) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(standardisation.individuals, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(individuals)), size = 1.5) +
  # facet_grid(cols = vars(individuals)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Number of Individuals', values = c("red", "blue", "gold")) +
  ggtitle('1 Simulation Number Individuals')


#### impact of collection density

standardisation.collection1 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 15 & observation.time == 1)))$ind.int
standardisation.collection1$observation.time = 1
standardisation.collection0.66 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 15 & observation.time == 0.66)))$ind.int
standardisation.collection0.66$observation.time = 0.66
standardisation.collection0.33 = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 15 & observation.time == 0.33)))$ind.int
standardisation.collection0.33$observation.time = 0.33
standardisation.collection = rbind(standardisation.collection0.33, standardisation.collection0.66, standardisation.collection1)

collection.impact = ggplot(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 15) %>% sample_frac(0.3, replace = F), aes(x = interactions.dyad, y = cor.halves, color = as.factor(observation.time))) +
  geom_point(alpha = 0.6, size = 2) + ylim(0,1) + xlim(0,30) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(standardisation.collection, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(observation.time)), size = 1.5) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Observation Time', values = c("red", "blue", "gold"), labels = c("1/3 Days", "2/3 Days", "All Days")) +
  ggtitle('2 Simulation Collection Effort')


#### impact of certainty
all.results$certainty.a = all.results$certainty
all.results$certainty.a[all.results$certainty=='high'] = '1'
all.results$certainty.a[all.results$certainty=='low'] = '3'
all.results$certainty.a[all.results$certainty=='med'] = '2'
all.results$certainty.a = as.factor(as.character(all.results$certainty.a))

standardisation.certaintyhigh = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'high' & individuals == 15 & observation.time == 1)))$ind.int
standardisation.certaintyhigh$certainty.a = '1'
standardisation.certaintymedium = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'med' & individuals == 15 & observation.time == 1)))$ind.int
standardisation.certaintymedium$certainty.a = '2'
standardisation.certaintylow = standardisation(consistency.frame = data.frame(filter(all.results, inverted == 0 & certainty == 'low' & individuals == 15 & observation.time == 1)))$ind.int
standardisation.certaintylow$certainty.a = '3'
standardisation.certainty = rbind(standardisation.certaintylow, standardisation.certaintymedium, standardisation.certaintyhigh)

certainty.impact = ggplot(filter(all.results, inverted == 0 & observation.time == 1 & individuals == 15 & certainty != 'random') %>% sample_frac(0.1, replace = F), aes(x = interactions.dyad, y = cor.halves, color = as.factor(certainty.a))) +
  geom_point(alpha = 0.6, size = 2) + ylim(-0.1,1) + xlim(0,30) + 
  geom_line(standardisation.certainty, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(certainty.a)), size = 1.5) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 1) +
  # facet_grid(cols = vars(certainty)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad')  +
  scale_color_manual(name = 'Certainty of Partner Choice', values = c("red", "blue", "gold"), labels = c("High", "Medium", "Low")) +
  ggtitle('Simulation Certainty Partner Choice')



#### impact of collection conditions

all.results$condition = 'None'
all.results$condition[all.results$inverted == 1] = 'inverted'
all.results$condition[all.results$inverted == 0] = 'consistent'
all.results$condition[all.results$certainty == 'random'] = 'random'


standardisation.conditionconsistent = standardisation(consistency.frame = data.frame(filter(all.results, certainty == 'high' & individuals == 15 & observation.time == 1 & condition == 'consistent')))$ind.int
standardisation.conditionconsistent$condition = 'consistent'
standardisation.conditioninverted = standardisation(consistency.frame = data.frame(filter(all.results, certainty == 'high' & individuals == 15 & observation.time == 1 & condition == 'inverted')))$ind.int
standardisation.conditioninverted$condition = 'inverted'
standardisation.conditionrandom = standardisation(consistency.frame = data.frame(filter(all.results, certainty == 'random' & individuals == 15 & observation.time == 1)))$ind.int
standardisation.conditionrandom$condition = 'random'
standardisation.condition = rbind(standardisation.conditionrandom, standardisation.conditioninverted, standardisation.conditionconsistent)


condition.impact = ggplot(filter(all.results, observation.time == 1 &
                                   individuals == 15 &
                                   certainty %in% c('high', 'random') & condition != 'None') %>% sample_frac(0.3, replace = F),
                          aes(x = interactions.dyad, y = cor.halves, color = as.factor(condition))) +
  geom_point(alpha = 0.4, size = 2) + ylim(-0.3,1) +
  geom_line(standardisation.condition, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(condition)), size = 1.5) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_hline(aes(yintercept = 0), linetype = 1) +
  theme_minimal()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  # facet_grid(cols = vars(condition)) +
  scale_color_manual(name = 'Condition', values = c("red", "blue", "gold")) +
  ggtitle('Simulation Condition')


library(gridExtra)

tiff("Fig 3.tiff", width = 12, height = 8, units = 'in', res = 300)
grid.arrange(individual.impact, collection.impact, ncol = 2)
dev.off()
tiff("Fig 4.tiff", width = 8, height = 6, units = 'in', res = 300)
certainty.impact
dev.off()
tiff("Fig 5.tiff", width = 8, height = 6, units = 'in', res = 300)
condition.impact
dev.off()