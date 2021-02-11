#### Analysis of mangabey and chimpanzee data for Mielke et al 2020 'Consistency of social interactions in sooty mangabeys and chimpanzees'

# load data and functions
load("prepared_data.RData")
source('consistency_function.R')
library(compiler)
library(tidyverse)
library(ggplot2)
library(gridExtra)
consistency = cmpfun(consistency)

#### create consistencies: East group
east.consistency.grooming = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$grooming.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'green', behaviour = 'grooming', average.duration = sum(east.data.set$grooming.sent)/sum(east.data.set$grooming.interactions.sent))
east.consistency.aggression.contact = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$aggression.physical.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'contact aggression')
east.consistency.aggression.noncontact = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$aggression.nonphysical.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'noncontact aggression')
east.consistency.aggression = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$aggression.physical.sent + east.data.set$aggression.nonphysical.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'aggression')
east.consistency.proximity = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, undirectional = TRUE, date = east.data.set$date, interactions = east.data.set$proximity3m, observation.time = east.data.set$obs.time.without.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'proximity', average.duration = sum(east.data.set$proximity3m)/sum(east.data.set$proximity3m.interactions))
east.consistency.bodycontact = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, undirectional = TRUE,  date = east.data.set$date, interactions = east.data.set$proximity, observation.time = east.data.set$obs.time.without.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'body contact', average.duration = sum(east.data.set$proximity)/sum(east.data.set$proximity.interactions))
east.consistency.foodshare = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$food.share.sent, observation.time = east.data.set$obs.time.without.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'food sharing')
east.consistency.pant = consistency(individual1 = east.data.set$individual1, individual2 = east.data.set$individual2, date = east.data.set$date, interactions = east.data.set$pant.grunt.sent, observation.time = east.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'pant grunt')

#### create consistencies: South group
south.consistency.grooming = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$grooming.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'blue', behaviour = 'grooming', average.duration = sum(south.data.set$grooming.sent)/sum(south.data.set$grooming.interactions.sent))
south.consistency.aggression.contact = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$aggression.physical.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'blue',behaviour = 'contact aggression')
south.consistency.aggression.noncontact = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$aggression.nonphysical.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'blue',behaviour = 'noncontact aggression')
south.consistency.aggression = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$aggression.physical.sent + south.data.set$aggression.nonphysical.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'blue',behaviour = 'aggression')
south.consistency.proximity = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, undirectional = TRUE,  interactions = south.data.set$proximity3m, observation.time = south.data.set$obs.time.without.ass, k.seq = 0.01, j = 100, plot.col = 'blue',behaviour = 'proximity', average.duration = sum(south.data.set$proximity3m)/sum(south.data.set$proximity3m.interactions))
south.consistency.bodycontact = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, undirectional = TRUE, interactions = south.data.set$proximity, observation.time = south.data.set$obs.time.without.ass, k.seq = 0.01, j = 100, plot.col = 'blue',behaviour = 'body contact', average.duration = sum(south.data.set$proximity)/sum(south.data.set$proximity.interactions))
south.consistency.foodshare = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$food.share.sent, observation.time = south.data.set$obs.time.without.ass, k.seq = 0.01, j = 100, plot.col = 'blue',behaviour = 'food sharing')
south.consistency.pant = consistency(individual1 = south.data.set$individual1, individual2 = south.data.set$individual2, date = south.data.set$date, interactions = south.data.set$pant.grunt.sent, observation.time = south.data.set$obs.time.with.ass, k.seq = 0.01, j = 100, plot.col = 'green',behaviour = 'pant grunt')

#### create consistencies: Mangabey group
mang.consistency.grooming = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$grooming.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.01, j = 100, plot.col = 'yellow', behaviour = 'grooming', average.duration = sum(mang.data.set$grooming.sent)/sum(mang.data.set$grooming.interactions.sent))
mang.consistency.aggression.contact = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$aggression.physical.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.01, j = 100, plot.col = 'yellow',behaviour = 'contact aggression')
mang.consistency.aggression.noncontact = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$aggression.nonphysical.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.01, j = 100, plot.col = 'yellow',behaviour = 'noncontact aggression')
mang.consistency.aggression = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$aggression.physical.sent + mang.data.set$aggression.nonphysical.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.01, j = 100, plot.col = 'yellow',behaviour = 'aggression')
mang.consistency.proximity = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$proximity3m, undirectional = TRUE, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.01, j = 50, plot.col = 'yellow',behaviour = 'proximity')
mang.consistency.bodycontact = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$proximity, undirectional = TRUE, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.01, j = 100, plot.col = 'yellow',behaviour = 'body contact')
mang.consistency.supplant = consistency(individual1 = mang.data.set$individual1, individual2 = mang.data.set$individual2, date = mang.data.set$date, interactions = mang.data.set$supplant.sent, observation.time = mang.data.set$observation.time.with.assistants, k.seq = 0.01, j = 100, plot.col = 'yellow',behaviour = 'supplant')

#### overview of standardised consistency values
east.stand = rbind(east.consistency.grooming$standardised,
                   east.consistency.bodycontact$standardised,
                   east.consistency.proximity$standardised,
                   east.consistency.aggression.contact$standardised,
                   east.consistency.aggression.noncontact$standardised,
                   east.consistency.aggression$standardised,
                   east.consistency.foodshare$standardised,
                   east.consistency.pant$standardised)
east.stand$behaviour = c('grooming', 'body contact', 'proximity', 'contact aggression', 'noncontact aggression', 'aggression', 'food share', 'pant grunt')

mang.stand = rbind(mang.consistency.grooming$standardised,
                   mang.consistency.bodycontact$standardised,
                   mang.consistency.proximity$standardised,
                   mang.consistency.aggression.contact$standardised,
                   mang.consistency.aggression.noncontact$standardised,
                   mang.consistency.aggression$standardised,
                   mang.consistency.supplant$standardised)
mang.stand$behaviour = c('grooming', 'body contact', 'proximity', 'contact aggression', 'noncontact aggression', 'aggression', 'supplant')

south.stand = rbind(south.consistency.grooming$standardised,
                   south.consistency.bodycontact$standardised,
                   south.consistency.proximity$standardised,
                   south.consistency.aggression.contact$standardised,
                   south.consistency.aggression.noncontact$standardised,
                   south.consistency.aggression$standardised,
                   south.consistency.foodshare$standardised,
                   south.consistency.pant$standardised)
south.stand$behaviour = c('grooming', 'body contact', 'proximity', 'contact aggression', 'noncontact aggression', 'aggression', 'food share', 'pant grunt')

#combined
all.stand = rbind(
  cbind(south.stand, group = 'south'),
  cbind(east.stand, group = 'east'),
  cbind(mang.stand, group = 'mangabey')
)

all.stand = all.stand[all.stand$behaviour!='food share' & all.stand$behaviour!='contact aggression' & all.stand$behaviour!='aggression',]
all.stand$behaviour[all.stand$behaviour%in%c('supplant', 'pant grunt')] = 'pant grunt/supplant'


############ Create plots comparing the groups

## grooming
s.groomingeast = standardisation(consistency.frame = east.consistency.grooming$consistency)$ind.int
s.groomingeast$group = 'east'
s.groomingsouth = standardisation(consistency.frame = south.consistency.grooming$consistency)$ind.int
s.groomingsouth$group = 'south'
s.groomingmang = standardisation(consistency.frame = mang.consistency.grooming$consistency)$ind.int
s.groomingmang$group = 'mangabey'
s.grooming = rbind(s.groomingeast, s.groomingsouth, s.groomingmang)
all.grooming  = rbind(
  cbind(east.consistency.grooming$consistency, 
        group = rep('east', nrow(east.consistency.grooming$consistency))),
  cbind(south.consistency.grooming$consistency, 
        group = rep('south', nrow(south.consistency.grooming$consistency))),
  cbind(mang.consistency.grooming$consistency, 
        group = rep('mangabey', nrow(mang.consistency.grooming$consistency)))
  )

grooming.plot = ggplot(all.grooming  %>%
                             sample_frac(0.4, replace = T),
                           aes(x = interactions.per.dyad, y = cor.halves, color = group)) +
  geom_point(alpha = 0.4, size = 1) + ylim(0,1) + xlim(0,10) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(s.grooming, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(group)), size = 1.5) +
  #facet_grid(cols = vars(group)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Group', values = c("red", "blue", "gold")) +
  ggtitle('Grooming')


## bodycontact
s.bodycontacteast = standardisation(consistency.frame = east.consistency.bodycontact$consistency)$ind.int
s.bodycontacteast$group = 'east'
s.bodycontactsouth = standardisation(consistency.frame = south.consistency.bodycontact$consistency)$ind.int
s.bodycontactsouth$group = 'south'
s.bodycontactmang = standardisation(consistency.frame = mang.consistency.bodycontact$consistency)$ind.int
s.bodycontactmang$group = 'mangabey'
s.bodycontact = rbind(s.bodycontacteast, s.bodycontactsouth, s.bodycontactmang)
all.bodycontact  = rbind(
  cbind(east.consistency.bodycontact$consistency, 
        group = rep('east', nrow(east.consistency.bodycontact$consistency))),
  cbind(south.consistency.bodycontact$consistency, 
        group = rep('south', nrow(south.consistency.bodycontact$consistency))),
  cbind(mang.consistency.bodycontact$consistency, 
        group = rep('mangabey', nrow(mang.consistency.bodycontact$consistency)))
)

bodycontact.plot = ggplot(all.bodycontact  %>%
                         sample_frac(0.5, replace = F),
                       aes(x = interactions.per.dyad, y = cor.halves, color = group)) +
  geom_point(alpha = 0.4, size = 1) + ylim(0,1) + xlim(0,30) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(s.bodycontact, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(group)), size = 1.5) +
  #facet_grid(cols = vars(group)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Group', values = c("red", "blue", "gold")) +
  ggtitle('Body contact')


## proximity
s.proximityeast = standardisation(consistency.frame = east.consistency.proximity$consistency)$ind.int
s.proximityeast$group = 'east'
s.proximitysouth = standardisation(consistency.frame = south.consistency.proximity$consistency)$ind.int
s.proximitysouth$group = 'south'
s.proximitymang = standardisation(consistency.frame = mang.consistency.proximity$consistency)$ind.int
s.proximitymang$group = 'mangabey'
s.proximity = rbind(s.proximityeast, s.proximitysouth, s.proximitymang)
all.proximity  = rbind(
  cbind(east.consistency.proximity$consistency, 
        group = rep('east', nrow(east.consistency.proximity$consistency))),
  cbind(south.consistency.proximity$consistency, 
        group = rep('south', nrow(south.consistency.proximity$consistency))),
  cbind(mang.consistency.proximity$consistency, 
        group = rep('mangabey', nrow(mang.consistency.proximity$consistency)))
)

proximity.plot = ggplot(all.proximity  %>%
                          sample_frac(0.5, replace = F),
                          aes(x = interactions.per.dyad, y = cor.halves, color = group)) +
  geom_point(alpha = 0.4, size = 1) + ylim(0,1) + xlim(0,35) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(s.proximity, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(group)), size = 1.5) +
  #facet_grid(cols = vars(group)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Group', values = c("red", "blue", "gold")) +
  ggtitle('Proximity')


## aggression contact
s.aggression.contacteast = standardisation(consistency.frame = east.consistency.aggression.contact$consistency)$ind.int
s.aggression.contacteast$group = 'east'
s.aggression.contactsouth = standardisation(consistency.frame = south.consistency.aggression.contact$consistency)$ind.int
s.aggression.contactsouth$group = 'south'
s.aggression.contactmang = standardisation(consistency.frame = mang.consistency.aggression.contact$consistency)$ind.int
s.aggression.contactmang$group = 'mangabey'
s.aggression.contact = rbind(s.aggression.contacteast, s.aggression.contactsouth, s.aggression.contactmang)
all.aggression.contact  = rbind(
  cbind(east.consistency.aggression.contact$consistency, 
        group = rep('east', nrow(east.consistency.aggression.contact$consistency))),
  cbind(south.consistency.aggression.contact$consistency, 
        group = rep('south', nrow(south.consistency.aggression.contact$consistency))),
  cbind(mang.consistency.aggression.contact$consistency, 
        group = rep('mangabey', nrow(mang.consistency.aggression.contact$consistency)))
)

aggression.contact.plot = ggplot(all.aggression.contact  %>%
                                   sample_frac(0.5, replace = F),
                        aes(x = interactions.per.dyad, y = cor.halves, color = group)) +
  geom_point(alpha = 0.4, size = 1) + ylim(0,1) + xlim(0,3) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(s.aggression.contact, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(group)), size = 1.5) +
  #facet_grid(cols = vars(group)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Group', values = c("red", "blue", "gold")) +
  ggtitle('Aggression Contact')


## aggression noncontact
s.aggression.noncontacteast = standardisation(consistency.frame = east.consistency.aggression.noncontact$consistency)$ind.int
s.aggression.noncontacteast$group = 'east'
s.aggression.noncontactsouth = standardisation(consistency.frame = south.consistency.aggression.noncontact$consistency)$ind.int
s.aggression.noncontactsouth$group = 'south'
s.aggression.noncontactmang = standardisation(consistency.frame = mang.consistency.aggression.noncontact$consistency)$ind.int
s.aggression.noncontactmang$group = 'mangabey'
s.aggression.noncontact = rbind(s.aggression.noncontacteast, s.aggression.noncontactsouth, s.aggression.noncontactmang)
all.aggression.noncontact  = rbind(
  cbind(east.consistency.aggression.noncontact$consistency, 
        group = rep('east', nrow(east.consistency.aggression.noncontact$consistency))),
  cbind(south.consistency.aggression.noncontact$consistency, 
        group = rep('south', nrow(south.consistency.aggression.noncontact$consistency))),
  cbind(mang.consistency.aggression.noncontact$consistency, 
        group = rep('mangabey', nrow(mang.consistency.aggression.noncontact$consistency)))
)

aggression.noncontact.plot = ggplot(all.aggression.noncontact  %>%
                                      sample_frac(0.5, replace = F),
                                 aes(x = interactions.per.dyad, y = cor.halves, color = group)) +
  geom_point(alpha = 0.4, size = 1) + ylim(0,1) + xlim(0,6) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(s.aggression.noncontact, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(group)), size = 1.5) +
  #facet_grid(cols = vars(group)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Group', values = c("red", "blue", "gold")) +
  ggtitle('Aggression Noncontact')


## pant grunt/supplant
s.panteast = standardisation(consistency.frame = east.consistency.pant$consistency)$ind.int
s.panteast$group = 'east'
s.pantsouth = standardisation(consistency.frame = south.consistency.pant$consistency)$ind.int
s.pantsouth$group = 'south'
s.pantmang = standardisation(consistency.frame = mang.consistency.supplant$consistency)$ind.int
s.pantmang$group = 'mangabey'
s.pant = rbind(s.panteast, s.pantsouth, s.pantmang)
all.pant  = rbind(
  cbind(east.consistency.pant$consistency, 
        group = rep('east', nrow(east.consistency.pant$consistency))),
  cbind(south.consistency.pant$consistency, 
        group = rep('south', nrow(south.consistency.pant$consistency))),
  cbind(mang.consistency.supplant$consistency, 
        group = rep('mangabey', nrow(mang.consistency.supplant$consistency)))
)

pant.plot = ggplot(all.pant  %>%
                                      sample_frac(0.5, replace = F),
                                    aes(x = interactions.per.dyad, y = cor.halves, color = group)) +
  geom_point(alpha = 0.4, size = 1) + ylim(0,1) + xlim(0,6) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(s.pant, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(group)), size = 1.5) +
  #facet_grid(cols = vars(group)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Group', values = c("red", "blue", "gold")) +
  ggtitle('Pant Grunt/ Supplant')



## food share
s.foodshareeast = standardisation(consistency.frame = east.consistency.foodshare$consistency)$ind.int
s.foodshareeast$group = 'east'
s.foodsharesouth = standardisation(consistency.frame = south.consistency.foodshare$consistency)$ind.int
s.foodsharesouth$group = 'south'
s.foodshare = rbind(s.foodshareeast, s.foodsharesouth)
all.foodshare  = rbind(
  cbind(east.consistency.foodshare$consistency, 
        group = rep('east', nrow(east.consistency.foodshare$consistency))),
  cbind(south.consistency.foodshare$consistency, 
        group = rep('south', nrow(south.consistency.foodshare$consistency)))
)

foodshare.plot = ggplot(all.foodshare  %>%
                                      sample_frac(0.5, replace = F),
                                    aes(x = interactions.per.dyad, y = cor.halves, color = group)) +
  geom_point(alpha = 0.4, size = 1) + ylim(0,1) + xlim(0,3) +
  geom_hline(aes(yintercept = 0.5), linetype = 2) +
  geom_line(s.foodshare, mapping = aes(x = average.interactions.per.dyad, y = average.median, color = as.factor(group), shape = group), size = 1.5) +
  #facet_grid(cols = vars(group)) +
  theme_classic()  + labs(y = "Spearman Correlation Coefficient", x = 'Interactions per Dyad') +
  scale_color_manual(name = 'Group', values = c("red", "blue", "gold")) +
  ggtitle('Food Share')




#### standardised results
stand.plot = ggplot(all.stand, aes(x = behaviour, y = interaction.per.dyad, color = group, fill = group, shape = group)) +
  geom_point(size = 3) +
  theme_bw() +
  labs(y = "Interactions per Dyad to reach r = 0.5", x = 'Interaction Type') +
  ggtitle('Summary Standardised Consistency') + 
  scale_color_manual(values = c("red", "blue", "gold"))



tiff("Fig Grooming Food.tiff", width = 12, height = 8, units = 'in', res = 300)
grid.arrange(grooming.plot, foodshare.plot, ncol = 2)
dev.off()

tiff("Fig Bodycontact Proximity.tiff", width = 12, height = 8, units = 'in', res = 300)
grid.arrange(bodycontact.plot, proximity.plot, ncol = 2)
dev.off()

tiff("Fig Aggression.tiff", width = 8, height = 12, units = 'in', res = 300)
grid.arrange(aggression.noncontact.plot, aggression.contact.plot, pant.plot)
dev.off()

tiff("Fig Summary.tiff", width = 8, height = 6, units = 'in', res = 300)
stand.plot
dev.off()

