#Plotting social network metric measures, comparing pre- post hurricane, by quarter

library(ggplot2)
## Load Permutated Network Metrics
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/Original_Sam_Rscripts")# set working directory
permutedNetsQ <- read.csv("permutedNetsQ.txt")

########################################################  
## Global network metric distribution comparisonss
########################################################

# open pdf file for later saving
pdf(file="GlobalNetworkMetrics.pdf", width=3.75, height=4.25, onefile = T) #width and height of the graphics region in inches. One file if true = multiple graphs in one file

# For Network Density
{ ggplot(permutedNetsQ, aes(x= as.factor(quarter), y=dens, color=as.factor(isPost), fill=as.factor(isPost) ))+
    geom_boxplot()+
    facet_grid(~group)+
    theme_bw()
} 
# For Network equity
{ ggplot(permutedNetsQ, aes(x= as.factor(quarter), y=gini, color=as.factor(isPost), fill=as.factor(isPost) ))+
    geom_boxplot()+
    facet_grid(~group)+
    theme_bw()
}

########################################################  
## Preference for kin
########################################################

# open pdf file for later saving
pdf(file="PreferenceKin.pdf", width=3.75, height=4.25, onefile = T) #width and height of the graphics region in inches. One file if true = multiple graphs in one file

# For Network Density
{ ggplot(permutedNetsQ, aes(x= as.factor(quarter), y=dens, color=as.factor(isPost), fill=as.factor(isPost) ))+
    geom_boxplot()+
    facet_grid(~group)+
    theme_bw()
} 
# For Network equity
{ ggplot(permutedNetsQ, aes(x= as.factor(quarter), y=gini, color=as.factor(isPost), fill=as.factor(isPost) ))+
    geom_boxplot()+
    facet_grid(~group)+
    theme_bw()
}
