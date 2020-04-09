#Cayo monkey data
#Get individual network measures by year for centrality, weighted degree, clustering coefficient, betweenness, weighted eigenvector

# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/") 
source("Social_Network_Analysis/CalcSubsampledScans.R")
source("Social_Network_Analysis/functions_GlobalNetworkMetrics.R")
source("Social_Network_Analysis/KinshipPedigree.R")

#Load scan data, population and dominance info
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")
dominance_info =read.table("Behavioral_Data/Database Complete/Data All Raw/DOMINANCE.txt",header = T)

AllStats = list()
hrs=5

# 1. Calculate random subsamples
randomScans = calcRandomScans(allScans)

#For each group, each year separately: 
group = c("V","KK")

for (g in 1:length(group)){ #For each group
  randscansG = randomScans[which(randomScans$group==group[g]),] 
  
  years = unique(randscansG$year)
  for (y in 1:length(years)){ #For each year in that group
    randscansY = randscansG[which(randscansG$year==years[y]),] 
    year = years[y]
    
    isPost = c(0,1)
    for (h in 1:length(isPost)){ #pre- and post-hurricane 
      
      rscans = randscansY[which(randscansY$isPost==isPost[h]),] 
      # 2. Find the number of unique IDs.
      #Find all unique IDs
      unqIDs = unique(c(as.character(rscans$focalID)))
      
      # 3. Output the Master Edgelist of all possible pairs given the unique IDs.
      masterEL = calcMasterEL(unqIDs)
      
      # 4. Output weighted edgelist from the Master Edgelist.
      options(warn = -1) #set options to ignore all warnings
      action = "groom"; proxdata =0
      weightedEL = calcEdgeList(rscans,masterEL, action, proxdata)
      weightedEL$weight <- round(weightedEL$count / hrs, 5) #add weight information by dividing by the #hrs spent observing
      weightedEL$count <- NULL;weightedEL$conc <- NULL #delete those calumn variables
      
      if (years[y]!=2013) { #KK2013 has to be excluded
        
        #Need to upload an adjacency matrix, rather than socprog style data...
        adjMat = dils::AdjacencyFromEdgelist(weightedEL)
        data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
        
        #read adjacency matrix
        m=as.matrix(data) # coerces the data set as a matrix
        am.g=graph.adjacency(m,mode="directed",weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
        am.g #igraph object properties The letters on the first line (there can be up to 4) indicates some basic information about the graph. 
        #The first letter indicates whether this is a directed ('D') or undirected ('U') graph. 
        #The 2nd letter tells you if  this is a named ('N') graph--i.e., whether or not the vertex set has a 'name' attribute. 
        #The 3rd letter tells you if this graph is weighted ('W'). 
        #The fourth letter is 'B' for bipartite graphs. These letter codes are followed by two numbers: the first is the number of vertices and the second is the number of edges.
        
        
        # #Get the network measures
        # #Weighted degree (Strength, undirected)
        # A.Weightdeg<-degree(data)
        # #weighted indegree
        # A.Weight.IN.deg <-degree(data, gmode="digraph", diag=FALSE, tmaxdev=FALSE, cmode="indegree", rescale=FALSE, ignore.eval=FALSE)
        # #weighted outdegree
        # A.Weight.OUT.deg <-degree(data, gmode="digraph", diag=FALSE, tmaxdev=FALSE, cmode="outdegree", rescale=FALSE, ignore.eval=FALSE)
        # #Binary degree (inDegree)
        # A.Bindeg<-apply(data,2,function(a)sum(a>0))
        # #binary outdegree
        # A.Boutdeg<-apply(data,1,function(a)sum(a>0))
        # #clustering coeff (local)
        # A.Clust<-transitivity(am.g,"local")
        # #Weighted betweenness
        # A.Weight.between<-betweenness(data,gmode="graph",ignore.eval=F)
        # #Weighted eigenvector centrality
        # A.Weight.eig.cen<-evcent(data,gmode="graph",ignore.eval=F)
        # 
        # #Create a dataframe with all individuals included
        # networkMeasures<-data.frame(cbind(A.Weightdeg,A.Weight.IN.deg,A.Weight.OUT.deg, A.Bindeg, A.Boutdeg, A.Clust,A.Weight.between,A.Weight.eig.cen))
        # 
        # #Output all network measures to the final file
        # write.table(networkMeasures, file = "A.2013F_Centrality_Output.csv", sep = ",", col.names=T, row.names=T)
        
        
        
        #increase space between nodes if overlapping
        #fruchterman reingold layout
        l <- layout.fruchterman.reingold(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)
        #changes size of labels of vertices
        V(am.g)$label.cex <- 0.5
        
        
        #plot graph
        png(paste("Social Network ",group[g],years[y],".",isPost[h],".png",sep=""))
        plot.igraph(am.g,layout=l, vertex.label=V(am.g)$name, vertex.color="CYAN1", vertex.size=7,edge.color="grey20", 
                    edge.width=E(am.g)$weight*2,edge.arrow.size = 0.5, main = paste("Social Network ",group[g],years[y],".",isPost[h],sep=""))
        dev.off()
        
    
        # plot.igraph(am.g,layout=l, vertex.color="CYAN1", vertex.size=7,edge.color="grey20", 
        #             edge.width=E(am.g)$weight*2,edge.arrow.size = 0.5)
        # 
        # 
        # #spring embedded layout
        # s <- layout.spring(am.g, spring.length=1000,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)
        # 
        # #REMOVE NODE LABEL
        # plot.igraph(am.g,layout=l, vertex.label=NA, vertex.color="orange", vertex.size=6,edge.color="grey20",edge.width=E(am.g)$weight*0.01)
        # 
        # #plot spring embedded graph
        # plot.igraph(am.g,layout=s, vertex.size=6, vertex.label.cex=0.5, edge.color="grey20",edge.width=E(am.g)$weight*0.01)
        
        
        #Import an attribute file for labelling nodes
        # attr<-read.csv("ATTRIBUTES_allsubjects.csv")
        # str(attr)
        
        #link up attributes file with network (DOESN'T WORK YET)
        #V(am.g)$agec=as.factor(att$agec[match(V(am.g)$name,attr$id)])
        #V(am.g)$agec
        
        #set size of nodes by animal age
        #V(am.g)$size=V(am.g)$agec*6 #multiplied by 5 because nodes too small otherwise
        
        #set colour of sexes
        #V(am.g)$color=V(am.g)$sex #assign the "Sex" attribute as the vertex color
        #V(am.g)$color=gsub("0","orange",V(am.g)$color) #Females will be orange
        #V(am.g)$color=gsub("1","lightblue",V(am.g)$color) #Males will be lightblue
        #V(am.g)$color=gsub("2","white",V(am.g)$color) #unknown sex will be white
        
      }
    }
  }
}  