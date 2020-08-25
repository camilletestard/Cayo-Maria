# Generate_TempExpRandomGraphMetrics: 
# This script uses TERGMs to model bond formation and dissolution. Here, I focus on 
# grooming network data. We can include a number of parameters to test what is driving bond formation or dissolution 
# (e.g. homophily terms, closure of triads, reciprocity etc.). Once again, i run the model over mutliple iterations to account
# for all of the data (through subsampling).
# Bond formation model ~ edges + gwesp + mutual + kinship edge cov.+ Proximity edge cov.
# edges = control for the change in degree
# gwesp = model whether there are more triangles than expected by chance for a network of this size and density, and thus
# that there is some sort of explicit triangle closure effect going on.
# mutual = test whether bond formation is more likely in the case of reciprocating a bond
# kinship & proximity edge covariate = test whether bond formation between two nodes is more or less likely to form as kin 
# relationship or proximity increases 
# Positive coeff = relationshis is more likely than chance to form (negative -> less likely)
# Bond dissolution model ~ edges + kinship edge cov.+ Proximity edge cov.
# Positive coeff = bond is more likely than chance to persist in the next time step.
# For more information:
#   Silk et al 2017 (Animal Behavior)
# http://statnet.org/Workshops/ergm_tutorial.html#appendix_a:_clarifying_the_terms_%E2%80%93_ergm_and_network
# http://statnet.org/Workshops/tergm_tutorial.html

#load required packages
library(network)
library(ergm)
library(dplyr)
# library(sna)
# library(igraph)
# library(tnet)
library(stringr)
library(ggplot2)
library(statnet)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)

# http://statnet.org/Workshops/ergm_tutorial.html#appendix_a:_clarifying_the_terms_%E2%80%93_ergm_and_network
# http://statnet.org/Workshops/tergm_tutorial.html

#load local functions
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/cleaned_code/functions") 
source("CalcSubsampledScans.R")
source("functions_Homophily.R")
source("KinshipPedigree.R")

#Load scan data, population and dominance info
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans.txt")
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("R.Data/SocialCapital.RData")
SocialCapital.ALL$groupyear = paste(SocialCapital.ALL$group, SocialCapital.ALL$year,sep="")
#Create gregariousness category
threshold=as.numeric(quantile(SocialCapital.ALL$std.DSIgroom, probs = 0.70))
SocialCapital.ALL$groomCat="greg"; 
SocialCapital.ALL$groomCat[which(SocialCapital.ALL$std.DSIgroom<threshold)]="shy"

#Compute pedigree
pedigree=bigped[,c("ID","DAM","SIRE")]
ped <- KinshipPedigree(pedigree)

num_iter = 275

#For each group, each year separately: 
group = c("V","V","V","KK","KK") #c("V","V","V","V","V","KK","KK","KK","S")
years = c(2015, 2016,2017,2015, 2017)#c(2015,2016,2017,2018, 2019, 2015, 2017, 2018, 2019)
groupyears =c("V2015","V2016","V2017","KK2015","KK2017") #c("V2015","V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018", "S2019") 

#Initilize dataframes, time and iteration variables.
TERGMeffects = data.frame(); TERGMeffects.ALL=data.frame(); count=0
start_time <- Sys.time(); iter=1; a=1; gy=1

# load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/TERGMeffects.RData")
for (iter in 1:num_iter){ #for all iterations
  
  print(paste("%%%%%%%%%%%%%%%%%% ", iter, " %%%%%%%%%%%%%%%%%%")) #print iteration to keep track
  
  # 1. Calculate random subsamples
  randomScans = calcRandomScans(allScans)
  randomScans$groupyear = paste(randomScans$group, randomScans$year,sep="")
  
  # 2. For each group, each year and pre-/post-hurr separately, compute weighted edge list: 
  gy=1
  for (gy in 1:length(groupyears)){ #For each groupyear 
    randscansY = randomScans[which(randomScans$groupyear==groupyears[gy]),] #subselect scans of group G
    
    isPost = c(0,1); h=1
    for (h in 1:length(isPost)){ #pre- and post-hurricane 
      
      rscans = randscansY[which(randscansY$isPost==isPost[h]),] #subselect scans of group G, year Y and hurricane status H
      numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
      
      #Find all unique IDs
      unqIDs = unique(as.character(rscans$focalID))
      
      # Output the Master Edgelist of all possible pairs given the unique IDs.
      masterEL = calcMasterEL_groom(unqIDs)
      
      # Output weighted edgelist from the Master Edgelist for grooming network
      weightedEL.groom = calcEdgeList_groom(rscans,masterEL)
      weightedEL.groom = weightedEL.groom[,c("givingID","receivingID","count")]
      weightedEL.groom$conc = paste(weightedEL.groom$givingID, weightedEL.groom$receivingID, sep=".")
      
      weightedEL.groom$numscans <- (numscans$freq[match(weightedEL.groom$givingID, numscans$id)] + numscans$freq[match(weightedEL.groom$receivingID, numscans$id)])/2 
      weightedEL.groom$weight <- round(weightedEL.groom$count / weightedEL.groom$numscans, 5) #add weight information by dividing by avg #observations for each ID pair
      meanweight = mean(weightedEL.groom$weight[weightedEL.groom$weight>0]) #compute nonzero mean weight
      weightedEL.groom$weight <- weightedEL.groom$weight/meanweight
      
      el = weightedEL.groom[,c("givingID","receivingID",'weight'),];
      adjMat = dils::AdjacencyFromEdgelist(el)# create adjacency matrix based on edge list.
      mat = adjMat[["adjacency"]]; rownames(mat) = adjMat[["nodelist"]]; colnames(mat) = adjMat[["nodelist"]]
      net<-as.network.matrix(mat,loops=FALSE,directed=T)
      
      #set kinship as edge attribute
      KC      <- NULL; for(i in 1:length(el[,1])){ 
        KC[i] <-  ped[which(rownames(ped)==as.character(el$receivingID[i])) , which(colnames(ped)==as.character(el$givingID[i]))]
      }
      set.network.attribute(net, "kinship",KC)
      
      #set proximity as edge attribute
      options(warn = -1) #set options to ignore all warnings
      weightedEL.prox = calcEdgeList(rscans,masterEL) #Get counts for proximity network
      weightedEL.prox$numscans <- (numscans$freq[match(weightedEL.prox$givingID, numscans$id)] + numscans$freq[match(weightedEL.prox$receivingID, numscans$id)])/2 
      weightedEL.prox$weight <- round(weightedEL.prox$count / weightedEL.prox$numscans, 5) #add weight information by dividing by avg #observations for each ID pair
      meanweight = mean(weightedEL.prox$weight[weightedEL.prox$weight>0]) #compute nonzero mean weight
      weightedEL.prox$weight <- weightedEL.prox$weight/meanweight
      el.prox = weightedEL.prox[,c("givingID","receivingID",'weight'),];
      prox <-NULL; for(i in 1:nrow(el.prox)){
        prox[i]<- el.prox$weight[which(el.prox$givingID==el$givingID[i] & el.prox$receivingID==el$receivingID[i])]
      }
      set.network.attribute(net, "prox",prox)
      
      if(isPost[h]==0){net.pre=net}
      if(isPost[h]==1){net.post=net}
      
    }
    #combine pre- and post networks
    prePostList = list(net.pre, net.post) 
    prePostNet <- networkDynamic(network.list=prePostList)#creating a "dynamic network" structure for temporal analyses
    
    # #plot networks
    # par(mfrow = c(2,2), oma=c(1,1,1,1), mar=c(4,1,1,1))
    # plot(network.extract(prePostNet, at = 0), main = "Time 1", 
    #      displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
    # plot(network.extract(prePostNet, at = 1), main = "Time2", 
    #      displaylabels = T, label.cex = 0.6, vertex.cex = 2, pad = 0.5)
    # 
    # #Descriptive temporal network analysis
    # tSnaStats(prePostNet,"degree") # Changes in degree centrality
    # tErgmStats(prePostNet, "~ edges+triangle") # Notice the increase in triangles
    # 
    # #Visualize dynamics: 
    # render.d3movie(prePostNet, 
    #                plot.par=list(displaylabels=T))
    # proximity.timeline(prePostNet,default.dist = 6,
    #                    mode = 'sammon',labels.at = 17,vertex.cex = 4)
    
    #Fitting TERGM: edges is equivalent to an intercept in a GLM, triangle/gwesp is a transitivty effect (closure of triads)
    #nodematch is an assortativity effect of sex and nodecov is the continuous homophilic effect of rank/age
    
    #Interpretation of results
    #Positive parameters in FORMATION model = relationship is more likely than chance to FORM if X
    #Positive parameters in DISSOLUTION model = relationship is more likely than chance to PERSIST if X
    
    #IMPORTANT NOTE: difference between triangle, cyclicities, transitiveties, gwesp. GWESP is much easier to fit
    #and gets to a similar answer.
    
    #IMPORTANT NOTE: Difference between nodecov and absdiff?
    
    #Compute model using stergm. Use try as model convergence can be an issue
    t<- try (mod<-stergm(prePostNet,
                         formation = ~edges+gwesp(decay=0.1, fixed=T)+mutual+edgecov("prox"),
                         dissolution = ~edges+edgecov("prox"),
                         estimate="CMLE"))

    if("try-error" %in% class(t)){count=count+1} #keep track of errors, model which do not converge
    if(!("try-error" %in% class(t))){ #if don't converge
      
      print(summary(mod))
      #Note: nodefactor & edges are linearly dependent when both sexes are included.
      #There is an issue when trying to include gwesp in dissolution. Probably because none 
      #of the edge that dissolve were part of a triangle
      
      #Because we are running the model on multiple iterations of the data keeping track of the output
      TERGMeffects[1, c("iter", "groupyear")] = c(iter, groupyears[gy])
      TERGMeffects[1, c("form.edge", "form.triangle.close","form.reciprocity","form.prox")] <- unlist(coef(mod)["formation"])
      TERGMeffects[1, c("diss.edge","diss.prox")] <- unlist(coef(mod)["dissolution"])
      TERGMeffects.ALL = rbind(TERGMeffects.ALL,TERGMeffects)
      
    }
    rm(mod)
    # #Plot graph
    # #set up colours as an equivalent network attribute to sex
    # cols=vector();cols[sex=="M"]="cyan"; cols[sex=="F"]="pink"
    # set.vertex.attribute(net,"cols", cols)
    # 
    # #set degree as a vertex attribute for plotting purposes
    # set.vertex.attribute(net,"deg",colSums(b))
    
    # #plot the network using igraph. Node size is degree, node label is size, node colour is colour/sex
    # net2<-igraph::graph.adjacency(b,mode=network_mode)
    # dev.new()
    # par(mfrow=c(1,1))
    # igraph::plot.igraph(net2,vertex.color=cols,vertex.label.color="black",vertex.label.cex=0.8,vertex.size=5+colSums(b)^1.2,edge.color="dark grey",edge.width=1,edge.curved=0.2)
    # # plot(net2,vertex.color=cols,vertex.label.color="black",vertex.label.cex=0.8,vertex.size=5+colSums(b)^1.2,edge.color="dark grey",edge.width=1,edge.curved=0.2)
    
    # #run convergence tests on model (plots are automatic)
    # aa<-mcmc.diagnostics(mod2,vars.per.page=5,las=1)
    # 
    # #run goodness of fit tests on the model
    # a<-gof(mod2)
    # 
    # #plot goodness of fit tests
    # dev.new()
    # par(mfrow=c(3,1))
    # plot(a,las=1,cex.axis=1.1,cex.lab=1.5)
    # 

    #Save
    save(TERGMeffects.ALL, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/TERGMeffects_NoHom.RData")
  }
}

# }
