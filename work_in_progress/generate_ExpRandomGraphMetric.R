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
allScans = read.csv("Behavioral_Data/Data All Cleaned/allScans2019.txt")
bigped <- read.delim("Behavioral_Data/SubjectInfo_2010-2017/PEDIGREE.txt", sep="\t")
setwd("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/")
load("R.Data/SocialCapital.RData")
SocialCapital.ALL$groupyear = paste(SocialCapital.ALL$group, SocialCapital.ALL$year,sep="")

#Compute pedigree for all IDs in this group
allIDs= allScans$focalID[which(allScans$group == "KK"|allScans$group == "V")];
groupIDs = as.character(unique(allIDs))
IDmatch = match(groupIDs, as.character(bigped$ID)); discard.na = which(is.na(IDmatch))
if(length(discard.na)!=0) 
{pedigree = bigped[IDmatch[-discard.na],c("ID","DAM","SIRE")]
} else {pedigree = bigped[IDmatch,c("ID","DAM","SIRE")]}
ped <- KinshipPedigree(pedigree)

actions = c("groom", "prox")
num_iter = 100

#For each group, each year separately: 
group = c("V","V","V","KK","KK") #c("V","V","V","V","V","KK","KK","KK","S")
years = c(2015, 2016,2017,2015, 2017)#c(2015,2016,2017,2018, 2019, 2015, 2017, 2018, 2019)
groupyears =c("V2015","V2016","V2017","KK2015","KK2017") #c("V2015","V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018", "S2019") 


ERGMeffects = data.frame(); ERGMeffects.ALL=data.frame()
start_time <- Sys.time(); iter=1; a=1; gy=1
for (a in 1:length(actions)) { #for both proximity and grooming 
  
  if (actions[a] == "prox") {network_mode = "undirected"; dir=F}
  if (actions[a] == "groom") {network_mode = "directed"; dir=T}
  
  for (iter in 1:num_iter){ #for all iterations
    
    print(paste("%%%%%%%%%%%%%%%%%%",paste(actions[a],iter), "%%%%%%%%%%%%%%%%%%"))
    
    # 1. Calculate random subsamples
    randomScans = calcRandomScans(allScans)
    randomScans$groupyear = paste(randomScans$group, randomScans$year,sep="")
    
    # 2. For each group, each year and pre-/post-hurr separately, compute weighted edge list: 
    for (gy in 1:length(groupyears)){ #For each group
      randscansY = randomScans[which(randomScans$groupyear==groupyears[gy]),] #subselect scans of group G
      
      isPost = c(0,1); h=1
      for (h in 1:length(isPost)){ #pre- and post-hurricane 
        
        rscans = randscansY[which(randscansY$isPost==isPost[h]),] #subselect scans of group G, year Y and hurricane status H
        numscans = as.data.frame(table(as.character(rscans$focalID))); names(numscans) =c("id","freq")
        
        #Find all unique IDs
        unqIDs = unique(as.character(rscans$focalID))
        
        # Output the Master Edgelist of all possible pairs given the unique IDs.
        if (actions[a] == "prox") {masterEL = calcMasterEL(unqIDs)}
        if (actions[a] == "groom") {masterEL = calcMasterEL_groom(unqIDs)}
        
        # Output weighted edgelist from the Master Edgelist.
        options(warn = -1) #set options to ignore all warnings
        if (actions[a] == "prox") {weightedEL = calcEdgeList(rscans,masterEL)}
        if (actions[a] == "groom") {weightedEL = calcEdgeList_groom(rscans,masterEL)
        weightedEL$alter = weightedEL$givingID; weightedEL$ego = weightedEL$receivingID
        #IMPORTANT NOTE: alter = giving ID and ego = receiving ego. Important for later.
        weightedEL$givingID <- NULL; weightedEL$receivingID <-NULL
        weightedEL = weightedEL[,c("alter","ego","count")]
        weightedEL$conc = paste(weightedEL$alter, weightedEL$ego, sep=".")
        }
        weightedEL$numscans <- (numscans$freq[match(weightedEL$alter, numscans$id)] + numscans$freq[match(weightedEL$ego, numscans$id)])/2 
        weightedEL$weight <- round(weightedEL$count / weightedEL$numscans, 5) #add weight information by dividing by avg #observations for each ID pair
        meanweight = mean(weightedEL$weight[weightedEL$weight>0]) #compute nonzero mean weight
        weightedEL$weight <- weightedEL$weight/meanweight
        # weightedEL = weightedEL[which(weightedEL$weight != 0),]#only keep nonzero weighted edges
        # weightedEL$iter = iter; weightedEL$group = group[g]; weightedEL$year = years[y]; weightedEL$isPost = isPost[h]; weightedEL$action = actions[a]
        
        #   #Save network
        #   Networks = rbind(Networks, weightedEL)# row bind
        #   
        # } #end of isPost for loop
        # data = Networks[which(Networks$group == group[g] & Networks$year == years[y]),]
        # data.0 = data[which(data$isPost == 0),]; data.1 = data[which(data$isPost == 1),] #create two structures containing pre-hurricane data (data.0) and post-hurricane data
        # 
        # #Find new partners
        # newP = data.1[which(is.na(match(data.1$conc, data.0$conc))),] #new pairs = pairs that were not in pre-hurricane network
        # 
        #Transform into matrix
        el= weightedEL[,c("alter","ego",'weight'),]; #newP[,c("alter","ego",'count'),]
        adjMat = dils::AdjacencyFromEdgelist(el)# create adjacency matrix based on edge list.
        data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
        
        #read in example network and create 
        # setwd("C:/Users/Camille Testard/Desktop/Analyzing_social_networks")
        # b<-read.csv("ergmtoynet.csv")
        # names<-b[,1]
        # b<-as.matrix(b[,2:ncol(b)])
        # rownames(b)<-colnames(b)<-names
        b<-data
        
        # #plot network using igraph
        # plot(igraph::graph.adjacency(b,mode=network_mode))
        # 
        #convert matrix into network package object
        net<-as.network.matrix(b,loops=FALSE,directed=dir)
        
        #set kinshp as edge attribute
        KC      <- NULL; for(i in 1:length(el[,1])){ 
          KC[i] <-  ped[which(rownames(ped)==as.character(el$ego[i])) , which(colnames(ped)==as.character(el$alter[i]))]
        }
        set.network.attribute(net, "kinship", KC)
        
        #set sex as vertex attribute
        sex = as.character(rscans$sex[match(as.character(net %v% "vertex.names"), as.character(rscans$focalID))])
        set.vertex.attribute(net,"sex", sex)
        
        #set age as vertex attribute
        age = as.numeric(rscans$age[match(as.character(net %v% "vertex.names"), as.character(rscans$focalID))])
        set.vertex.attribute(net,"age", age)
        
        #set rank as vertex attribute
        rank = as.numeric(rscans$percentrank[match(as.character(net %v% "vertex.names"), as.character(rscans$focalID))])
        if (length(which(is.na(rank)))!=0){rank[which(is.na(rank))]=runif(length(which(is.na(rank))), min=0, max=100)}
        set.vertex.attribute(net,"rank", rank)
        
        SocialCapital = SocialCapital.ALL[which(SocialCapital.ALL$groupyear==groupyears[gy]),]
        # Set PRE-HURRICANE standard Groom Strength as vertex attrbute
        groom = as.numeric(SocialCapital$std.DSIgroom[match(as.character(net %v% "vertex.names"), as.character(SocialCapital$id))])
        set.vertex.attribute(net,"groom", groom)
        
        # Set PRE-HURRICANE standard number of partners as vertex attrbute
        numP = as.numeric(SocialCapital$std.numPartnersGroom[match(as.character(net %v% "vertex.names"), as.character(SocialCapital$id))])
        set.vertex.attribute(net,"numP", numP)
        
        #fit ERGM: edges is equivalent to an intercept in a GLM, triangle is a transitivty effect
        #nodematch is an assortativity effect of sex/colour and nodecov is the continuous effect of size
        # #on the number of edges 
        # mod<-ergm(net~edges); summary(mod)
        # mod1<-ergm(net~edges+triangle); summary(mod1)
        # mod2<-ergm(net~edges+triangle+nodematch("sex",diff=TRUE)); summary(mod2)
        # mod3<-ergm(net~edges+triangle+nodematch("sex",diff=TRUE)+absdiff("age")); summary(mod3)
        # mod4<-ergm(net~edges+triangle+nodematch("sex",diff=TRUE)+absdiff("age")+absdiff("rank")); summary(mod4)#+edgecov("kinship")); 
        # mod5<-ergm(net~edges+triangle+nodematch("sex",diff=TRUE)+absdiff("age")+absdiff("rank")+absdiff("groom")); summary(mod5)
        summary(net~edges+ gwesp+ mutual +nodematch("sex",diff=TRUE)+absdiff("age")+absdiff("rank")+absdiff("groom")+absdiff("numP"))
        mod6<-ergm(net~edges+ #overall probability of bond forming
                     nodefactor("sex") + nodecov("age")+nodecov("rank")+ #"sociality" coefficients. How more likely is it that you 
                     #will form a bond given that you are from level "i" of category "X"
                     gwesp+ #Triad closure coefficients. Gives each additional shared partner a declining positive impact on the 
                     #probability of two nodes forming a tie.
                     nodematch("sex",diff=TRUE)+absdiff("age")+absdiff("rank")+absdiff("groom")+absdiff("numP"))# Homophily or selective mixing coefficients.
        summary(mod6)
        mod6<-ergm(net~edges+ nodecov("age")+nodecov("rank")+
                     gwesp+
                     nodematch("sex",diff=TRUE)+absdiff("age")+absdiff("rank")+absdiff("groom")+absdiff("numP")); summary(mod6)
        
        #Note: this model cannot handle NAs
        #Note 2: the model often fails to converge when including "triangle". Let it out for now.
        
        ERGMeffects[1, c("iter", "actions", "groupyear","ispost")] = c(iter, actions[a], groupyears[gy], isPost[h])
        ERGMeffects[1, c("edge", "homoph.sexF","homoph.sexM","homoph.age","homoph.rank","homoph.groom","homoph.numP")] <- coef(mod6)
        ERGMeffects.ALL = rbind(ERGMeffects.ALL,ERGMeffects)
        
        #Plot graph
        #set up colours as an equivalent network attribute to sex
        cols=vector();cols[sex=="M"]="cyan"; cols[sex=="F"]="pink"
        set.vertex.attribute(net,"cols", cols)

        #set degree as a vertex attribute for plotting purposes
        set.vertex.attribute(net,"deg",colSums(b))

        # #plot the network using igraph. Node size is degree, node label is size, node colour is colour/sex
        # net2<-igraph::graph.adjacency(b,mode=network_mode)
        # dev.new()
        # par(mfrow=c(1,1))
        # igraph::plot.igraph(net2,vertex.color=cols,vertex.label.color="black",vertex.label.cex=0.8,vertex.size=5+colSums(b)^1.2,edge.color="dark grey",edge.width=1,edge.curved=0.2)
        # # plot(net2,vertex.color=cols,vertex.label.color="black",vertex.label.cex=0.8,vertex.size=5+colSums(b)^1.2,edge.color="dark grey",edge.width=1,edge.curved=0.2)

        # #run convergence tests on model (plots are automatic)
        # aa<-mcmc.diagnostics(mod6,vars.per.page=5,las=1)
        # 
        # #run goodness of fit tests on the model
        # a<-gof(mod6)
        # 
        # #plot goodness of fit tests
        # dev.new()
        # par(mfrow=c(3,1))
        # plot(a,las=1,cex.axis=1.1,cex.lab=1.5)
        # 
        # #simulate 3 networks using the fitted model
        # #NB.as the simulations are stochastic, the output will not be identical to that presented in the paper
        # sim.nets<-simulate(mod6,3)
        # 
        # #plot these three simulated networks in separate windows
        # sim1<-igraph::graph.adjacency(as.matrix.network(sim.nets[[1]]),mode=network_mode)
        # dev.new()
        # par(mfrow=c(1,1))
        # plot(sim1,vertex.color=cols,vertex.label.color="black",vertex.label.cex=0.8,vertex.size=5+colSums(b)^1.2,edge.color="dark grey",edge.width=1,edge.curved=0.2)
        # sim2<-igraph::graph.adjacency(as.matrix.network(sim.nets[[2]]),mode=network_mode)
        # dev.new()
        # par(mfrow=c(1,1))
        # plot(sim2,vertex.color=cols,vertex.label.color="black",vertex.label.cex=0.8,vertex.size=5+colSums(b)^1.2,edge.color="dark grey",edge.width=1,edge.curved=0.2)
        # sim3<-igraph::graph.adjacency(as.matrix.network(sim.nets[[3]]),mode=network_mode)
        # dev.new()
        # par(mfrow=c(1,1))
        # plot(sim3,vertex.color=cols,vertex.label.color="black",vertex.label.cex=0.8,vertex.size=5+colSums(b)^1.2,edge.color="dark grey",edge.width=1,edge.curved=0.2)
        
        save(ERGMeffects.ALL, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/ERGMeffects.RData")
      }
    }
  }
}
