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
num_iter = 50

#For each group, each year separately: 
group = c("V","V","V","KK","KK") #c("V","V","V","V","V","KK","KK","KK","S")
years = c(2015, 2016,2017,2015, 2017)#c(2015,2016,2017,2018, 2019, 2015, 2017, 2018, 2019)
groupyears =c("V2015","V2016","V2017","KK2015","KK2017") #c("V2015","V2016","V2017","V2018","V2019","KK2015","KK2017","KK2018", "S2019") 


TERGMeffects = data.frame(); TERGMeffects.ALL=data.frame(); count=0
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
        # weightedEL$iter = iter; weightedEL$group = group[gy]; weightedEL$year = years[gy];  weightedEL$isPost = isPost[h]; #weightedEL$action = actions[a]
        
        #   #Save network
        #   Networks = rbind(Networks, weightedEL)# row bind
        #   
        # } #end of isPost for loop
        # # data = Networks[which(Networks$group == group[gy] & Networks$year == years[gy]),]
        # data.0 = Networks[which(Networks$isPost == 0),]; data.1 = Networks[which(Networks$isPost == 1),] #create two structures containing pre-hurricane data (data.0) and post-hurricane data
        
        el = weightedEL[,c("alter","ego",'weight'),]; #newP[,c("alter","ego",'count'),]
        adjMat = dils::AdjacencyFromEdgelist(el)# create adjacency matrix based on edge list.
        mat = adjMat[["adjacency"]]; rownames(mat) = adjMat[["nodelist"]]; colnames(mat) = adjMat[["nodelist"]]
        net<-as.network.matrix(mat,loops=FALSE,directed=dir)
        
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
        
        if(isPost[h]==0){net.pre=net}
        if(isPost[h]==1){net.post=net}
        
      }
      
      prePostList = list(net.pre, net.post)
      prePostNet <- networkDynamic(network.list=prePostList)
      
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
      # 
      #fit TERGM: edges is equivalent to an intercept in a GLM, triangle/gwesp is a transitivty effect
      #nodematch is an assortativity effect of sex and nodecov is the continuous effect of rank/age
      
      #IMPORTANT NOTE: Interpretation of results
      #Positive parameters in FORMATION model = relationship is more likely than chance to FORM if X
      #Positive parameters in DISSOLUTION model = relationship is more likely than chance to PERSIST if X
      
      #IMPORTANT NOTE: difference between triangle, cyclicities, transitiveties, gwesp. GWESP is much easier to fit
      #and gets to a similar answer.
      
      t<- try (mod<-stergm(prePostNet,
                           formation = ~edges+gwesp(decay=0.1, fixed=T)+mutual+
                             # nodecov("age")+nodecov("rank")+nodecov("groom")+nodecov("numP")+
                             nodematch("sex",diff=TRUE)+absdiff("age")+absdiff("rank")+absdiff("groom")+absdiff("numP"),
                           dissolution = ~edges+
                             # gwesp(decay=0.1, fixed=T)+mutual+
                             # nodecov("age")+nodecov("rank")+nodecov("groom")+nodecov("numP")+nodefactor("sex", base=1),
                             nodematch("sex",diff=TRUE)+absdiff("age")+absdiff("rank")+absdiff("groom")+absdiff("numP"),
                           estimate="CMLE"))
      # constraints= ~bd(maxout = 7))
      
      if("try-error" %in% class(t)){count=count+1}
      if(!("try-error" %in% class(t))){

        print(summary(mod))
        #Note: nodefactor & edges are linearly dependent when both sexes are included.
        #2. There is an issue when trying to include gwesp in dissolution. Probably because none 
        #of the edge that dissolve were part of a triangle
        
        
        TERGMeffects[1, c("iter", "actions", "groupyear")] = c(iter, actions[a], groupyears[gy])
        TERGMeffects[1, c("form.edge", "form.triangle.close","form.reciprocity","form.homoph.sexF","form.homoph.sexM",
                          "form.homoph.age","form.homoph.rank","form.homoph.groom","form.homoph.numP")] <- unlist(coef(mod)["formation"])
        TERGMeffects[1, c("diss.edge","diss.homoph.sexF","diss.homoph.sexM",
                          "diss.homoph.age","diss.homoph.rank","diss.homoph.groom","diss.homoph.numP")] <- unlist(coef(mod)["dissolution"])
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
      
      save(TERGMeffects.ALL, file ="C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/TERGMeffects.RData")
    }
  }
}
# }
