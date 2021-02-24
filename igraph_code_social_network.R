#Visualize social network graph
# This script aims at visualizing social networks using igraph

# load libraries
library(dplyr)
library(sna)
library(igraph)
library(tnet)
library(ineq)
library(stringr)
library(ggplot2)


        #Need to upload an adjacency matrix to social network graph
        adjMat = dils::AdjacencyFromEdgelist(weightedEL)# create adjacency matrix based on edge list.
        data = adjMat[["adjacency"]]; rownames(data) = adjMat[["nodelist"]]; colnames(data) = adjMat[["nodelist"]]
        
        #read adjacency matrix
        m=as.matrix(data) # coerces the data set as a matrix
        am.g=graph.adjacency(m,mode= network_mode,weighted=T) # this will create an directed 'igraph object'. Change qualifiers to make "undirected" or unweighted (null)
        am.g #igraph object properties The letters on the first line (there can be up to 4) indicates some basic information about the graph. 
        #The first letter indicates whether this is a directed ('D') or undirected ('U') graph. 
        #The 2nd letter tells you if  this is a named ('N') graph--i.e., whether or not the vertex set has a 'name' attribute. 
        #The 3rd letter tells you if this graph is weighted ('W'). 
        #The fourth letter is 'B' for bipartite graphs. These letter codes are followed by two numbers: the first is the number of vertices and the second is the number of edges.
        
        #increase space between nodes if overlapping. Choose graph layout.
        if (isPost[h] ==0) {l <- layout.fruchterman.reingold(am.g, repulserad=vcount(am.g)^5,
                                                             area=vcount(am.g)^3)
        graph.dens.pre[gy] = round(edge_density(am.g),3);
        } else {graph.dens.post[gy] = round(edge_density(am.g),3)}
        #layout.spring(am.g,niter=500,area=vcount(am.g)^2.3,repulserad=vcount(am.g)^2.8)} #layout_in_circle}#
        #Node: I added the "if isPost =0" clause to make sure the node position is the same when comparing pre- and post graphs.
        #changes size of labels of vertices
        V(am.g)$label.cex <- 0.8
        
        #link up attributes file with network
        V(am.g)$sex=as.factor(dominance_info$SEX[match(V(am.g)$name,dominance_info$ID)]) #sex attribute
        # V(am.g)$agec=as.factor(att$agec[match(V(am.g)$name,attr$id)]) #age attribute
        # 
        # #set size of nodes by animal age
        # V(am.g)$size=V(am.g)$agec*6 #multiplied by 5 because nodes too small otherwise
        
        #set colour of sexes
        V(am.g)$color=V(am.g)$sex #assign the "Sex" attribute as the vertex color
        V(am.g)$color=gsub("1","plum1",V(am.g)$color) #Females will be orange
        V(am.g)$color=gsub("2","seagreen2",V(am.g)$color) #Males will be lightblue
        V(am.g)$color=gsub("0","white",V(am.g)$color) #unknown sex will be white
        
        #Set color of vertex labels
        color_vector=V(am.g)$sex #assign the "Sex" attribute as the vertex color
        color_vector=gsub("1","purple",color_vector) #Females will be orange
        color_vector=gsub("2","darkblue",color_vector) #Males will be lightblue
        color_vector=gsub("0","grey",color_vector) #unknown sex will be white
        
        #set degree attribute
        V(am.g)$degree=igraph::degree(am.g)
        
        #set path for saving and plot graph
        # if (network_action == "groom"){setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/GroomNetworks/Sampling_R2R/test_v2")
        #   if (isPost[h]==0){title = paste("Grooming Network ",group[g],years[y]," pre-hurricane, mean (SD) scans: ", mean_numScans_list[[g]][y],"(",sd_numScans_list[[g]][y],"); density:", graph.dens.pre[gy],sep="")
        #   } else {title = paste("Grooming Network ",group[g],years[y]," post-hurricane"," pre-hurricane, mean (SD) scans: ", mean_numScans_list[[g]][y]," (",sd_numScans_list[[g]][y],"); density:", graph.dens.post[gy],sep="")}}
        # 
        if (network_action == "groom"){setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/GroomNetworks/Sampling_R2R/test_v2")
          if (isPost[h]==0){title = paste("Grooming Network ",group[g],years[y]," pre-hurricane, mean (SD) scans: ", mean_num_scans,"(",sd_num_scans,"); density:", graph.dens.pre[gy],sep="")
          } else {title = paste("Grooming Network ",group[g],years[y]," post-hurricane"," pre-hurricane, mean (SD) scans: ", mean_num_scans," (",sd_num_scans,"); density:", graph.dens.post[gy],sep="")}}
        
        if (network_action == "prox"){setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/SocialNetworkGraph/ProxNetworks")
          if (isPost[h]==0){title = paste("Proximity Network ",group[g],years[y]," pre-hurricane, mean (SD) scans: ", mean_numScans_list[[g]][y]," (",sd_numScans_list[[g]][y],"); density:", graph.dens.pre[gy],sep="")
          } else {title = paste("Proximity Network ",group[g],years[y]," post-hurricane"," pre-hurricane, mean (SD) scans: ", mean_numScans_list[[g]][y]," (",sd_numScans_list[[g]][y],"); density:", graph.dens.post[gy],sep="")}}
        
        tiff(paste("Social Network ",group[g],years[y],".",isPost[h],".tiff",sep=""), 
             units="in", width=10, height=8, res=300, compression = 'lzw')
        plot.igraph(am.g, layout=l, vertex.label=V(am.g)$name, vertex.color=V(am.g)$color, vertex.label.color=color_vector, vertex.label.font=2,
                    vertex.size=2*V(am.g)$degree+2,edge.color="grey20", 
                    edge.width=E(am.g)$weight*1.5,edge.arrow.size = 0.5,edge.curved=0.5,
                    main = title)
        dev.off()
        
        tiff(paste("NumScans vs Degree ",group[g],years[y],".",isPost[h],".tiff",sep=""), 
             units="in", width=10, height=8, res=300, compression = 'lzw')
        plot(V(am.g)$degree, numscans$freq, cex=2, pch = 20, ylab='#scans',xlab='degree', 
             col=V(am.g)$color, xlim=c(-1,15), ylim=c(1,120), cex.lab=1.5, cex.axis=1.25,main=paste(group[g],years[y]))
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
        
        
        
      }
    }
  }  
}

tiff(paste("NumScans_vs_density.tiff",sep=""), 
     units="in", width=10, height=8, res=300, compression = 'lzw')
plot(graph.dens.pre, mean_numScans, ylim=c(50,130), xlim=c(0,0.05), cex=2, pch = 20,col=c('red','red','red','blue','blue'),
     ylab='mean #scans',xlab='network density', cex.lab=1.5, cex.axis=1.25)
text(graph.dens.pre, mean_numScans, c("2015","2016","2017","2015","2017"),
     cex=1, pos=3,col="black") 
legend("bottomleft", inset=.02, title="Group",
       c("V","KK"), fill=c("red","blue"), horiz=TRUE, cex=0.8)
dev.off()