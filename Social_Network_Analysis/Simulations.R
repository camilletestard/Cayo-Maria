#SIMULATIONS

library(igraph)
library(tnet)

#1. Does increasing density leads to a decrease in gini coeff.
## Generate a random graph using igraph:
gini_coeff = as.numeric(); gini_coeff_deg = as.numeric(); gini_coeff_deg_standard = as.numeric(); clust = as.numeric() 
density = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
for (p in 1:length(density)){
  rg = sample_gnp(100, density[p], directed = T, loops = FALSE)
  eigcent = eigen_centrality(rg, directed = T)
  gini_coeff[p] = ineq::ineq(as.numeric(eigcent[["vector"]]), "gini")
  gini_coeff_deg[p] = ineq::ineq(as.numeric(degree(rg)), "gini")
  gini_coeff_deg_standard[p] = ineq::ineq(as.numeric(degree(rg)), "gini")/density
  clust[p] <- as.numeric(transitivity(rg))
}
plot(density,gini_coeff)
plot(density,gini_coeff_deg)
plot(density,gini_coeff_deg_standard)
plot(density,clust)

#2. Does increasing #nodes leads to a decrease in gini coeff.
## Generate a random graph using igraph:
gini_coeff = as.numeric(); gini_coeff_deg = as.numeric(); clust = as.numeric() 
nodes = c(10,20,30,40,50,60,70,80,90,100)
for (p in 1:length(nodes)){
  rg = sample_gnp(nodes[p], 0.5, directed = T, loops = FALSE)
  eigcent = eigen_centrality(rg, directed = T)
  gini_coeff[p] = ineq::ineq(as.numeric(eigcent[["vector"]])  , "gini")
  gini_coeff_deg[p] = ineq::ineq(as.numeric(degree(rg)), "gini")
  clust[p] <- as.numeric(transitivity(rg))
}
plot(nodes,gini_coeff)
plot(nodes,gini_coeff_deg)
plot(nodes,clust)