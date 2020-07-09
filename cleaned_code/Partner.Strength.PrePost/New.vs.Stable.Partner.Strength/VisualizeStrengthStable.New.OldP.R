#Visualize strength to stable/new/old partners. how has the composition of the social environment changed 
#pre-topost hurricane?

#Load data
load("C:/Users/Camille Testard/Documents/GitHub/Cayo-Maria/R.Data/Strength.Stable.New.Old.Partners.RData")

##########################################################
#Compare strength to stable partners pre- to post- hurricane
#Separate data by groom/prox
data.stableP.groom = ID.strength.stableP.All[which(ID.strength.stableP.All$action == "groom"),]
data.stableP.prox = ID.strength.stableP.All[which(ID.strength.stableP.All$action == "prox"),]

#GROOM

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results/New.vs.Stable.Partner.Strength") #set saving directory

#Groom give + Groom get, for all iterations
tiff("GroomStrength.StableP.tiff", units="in", width=7, height=6, res=300, compression = 'lzw')
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 0)]), col=rgb(1,0,0,0.5), breaks=20, ylim = c(0, 10000), 
     main = "Grooming strength to existing partners pre-/post- hurricane", xlab = "Grooming Strength")
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 1)]), col=rgb(0,1,1,0.5), breaks=20, add = T)
box()
legend("topright", c("pre", "post"), fill=c("red", "cyan"))

#For group V only
tiff("GroomStrength.StableP.V.tiff", units="in", width=7, height=6, res=300, compression = 'lzw')
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 0 & data.stableP.groom$group=="V")]), col=rgb(1,0,0,0.5), breaks=20, ylim = c(0, 10000), 
     main = "Grooming strength to existing partners pre-/post- hurricane - Group V", xlab = "Grooming Strength")
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 1& data.stableP.groom$group=="V")]), col=rgb(0,1,1,0.5), breaks=20, add = T)
box()
legend("topright", c("pre", "post"), fill=c("red", "cyan")); dev.off()

#For group KK only
tiff("GroomStrength.StableP.KK.tiff", units="in", width=7, height=6, res=300, compression = 'lzw')
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 0 & data.stableP.groom$group=="KK")]), col=rgb(1,0,0,0.5), breaks=20, ylim = c(0, 10000), 
     main = "Grooming strength to existing partners pre-/post- hurricane - Group KK", xlab = "Grooming Strength")
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 1& data.stableP.groom$group=="KK")]), col=rgb(0,1,1,0.5), breaks=20, add = T)
box()
legend("topright", c("pre", "post"), fill=c("red", "cyan")); dev.off()

# #For 1 iteration
# hist(data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 0 & data.stableP.groom$iter==1)], col=rgb(1,0,0,0.5), breaks=10, ylim = c(0, 20),
#      main = "Grooming strength to existing partners pre-/post- hurricane", xlab = "log(Grooming Strength)")
# hist(data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 1 & data.stableP.groom$iter==1)], col=rgb(0,1,1,0.5), breaks=10, add = T)
dev.off()

# #For groom give, for all iterations
# tiff("GroomGiveStrength.StableP.tiff", units="in", width=7, height=6, res=300, compression = 'lzw')
# hist((data.stableP.groom$strength.give[which(data.stableP.groom$isPost == 0)]), col=rgb(1,0,0,0.5), breaks=20, ylim = c(0, 200),
#      main = "Grooming Give strength to existing partners pre-/post- hurricane", xlab = "Grooming Strength")
# hist((data.stableP.groom$strength.give[which(data.stableP.groom$isPost == 1)]), col=rgb(0,1,1,0.5), breaks=40, add = T)
# box()
# legend("topright", c("pre", "post"), fill=c("red", "cyan"))
# dev.off()
# 
# #For groom get, for all iterations
# tiff("GroomGetStrength.StableP.tiff", units="in", width=7, height=6, res=300, compression = 'lzw')
# hist((data.stableP.groom$strength.get[which(data.stableP.groom$isPost == 0)]), col=rgb(1,0,0,0.5), breaks=30, ylim = c(0, 200),
#      main = "Grooming Get strength to existing partners pre-/post- hurricane", xlab = "log(Grooming Strength)")
# hist((data.stableP.groom$strength.get[which(data.stableP.groom$isPost == 1)]), col=rgb(0,1,1,0.5), breaks=30, add = T)
# box()
# legend("topright", c("pre", "post"), fill=c("red", "cyan"))
# dev.off()

#PROXIMITY
tiff("ProxStrength.StableP.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
hist(log(data.stableP.prox$strength.all[which(data.stableP.prox$isPost == 0)]), col=rgb(1,0,0,0.5), breaks=20, ylim = c(0, 6000), xlim=c(-5,1), 
     main = "Proximity strength to existing partners pre-/post- hurricane", xlab = "log(Proximity Rate)")
hist(log(data.stableP.prox$strength.all[which(data.stableP.prox$isPost == 1)]), col=rgb(0,1,1,0.5), breaks=40, add = T)
box()
legend("topright", c("pre", "post"), fill=c("red", "cyan"))
dev.off()

##########################################################
#Visualize strength to new partners
data.newP.groom = ID.strength.newP.All[which(ID.strength.newP.All$action == "groom"),]
data.newP.prox = ID.strength.newP.All[which(ID.strength.newP.All$action == "prox"),]

tiff("GroomStrength.NewP.tiff", units="in", width=7, height=6, res=300, compression = 'lzw')
hist((data.newP.groom$strength.all), col=rgb(0.25,0.75,0.25,0.5), breaks=50, ylim = c(0, 200), 
     main = "Grooming strength to new partners vs stable partners post-hurricane", xlab = "Grooming Strength")
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 1)]), col=rgb(1,0.5,0,0.7), breaks=30, add = T)
box()
legend("topright", c("New partners (post)", "Stable partners (post)"), fill=c("limegreen", "orange"))
dev.off()

tiff("ProxStrength.NewP.tiff",units="in", width=7, height=6, res=300, compression = 'lzw')
hist(log(data.newP.prox$strength.all), col=rgb(0.25,0.75,0.25,0.5), breaks=30, ylim = c(0, 500), 
     main = "Proximity strength to new partners vs stable partners post-hurricane", xlab = "log(Proximity Rate)")
hist(log(data.stableP.prox$strength.all[which(data.stableP.prox$isPost == 1)]), col=rgb(1,0.5,0,0.7), breaks=30, add = T)
box()
legend("topright", c("New partners (post)", "Stable partners (post)"), fill=c("limegreen", "orange"))
dev.off()

##########################################################
#Visualize strength to old partners
data.oldP.groom = ID.strength.oldP.All[which(ID.strength.oldP.All$action == "groom"),]
data.oldP.prox = ID.strength.oldP.All[which(ID.strength.oldP.All$action == "prox"),]

tiff("GroomStrength.oldP.tiff", units="in", width=7, height=6, res=300, compression = 'lzw')
hist((data.oldP.groom$strength.all), col=rgb(0.6,0.2,0.95,0.5), breaks=20, ylim = c(0, 200), 
     main = "Grooming strength to old partners vs stable partners pre-hurricane", xlab = "Grooming Strength")
hist((data.stableP.groom$strength.all[which(data.stableP.groom$isPost == 0)]), col=rgb(0.5,0.5,0.6,0.8), breaks=20, add = T)
box()
legend("topright", c("Old partners (pre)", "Stable partners (pre)"), fill=c("blueviolet", "grey"))
dev.off()

tiff("ProxStrength.OldP.tiff", 
     units="in", width=7, height=6, res=300, compression = 'lzw')
hist(log(data.oldP.prox$strength.all), col=rgb(0.6,0.2,0.95,0.5), breaks=20, ylim = c(0, 200),  
     main = "Proximity strength to old partners vs stable partners pre-hurricane", xlab = "log(Proximity Strength)")
hist(log(data.stableP.prox$strength.all[which(data.stableP.prox$isPost == 0)]), col=rgb(0.5,0.5,0.6,0.8), breaks=20, add = T)
box()
legend("topright", c("Old partners (pre)", "Stable partners (pre)"), fill=c("blueviolet", "grey"))
dev.off()

##########################################################
#Compare the number of stable/old/new pairs
data.groom = population.strength.All[which(population.strength.All$action == "groom"),]
data.prox = population.strength.All[which(population.strength.All$action == "prox"),]

hist(data.groom$numStablePairs, breaks=10, col=rgb(0,0,1,0.5), xlim =c(0, 120))
hist(data.groom$numOldPairs, breaks=20, col=rgb(1,0,0,0.5), add=T)
hist(data.groom$numNewPairs, breaks=30, col=rgb(0,1,0,0.5), add=T)
