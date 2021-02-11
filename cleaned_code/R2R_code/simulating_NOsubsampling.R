#Simulation to check the impact of sub-sampling on handling data sparsity issue

#################################################################
#COMPUTE FALSE NEGATIVE POSITIVE
#Assuming there are no true difference in probability pre- and post- (i.e. pgroom is the same)
#################################################################

n.obs= 500 #Total number of observations pre- and post- event
p.groom = c(0.01, 0.05, 0.1, 0.15, 0.20, 0.3) #Probability of grooming assumed pre and post (it is the same)
pre.data = seq(20,151,10) #Amount of data (i.e. assuming data sparsity)
n.boot = 500 #varying what the pre-data looks like (keeping it fixed)
n.boot2 = 500 #number of bootstrap iterations, circling through post-hurricane data

p=1; i=12; n=1; nb=1
diff.pgroom=array(NA,c(n.boot, n.boot2,length(pre.data),length(p.groom)))

for (p in 1:length(p.groom)){ #Looping through assumed p(groom)
  isGrooming = rep(0, n.obs)
  isGrooming[sample(1:n.obs, p.groom[p]*n.obs, replace=F)] = 1
  
  for (nb in 1:n.boot){
    
    for (i in 1:length(pre.data)){ #Looping through downsampled scan data (simulating the sparsity of pre-hurricane data)
      isGrooming.pre = isGrooming[sample(1:n.obs, pre.data[i], replace=F)]
      p.groom.pre = length(which(isGrooming.pre==1))/length(isGrooming.pre)
      
      for (n in 1:n.boot2){ #Bootstrapping, i.e. looping through the post-hurricane data which is assumed to be "full"
        isGrooming.post = isGrooming[sample(1:n.obs, n.obs, replace=T)]
        p.groom.post = length(which(isGrooming.post==1))/length(isGrooming.post)
        
        diff.pgroom[nb,n,i,p] = p.groom.post-p.groom.pre
        if(is.na(diff.pgroom[nb,n,i,p])){stop()}
      }
    }
  }
}

diff.detected=array(0,c(n.boot,length(pre.data),length(p.groom)))
i=14; p=1; nb=1

for (nb in 1:n.boot){
  for (p in 1:length(p.groom)){ #Looping through assumed p(groom)
    for (i in 1:length(pre.data)){ #Looping through downsampled scan data 
      CI=quantile(diff.pgroom[nb,,i,p],probs = c(0.025, 0.975), na.rm = T)
      if (prod(CI)>0){
        if (sum(CI)<0){
          diff.detected[nb,i,p] = -1
        } else {diff.detected[nb,i,p] = 1}
      }
    }
  }
}

i=1; p=1; nb=1; false.pos=array(NA,c(length(pre.data),length(p.groom)))
for (p in 1:length(p.groom)){
  for (i in 1:length(pre.data)){
    false.pos[i,p] = length(which(diff.detected[,i,p]!=0))/n.boot
  }
}

setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/Results")
tiff("false_positive_NoSub_sim.tiff") 
plot(NULL,type="l",ylim=c(0,1.1),xlim=c(20,150),xlab="sample size",ylab="p(false positive)",cex.lab=1.6)
lines(pre.data,false.pos[,1],col="blue", lwd=2)
lines(pre.data,false.pos[,2],col="darkgreen", lwd=2)
lines(pre.data,false.pos[,3],col="purple", lwd=2)
lines(pre.data,false.pos[,4],col="darkorange", lwd=2)
lines(pre.data,false.pos[,5],col="red", lwd=2)
lines(pre.data,false.pos[,6],col="yellow", lwd=2)
abline(h = 0.05, col="black",lty=2, lwd=2)
legend("topright",legend=c('p(groom)=0.01','p(groom)=0.05','p(groom)=0.10','p(groom)=0.15',
                           'p(groom)=0.20', 'p(groom)=0.30'), lwd=c(2,2,2,2,2,2), 
       col=c("blue","darkgreen","purple","darkorange","red","yellow"))
dev.off()
