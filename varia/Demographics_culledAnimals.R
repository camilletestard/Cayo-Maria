########################################
# Demographics of the culled animals.
setwd("C:/Users/Camille Testard/Desktop/Desktop-Cayo-Maria/") 
culledID = read.csv("Behavioral_Data/Cayo Biobank Tissue Catalog.csv"); names(culledID)[1]="id"
data = culledID[which(culledID$group == "HH" | culledID$group == "KK"),]
hist(data$age, breaks = 20)

length(which(data$age>=6 & data$group =="HH")); length(which(data$age>=6 & data$group =="KK"))
length(which(data$age>=6 & data$group =="HH" & data$sex=="F")); length(which(data$age>=6 & data$group =="HH" & data$sex=="M"))
length(which(data$age>=6 & data$group =="KK" & data$sex=="F")); length(which(data$age>=6 & data$group =="KK" & data$sex=="M"))

min(data$age);max(data$age)

ggplot(data, aes(x=age, colour=sex))+
  geom_histogram(fill="white")+
  facet_grid(rows=vars(sex), cols = vars(group))+
  xlim(4,20)+
  ggtitle("Age distribution")

########################################
#Power analysis
power=vector(mode="numeric", length =10)
iter = 1
for (i in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)){
  p.out <- pwr.t2n.test(n1=69,n2=100,d=i,sig.level = 0.05)
  power[iter] = p.out$power
  iter = iter +1
}
plot(power)
