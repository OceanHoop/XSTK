data = read.csv("D:/Project/Dataset1.csv")

print(colnames(data))

colnames(data)[1] <- "CEMENT"
colnames(data)[2] <- "Blast.furance.Slag"
colnames(data)[3] <- "Fly.ash"
colnames(data)[4] <- "Water"
colnames(data)[5] <- "Super.plasticizer"
colnames(data)[6] <- "Coarse.Aggregate"
colnames(data)[7] <- "Fine.Agrregate"
colnames(data)[8] <- "Age.of.testing.day"
colnames(data)[9] <- "Concrete.compressive.strength"
str(data)

 anyNA(data)

dataclean = data

summary(dataclean)

library(tidyr)
library(ggplot2)

ggplot(gather(dataclean),aes(value))+geom_histogram(bins=10,col="blue")+facet_wrap(~key,scales='free')

ggplot(gather(dataclean),aes(,value))+geom_boxplot()+facet_wrap(~key,scales="free",ncol=4)

pairs(dataclean)

Blast.furance.Slag <-  which(dataclean$Blast.furance.Slag  %in% boxplot(dataclean$Blast.furance.Slag , plot=FALSE)$out)
Fly.ash <- which(dataclean$Fly.ash %in% boxplot(dataclean$Fly.ash, plot=FALSE)$out)
Water <- which(dataclean$Water %in% boxplot(dataclean$Water, plot=FALSE)$out)
Super.plasticizer <- which(dataclean$Super.plasticizer  %in% boxplot(dataclean$Super.plasticizer , plot=FALSE)$out)
Coarse.Aggregate  <-  which(dataclean$Coarse.Aggregate   %in% boxplot(dataclean$Coarse.Aggregate  , plot=FALSE)$out)
Fine.Agrregate  <-  which(dataclean$Fine.Agrregate    %in% boxplot(dataclean$Fine.Agrregate   , plot=FALSE)$out)
Age.of.testing.day <-  which(dataclean$Age.of.testing.day    %in% boxplot(dataclean$Age.of.testing.day  , plot=FALSE)$out)

dataclean2 <- dataclean[-c(Blast.furance.Slag,Fly.ash,Water,Super.plasticizer,Coarse.Aggregate,Fine.Agrregate,Age.of.testing.day),]
nrow(dataclean2)


ggplot(gather(dataclean2),aes(,value))+geom_boxplot()+facet_wrap(~key,scales="free",ncol=4)


M <- cor(dataclean2, method = 'spearman')
corrplot(M,method = 'number')




