rm(list=ls())
airlines=read.csv("C:/users/zahid/downloads/AirlinesCluster.csv")
str(airlines)
summary(airlines)
library(caret)
preproc = preProcess(airlines) #centers and scale the variables
preproc
airlinesNorm = predict(preproc, airlines) #normalization
summary(airlinesNorm)
distance=dist(airlinesNorm,method="euclidean")
str(dist)
cluster=hclust(distance, method="ward")
plot(cluster)
rect.hclust(cluster, k=7, border="red")
clusterGroups=cutree(cluster, k=5)
table(clusterGroups)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
set.seed(88)
KMC=kmeans(airlines,centers=5,iter.max=1000)
table(KMC$cluster)
table(clusterGroups)
KMC$centers
