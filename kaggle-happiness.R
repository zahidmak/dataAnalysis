# Kaggle competition
rm(list=ls())
library(mice)
train=read.csv("c:/users/zahid/downloads/train.csv")
test=read.csv("c:/users/zahid/downloads/test.csv")
train=complete(mice(train))
test=complete(mice(test))
max(train$YOB)
train$Happy = as.factor(train$Happy)
train$YOB[train$YOB < 1930] = 0
train$YOB[train$YOB > 2004] = 0

str(train)
summary(train)
# There are NA's in data to we have to impute the data(replace NA with average value of that variable)
library(mice)

#test[test=='']=NA
imputedtrain=complete(mice(train))
imputedtest=complete(mice(test))
summary(imputedtrain)
#imputedtrain[imputedtrain == '']=NA
str(imputedtrain)
summary(imputedtrain)




library(rpart)
library(rpart.plot)
CART=rpart(Happy~.-UserID,data=train)
prp(CART)
pred=predict(CART)
str(pred)
str(test)
table(train$Happy, pred[,2])
predtest= predict(CART, newdata=test)
str(predtest)
output=cbind(test$UserID,predtest[,2])
str(output)
View(output)
write.csv(output, "C:/users/zahid/downloads/output.csv")

#Random Forest
library(randomForest)
rmTrain = randomForest(Happy~. - UserID, data =train,ntree=5000)
summary(rmTrain)
rmTrain$predicted
rmPredict = predict(rmTrain,type='prob')
table(train$Happy, rmPredict[,2]>=0.5)
?randomForest
predtest=predict(rmTrain,newdata=test,type="prob")
str(predtest)
output=cbind(test$UserID,predtest[,2])
str(output)
View(output)
varImpPlot( rmTrain, n.var = 40, main = "Importance of variables" )
write.csv(output, "C:/users/zahid/downloads/afiz2.csv")

#Random forest with significant variable
rmTrain = randomForest(Happy~ EducationLevel+Income+YOB+Q118237+Party+votes+Q101162+HouseholdStatus+Q107869+Q119334+Q102289+Q102906+Q120014+Q98869+Q106997+Q121011+Q108855+Q124742+Q108343+Q117186+Q124122+Q108342+Q115610+Q116953+Q113584+Q114961+Q111848+Q123621+Q120012+Q115390+Q116448+Q108856+Q118892+Q113181+Q98197+Q117193+Q116881+Q118232+Q122769+Q120194 - UserID, data =train, ntree=5000)
summary(rmTrain)
rmTrain$predicted
rmPredict = predict(rmTrain,type='prob')
table(train$Happy, rmPredict[,2]>=0.5)
?randomForest
predtest=predict(rmTrain,newdata=test,type="prob")
str(predtest)
output=cbind(test$UserID,predtest[,2])
str(output)
View(output)
varImpPlot( rmTrain, n.var = 40, main = "Importance of variables" )
write.csv(output, "C:/users/zahid/downloads/afiz.csv")





#CART with complexity parameter
library(caret)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
fitControl= trainControl(method="cv", number=10)
train(Happy ~ . -UserID , data = train, method="rpart", trControl=fitControl, tuneGrid=cartGrid)
CART=rpart(Happy~.-UserID, data=train, control=rpart.control(cp=0.008))
pred=predict(CART)
str(pred)
table(train$Happy, pred[,2])
predtest=predict(CART,newdata=test)
output=cbind(test$UserID,predtest[,2])
str(output)
View(output)
write.csv(output, "C:/users/zahid/downloads/output1.csv")


#Clustering
temp=agnes(train)
?agnes
summary(temp)
train[train == 'Yes']=2
str(train)
str(train)
summary(train)
trainCluster=train
str(trainCluster)
distance=dist(train,method="euclidean")
str(dist)
cluster=hclust(distance, method="ward")
plot(cluster)
kmc=kmeans(train, centers=7)
table(KMC$cluster)
table(clusterGroups)
KMC$centers

install.packages("multicore")



