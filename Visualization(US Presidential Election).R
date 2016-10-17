rm(list=ls())
library(ggplot2)
library(ggmap)
statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x=long, y=lat, group=group))+geom_polygon(fill="white", color="blue")+coord_map('mercator')
?mapproject 
polling=read.csv("C:/users/zahid/downloads/PollingImputed.csv")
Train=subset(polling, Year==2004 | Year==2008)
Test=subset(polling, Year ==2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(TestPredictionBinary)
table(Test$Republican, TestPrediction>0.5)
mean(TestPrediction)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),] #so map will be drawn properly
str(predictionMap)
str(statesMap)
?merge
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
table(Test$State=="florida", TestPrediction)
str(TestPrediction)
str(Test)
temp=cbind(Test$State, TestPrediction)
Test$Republican[6]
?geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
?alpha

