# Analysing students reading ability
train= read.csv("C:/Users/Zahid/Downloads/pisa2009train.csv")
test= read.csv("C:/Users/Zahid/Downloads/pisa2009test.csv")
str(train)
?tapply
tapply(train$readingScore, train$male, mean)
summary(train)

#removing missing data i.e NA's
train=na.omit(train)
test=na.omit(test)
train$raceeth=relevel(train$raceeth,"White")
test$raceeth=relevel(test$raceeth,"White")
str(train)
lmScore=lm(readingScore~.,data=train)
summary(lmScore)
prediction=predict(lmScore,newdata=train)
prediction
SSE=sum((prediction-train$readingScore)^2)
SSE
RMSE=sqrt((SSE)/nrow(train))
RMSE
model1=step(lm(readingScore~.,data=train))
summary(model1)
prediction1=predict(lmScore,newdata=test)
summary(prediction1)
diff=max(prediction1)-min(prediction1)
diff
SSE=sum((prediction1-mean(test$readingScore))^2)
SSE
RMSE=sqrt(SSE/nrow(test))
RMSE
mean(train$readingScore-mean(train$readingScore))
mean(train$readingScore)
SSE=sum((test$readingScore-mean(test$readingScore))^2)
SSE
SST=sum((mean(train$readingScore)-test$readingScore)^2)
SST
1-SSE/SST
lmScore1=lm(readingScore~.,data=test)
summary(lmScore1)