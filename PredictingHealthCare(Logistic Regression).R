# logistic regression
quality=read.csv("C:/Users/Zahid/Downloads/quality.csv")
str(quality)
install.packages("caTools")
library("caTools")
set.seed(88)
split=sample.split(quality$PoorCare, SplitRatio=0.75)
qualityTrain=subset(quality,split==TRUE)
qualityTest= subset(quality, split==FALSE)
nrow(qualityTrain)
QualityLog=glm(PoorCare~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)
predictTrain = predict(QualityLog, type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

QualityLog1=glm(PoorCare~ StartedOnCombination+ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog1)

table(qualityTrain$PoorCare,predictTrain>0.5)
10/25 #Sensitivity'
70/74
table(qualityTrain$PoorCare,predictTrain>0.7)
8/25
73/74
table(qualityTrain$PoorCare,predictTrain>0.2)
16/25
54/74
install.packages("ROCR")
library("ROCR")
ROCRpred=prediction(predictTrain, qualityTrain$PoorCare)
ROCRpred
ROCRperf= performance(ROCRpred, "tpr","fpr")
plot(ROCRperf, colorize=TRUE,print.cutoffs.at=seq(0,1,0.1, text.adj=c(-0.2,1.7)))
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
summary(predictTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc



