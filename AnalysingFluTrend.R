#Analysing Flu trend
rm(list=ls())
FluTrain=read.csv("C:/Users/Zahid/Downloads/FluTrain.csv")
FluTest=read.csv("C:/Users/Zahid/Downloads/FluTest.csv")
str(FluTrain)

which.max(FluTrain$ILI)
FluTrain$Week[303]
which.max(FluTrain$Queries)
hist(FluTrain$ILI,breaks=417)
plot(log(FluTrain$ILI), FluTrain$Queries)
?hist
FluTrend1=lm(log(ILI)~Queries,data=FluTrain)
summary(FluTrend1)
correlation=cor(log(FluTrain$ILI), FluTrain$Queries)
exp(-0.5*correlation)
log(1/correlation)
correlation^2
PredTest1=exp(predict(FlueTrend1,newdata=FluTest))
FluTest$PredictedValues=PredTest1
FluTest
SSE=sum((PredTest1-FluTest$ILI)^2)
SSE
sqrt(mean((PredTest1-FluTest$ILI)^2))
(2.2934216-2.187378)/2.2934216
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)
plot(log(FluTrain$ILI), log(FluTrain$ILILag2))
FluTrend2=lm(log(ILI)~Queries+log(ILILag2),data=FluTrain)
summary(FluTrend1)
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)
str(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
FluTest
summary(FluTest)
PredTest2=exp(predict(FluTrend2,newdata=FluTest))
SSE=sum((PredTest2-FluTest$ILI)^2)
SSE
SST=sum((FluTest$ILI-FluTrain$ILI)^2)
SST
1-SSE/SST
RMSE=sqrt(SSE/nrow(FluTest))
RMSE
nrow(FluTest)