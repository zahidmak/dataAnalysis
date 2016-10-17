# Predicting if criminal will make crime on parloe time
rm(list=ls())
parole= read.csv("C:/Users/Zahid/Downloads/parole.csv")
str(parole)
table(parole$violator)
malef=factor(parole$male)
malef
levels(malef)
is.ordered(malef)
statef=factor(parole$state)
statef
parole$crime= as.factor(parole$crime)
str(gg)
is.ordered(statef)
agef=factor(parole$age)
agef
is.ordered(agef)
timef=factor(parole$time.served)
timef
is.ordered(timef)
maxf=factor(parole$max.sentence)
is.ordered(maxf)
mulf=factor(parole$multiple.offenses)
is.ordered(mulf)
crimef=factor(parole$crime)
is.ordered(crimef)

crimef=as.factor(parole$crime)
summary(crimef)
table(parole$crime)
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
str(train)
str(test)
split = sample.split(parole$violator, SplitRatio = 0.7)
train1 = subset(parole, split == TRUE)
test1 = subset(parole, split == FALSE)

table(parole$state)
table(parole$crime)
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
str(train)
str(test)
model1=glm(violator~.,data=train, family=binomial)
summary(model1)
pred1= predict(model1, type="response", newdata=test)
table(test$violator, pred1>=0.4)
table(parole$violator)
library(ROCR)
predROCR=prediction(pred1, test$violator)
str(predROCR)
ROCRperf=performance(predROCR,"tpr","fpr")
attributes(performance(predROCR, 'auc'))$y.values[[1]]
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1))
str(ROCRperf)
install.packages("AUC")
auc(ROCRperf, min=0, max=1)
?auc
library(AUC)
auc(roc(pred1, test$violator))
summary(model1)







