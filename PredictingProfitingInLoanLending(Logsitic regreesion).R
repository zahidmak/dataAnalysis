# predicting if to provide loan
rm(list=ls())
loan=read.csv("C:/Users/Zahid/Downloads/loans(1).csv")
str(loan)
summary(loan)
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loan), "not.fully.paid")
vars.for.imputation
imputed = complete(mice(loan[vars.for.imputation]))
imputed
loan[vars.for.imputation] = imputed
loans=loans_imputed
loans_imputed=read.csv("C:/Users/Zahid/Downloads/loans_imputed.csv")
nrow(loans$not.fully.paid!=loans_imputed$not.fully.paid)
set.seed(144)
?split
library(caTools)
split=sample.split(loans$not.fully.paid, SplitRatio=0.7)
train=subset(loans, split==TRUE)
test=subset(loans, split==FALSE)
mod=glm(not.fully.paid~., data=train, family=binomial)
summary(mod)
pred=predict(mod, type="response", newdata=test)
test$predicted.risk=pred
str(test)
table(test$not.fully.paid, pred>=0.5)
table(test$not.fully.paid)
library(ROCR)
predROCR= prediction(pred, test$not.fully.paid)
ROCRperf=performance(predROCR, "tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1))
??auc
attributes(performance(predROCR, 'auc'))$y.values[[1]]
mod2=glm(not.fully.paid~ int.rate, data=train, family=binomial)
summary(mod2)
?cor
cor(train$int.rate, train$log.annual.inc)
pred2=predict(mod2, type="response",newdata=test)
max(pred2)
table(test$not.fully.paid, pred2>=0.5)
predROCR= prediction(pred2, test$not.fully.paid)
ROCRperf=performance(predROCR, "tpr","fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1))
??auc
attributes(performance(predROCR, 'auc'))$y.values[[1]]
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)
highInterest=subset(test,test$int.rate>=0.15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans=subset(highInterest, highInterest$profit>=cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)