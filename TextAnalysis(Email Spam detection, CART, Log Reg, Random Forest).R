rm(list=ls())
emails=read.csv("C:/Users/Zahid/Downloads/emails.csv", stringsAsFactors=FALSE)
table(emails$spam)
emails[1]
which.max(nchar(emails$text))
nchar(emails$text)[2651]
which.min(nchar(emails$text))
nchar(emails$text)[1992]
library(tm)
corpus=Corpus(VectorSource(emails$text))
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)
corpus= tm_map(corpus, removeWords, stopwords('english'))
corpus= tm_map(corpus, stemDocument)
dtm=DocumentTermMatrix(corpus)
str(dtm)
spdtm=removeSparseTerms(dtm, sparse=0.95)
emailsSparse=as.data.frame(as.matrix(spdtm))
colnames(emailsSparse)=make.names(colnames(emailsSparse))
?colnames
which.max(colSums(emailsSparse))
nrow(emailsSparse)
emailsSparse$spam=emails$spam
colSums(emailsSparse)>=5000
table(colSums(emailsSparse)>=5000)
table((sort(colSums(subset(emailsSparse, spam == 1))))>=1000)
emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)

split=sample.split(emailsSparse$spam, 0.7)
train=subset(emailsSparse, split==TRUE)
test=subset(emailsSparse, split==FALSE)
spamLog=glm(spam~.,data=train)
library(rpart)
library(rpart.plot)
library(randomForest)
spamCART=rpart(spam~.,data=train)
set.seed(123)
spamRF= randomForest(spam~., data=train)
predLog=predict(spamLog)
predCART=predict(spamCART)[,2]
predRF=predict(spamRF,type="prob")[,2]
table(predLog<0.00001)
table(predLog>0.99999)
table(predLog<0.99999 & predLog> 0.00001)
summary(spamLog)
prp(spamCART)
table(train$spam, predLog>=0.5)
(3052+954)/nrow(train)
library(ROCR)
predROCR= prediction(predLog,train$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
table(train$spam, predCART>=0.5)
(2885+894)/nrow(train)
predROCR= prediction(predCART,train$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
table(train$spam, predRF)
(3013+914)/nrow(train)
predROCR= prediction(predRF,train$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
predLog=predict(spamLog, newdata=test)
predCART=predict(spamCART,newdata=test)[,2]
predRF=predict(spamRF,newdata=test,type="prob")[,2]
table(test$spam, predLog>=0.5)
(1258+376)/nrow(test)
predROCR= prediction(predLog,test$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
table(test$spam, predCART>=0.5)
(1228+386)/nrow(test)
predROCR= prediction(predCART,test$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
table(test$spam, predRF>=0.5)
(1290+386)/nrow(test)
predROCR= prediction(predRF,test$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
wordCount = rowSums(as.matrix(dtm))
hist(log(wordCount))
emailsSparse$logWordCount=log(wordCount)
?boxplot
boxplot(logWordCount~spam, data=emailsSparse)
train2=subset(emailsSparse, split==TRUE)
test2=subset(emailsSparse, split==FALSE)
spam2CART=rpart(spam~.,data=train2)
set.seed(123)
spam2RF=randomForest(spam~., data=train2)
summary(spam2CART)
prp(spam2CART)
pred2CART=predict(spam2CART, newdata=test2)
table(test2$spam, pred2CART[,2]>=0.5)
(1214+384)/nrow(test2)
predROCR= prediction(pred2CART[,2],test2$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
pred2RF=predict(spam2RF, newdata=test2,type="prob")[,2]
table(test2$spam, pred2RF>=0.5)
(1296+383)/nrow(test2)
predROCR= prediction(pred2RF,test2$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]
install.packages("RTextTools")
library(RTextTools)
dtm2gram = create_matrix(as.character(corpus), ngramLength=2)
str(dtm2gram)
spdtm2gram=removeSparseTerms(dtm2gram, 0.95)
str(spdtm2gram)
emailsSparse2gram = as.data.frame(as.matrix(spdtm2gram))
emailsCombined = cbind(emailsSparse, emailsSparse2gram)
colnames(emailsSparse2gram)=make.names(colnames(emailsSparse2gram))
trainCombined= subset(emailsCombined, split==TRUE)
testCombined=subset(emailsCombined, split==FALSE)
spamCARTcombined=rpart(spam~.,data=trainCombined)

library(randomForest)

prp(spamCARTcombined,varlen=0)
predCARTcombined=predict(spamCARTcombined,newdata=testCombined)[,2]
table(testCombined$spam, predCARTcombined>=0.5)
(1233+374)/nrow(testCombined)
predROCR= prediction(predCARTcombined,testCombined$spam)
ROCRperf=performance(predROCR, 'tpr', 'fpr')
attributes(performance(predROCR, 'auc'))$y.values[[1]]

str(trainCombined)
set.seed(123)
spamRFcombined=randomForest(spam ~ .,data=trainCombined)

