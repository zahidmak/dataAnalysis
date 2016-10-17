rm(list=ls())
emails=read.csv("C:/Users/Zahid/Downloads/energy_bids.csv", stringsAsFactors=FALSE)
str(emails)
emails$email[1]
emails$responsive[1]
emails$email[2]
emails$responsive[2]
table(emails$responsive)
library(tm)
corpus=Corpus(VectorSource(emails$email))
corpus[[1]]
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)
corpus=tm_map(corpus, removeWords, stopwords("english"))
corpus=tm_map(corpus, stemDocument)
dtm=DocumentTermMatrix(corpus)
dtm
dtm=removeSparseTerms(dtm,0.97)
labeledTerms=as.data.frame(as.matrix(dtm))
labeledTerms$responsive=emails$responsive
str(labeledTerms)
library(caTools)
set.seed(144)
split=sample.split(labeledTerms$responsive, 0.7)
train = subset(labeledTerms, split==TRUE)
test = subset(labeledTerms, split==FALSE)
library(rpart)
library(rpart.plot)
emailCART= rpart(responsive~., data=train, method="class")
prp(emailCART)
pred=predict(emailCART, newdata=test)
pred[1:10,]
pred.prob= pred[,2]
table(test$responsive, pred.prob>=0.5)
table(test$responsive)
library(ROCR)
predROCR= prediction(pred.prob, test$responsive)
ROCRperf= performance(predROCR, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)
performance(predROCR, "auc")@y.values
