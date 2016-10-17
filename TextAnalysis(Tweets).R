# Text analytics
rm(list=ls())
tweets=read.csv("C:/Users/Zahid/Downloads/tweets.csv", stringsAsFactors=FALSE)
str(tweets)
tweets$Negative=as.factor(tweets$Avg<=-1)
table(tweets$Negative)
install.packages("tm") #text mining
library(tm)
install.packages("SnowballC") #Word stemmer
library(SnowballC)
corpus=Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)
stopwords("english")[1:100]
corpus=tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus=tm_map(corpus, stemDocument)
frequencies=DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies,lowfreq=20)
sparse=removeSparseTerms(frequencies, 0.995)
?removeSparseTerms
sparse
tweetsSparse=as.data.frame(as.matrix(sparse))
tweetsSparse
colnames(tweetsSparse)=make.names(colnames(tweetsSparse))
make.names(colnames(tweetsSparse))
colnames(tweetsSparse)
tweetsSparse$Negative
tweetsSparse$Negative=tweets$Negative
str(tweets$Negative)
str(tweetsSparse$Negative)
library(caTools)
set.seed(123)
split=sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse=subset(tweetsSparse, split==TRUE)
testSparse=subset(tweetsSparse, split==FALSE)
library(rpart)
library(rpart.plot)
tweetCART=rpart(Negative~., data=trainSparse, method="class")
prp(tweetCART)
trainSparse
predictCART=predict(tweetCART, newdata=testSparse, type="class")
table(testSparse$Negative, predictCART)
library(randomForest)
set.seed(123)
tweetRF=randomForest(Negative~., data=trainSparse)
predictRF=predict(tweetRF, newdata=testSparse)
table(testSparse$Negative, predictRF)
tweetLog=glm(Negative~.,data=trainSparse, family=binomial)
predictions = predict(tweetLog, newdata=testSparse, type="response")
table(testSparse$Negative, predictions>=0.5)
