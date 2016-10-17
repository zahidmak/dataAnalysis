
rm(list=ls())
tweets=read.csv("C:/Users/Zahid/Downloads/tweets.csv", stringsAsFactors=FALSE)
str(tweets)



library(tm)

library(SnowballC)
corpus=Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)

corpus=tm_map(corpus, removeWords, c( stopwords("english")))

frequencies=DocumentTermMatrix(corpus)
allTweets=as.data.frame(as.matrix(frequencies))

install.packages("wordcloud")
library(wordcloud)
?wordcloud
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.25))

corpus=Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus=tm_map(corpus, tolower)
corpus=tm_map(corpus, removePunctuation)

corpus=tm_map(corpus, removeWords, c("apple", stopwords("english")))

frequencies=DocumentTermMatrix(corpus)
allTweets=as.data.frame(as.matrix(frequencies))
library(RColorBrewer)
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
wordcloud(colnames(allTweets),colSums(allTweets),c(2, 0.25),2,,TRUE,TRUE,.15,pal)
?wordcloud
display.brewer.all()
