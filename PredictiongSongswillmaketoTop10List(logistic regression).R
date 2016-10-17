# Predictiong Songs will make to Top 10 List
rm(list=ls())
songs=read.csv("C:/Users/Zahid/Downloads/songs.csv")
str(subset(songs, songs$year==2010))
temp = subset(songs, songs$artistname=="Michael Jackson" & songs$Top10==1)
temp
table(songs$timesignature)
str(songs)
which.max(songs$tempo)
songs$songtitle[6206]
train=subset(songs, songs$year<=2009)
test = subset(songs, songs$year==2010)
str(train)
SongsLog1 = glm(Top10 ~ ., data=train, family=binomial)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[, !(names(train) %in% nonvars) ]
test= test[,!(names(test) %in% nonvars)]
model1=glm(Top10 ~ ., data=train, family=binomial)
summary(model1)
str(train)
cor(train)

model2= glm(Top10 ~.-loudness, data=trainEnergy, family=binomial) #without loudness
summary(model2)
model3= glm(Top10 ~. -energy, data=trainLoudness, family=binomial) #without energy
summary(model3)
summary(model1)
pred1 = predict(model3, type="response", newdata=test)
table(test$Top10, pred1>=0.45)
pred2 = predict(model1, type="response", newdata=test)
table(test$Top10, pred2>=0.45)
table(test$Top10)









