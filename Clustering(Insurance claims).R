rm(list=ls())
reimbursement=read.csv("C:/users/zahid/downloads/reimbursement.csv")
str(reimbursement)
rowSums(reimbursement)
modre=reimbursement
modre$age=NULL
modre$reimbursement2008=NULL
modre$reimbursement2009=NULL
str(modre)
table(rowSums(modre)>=1)
tail(sort(cor(modre)),15)
hist(reimbursement$reimbursement2009)
reimbursement$reimbursement2008 = log(reimbursement$reimbursement2008+1)
reimbursement$reimbursement2009 = log(reimbursement$reimbursement2009+1)
table(reimbursement$reimbursement2009==0)
claims=reimbursement
set.seed(144)

spl = sample(1:nrow(claims), size=0.7*nrow(claims))

train = claims[spl,]

test = claims[-spl,]
lm.claims=lm(reimbursement2009 ~ ., data=train)
summary(lm.claims)
pred=predict(lm.claims, newdata=test)
str(pred)
sqrt(sum((test$reimbursement2009-pred)^2)/nrow(test))
sqrt(sum((test$reimbursement2009-mean(train$reimbursement2009))^2))
sum((test$reimbursement2009-(mean(train$reimbursement2009)))^2/nrow(test))
sqrt(sum((test$reimbursement2009-mean(train$reimbursement2008))^2)/nrow(test))
sqrt(mean((test$reimbursement2008 - test$reimbursement2009)^2))
train.limited = train

train.limited$reimbursement2009 = NULL

test.limited = test

test.limited$reimbursement2009 = NULL
library(caret)

preproc = preProcess(train.limited)

train.norm = predict(preproc, train.limited)

test.norm = predict(preproc, test.limited)
mean(train.norm$arthritis)
mean(test.norm$arthritis)
set.seed(144)
km=kmeans(train.norm, center=3)
table(km$cluster)
str(km)
km$centers
library(flexclust)

km.kcca = as.kcca(km, train.norm)

cluster.train = predict(km.kcca)

cluster.test = predict(km.kcca, newdata=test.norm)
str(cluster.test)
table(cluster.test)
train1=subset(train, cluster.train==1)
train2=subset(train, cluster.train==2)
train3=subset(train, cluster.train==3)
test1=subset(test, cluster.test==1)
test2=subset(test, cluster.test==2)
test3=subset(test, cluster.test==3)
mean(train3$reimbursement2009)
lm1=lm(reimbursement2009~.,data=train1)
lm2=lm(reimbursement2009~.,data=train2)
lm3=lm(reimbursement2009~.,data=train3)
summary(lm3)
pred.test1=predict(lm1, newdata=test1)
pred.test2=predict(lm2, newdata=test2)
pred.test3=predict(lm3, newdata=test3)
mean(pred.test3)
sqrt(mean(sum((test3$reimbursement2009-pred.test3)^2)))
all.predictions = c(pred.test1, pred.test2, pred.test3)

all.outcomes = c(test1$reimbursement2009, test2$reimbursement2009, test3$reimbursement2009)
str(all.predictions)
sqrt(mean(sum((all.outcomes-all.predictions)^2)))
sqrt(mean((all.predictions - all.outcomes)^2))
