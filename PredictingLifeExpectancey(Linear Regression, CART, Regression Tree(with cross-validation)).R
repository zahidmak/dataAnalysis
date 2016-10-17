rm(list=ls())
data(state)
statedata=data.frame(state.x77)
str(statedata)
lm=lm(Life.Exp~.,data=statedata)
summary(lm)
pred=predict(lm)
sse=sum((statedata$Life.Exp-pred)^2)
sse
lm2=lm(Life.Exp~ Population+Murder+Frost+HS.Grad, data=statedata)
summary(lm2)
pred=predict(lm2)
sse=sum((statedata$Life.Exp-pred)^2)
sse
cor(statedata)
library(rpart)
library(rpart.plot)
CART=rpart(Life.Exp~., data=statedata)
prp(CART)
pred=predict(CART)
sse=sum((statedata$Life.Exp-pred)^2)
sse
CART=rpart(Life.Exp~., data=statedata, control=rpart.control(minbucket=5))
prp(CART)
pred=predict(CART)
sse=sum((statedata$Life.Exp-pred)^2)
sse
CART=rpart(Life.Exp~Area, data=statedata, control=rpart.control(minbucket=1))
pred=predict(CART)
prp(CART)
sse=sum((statedata$Life.Exp-pred)^2)
sse
library(caret)
set.seed(111)
fitControl= trainControl(method="cv", number=10)
cartGrid= expand.grid(.cp=(1:50)*0.01)
train(Life.Exp ~ ., data = statedata, method="rpart", trControl=fitControl, tuneGrid=cartGrid)
CART=rpart(Life.Exp~., data=statedata, control=rpart.control(cp=0.12))
prp(CART)
pred=predict(CART)
prp(CART)
sse=sum((statedata$Life.Exp-pred)^2)
sse
train(Life.Exp ~ Area, data = statedata, method="rpart", trControl=fitControl, tuneGrid=cartGrid)
CART=rpart(Life.Exp~Area, data=statedata, control=rpart.control(cp=0.05))
prp(CART)
pred=predict(CART)
prp(CART)
sse=sum((statedata$Life.Exp-pred)^2)
sse














