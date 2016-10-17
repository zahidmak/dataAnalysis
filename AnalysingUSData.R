# Analysing state data(US)
rm(list=ls())
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
str(statedata)
?state

plot(statedata$x,statedata$y)
tapply(statedata$HS.Grad,statedata$state.region,max)
?boxplot
boxplot(statedata$Murder~statedata$state.region,data=statedata)
NorthEastStates=subset(statedata,statedata$state.region=="Northeast")
boxplot(NorthEastStates$Murder~NorthEastStates$state.name,data=NorthEastStates)
model1=lm(Life.Exp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=statedata)
summary(model1)
plot(statedata$Income, statedata$Life.Exp)
model2=step(lm(Life.Exp ~ Population+Income+Illiteracy+Murder+HS.Grad+Frost+Area,data=statedata))
summary(model2)
pred=predict(model2)
summary(pred)
which.min(pred)
which.min(statedata$Life.Exp)
statedata$state.name[40]
which.max(pred)
which.max(statedata$Life.Exp)
statedata$state.name[11]
which.min(residuals(model2))
which.max(residuals(model2))
residuals(pred)
str(pred)
sort(abs(model2$residuals))
sort(abs(statedata$Life.Exp - predict(model2)))







