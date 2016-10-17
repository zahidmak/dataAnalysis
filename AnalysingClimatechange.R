#Analysing Climate change
rm(list=ls())
data=read.csv("C:/Users/Zahid/Downloads/climate_change.csv")
str(data)
trainingset=subset(data, data$Year<=2006)
testingset= subset(data, data$Year>2006)
model1= lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data=trainingset)
summary(model1)
cor(trainingset)
model2= lm(Temp ~ MEI+N2O+TSI+Aerosols, data=trainingset)
summary(model2)
model3=step(lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data=trainingset))
summary(model3)
predictTest=predict(model3,newdata=testingset)
SSE=sum((predictTest-testingset$Temp)^2)
SSE
SST=sum((testingset$Temp-mean(trainingset$Temp))^2)
SST
R2=1-SSE/SST
R2