#Final exam Assignment 1
rm(list=ls())
elantra=read.csv("C:/users/zahid/downloads/elantra.csv")
str(elantra)
train=subset(elantra, elantra$Year<=2012)
test=subset(elantra, elantra$Year>2012)


#Logical Regression model
model=lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries, data=train)
summary(model)
model1=lm(ElantraSales~Unemployment+CPI_all+CPI_energy+Queries+Month, data=train)
summary(model1)
