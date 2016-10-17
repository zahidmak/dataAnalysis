# Analysing Moneyball baseball game
baseball= read.csv("C:/Users/ZAHID/Downloads/baseball.csv")
str(baseball)
moneyball=subset(baseball, Year<2002)
str(moneyball)
moneyball$RD = moneyball$RS-moneyball$RA
str(moneyball$RD)
WinsReg=lm(W~ RD, data=moneyball)
summary(WinsReg)
test= read.csv("C:/Users/ZAHID/Downloads/baseballTest.csv")
test$RD=test$RS-test$RA
str(test)
prediction= predict(WinsReg, newData=test)
mean(prediction)
y=80.879828+ 0.105626*99
y

RunsReg=lm(RA ~ OOBP+OSLG, data=moneyball)
summary(RunsReg)
y=-804.63 + 2737.77*0.391+1584.91*0.540
y
y1=-837.38 + 2913.60*0.297+1514.29*0.370
y1
data=read.csv("C:/Users/ZAHID/Desktop/Book1.csv")
str(data)
data$Result=with(data,-804.63 + 2737.77*OBP+1584.91*SLG )

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012=c(94,88,95,88,93,94,98,97,93,94)
wins2013=c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2013)