rm(list=ls())
library(mice)
train=read.csv("c:/users/zahid/downloads/train.csv", stringsAsFactors=FALSE)
test=read.csv("c:/users/zahid/downloads/test.csv",stringsAsFactors=FALSE)
trainOg=read.csv("c:/users/zahid/downloads/train.csv")
str(trainOg)
train=complete(mice(train))
test=complete(mice(test))
max(train$YOB)
train$Happy = as.factor(train$Happy)
train$YOB[train$YOB < 1930] = 0
train$YOB[train$YOB > 2004] = 0
str(train)
train[train=='']=0
train[train=='Yes']=1
train[train=='No']=-1
train[train=='Public']=1  
train[train=='Private']=-1  
train[train=='Science']=1 
train[train=='Art']=-1
train[train=='Study first']=1 
train[train=='Try first']=-1
train[train=='Giving']=1
train[train=='Receiving']=-1 
train[train=='Idealist']=1 
train[train=='Pragmatist']=-1 
train[train=='Cool headed']=1 
train[train=='Hot headed']=-1
train[train=='Standard hours']=1
train[train=='Odd hours']=-1
train[train=='Right']=1
train[train=='Happy']=-1
train[train=='A.M.']=1
train[train=='P.M.']=-1
train[train=='Start']=1
train[train=='End']=-1
train[train=='Circumstances']=1
train[train=='Me']=-1
train[train=='Mysterious']=1
train[train=='TMI']=-1
train[train=='Tunes']=1
train[train=='Talk']=-1
train[train=='People']=1
train[train=='Technology']=-1
train[train=='Supportive']=1
train[train=='Demanding']=-1
train[train=='Mac']=1
train[train=='PC']=-1
train[train=='Risk-friendly']=1
train[train=='Cautious']=-1
train[train=='Yes!']=1
train[train=='Umm...']=-1
train[train=='Socialize']=1
train[train=='Space']=-1
train[train=='Online']=1
train[train=='In-person']=-1
train[train=='Yay people!']=1
train[train=='Grrr people']=-1
train[train=='Own']=1
train[train=='Rent']=-1
train[train=='Dad']=1
train[train=='Mom']=-1
train[train=='Optimist']=1
train[train=='Pessimist']=-1
train[train=='Nope']=1
train[train=='Check!']=-1
train[train=='Only-child']=-1


#Skip this part
train$Party=as.factor(train$Party)
train$EducationLevel=as.factor(train$EducationLevel)
train$HouseholdStatus=as.factor(train$HouseholdStatus)
train$Income=as.factor(train$Income)
train$Gender=as.factor(train$Gender)



summary(model)

train$  Q124742    =   	as.numeric	(	train$	Q124742       	)
train$	 Q124122    =  	as.numeric	(	train$	 Q124122      	)
train$	 Q123464   =   	as.numeric	(	train$	 Q123464      	)
train$	 Q123621   =   	as.numeric	(	train$	 Q123621      	)
train$	 Q122769   =   	as.numeric	(	train$	 Q122769      	)
train$	 Q122770   =   	as.numeric	(	train$	 Q122770      	)
train$	 Q122771   =   	as.numeric	(	train$	 Q122771      	)
train$	 Q122120   =   	as.numeric	(	train$	 Q122120      	)
train$	 Q121699   =   	as.numeric	(	train$	 Q121699      	)
train$	 Q121700   =   	as.numeric	(	train$	 Q121700      	)
train$	 Q120978   =   	as.numeric	(	train$	 Q120978      	)
train$	 Q121011   =   	as.numeric	(	train$	 Q121011      	)
train$	 Q120379   =   	as.numeric	(	train$	 Q120379      	)
train$	 Q120650   =   	as.numeric	(	train$	 Q120650      	)
train$	 Q120472   =   	as.numeric	(	train$	 Q120472      	)
train$	 Q120194   =   	as.numeric	(	train$	 Q120194      	)
train$	 Q120012   =   	as.numeric	(	train$	 Q120012      	)
train$	 Q120014   =   	as.numeric	(	train$	 Q120014      	)
train$	 Q119334   =   	as.numeric	(	train$	 Q119334      	)
train$	 Q119851   =   	as.numeric	(	train$	 Q119851      	)
train$	 Q119650   =   	as.numeric	(	train$	 Q119650      	)
train$	 Q118892   =     	as.numeric	(	train$	 Q118892        	)
train$	 Q118117   =     	as.numeric	(	train$	 Q118117        	)
train$	 Q118232   =     	as.numeric	(	train$	 Q118232        	)
train$	 Q118233   =     	as.numeric	(	train$	 Q118233        	)
train$	Q118237    =    	as.numeric	(	train$	Q118237        	)
train$	Q117186    =    	as.numeric	(	train$	Q117186        	)
train$	Q117193    =    	as.numeric	(	train$	Q117193        	)
train$	 Q116797   =     	as.numeric	(	train$	 Q116797        	)
train$	 Q116881   =     	as.numeric	(	train$	 Q116881        	)
train$	 Q116953   =     	as.numeric	(	train$	 Q116953        	)
train$	 Q116601   =     	as.numeric	(	train$	 Q116601        	)
train$	 Q116441   =     	as.numeric	(	train$	 Q116441        	)
train$	 Q116448   =    	as.numeric	(	train$	 Q116448       	)
train$	 Q116197   =     	as.numeric	(	train$	 Q116197        	)
train$	 Q115602   =     	as.numeric	(	train$	 Q115602        	)
train$	 Q115777   =     	as.numeric	(	train$	 Q115777        	)
train$	 Q115610   =     	as.numeric	(	train$	 Q115610        	)
train$	 Q115611   =     	as.numeric	(	train$	 Q115611        	)
train$	 Q115899   =     	as.numeric	(	train$	 Q115899        	)
train$	 Q115390   =     	as.numeric	(	train$	 Q115390        	)
train$	 Q114961   =     	as.numeric	(	train$	 Q114961        	)
train$	 Q114748   =     	as.numeric	(	train$	 Q114748        	)
train$	 Q115195   =     	as.numeric	(	train$	 Q115195        	)
train$	 Q114517   =     	as.numeric	(	train$	 Q114517        	)
train$	 Q114386   =     	as.numeric	(	train$	 Q114386        	)
train$	 Q113992   =     	as.numeric	(	train$	 Q113992        	)
train$	 Q114152   =    	as.numeric	(	train$	 Q114152       	)
train$	 Q113583   =    	as.numeric	(	train$	 Q113583       	)
train$	 Q113584   =    	as.numeric	(	train$	 Q113584       	)
train$	 Q113181   =    	as.numeric	(	train$	 Q113181       	)
train$	 Q112478   =    	as.numeric	(	train$	 Q112478       	)
train$	 Q112512   =     	as.numeric	(	train$	 Q112512        	)
train$	 Q112270   =     	as.numeric	(	train$	 Q112270        	)
train$	 Q111848   =     	as.numeric	(	train$	 Q111848        	)
train$	 Q111580   =     	as.numeric	(	train$	 Q111580        	)
train$	 Q111220   =     	as.numeric	(	train$	 Q111220        	)
train$	 Q110740   =     	as.numeric	(	train$	 Q110740        	)
train$	 Q109367   =     	as.numeric	(	train$	 Q109367        	)
train$	 Q108950   =     	as.numeric	(	train$	 Q108950        	)
train$	 Q109244   =     	as.numeric	(	train$	 Q109244        	)
train$	 Q108855   =     	as.numeric	(	train$	 Q108855        	)
train$	 Q108617   =     	as.numeric	(	train$	 Q108617        	)
train$	 Q108856   =     	as.numeric	(	train$	 Q108856        	)
train$	 Q108754   =     	as.numeric	(	train$	 Q108754        	)
train$	 Q108342   =     	as.numeric	(	train$	 Q108342        	)
train$	 Q108343   =     	as.numeric	(	train$	 Q108343        	)
train$	 Q107869   =     	as.numeric	(	train$	 Q107869        	)
train$	 Q107491   =     	as.numeric	(	train$	 Q107491        	)
train$	 Q106993   =     	as.numeric	(	train$	 Q106993        	)
train$	 Q106997   =     	as.numeric	(	train$	 Q106997        	)
train$	 Q106272   =     	as.numeric	(	train$	 Q106272        	)
train$	 Q106388   =     	as.numeric	(	train$	 Q106388        	)
train$	 Q106389   =     	as.numeric	(	train$	 Q106389        	)
train$	 Q106042   =     	as.numeric	(	train$	 Q106042        	)
train$	 Q105840   =     	as.numeric	(	train$	 Q105840        	)
train$	 Q105655   =     	as.numeric	(	train$	 Q105655        	)
train$	 Q104996   =     	as.numeric	(	train$	 Q104996        	)
train$	 Q103293   =     	as.numeric	(	train$	 Q103293        	)
train$	 Q102906   =     	as.numeric	(	train$	 Q102906        	)
train$	 Q102674   =     	as.numeric	(	train$	 Q102674        	)
train$	 Q102687   =     	as.numeric	(	train$	 Q102687        	)
train$	 Q102289   =     	as.numeric	(	train$	 Q102289        	)
train$	 Q102089   =     	as.numeric	(	train$	 Q102089        	)
train$	 Q101162   =     	as.numeric	(	train$	 Q101162        	)
train$	 Q101163   =     	as.numeric	(	train$	 Q101163        	)
train$	 Q101596   =     	as.numeric	(	train$	 Q101596        	)
train$	 Q100689   =     	as.numeric	(	train$	 Q100689        	)
train$	 Q100680   =     	as.numeric	(	train$	 Q100680        	)
train$	 Q100562   =     	as.numeric	(	train$	 Q100562        	)
train$	 Q99982    =     	as.numeric	(	train$	 Q99982         	)
train$   Q100010    =       as.numeric	(	train$	 Q100010        	)

train[train=='$25,001 - $50,000'	]	=		2
train[train=='$75,000 - $100,000'	]	=		4
train[train=='$50,000 - $74,999']	=		3
train[train=='under $25,000'	]	=		1
train[train=='over $150,000']=			6
train[train=='$100,001 - $150,000'	]=			5

train[train=='Married (w/kids)'	]=			6
train[train=='Single (no kids)'	]	=		2
train[train=='Married (no kids)'	]	=		4
train[train=='Domestic Partners (no kids)'	]	=		5
train[train=='Single (w/kids)'	]		=	1
train[train=='Domestic Partners (w/kids)'	]=			3
					
train[train=="Master's Degree"	]	=		6
train[train=='High School Diploma'	]	=		2
train[train=='Current K-12'	]		=	1
train[train=='Current Undergraduate'	]=			3
train[train=="Bachelor's Degree"	]		=	5
train[train=="Associate's Degree"	]	=		4
train[train=='Doctoral Degree'	]	=		7
				
train[train=="Democrat"]=4
train[train=="Independent"]=3
train[train=="Republican"]=2
train[train=="Libertarian"]= 5
train[train=="Other"]=1


train[train=='Male'	]		=	0
train[train=='Female'	]	=		1
					
train$Income=as.numeric(train$Income)					
train$HouseholdStatus=as.numeric(train$HouseholdStatus)
train$EducationLevel=as.numeric(train$EducationLevel)
train$Party=as.numeric(train$Party)
train$Gender=as.numeric(train$Gender)

str(train)



str(train)
model=step(glm(Happy~.-UserID,data=train, family=binomial))
pred=predict(model,type="response")
table(train$Happy, pred>=0.5)
summary(modellm)

test[test=='']=0
test[test=='Yes']=1
test[test=='No']=-1
test[test=='Public']=1  
test[test=='Private']=-1  
test[test=='Science']=1 
test[test=='Art']=-1
test[test=='Study first']=1 
test[test=='Try first']=-1
test[test=='Giving']=1
test[test=='Receiving']=-1 
test[test=='Idealist']=1 
test[test=='Pragmatist']=-1 
test[test=='Cool headed']=1 
test[test=='Hot headed']=-1
test[test=='Standard hours']=1
test[test=='Odd hours']=-1
test[test=='Right']=1
test[test=='Happy']=-1
test[test=='A.M.']=1
test[test=='P.M.']=-1
test[test=='Start']=1
test[test=='End']=-1
test[test=='Circumstances']=1
test[test=='Me']=-1
test[test=='Mysterious']=1
test[test=='TMI']=-1
test[test=='Tunes']=1
test[test=='Talk']=-1
test[test=='People']=1
test[test=='Technology']=-1
test[test=='Supportive']=1
test[test=='Demanding']=-1
test[test=='Mac']=1
test[test=='PC']=-1
test[test=='Risk-friendly']=1
test[test=='Cautious']=-1
test[test=='Yes!']=1
test[test=='Umm...']=-1
test[test=='Socialize']=1
test[test=='Space']=-1
test[test=='Online']=1
test[test=='In-person']=-1
test[test=='Yay people!']=1
test[test=='Grrr people']=-1
test[test=='Own']=1
test[test=='Rent']=-1
test[test=='Dad']=1
test[test=='Mom']=-1
test[test=='Optimist']=1
test[test=='Pessimist']=-1
test[test=='Nope']=1
test[test=='Check!']=-1
test[test=='Only-child']=-1



#Skip this part
test$Party=as.factor(test$Party)
test$EducationLevel=as.factor(test$EducationLevel)
test$HouseholdStatus=as.factor(test$HouseholdStatus)
test$Income=as.factor(test$Income)
test$Gender=as.factor(test$Gender)



summary(model)

test$  Q124742    =     as.numeric	(	test$	Q124742       	)
test$	 Q124122    =  	as.numeric	(	test$	 Q124122      	)
test$	 Q123464   =   	as.numeric	(	test$	 Q123464      	)
test$	 Q123621   =   	as.numeric	(	test$	 Q123621      	)
test$	 Q122769   =   	as.numeric	(	test$	 Q122769      	)
test$	 Q122770   =   	as.numeric	(	test$	 Q122770      	)
test$	 Q122771   =   	as.numeric	(	test$	 Q122771      	)
test$	 Q122120   =   	as.numeric	(	test$	 Q122120      	)
test$	 Q121699   =   	as.numeric	(	test$	 Q121699      	)
test$	 Q121700   =   	as.numeric	(	test$	 Q121700      	)
test$	 Q120978   =   	as.numeric	(	test$	 Q120978      	)
test$	 Q121011   =   	as.numeric	(	test$	 Q121011      	)
test$	 Q120379   =   	as.numeric	(	test$	 Q120379      	)
test$	 Q120650   =   	as.numeric	(	test$	 Q120650      	)
test$	 Q120472   =   	as.numeric	(	test$	 Q120472      	)
test$	 Q120194   =   	as.numeric	(	test$	 Q120194      	)
test$	 Q120012   =   	as.numeric	(	test$	 Q120012      	)
test$	 Q120014   =   	as.numeric	(	test$	 Q120014      	)
test$	 Q119334   =   	as.numeric	(	test$	 Q119334      	)
test$	 Q119851   =   	as.numeric	(	test$	 Q119851      	)
test$	 Q119650   =   	as.numeric	(	test$	 Q119650      	)
test$	 Q118892   =     	as.numeric	(	test$	 Q118892        	)
test$	 Q118117   =     	as.numeric	(	test$	 Q118117        	)
test$	 Q118232   =     	as.numeric	(	test$	 Q118232        	)
test$	 Q118233   =     	as.numeric	(	test$	 Q118233        	)
test$	Q118237    =    	as.numeric	(	test$	Q118237        	)
test$	Q117186    =    	as.numeric	(	test$	Q117186        	)
test$	Q117193    =    	as.numeric	(	test$	Q117193        	)
test$	 Q116797   =     	as.numeric	(	test$	 Q116797        	)
test$	 Q116881   =     	as.numeric	(	test$	 Q116881        	)
test$	 Q116953   =     	as.numeric	(	test$	 Q116953        	)
test$	 Q116601   =     	as.numeric	(	test$	 Q116601        	)
test$	 Q116441   =     	as.numeric	(	test$	 Q116441        	)
test$	 Q116448   =    	as.numeric	(	test$	 Q116448       	)
test$	 Q116197   =     	as.numeric	(	test$	 Q116197        	)
test$	 Q115602   =     	as.numeric	(	test$	 Q115602        	)
test$	 Q115777   =     	as.numeric	(	test$	 Q115777        	)
test$	 Q115610   =     	as.numeric	(	test$	 Q115610        	)
test$	 Q115611   =     	as.numeric	(	test$	 Q115611        	)
test$	 Q115899   =     	as.numeric	(	test$	 Q115899        	)
test$	 Q115390   =     	as.numeric	(	test$	 Q115390        	)
test$	 Q114961   =     	as.numeric	(	test$	 Q114961        	)
test$	 Q114748   =     	as.numeric	(	test$	 Q114748        	)
test$	 Q115195   =     	as.numeric	(	test$	 Q115195        	)
test$	 Q114517   =     	as.numeric	(	test$	 Q114517        	)
test$	 Q114386   =     	as.numeric	(	test$	 Q114386        	)
test$	 Q113992   =     	as.numeric	(	test$	 Q113992        	)
test$	 Q114152   =    	as.numeric	(	test$	 Q114152       	)
test$	 Q113583   =    	as.numeric	(	test$	 Q113583       	)
test$	 Q113584   =    	as.numeric	(	test$	 Q113584       	)
test$	 Q113181   =    	as.numeric	(	test$	 Q113181       	)
test$	 Q112478   =    	as.numeric	(	test$	 Q112478       	)
test$	 Q112512   =     	as.numeric	(	test$	 Q112512        	)
test$	 Q112270   =     	as.numeric	(	test$	 Q112270        	)
test$	 Q111848   =     	as.numeric	(	test$	 Q111848        	)
test$	 Q111580   =     	as.numeric	(	test$	 Q111580        	)
test$	 Q111220   =     	as.numeric	(	test$	 Q111220        	)
test$	 Q110740   =     	as.numeric	(	test$	 Q110740        	)
test$	 Q109367   =     	as.numeric	(	test$	 Q109367        	)
test$	 Q108950   =     	as.numeric	(	test$	 Q108950        	)
test$	 Q109244   =     	as.numeric	(	test$	 Q109244        	)
test$	 Q108855   =     	as.numeric	(	test$	 Q108855        	)
test$	 Q108617   =     	as.numeric	(	test$	 Q108617        	)
test$	 Q108856   =     	as.numeric	(	test$	 Q108856        	)
test$	 Q108754   =     	as.numeric	(	test$	 Q108754        	)
test$	 Q108342   =     	as.numeric	(	test$	 Q108342        	)
test$	 Q108343   =     	as.numeric	(	test$	 Q108343        	)
test$	 Q107869   =     	as.numeric	(	test$	 Q107869        	)
test$	 Q107491   =     	as.numeric	(	test$	 Q107491        	)
test$	 Q106993   =     	as.numeric	(	test$	 Q106993        	)
test$	 Q106997   =     	as.numeric	(	test$	 Q106997        	)
test$	 Q106272   =     	as.numeric	(	test$	 Q106272        	)
test$	 Q106388   =     	as.numeric	(	test$	 Q106388        	)
test$	 Q106389   =     	as.numeric	(	test$	 Q106389        	)
test$	 Q106042   =     	as.numeric	(	test$	 Q106042        	)
test$	 Q105840   =     	as.numeric	(	test$	 Q105840        	)
test$	 Q105655   =     	as.numeric	(	test$	 Q105655        	)
test$	 Q104996   =     	as.numeric	(	test$	 Q104996        	)
test$	 Q103293   =     	as.numeric	(	test$	 Q103293        	)
test$	 Q102906   =     	as.numeric	(	test$	 Q102906        	)
test$	 Q102674   =     	as.numeric	(	test$	 Q102674        	)
test$	 Q102687   =     	as.numeric	(	test$	 Q102687        	)
test$	 Q102289   =     	as.numeric	(	test$	 Q102289        	)
test$	 Q102089   =     	as.numeric	(	test$	 Q102089        	)
test$	 Q101162   =     	as.numeric	(	test$	 Q101162        	)
test$	 Q101163   =     	as.numeric	(	test$	 Q101163        	)
test$	 Q101596   =     	as.numeric	(	test$	 Q101596        	)
test$	 Q100689   =     	as.numeric	(	test$	 Q100689        	)
test$	 Q100680   =     	as.numeric	(	test$	 Q100680        	)
test$	 Q100562   =     	as.numeric	(	test$	 Q100562        	)
test$	 Q99982    =     	as.numeric	(	test$	 Q99982         	)
test$   Q100010    =     	as.numeric	(	test$	 Q100010        	)

test[test=='$25,001 - $50,000'	]	=		2
test[test=='$75,000 - $100,000'	]	=		4
test[test=='$50,000 - $74,999']	=		3
test[test=='under $25,000'	]	=		1
test[test=='over $150,000']=			6
test[test=='$100,001 - $150,000'	]=			5

test[test=='Married (w/kids)'	]=			6
test[test=='Single (no kids)'	]	=		2
test[test=='Married (no kids)'	]	=		4
test[test=='Domestic Partners (no kids)'	]	=		5
test[test=='Single (w/kids)'	]		=	1
test[test=='Domestic Partners (w/kids)'	]=			3

test[test=="Master's Degree"	]	=		6
test[test=='High School Diploma'	]	=		2
test[test=='Current K-12'	]		=	1
test[test=='Current Undergraduate'	]=			3
test[test=="Bachelor's Degree"	]		=	5
test[test=="Associate's Degree"	]	=		4
test[test=='Doctoral Degree'	]	=		7

test[test=="Democrat"]=4
test[test=="Independent"]=3
test[test=="Republican"]=2
test[test=="Libertarian"]= 5
test[test=="Other"]=1


test[test=='Male'	]		=	0
test[test=='Female'	]	=		1

test$Income=as.numeric(test$Income)					
test$HouseholdStatus=as.numeric(test$HouseholdStatus)
test$EducationLevel=as.numeric(test$EducationLevel)
test$Party=as.numeric(test$Party)
test$Gender=as.numeric(test$Gender)

str(test)
predtest=predict(model, newdata=test,type="response")
summary(model)
str(pred)
table(train$Happy, pred>=0.5)
summary(model)
str(test)
output=cbind(test$UserID,predtest)
str(output)
View(output)
varImpPlot( rmTrain, n.var = 40, main = "Importance of variables" )
write.csv(output, "C:/users/zahid/downloads/output17.csv")

library(randomForest)
forest=randomForest(Happy~.-UserID, data=train, ntree=5000)
predforest=predict(forest)
table(train$Happy,predforest>=0.5)
predfor=predict(forest)
str(predforest)
table(train$Happy, predfor>=0.5)




















str(train)




