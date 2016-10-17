rm(list=ls())
parole=read.csv("C:/users/zahid/downloads/parole.csv")
parole$male = as.factor(parole$male)

parole$state = as.factor(parole$state)

parole$crime = as.factor(parole$crime)
table(parole$state, parole$crime)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="blue")
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(male ~ .)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5) + facet_grid(.~male)
ggplot(data = parole, aes(x = age,fill = male)) + geom_histogram(binwidth = 5) + facet_grid(.~male)
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, alpha=0.5,position="identity")

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1,color="blue")
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 0.1,color="blue")
ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1,color="blue")+facet_grid(.~crime)
ggplot(data = parole, aes(x = time.served,fill=crime)) + geom_histogram(binwidth = 1,color="blue",alpha=0.5,position="identity")
