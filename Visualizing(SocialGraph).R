rm(list=ls())
edges=read.csv("C:/users/zahid/downloads/edges.csv")
users=read.csv("C:/users/zahid/downloads/users.csv")
str(edges)
str(users)
(rowSums( table(edges)))
table(users$locale)
table(users$gender, users$school)
install.packages("igraph")
library(igraph)
?graph.data.frame
g= graph.data.frame(edges, FALSE, users)

#g = graph.data.frame(edges, TRUE, users)
plot(g, vertex.size=5, vertex.label=NA)
min(degree(g))
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color[V(g)$school == "A"] = "blue"

V(g)$color[V(g)$school == "AB"] = "green"


V(g)$color[V(g)$school == "A"] = "purple"

V(g)$color[V(g)$school == "B"] = "brown"


?igraph.plotting
