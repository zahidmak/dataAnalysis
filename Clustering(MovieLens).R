rm(list=ls())
movies=read.table("C:/users/Zahid/Downloads/movieLens.txt", header=FALSE, sep="|", quote="\"")
str(movies)
colnames(movies)= c("ID", "Title","ReleaseDate", "Vi(deoReleaseDate", "IMDB", "Unknown","Action", "Adventure", "Animation", "Childrens","Comedy", "Crime","Documentry", "Drama","Fantasy","FilmNoir", "Horror", "Musical","Mystery","Romance","SciFi","Thriller","War", "Western")
str(movies)
movies$ID=NULL
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL
movies=unique(movies)
table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)
distances=dist(movies[2:20], method="euclidean")
clusterMovies=hclust(distances, method="ward")
plot(clusterMovies)
clusterGroups=cutree(clusterMovies, k=10)
tapply(movies$Romance,clusterGroups, mean)
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
cluster2=subset(movies, clusterGroups==2)
cluster2$Title[1:10]

clusterGroups=cutree(clusterMovies, k=2)

cluster2=subset(movies, clusterGroups==2)
str(cluster2)



