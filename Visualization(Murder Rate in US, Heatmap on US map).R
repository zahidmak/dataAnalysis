rm(list=ls())
murders=read.csv("C:/users/zahid/downloads/murders.csv")
str(murders)
statesMap=map_data("state")
str(statesMap)
ggplot(statesMap, aes(x=long,y=lat, group=group))+geom_polygon(fill="white", color="black")+coord_map("mercator")
murders$region=tolower(murders$State)
murderMap=merge(statesMap,murders,by="region")
str(murderMap)
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Murders))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend")
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=Population))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend")
murderMap$MurderRate=murderMap$Murders/murderMap$Population*100000
murderMap$GunOwnershipRate=murderMap$GunOwnership/murderMap$Population*100000
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend")
ggplot(murderMap,aes(x=long,y=lat,group=group,fill=MurderRate))+geom_polygon(color="black")+scale_fill_gradient(low="black", high="red", guide="legend",limits=c(0.9,10))

ggplot(murderMap, aes(x = long, y = lat, group=group, fill = GunOwnership)) + geom_polygon(color="black") + scale_fill_gradient(low = "black", high = "red", guide="legend")
str(murderMap)
