
countries=read.csv("localisation.csv")

#map("worldHires","word",  col="gray90", fill=TRUE)#,xlim=c(minx-.1*abs(maxx-minx),maxx+.1*abs(maxx-minx)),ylim=c(miny-.1*abs(maxy-miny),maxy+.1*abs(maxy-miny)),mar=c(0,0,1,0))
plot(countries[,2],countries[,3])
points(countries[1,2],countries[1,3],col="red")
