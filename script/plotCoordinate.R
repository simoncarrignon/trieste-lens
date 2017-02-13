
par(mfrow=c(2,1),mar=c(2,2,2,2))

import=read.csv("final_table_dist-import.csv")
countries=countries[ grep("total",countries$country,invert=T) ,]

hist(as.numeric(countries$volume[countries$years == 1909]),breaks=100)
boxplot(as.numeric(countries$volume) ~ countries$year)
countries=read.csv("final_table_dist-export.csv")
countries$volume=as.numeric(as.character(countries$volume))
write.csv(countries,"final_table_dist-export.csv",row.names=F)
countries=countries[ grep("total",countries$country,invert=T) ,]
plot(as.numeric(as.character(countries$volume)) ~ countries$year)
boxplot(as.numeric(as.character(countries$volume)) ~ countries$year,log="y")
pointsCountry(countries,"usa")

u= getCountries(countries,"usa")
points(getCountries(countries,"usa")$volumes ~$year ,col="red")
#map("worldHires","word",  col="gray90", fill=TRUE)#,xlim=c(minx-.1*abs(maxx-minx),maxx+.1*abs(maxx-minx)),ylim=c(miny-.1*abs(maxy-miny),maxy+.1*abs(maxy-miny)),mar=c(0,0,1,0))
plot(countries[,2],countries[,3])
points(countries[1,2],countries[1,3],col="red")
