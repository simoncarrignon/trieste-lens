#This R file allow to write some different output and do some basic simple graphs

plotAllCountry<-function(d,...){
	plot(as.numeric(as.character(d$boat)) ~ d$year,type="n",log="y",...)
	ccol=1
	for(i in unique(d$country)){

		cur=d[d$country == i & d$type == "steamboat"  ,]
		print(cur)
		lines(as.numeric(as.character(cur$boat)) ~ cur$year,col=ccol)
		ccol=ccol+1
	}

}

addCoordinateAndDist<-function(d){
	res=d
	long=rep(NA,nrow(d))
	lat=rep(NA,nrow(d))
	distance=rep(NA,nrow(d))
	res=cbind(res,long,lat,distance)
	countries=read.csv("localisation.csv")
	for(i in unique(countries$country)){
		res$long[res$country == i] =  countries$long[countries$country == i]
		res$lat[res$country == i] =  countries$lat[countries$country == i]
		res$distance[res$country == i] =  as.character(countries$distance[countries$country == i])
	}
	return(res)
}

writeAllTimeSeries<-function(d){
	for(i in unique(d$country)){
		cura=d[d$country == i & d$volume == "steamboat" ,]
		curb=d[d$country == i & d$volume == "vessel" ,]
		write.csv(cura,paste("time/",i,"-steam.csv",sep=""))
		write.csv(curb,paste("time/",i,"-vessel.csv",sep=""))
	}
}
writeAllyears<-function(d,stri){
	for(i in unique(d$year)){
		cura=d[d$year == i & d$type == "steamboat" ,]
		curb=d[d$year == i & d$type == "vessel" ,]
		cura=cura[,c("year","volume","country")]
		curb=curb[,c("year","volume","country")]
		cura$volume=as.numeric(as.character(cura$volume))
		curb$volume=as.numeric(as.character(curb$volume))
		write.csv(cura,paste("years/",stri,"/",i,"-steam.csv",sep=""),row.names=F)
		write.csv(curb,paste("years/",stri,"/",i,"-vessel.csv",sep=""),row.names=F)
	}
}

#Import data
allimport=read.csv("final_table-import.csv")
allexport=read.csv("final_table-export.csv")

#Reorder the data by year (better visualisation)
allexport=allexport[order(allexport$year),]
allimport=allimport[order(allimport$year),]


#print the graph:

#png("tonnage-export.png")
#plotAllCountry(allexport,ylab="Tonnage",xlab="",main="Evolution of tonnage exportation \n by country and year (log scale)")
#dev.off()
#
#png("tonnage-import.png")
#plotAllCountry(allimport,ylab="Tonnage",xlab="",main="Evolution of tonnage importation \n by country and year (log scale)")
#dev.off()
#
allexport=addCoordinateAndDist(allexport)
allimport=addCoordinateAndDist(allimport)
writeAllyears(allexport,"export")
writeAllyears(allimport,"import")

allimport=write.csv(allimport,"final_table_dist-import.csv")
allexport=write.csv(allexport,"final_table_dist-export.csv")

