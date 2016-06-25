
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

writeAllTimeSeries<-function(d){
	for(i in unique(iall$country)){
		cura=d[d$country == i & d$volume == "steamboat" ,]
		curb=d[d$country == i & d$volume == "vessel" ,]
		write.csv(cura,paste("time/",i,"-steam.csv",sep=""))
		write.csv(curb,paste("time/",i,"-vessel.csv",sep=""))
	}
}
writeAllyears<-function(d){
	for(i in unique(iall$country)){
		cura=d[d$country == i & d$volume == "steamboat" ,]
		curb=d[d$country == i & d$volume == "vessel" ,]
		write.csv(cura,paste("time/",i,"-steam.csv",sep=""))
		write.csv(curb,paste("time/",i,"-vessel.csv",sep=""))
	}
}

#Import data
allimport=read.csv("final_table-import.csv")
allexport=read.csv("final_table-export.csv")

#Reorder the data by year (better visualisation)
allexport=allexport[order(allexport$year),]
allimport=allimport[order(allimport$year),]


#print the graph:

png("tonnage-export.png")
plotAllCountry(allexport,ylab="Tonnage",xlab="",main="Evolution of tonnage exportation \n by country and year (log scale)")
dev.off()

png("tonnage-import.png")
plotAllCountry(allimport,ylab="Tonnage",xlab="",main="Evolution of tonnage importation \n by country and year (log scale)")
dev.off()

