if(require("vioplot")){library(vioplot)}
#This R file allow to write some different output and do some basic simple graphs
getCountries <- function(d,coun){
	return(d[d$country == coun,])

}
pointsCountry <- function(d,coun){

	curc=d[d$country == coun,]
	points(curc$volume ~ curc$year ,col="red")
}

plotAllCountry<-function(d,...){
	d=d[order(d$year),]

	plot(d$volume ~ d$year,type="n",...)
	ccol=1
	for(i in unique(d$country)){
		

		curvess=d[d$country == i & d$type == "vessel"  ,]
		cursteam=d[d$country == i & d$type == "steamboat"  ,]
		
		#cur=d[d$country == i & d$type == "steam"  ,]
		#print(cur)
		lines(cursteam$volume ~ cursteam$year,col=ccol)
		ccol=ccol+1
	}

}

addCoordinateAndDist<-function(d){
	res=d[,c("year","boat","volume","type","country")]
	port=rep(NA,nrow(d))
	long=rep(NA,nrow(d))
	lat=rep(NA,nrow(d))
	distance=rep(NA,nrow(d))
	res=cbind(res,port,long,lat,distance)
	countries=read.csv("MILES_export_country-loc.csv")
	countries=rbind(read.csv("MILES_import_country-loc.csv"))
	for(i in unique(countries$country)){
		print(i)
		res$long[res$country == i] =  countries$long[countries$country == i]
		res$port[res$country == i] =  as.character(countries$port[countries$country == i])
		res$lat[res$country == i] =  countries$lat[countries$country == i]
		res$distance[res$country == i] =  countries$nautical.distance[countries$country == i]
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

plotBoth <- function(data){
	   years=unique(data$year)
	   plot(1,1, type="n",ylim=c(-10,1600000),xlim=c(1,length(years)*2),xaxt="n",ylab="Volume",xlab="Years")#,main=paste("Density=",(1000-i)/1000 ,sep=""))
	   axis(1,at=seq(1,length(years)*2,2),labels=sort(years))
	   sapply(seq(1,length(years)*2,2),function(k){
		  print(seq_along(years))
		  print(years)
		  vioplot(data$volume[data$year == years[k]],at=k,add=T,col="white")
		  vioplot(data$volume[data$year == years[k]],at=k+1,add=T)})
}

plotEvol <- function(data){
	   years=unique(data$year)
	   plot(1,1, type="n",ylim=c(-10,1600000),xlim=c(1,length(years)),xaxt="n",ylab="Volume",xlab="Years")#,main=paste("Density=",(1000-i)/1000 ,sep=""))
	   axis(1,at=seq_along(years),labels=sort(years))
	   sapply(seq_along(years),function(k){
		  print(seq_along(years))
		  print(years)
		  vioplot(data$volume[data$year == years[k]],at=k,add=T,col="white")})
}


#miles=0
#write.csv(cbind(miles,import_country),"import_country.csv",row.names=F)
#write.csv(cbind(miles,export_country),"export_country.csv",row.names=F)
#export_country,
#import_country=as.character(sort(unique(allimport$country)))
#export_country=as.character(sort(unique(allexport$country)))
#write.csv(
#
##Import data
allimport=read.csv("final_table-import.csv")
allexport=read.csv("final_table-export.csv")
allimport=na.omit(allimport)
allexport=na.omit(allexport)
write.csv(print(unique(c(as.character(allexport$country),as.character(allimport$country)))),"all_cities.csv",row.names=F)

#allImpLong=addCoordinateAndDist(allimport)
#allExpLong=addCoordinateAndDist(allexport)
allimport=allimport[ grep("total",allimport$country,invert=T) ,]

#Reorder the data by year (better visualisation)
#allexport=allexport[order(allexport$year),]
#allimport=allimport[order(allimport$year),]


#print the graph:

#png("tonnage-export.png")
#plotAllCountry(allexport,ylab="Tonnage",xlab="",main="Evolution of tonnage exportation \n by country and year (log scale)")
#dev.off()
#
#png("tonnage-import.png")
#plotAllCountry(allimport,ylab="Tonnage",xlab="",main="Evolution of tonnage importation \n by country and year (log scale)")
#dev.off()
#
#writeAllyears(allexport,"export")
#writeAllyears(allimport,"import")
#
write.csv(allImpLong,"final_table_dist-import.csv",row.names=F)
write.csv(allExpLong,"final_table_dist-export.csv",row.names=F)

    sapply(unique(allimport$year),function(i){
	   pdf(paste("tmp/100/agentwrtK_D-",formatC(1000-i,width=4,format="d",flag="0"),".pdf",sep=""),pointsize=22)
	   par(mar=c(5,4,2,.5),cex.lab=1.2)
	   t_comp=getLastIt(alld100WOUT[alld100WOUT$Sparsity ==i,])

	   dev.off()
	})
png("img/distrib_export.png",width=1600,height=800)
plotEvol(allexport)
dev.off()
png("img/distrib_import.png",width=1600,height=800)
plotEvol(allimport)
dev.off()
