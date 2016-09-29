if(require("vioplot")){library(vioplot)}
if(require("poweRlaw")){library(poweRlaw)}

options("scipen"=100, "digits"=4)

estimateAlpha<-function(data){
    mpl=displ$new(data)
    est=estimate_xmin(mpl)
    mpl$setXmin(est)
    return(mpl$pars)
}
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
	for(i in unique(d$new_loc)){
		

		curvess=d[d$new_loc == i & d$type == "vessel"  ,]
		cursteam=d[d$new_loc == i & d$type == "steamboat"  ,]
		
		#cur=d[d$new_loc == i & d$type == "steam"  ,]
		#print(cur)
		lines(cursteam$volume ~ cursteam$year,col=ccol)
		text(cursteam$year[1],cursteam$volume[1],label=i,col=ccol)
		ccol=ccol+1
	}
	#legend("topleft",legend=unique(d$new_loc),col=1:length(unique(d$country)))

}

plotAllContinent<-function(d,...){

	plot(1,1,ylim=c(1,max(d,na.rm=T)),xlim=c(1855,1910),type="n",...)
	ccol=1
	years=as.numeric(as.character(rownames(d)))
	rampcol=rainbow(length(colnames(d)))
	for(i in colnames(d)){
		lines(d[,i]~years,col=rampcol[ccol])
		
		#curvess=d[d$new_loc == i & d$type == "vessel"  ,]
		#	
		#cursteam=d[d$new_loc == i & d$type == "steamboat"  ,]
		#
		##cur=d[d$new_loc == i & d$type == "steam"  ,]
		##print(cur)
		#lines(cursteam$volume ~ cursteam$year,col=ccol)
		#print(years[!is.na(d[,i])])
	y=min(years[!is.na(d[,i])])
	if(y==Inf) y=NA

		ind=seq_along(years)[years==y]
	val=d[ind,i]
	#print(y)
		#text(y,val  ,label=i,col=rampcol[ccol])
		#text(y,d[years[d],i]  ,label=i,col=rampcol[ccol])
		text(years[1], d[1,i] ,label=i,col=rampcol[ccol])
		ccol=ccol+1
	}
	#legend("topleft",legend=unique(d$new_loc),col=1:length(unique(d$country)))

}
plotAllCountry2<-function(d,...){

	plot(1,1,ylim=c(1,max(d,na.rm=T)),xlim=c(1860,1910),type="n",...)
	ccol=1
	years=as.numeric(as.character(colnames(d)))
	for(i in rownames(d)){
		print(i)
		lines(d[i,]~years,col=ccol)
		
		#curvess=d[d$new_loc == i & d$type == "vessel"  ,]
		#	
		#cursteam=d[d$new_loc == i & d$type == "steamboat"  ,]
		#
		##cur=d[d$new_loc == i & d$type == "steam"  ,]
		##print(cur)
		#lines(cursteam$volume ~ cursteam$year,col=ccol)
		text(years[1], d[i,1] ,label=i,col=ccol)
		ccol=ccol+1
	}
	#legend("topleft",legend=unique(d$new_loc),col=1:length(unique(d$country)))

}

addCoordinateAndDist<-function(d){
	#res=d[,c("year","boat","volume","type","country")]
	#port=rep(NA,nrow(d))
	#lon=rep(NA,nrow(d))
	#lat=rep(NA,nrow(d))
	#new_loc=rep(NA,nrow(d))
	res=d#[,c("year","boat","volume","type","country","continent")]
	nautical_miles=rep(NA,nrow(d))
	sea_basin=rep(NA,nrow(d))
	continent=rep(NA,nrow(d))
	#new_loc=rep(NA,nrow(d))

	#distance=rep(NA,nrow(d))
	#res=cbind(res,new_loc,port,lon,lat,distance)
	res=cbind(res,sea_basin,continent,nautical_miles)
	countries=read.csv("all_countries_tano_philip_ok.csv")
	#countries=rbind(read.csv("MILES_import_country-loc.csv"))
	for(i in unique(countries$OLD)){
		print(i)
		res$nautical_miles[res$country == i] =  as.numeric(as.character(countries$nautical_miles[countries$OLD == i]))
		res$sea_basin[res$country == i] =  as.character(countries$sea_basin[countries$OLD == i])
		res$continent[res$country == i] =  as.character(countries$continent[countries$OLD == i])
		#res$new_loc[res$country == i] =  as.character(countries$NEW[countries$OLD == i])
		#res$distance[res$country == i] =  countries$nautical.distance[countries$OLD == i]
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
	   years=sort(unique(data$year))
	   plot(1,1, type="n",ylim=c(-10,1500000),xlim=c(1,length(years)),xaxt="n",ylab="Volume",xlab="Years")#,main=paste("Density=",(1000-i)/1000 ,sep=""))
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
allimport=read.csv("final_table_dist-import.csv")
allexport=read.csv("final_table_dist-export.csv")

allimport=na.omit(allimport)
allexport=na.omit(allexport)
write.csv(print(unique(c(as.character(allexport$country),as.character(allimport$country)))),"all_cities.csv",row.names=F)

convertPos <- function(){
	allimport$lon=as.numeric(gsub(",",".",allimport$lon))
	allexport$lon=as.numeric(gsub(",",".",allexport$lon))
	allexport$lat=as.numeric(gsub(",",".",allexport$lat))
	trieste=c(45.38,13.48)
	plotAndLinkPort(allexport)
}

plotAndLinkPort<-function(dataset,...){
    tradesize=sort(tapply( dataset$volume, dataset$country, sum ))
    dataset=unique(dataset[,c("country","lon","lat")])
    tradesize=cbind.data.frame("vol"=tradesize,"country"=names(tradesize))
    dataset=merge(tradesize,dataset)

    par(mar=rep(0,4))
    plot(dataset$lon,dataset$lat,axes=F,ylab="",xlab="",pch=20,cex=log(tradesize$vol)/5,col=alpha("orange",.5),type="n",...)
    segments(dataset$lon,dataset$lat,rep(trieste[2],ncol(dataset)),rep(trieste[1],ncol(dataset)),col=alpha("black",.2))
    map("world",add=T)
    points(dataset$lon,dataset$lat,pch=20,cex=log(tradesize$vol)/5,col=alpha("orange",.5))
    points(trieste[2],trieste[1],pch=20,cex=2,col=alpha("red",.9))

}
#allImpLong=addCoordinateAndDist(allimport)
#allExpLong=addCoordinateAndDist(allexport)
allImpLong=allimport
allExpLong=allexport
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
write.csv(allimport,"final_table_dist-import.csv",row.names=F)
write.csv(allexport,"final_table_dist-export.csv",row.names=F)
png("img/distrib_import.png",width=1600,height=800)
plotEvol(allimport)
dev.off()

tapply(allImpLong$volume,allImpLong[,c("year","country")],sum)

summarize <- function(d,sorting){tapply(d$volume,d[,c("year",sorting)],sum)}

plotSubset <- function(d,sorting,type,...){
	mtype=sorting
	if(sorting=="new_loc"){mtype="Country"}
	if(sorting=="sea_basin"){mtype="Sea Basin"}
	if(sorting=="continent"){mtype="Continent"}
	plotAllContinent(summarize(d[d$continent != "Central Europe",],sorting),main=paste(type,"Volume by",mtype),xlab="year",ylab="tons",...)
}

png("ImportByContinent.png")
plotSubset(allImpLong,"continent","Import")
dev.off()
png("ImportByContinent-log.png")
plotSubset(allImpLong,"continent","Import",log="y")
dev.off()
png("ExportByContinent.png")
plotSubset(allExpLong,"continent","Export")
dev.off()
png("ExportByContinent-log.png")
plotSubset(allExpLong,"continent","Export",log="y")
dev.off()

png("ImportBysea_basin.png")
plotSubset(allImpLong,"sea_basin","Import")
dev.off()
png("ImportBysea_basin-log.png")
plotSubset(allImpLong,"sea_basin","Import",log="y")
dev.off()
png("ExportBysea_basin.png")
plotSubset(allExpLong,"sea_basin","Export")
dev.off()
png("ExportBysea_basin-log.png")
plotSubset(allExpLong,"sea_basin","Export",log="y")
dev.off()

png("ImportBynew_loc.png")
plotSubset(allImpLong,"new_loc","Import")
dev.off()
png("ImportBynew_loc-log.png")
plotSubset(allImpLong,"new_loc","Import",log="y")
dev.off()
png("ExportBynew_loc.png")
plotSubset(allExpLong,"new_loc","Export")
dev.off()
png("ExportBynew_loc-log.png")
plotSubset(allExpLong,"new_loc","Export",log="y")
dev.off()
