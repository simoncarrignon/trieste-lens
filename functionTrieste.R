if(require("vioplot")){library(vioplot)}
library(scales)
require(maps)

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

#######################################################
#######################################################
pointsCountry <- function(d,coun){

    curc=d[d$country == coun,]
    points(curc$volume ~ curc$year ,col="red")
}

#######################################################
#######################################################


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
#######################################################
#######################################################

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

#######################################################
#######################################################
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

#######################################################
#######################################################
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

#######################################################
#######################################################
writeAllTimeSeries<-function(d){
    for(i in unique(d$country)){
	cura=d[d$country == i & d$volume == "steamboat" ,]
	curb=d[d$country == i & d$volume == "vessel" ,]
	write.csv(cura,paste("time/",i,"-steam.csv",sep=""))
	write.csv(curb,paste("time/",i,"-vessel.csv",sep=""))
    }
}
#######################################################
#######################################################
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

#######################################################
#######################################################
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

#######################################################
#######################################################
plotEvol <- function(data){
    years=sort(unique(data$year))
    plot(1,1, type="n",ylim=c(-10,1500000),xlim=c(1,length(years)),xaxt="n",ylab="Volume",xlab="Years")#,main=paste("Density=",(1000-i)/1000 ,sep=""))
    axis(1,at=seq_along(years),labels=sort(years))
    sapply(seq_along(years),function(k){
	   print(seq_along(years))
	   print(years)
	   vioplot(data$volume[data$year == years[k]],at=k,add=T,col="white")})
}


#######################################################
#######################################################
convertPos <- function(){
    allimport$lon=as.numeric(gsub(",",".",allimport$lon))
    allexport$lon=as.numeric(gsub(",",".",allexport$lon))
    allexport$lat=as.numeric(gsub(",",".",allexport$lat))
    plotAndLinkPort(allexport)
}

#######################################################
#######################################################
plotAndLinkPort<-function(dataset,...){
    portcoor=paste(dataset$lon,dataset$lat)
    dataset=cbind(dataset,portcoor)
    tradesize=sort(tapply( dataset$volume, dataset$portcoor, sum ))
    tradesize=cbind.data.frame("vol"=tradesize,"portcoor"=names(tradesize))
    dataset=unique(dataset[,c("portcoor","lon","lat")])
    dataset=merge(tradesize,dataset)
    trieste=c(45.38,13.48)

    par(mar=rep(0,4))
    plot(dataset$lon,dataset$lat,axes=F,ylab="",xlab="",pch=20,cex=log(tradesize$vol)/5,col=alpha("orange",.5),type="n",...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
	 "sky blue")
    map("world",add=T,interior=F,fill=T,col=c("white"),border="white")
    segments(dataset$lon,dataset$lat,rep(trieste[2],ncol(dataset)),rep(trieste[1],ncol(dataset)),col=alpha("orange",.2))
    points(dataset$lon,dataset$lat,pch=20,cex=log(tradesize$vol)/5,col=alpha("orange",.5))
    points(trieste[2],trieste[1],pch=20,cex=2,col=alpha("red",.9))

plotAndLinkPortProj<-function(dataset,...){
    portcoor=paste(dataset$lon,dataset$lat)
    dataset=cbind(dataset,portcoor)
    tradesize=sort(tapply( dataset$volume, dataset$portcoor, sum ))
    tradesize=cbind.data.frame("vol"=tradesize,"portcoor"=names(tradesize))
    dataset=unique(dataset[,c("portcoor","lon","lat")])
    dataset=merge(tradesize,dataset)
    trieste=c(45.38,13.48)
    o=c(trieste,0)
    par(mar=rep(0,4))
    xy=mapproject(dataset$lon,dataset$lat,orientation=o)
    plot(xy,axes=F,ylab="",xlab="",pch=20,cex=log(tradesize$vol)/5,col=alpha("orange",.5),type="n",...)
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=alpha("white",.4))
    xy=map("world",add=T,interior=F,fill=T,col=c("white"),border="white",proj="orthographic", orientation=o)
    xy <- na.omit(data.frame(do.call(cbind, xy[c("x","y")])))
    polygon(max(xy$x)*sin(seq(0,2*pi,length.out=100)),max(xy$y)*cos(seq(0,2*pi,length.out=100)), 
		    col="sky blue", border=alpha("black",.2), lwd=2)
    xy=map("world",add=T,interior=F,fill=T,col=c("lemonchiffon"),border="lemonchiffon",proj="orthographic", orientation=o)
    segments(mapproject(dataset$lon,dataset$lat,orientation=o)$x,mapproject(dataset$lon,dataset$lat,orientation=o)$y,mapproject(rep(trieste[2],ncol(dataset)),rep(trieste[1],ncol(dataset)),orientation=o)$x,mapproject(rep(trieste[2],ncol(dataset)),rep(trieste[1],ncol(dataset)),orientation=o)$y,col=alpha("orange",.2))
    points(mapproject(dataset$lon,dataset$lat,orientation=o),pch=20,cex=log(tradesize$vol)/5,col=alpha("orange",.5))
    points(mapproject(trieste[2],trieste[1],orientation=o),pch=20,cex=2,col=alpha("red",.9))

}
}
#######################################################
#######################################################
summarize <- function(d,sorting){tapply(d$volume,d[,c("year",sorting)],sum)}

#######################################################
#######################################################
plotSubset <- function(d,sorting,type,...){
    mtype=sorting
    if(sorting=="new_loc"){mtype="Country"}
    if(sorting=="sea_basin"){mtype="Sea Basin"}
    if(sorting=="continent"){mtype="Continent"}
    plotAllContinent(summarize(d[d$continent != "Central Europe",],sorting),main=paste(type,"Volume by",mtype),xlab="year",ylab="tons",...)
}

#######################################################
#######################################################
plotStuff <- function(){
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
}

#######################################################
#######################################################
vioplotAll <- function(x,y,...){
    xs=sort(unique(x)) #should be removed => done
    prop=list(summary(x),summary(y))
    ylim=summary(y)[c("Min.","Max.")]
    xlim=summary(x)[c("Min.","Max.")]
    plot(1,1, type="n",ylim=ylim,xlim=c(1,length(xs)),axes=F,...)
    sapply(seq_along(xs),function(k){vioplot(y[x==xs[k]] ,at=k,add=T,col="white")})
    axis(1,at=seq_along(xs),labels=sort(xs))
    axis(2)
    box()
}
#######################################################
#######################################################
checkYear <- function(year,dataset){
    c1898= dataset[ dataset$year == year,]
    par(las=3)
    plot(  c1898$boat ~ c1898$new_loc,cex.axis=.8)
}

#######################################################
#######################################################
doitgif <- function(){
    minlon=min(allimport$lon)
    minlat=min(allimport$lat)
    maxlon=max(allimport$lon)
    maxlat=max(allimport$lat)
    ylim=c(minlat,maxlat)
    xlim=c(minlon,maxlon)
    sapply(unique(allimport$year),function(y){
	   png(paste("map_trieste_",y,".png",sep=""),width=900,height=500)
	   plotAndLinkPort(allimport[allimport$year == y,],ylim=ylim,xlim=xlim)
	   text(80,-25,y,cex=2,col="white")
	   dev.off()
	   })
}

doitgifProj <- function(){
    xy=mapproject(allimport$lon,allimport$lat,orientation=c(trieste,0))
    xy <- na.omit(data.frame(do.call(cbind, xy[c("x","y")])))
    minlon=min(xy$x)
    minlat=min(xy$y)
    maxlon=max(xy$x)
    maxlat=max(xy$y)
    ylim=c(minlat,maxlat)
    xlim=c(minlon,maxlon)
    sapply(unique(allimport$year),function(y){
	   png(paste("map_trieste_proj_",y,".png",sep=""),width=900,height=500)
	   plotAndLinkPortProj(allimport[allimport$year == y,],ylim=ylim,xlim=xlim)
	   text(.8,-.9,y,cex=2,col="black",font=5 )
	   
	   dev.off()
	   })
}

#######################################################
#######################################################
Bigplot <- function(dataset){
    #########
    #Three graphes on insurance plus one graph on number of countries 
    par(fig=c(0,1,0.3,1))

    par(mar=c(0,5,1,5))
    colpremium="dark orange"
    coltransp="dark green"
    plot(corgeneralli$REVENUES_TOTAL_A.B ~ corgeneralli$YEAR,type="l",col=colpremium,log="y",ylab="Florins",xlab="",main="",ylim=c(100000,1000000000),lwd=4,axes=F)
    points(generalli$REVENUES_TOTAL_A.B[generalli$YEAR >= 1896]~ generalli$YEAR[generalli$YEAR >= 1896],type="l",col=colpremium,lwd=4,lty=3)
    points(corgeneralli[,2] ~ corgeneralli$YEAR,type="l",col=coltransp,lwd=4)
    points(generalli[generalli$YEAR >= 1896 ,2] ~ generalli$YEAR[generalli$YEAR >= 1896],type="l",lty=3,col=coltransp,lwd=4)
    points(generalli[generalli$YEAR >= 1896 ,2] ~ generalli$YEAR[generalli$YEAR >= 1896],type="l",lty=3,col=coltransp,lwd=4)
    #text(1905,2000000,"Profit",cex=.9,col="dark green")
    text(1905,1000000,"Tranport Premium",cex=.9,col=coltransp)
    text(1905,70000000,"Total Revenues",cex=.9,col=colpremium)
    at=axTicks(2)
    labels <- sapply(at, function(i) as.expression(bquote(10^ .(log10(i)))))
    axis(2,at=at,label=labels)
    par(new=T)
    colrat=alpha("dark green",.5)
    plot(corgeneralli[,2] / corgeneralli$REVENUES_TOTAL_A.B*100 ~ corgeneralli$YEAR,axes=F,xlab="",ylab="",type="h",col=colrat)
    at=axTicks(4)

    axis(4,col=colrat ,col.axis=colrat,col.ticks=colrat,label=paste(at,"%",sp=""),at=at)
    axis(1,at=c(min(corgeneralli$YEAR),max(corgeneralli$YEAR)),label=c("",""),cex.axis=.8,lwd.ticks=0,hadj=0)
    mtext(max(corgeneralli$YEAR),side=1,at=max(corgeneralli$YEAR),cex=.8)
    mtext(min(corgeneralli$YEAR),side=1,at=min(corgeneralli$YEAR),cex=.8)

    mtext("Percent of transport over total",4,3,col=colrat)
    text(1905,6, expression(frac(Transport,Total)),col=colrat)


    par(fig=c(0,1,0,.3),new=T)

    par(mar=c(4,5,1,5))
    plotBoatType(dataset,"boat",ylab="Number of boats",xlim=c(min(corgeneralli$YEAR),max(corgeneralli$YEAR)))
}


#######################################################
#######################################################
plotBoatType<-function(dataset,value,...){
    par(xpd=NA)
    sumtot=tapply(dataset[,value],dataset$year,sum)
    plot(unique(dataset$year),sumtot,type="l",ylim=c(1,max(sumtot)),xlab="Years",axes=F,cex.axis=.6,lwd=2,...)
    type=unique(dataset$type)
    coltype=c("brown","dark blue")
    names(coltype)=type
    sapply(type,function(i){
	   sumtype=tapply(dataset[dataset$type ==i,value],dataset$year[dataset$type ==i],sum)
	   points(unique(dataset$year),sumtype,type="l",col=coltype[i],lwd=2)
	   text(1910,sumtype[length(sumtype)],i,col=coltype[i],pos=4,cex=.8)
	   })
    text(1910,sumtot[length(sumtot)],"total",pos=4,cex=.8)
    axis(2,labels=c(0,round(max(sumtot)/2,-2),round(max(sumtot),-2)),at=c(0,round(max(sumtot)/2,-2),round(max(sumtot),-2)),cex.axis=.7,las=1)
    axis(1,line=1)
}

#######################################################
#######################################################
printAllNboat <- function(){
    main="Number of"
    dataset=allimport
    suff="imp"
    sapply(unique(dataset$new_loc),function(loc){
	   print(loc)
	   png(paste(suff,"/nboat_",loc,".png",sep=""))
	   countrie=dataset[dataset$new_loc == loc,]
	   plot(countrie$boat[countrie$type==type] ~countrie$year[countrie$type=="steamboat"],main=paste(main,"boat for",loc),ylab="number of boat",xlab="years",add=F,col=coltype[type],xlim=c(1851,1914),type="n",ylim=c(0,max(countrie$boat,na.ommit=T)))
	   for(type in alltype){
	       points(countrie$boat[countrie$type==type] ~countrie$year[countrie$type==type],main=paste(main,type),ylab="boat",col=coltype[type],log="y",pch=20)
	   }
	   legend("topleft",col=coltype,legend=alltype,pch=20)
	   dev.off()
	   })
}

#######################################################
#######################################################
printAllRatio <- function(){
    main="Volume by"
    dataset=allimport
    suff="imp"
    sapply(unique(dataset$new_loc),function(loc){
	   print(loc)
	   png(paste(suff,"/ratio_",loc,".png",sep=""))
	   countrie=dataset[dataset$new_loc == loc,]
	   plot(countrie$volume[countrie$type==type]/countrie$boat[countrie$type==type] ~countrie$year[countrie$type=="steamboat"],main=paste(main,"boat for",loc),ylab="volume/boat",xlab="years",add=F,col=coltype[type],xlim=c(1851,1914),type="n",log="y",ylim=c(8,max(countrie$volume/countrie$boat,na.ommit=T)*10))
	   for(type in alltype){
	       points(countrie$volume[countrie$type==type]/countrie$boat[countrie$type==type] ~countrie$year[countrie$type==type],main=paste(main,type),ylab="volume/boat",col=coltype[type],log="y",pch=20)
	   }
	   legend("topleft",col=coltype,legend=alltype,pch=20)
	   dev.off()
	   })
}
#######
#######################################################
#######################################################
#######################################################
#######################################################
