if(require("vioplot")){library(vioplot)}
library(scales)
#Some script used ot create graphes for gaetano generalli
#I delete this line. Don't know why it appears. It was a duplicata of a steamboat from a particular year and particular place
#1886,1557,82616,"steamboat","austro illirian litoral","Austria-Hungary","Other Austro-Hungarian ports",13.776819,46.649526,"Mediterranean","Central Europe",0
#1898,6759,156860,"vessel","austria","Austria-Hungary","Other Austro-Hungarian ports",13.776818,45.649526,"Mediterranean","Central Europe",0

options("scipen"=100, "digits"=4)##this is very import as it allow to avoid LOT of problems comming from the fact that will reading the 
prices=read.csv("price.csv")
generalli=read.csv("tano_data/transport")
points(generalli[,2] ~ generalli$YEAR,type="l")
plot(generalli$PROFIT ~ generalli$YEAR,type="l",col="red",log="y",ylab="",xlab="")
plot(generalli$PROFIT ~ generalli$YEAR,type="l",col="red",ylab="",xlab="")
plot(generalli$PROFIT ~ generalli$YEAR,type="l",col="red",log="y",ylab="",xlab="")
png("transportpremium.png")
plot(generalli[,2] ~ generalli$YEAR,type="l",col="red",ylab="Florins",xlab="Time",main="Transport Premium Total")
dev.off()

corgeneralli=generalli
convertflorint<-function(){
    corgeneralli[generalli$YEAR >= 1896 ,2:8] = generalli[generalli$YEAR >= 1896 ,2:8]/2
    corgeneralli[generalli$CURRENCY == "lire" ,2:8] = generalli[generalli$CURRENCY == "lire" ,2:8]/2.64
}

pdf("transport_vs_total_premium_log_plus_numboat.pdf")
Bigplot(allexport)
dev.off()

Bigplot <- function(dataset){
    #########
    #Three graphes on insurance plus one graph on number of countries 
    par(fig=c(0,1,0.3,1))

    par(mar=c(0,5,1,5))
    colpremium="dark orange"
    coltransp="dark green"
    plot(corgeneralli$REVENUES.TOTAL.A.B ~ corgeneralli$YEAR,type="l",col=colpremium,log="y",ylab="Florins",xlab="",main="",ylim=c(100000,1000000000),lwd=4,axes=F)
    points(generalli$REVENUES.TOTAL.A.B[generalli$YEAR >= 1896]~ generalli$YEAR[generalli$YEAR >= 1896],type="l",col=colpremium,lwd=4,lty=3)
    points(corgeneralli[,2] ~ corgeneralli$YEAR,type="l",col=coltransp,lwd=4)
    points(generalli[generalli$YEAR >= 1896 ,2] ~ generalli$YEAR[generalli$YEAR >= 1896],type="l",lty=3,col=coltransp,lwd=4)
    points(generalli[generalli$YEAR >= 1896 ,2] ~ generalli$YEAR[generalli$YEAR >= 1896],type="l",lty=3,col=coltransp,lwd=4)
    #text(1905,2000000,"Profit",cex=.9,col="dark green")
    text(1905,1000000,"Tranport Premium",cex=.9,col=coltransp)
    text(1905,70000000,"Total Premium",cex=.9,col=colpremium)
    at=axTicks(2)
    labels <- sapply(at, function(i) as.expression(bquote(10^ .(log10(i)))))
    axis(2,at=at,label=labels)
    par(new=T)
    colrat=alpha("dark green",.5)
    plot(corgeneralli[,2] / corgeneralli$REVENUES.TOTAL.A.B*100 ~ corgeneralli$YEAR,axes=F,xlab="",ylab="",type="h",col=colrat)
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



par(new=T)
plot(unique(allimport$year),tapply(allimport$volume,allimport$year,sum),type="l",col="red",log="y")#ylim=c(20,35),ylab="Numer trade partnair",xlab="Years",type="l")

plot(unique(allimport$year),tapply(allimport$volume,allimport$year,sum)/tapply(allimport$boat,allimport$year,sum),type="l",col="red")

alltype=unique(allimport$type)
    coltype=c("brown","dark blue")
    names(coltype)=alltype
    type="steamboat"
    type="vessel"

pdf(paste("volume_by_boat.pdf",sep=""),width=9,height=7)
    par(mfrow=c(1,2))
ylim=c(0,5000)
for(type in alltype){
    boxplot(allimport$volume[allimport$type==type]/allimport$boat[allimport$type==type] ~allimport$year[allimport$type==type],main=paste(main,type),ylab="volume/boat",xlab="years",add=F,col=coltype[type],ylim=ylim)
}
dev.off()

boxplot(allimport$volume[allimport$type=="vessel"]/allimport$boat[allimport$type=="vessel"] ~allimport$year[allimport$type=="vessel"],main=paste(main,"type=","vessel"))


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
allimport[ allimport$new_loc == "UK" & allimport$year >= 1900 & allimport$year <=2870 & allimport$type == "vessel" & allimport$volume/allimport$boat > 500,]
allexport[ allexport$year > 1890 & allexport$type == "vessel" & allexport$boat > 5000,]


allimport=read.csv("final_table_dist-import.csv")
allexport=read.csv("final_table_dist-export.csv")
allI=tapply(allimport$volume, allimport[,c("new_loc","year")],sum)		
allI[is.na(allI)]=0
countYear=apply(allI,2, function(i){length(which(i>0))})


###GET STAT
mean_per_country=tapply( allimport$volume, allimport[,c("new_loc","year")], mean)
sd_per_country=tapply( allimport$volume, allimport[,c("new_loc","year")], sd)
meanYearCountr=apply(mean_per_country,2,mean,na.rm=T)
meanSdYearCountr=apply(sd_per_country,2,mean,na.rm=T)
sdYearCountr=apply(mean_per_country,2,sd,na.rm=T)
plot(sdYearCountr/meanYearCountr,type="l")

plot(meanSdYearCountr/meanYearCountr,type="l")
write.csv(allimport[allimport$type == "vessel" & allimport$year < 1870 & allimport$volume/allimport$boat > 1000,],"req.csv")

y1959=allimport[allimport$year == 1859,]
y1909=allimport[allimport$year == 1908,]
 y1959=allimport$volume[allimport$type==type && allimport$year == 1859]/allimport$boat[allimport$type==type&& allimport$year == 1859]
y1909= allimport$volume[allimport$type==type & allimport$year == 1909]/allimport$boat[allimport$type==type & allimport$year == 1909]
 vioplot(y1909,col="white")
 vioplot(y1959,col="white")


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
checkYear <- function(year,dataset){
    c1898= dataset[ dataset$year == year,]
    par(las=3)
    plot(  c1898$boat ~ c1898$new_loc,cex.axis=.8)
}

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
	text(80,-25,y)
	dev.off()
	})
}



