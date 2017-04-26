require(maps)

#general setting of some variable widely used:

colpremium="dark orange"
coltransp="dark green"
colpremium="dark orange"
generalli=read.csv("../data/new_generalli.csv")
allimport=read.csv("../data/final_table_dist-import.csv")
allexport=read.csv("../data/final_table_dist-export.csv")

#corgeneralli contain the same value than generalli except that they are corrected to stay in florin
#previously kept to display the dotted line

corgeneralli=generalli
corgeneralli[corgeneralli$YEAR > 1896 ,2:12] = generalli[generalli$YEAR > 1896 ,2:12]/2
corgeneralli[corgeneralli$CURRENCY == "lire" ,2:11] = generalli[generalli$CURRENCY == "lire" ,2:11]/2.64

mapNewDataset<-function(){
    pdf("map1838.pdf",width=9,height=5)
    subset1838=read.csv("../data/subset1838.csv")
    ss38 = unique(allimport[ allimport$port %in% subset1838$port, c("lat","lon")])
    ss38$volume=1000
    plotAndLinkPort(ss38,xlim=range(allimport$lon),ylim=range(allimport$lat))
    dev.off()
}

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

}



graphFlorins<-function(){
    pdf("wageAgaintSilver.pdf")
    options("scipen"=100, "digits"=4)##this is very import as it allow to avoid LOT of problems comming from the fact that will reading the 
    par(mar=c(4,5,1,6))
    plot(corgeneralli$REVENUES_TOTAL_A.B ~ corgeneralli$YEAR,type="l",col=colpremium,lwd=4,lty=1,axes=F,xlab="Years",ylab="")
    #points(corgeneralli$REVENUES_TOTAL_A.B ~ corgeneralli$YEAR,type="l",col=colpremium,lwd=1,lty=1)
    axis(2,col=colpremium ,col.axis=colpremium,col.ticks=colpremium)#label=paste(at,"%",sp=""),at=at)
    mtext("Total Revenues in Florins",2,3,col=colpremium)

    axis(1)

    #par(new=T)
    #plot(generalli$Grams_of_gold_.gAu._per_1fl ~ generalli$YEAR,type="l",axes="F",col="red",lwd=4,xlab="",ylab="")
    #axis(4,col="red",col.axis="red",col.ticks="red")
    #mtext("Price of Gold",4,col="red",line=2)


    par(new=T)

    plot(generalli$Wage_per_day_in_grams_of_.silver_.gAg._inKS ~ generalli$YEAR,type="l",axes="F",xlab="",ylab="",col="blue",lwd=4)
    mtext("Wage/day in grams of silver",4,col="blue",line=2)
    axis(4,col="blue",col.axis="blue",col.ticks="blue")
    mtext("Wage/day in grams of silver",4,col="blue",line=2)
    dev.off()



}

graphDecompositionPremiums<-function(){
    pdf("decomposition_premiums.pdf")
    plot(corgeneralli$REVENUES_TOTAL_A.B ~ corgeneralli$YEAR,type="l",col=colpremium,log="y",ylab="Florins",xlab="Years",main="",ylim=c(100000,1000000000),lwd=4,axes=F)
    points(corgeneralli$REVENUES_TOTAL_A.B[corgeneralli$YEAR >= 1896]~ corgeneralli$YEAR[corgeneralli$YEAR >= 1896],type="l",col=colpremium,lwd=4,lty=3)
    points(corgeneralli[,2] ~ corgeneralli$YEAR,type="l",col=coltransp,lwd=4)
    points(corgeneralli$Fire.Theft.Glass_PREMIUMS.bal_A. ~ corgeneralli$YEAR,type="l",col=3,lwd=4)
    points(corgeneralli$Life_a_PREMIUMS_death.balB. ~ corgeneralli$YEAR,type="l",col=4,lwd=4)
    points(corgeneralli$Life_b_PREMIUMS_life.balB. ~ corgeneralli$YEAR,type="l",col=5,lwd=4)
    #    points(corgeneralli$Life_a_PREMIUMS_death.balB.+corgeneralli$Life_b_PREMIUMS_life.balB. ~ corgeneralli$YEAR,type="l",col=6,lwd=4)
    corgeneralli$Transport_PREMIUMS.bal_A./corgeneralli$REVENUES_TOTAL_A.B - corgeneralli$X._TRANSPORT_PREMIUMS_ON_REVENUES_balance_A.B/100
    lines(corgeneralli$X._TRANSPORT_PREMIUMS_ON_REVENUES_balance_A.B ~ corgeneralli$YEAR,col=8,lwd=4)
    at=axTicks(2)
    labels <- sapply(at, function(i) as.expression(bquote(10^ .(log10(i)))))
    axis(2,at=at,label=labels)
    axis(1,cex=.7)
    #legend("topleft",c("Total Revenues","Tranporti Premium","Fire & Theft","Life","Death","Life+Death"),col=c(colpremium,coltransp,3,4,5,6),lwd=2)
    legend("topleft",c("Total Revenues","Transport","Fire & Theft","Life","Death"),col=c(colpremium,coltransp,3,4,5),lwd=2)
    dev.off()
}



VolumeVsRevenu<-function(){


    pdf("transport.pdf")
    #########
    #Three graphes on insurance plus one graph on number of countries 
    par(fig=c(0,1,0.3,1))

    par(mar=c(0,5,1,5))
    colpremium="dark orange"
    coltransp="dark green"
    plot(corgeneralli$REVENUES_TOTAL_A.B ~ corgeneralli$YEAR,type="l",col=colpremium,log="y",ylab="Florins",xlab="",main="",ylim=c(100000,1000000000),lwd=4,axes=F)
    #points(generalli$REVENUES_TOTAL_A.B[generalli$YEAR >= 1896]~ generalli$YEAR[generalli$YEAR >= 1896],type="l",col=colpremium,lwd=4,lty=3)
    points(corgeneralli[,2] ~ corgeneralli$YEAR,type="l",col=coltransp,lwd=4)
    #points(generalli[generalli$YEAR >= 1896 ,2] ~ generalli$YEAR[generalli$YEAR >= 1896],type="l",lty=3,col=coltransp,lwd=4)
    #points(generalli[generalli$YEAR >= 1896 ,2] ~ generalli$YEAR[generalli$YEAR >= 1896],type="l",lty=3,col=coltransp,lwd=4)
    #text(1905,2000000,"Profit",cex=.9,col="dark green")
    text(1905,1000000,"Transport",cex=.9,col=coltransp)
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

    par(xpd=NA)
    dataset=allimport
    sumtot=tapply(dataset[,"volume"],dataset$year,sum)
    dataset=allexport
    sumtot=sumtot+tapply(dataset[,"volume"],dataset$year,sum)
    plot(unique(dataset$year),sumtot,type="l",ylim=c(1,max(sumtot)),xlab="Years",axes=F,cex.axis=.6,lwd=2,ylab="",xlim=c(1851,1914))
    text(1900,sumtot[length(sumtot)]/2.5,"Trieste - Total tonnage of ships",pos=4,cex=.8)
    axis(2,labels=c(0,round(max(sumtot)/2,-2),round(max(sumtot),-2)),at=c(0,round(max(sumtot)/2,-2),round(max(sumtot),-2)),cex.axis=.7,las=1)
    axis(1,line=1)
    events=read.csv("../data/panic73.csv",sep="\t",header=F)

    abline(v=events$V1,lwd=6,col=alpha("black",.1))
    text(events$V1,rep(12000,nrow(events)),substr(events$V2,1,13),col=alpha("red",.6),srt=25,cex=.7)
    dev.off()
}
