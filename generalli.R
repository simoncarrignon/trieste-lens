
#Some script used ot create graphes for gaetano generalli
#I delete this line. Don't know why it appears. It was a duplicata of a steamboat from a particular year and particular place
#1886,1557,82616,"steamboat","austro illirian litoral","Austria-Hungary","Other Austro-Hungarian ports",13.776819,46.649526,"Mediterranean","Central Europe",0
#1898,6759,156860,"vessel","austria","Austria-Hungary","Other Austro-Hungarian ports",13.776818,45.649526,"Mediterranean","Central Europe",0

options("scipen"=100, "digits"=4)##this is very import as it allow to avoid LOT of problems comming from the fact that will reading the 
prices=read.csv("price.csv")
generalli=read.csv("tano_data/transport")
generalli=read.csv("new_generalli.csv")
points(generalli[,2] ~ generalli$YEAR,type="l")
plot(generalli$PROFIT ~ generalli$YEAR,type="l",col="red",log="y",ylab="",xlab="")
plot(generalli$PROFIT ~ generalli$YEAR,type="l",col="red",ylab="",xlab="")
plot(generalli$PROFIT ~ generalli$YEAR,type="l",col="red",log="y",ylab="",xlab="")
png("transportpremium.png")
plot(generalli[,2] ~ generalli$YEAR,type="l",col="red",ylab="Florins",xlab="Time",main="Transport Premium Total")
dev.off()

corgeneralli=generalli

convertflorint<-function(){
    corgeneralli[corgeneralli$YEAR > 1896 ,2:12] = generalli[generalli$YEAR > 1896 ,2:12]/2
    corgeneralli$REVENUES_TOTAL_A.B[corgeneralli$YEAR > 1896] =corgeneralli$REVENUES_TOTAL_A.B[corgeneralli$YEAR > 1896]/2
    corgeneralli[corgeneralli$CURRENCY == "lire" ,2:11] = generalli[generalli$CURRENCY == "lire" ,2:12]/2.64
}

pdf("transport_vs_total_premium_log_plus_numboat.pdf")
Bigplot(allimport)
events=read.csv("events.csv",sep="\t",header=F)
abline(v=events$V1,lwd=20,col=alpha("black",.1))
text(events$V1,rep(12000,nrow(events)),substr(events$V2,1,13),col=alpha("red",.6),srt=25,cex=.7)
dev.off()



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


allimport[ allimport$new_loc == "UK" & allimport$year >= 1900 & allimport$year <=2870 & allimport$type == "vessel" & allimport$volume/allimport$boat > 500,]
allexport[ allexport$year > 1890 & allexport$type == "vessel" & allexport$boat > 5000,]


allimport=read.csv("final_table_dist-import.csv")
allexport=read.csv("final_table_dist-export.csv")
allI=tapply(allimport$volume, allimport[,c("new_loc","year")],sum)		
allI[is.na(allI)]=0
countYear=apply(allI,2, function(i){length(which(i>0))})


###GET STAT
mean_per_country=tapply( allexport$volume, allexport[,c("new_loc","year")], mean)
sd_per_country=tapply( allexport$volume, allexport[,c("new_loc","year")], sd)
meanYearCountr=apply(mean_per_country,2,mean,na.rm=T)
meanSdYearCountr=apply(sd_per_country,2,mean,na.rm=T)
sdYearCountr=apply(mean_per_country,2,sd,na.rm=T)
lines(sdYearCountr/meanYearCountr,type="l",xlab="",col="red",ylab="")
axis(1,label=names(sdYearCountr),at=1:length(sdYearCountr))
axis(2)


plot(meanSdYearCountr/meanYearCountr,type="l")
write.csv(allimport[allimport$type == "vessel" & allimport$year < 1870 & allimport$volume/allimport$boat > 1000,],"req.csv")

y1959=allimport[allimport$year == 1859,]
y1909=allimport[allimport$year == 1908,]
 y1959=allimport$volume[allimport$type==type && allimport$year == 1859]/allimport$boat[allimport$type==type&& allimport$year == 1859]
y1909= allimport$volume[allimport$type==type & allimport$year == 1909]/allimport$boat[allimport$type==type & allimport$year == 1909]
 vioplot(y1909,col="white")
 vioplot(y1959,col="white")


	plotAndLinkPort(allimport)


    colpremium="dark orange"
    coltransp="dark green"

    par(fig=c(0,1,0.4,1))

    par(mar=c(0,5,1,5))

    pdf("decomposition_premiums.pdf")
    plot(corgeneralli$REVENUES_TOTAL_A.B ~ corgeneralli$YEAR,type="l",col=colpremium,log="y",ylab="Florins",xlab="Years",main="",ylim=c(100000,1000000000),lwd=4,axes=F)
    points(generalli$REVENUES_TOTAL_A.B[generalli$YEAR >= 1896]~ generalli$YEAR[generalli$YEAR >= 1896],type="l",col=colpremium,lwd=4,lty=3)
    points(corgeneralli[,2] ~ corgeneralli$YEAR,type="l",col=coltransp,lwd=4)
    points(corgeneralli$Fire.Theft.Glass_PREMIUMS.bal_A. ~ corgeneralli$YEAR,type="l",col=3,lwd=4)
    points(corgeneralli$Life_a_PREMIUMS_death.balB. ~ corgeneralli$YEAR,type="l",col=4,lwd=4)
    points(corgeneralli$Life_b_PREMIUMS_life.balB. ~ corgeneralli$YEAR,type="l",col=5,lwd=4)
    points(corgeneralli$Life_a_PREMIUMS_death.balB.+corgeneralli$Life_b_PREMIUMS_life.balB. ~ corgeneralli$YEAR,type="l",col=6,lwd=4)
    corgeneralli$Transport_PREMIUMS.bal_A./corgeneralli$REVENUES_TOTAL_A.B - corgeneralli$X._TRANSPORT_PREMIUMS_ON_REVENUES_balance_A.B/100
    lines(corgeneralli$X._TRANSPORT_PREMIUMS_ON_REVENUES_balance_A.B ~ corgeneralli$YEAR,col=8,lwd=4)
    at=axTicks(2)
    labels <- sapply(at, function(i) as.expression(bquote(10^ .(log10(i)))))
    axis(2,at=at,label=labels)
    axis(1,cex=.7)
    legend("topleft",c("Total Revenues","Tranporti Premium","Fire & Theft","Life","Death","Life+Death"),col=c(colpremium,coltransp,3,4,5,6),lwd=2)
    dev.off()

    par(fig=c(0,1,0.22,.38),new=T)
    par(mar=c(0,5,1,5))
    plot(generalli$Wage_per_day_in_grams_of_.silver_.gAg._inKS ~ generalli$YEAR,type="l",axes="F",xlab="Years",ylab="wage/day")
    axis(2)

    par(fig=c(0,1,0.0,.22),new=T)
    par(mar=c(4,5,1,5))
    plot(generalli$Grams_of_gold_.gAu._per_1fl ~ generalli$YEAR,type="l",axes="F",xlab="Years",ylab="Price of Gold")
    axis(1)
    axis(2)

    par(mar=c(4,5,1,6))
    plot(generalli$REVENUES_TOTAL_A.B ~ generalli$YEAR,type="l",col=colpremium,lwd=4,lty=3,axes=F,xlab="Years".ylab="")
    points(corgeneralli$REVENUES_TOTAL_A.B ~ corgeneralli$YEAR,type="l",col=colpremium,lwd=4)
    axis(2,col=colpremium ,col.axis=colpremium,col.ticks=colpremium)#label=paste(at,"%",sp=""),at=at)
    mtext("Total Revenu in Florins",2,3,col=colpremium)

    axis(1)

    par(new=T)
    plot(generalli$Grams_of_gold_.gAu._per_1fl ~ generalli$YEAR,type="l",axes="F",col="red",lwd=4,xlab="",ylab="")
    axis(4,col="red",col.axis="red",col.ticks="red")
    mtext("Price of Gold",4,col="red",line=2)


    par(new=T)
   
    plot(generalli$Wage_per_day_in_grams_of_.silver_.gAg._inKS ~ generalli$YEAR,type="l",axes="F",xlab="",ylab="",col="blue",lwd=4)
    axis(4,col="blue",col.axis="blue",col.ticks="blue",line=3)
    mtext("Wage/day in florin",4,col="blue",line=5)

    #text(1905,1000001,"Tranport Premium",cex=.9,col=coltransp)
    #text(1905,70000000,"Total Premium",cex=.9,col=colpremium)
    #text(1905,50000000,"Fire & Theft",cex=.9,col=3)
    #text(1905,50000000,"Life",cex=.9,col=4)
    #text(1905,50000000,"death",cex=.9,col=5)

