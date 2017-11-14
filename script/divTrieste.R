source("functionTrieste.R")
#miles=0
#write.csv(cbind(miles,import_country),"import_country.csv",row.names=F)
#write.csv(cbind(miles,export_country),"export_country.csv",row.names=F)
#export_country,
#import_country=as.character(sort(unique(allimport$country)))
#export_country=as.character(sort(unique(allexport$country)))
#write.csv(
#
##Import data
allimport=read.csv("../data/final_table_dist-import.csv")
#allimport=read.csv("../data/01609-final_table_dist-import_CLEANED.csv")
#allexport=read.csv("../data/01609-final_table_dist-export_CLEANED.csv")
allexport=read.csv("../data/final_table_dist-export.csv")
events=read.csv("../data/events.csv",sep="\t",header=F)

allimport=na.omit(allimport)
allexport=na.omit(allexport)
write.csv(print(unique(c(as.character(allexport$country),as.character(allimport$country)))),"all_cities.csv",row.names=F)

#allImpLong=addCoordinateAndDist(allimport)
#allExpLong=addCoordinateAndDist(allexport)
allImpLong=allimport
allExpLong=allexport
allimport=allimport[ grep("total",allimport$country,invert=T) ,] #useless as it seems dataset has been cleaned

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
plot(subset(allimport, year==1869)$volume,subset(allimport, year==1870)$new <- loc)
boxplot(allimport$volume ~ allimport$year,log="y")

tapply(allImpLong$volume,allImpLong[,c("year","country")],sum)


#bmp("test.bmp")
    par(xpd=NA)
par(mfrow=c(2,1))
    par(mar=c(0,5,1,1))
plot(unique(allimport[allimport$type == "steamboat",]$year),tapply(allimport[allimport$type == "steamboat",]$volume, allimport[allimport$type == "steamboat",]$year, sum),col="blue",type="l",axes=F,xlab="",ylab="Volume of Trade",lwd=3,ylim=c(0,3900000))

lines(unique(allimport[allimport$type == "vessel",]$year),tapply(allimport[allimport$type == "vessel",]$volume, allimport[allimport$type == "vessel",]$year, sum),col="green",lwd=3)

lines(unique(allexport[allexport$type == "steamboat",]$year),tapply(allexport[allexport$type == "steamboat",]$volume, allexport[allexport$type == "steamboat",]$year, sum),col="blue",lty=3,lwd=3)

lines(unique(allexport[allexport$type == "vessel",]$year),tapply(allexport[allexport$type == "vessel",]$volume, allexport[allexport$type == "vessel",]$year, sum),col="green",lty=3,lwd=3)

legend("topleft",legend=c("steamboat import","\t\texport","vessel import","\t  export"),col=c("blue","blue","green","green"),lty=c(1,3,1,3),lwd=3,bty='n')

lines(unique(allexport$year),tapply(allexport$volume, allexport$year, sum),col="red",lty=3,lwd=3)
lines(unique(allimport$year),tapply(allimport$volume, allimport$year, sum),col="red",lwd=3)

legend("topleft",legend=c("steamboat import","\t\texport","vessel import","\t  export","total import","total export"),col=c("blue","blue","green","green","red","red"),lty=c(1,3),lwd=3,bty='n')
legend("topleft",legend=c("Total Volume import","Total Volume export"),col=c("red","red"),lty=c(1,3),lwd=3,bty='n')
axis(2)
axis(1)
box()
text(events$V1,rep(-200000,nrow(events)),substr(events$V2,1,13),col=alpha("red",1),srt=25,cex=.7)

    par(mar=c(4,5,1,1))

plot(unique(allimport$year),tapply(allimport$port, allimport$year, length),col="dar kred",type="l",axes=F,xlab="",ylab="Nb of unique port",lwd=3,ylim=c(0,60))
lines(unique(allimport$year),tapply(allexport$port, allexport$year, length),col="dark red",,lty=3,lwd=3)
legend(1890,30,legend=c("import","export","revenu from \ntransport insurance"),col=c("dark red","dark red","dark orange"),lty=c(1,3,1),lwd=3,bty='n')
axis(1)
axis(2)
par(new=T)
    plot(corgeneralli[corgeneralli$YEAR %in% unique(allimport$year),2] ~ corgeneralli$YEAR[corgeneralli$YEAR %in% unique(allimport$year)] ,type="l",col="dark orange",lwd=4,axes=F,ylab="",xlab="",log="y")

#par(new=T)
#    sumtot=tapply(allimport[,"boat"],allimport$year,sum)
#    plot(unique(allimport$year),sumtot,type="l",ylim=c(1,max(sumtot)),axes=F,cex.axis=.6,lwd=2)
#	   lines(unique(allimport[allimport$type == "vessel",]$year),tapply(allimport[allimport$type == "vessel",]$boat,allimport[allimport$type == "vessel",]$year,sum),col="green",lwd=2)
#	   lines(unique(allexport[allexport$type == "vessel",]$year),tapply(allexport[allexport$type == "vessel",]$boat,allexport[allexport$type == "vessel",]$year,sum),col="green",lwd=2,lty=3)
#
box()
abline(v=events$V1,lwd=2,col=alpha("black",.1))
#text(events$V1,rep(10,nrow(events)),substr(events$V2,1,13),col=alpha("red",.6),srt=25,cex=.7)
#dev.off()

plot(tapply(allimport$volume, allimport$year, sum),col="red",type="l",axes=F,xlab="",ylab="")
plot(tapply(allimport$volume, allimport$year, sum),col="red",type="l",axes=F,xlab="",ylab="")

plot(tapply(allimport$volume, allimport$year, sum)-tapply(allexport$volume, allexport$year, sum),col="red",type="l",xlab="",ylab="")
plot(tapply(allimport$volume, allimport$year, sum)-tapply(allexport$volume, allexport$year, sum),col="red",type="l",xlab="",ylab="")
)

plot

lines(median(allimport$volume[allimport$type == "steamboat" & allimport$year < 1860] / allimport$boat[ allimport$type == "steamboat"& allimport$year < 1860])*tapply(allimport$boat[ allimport$type == "steamboat"],allimport$year[ allimport$type == "steamboat"],sum)~ unique(allimport$year),col= "blue")
plot(tapply(allimport$volume[ allimport$type == "steamboat"],allimport$year[ allimport$type == "steamboat"],sum)~ unique(allimport$year),type="l")


subAllI=allI[reordered,]
par(mar=c(4,7,1,1))
image(t(as.matrix(log10(subAllI))),axes=F,col= colorRampPalette(c("blue", "green"))(n = 1000))
axis(1,at=seq(0,1,length.out=ncol(subAllI)/3),labels=colnames(subAllI)[seq(1,ncol(subAllI),length.out=ncol(subAllI)/3)],las=2)
axis(2,at=seq(0,1,length.out=nrow(subAllI)),labels=rownames(subAllI)[seq(1,nrow(subAllI),length.out=nrow(subAllI))],las=2)



plot(unique(allimport$year),tapply(allimport$volume , allimport$year, sd)/tapply(allimport$volume , allimport$year, mean),type="l",col="red")
points(unique(allexport$year),tapply(allexport$volume , allexport$year, sd)/tapply(allexport$volume , allexport$year, mean),type="l")

plot(unique(allimport$year),apply(tapply(allimport$volume , allimport[c("year", "new_loc")],sd),1,mean,na.rm=TRUE)/apply(tapply(allimport$volume , allimport[c("year", "new_loc")],sum),1,mean,na.rm=TRUE),type="l" ,ylab="")
points(unique(allexport$year),apply(tapply(allexport$volume , allexport[c("year", "new_loc")],sd),1,mean,na.rm=TRUE)/apply(tapply(allexport$volume , allexport[c("year", "new_loc")],sum),1,mean,na.rm=TRUE),type="l" ,col="red" )

##plot number of country
plot(unique(allimport$year),tapply(allimport$new_loc,allimport$year,length),type="l",title="nb of new_loc",ylim=c(0,60),ylab="Number of Country",xlab="")

par(mfrow=c(2,2))
###plot sd only 
plot(unique(allimport$year),apply(tapply(allimport$volume , allimport[c("year", "new_loc")],sd),1,sd,na.rm=TRUE),main="evol. of sd",log="y",ylab="sd",xlab="")
###plot means only
plot(unique(allimport$year),apply(tapply(allimport$volume , allimport[c("year", "new_loc")],mean),1,mean,na.rm=TRUE),main="evol. of mean",log="y",ylab="mean",xlab="")

###plot ration sd/mean
sumvessels=tapply(allimport$volume , allimport[c("year", "new_loc")],sum)
sumvesselse=tapply(allexport$volume , allexport[c("year", "new_loc")],sum)
plot(unique(allimport$year),apply(sumvessels,1,sd,na.rm=TRUE)/(apply(sumvessels,1,mean,na.rm=TRUE)),type="l" ,ylab="sd/mean",xlab="")
plot(unique(allexport$year),apply(sumvesselse,1,sd,na.rm=TRUE)/(apply(sumvesselse,1,mean,na.rm=TRUE)),type="l",col="red")
points(unique(allexport$year),apply(tapply(allexport$volume , allexport[c("year", "new_loc")],sd),1,mean,na.rm=TRUE)/apply(tapply(allexport$volume , allexport[c("year", "new_loc")],sum),1,mean,na.rm=TRUE),type="l" ,col="red" )

###Add the name of the big players
for(y in unique(allimport$year)){
    alli=allimport[allimport$year == y,]
    vol=tapply(alli$volum,alli$new_loc,sum)
text(rep(y,10),seq(.5,1.5,.1),labels=names(sort(vol,decreasing=T))[1:10],cex=.5)
}


meanvol=tapply(allimport$volume , allimport[c("year", "new_loc")],mean)
totavol=apply(meanvol,1,sum,na.rm=T)
totavolCountry=apply(meanvol,2,sum,na.rm=T)
barplot(log(t(meanvol/totavol))
barplot(t(tapply(allimport$volume , allimport[c("year", "new_loc")],mean)/apply(tapply(allimport$volume , allimport[c("year", "new_loc")],mean),2,sum,na.rm=T)))

image(meanvol/totavol)
color=rainbow(length(unique(allimport$new_loc)))
par(mfrow=c(1,2))
wght=meanvol/totavol
 barplot(t(wght[,order(totavolCountry)]),space=0,border=NA,col=brewer.pal(11, "Spectral"))
numloc=length(unique(allimport$new_loc))
 plot(rep(1,numloc),1:length(unique(allimport$new_loc)),col=color,pch=20,axes=F,xlab="",ylab="",cex=2)
 text(rep(1.2,numloc),1:numloc,label=unique(as.character(allimport$new_loc)))
 image(as.matrix((wght) ) )



