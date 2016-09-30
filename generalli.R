
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
Bigplot(allimport)
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


	plotAndLinkPort(allimport)



