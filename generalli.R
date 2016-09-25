#Some script used ot create graphes for gaetano generalli

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
png("transport_vs_total_premium_log.png")

#########
#Three graphes on insurance plus one graph on number of countries 
par(mfrow=c(2,1))
par(mar=c(0,5,0,5))
plot(generalli$REVENUES.TOTAL.A.B ~ generalli$YEAR,type="l",col="dark red",log="y",ylab="Florins",xlab="Time",main="",ylim=c(100000,1000000000),lwd=4,axes=F)
points(generalli[,2] ~ generalli$YEAR,type="l",col="dark blue",lwd=4)
#points(generalli$PROFIT ~ generalli$YEAR,type="l",col="darkgreen",lwd=4)
#text(1905,2000000,"Profit",cex=.9,col="dark green")
text(1905,10000000,"Tranport Premium",cex=.9,col="dark blue")
text(1905,100000000,"Total Premium",cex=.9,col="dark red")
par(new=T)
plot(generalli[,2] / generalli$REVENUES.TOTAL.A.B*100 ~ generalli$YEAR,axes=F,xlab="",ylab="",type="h")
axis(4)
axis(2)

mtext("Percent of total",4,3)
text(1910,4, expression(frac(Transport,Total)))


par(mar=c(5,5,0,5))
#plot(names(countYear),countYear,ylim=c(20,35),ylab="Numer trade partnair",xlab="Years",type="l")
plot(unique(allimport$year),tapply(allimport$boat,allimport$year,sum),type="l")#ylim=c(20,35),ylab="Numer trade partnair",xlab="Years",type="l")
par(new=T)
plot(unique(allimport$year),tapply(allimport$volume,allimport$year,sum),type="l",col="red",log="y")#ylim=c(20,35),ylab="Numer trade partnair",xlab="Years",type="l")
plot(unique(allimport$year),tapply(allimport$volume,allimport$year,sum)/tapply(allimport$boat,allimport$year,sum),type="l",col="red")
#######
plot(

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

