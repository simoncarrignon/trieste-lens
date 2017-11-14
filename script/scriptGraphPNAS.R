source("functionTrieste.R") 
source("scriptGeneralli042017.R") #script for the generali's graph
library(colorspace)
####Set of script to draw graphs of PNAS submission:w

allimport=read.csv("../data/final_table_dist-import.csv")
allexport=read.csv("../data/final_table_dist-export.csv")
#events=read.csv("../data/events.csv",sep="\t",header=F)

allimport=na.omit(allimport)
allexport=na.omit(allexport)

pdf("map-global.pdf",width=10,height=3.5)
plotAndLinkPort(rbind(allimport,allexport))#this is the map with connection to all port and total tonnage over the whole period (import + export)
box()
dev.off()


allI=tapply(allimport$volume, allimport[,c("new_loc","year")],sum)		
allI[is.na(allI)]=0
reordered=c("UK","Argentina","Brazil","USA","South Africa, UK","Algeria, France","Netherlands","Malta, UK","Belgium","Spain","Germany","Romania","Russia","Greece","France","Turkey","Italy","Egypt","Japan","India, UK","China")
subAllI=allI[reordered,]
totavol=apply(allI,2,sum,na.rm=T)
pdf("importOrder.pdf")
par(mar=c(4,7,1,1))
logAllI=log(subAllI)
meanAllI=subAllI/totavol
meanAllI=allI/totavol
##image(t(as.matrix(logAllI)),axes=F,col= colorRampPalette(c("red", "yellow","blue"))(n = 1000))
##image(t(as.matrix(logAllI)),axes=F,col= diverge_hcl(872, c = c(100, 0), l = c(50, 90), power = 1.3))
##image(t(as.matrix(logAllI)),axes=F,col=heat_hcl(872, h = c(0, -100), c = c(40, 80), l = c(75, 40),  power = 1))
image(t(as.matrix(logAllI)),axes=F,col=rev(heat_hcl(870, h = c(0, -100), c = c(40, 80), l = c(75, 40),  power = 1.5)))
image(t(as.matrix(log(meanAllI))),axes=F,col=heat_hcl(870, h = c(0, -100), c = c(40, 80), l = c(75, 40),  power = 1))
axis(1,at=seq(0,1,length.out=ncol(subAllI)/3),labels=colnames(subAllI)[seq(1,ncol(subAllI),length.out=ncol(subAllI)/3)],las=2) #draw some date (1 every 3 years)
axis(2,at=seq(0,1,length.out=nrow(subAllI)),labels=rownames(subAllI)[seq(1,nrow(subAllI),length.out=nrow(subAllI))],las=2) #draws country names
dev.off()

allI=tapply(allexport$volume, allexport[,c("new_loc","year")],sum)		
allI[is.na(allI)]=0
reordered=c("UK","Argentina","Brazil","USA","South Africa, UK","Algeria, France","Netherlands","Malta, UK","Belgium","Spain","Germany","Romania","Russia","Greece","France","Turkey","Italy","Egypt","Japan","India, UK","China")
subAllI=allI[reordered,]
pdf("exportOrder.pdf")
par(mar=c(4,7,1,1))
logAllI=log(subAllI)
##image(t(as.matrix(logAllI)),axes=F,col= colorRampPalette(c("red", "yellow","blue"))(n = 1000))
##image(t(as.matrix(logAllI)),axes=F,col= diverge_hcl(872, c = c(100, 0), l = c(50, 90), power = 1.3))
##image(t(as.matrix(logAllI)),axes=F,col=heat_hcl(872, h = c(0, -100), c = c(40, 80), l = c(75, 40),  power = 1))
image(t(as.matrix(logAllI)),axes=F,col=heat_hcl(870, h = c(180, -100), c = c(40, 80), l = c(75, 40),  power = 1))
axis(1,at=seq(0,1,length.out=ncol(subAllI)/3),labels=colnames(subAllI)[seq(1,ncol(subAllI),length.out=ncol(subAllI)/3)],las=2) #draw some date (1 every 3 years)
axis(2,at=seq(0,1,length.out=nrow(subAllI)),labels=rownames(subAllI)[seq(1,nrow(subAllI),length.out=nrow(subAllI))],las=2) #draws country names
dev.off()


####Print a scale for import export
dat=logAllI
lim=range(dat,na.rm=T)
sz=length(unique(sort(dat)))

pdf("scale.pdf",width=2)
par(mar=c(1,4,1,3))
image(x=1,y=seq(lim[1],lim[2],length.out=sz),z=t(as.matrix(seq(lim[1],lim[2],length.out=sz))),col=heat_hcl(sz, h = c(0, -100), c = c(40, 80), l = c(75, 40),  power = 1),axes=F,ylab="Volume (Tonnage)",xlab="",xlim=c(0,1))
po=2:5
at=log(10^(po))
labels <- sapply(po, function(i) as.expression(bquote(10^ .(i))))
axis(2,at=at,label=labels,las=1)
dev.off()
leg_my(subAllI)
####################

###Graph on generalli's data

VolumeVsRevenu()
graphDecompositionPremiums()
graphFlorins()
####################


###boxplot of percentage of trade

pdf("shareTradeImport_port.pdf")
par(xpd=NA)
meanvol=tapply(allimport$volume , allimport[c("year", "port")],sum)
totavol=apply(meanvol,1,sum,na.rm=T)
totavolCountry=apply(meanvol,2,sum,na.rm=T)
color=rainbow(length(unique(allimport$new_loc)))
color=brewer.pal(11, "Spectral")
wght=meanvol/totavol
wght[is.na(wght)]=0 
 top10=names(sort(totavolCountry,decreasing=T)[1:11])
matToPrin=t(wght[,order(totavolCountry)])
 barplot(matToPrin,space=0,border=NA,col=color,ylab="Fraction of trade (import)")
numloc=length(unique(allimport$new_loc))
high=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-10):nrow(matToPrin)]
low=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-11):(nrow(matToPrin)-1)]
 text(rep(50.2,11),(high+low)/2,label=rev(top10),col="black")
 dev.off()

pdf("shareTradeExport_port.pdf")
par(xpd=NA)
meanvol=tapply(allexport$volume , allexport[c("year", "port")],sum)
totavol=apply(meanvol,1,sum,na.rm=T)
totavolCountry=apply(meanvol,2,sum,na.rm=T)
color=rainbow(length(unique(allexport$new_loc)))
color=brewer.pal(11, "Spectral")
wght=meanvol/totavol
wght[is.na(wght)]=0 
 top10=names(sort(totavolCountry,decreasing=T)[1:11])
matToPrin=t(wght[,order(totavolCountry)])
 barplot(matToPrin,space=0,border=NA,col=color,ylab="Fraction of trade (export)")
numloc=length(unique(allexport$new_loc))
high=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-10):nrow(matToPrin)]
low=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-11):(nrow(matToPrin)-1)]
 text(rep(50.2,11),(high+low)/2,label=rev(top10),col="black")
 dev.off()

pdf("shareTradeImport_country.pdf")
par(xpd=NA)
meanvol=tapply(allimport$volume , allimport[c("year", "new_loc")],sum)
totavol=apply(meanvol,1,sum,na.rm=T)
totavolCountry=apply(meanvol,2,sum,na.rm=T)
color=rainbow(length(unique(allimport$new_loc)))
color=brewer.pal(11, "Spectral")
wght=meanvol/totavol
wght[is.na(wght)]=0 
 top10=names(sort(totavolCountry,decreasing=T)[1:11])
matToPrin=t(wght[,order(totavolCountry)])
 barplot(matToPrin,space=0,border=NA,col=color,ylab="Fraction of trade (import)")
numloc=length(unique(allimport$new_loc))
high=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-10):nrow(matToPrin)]
low=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-11):(nrow(matToPrin)-1)]
 text(rep(50.2,11),(high+low)/2,label=rev(top10),col="black")
 dev.off()

pdf("shareTradeExport_country.pdf")
par(xpd=NA)
meanvol=tapply(allexport$volume , allexport[c("year", "new_loc")],sum)
totavol=apply(meanvol,1,sum,na.rm=T)
totavolCountry=apply(meanvol,2,sum,na.rm=T)
color=rainbow(length(unique(allexport$new_loc)))
color=brewer.pal(11, "Spectral")
wght=meanvol/totavol
wght[is.na(wght)]=0 
 top10=names(sort(totavolCountry,decreasing=T)[1:11])
matToPrin=t(wght[,order(totavolCountry)])
 barplot(matToPrin,space=0,border=NA,col=color,ylab="Fraction of trade (export)")
numloc=length(unique(allexport$new_loc))
high=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-10):nrow(matToPrin)]
low=cumsum(matToPrin[,ncol(matToPrin)])[(nrow(matToPrin)-11):(nrow(matToPrin)-1)]
 text(rep(50.2,11),(high+low)/2,label=rev(top10),col="black")
 dev.off()




 text(rep(40.2,numloc),seq(0.1,.9,length.out=11),label=rev(top10),col="white")

 plot(rep(1,numloc),1:length(unique(allimport$new_loc)),col=color,pch=20,axes=F,xlab="",ylab="",cex=2)
 text(rep(1.2,numloc),1:numloc,label=names(sort(totavolCountry,decreasing=T))[1:11])
 image(as.matrix((wght) ) )


gen=read.csv("generalTot.cs",row.names=1)

pdf("generali4.pdf")
color=brewer.pal(5, "Set1")
plot(rownames(gen),gen$Municipal,type="n",ylim=c(0,max(gen[,1:(ncol(gen)-1)])),ylab="olume in Florins",xlab="years")
sapply(1:(ncol(gen)-1),function(i)lines(gen[,i] ~ rownames(gen),col=color[i],lwd=3))
legend("top",legend=names(gen)[1:(ncol(gen)-1)],col=color[1:(ncol(gen)-1)],lwd=3)
 barplot(t(gen),space=0,border=NA,col=color,ylab="Fraction of trade (export)",log="y")
dev.off()



pdf("generali5.pdf")
allGen=read.csv("../data/generalli_incomes.csv")
meanGen=tapply(allGen$Value , allGen[c("Year", "Country")],sum)
meanGen[is.na(meanGen)]=0
totGen=apply(meanGen,1,sum)
freqGen=meanGen/totGen
color=brewer.pal(ncol(meanGen), "Set3")
logM=log(meanGen)
logM[logM<=0]=0
#barplot(t(logM/apply(logM,1,sum)),col=color,legend=T)
barplot(t(freqGen),col=color,xlab="Year",ylab="Percent of Holdings")
legend("bottomrigh",legend=colnames(meanGen)[c(1,4,5,7,8,9)],fill=color[c(1,4,5,7,8,9)],bg="white")
dev.off()

