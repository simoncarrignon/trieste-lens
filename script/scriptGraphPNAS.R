source("functionTrieste.R")
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
pdf("importOrder.pdf")
par(mar=c(4,7,1,1))
logAllI=log(subAllI)
##image(t(as.matrix(logAllI)),axes=F,col= colorRampPalette(c("red", "yellow","blue"))(n = 1000))
##image(t(as.matrix(logAllI)),axes=F,col= diverge_hcl(872, c = c(100, 0), l = c(50, 90), power = 1.3))
##image(t(as.matrix(logAllI)),axes=F,col=heat_hcl(872, h = c(0, -100), c = c(40, 80), l = c(75, 40),  power = 1))
image(t(as.matrix(logAllI)),axes=F,col=heat_hcl(870, h = c(0, -100), c = c(40, 80), l = c(75, 40),  power = 1))
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


####PRint a scale for import export
leg_my <- function(dat)
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

