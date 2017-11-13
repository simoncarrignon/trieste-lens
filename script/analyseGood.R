allGood= read.csv("../fulldat.csv")
allGood.import = read.csv("../sampleEdit/1866_fulldat-imp.csv")

cent.import=allGood.import[grep("[cg].nt",as.character(allGood.import$unit)),]  ##comprar only good counted in the same unit. 
gsum.import=tapply(cent.import$quantity, cent.import$type,sum)
a.import=hist(log(gsum.import),breaks=10)
 plot(a$counts ~ exp(a$breaks[2:15])) 

allGood.export = read.csv("../fulldat.csv")

cent.export=allGood.export[grep("[cg].nt",as.character(allGood.export$unit)),]  ##comprar only good counted in the same unit. 
gsum.export=tapply(cent.export$quantity, cent.export$type,sum)
a.export=hist(log(gsum.export),breaks=10)
 plot(a.export$counts ~ exp(a.export$breaks[2:15]),log="y",xlab="Volume in centi") 
 points(a.import$counts ~ exp(a.import$breaks[2:15]),col="red") 

allGood.export76 = read.csv("../sampleEdit/1876_fulldat-exp.csv")

cent.export76=allGood.export76[grep("quint*",as.character(allGood.export76$unit)),]  ##comprar only good counted in the same unit. 
gsum.export76=tapply(cent.export76$quantity, cent.export76$type,sum)
a.export76=hist(log(gsum.export76),breaks=10)



tofilefolder="~/projects/PhD/doc/thesis/201709_presentation/images/"
pdf(file.path(tofilefolder,"distribgood76.pdf"))
plot(a.export76$counts ~ exp(a.export76$breaks[2:12]),log="yx",xlab="Volume in quintal",main="Distribution of volume of good year 1976") 
text(4.15*100000000,2.2,"steal & cardamomo")
text(1.15*10000000,5.2,"lavorato?,\ndry fruits,\nstraw and hay,\nchocolate...",adj=c(0,0)) 
dev.off()

pdf(file.path(tofilefolder,"distribgood66.pdf"))
plot(a.export$counts ~ exp(a.export$breaks[2:15]),log="yx",xlab="Volume in centi",ylab="Number of type of goods",main="Dsitribution of volume of good year 1966")
text(1200000,1.1,"flour")
text(1.15*100000,5.2,"alcool and wine,\n\nsugare,...",adj=c(0,0)) 
dev.off()

                 farina     acquavite e spiriti                   carta
                 471006                  114493                   68012
		      zucchero raffinato                 butirro lavori da scarpellino w
		                   64607                   62739                   45464
		              i sommacco    ferro greggio e fuso                 purgato
			                        44412                   43592                   38315
			                      acciajo                    riso                   birra
					                        36625                   28956                   28133
					          lpietre da fabbrica
						                    26358

