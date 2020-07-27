setwd("D:\\") 
input="CIBERSORT-Results.txt" 
outpdf="CIBERSORT.barplot.pdf" 
pFilter=0.05

immune=read.table("CIBERSORT-Results.txt",sep="\t",header=T,row.names=1,check.names=F)
immune=immune[immune[,"P-value"]<pFilter,]
immune=as.matrix(immune[,1:(ncol(immune)-3)])
data=t(immune)
col=rainbow(nrow(data),s=0.7,v=0.7)

pdf(outpdf,height=10,width=22)   
par(las=1,
    mar=c(8,5,4,16),
	mgp=c(3,0.1,0),
	cex.axis=1.5
	)
a1 = barplot(data,
             col=col,
			 yaxt="n",
			 ylab="Relative Percent",
			 xaxt="n",
			 cex.lab=1.8
			 )
a2=axis(2,tick=F,labels=F)
axis(2,a2,paste0(a2*100,"%"))
axis(1,a1,labels=F)
par(srt=60,xpd=T);text(a1,-0.02,colnames(data),adj=1,cex=0.6);par(srt=0)
ytick2 = cumsum(data[,ncol(data)])
ytick1 = c(0,ytick2[-length(ytick2)])
legend(par('usr')[2]*0.98,par('usr')[4],legend=rownames(data),col=col,pch=15,bty="n",cex=1.3)
dev.off()

tiff(file="CIBERSORT.barplot.tiff",width = 35,height = 30,units ="cm",compression="lzw",bg="white",res=600)
par(las=1,
    mar=c(8,5,4,16),
	mgp=c(3,0.1,0),
	cex.axis=1.5
	)
a1 = barplot(data,
             col=col,
			 yaxt="n",
			 ylab="Relative Percent",
			 xaxt="n",
			 cex.lab=1.8
			 )
a2=axis(2,tick=F,labels=F)
axis(2,a2,paste0(a2*100,"%"))
axis(1,a1,labels=F)
par(srt=60,xpd=T);text(a1,-0.02,colnames(data),adj=1,cex=0.6);par(srt=0)
ytick2 = cumsum(data[,ncol(data)])
ytick1 = c(0,ytick2[-length(ytick2)])
legend(par('usr')[2]*0.98,par('usr')[4],legend=rownames(data),col=col,pch=15,bty="n",cex=1.3)
dev.off()
