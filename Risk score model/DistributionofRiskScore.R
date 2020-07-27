#install.packages("pheatmap")
Dataset<-"LIHC"

#setwd("C:/")
rt=read.table("Input.txt",sep="\t",header=T,row.names=1,check.names=F)
rt=rt[order(rt$RSdisc),]
rt1=rt[c(3:(ncol(rt)-2))]
rt1=t(rt1)

#rt1=log2(rt1+1)
library(pheatmap)
annotation=data.frame(Risk=rt[,ncol(rt)])
rownames(annotation)=rownames(rt)

OutputMatrix<-rt1

for (i in 1: nrow(OutputMatrix))
{
	rowi<-as.numeric(OutputMatrix[i,])
	rowmean<-mean(rowi)
	rowsd<-sd(rowi)
	OutputMatrix[i,]<-(rowi-rowmean)/rowsd
}

for (i in 1: nrow(OutputMatrix))
{
	for (j in 1: ncol(OutputMatrix))
	{
		if (OutputMatrix[i,j] > 3)
		{
			OutputMatrix[i,j] <- 3
		}
		if (OutputMatrix[i,j] < -3)
		{
			OutputMatrix[i,j] <- -3
		}		
	}
}

#pdf(file="heatmap.pdf",width = 12,height = 5)
ann_colors=list(Risk= c(High = "Salmon1", Low ="Cyan3" ))

tiff(file=paste0(Dataset, ".", "heatmapless1.tiff"),width = 17,height = 6,units ="cm",compression="lzw",bg="white",res=600)#units ="cm",
heatmap1<-pheatmap(OutputMatrix, 
         #, color=random
		 ,color = colorRampPalette(c("navy", "white", "firebrick3"))(100)
         , annotation_colors = ann_colors
		 , fontsize = 8
		 , annotation=annotation
         , cluster_rows = FALSE
         , show_rownames=TRUE
			#, fontsize_row=10
		 , cluster_cols =FALSE
		 , show_colnames=FALSE
            #, fontsize_col=2     
		#, scale="none"
		, display_numbers = FALSE 
        #, cellwidth = 2
		#, cellheight = 5
        #, color = colorRampPalette(c("green", "black", "red"))(50) 
				   )
dev.off()


riskClass=rt[,"RSdiscRisk"]
lowLength=length(riskClass[riskClass=="Low"])
highLength=length(riskClass[riskClass=="High"])
line=rt[,"RSdisc"]
line[line>10]=10
#pdf(file="riskScore.pdf",width = 12,height = 5)
tiff(file=paste0(Dataset, ".", "riskScore.tiff"),width = 17,height = 8,units ="cm",compression="lzw",bg="white",res=600)#units ="cm",
riskScoreplot<-plot(line,
     type="p",
     pch=20,
	 cex=1, 
     xlab="Patients (increasing risk socre)",
     ylab="Risk Score",
     col=c(rep("green",lowLength),
     rep("red",highLength)))
abline(h=median(rt$riskScore),v=lowLength,lty=2)
dev.off()

color=as.vector(rt$fustat)
color[color==1]="red"
color[color==0]="green"
#pdf(file="survStat.pdf",width = 12,height = 5)
tiff(file=paste0(Dataset, ".", "survStat.tiff"),width = 17,height = 8,units ="cm",compression="lzw",bg="white",res=600)#units ="cm",

survStatplot<-plot(rt$futime,
	 pch=19,
	 cex=1, 
     xlab="Patients (increasing risk socre)",
     ylab="Survival time (years)",
     col=color)
abline(v=lowLength,lty=2)
dev.off()


