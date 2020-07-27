#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")
setwd("D:\\")                      

options(stringsAsFactors=F)
library(limma)

rt<-read.table("TMB.Clinic.txt", ,sep="\t",header=T,check.names=F,row.names=1)
gene=colnames(rt)[1]        

library(beeswarm)

for(clinical in colnames(rt[,2:ncol(rt)]))
{
  data=rt[,c(gene,clinical)]
  colnames(data)=c("expression","clinical")
  data=data[(data[,"clinical"]!="Unknow"),]
  tab1=table(data[,"clinical"])
  labelNum=length(tab1)
  if(labelNum==2){
    cliTest<-wilcox.test(expression ~ clinical, data=data)
  }else{
    cliTest<-kruskal.test(expression ~ clinical, data = data)}
  pValue=cliTest$p.value
  xlabel=vector()
  #dotCol=c(2,3)
  dotCol=c("blue","red")
  if(labelNum==3){dotCol=c(2,3,4)}
  if(labelNum==4){dotCol=c(2,3,4,5)}
  if(labelNum>4){dotCol=rainbow(labelNum)}
  for(j in 1:labelNum){xlabel=c(xlabel,names(tab1[j]))}
  pval=ifelse(pValue<0.001,"<0.001",paste0("=",sprintf("%.03f",pValue)))
  b = boxplot(expression ~ clinical, data = data,outline = FALSE, plot=F)
  yMin=min(b$stats);yMax = max(b$stats/5+b$stats)
  ySeg = max(b$stats/10+b$stats);ySeg2 = max(b$stats/12+b$stats)
  n = ncol(b$stats)
  outPdf=paste(pValue,"-",clinical, ".pdf", sep = "")
  pdf(file=outPdf, width=15/2.54, height=15/2.54)
  par(mar = c(4.5,6,3,3))
  boxplot(expression ~ clinical, data = data,names=xlabel,#col=dotCol,
          ylab = "TMB",main=clinical,xlab="",
          cex.main=1.3, cex.lab=1.2, cex.axis=1.1,ylim=c(yMin,yMax),outline = FALSE)
  beeswarm(expression ~ clinical, data = data, col =dotCol, lwd=0.1,
		         pch = 16, add = TRUE, corral="wrap")		  
  segments(1,ySeg, n,ySeg);segments(1,ySeg, 1,ySeg2);segments(n,ySeg, n,ySeg2)
  text((1+n)/2,ySeg,labels=paste0("P",pval),cex=1.2,pos=3)
  dev.off()
}
