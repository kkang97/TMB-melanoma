#install.packages("vioplot")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")

library(vioplot)
library(limma)

pFilter=0.05
setwd("C:\\Users")

immune=read.table("CIBERSORT-Results.txt",sep="\t",header=T,row.names=1,check.names=F)
immune=immune[immune[,"P-value"]<pFilter,]
immune=as.matrix(immune[,1:(ncol(immune)-3)])
rownames(immune)=gsub("(.*?)\\-(.*?)\\-(.*?)\\-(.*?)\\-.*","\\1\\-\\2\\-\\3",rownames(immune))
immune=avereps(immune)

tmb=read.table("TMB.txt",sep="\t",header=T,check.names=F,row.names=1)
tmb=as.matrix(tmb)
row.names(tmb)=gsub("(.*?)\\-(.*?)\\-(.*?)\\-(.*?)\\-.*","\\1\\-\\2\\-\\3",row.names(tmb))
tmb=avereps(tmb)
lowTmb=tmb[tmb[,"TMB"]<=median(tmb[,"TMB"]),]
highTmb=tmb[tmb[,"TMB"]>median(tmb[,"TMB"]),]
lowTmbName=names(lowTmb)
highTmbName=names(highTmb)

lowTmbImm=intersect(row.names(immune),lowTmbName)
highTmbImm=intersect(row.names(immune),highTmbName)
rt=rbind(immune[lowTmbImm,],immune[highTmbImm,])
lowTmbNum=length(lowTmbImm)
highTmbNum=length(highTmbImm)

pdf("vioplot.pdf",height=8,width=13)
par(las=1,mar=c(10,6,3,3))
x=c(1:ncol(rt))
y=c(1:ncol(rt))
plot(x,y,
     xlim=c(0,63),ylim=c(min(rt),max(rt)+0.02),
     main="",xlab="", ylab="Fraction",
     pch=21,
     col="white",
     xaxt="n")

for(i in 1:ncol(rt)){
  if(sd(rt[1:lowTmbNum,i])==0){
    rt[1,i]=0.001
  }
  if(sd(rt[(lowTmbNum+1):(lowTmbNum+highTmbNum),i])==0){
    rt[(lowTmbNum+1),i]=0.001
  }
  lowTmbData=rt[1:lowTmbNum,i]
  highTmbData=rt[(lowTmbNum+1):(lowTmbNum+highTmbNum),i]
  vioplot(lowTmbData,at=3*(i-1),lty=1,add = T,col = 'green')
  vioplot(highTmbData,at=3*(i-1)+1,lty=1,add = T,col = 'red')
  wilcoxTest=wilcox.test(lowTmbData,highTmbData)
  p=wilcoxTest$p.value
  mx=max(c(lowTmbData,highTmbData))
  lines(c(x=3*(i-1)+0.2,x=3*(i-1)+0.8),c(mx,mx))
  text(x=3*(i-1)+0.5, y=mx+0.02, col =ifelse(p<0.05,"red","black") , labels=ifelse(p<0.001, paste0("p<0.001"), paste0("p=",sprintf("%.03f",p))), cex = 0.8)
}
text(seq(1,64,3),-0.05,xpd = NA,labels=colnames(rt),cex = 1,srt = 45,pos=2)
dev.off()
