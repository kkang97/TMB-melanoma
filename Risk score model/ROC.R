#install.packages("survival")
#install.packages("survivalROC")

setwd("C:\\Users")       
library(survival)
library(survivalROC)


rt=read.table("RSROCInput.txt",header=T,sep="\t")
rt <- subset(rt, futime > 0)
#rt[,2]<-rt[,2]/365

diff=survdiff(Surv(futime, fustat) ~Risk,data = rt)
pValue=1-pchisq(diff$chisq,df=1)
if(pValue<0.001){
  pValue="p<0.001"
}else{
  pValue=paste0("p=",sprintf("%.03f",pValue))
}
fit <- survfit(Surv(futime, fustat) ~ Risk, data = rt)

ROCNum<-c()
YearNumSet<-c(0.5,1,2,3,4,5,6,7,8,9,10)
for (i in YearNumSet)
{
  YearNum<-i
  pdfName<-paste("ROC.", YearNum, "years.surv_cutpoint.pdf", sep="")
  pdf(file=pdfName,width=6,height=6)
  par(oma=c(0.5,1,0,1),font.lab=1.5,font.axis=1.5)
  roc=survivalROC(Stime=rt$futime, 
                  status=rt$fustat, 
                  marker = rt$RSdisc,
                  predict.time =YearNum,
                  method="KM"
  )
  plot(roc$FP, 
       roc$TP, 
       type="l", 
       xlim=c(0,1), 
       ylim=c(0,1),
       col='red', 
       xlab="False positive rate", 
       ylab="True positive rate",
       main=paste(YearNum, "-year-ROC curve (", "AUC = ",sprintf("%.3f",roc$AUC),")"),
       lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2
  )
  abline(0,1)
  dev.off()
  ROCNum<-c(ROCNum, roc$AUC)
}
OutputAUCFile<-cbind(YearNumSet, ROCNum)
write.table(x=OutputAUCFile, file="AUCs.txt", sep="\t", row.names=TRUE, col.names=NA, quote=FALSE)


