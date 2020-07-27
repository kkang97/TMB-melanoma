#install.packages('survival')
#source("https://bioconductor.org/biocLite.R")
#biocLite("qvalue")

setwd("C:\\Users")

options(stringsAsFactors=FALSE)
library(survival)
library(survminer)
outTab=data.frame()

picDir="picture"
dir.create(picDir)

library(survival)
library(qvalue)
rt=read.table("ExpTimeImmune.txt",#输入文件名
              header=T,sep="\t",row.names=1,check.names=F)
rt[,"futime"]=rt[,"futime"]/365



## 寻找最佳点和分类
sur.cut <- surv_cutpoint(rt, time="futime", event="fustat",
                         variables=colnames(rt[,3:ncol(rt)])
)
summary(sur.cut)
labels <- paste(c(">", "<="),
                rep(summary(sur.cut)[,"cutpoint"], each=2), sep="")
for(i in colnames(rt[,3:ncol(rt)])){
  #  outfile <- paste("picture/cox_cutoff_", i, ".jpg", sep="")
  #  jpeg(outfile, width=20, height=20, units="cm", res=350)
  #  print(plot(sur.cut, i, palette="npg"))
  #  dev.off()
  
  outPdf=paste("picture/cox_cutoff", i,".pdf", sep="")
  pdf(file=outPdf, onefile=FALSE)
  print(plot(sur.cut, i, palette="npg"))
  dev.off()
}
oldnames <- colnames(rt)
colnames(rt) <- gsub("-", ".", colnames(rt))
sur.cut <- surv_cutpoint(rt, time="futime", event="fustat",
                         variables=colnames(rt[,3:ncol(rt)])
)
summary(sur.cut)
sink("cutoff.unreadable.txt")
sur.cut
sink()
#End

rt <- surv_categorize(sur.cut, variables=colnames(rt[,3:ncol(rt)]))
head(rt)
colnames(rt) <- oldnames



outlst <- list()
for(i in colnames(rt[,3:ncol(rt)])){
  cox <- coxph(Surv(futime, fustat) ~ rt[, i], data=rt)
  coxSummary = summary(cox)
  coxP=coxSummary$coefficients[,"Pr(>|z|)"]
  
  rt1=rt[rt[, i] == "high",]
  rt2=rt[rt[, i] == "low",]
  n1=nrow(rt1)
  n2=nrow(rt2)
  surTab1=summary(survfit(Surv(futime, fustat) ~ 1, data = rt1))
  surTab2=summary(survfit(Surv(futime, fustat) ~ 1, data = rt2))
  medianTab1=surTab1$table
  medianTab2=surTab2$table
  
  model <- survdiff(Surv(futime, fustat) ~ rt[, i], data = rt)
  chisq <- model[["chisq"]]
  df <- length(model[["n"]]) - 1
  pvalue <- pchisq(chisq, df, lower.tail=FALSE)
  
  label <- labels[rep(colnames(rt[,3:ncol(rt)]), each=2) == i]
  outlst[[i]] <- cbind(
    data.frame(var=i, level=c("high", "low"), label=label),
    rbind(medianTab1, medianTab2), pvalue=pvalue)
  
  diff=survdiff(Surv(futime, fustat) ~ rt[,i],data = rt)
  fit <- survfit(Surv(futime, fustat) ~ rt[,i], data = rt)
  pValue=1-pchisq(diff$chisq, df=1)
  outTab=rbind(outTab,cbind(gene=i,coxSummary$coefficients,coxSummary$conf.int,KM=pValue,
                            H_med=medianTab1["median"],H_0.95LCL=medianTab1["0.95LCL"],H_0.95UCL=medianTab1["0.95UCL"],
                            L_med=medianTab2["median"],L_0.95LCL=medianTab2["0.95LCL"],L_0.95UCL=medianTab2["0.95UCL"]))
  pval=0
  if(pValue<0.05){
    pval=signif(pValue,4)
    pval=format(pval, scientific = TRUE)
  }else{
    pval=round(pValue,3)
  }
  if(pValue<0.05){
    fig <- ggsurvplot(fit, data=rt, conf.int=FALSE, pval=TRUE,risk.table=FALSE,
                      legend.title=i, legend.labs=label, 
                      ggtheme=theme(
                        legend.key=element_rect(fill="white", colour=NA),
                        panel.border=element_rect(fill="transparent", colour="black"),
                        panel.background=element_rect(fill="white") #,
                        #panel.grid.major=element_line(colour="grey"),
                        #panel.grid.minor=element_line(colour="grey")
                      ))
    
    geneName=unlist(strsplit(i,"\\|"))[1]
    tiffFile=paste(geneName,".survival.tiff",sep="")
    outTiff=paste(picDir,tiffFile,sep="\\")
    tiff(file=outTiff,width = 15,height = 15,units ="cm",compression="lzw",bg="white",res=600)
    #plot(fit, col=c("blue","red"), xlab="Time (years)", ylab="Overall survival",
    #     main=paste(geneName,"(p=",pval, ")", sep=""),mark.time=T,ylim=c(0,1.1),
    #     lwd = 2, cex.main=1.3, cex.lab=1.2, cex.axis=1.2, font=1.2)
    #legend("topright", c(paste("Low expression"), 
    #       paste("High expression")), 
    #       col=c("blue","red"), bty="n", lwd = 2, cex=0.8)
    print(fig)
    dev.off()
    
    outPdf=paste("picture/", geneName,".survival.pdf",sep="")
    pdf(file=outPdf, onefile=FALSE)
    print(fig)
    dev.off()
  }
}
outdf <- do.call(rbind, outlst)
write.csv(outdf, "surv_median.csv", row.names=FALSE, na="")