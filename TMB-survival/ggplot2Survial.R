#install.packages("survival")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("limma")
setwd("C:\\Users")

library(limma)
library(survival)
library(survminer)
library(qvalue)
library(ggplot2)

surData=read.table("TMBSurvival.txt",sep="\t",check.names=F,header=T,row.names=1)
surData$futime=surData$futime/365

var="TMB"
diff=survdiff(Surv(futime, fustat) ~Group,data = surData)
# pValue=1-pchisq(diff$chisq,df=1)
# if(pValue<0.001){
# pValue="p<0.001"
# }else{
# pValue=paste0("p=",sprintf("%.03f",pValue))
# }
fit <- survfit(Surv(futime, fustat) ~ Group, data = surData)

SurvivalPlotName<-paste("survival.",".tiff",sep = "",collapse = NULL)
tiff(file=SurvivalPlotName, width=15, height=15,units ="cm", res=600)
ggsurvplot(fit
           , data=surData
           , conf.int=FALSE #是否显示置信区间
           , pval=TRUE #是否显示p值
           , surv.median.line="hv"
           #, title="Overall survival" #改标题，或者注释掉
           , xlab="Time (year)"
           #, ylab=""
           , legend="top" #图例位置在上方
           , legend.title = var #图例的标题
           , legend.labs=c("High", "Low") #具体图例
           , risk.table = TRUE
)
dev.off()

