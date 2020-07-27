#install.packages('survival')
#install.packages("survminer")

library(survival)
library(survminer)
setwd("C:\\Users")
rt=read.table("indepInput.txt",header=T,sep="\t",check.names=F,row.names=1)

multiCox=coxph(Surv(futime, fustat) ~ ., data = rt)
multiCoxSum=summary(multiCox)

outTab=data.frame()
outTab=cbind(
             HR=multiCoxSum$conf.int[,"exp(coef)"],
             HR.95L=multiCoxSum$conf.int[,"lower .95"],
             HR.95H=multiCoxSum$conf.int[,"upper .95"],
             pvalue=multiCoxSum$coefficients[,"Pr(>|z|)"])
outTab=cbind(id=row.names(outTab),outTab)
write.table(outTab,file="multiCox.xls",sep="\t",row.names=F,quote=F)

pdf(file="forest.pdf",
       width = 8,            
       height = 5,           
       )
ggforest(multiCox,
         main = "Hazard ratio",
         cpositions = c(0.02,0.22, 0.4), 
         fontsize = 0.7, 
         refLabel = "reference", 
         noDigits = 2)
dev.off()