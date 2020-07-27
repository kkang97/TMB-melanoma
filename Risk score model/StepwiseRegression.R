abc<-read.table("Input.txt" , header = T , sep = "\t" , check.names = F)
names(abc)

for ( i in 3:length(abc))
{
  abc[,i]<-factor(abc[,i],levels =c(0,1), labels = c("Low","High"))
}

#####stepwise model selection####
library(survival)
library(MASS)  ###you can use this package to do model selection, stepAIC(model,derection="forward,backward")###

y<-Surv(abc$futime , abc$fustat) 
model=coxph(y~ASPA +STRIP2 +MYBL2 +ATP1B2 +PIRT +SALL4 +NTN1,
            data=abc)
summary(model)

step.model <- step(model,direction="backward")
summary(step.model)

cox.zph(step.model,transform=rank)

y2<-Surv(abc$futime , abc$fustat) 
model2=coxph(y2~GFRA2 + ASPA + CARTPT,
            data=abc)
summary(model2)

step.model2 <- step(model2,direction="backward")
summary(step.model2)
cox.zph(step.model2,transform=rank)





