#install.packages("survival")
setwd("C:\\Users")

library(survival)
rt=read.table("lassoInput.txt",header=T,sep="\t",check.names=F,row.names=1)
rt[,3:ncol(rt)]=log2(rt[,3:ncol(rt)]+1)
rt[,"futime"]=rt[,"futime"]/365

dim(rt)

rt <- subset(rt, futime > 0)
dim(rt)

library(survival)
library(glmnet)
set.seed(1000)
x <- as.matrix(rt[,3:ncol(rt)])

y <- Surv(rt$futime, rt$fustat)

model <- glmnet(x, y, family="cox", alpha=1)

pdf("lasso01.pdf")
plot(model, xvar = "lambda", label = TRUE)
dev.off()

cv.model <- cv.glmnet(x, y, family="cox", alpha=1, nfolds=10)

pdf("lasso02.pdf")
plot(cv.model)
dev.off()

pdf("lasso03.pdf")
plot(cv.model$glmnet.fit, xvar="lambda", label=TRUE)
dev.off()

cv.model$lambda.min
cv.model$lambda.1se
sink("cvmodelLambdamin1se.txt")
print("cv.model$lambda.min")
cv.model$lambda.min
print("cv.model$lambda.1se")
cv.model$lambda.1se
sink()

library(boot)
lambda_boot <- function(data, indices) {
    d <- data[indices,]
    x <- as.matrix(rt[,3:ncol(d)])
    y <- Surv(d$futime, d$fustat)
    cv.model <- cv.glmnet(x, y, family="cox", alpha=1, nfolds=10)
    return(cv.model$lambda.1se)
}

library(boot)
set.seed(1000)

bootcorr <- boot(data=rt, statistic=lambda_boot, R=1000)

bootcorr
summary(bootcorr)
#bootcorr$t
pdf("lasso-bootcorr.pdf")
plot(bootcorr)
dev.off()
pdf("lasso-bootcorr[[t]].pdf")
plot(bootcorr[["t"]])
dev.off()

lambda.1se <- bootcorr[["t"]]

x<-coef(cv.model, s = c(lambda.1se[,1]))

ad <- as.matrix(x)