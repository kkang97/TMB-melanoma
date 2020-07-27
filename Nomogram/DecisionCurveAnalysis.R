source("dca.R")
source("stdca.R")
library(survival)

fit <- coxph(Surv(futime, fustat) ~ RiskScore+Stage, data=abc)
abc$pr_failure <- c(1 - (summary(survfit(fit, newdata=abc), times=1)$surv)) #times=1表示1年
km1 <- stdca(data=abc, outcome="fustat", ttoutcome="futime", timepoint=1,
             predictors="pr_failure", xstop=0.75, smooth=TRUE)

fit <- coxph(Surv(futime, fustat) ~ RiskScore, data=abc)
abc$pr_failure <- c(1 - (summary(survfit(fit, newdata=abc), times=1)$surv)) 
km2 <- stdca(data=abc, outcome="fustat", ttoutcome="futime", timepoint=1,
             predictors="pr_failure", xstop=0.75, smooth=TRUE)

fit <- coxph(Surv(futime, fustat) ~ Stage, data=abc)
abc$pr_failure <- c(1 - (summary(survfit(fit, newdata=abc), times=1)$surv)) 
km3 <- stdca(data=abc, outcome="fustat", ttoutcome="futime", timepoint=1,
             predictors="pr_failure", xstop=0.75, smooth=TRUE)

library(RColorBrewer)
mycolor<-brewer.pal(8,"Set1")
mycolor

pdf ("DCA-1y.pdf")
plot(km1$net.benefit$threshold, km1$net.benefit$none, cex.axis=1.2,cex.lab=1.2, main= "1-Year DCA curves",
     type = "l", lwd=2.5, col=8,
     xlim=c(0,0.6), ylim=c(-0.1, 0.3),
     xlab = "Threshold Probability",
     ylab = "Net Benefit")
lines(km1$net.benefit$threshold, km1$net.benefit$all, type="l", lty=2, col=1,lwd=2.5)
lines(km1$net.benefit$threshold, km1$net.benefit$pr_failure, col=mycolor[1],lwd=4)
lines(km1$net.benefit$threshold, km2$net.benefit$pr_failure, type="l", col=mycolor[2],lwd=3)
lines(km1$net.benefit$threshold, km3$net.benefit$pr_failure, type="l", col=mycolor[3],lwd=3)

legend("topright",inset=0.001, cex=1, legend=c("None","All","Combined","Risk Score","Stage"),
       text.col = c(8,1,mycolor[1:3]),
       text.font = 2,
       lty=c(1,2,1,1,1), lwd=2, col=c(8,1,mycolor[1:3]),box.col="transparent")
dev.off()
#End of DCA-1y


