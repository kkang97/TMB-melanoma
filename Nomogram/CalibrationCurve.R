library(rms)
setwd("C:\\Users\\86181\\Desktop\\KK\\67ICGCexp-449vTMBimmune\\22.Nomogram\\verify")
rt=read.table("input.txt",sep="\t",header=T,row.names=1,check.names=F)

dd <- datadist(rt)
options(datadist="dd")

f <- cph(Surv(futime, fustat) ~ ., x=T, y=T, surv=T, data=rt, time.inc=365)
surv <- Survival(f)

nom <- nomogram(f, fun=list(function(x) surv(365, x), function(x) surv(730, x), function(x) surv(1095, x)), 
                lp=F, funlabel=c("1-year survival", "2-year survival", "3-year survival"), 
                maxscale=100, 
                fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,0.2,0.1,0.05))  

pdf(file="nomogram.pdf",height=6,width=10)
plot(nom)
dev.off()

sum.surv <- summary(coxph(Surv(futime, fustat) ~ .,data = rt))
c_index <-sum.surv$concordance
c_index

f1 <- cph(Surv(futime, fustat) ~., x=T, y=T, surv=T, data=rt, time.inc=365)
cal1 <- calibrate(f1, cmethod="KM", method="boot", u=365, m=96,B=1000)
pdf(file="1-year CalCurve.pdf", height = 6, width = 10)
plot(cal1)
plot(cal1,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0.88,1),ylim=c(0.75,1.0),
     xlab="Nomogram-Predicted Probabilityof 1-Year Survival",ylab="Actual 1-Year Survival (proportion)",
     col=c(rgb(192,98,83,maxColorValue=255)))
lines(cal1[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))
dev.off()

f2 <- cph(Surv(futime, fustat) ~., x=T, y=T, surv=T, data=rt, time.inc=730)
cal2 <- calibrate(f2, cmethod="KM", method="boot", u=730, m=96,B=1000)
pdf(file="2-year CalCurve.pdf", height = 6, width = 10)
plot(cal2)
plot(cal2,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0.6,0.9),ylim=c(0.5,1.0),
     xlab="Nomogram-Predicted Probabilityof 2-Year Survival",ylab="Actual 2-Year Survival (proportion)",
     col=c(rgb(192,98,83,maxColorValue=255)))
lines(cal2[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))
dev.off()

f3 <- cph(Surv(futime, fustat) ~., x=T, y=T, surv=T, data=rt, time.inc=1095)
cal3 <- calibrate(f3, cmethod="KM", method="boot", u=1095, m=96, B=1000)
pdf(file="3-year CalCurve.pdf", height = 6, width = 10)
plot(cal3)
plot(cal3,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0.4,0.85),ylim=c(0.3,1.0),
     xlab="Nomogram-Predicted Probabilityof 3-Year Survival",ylab="Actual 3-Year Survival (proportion)",
     col=c(rgb(192,98,83,maxColorValue=255)))
lines(cal3[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))
dev.off()

f5 <- cph(Surv(futime, fustat) ~., x=T, y=T, surv=T, data=rt, time.inc=1825)
cal5 <- calibrate(f5, cmethod="KM", method="boot", u=1825, m=73, B=1000)
pdf(file="5-year CalCurve.pdf", height = 6, width = 10)
plot(cal5)
plot(cal5,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0.2,0.80),ylim=c(0.2,0.9),
     xlab="Nomogram-Predicted Probabilityof 5-Year Survival",ylab="Actual 5-Year Survival (proportion)",
     col=c(rgb(192,98,83,maxColorValue=255)))
lines(cal5[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))
dev.off()

f10 <- cph(Surv(futime, fustat) ~., x=T, y=T, surv=T, data=rt, time.inc=3650)
cal10 <- calibrate(f10, cmethod="KM", method="boot", u=3650, m=96, B=1000)
pdf(file="10-year CalCurve.pdf", height = 6, width = 10)
plot(cal10)
plot(cal10,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0.0,0.60),ylim=c(0.0,0.7),
     xlab="Nomogram-Predicted Probabilityof 10-Year Survival",ylab="Actual 10-Year Survival (proportion)",
     col=c(rgb(192,98,83,maxColorValue=255)))
lines(cal10[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))
dev.off()