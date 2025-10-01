install.packages("glmulti")
install.packages("metafor")
library(metafor)
library(ggplot2)
library(glmulti)

d1 <- read.csv("Metayield.csv")


d2<-escalc(measure="ROM",data=d1,m1i=Yield2,sd1i=Yield_SD2,n1i=n2,m2i=Yield1,sd2i=Yield_SD1,n2i=n1)
random1<-rma(yi,vi, data=d2, method="REML")
summary(random1)
r2<-rma(yi,vi, mods=~SOC,data=d2, method="REML")
summary(r2)

preds <- predict(r2, newmods=c(0:100))

wi<- 1/sqrt(d2$vi)
size  <-0.8 + 0.4 * (wi - min(wi))/(max(wi) - min(wi))

plot(d2$SOC, d2$yi, main="Yield",pch=1, cex=1.5, col="dodgerblue4",ylab="",
     xlab="SOC",
     las=1, bty="or", font=2,cex.lab=1.0,ylim=c(-1,1.5),xlim=c(0,50))
lines(0:100, preds$pred)
lines(0:100, preds$ci.lb, lty="dashed")
lines(0:100, preds$ci.ub, lty="dashed")
abline(h=0, lty="dotted",col="red",lwd=2)
title(ylab="Effect size(log resposne ratio)", line=2.3, cex.lab=1.5)
text(c(30), 1.0, c("Qm=0.05, P=0.83"),cex=1.0,font=1,col="red")
