ASKISI_2

ERWTHSH_01
y<-read.table("data.txt",header=TRUE)
names(y)
attach(y)
library(fitdistrplus)
weiMLE<-fitdist(x,"weibull")
summary(weiMLE) #AIC

lnoMLE<-fitdist(x,"lnorm")
summary(lnoMLE)

logMLE<-fitdist(x,"logis")
summary(logMLE)

library(actuar)

lloMLE<-fitdist(x,"llogis")
summary(lloMLE)

invMLE<-fitdist(x,"invgamma")
summary(invMLE)

gamMLE<-fitdist(x,"gamma")
summary(gamMLE)


paretMLE<-fitdist(x,"pareto")
summary(paretMLE) #AIC


ERWTHSH_02
hist(x,prob=TRUE,xlim=c(0,500),ylim=c(0,0.02))
u<-seq(0,500)
lines(u,dweibull(u,shape=weiMLE$estimate[1],scale=weiMLE$estimate[2]),col="green",lwd=0.8)

hist(x,prob=TRUE,xlim=c(0,500),ylim=c(0,0.02))
lines(u,dpareto(u,shape=paretMLE$estimate[1],scale=paretMLE$estimate[2]),col="red",lwd=0.8)

ERWTHSH_03
qqcomp(list(weiMLE,paretMLE),fitcol="black",main="QQ-plot on X",legendtext=c("Weibull","Pareto"),fitpch=1:4)




