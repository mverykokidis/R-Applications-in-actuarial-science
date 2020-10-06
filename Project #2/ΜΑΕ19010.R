!ASKISI_01

library(actuar)

probability<-1-1e-15
lim <- floor(qgeom(probability,prob=1/3)) ;lim

fN<-dgeom(0:85,prob=1/3)
sum(fN)

lim2<-floor(qgeom(probability,prob=1/4)) ;lim2
fX<-dgeom(0:120,prob=1/4)
sum(fX)

Gs.conv<-aggregateDist("convolution",model.freq=fN,model.sev=fX)
Gs.conv

gs.conv<-diff(Gs.conv) ; gs.conv

Gs.rec<-aggregateDist("recursive",model.freq="geometric",model.sev=fX,prob=1/3)
Gs.rec

gs.rec<-diff(Gs.rec) ;gs.rec

set.seed(19010)
mfreq<-expression(data=rgeom(1/3))
msev<-expression(data=rgeom(prob=1/4))
Gs.simul<-aggregateDist("simulation",nb.simul=1000000,mfreq,msev)
Gs.simul

gs.simul<-diff(Gs.simul) ; gs.simul

c<-20
x<-seq(0,c)
mat<-cbind(x,Gs.conv(x),Gs.rec(x),Gs.simul(x))
colnames(mat)<-c("x","G(x)_Convolution","G(x)_Recursive","G(x)_Simulation")
rownames(mat)<-rep("",nrow(mat))
mat

d<-c+1
matg<-cbind(x,gs.conv[1:d],gs.rec[1:d],gs.simul[1:d])
rownames(matg)<-rep("",nrow(mat))
colnames(matg)<-c("x","g(x)_Convolution","g(x)_Recursive","g(x)_Simulation")
matg

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

ASKHSH_3

library(actuar)
h<-0.001
fx<-discretize(punif(x,min=0,max=50),method="unbiased",lev=levunif(x,min=0,max=50),from=0,to=50,step=h)
length(fx)

probability<- 1-1e-15
n<-3 ; p<-0.6
lim<- floor(qbinom(probability,size=n,prob=p))
lim
fN<-dbinom(0:3,size=n,prob=p)
sum(fN)
Gs.conv<-aggregateDist("convolution",model.freq=fN,model.sev=fx,x.scale=h)
Gs.conv

Gs.rec<-aggregateDist("recursive",model.freq="binom",model.sev=fx,size=3,prob=0.6,x.scale=0.001,maxit=1000000)
Gs.rec

set.seed(19010)
mfreq<-expression(data=rbinom(n,p))
msev<- expression(data=runif(min=0,max=50))
Gs.simul<- aggregateDist("simulation",nb.simul=1000000,mfreq,msev)
Gs.simul

x<-seq(0,150,10)
mat2<-cbind(x,Gs.conv(x),Gs.rec(x),Gs.simul(x))
colnames(mat2)<-c("x","G(x)_Convolution","G(x)_Recursive","G(x)_Simulation")
rownames(mat2)<-rep("",nrow(mat2))
mat2
