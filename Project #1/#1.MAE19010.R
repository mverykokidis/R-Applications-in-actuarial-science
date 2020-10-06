!ASKISI 1
v<-c(73,6,77,81,91,101,135,61,65,68,18,20,23,12,14,18,23,26,26,27,2,3,3,40,41,41,6,8,11,12,37,38,38,6,73,6,51)

I.
mean(v)
median(v)
max(v)-min(v)
var(v)
as<-sqrt(var(v))^(-3)*(mean((v[]-mean(v))^3)); as
quantile(v,0.25) ; quantile(v,0.75)

II.
boxplot(v,main="Explosions in Hawain from 1830 to 19550",col=7,horizontal=T) 
qqnorm(v) ; qqline(v) 

III.
library(nortest)
lillie.test(v)
shapiro.test(v)

IV.
class<-seq(0,160,20)
hist(v,breaks=class,prob=TRUE)
a<-1.3
b<-28.07
k<-rgamma(v,shape=a,scale=b);k
lines(density(k),col="green")


V.
l<-0.028
x1<-qexp(ppoints(v),l)
qqplot(x1,v,main="Exponential Distribution with rate=0.028")
a1<-qexp(0.25,l)
b1<-quantile(v,0.25)
a2<-qexp(0.75,l)
b2<-quantile(v,0.75)
abline(b1-a1*((b2-b1)/(a2-a1)),(b2-b1)/(a2-a1))

ks.test(v,"pexp",rate=l,exact=TRUE)
ks.test(v,"pexp",rate=l,exact=FALSE)

VI.
wilcox.test(v,mu=20,exact=FALSE,correct=FALSE)
library(BSDA)
SIGN.test(v,md=20)




!ASKISI 2

p<-0.1
n<-20
k<-0:n
pM0<-0.05

Αi.
f<-dgeom(k,prob=p) ; f
cT<-1/(1-f[1]) ; pT<-c(0,cT*f[-1]); pT

Aii.
cM<-(1-pM0)/(1-f[1]);pM<-c(pM0,cM*f[-1]);pM

m<-cbind(k,pT,pM,f)
rownames(m)<-rep("",nrow(m))
colnames(m)<-c("k","Truncated Geometric","Modified Geometric","Geometric")
round(m,digits=7)
x<-seq(0,20,1)
plot(x,xlab="x",ylab="f(x)",xlim=c(0,20),ylim=c(0,0.1),main="Geometric,Truncated and Modified Geometric,p=0.1,pM0=0.05")
points(pT,pch=3)
points(pM,pch=4)
points(f)
legend(15,0.08,c("Truncated","Modified","Geometric"),pch=c(3,4,1))

B
fx1<-pM
fx2<-convolve(fx1,rev(fx1),type="o")
fx3<-convolve(fx1,rev(fx2),type="o")
fx4<-convolve(fx1,rev(fx3),type="o")
fx5<-convolve(fx1,rev(fx4),type="o")
fx6<-convolve(fx1,rev(fx5),type="o")
round(fx6,digits=7)
sum(fx6[1:7])




!ASKISI 3
a<-read.table("Askisi3.txt",header=TRUE) ; a
attach(a)

i.
lm(Y~X1+X2+X3)

ii.
m<-lm(Y~X1+X2+X3)
summary(m)


iii.
!X2 MONO SIMANTIKI 
SIGRINOUME ME STILI PR(>|T|)

iv.
confint(m,level=0.99)

v.
SST<-sum((Y-mean(Y))^2) ;SST
SSR<-sum((predict(m)-mean(Y))^2);SSR
SSE<-SST-SSR;SSE


vi.
N<-20
P<-3
S2<-SSE/(N-P-1) ;S2

vii.
predict(m,newdata=data.frame(X1=75,X2=70,X3=29.5),interval="confidence",level=0.90)

viii.
library(MASS)
mod1<-stepAIC(m,direction="both",scope=(~X3+X2+X1),k=2)
mod1<-stepAIC(m,direction="both",scope=(~X3+X2+X1),k=log(20))


