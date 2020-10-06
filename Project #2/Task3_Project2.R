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
