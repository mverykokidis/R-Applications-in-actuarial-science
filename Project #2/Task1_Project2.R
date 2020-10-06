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



