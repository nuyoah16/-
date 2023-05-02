#生成服从f(x)=0.3N(0,1)+0.7N(2,2)的随机数
#混合正态分布的概率密度曲线
x=seq(-5,5,0.01)
f=0.3*dnorm(x)+0.7*dnorm(x,mean=2,sd=2)
xlim=c(-5,5)
par(mfrow=c(3,1))
plot(x,f,type='l',lwd=3,col='red',xlim=xlim,main='f(x)=0.3N(0,1)+0.7N(2,2)') 
#随机数的采样
n=1000
u=runif(n)
v1=rnorm(n)
v2=rnorm(n,mean=2,sd=2)
y=matrix(0,1,n)#为生成的随机数
for(i in 1:n){
  if(u[i]<0.3){
    y[i]=v1[i]
  }else{
    y[i]=v2[i] 
  }
}
hist(y,xlim=xlim,main='random numbers of indirect sampling')
#R中代码生成
dist=c(rep(1,3),rep(2,7)) 
x1=rnorm(n,c(0,2)[sample(dist,n,replace=TRUE)],sd=c(1,2)[sample(dist,n,replace=TRUE)])
hist(x1,xlim=xlim,main='random numbers of R-auto' )