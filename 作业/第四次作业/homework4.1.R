#N(0,1)生成0.3N(0,1)+0.7N(2,2)
mix=function(x){
  out=0.3*dnorm(x)+0.7*dnorm(x,mean=2,sd=sqrt(2))
  return(out)
}
#确定m的取值
x=seq(-2,1.5,0.1)
f=mix(x)
f1=dnorm(x)
m0=f/f1
m=max(m0)
#生成随机数
n=1000
u=runif(n)
v=rnorm(n)
y=c()
accp=0
for(i in 1:n){
  temp=(1/m)*(mix(v[i]))/dnorm(v[i])
  if(u[i]<temp){
    y=c(y,v[i])
    accp=accp+1
  }
}
percentage=accp/n
print(percentage)
hist(y,prob=TRUE,main='random numbers of mix distribution')
#lines(density(y),col='red',bw='nrd')