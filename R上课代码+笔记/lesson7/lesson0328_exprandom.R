#利用均匀分布生成指数分布随机数
myexpr=function(n,lambda){
  u=runif(n)
  x=-log(1-u)/lambda
  return(x)
}
par(mfrow=c(1,2))
x1=myexpr(1000,3)
xlim=c(0,3)
ylim=c(0,400)
hist(x1,xlim=xlim,ylim=ylim)
#利用内置函数生成指数分布随机数
x2=rexp(1000,3)
hist(x2,xlim=xlim,ylim=ylim)