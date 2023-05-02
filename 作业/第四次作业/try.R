x=seq(-5,5,0.01)
f=0.3*dnorm(x)+0.7*dnorm(x,mean=2,sd=2)
xlim=c(-5,5)
ylim=c(0,0.7)
#par(mfrow=c(3,1))
plot(x,f,axes=FALSE,type='l',lwd=3,col='red')
axis(1)
axis(2,col='red')
#xlim=c(-5,5)
# n=1000
# u=runif(n)
# v1=rnorm(n)
# v2=rnorm(n,mean=2,sd=2)
# y=matrix(0,1,n)
# for(i in 1:n){
#   if(u[i]<0.3){
#     y[i]=v1[i]
#   }else{
#    y[i]=v2[i] 
#   }
# }
#hist(y,xlim = xlim)

dist=c(rep(1, 3), rep(2, 7)) # 按照概率生成均值
x=rnorm(n,c(0, 2)[sample(dist, n, replace = TRUE)], sd = c(1, 2)[sample(dist, n, replace = TRUE)])
par(new=TRUE)
hist(x)
axis(4)