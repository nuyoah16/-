#逻辑回归
library("showtext")
showtext.auto(enable = TRUE)
#定义逻辑回归对数似然函数的一阶、二阶导数
gx1=function(y,beta,x2,x3){
  out=matrix(0,3,1)
  for(i in 1:n) {
    temp1=y[i]-1+(exp(-(beta[1]+beta[2]*x2[i]+beta[3]*x3[i]))/(1+exp(-(beta[1]+beta[2]*x2[i]+beta[3]*x3[i]))))
    temp2=matrix(c(1,x2[i],x3[i]))
    temp3=temp1*temp2
    out=out+temp3
  }
  return(out)
}
dgx=function(y,beta,x2,x3){
  out1=matrix(0,3,3)
  for(i in 1:n){
    a1=(1/(1+exp(-(beta[1]+beta[2]*x2[i]+beta[3]*x3[i])))**2)*exp(-(beta[1]+beta[2]*x2[i]+beta[3]*x3[i]))
    a2=matrix(c(1,x2[i],x3[i]))
    a3=matrix(c(-1,-x2[i],-x3[i]),1,3)
    out1=out1+a1*(a2%*%a3)
  }
  return(out1)
}
#利用Newton算法求解
set.seed(123)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = rbinom(n, 1, plogis(1 + 2*x1 + 3*x2))
beta0=matrix(c(1,1,1))
beta1=c(1)
beta2=c(1)
beta3=c(1)
res=1
times=0
all_times=c(0)
while(max(res)>=10^(-10)){
  beta_new=beta0-solve(dgx(y,beta0,x1,x2))%*%gx(y,beta0,x1,x2)
  beta1=c(beta1,beta_new[1])
  beta2=c(beta2,beta_new[2])
  beta3=c(beta3,beta_new[3])
  res=gx(y,beta_new,x1,x2)
  beta0=beta_new
  times=times+1
  all_times=c(all_times,times)
}
print(beta_new)
#与glm函数生成结果对比
model = glm(y ~ x1 + x2, family = binomial(link = "logit"))
print(model)
#绘制收敛曲线
par(mfrow=c(3,1))
plot(all_times,beta1,type="l",col="red",xlab="times",ylab="beta",ylim=c(min(min(beta1),min(beta2),min(beta3)),max(max(beta1),max(beta2),max(beta3))),main='牛顿迭代算法迭代次数与参数取值示意图',lwd=2.5)
points(all_times,beta2,type="l",col="blue",xlab="times",ylab="beta2",lwd=2.5)
points(all_times,beta3,type="l",col="black",xlab="times",ylab="beta3",lwd=2.5)
legend('topleft',c("beta1","beta2","beta3"),col=c("red","blue","black"),lty=1)

