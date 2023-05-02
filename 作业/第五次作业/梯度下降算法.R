#梯度下降算法
# 11111111

#22222222
# 333333

library("showtext")
showtext.auto(enable = TRUE)
#定义逻辑回归对数似然函数的一阶导数
gx=function(y,beta,x2,x3){
    temp1=y-1+(exp(-(beta[1]+beta[2]*x2+beta[3]*x3))/(1+exp(-(beta[1]+beta[2]*x2+beta[3]*x3))))
    temp2=rbind(rep(1,length(y)),x2,x3)
    out=temp2%*%temp1
  return(out)
}
set.seed(123)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = rbinom(n,1,plogis(1+2*x1+3*x2))
#归一化处理
# x1_new=(x1-min(x1))/(max(x1)-min(x1))
# x2_new=(x2-min(x2))/(max(x2)-min(x2))
# y_new=(y-min(y))/(max(y)-min(y))
beta0=matrix(c(1,1,1))
beta1=c(beta0[1])
beta2=c(beta0[2])
beta3=c(beta0[3])
alpha=0.001
times=0
all_times=c(0)
d=matrix(c(1,1,1))
while(max(abs(d))>=10^(-10)){
  #browser()
  beta_new=beta0+alpha*gx(y,beta0,x1,x2)
  d=beta_new-beta0
  beta1=c(beta1,beta_new[1])
  beta2=c(beta2,beta_new[2])
  beta3=c(beta3,beta_new[3])
  beta0=beta_new
  times=times+1
  all_times=c(all_times,times)
}
print(beta_new)
#绘制收敛曲线
plot(all_times,beta1,type="l",col="red",xlab="times",ylab="beta",ylim=c(min(min(beta1),min(beta2),min(beta3)),max(max(beta1),max(beta2),max(beta3))),main='梯度下降算法迭代次数与参数取值示意图',lwd=2.5)
points(all_times,beta2,type="l",col="blue",xlab="times",ylab="beta2",lwd=2.5)
points(all_times,beta3,type="l",col="black",xlab="times",ylab="beta3",lwd=2.5)
legend('topleft',c("beta1","beta2","beta3"),col=c("red","blue","black"),lty=1)