#随机梯度下降算法
library("showtext")
showtext.auto(enable = TRUE)
#定义逻辑回归对数似然函数的一阶导数
#index为参与求和的数据行号
gx=function(total,beta,index){
    #debug(gx)
    temp1=total$Y[index]-1+(exp(-(beta[1]+beta[2]*total$X1[index]+beta[3]*total$X2[index]))/(1+exp(-(beta[1]+beta[2]*total$X1[index]+beta[3]*total$X2[index]))))
    temp2=rbind(rep(1,length(total$X1[index])),total$X1[index],total$X2[index])
    out=temp2%*%temp1#这个计算？？temp1为1*100，temp2为3*100
  return(out)
}
set.seed(123)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
y = rbinom(n,1,plogis(1+2*x1+3*x2))
total=data.frame(Y=y,X1=x1,X2=x2)
beta0=matrix(c(1,0,1))
beta1=c(beta0[1])
beta2=c(beta0[2])
beta3=c(beta0[3])
beta1_f=c()
beta2_f=c()
beta3_f=c()
times=0
all_times=c(0)
alpha=0.001
s=10#每个batch里的数据量
#进行m轮epoch
m=65
for(i in 1:m){
  for(j in 1:n/s){
    index=seq(1+(j-1)*s:j*s)
    beta_new=beta0+alpha*gx(total,beta0,index)
    beta1=c(beta1,beta_new[1])
    beta2=c(beta2,beta_new[2])
    beta3=c(beta3,beta_new[3])
    beta0=beta_new
    times=times+1
    all_times=c(all_times,times)
    if(times>=60000){
      beta1_f=c(beta1_f,beta_new[1])
      beta2_f=c(beta2_f,beta_new[2])
      beta3_f=c(beta3_f,beta_new[3])
    }
  }
  total=total[sample(nrow(total)),c('Y','X1','X2')]
} 
beta_final=matrix(c(mean(beta1_f),mean(beta2_f),mean(beta3_f)))
print(beta_final)
#绘制收敛曲线
plot(all_times,beta1,type="l",col="red",xlab="times",ylab="beta",ylim=c(min(min(beta1),min(beta2),min(beta3)),max(max(beta1),max(beta2),max(beta3))),main='随机梯度下降算法迭代次数与参数取值示意图',lwd=2.5)
points(all_times,beta2,type="l",col="blue",xlab="times",ylab="beta2",lwd=2.5)
points(all_times,beta3,type="l",col="black",xlab="times",ylab="beta3",lwd=2.5)
legend('topleft',c("beta1","beta2","beta3"),col=c("red","blue","black"),lty=1)