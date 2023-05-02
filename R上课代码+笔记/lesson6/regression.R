#求解一元线性回归的参数，下列两种方法相互印证
#1.对三个未知参数进行估计,利用似然函数的原理
#先写出含有未知参数的函数
#再对函数进行优化处理
logNormLikelihood <- function(par, y, x)
{
  beta0 <- par[1]
  beta1 <- par[2]
  sigma <- par[3]
  mean <- beta0 + x*beta1
  logDens <- dnorm(x = y, mean = mean, sd = sigma, log = TRUE)
  loglikelihood <- sum(logDens)
  return(loglikelihood)
}
n <- 1000
x <- rnorm(n, 3, 1)
y <- 1+x*3+rnorm(n,mean=0,sd=0.5)
optimOut <- optim(c(0.2, 0.3, 0.5), logNormLikelihood,
                  control = list(fnscale = -1),#计算最大值
                  x = x, y = y)

beta0Hat <- optimOut$par[1]
beta1Hat <- optimOut$par[2]
sigmaHat <- optimOut$par[3]
yHat <- beta0Hat + beta1Hat*x
#2.利用R中的函数进行一元线性回归
myLM=lm(y~x)
myLMCoef=myLM$coefficients
yHatOLS=myLMCoef[1]+myLMCoef[2]*x
plot(x, y, pch = 20, col = "blue")
#order函数默认用于返回向量从小到大排序在原始向量中的位次（秩）
temp=order(x)
print(temp)
points(sort(x), yHat[temp], type = "l", col = "red", lwd = 10)
points(sort(x), yHatOLS[temp], type = "l", lty = "dashed",
       col = "yellow", lwd = 2, pch = 20)
