#先计算出mu,sigma所有可能的组合，再对每对mu,sigma对应的似然函数进行计算，取值最大的为最优sigma和mu
logNormLike <- function(mu, sigma, data)
{
  out = sum(dnorm(
    x = data, mean = mu, sd = sigma,
    log = TRUE))
  return(out)
}
x <- c(294, 262, 196, 79, 191, 677)
mu = 260:300
sigma = 180:220

parMat <- expand.grid(mu, sigma)
muALL <- parMat[, 1]
sigmaALL <- parMat[, 2]
myLogLike <- matrix(NA, 1, length(sigma))
for(i in 1:length(sigmaALL))
{
  myLogLike[i] <- logNormLike(mu = muALL[i], sigma = sigmaALL[i], data = x)
}
index=which.max(myLogLike)
print(paste('best mu is',muALL[index]))
print(paste('best mu is',sigmaALL[index]))