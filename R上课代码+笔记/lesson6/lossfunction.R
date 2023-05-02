loss <- function(par, y, x)
{
  beta0 <- par[1]
  beta1 <- par[2]
  yhat <- beta0+beta1*x
  error=(yhat-y)^2
  lossfunction=mean(error)
  return(lossfunction)
}

x <- rnorm(100, 3, 1)
y <- 1+x*3
optimOut <- optim(c(0.2, 0.3), loss,
                  x = x, y = y)
optimOut$par[1]
optimOut$par[2]


