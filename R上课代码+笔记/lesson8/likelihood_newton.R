logNormLike <- function(mu, sigma, data)
{
  out = sum(dnorm(
    x = data, mean = mu, sd = sigma,
    log = TRUE))
  return(out)
}
data=c(294,262,196,79,191,677)
sigma=200
#dfx=(1/(sigma^2))*(exp(-((294-mu)^2)/2*(sigma^2))