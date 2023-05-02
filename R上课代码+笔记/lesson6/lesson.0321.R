logNormLike <- function(mu, sigma, data)
{
  out = sum(dnorm(
    x = data, mean = mu, sd = sigma,
    log = TRUE))
  return(out)
}
set.seed(123)
logNormLike(mu = 2, sigma = 0.9, data = rnorm(10000, mean=2,sd=1))

NormLike <- function(mu, sigma, data)
{
  out = prod(dnorm(x = data, mean = mu, sd = sigma))
  return(out)
}
set.seed(123)
logNormLike(mu = 2.1, sigma = 1, data = rnorm(10000, mean=2,sd=1))



