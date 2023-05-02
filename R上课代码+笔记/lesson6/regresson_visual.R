beta0 <- 1
beta1 <- 3
sigma <- 0.5

n <- 1000
x <- rnorm(n, 4, 3)
y <- beta0 +x*beta1 + rnorm(n, mean = 0, sd = sigma)
plot(x, y, col = "blue", pch = 20)