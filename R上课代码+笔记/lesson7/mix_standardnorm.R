w = 0.3
mu1 = 0
mu2 = 2
sigma1 = 1
sigma2 = sqrt(2)
par(mfrow=c(2,1))
xgrid<-seq(-1.5, 1.5, 0.01)
nm <- dnorm(xgrid)
mix <-w*dnorm(xgrid, mu1, sigma1) + (1-w)*dnorm(xgrid, mu2, sigma2)
plot(xgrid, nm, "l", lwd=2, col='red',main='Normal: Red, Mixture: Blue',
     xlab = 'x', ylab = 'y', ylim=c(0, max(nm,mix)))
lines(xgrid, mix, lwd=2,col='blue')
M0 = (w*dnorm(xgrid, mu1, sigma1) + (1-w)*dnorm(xgrid, mu2, sigma2))/dnorm(xgrid) 
M = max(M0)
Mnm <-M*nm
plot(xgrid, Mnm, lwd=2, type="l",col='red',main='Scaled normal: Red, Mixture: Blue',xlab = 'x',ylab = 'y')
lines(xgrid, mix, lwd=2, col='blue')

n<-500
xp<-rnorm(n)
yp<-M*dnorm(xp)*runif(n)
dmix = w*dnorm(xp, mu1, sigma1) + (1-w)*dnorm(xp, mu2, sigma2)
points(xp[yp>=dmix],yp[yp>=dmix], col='red',pch=20, cex=0.2)
points(xp[yp<dmix],yp[yp<dmix], pch=16, cex=0.2, col='blue')
print(M)