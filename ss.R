# generate data
set.seed(1237)
n <- 100
x <- seq(0, 1, length.out = n)
fx <- 2 + 3 * x + sin(2 * pi * x)
y <- fx + rnorm(n, sd = 0.5)

library(npreg)
reg=ss(x,y,nknots=10)

plot(x,y,col=2)
lines(reg,lwd=2) #plot ss
abline(lm(y~x), lty = 2) #plot lm
legend("bottomright", legend = c("ss", "lm"), lty = 1:2,lwd=2:1, bty = "n")

summary.ss(reg)

predict.ss(reg,x=0.3)

plot.ss(reg)