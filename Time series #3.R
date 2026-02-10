##
## Time series 3 ##

rm(list=ls())
graphics.off()
set.seed(123)


## ---------- ARCH model --------------------- 
# estimate SP500 using ARCH(1) process
library(quantmod)
library(tseries)


# part 1: load data
SP500 <- getSymbols('^GSPC', form='1990-01-01', to='2025-12-12', auto.assign = F)
SP500.return <- dailyReturn(SP500, type='arithmetic')

date <- index(SP500.return)
SP500.return <- SP500.return - mean(SP500.return)  # for simpleification of the model

# part 2: estimation
ARCH1 <- garch(SP500.return, c(0, 1))
theta1 <- ARCH1$coef[1]
theta2 <- ARCH1$coef[2]


# part 3: conditinal variance 
y <- SP500.return
n <- length(y)

sigma2 <- sd(y)^2 * rep(1, n)
e <- y

for(t in 2:n){
  sigma2[t] <- theta1 + theta2*e[t-1]^2
}


# part 4: graph of the conditional variance
par(mfrow=c(2,1), mar=c(5, .5, .1, 1))

plot(date, sigma2, type='l', xlab='date', ylab=expression(sigma2[t]^2),
     col='darkorange', main="volatility")
plot(date, SP500.return, type="l", xlab='date', ylab="return",
     col="darkred", main="return of SP500")


