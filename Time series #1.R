##
## Time series 1 ##


## ---------- White noise --------------------- 

# White noise process
rm(list=ls())

y <- vector(length=1000)
for(i in 1:length(y)){
  y[i] <- rnorm(1, 0, 1)
}
par(mfrow=c(1,1))
plot(y, type='l', xlab='period', ylab='y(t)', col='2')
legend('bottomleft', inset=0.05, 'White noise process',
       lwd=2, lty=1, col='2')

## ---------------------------------------- 


## ----------  AR(1) process --------------

# AR(1) : y(t) = mu + phi*y(t-1) + e(t), e(t) ~ N(0, sigma^2)
rm(list=ls())
set.seed(1)

y <- vector(length=1000)         # y(t)
mu <- 0.2                        # mu    
e <- rnorm(length(y), 0, 1)      # e(t) ~ N(0, 1)

y[1] <- 0       # initial y[1]
phi1 <- 0.98

for(t in 2:length(y)){
  y[t] <- mu + phi1*y[t-1] + e[t]
}

plot(y, type='l', xlab="period", ylab="y(t)", col='2')
legend("bottomleft", inset=0.05, c(paste("Phi1=", phi1)), 
       lwd=2, lty=1, col='2')

## ---------------------------------------- 


## ----------  ACF and PACF --------------

# Autocorrelation Function: ACF
acf(y, lag=12, main="ACF")

# Partial Autocorrelation Function: PACF
pacf(y, lag=12, main="PACF")

## ---------------------------------------- 


## ----------  Given various of phi1, generate y --------------

par(mfcol=c(2,2))

y[1] <- 0       # initial y[1]

for(phi1 in c(0, .5, .9, 1)){
  
  for(t in 2:length(y)){
    y[t] <- phi1*y[t-1] + e[t]
  }
  
  plot(y, type='l', xlan='period', ylab='y(t)',
       col='2')
  legend('bottomleft', inset=0.05, c(paste('Pih1= ', phi1)),
         lwd=2, lty=1, col='2')
}

## ---------------------------------------- 


## ----------  AR(2) process --------------

# AR(2) : y(t) = phi1*y(t-1) + phi2*y(t-2) + e(t), e(t) ~ N(0, sigma^2)

par(mfrow=c(2,2))

y[1] <- 0 ; y[2] <- 0 

for(phi in c(0, .5, .9, 1)){
  
  if(phi ==0){
    phi1=0 #phi = phi1 + phi2
    phi2=0
  } else if(phi == .5){
    phi1=.25
    phi2=.25
  } else if(phi == .9){
    phi1=.45
    phi2=.45
  } else{
    phi1=.5
    phi2=.5
  }
  
  for(t in 3:length(y)){
    y[t] <- phi1*y[t-1] + phi2*y[t-2] + e[t]
  }
  
  plot(y, type='l', xlab='period', ylab='y', col='2')
  legend('bottomleft', inset=.05, c(paste('phi1 + phi2 = ', phi)),
         lwd=2, lty=1, col='2')
  
}
par(mfrow=c(1,1))
## ---------------------------------------- 


## ----------  MA(1) process --------------

# MA(1) : y(t) = e[t] + theta*e[t-1], e[t] ~ N(0, 1)
rm(list=ls())

y <- vector(length=1000)
set.seed(1)

e <- rnorm(length(y), 0, 1)    # e[t] ~ N(0, 1)

y[1] <- 0

par(mfrow=c(1,2))
for(theta in c(.8, -.8)){
  
  for(t in 2:length(y)){
    y[t] <- e[t] + theta*e[t-1]
  }
  
  plot(y, type='l', xlab='period', ylab='y', col='2')
  legend('topleft', inset=.05, c(paste('theta= ', theta)),
         lwd=2, lty=1, col='2') 
}
par(mfrow=c(1,1))
## ---------------------------------------- 
