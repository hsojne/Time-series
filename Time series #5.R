##
## Time series 5 ##

rm(list=ls())
graphics.off()

## ---------- Stochastic volatility model ---------- 
## e[t] = exp(x[t]/2)*z[t], z[t] ~ N(0, 1)
## x[t] = log(sigma[t]^2)
## x[t+1] = omega + phi*x[t] + eta[t]
## eta[t] ~ N(0, sigma_eta^2)


## part 1: set true values

dome <- -.1 ; dphi <- .95 ; dsig <- .2    # omega, phi, and sigme_eta

ns <- 1000    # the number of observations

veps <- vx <- matrix(0, ns, 1)

dinm <- dome / (1 - dphi)  # stationary mean for initial
dimv <- dsig^2 / (1 - dphi^2) # stationary variance for initial


# part 2: simulate observations

ve <- rnorm(ns)     # simulation z[t]
veta <- rnorm(ns)  # simulation eta[t]

vx[1] <- dinm + sqrt(dimv)*rnorm(1)
for(t in 1:ns){
  veps[t] <- exp(vx[t]/2) * ve[t]   # compute return
  
  if(t < ns){
    vx[t+1] <- dome + dphi*vx[t] + dsig*veta[t]
  }
}


# part 3: draw figure

par(mfrow=c(2,1))

plot(exp(vx/2), type='l', col='2', main='"Simulated Volatility (sigma)',
     xlab='t', ylab='sigma(t)')

plot(veps, type='l', col='2', main='"Simulated return (y)',
     xlab='t', ylab='epsilon(t)')

## ------------------------------------------------------------

## ---------- Kalman filter and smoother ---------- 
## e[t] = exp(x[t]/2)*z[t], z[t] ~ N(0, 1)
## x[t] = log(sigma[t]^2)
## x[t+1] = omega + phi*x[t] + eta[t]
## eta[t] ~ N(0, sigma_eta^2)
##
## [state space representation]
## y[t] = log(e[t]^2) + 1.27
## u[t] = log(z[t]^2) + 1.27
##
## y[t] = x[t] + u[t], u[t] ~ N)0, pi^2/2)
## x[t] = x[t-1]+ eta[t]


## part 1: set parameters and variables
mdata <- cbind(veps, vx)

veps <- mdata[, 1]  # e[t]
vxtrue <- mdata[, 2] # true of x[t]

dome <- -.1 ; dphi <- .95 ; dsig <- .2    # omega, phi, and sigma_eta

dc <- 0.0001                         # adjustment constant
vy <- log(veps^2 + dc) + 1.27        #  y[t]

n <- length(vy)                      # the number of observations

vxtu <- vptu <- vn <- vF     <- matrix(0, ns, 1)
vxtf <- vptf <- vxts <- vpts <- matrix(0, ns+1, 1)

dsu2 <- pi^2 / 2         #sigma_u


## part 2: Kalman Filter

vxtf[1] <- dome / (1 - dphi)        # x[1|0]
vptf[1] <- dsig^2 / (1 - dphi^2)    # P[1|0]

for(t in 1:ns){
  vn[t] <- vy[t] - vxtf[t]          # nu[t]
  vF[t] <- vptf[t] + dsu2           # F[t]
  
  vxtu[t] <- vxtf[t] + vptf * vn[t] / vF[t]    # update: x[t|t]
  vptu[t] <- vptf[t] - vptf[t]^2 / vF[t]       # update: P[t|t]
  
  vxtf[t+1] <- dome + dphi * vxtu[t]           # forecast: x[t+1|t]
  vptf[t+1] <- dphi^2 + vptu[t] + dsig^2        # forecast: P[t+1|t]

}


## part 3: smoother

vxts[ns+1] <- vxtf[ns+1]            # x[T+1|T]
vpts[ns+1] <- vptf[ns+1]            # P[T+1|T]
for(t in ns:1){
  dps <- dphi * vptu[t] / vptf[t+1] # P*[t]
  
  vxts[t] <- vxtu[t] + dps * (vxts[t+1] - vxtf[t+1])   # x[t|T]
  vpts[t] <- vptu[t] + dps^2 * (vpts[t+1] - vptf[t+1]) # P[t|T]
}


## part 4: likelihood

dlik <- -.5 * sum(log(2*pi) + log(vF) + vn^2 / vF)      # log likelihood


## part 5: draw figure

nd <- 200 
vyl <- c(-4, 0)

par(mfrow=c(1,1))

plot(vxtrue[1:nd], type='l', col='2',
     xlab='t', ylab='x(t)', xlim=c(1, nd), ylim=vyl)

par(new=T)
plot(vxtu[1:nd], type='l', col='3',
     xlim=c(1, nd), ylim=vyl, ann=F)

par(new=T)
plot(vxts[1:nd], type='l', col='4',
     xlim=c(1, nd), ylim=vyl, ann=F)

legend("bottomleft",
       legend=c("True", "Filtered", "Smoothed"),
       lty=1, col=2:4)
title("Kalman Filter and Smoother: x(t)")

## ------------------------------------------------------------








