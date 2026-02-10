##
## Time series 2 ##

rm(list=ls())
graphics.off()
set.seed(123)

library(forecast)

## ---------- ARIMA(1, 0, 1) --------------------- 

# ARIMA(1, 0, 1) : y[t] = phi*y[t-1] + e[t] + theta*e[t-1]
# e[t] ~ N(0, 1)

# modeling
phi=.9 ; theta=.3; n=1000
y=rep(0, n)
e=rnorm(n)

for(t in 2:n){
  y[t] = phi*y[t-1] + e[t] + theta*e[t-1]
}

x=seq(1, n, 1)
plot(x, y, type='l', col='2', main="ARIMA(1, 0, 1)",
     xlab="period", ylab="value")

# estimation
results <- arima(y, order=c(1, 0, 1))
results

# forecasting
n=length(y)

output.arima <- arima(y[1:(n-10)], 
                      order=c(1, 0, 1))  # estimation based on y[1:(n-10)]
fcast <- forecast(output.arima, h=10)

plot(as.numeric(fcast$mean), main="Forecasting and 80% interval", type='l',
     col='orange', ylim=c(-3, 6), ylab="y")
lines(as.numeric(fcast$lower[, 1]), type='l', col='darkred')
lines(as.numeric(fcast$upper[, 1]), type='l', col='darkred')
lines(y[991:n], type='l', col='red')
legend('bottomleft', legend=c('prediction', 'lower bound', 'upper bound', 'actual series'),
       lty=c(1, 1, 1, 1), col=c("orange", "darkred", "darkred", "red"),
       cex=1.5, box.lty=0)


# plot the forecasts along with the actual series from 980 to 990
forecast <- c(y[981:990], as.numeric(fcast$mean))
lower.forecast <- c(y[981:990], as.numeric(fcast$lower[, 1]))
upper.forecast <- c(y[981:990], as.numeric(fcast$upper[, 1]))
actual.series <- y[981:n]

plot(981:n, forecast, main="forecasts and 80% interval", type='l',
     col='2', ylim=c(-3, 6), xlab="period", ylab='y')
polygon(c(990:n, rev(990:n)),
        c(upper.forecast[10:20], rev(lower.forecast[10:20])),
        col="gray92", border="gray92")
lines(981:n, forecast, type='l', col='blue', lwd=2)
lines(981:n, actual.series, type='l', col='red', lwd=2)
legend('bottomleft', legend=c("prediction", "Confidence Interval", "actual series"), 
       lty=c(1,1,1),lwd=c(2,6,2), col=c("blue","gray60", "red"), cex=1.5, box.lty=0)

## ----------------------------------------------------