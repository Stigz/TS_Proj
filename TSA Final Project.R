BeersData <- read.csv("~/Desktop/Beers70to90.csv")
Beers.ts <- ts(BeersData, start = c(1970, 1), frequency = 12)


par(mfrow=c(1,2))
plot.ts(Beers.ts, main='Time Series')
acf(Beers.ts, main='ACF of Original Time Series')

bc <- BoxCox.ar(Beers.ts)
maxLambda <- bc$mle #the Maximum is -.3
Beers.ts.lam <- Beers.ts^maxLambda #the transformed data values
par(mfrow=c(1,1))
plot.ts(Beers.ts.lam)
Beers.log <-log(Beers.ts)
plot.ts(Beers.log)


diff <- diff(Beers.log)
plot.ts(diff)
acf(diff(as.vector(Beers.log)))
points(y=(diff((Beers.log), lag=12)), x=time(diff(Beers.log), lag=12), pch=as.vector(season(diff(Beers.log), lag=12)))

plot.ts(diff(diff(Beers.log), lag=12))
acf(diff(diff(as.vector(Beers.log)), lag=12))
points((diff(diff(Beers.log)), lag=12), x=time(diff(diff(Beers.log), lag=12)), pch=as.vector(season((diff(diff(Beers.log), lag=12)))))

plot.ts(diff(diff(diff(Beers.log), lag=12)))
acf(diff(diff(diff(as.vector(Beers.log)), lag=12)))

Beer.mod = arima(Beers.log, order=c(2,2,0),seasonal=list(order=c(0,0,1),period=12))
Beer.mod
