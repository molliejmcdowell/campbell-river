auto.mean <- auto.arima(ec.mean.vector, xreg = ec.mean.matrix)
forecast.mean <- forecast.Arima(ec.mean.arima, xreg = ec.mean.predmat)

plot(ag.postharvest$dt, forecast.mean$mean)
lines(ag.postharvest$dt, ag.postharvest$ec.mean)
