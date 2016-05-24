################################################
# ARIMA regression model
################################################

######################################################################
# mean electrical conductivity with one external regressor
######################################################################

# model
meanlogq <- log(ag.preharvest$q.mean)
sin.doy <- ag.preharvest$sin.doy
cos.doy <- ag.preharvest$cos.doy

ec.mean.vector <- ag.preharvest$ec.mean
ec.mean.matrix <- cbind(meanlogq, sin.doy, cos.doy)

ec.mean.arima <- arima(ec.mean.vector, xreg = ec.mean.matrix, order = c(1, 0, 0), method = "ML")
#ec.mean.arima <- auto.arima(ec.mean.vector, xreg = ec.mean.matrix)
str(ec.mean.arima)
summary(ec.mean.arima)

ec.mean.res <- residuals(ec.mean.arima)

# predict post harvest stream temperature
postmeanlogq <- log(ag.postharvest$q.mean)
sin.doy <- ag.postharvest$sin.doy
cos.doy <- ag.postharvest$cos.doy
postecmean <- ag.postharvest$ec.mean

ec.mean.predmat <- cbind(postmeanlogq, sin.doy, cos.doy)

ec.mean.predict <- predict(ec.mean.arima, newxreg = ec.mean.predmat, n.ahead = 1)
#ec.mean.predict <- forecast.Arima(ec.mean.arima, xreg = ec.mean.predmat)
str(ec.mean.predict)
summary(ec.mean.predict)

ec.mean.predvals <- ec.mean.predict$pred

######################################################################
# minimum electrical conductivity with one external regressor
######################################################################

# model
minlogq <- log(ag.preharvest$q.min)
sin.doy <- ag.preharvest$sin.doy
cos.doy <- ag.preharvest$cos.doy

ec.min.vector <- ag.preharvest$ec.min
ec.min.matrix <- cbind(minlogq, sin.doy, cos.doy)

ec.min.arima <- arima(ec.min.vector, xreg = ec.min.matrix, order = c(1, 0, 0), method = "ML")
#ec.min.arima <- auto.arima(ec.min.vector, xreg = ec.min.matrix)
str(ec.min.arima)
summary(ec.min.arima)

ec.min.res <- residuals(ec.min.arima)

# predict post harvest stream temperature
postminlogq <- log(ag.postharvest$q.min)
sin.doy <- ag.postharvest$sin.doy
cos.doy <- ag.postharvest$cos.doy

ec.min.predmat <- cbind(postminlogq, sin.doy, cos.doy)

ec.min.predict <- predict(ec.min.arima, newxreg = ec.min.predmat, n.ahead = 1)
#ec.min.predict <- forecast.Arima(ec.min.arima, xreg = ec.min.predmat)
str(ec.min.predict)
summary(ec.min.predict)

ec.min.predvals <- ec.min.predict$pred

######################################################################
# maximum electrical conductivity with one external regressor
######################################################################

# model
maxlogq <- log(ag.preharvest$q.max)
sin.doy <- ag.preharvest$sin.doy
cos.doy <- ag.preharvest$cos.doy

ec.max.vector <- ag.preharvest$ec.max
ec.max.matrix <- cbind(maxlogq, sin.doy, cos.doy)

ec.max.arima <- arima(ec.max.vector, xreg = ec.max.matrix, order = c(1, 0, 0), method = "ML")
#ec.max.arima <- auto.arima(ec.max.vector, xreg = ec.max.matrix)
str(ec.max.arima)
summary(ec.max.arima)

ec.max.res <- residuals(ec.max.arima)

# predict post harvest stream temperature
postmaxlogq <- log(ag.postharvest$q.max)
sin.doy <- ag.postharvest$sin.doy
cos.doy <- ag.postharvest$cos.doy

ec.max.predmat <- cbind(postmaxlogq, sin.doy, cos.doy)

ec.max.predict <- predict(ec.max.arima, newxreg = ec.max.predmat, n.ahead = 1)
#ec.max.predict <- forecast.Arima(ec.max.arima, xreg = ec.max.predmat)
str(ec.max.predict)
summary(ec.max.predict)

ec.max.predvals <- ec.max.predict$pred

######################################################################
# electrical conductivity with one external regressor
######################################################################

# model
logq <- log(preharvest.x$q)
sin.doy <- preharvest.x$sin.doy
cos.doy <- preharvest.x$cos.doy

ec.vector <- preharvest.x$ec
ec.matrix <- cbind(logq, sin.doy, cos.doy)

ec.arima <- arima(ec.vector, xreg = ec.matrix, order = c(1, 0, 0), method = "ML")
#ec.arima <- auto.arima(ec.vector, xreg = ec.matrix)
str(ec.arima)
summary(ec.arima)

ec.res <- residuals(ec.arima)

# predict post harvest stream temperature
postlogq <- log(postharvest.x$q)
sin.doy <- postharvest.x$sin.doy
cos.doy <- postharvest.x$cos.doy

ec.predmat <- cbind(postlogq, sin.doy, cos.doy)

ec.predict <- predict(ec.arima, newxreg = ec.predmat, n.ahead = 1)
#ec.predict <- forecast.Arima(ec.arima, xreg = ec.predmat)
str(ec.predict)
summary(ec.predict)

ec.predvals <- ec.predict$pred

