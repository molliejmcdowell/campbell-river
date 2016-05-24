################################################
# ARIMA regression model
################################################

######################################################################
# mean electrical conductivity with one external regressor
######################################################################

# model
meanlogbf <- log(ag.preharvest$bf.mean)

ec.mean.vector <- ag.preharvest$ec.mean
ec.mean.matrix <- meanlogbf

ec.mean.arima <- arima(ec.mean.vector, xreg = ec.mean.matrix, order = c(1, 0, 0), method = "ML")
str(ec.mean.arima)
summary(ec.mean.arima)

# coefficients
ec.mean.res <- residuals(ec.mean.arima)
ec.mean.ar <- ec.mean.arima$coef[1]
ec.mean.b0 <- ec.mean.arima$coef[2]
ec.mean.b1 <- ec.mean.arima$coef[3]
ec.mean.rse <- sqrt(ec.mean.arima$sigma2)
ec.mean.arima.df <- data.frame(ec.mean.ar, ec.mean.b0,
  ec.mean.b1, ec.mean.rse)

# predict post harvest stream temperature
postmeanlogbf <- log(ag.postharvest$bf.mean)
postecmean <- ag.postharvest$ec.mean

ecmean.predmat <- postmeanlogbf

ecmean.predict <- predict(ec.mean.arima, newxreg = ecmean.predmat, n.ahead = 1)
str(ecmean.predict)
summary(ecmean.predict)

ecmean.predvals <- ecmean.predict$pred

# diagnostic plots

# observed versus predicted postharvest values
plot(ag.postharvest$dt, ag.postharvest$ec.mean)
lines(ag.postharvest$dt, ag.postharvest$ec.mean, type = "o", pch = 25, bg = "blue", col = "blue", lwd = 1.5)
lines(ag.postharvest$dt, ecmean.predvals, type = "o", bg = "red", col = "red", pch = 21, lwd = 1.5)

# histogram of residuals
hist(ec.mean.res)

plot(ec.mean.res)

# normal quantile plot
qqnorm(ec.mean.res)
qqline(ec.mean.res, col = "red")

######################################################################
# minimum electrical conductivity with one external regressor
######################################################################

# model
minlogbf <- log(ag.preharvest$bf.min)

ec.min.vector <- ag.preharvest$ec.min
ec.min.matrix <- minlogbf

ec.min.arima <- arima(ec.min.vector, xreg = ec.min.matrix, order = c(1, 0, 0), method = "ML")
str(ec.min.arima)
summary(ec.min.arima)

# coefficients
ec.min.res <- residuals(ec.min.arima)
ec.min.ar <- ec.min.arima$coef[1]
ec.min.b0 <- ec.min.arima$coef[2]
ec.min.b1 <- ec.min.arima$coef[3]
ec.min.rse <- sqrt(ec.min.arima$sigma2)
ec.min.arima.df <- data.frame(ec.min.ar, ec.min.b0,
   ec.min.b1, ec.min.rse)

# predict post harvest stream temperature
postminlogbf <- log(ag.postharvest$bf.min)

ecmin.predmat <- postminlogbf

ecmin.predict <- predict(ec.min.arima, newxreg = ecmin.predmat, n.ahead = 1)
str(ecmin.predict)
summary(ecmin.predict)

ecmin.predvals <- ecmin.predict$pred

# diagnostic plots

# observed versus predicted postharvest values
plot(ag.postharvest$dt, ag.postharvest$ec.min)
lines(ag.postharvest$dt, ag.postharvest$ec.min, type = "o", pch = 25, bg = "blue", col = "blue", lwd = 1.5)
lines(ag.postharvest$dt, ecmin.predvals, type = "o", bg = "red", col = "red", pch = 21, lwd = 1.5)

# histogram of residuals
hist(ec.min.res)

plot(ec.min.res)

# normal quantile plot
qqnorm(ec.min.res)
qqline(ec.min.res, col = "red")

######################################################################
# maximum electrical conductivity with one external regressor
######################################################################

# model
maxlogbf <- log(ag.preharvest$bf.max)

ec.max.vector <- ag.preharvest$ec.max
ec.max.matrix <- maxlogq

ec.max.arima <- arima(ec.max.vector, xreg = ec.max.matrix, order = c(1, 0, 0), method = "ML")
str(ec.max.arima)
summary(ec.max.arima)

# coefficients
ec.max.res <- residuals(ec.max.arima)
ec.max.ar <- ec.max.arima$coef[1]
ec.max.b0 <- ec.max.arima$coef[2]
ec.max.b1 <- ec.max.arima$coef[3]
ec.max.rse <- sqrt(ec.max.arima$sigma2)
ec.max.arima.df <- data.frame(ec.max.ar, ec.max.b0,
   ec.max.b1, ec.max.rse)

# predict post harvest stream temperature
postmaxlogbf <- log(ag.postharvest$bf.max)

ecmax.predmat <- postmaxlogbf

ecmax.predict <- predict(ec.max.arima, newxreg = ecmax.predmat, n.ahead = 1)
str(ecmax.predict)
summary(ecmax.predict)

ecmax.predvals <- ecmax.predict$pred

# diagnostic plots

# observed versus predicted postharvest values
plot(ag.postharvest$dt, ag.postharvest$ec.max)
lines(ag.postharvest$dt, ag.postharvest$ec.max, type = "o", pch = 25, bg = "blue", col = "blue", lwd = 1.5)
lines(ag.postharvest$dt, ecmax.predvals, type = "o", bg = "red", col = "red", pch = 21, lwd = 1.5)

# histogram of residuals
hist(ec.max.res)

plot(ec.max.res)

# normal quantile plot
qqnorm(ec.max.res)
qqline(ec.max.res, col = "red")
