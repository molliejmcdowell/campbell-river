################################################
# ARIMA regression model
################################################

######################################################################
# mean stream temperature with multiple external regressors
######################################################################

# model
meanlogq <- log(ag.preharvest$q.mean)
atmean <- ag.preharvest$at.mean

st.mean.vector <- ag.preharvest$st.mean
st.mean.matrix <- cbind(atmean, meanlogq)

st.mean.arima <- arima(st.mean.vector, xreg = st.mean.matrix, order = c(1, 0, 0), method = "ML")
str(st.mean.arima)
summary(st.mean.arima)

# coefficients
st.mean.res <- residuals(st.mean.arima)
st.mean.ar <- st.mean.arima$coef[1]
st.mean.b0 <- st.mean.arima$coef[2]
st.mean.b1 <- st.mean.arima$coef[3]
st.mean.b2 <- st.mean.arima$coef[4]
st.mean.b3 <- st.mean.arima$coef[5]
st.mean.rse <- sqrt(st.mean.arima$sigma2)
st.mean.arima.df <- data.frame(st.mean.ar, st.mean.b0,
   st.mean.b1, st.mean.b2, st.mean.b3, st.mean.rse)

# predict post harvest stream temperature
postmeanlogq <- log(ag.postharvest$q.mean)
postatmean <- ag.postharvest$at.mean

stmean.predmat <- cbind(postatmean, postmeanlogq)

stmean.predict <- predict(st.mean.arima, newxreg = stmean.predmat, n.ahead = 1)
str(stmean.predict)
summary(stmean.predict)

stmean.predvals <- stmean.predict$pred

# diagnostic plots

# observed versus predicted postharvest values
plot(ag.postharvest$dt, ag.postharvest$st.mean)
lines(ag.postharvest$dt, ag.postharvest$st.mean, type = "o", pch = 25, bg = "blue", col = "blue", lwd = 1.5)
lines(ag.postharvest$dt, stmean.predvals, type = "o", bg = "red", col = "red", pch = 21, lwd = 1.5)

# histogram of residuals
hist(st.mean.res)

plot(st.mean.res)

# normal quantile plot
qqnorm(st.mean.res)
qqline(st.mean.res, col = "red")

######################################################################
# minimum stream temperature with multiple external regressors
######################################################################

# model
minlogq <- log(ag.preharvest$q.min)
atmin <- ag.preharvest$at.min

st.min.vector <- ag.preharvest$st.min
st.min.matrix <- cbind(atmin, minlogq)

st.min.arima <- arima(st.min.vector, xreg = st.min.matrix, order = c(1, 0, 0), method = "ML")
str(st.min.arima)
summary(st.min.arima)

# coefficients
st.min.res <- residuals(st.min.arima)
st.min.ar <- st.min.arima$coef[1]
st.min.b0 <- st.min.arima$coef[2]
st.min.b1 <- st.min.arima$coef[3]
st.min.b2 <- st.min.arima$coef[4]
st.min.b3 <- st.min.arima$coef[5]
st.min.rse <- sqrt(st.min.arima$sigma2)
st.min.arima.df <- data.frame(st.min.ar, st.min.b0,
 st.min.b1, st.min.b2, st.min.b3, st.min.rse)

# predict post harvest stream temperature
postminlogq <- log(ag.postharvest$q.min)
postatmin <- ag.postharvest$at.min

stmin.predmat <- cbind(postatmin, postminlogq)

stmin.predict <- predict(st.min.arima, newxreg = stmin.predmat, n.ahead = 1)
str(stmin.predict)
summary(stmin.predict)

stmin.predvals <- stmin.predict$pred

# diagnostic plots

# observed versus predicted postharvest values
plot(ag.postharvest$dt, ag.postharvest$st.min)
lines(ag.postharvest$dt, ag.postharvest$st.min, type = "o", pch = 25, bg = "blue", col = "blue", lwd = 1.5)
lines(ag.postharvest$dt, stmin.predvals, type = "o", bg = "red", col = "red", pch = 21, lwd = 1.5)

# histogram of residuals
hist(st.min.res)

plot(st.min.res)

# normal quantile plot
qqnorm(st.min.res)
qqline(st.min.res, col = "red")

######################################################################
# maximum stream temperature with multiple external regressors
######################################################################

# model
maxlogq <- log(ag.preharvest$q.max)
atmax <- ag.preharvest$at.max

st.max.vector <- ag.preharvest$st.max
st.max.matrix <- cbind(atmax, maxlogq)

st.max.arima <- arima(st.max.vector, xreg = st.max.matrix, order = c(1, 0, 0), method = "ML")
str(st.max.arima)
summary(st.max.arima)

# coefficients
st.max.res <- residuals(st.max.arima)
st.max.ar <- st.max.arima$coef[1]
st.max.b0 <- st.max.arima$coef[2]
st.max.b1 <- st.max.arima$coef[3]
st.max.b2 <- st.max.arima$coef[4]
st.max.b3 <- st.max.arima$coef[5]
st.max.rse <- sqrt(st.max.arima$sigma2)
st.max.arima.df <- data.frame(st.max.ar, st.max.b0,
   st.max.b1, st.max.b2, st.max.b3, st.max.rse)

# predict post harvest stream temperature
postmaxlogq <- log(ag.postharvest$q.max)
postatmax <- ag.postharvest$at.max

stmax.predmat <- cbind(postatmax, postmaxlogq)

stmax.predict <- predict(st.max.arima, newxreg = stmax.predmat, n.ahead = 1)
str(stmax.predict)
summary(stmax.predict)

stmax.predvals <- stmax.predict$pred

# diagnostic plots

# observed versus predicted postharvest values
plot(ag.postharvest$dt, ag.postharvest$st.max)
lines(ag.postharvest$dt, ag.postharvest$st.max, type = "o", pch = 25, bg = "blue", col = "blue", lwd = 1.5)
lines(ag.postharvest$dt, stmax.predvals, type = "o", bg = "red", col = "red", pch = 21, lwd = 1.5)

# histogram of residuals
hist(st.max.res)

plot(st.max.res)

# normal quantile plot
qqnorm(st.max.res)
qqline(st.max.res, col = "red")

