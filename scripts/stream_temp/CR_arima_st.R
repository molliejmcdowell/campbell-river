################################################
# ARIMA regression model
################################################

#####################################
# prelim cleaning for new dfs
####################################
dt <- ag.postharvest$dt

######################################################################
# mean stream temperature with multiple external regressors
######################################################################

# model
meanlogq <- log(ag.preharvest$q.mean)
atmean <- ag.preharvest$at.mean
sin.doy <- ag.preharvest$sin.doy
cos.doy <- ag.preharvest$cos.doy

st.mean.vector <- ag.preharvest$st.mean
st.mean.matrix <- cbind(atmean, meanlogq, sin.doy, cos.doy)

st.mean.arima <- arima(st.mean.vector, xreg = st.mean.matrix, order = c(1, 0, 0), method = "ML")
str(st.mean.arima)
summary(st.mean.arima)

# coefficients
st.mean.res <- residuals(st.mean.arima)
st.mean.ar <- st.mean.arima$coef[1]
st.mean.b0 <- st.mean.arima$coef[2]
st.mean.b1 <- st.mean.arima$coef[3]
st.mean.b2 <- st.mean.arima$coef[4]
st.mean.rse <- sqrt(st.mean.arima$sigma2)
st.mean.arima.df <- data.frame(st.mean.ar, st.mean.b0,
   st.mean.b1, st.mean.b2, st.mean.rse)

# predict post harvest stream temperature
postmeanlogq <- log(ag.postharvest$q.mean)
postatmean <- ag.postharvest$at.mean
sin.doy <- ag.postharvest$sin.doy
cos.doy <- ag.postharvest$cos.doy

stmean.predmat <- cbind(postatmean, postmeanlogq, sin.doy, cos.doy)

stmean.predict.ar <- predict(st.mean.arima, newxreg = stmean.predmat, n.ahead = 1)
str(stmean.predict.ar)
summary(stmean.predict.ar)

stmean.predvals <- stmean.predict.ar$pred

# histogram of residuals
hist(st.mean.res)
plot(st.mean.res)

# normal quantile plot
qqnorm(st.mean.res)
qqline(st.mean.res, col = "red")

observed <- ag.postharvest$st.mean
predicted <- stmean.predvals
NSE(predicted, observed)
mae(predicted, observed)

stmean.arimadf <- cbind.data.frame(dt, observed, predicted)
stmean.arimadf <- melt(stmean.arimadf, id.vars = "dt",
  variable.name = "ovp", value.name = "st.mean")

ggplot(stmean.arimadf, aes(x = dt, y = st.mean, colour = ovp)) + 
  geom_line(lwd = 1) + 
  labs(x = "Time", y = "Stream temperature (ºC)",
    title = "Post-harvest daily mean stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "stmeanarima_ovp.png")

######################################################################
# minimum stream temperature with multiple external regressors
######################################################################

# model
minlogq <- log(ag.preharvest$q.min)
atmin <- ag.preharvest$at.min
sin.doy <- ag.preharvest$sin.doy
cos.doy <- ag.preharvest$cos.doy

st.min.vector <- ag.preharvest$st.min
st.min.matrix <- cbind(atmin, minlogq, sin.doy, cos.doy)

st.min.arima <- arima(st.min.vector, xreg = st.min.matrix, order = c(1, 0, 0), method = "ML")
str(st.min.arima)
summary(st.min.arima)

# coefficients
st.min.res <- residuals(st.min.arima)
st.min.ar <- st.min.arima$coef[1]
st.min.b0 <- st.min.arima$coef[2]
st.min.b1 <- st.min.arima$coef[3]
st.min.b2 <- st.min.arima$coef[4]
st.min.rse <- sqrt(st.min.arima$sigma2)
st.min.arima.df <- data.frame(st.min.ar, st.min.b0,
 st.min.b1, st.min.b2, st.min.rse)

# predict post harvest stream temperature
postminlogq <- log(ag.postharvest$q.min)
postatmin <- ag.postharvest$at.min
sin.doy <- ag.postharvest$sin.doy
cos.doy <- ag.postharvest$cos.doy

stmin.predmat <- cbind(postatmin, postminlogq, sin.doy, cos.doy)

stmin.predict.ar <- predict(st.min.arima, newxreg = stmin.predmat, n.ahead = 1)
str(stmin.predict.ar)
summary(stmin.predict.ar)

stmin.predvals <- stmin.predict.ar$pred

# histogram of residuals
hist(st.min.res)
plot(st.min.res)

# normal quantile plot
qqnorm(st.min.res)
qqline(st.min.res, col = "red")

observed <- ag.postharvest$st.min
predicted <- stmin.predvals
NSE(predicted, observed)
mae(predicted, observed)

stmin.arimadf <- cbind.data.frame(dt, observed, predicted)
stmin.arimadf <- melt(stmin.arimadf, id.vars = "dt",
  variable.name = "ovp", value.name = "st.min")

ggplot(stmin.arimadf, aes(x = dt, y = st.min, colour = ovp)) + 
  geom_line(lwd = 1) + 
  labs(x = "Time", y = "Stream temperature (ºC)",
    title = "Post-harvest daily minimum stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "stminarima_ovp.png")

######################################################################
# maximum stream temperature with multiple external regressors
######################################################################

# model
maxlogq <- log(ag.preharvest$q.max)
atmax <- ag.preharvest$at.max
sin.doy <- ag.preharvest$sin.doy
cos.doy <- ag.preharvest$cos.doy

st.max.vector <- ag.preharvest$st.max
st.max.matrix <- cbind(atmax, maxlogq, sin.doy, cos.doy)

st.max.arima <- arima(st.max.vector, xreg = st.max.matrix, order = c(1, 0, 0), method = "ML")
str(st.max.arima)
summary(st.max.arima)

# coefficients
st.max.res <- residuals(st.max.arima)
st.max.ar <- st.max.arima$coef[1]
st.max.b0 <- st.max.arima$coef[2]
st.max.b1 <- st.max.arima$coef[3]
st.max.b2 <- st.max.arima$coef[4]
st.max.rse <- sqrt(st.max.arima$sigma2)
st.max.arima.df <- data.frame(st.max.ar, st.max.b0,
   st.max.b1, st.max.b2, st.max.rse)

# predict post harvest stream temperature
postmaxlogq <- log(ag.postharvest$q.max)
postatmax <- ag.postharvest$at.max
sin.doy <- ag.postharvest$sin.doy
cos.doy <- ag.postharvest$cos.doy

stmax.predmat <- cbind(postatmax, postmaxlogq, sin.doy, cos.doy)

stmax.predict.ar <- predict(st.max.arima, newxreg = stmax.predmat, n.ahead = 1)
str(stmax.predict.ar)
summary(stmax.predict.ar)

stmax.predvals <- stmax.predict.ar$pred

# histogram of residuals
hist(st.max.res)
plot(st.max.res)

# normal quantile plot
qqnorm(st.max.res)
qqline(st.max.res, col = "red")

observed <- ag.postharvest$st.max
predicted <- stmax.predvals
NSE(predicted, observed)
mae(predicted, observed)

stmax.arimadf <- cbind.data.frame(dt, observed, predicted)
stmax.arimadf <- melt(stmax.arimadf, id.vars = "dt",
  variable.name = "ovp", value.name = "st.max")

ggplot(stmax.arimadf, aes(x = dt, y = st.max, colour = ovp)) + 
  geom_line(lwd = 1) + 
  labs(x = "Time", y = "Stream temperature (ºC)",
    title = "Post-harvest daily maximum stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "stmaxarima_ovp.png")
