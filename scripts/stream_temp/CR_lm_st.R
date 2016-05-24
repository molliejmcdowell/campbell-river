########################################
# linear regression model: stream temp
#######################################

#####################################
# prelim cleaning for new dfs
####################################
dt <- ag.postharvest$dt
date.time <- postharvest$date.time

#####################################
# linear model: st mean
#####################################

stmean.lm <- lm(st.mean ~ log(q.mean) + at.mean + sin.doy + cos.doy, data = ag.preharvest, na.action = na.exclude)
summary(stmean.lm)

stmean.predict <- predict.lm(stmean.lm, newdata = ag.postharvest, 
  interval = "prediction", level = 0.95)
stmean.preds <- stmean.predict[,1]

observed <- ag.postharvest$st.mean
predicted <- stmean.preds

stmean.lwr <- stmean.predict[,2]
stmean.upr <- stmean.predict[,3]
lower <- stmean.lwr
upper <- stmean.upr

lower <- as.data.frame(cbind(lower, lower))
lower <- melt(lower, value.name = "lower")

upper <- as.data.frame(cbind(upper, upper))
upper <- melt(upper, value.name = "upper")

NSE(predicted, observed)
mae(predicted, observed)
dwtest(stmean.lm)

stmean.lmdf <- cbind.data.frame(dt, observed, predicted)
stmean.lmdf <- melt(stmean.lmdf, id.vars = "dt",
  variable.name = "ovp", value.name = "st.mean")
stmean.lmdf <- cbind.data.frame(stmean.lmdf, lower, upper)

ggplot(stmean.lmdf, aes(x = dt, y = st.mean, colour = ovp)) + 
  geom_line(lwd = 1) + 
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = "grey") +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = "grey") +
  labs(x = "Time", y = "Stream temperature (ºC)",
    title = "Post-harvest daily mean stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "stmeanlm_ovp.png")

#####################################
# linear model: st min
#####################################

stmin.lm <- lm(st.min ~ log(q.min) + at.min + sin.doy + cos.doy, data = ag.preharvest, na.action = na.exclude)
summary(stmin.lm)

stmin.res <- residuals(stmin.lm)

hist(stmin.res)
qqnorm(stmin.res)

stmin.predict <- predict.lm(stmin.lm, newdata = ag.postharvest, 
  interval = "prediction", level = 0.95)
stmin.preds <- stmin.predict[,1]

observed <- ag.postharvest$st.min
predicted <- stmin.preds

stmin.lwr <- stmin.predict[,2]
stmin.upr <- stmin.predict[,3]
lower <- stmin.lwr
upper <- stmin.upr

lower <- as.data.frame(cbind(lower, lower))
lower <- melt(lower, value.name = "lower")

upper <- as.data.frame(cbind(upper, upper))
upper <- melt(upper, value.name = "upper")

NSE(predicted, observed)
mae(predicted, observed)
dwtest(stmin.lm)

stmin.lmdf <- cbind.data.frame(dt, observed, predicted)
stmin.lmdf <- melt(stmin.lmdf, id.vars = "dt",
  variable.name = "ovp", value.name = "st.min")
stmin.lmdf <- cbind.data.frame(stmin.lmdf, lower, upper)

ggplot(stmin.lmdf, aes(x = dt, y = st.min, colour = ovp)) + 
  geom_line(lwd = 1) + 
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = "grey") +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = "grey") +
  labs(x = "Time", y = "Stream temperature (ºC)",
    title = "Post-harvest daily minimum stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "stminlm_ovp.png")

#####################################
# linear model: st max
#####################################

stmax.lm <- lm(st.max ~ log(q.max) + at.max + sin.doy + cos.doy, data = ag.preharvest, na.action = na.exclude)
summary(stmax.lm)

stmax.predict <- predict.lm(stmax.lm, newdata = ag.postharvest, 
  interval = "prediction", level = 0.95)
stmax.preds <- stmax.predict[,1]

observed <- ag.postharvest$st.max
predicted <- stmax.preds

stmax.lwr <- stmax.predict[,2]
stmax.upr <- stmax.predict[,3]
lower <- stmax.lwr
upper <- stmax.upr

lower <- as.data.frame(cbind(lower, lower))
lower <- melt(lower, value.name = "lower")

upper <- as.data.frame(cbind(upper, upper))
upper <- melt(upper, value.name = "upper")

NSE(predicted, observed)
mae(predicted, observed)
dwtest(stmax.lm)

stmax.lmdf <- cbind.data.frame(dt, observed, predicted)
stmax.lmdf <- melt(stmax.lmdf, id.vars = "dt",
  variable.name = "ovp", value.name = "st.max")
stmax.lmdf <- cbind.data.frame(stmax.lmdf, lower, upper)

ggplot(stmax.lmdf, aes(x = dt, y = st.max, colour = ovp)) + 
  geom_line(lwd = 1) + 
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = "grey") +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = "grey") +
  labs(x = "Time", y = "Stream temperature (ºC)",
    title = "Post-harvest daily maximum stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "stmaxlm_ovp.png")

#####################################
# linear model: stram temp (cr.df.x)
#####################################

st.lm <- lm(stream.temp ~ log(q) + air.temp + sin.doy + cos.doy, data = preharvest.x)
summary(st.lm)

st.predict <- predict.lm(st.lm, newdata = postharvest.x, 
  interval = "prediction", level = 0.95)
st.preds <- st.predict[,1]

observed <- postharvest.x$stream.temp
predicted <- st.preds

st.lmdf <- cbind.data.frame(date.time, observed, predicted)
st.lmdf <- melt(st.lmdf, id.vars = "date.time",
  variable.name = "ovp", value.name = "st")

ggplot(st.lmdf, aes(x = date.time, y = st, colour = ovp)) + 
  geom_line(lwd = 1) + 
  labs(x = "Time", y = "Stream temperature (ºC)",
    title = "Post-harvest stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "stlm_observedpredicted.png")

