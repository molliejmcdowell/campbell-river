########################################
# linear regression model: EC
#######################################

#####################################
# prelim cleaning for new dfs
####################################
ag.postharvest <- subset.data.frame(ag.postharvest, dt <= as.POSIXct('2014-09-26 00:00:00'))
dt <- ag.postharvest$dt

date.time <- postharvest$date.time

#####################################
# linear model: EC mean
#####################################

ecmean.lm <- lm(ec.mean ~ log(q.mean) + sin.doy + cos.doy, data = ag.preharvest, na.action = na.exclude)
summary(ecmean.lm)

dwtest(ecmean.lm)

ecmean.res <- residuals(ecmean.lm)
ecmean.fits <- fitted(ecmean.lm)
pre.observed <- ag.preharvest$ec.mean
pre.dt <- ag.preharvest$dt

hist(ecmean.res)
qqnorm(ecmean.res)
qqline(ecmean.res)

ecmean.predict <- predict.lm(ecmean.lm, newdata = ag.postharvest, 
  interval = "prediction", level = 0.95)
ecmean.preds <- ecmean.predict[,1]

observed <- ag.postharvest$ec.mean
predicted <- ecmean.preds

ecmean.lwr <- ecmean.predict[,2]
ecmean.upr <- ecmean.predict[,3]
lower <- ecmean.lwr
upper <- ecmean.upr

lower <- as.data.frame(cbind(lower, lower))
lower <- melt(lower, value.name = "lower")

upper <- as.data.frame(cbind(upper, upper))
upper <- melt(upper, value.name = "upper")

NSE(predicted, observed)
mae(predicted, observed)

ecmean.lmdf <- cbind.data.frame(dt, observed, predicted)
ecmean.lmdf <- melt(ecmean.lmdf, id.vars = "dt",
  variable.name = "ovp", value.name = "ec.mean")
ecmean.lmdf <- cbind.data.frame(ecmean.lmdf, lower, upper)

ggplot(ecmean.lmdf, aes(x = dt, y = ec.mean, colour = ovp)) + 
  geom_line(lwd = 1) +
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = "grey") +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = "grey") +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily mean electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "ecmeanlm_ovp.png")

ggplot(ecmean.lmdf, aes(x = dt, y = ec.mean, colour = ovp)) + 
  geom_smooth() +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily mean electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "ecmeanlm_ovp_smooth.png")

ecmean.limitdf <- cbind.data.frame(dt, predicted, observed, upper, lower)

ggplot(ecmean.limitdf) + 
  geom_line(aes(x = dt, y = predicted), lwd = 1, col = cbPalette[8]) + 
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = cbPalette[1]) +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = cbPalette[1]) +
  ylim(0,100) +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily mean electrical conductivity")

ggsave(filename = "ecmeanlm_confidence.png")

ggplot(ecmean.limitdf) + 
  geom_line(aes(x = dt, y = predicted), lwd = 1, col = cbPalette[8]) + 
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = cbPalette[1]) +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = cbPalette[1]) +
  ylim(0,100) +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily mean electrical conductivity")

ggsave(filename = "ecmeanlm_prediction.png")

#####################################
# linear model: EC min
#####################################

ecmin.lm <- lm(ec.min ~ log(q.min) + sin.doy + cos.doy, data = ag.preharvest, na.action = na.exclude)
summary(ecmin.lm)

ecmin.res <- residuals(ecmin.lm)

hist(ecmin.res)
qqnorm(ecmin.res)
qqline(ecmin.res)

ecmin.predict <- predict.lm(ecmin.lm, newdata = ag.postharvest, 
  interval = "prediction", level = 0.95)
ecmin.preds <- ecmin.predict[,1]
ecmin.lwr <- ecmin.predict[,2]
ecmin.upr <- ecmin.predict[,3]

observed <- ag.postharvest$ec.min
predicted <- ecmin.preds

lower <- ecmin.lwr
upper <- ecmin.upr

lower <- as.data.frame(cbind(lower, lower))
lower <- melt(lower, value.name = "lower")

upper <- as.data.frame(cbind(upper, upper))
upper <- melt(upper, value.name = "upper")

NSE(predicted, observed)
mae(predicted, observed)

ecmin.lmdf <- cbind.data.frame(dt, observed, predicted)
ecmin.lmdf <- melt(ecmin.lmdf, id.vars = "dt",
  variable.name = "ovp", value.name = "ec.min")
ecmin.lmdf <- cbind.data.frame(ecmin.lmdf, upper, lower)

ggplot(ecmin.lmdf, aes(x = dt, y = ec.min, colour = ovp)) + 
  geom_line(lwd = 1) +
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = "grey") +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = "grey") +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily minimum electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "ecminlm_ovp.png")

ggplot(ecmin.lmdf, aes(x = dt, y = ec.min, colour = ovp)) + 
  geom_smooth() +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily minimum electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "ecminlm_ovp_smooth.png")

#####################################
# linear model: EC max
#####################################

ecmax.lm <- lm(ec.max ~ log(q.max) + sin.doy + cos.doy, data = ag.preharvest, na.action = na.exclude)
summary(ecmax.lm)

ecmax.res <- residuals(ecmax.lm)

hist(ecmax.res)
qqnorm(ecmax.res)

ecmax.predict <- predict.lm(ecmax.lm, newdata = ag.postharvest, 
  interval = "prediction", level = 0.95)
ecmax.preds <- ecmax.predict[,1]
ecmax.lwr <- ecmax.predict[,2]
ecmax.upr <- ecmax.predict[,3]

observed <- ag.postharvest$ec.max
predicted <- ecmax.preds

lower <- ecmax.lwr
upper <- ecmax.upr

lower <- as.data.frame(cbind(lower, lower))
lower <- melt(lower, value.name = "lower")

upper <- as.data.frame(cbind(upper, upper))
upper <- melt(upper, value.name = "upper")


NSE(predicted, observed)
mae(predicted, observed)

ecmax.lmdf <- cbind.data.frame(dt, observed, predicted)
ecmax.lmdf <- melt(ecmax.lmdf, id.vars = "dt",
  variable.name = "ovp", value.name = "ec.max")
ecmax.lmdf <- cbind.data.frame(ecmax.lmdf, upper, lower)

ggplot(ecmax.lmdf, aes(x = dt, y = ec.max, colour = ovp)) + 
  geom_line(lwd = 1) +
  geom_line(aes(x = dt, y = upper), lwd = 0.5, col = "grey") +
  geom_line(aes(x = dt, y = lower), lwd = 0.5, col = "grey") +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily maximum electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "ecmaxlm_ovp.png")

ggplot(ecmax.lmdf, aes(x = dt, y = ec.max, colour = ovp)) + 
  geom_smooth() +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest daily maximum electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "ecmaxlm_ovp_smooth.png")


#####################################
# linear model: EC (cr.df.x)
#####################################

ec.lm <- lm(ec ~ log(q) + log(bf) + sin.doy + cos.doy, data = preharvest.x)
summary(ec.lm)

ec.res <- residuals(ec.lm)

hist(ec.res)
qqnorm(ec.res)

ec.predict <- predict.lm(ec.lm, newdata = postharvest.x, 
  interval = "prediction", level = 0.95)
ec.preds <- ec.predict[,1]

observed <- postharvest$ec
predicted <- ec.preds

NSE(predicted, observed)
mae(predicted, observed)

ec.lmdf <- cbind.data.frame(date.time, observed, predicted)
ec.lmdf <- melt(ec.lmdf, id.vars = "date.time",
  variable.name = "ovp", value.name = "ec")

ggplot(ec.lmdf, aes(x = date.time, y = ec, colour = ovp)) + 
  geom_line(lwd = 1) + geom_smooth() +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
    title = "Post-harvest electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "eclm_observedpredicted.png")
