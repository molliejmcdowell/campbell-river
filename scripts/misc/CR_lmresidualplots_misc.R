# plot predicted and observed for the pre-harvest period

# Scatterplots of predicted versus residuals for lm EC

plot(fitted(ecmean.lm), residuals(ecmean.lm), 
  main = "Predicted daily mean EC values versus residuals",
  xlab = "Predicted daily mean EC values (µS)",
  ylab = "Residuals")

plot(fitted(ecmin.lm), residuals(ecmin.lm), 
  main = "Predicted daily minimum EC values versus residuals",
  xlab = "Predicted daily minimum EC values (µS)",
  ylab = "Residuals")

plot(fitted(ecmax.lm), residuals(ecmax.lm), 
		 main = "Predicted daily maximum EC values versus residuals",
		 xlab = "Predicted daily maximum EC values (µS)",
		 ylab = "Residuals")

# normal probability plots of residuals for lm EC

qqnorm(residuals(ecmean.lm),
  main = "Normal probability plot of residuals of daily mean EC prediction")
qqline(residuals(ecmean.lm), col = "red")

qqnorm(residuals(ecmax.lm),
  main = "Normal probability plot of residuals of daily maximum EC prediction")
qqline(residuals(ecmax.lm), col = "red")

qqnorm(residuals(ecmin.lm),
  main = "Normal probability plot of residuals of daily minimum EC prediction")
qqline(residuals(ecmin.lm), col = "red")

# histograms of residuals

hist(residuals(ecmean.lm),
  main = "Histogram of residuals of daily mean EC prediction",
  xlab = "Residuals")

hist(residuals(ecmax.lm),
  main = "Histogram of residuals of daily maximum EC prediction",
  xlab = "Residuals")

hist(residuals(ecmin.lm),
  main = "Histogram of residuals of daily minimum EC prediction",
  xlab = "Residuals")

# time series plots of residuals for the EC lm 

plot(pre.dt, ecmean.res,
  main = "Time-series of residuals of the daily mean EC prediction",
  ylab = "Residuals",
  xlab = "Time (year)")

plot(pre.dt, ecmax.res,
  main = "Time-series of residuals of the daily maximum EC prediction",
  ylab = "Residuals",
  xlab = "Time (year)")

plot(pre.dt, ecmin.res,
  main = "Time-series of residuals of the daily minimum EC prediction",
  ylab = "Residuals",
  xlab = "Time (year)")

# time series plots of predicted and observed pre harvest data

pre.predicted <- fitted(ecmax.lm)
pre.observed <- ag.preharvest$ec.max
pre.dt <- ag.preharvest$dt

pre.df <- cbind.data.frame(pre.dt, pre.observed, pre.predicted)
pre.df <- melt(pre.df, id.vars = "pre.dt",
  variable.name = "ovp", value.name = "ec")

ggplot(pre.df, aes(x = pre.dt, y = ec, colour = ovp)) + 
  geom_line(lwd = 1) +
  labs(x = "Time (year)", y = "Electrical conductivity (µS)",
     title = "Pre-harvest daily maximum electrical conductivity") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "preecmaxlm_ovp.png")


plot(pre.dt, ecmean.fits, lwd = 1)
lines(pre.dt, ec.pre.observed)

plot(predicted, observed)

plot(pre.dt, ecmean.fits-ec.pre.observed)
plot(pre.dt, ec.pre.observed-ecmean.fits)

plot(ecmean.res)
plot(ecmin.res)
plot(ecmax.res)
length(pre.dt)
length(ecmean.fits)


mean(preharvest$air.temp)
mean(postharvest$air.temp, na.rm = TRUE)
View(postharvest)
max(preharvest$air.temp)
max(postharvest$air.temp, na.rm = TRUE)
min(postharvest$air.temp, na.rm = TRUE)
min(preharvest$air.temp)
