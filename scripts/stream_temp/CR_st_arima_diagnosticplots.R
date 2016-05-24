##################################################################
# ARIMA: diagnostic plots - mean stream temp
##################################################################
st.mean.res <- residuals(st.mean.arima)

hist(st.mean.res, main = "Distribution of residuals (mean stream temperature)", xlab = "Residuals")

qqnorm(st.mean.res, main = "Normal probability plot of residuals (mean stream temperature)")
qqline(st.mean.res, col = "red")

##################################################################
# ARIMA: diagnostic plots - maximum stream temp
##################################################################
st.max.res <- residuals(st.max.arima)

hist(st.max.res, main = "Distribution of residuals (maximum stream temperature)", xlab = "Residuals")

qqnorm(st.max.res, main = "Normal probability plot of residuals (maximum stream temperature)")
qqline(st.max.res, col = "red")

##################################################################
# ARIMA: diagnostic plots - minimum stream temp
##################################################################
st.min.res <- residuals(st.min.arima)

hist(st.min.res, main = "Distribution of residuals (minimum stream temperature)", xlab = "Residuals")

qqnorm(st.min.res, main = "Normal probability plot of residuals (minimum stream temperature)")
qqline(st.min.res, col = "red")

######################################################################
# ARIMA: time series plots of predicted and observed pre harvest data
######################################################################

pre.predicted <- fitted(st.max.arima)
pre.observed <- ag.preharvest$st.max
pre.dt <- ag.preharvest$dt

NSE(pre.predicted, pre.observed)

pre.df <- cbind.data.frame(pre.dt, pre.observed, pre.predicted)
pre.df <- melt(pre.df, id.vars = "pre.dt",
	variable.name = "ovp", value.name = "st")

ggplot(pre.df, aes(x = pre.dt, y = st, colour = ovp)) + 
  geom_line(lwd = 1) +
  labs(x = "Time (year)", y = "Stream temperature (ºC)",
    title = "Pre-harvest daily minimum stream temperature") +
  scale_colour_manual(labels = c("Observed", "Predicted"), 
    values = c(cbPalette[6], cbPalette[8])) +
  scale_linetype_manual(values = c(1, 1)) +
  theme(legend.title=element_blank())

ggsave(filename = "prestminarima_ovp.png")


######################################################################
# ARIMA/LM: observed versus predicted for entire dataset
######################################################################

pre.predicted <- fitted(stmin.lm)
pre.observed <- ag.preharvest$st.min
predicted <- stmin.predict[,1]
observed <- ag.postharvest$st.min

predicted <- c(pre.predicted, predicted)
observed <- c(pre.observed, observed)
difference <- observed - predicted

ovp.df <- cbind.data.frame(ag.df$dt, difference)
colnames(ovp.df) <- c("dt", "difference")

ggplot(ovp.df, aes(x = dt, y = difference)) + 
	geom_line(lwd = 0.5) +
	labs(x = "Time (year)", y = "Difference (ºC)",
		title = "Observed minus predicted daily minimum stream temperatures")

ggsave(filename = "stminlm_omp.png")

##################################################################
# ARIMA: Ljung-Box tests - http://robjhyndman.com/hyndsight/ljung-box-test/
##################################################################
Box.test(st.mean.res, lag = 10, type = "Ljung-Box", fitdf = 1)
Box.test(st.min.res, lag = 10, type = "Ljung-Box", fitdf = 1)
Box.test(st.max.res, lag = 10, type = "Ljung-Box", fitdf = 1)

##################################################################
# ARIMA: ACF plots of residuals
##################################################################
acf(st.mean.res, na.action = na.exclude, main = "ACF of residuals (mean stream temperature)")
acf(st.min.res, na.action = na.exclude, main = "ACF of residuals (minimum stream temperature)")
acf(st.max.res, na.action = na.exclude, main = "ACF of residuals (maximum stream temperature)")
