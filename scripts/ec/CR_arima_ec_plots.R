cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##################################################################
# Ljung-Box tests - http://robjhyndman.com/hyndsight/ljung-box-test/
##################################################################
Box.test(ec.mean.res, lag = 10, type = "Ljung-Box", fitdf = 1)
Box.test(ec.min.res, lag = 10, type = "Ljung-Box", fitdf = 1)
Box.test(ec.max.res, lag = 10, type = "Ljung-Box", fitdf = 1)

##################################################################
# residual diagnostic plots: mean EC
##################################################################
ec.mean.res <- residuals(ec.mean.arima)

hist(ec.mean.res, main = "Distribution of residuals (mean electrical conductivity)", xlab = "Residuals")

qqnorm(ec.mean.res, main = "Normal probability plot of residuals (mean electrical conductivity)")
qqline(ec.mean.res, col = "red")

acf(ec.mean.res, na.action = na.exclude, main = "ACF of residuals (mean electrical conductivity)")

##################################################################
# residual diagnostic plots: maximum EC
##################################################################
ec.max.res <- residuals(ec.max.arima)

hist(ec.max.res, main = "Distribution of residuals (maximum electrical conductivity)", xlab = "Residuals")

qqnorm(ec.max.res, main = "Normal probability plot of residuals (maximum electrical conductivity)")
qqline(ec.max.res, col = "red")

acf(ec.max.res, na.action = na.exclude, main = "ACF of residuals (maximum electrical conductivity)")

##################################################################
# residual diagnostic plots: minimum EC
##################################################################
ec.min.res <- residuals(ec.min.arima)

hist(ec.min.res, main = "Distribution of residuals (minimum electrical conductivity)", xlab = "Residuals")

qqnorm(ec.min.res, main = "Normal probability plot of residuals (minimum electrical conductivity)")
qqline(ec.min.res, col = "red")

acf(ec.min.res, na.action = na.exclude, main = "ACF of residuals (minimum electrical conductivity)")

######################################################################
# observed minus predicted for whole dataset
######################################################################

###mean EC###
pre.predicted <- fitted(ec.mean.arima)
pre.observed <- ag.preharvest$ec.mean
predicted <- ec.mean.predvals
observed <- ag.postharvest$ec.mean

predicted <- c(pre.predicted, predicted)
observed <- c(pre.observed, observed)
difference <- observed - predicted

ovp.df <- cbind.data.frame(ag.df$dt, difference)
colnames(ovp.df) <- c("dt", "difference")

ggplot(ovp.df, aes(x = dt, y = difference)) + 
	geom_line(lwd = 0.5) +
	labs(x = "Time (year)", y = "Difference",
		title = "Observed minus predicted daily mean electrical conductivity")

ggsave(filename = "ecmeanarima_omp.png")

###minimum EC###
pre.predicted <- fitted(ec.min.arima)
pre.observed <- ag.preharvest$ec.min
predicted <- ec.mean.predvals
observed <- ag.postharvest$ec.min

predicted <- c(pre.predicted, predicted)
observed <- c(pre.observed, observed)
difference <- observed - predicted

ovp.df <- cbind.data.frame(ag.df$dt, difference)
colnames(ovp.df) <- c("dt", "difference")

ggplot(ovp.df, aes(x = dt, y = difference)) + 
	geom_line(lwd = 0.5) +
	labs(x = "Time (year)", y = "Difference",
		title = "Observed minus predicted daily minimum electrical conductivity")

ggsave(filename = "ecminarima_omp.png")

###minimum EC###
pre.predicted <- fitted(ec.max.arima)
pre.observed <- ag.preharvest$ec.max
predicted <- ec.max.predvals
observed <- ag.postharvest$ec.max

predicted <- c(pre.predicted, predicted)
observed <- c(pre.observed, observed)
difference <- observed - predicted

ovp.df <- cbind.data.frame(ag.df$dt, difference)
colnames(ovp.df) <- c("dt", "difference")

ggplot(ovp.df, aes(x = dt, y = difference)) + 
	geom_line(lwd = 0.5) +
	labs(x = "Time (year)", y = "Difference",
		title = "Observed minus predicted daily maximum electrical conductivity")

ggsave(filename = "ecmaxarima_omp.png")

######################################################################
# pre-harvest: observed versus predicted
######################################################################

### mean EC ###
pre.predicted <- fitted(ec.mean.arima)
pre.observed <- ag.preharvest$ec.mean
pre.dt <- ag.preharvest$dt

NSE(pre.predicted, pre.observed)

pre.df <- cbind.data.frame(pre.dt, pre.observed, pre.predicted)
pre.df <- melt(pre.df, id.vars = "pre.dt",
	variable.name = "ovp", value.name = "ec")

ggplot(pre.df, aes(x = pre.dt, y = ec, colour = ovp)) + 
	geom_line(lwd = 1) +
	labs(x = "Time (year)", y = "Electrical conductivity (μS)",
		title = "Pre-harvest daily mean electrical conductivity") +
	scale_colour_manual(labels = c("Observed", "Predicted"), 
		values = c(cbPalette[6], cbPalette[8])) +
	scale_linetype_manual(values = c(1, 1)) +
	theme(legend.title=element_blank())

ggsave(filename = "preecmeanarima_ovp.png")

### minimum EC ###
pre.predicted <- fitted(ec.min.arima)
pre.observed <- ag.preharvest$ec.min
pre.dt <- ag.preharvest$dt

NSE(pre.predicted, pre.observed)

pre.df <- cbind.data.frame(pre.dt, pre.observed, pre.predicted)
pre.df <- melt(pre.df, id.vars = "pre.dt",
	variable.name = "ovp", value.name = "ec")

ggplot(pre.df, aes(x = pre.dt, y = ec, colour = ovp)) + 
	geom_line(lwd = 1) +
	labs(x = "Time (year)", y = "Electrical conductivity (μS)",
		title = "Pre-harvest daily minimum electrical conductivity") +
	scale_colour_manual(labels = c("Observed", "Predicted"), 
		values = c(cbPalette[6], cbPalette[8])) +
	scale_linetype_manual(values = c(1, 1)) +
	theme(legend.title=element_blank())

ggsave(filename = "preecminarima_ovp.png")

### maximum EC ###
pre.predicted <- fitted(ec.max.arima)
pre.observed <- ag.preharvest$ec.max
pre.dt <- ag.preharvest$dt

NSE(pre.predicted, pre.observed)

pre.df <- cbind.data.frame(pre.dt, pre.observed, pre.predicted)
pre.df <- melt(pre.df, id.vars = "pre.dt",
	variable.name = "ovp", value.name = "ec")

ggplot(pre.df, aes(x = pre.dt, y = ec, colour = ovp)) + 
	geom_line(lwd = 1) +
	labs(x = "Time (year)", y = "Electrical conductivity (μS)",
		title = "Pre-harvest daily maximum electrical conductivity") +
	scale_colour_manual(labels = c("Observed", "Predicted"), 
		values = c(cbPalette[6], cbPalette[8])) +
	scale_linetype_manual(values = c(1, 1)) +
	theme(legend.title=element_blank())

ggsave(filename = "preecmaxarima_ovp.png")

######################################################################
# post-harvest: observed versus predicted
######################################################################

### mean EC ###
dt <- ag.postharvest$dt
observed <- ag.postharvest$ec.mean
predicted <- ec.mean.predvals

post.df <- cbind.data.frame(dt, observed, predicted)
post.df <- melt(post.df, id.vars = "dt",
	variable.name = "ovp", value.name = "ec.mean")

ggplot(post.df, aes(x = dt, y = ec.mean, colour = ovp)) + 
	geom_line(lwd = 1) + 
	labs(x = "Time", y = "Electrical conductivity (μS)",
		title = "Post-harvest daily mean electrical conductivity") +
	scale_colour_manual(labels = c("Observed", "Predicted"), 
		values = c(cbPalette[6], cbPalette[8])) +
	scale_linetype_manual(values = c(1, 1)) +
	theme(legend.title=element_blank())

ggsave(filename = "ecmeanarima_ovp.png")

### minimum EC ###
dt <- ag.postharvest$dt
observed <- ag.postharvest$ec.min
predicted <- ec.min.predvals

post.df <- cbind.data.frame(dt, observed, predicted)
post.df <- melt(post.df, id.vars = "dt",
	variable.name = "ovp", value.name = "ec.min")

ggplot(post.df, aes(x = dt, y = ec.min, colour = ovp)) + 
	geom_line(lwd = 1) + 
	labs(x = "Time", y = "Electrical conductivity (μS)",
		title = "Post-harvest daily minimum electrical conductivity") +
	scale_colour_manual(labels = c("Observed", "Predicted"), 
		values = c(cbPalette[6], cbPalette[8])) +
	scale_linetype_manual(values = c(1, 1)) +
	theme(legend.title=element_blank())

ggsave(filename = "ecminarima_ovp.png")

### maximum EC ###
dt <- ag.postharvest$dt
observed <- ag.postharvest$ec.max
predicted <- ec.max.predvals

post.df <- cbind.data.frame(dt, observed, predicted)
post.df <- melt(post.df, id.vars = "dt",
	variable.name = "ovp", value.name = "ec.max")

ggplot(post.df, aes(x = dt, y = ec.max, colour = ovp)) + 
	geom_line(lwd = 1) + 
	labs(x = "Time", y = "Electrical conductivity (μS)",
		title = "Post-harvest daily maximum electrical conductivity") +
	scale_colour_manual(labels = c("Observed", "Predicted"), 
		values = c(cbPalette[6], cbPalette[8])) +
	scale_linetype_manual(values = c(1, 1)) +
	theme(legend.title=element_blank())

ggsave(filename = "ecmaxarima_ovp.png")

### EC ###

dt <- postharvest.x$date.time
observed <- postharvest.x$ec
predicted <- ec.predvals

post.df <- cbind.data.frame(dt, observed, predicted)
post.df <- melt(post.df, id.vars = "dt",
	variable.name = "ovp", value.name = "ec")

ggplot(post.df, aes(x = dt, y = ec, colour = ovp)) + 
	geom_line(lwd = 1) + 
	labs(x = "Time", y = "Electrical conductivity (μS)",
		title = "Post-harvest electrical conductivity") +
	scale_colour_manual(labels = c("Observed", "Predicted"), 
		values = c(cbPalette[6], cbPalette[8])) +
	scale_linetype_manual(values = c(1, 1)) +
	theme(legend.title=element_blank())

ggsave(filename = "ecmaxarima_ovp.png")
