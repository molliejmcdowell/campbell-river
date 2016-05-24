##################################################################
# LM: diagnostic plots - mean stream temp
##################################################################
stmean.res <- residuals(stmean.lm)
hist(stmean.res, main = "Distribution of residuals of the linear model (mean stream temperature)", xlab = "Residuals")

qqnorm(stmean.res, main = "Normal probability plot of residuals (mean stream temperature)")
qqline(stmean.res, col = "red")

##################################################################
# LM: diagnostic plots - maximum stream temp
##################################################################
stmax.res <- residuals(stmax.lm)

hist(stmax.res, main = "Distribution of residuals (maximum stream temperature)", xlab = "Residuals")

qqnorm(stmax.res, main = "Normal probability plot of residuals (maximum stream temperature)")
qqline(stmax.res, col = "red")

##################################################################
# LM: diagnostic plots - minimum stream temp
##################################################################
stmin.res <- residuals(stmin.lm)

hist(stmin.res, main = "Distribution of residuals (minimum stream temperature)", xlab = "Residuals")

qqnorm(stmin.res, main = "Normal probability plot of residuals (minimum stream temperature)")
qqline(stmin.res, col = "red")

######################################################################
# st LM: time series plots of predicted and observed pre harvest data
######################################################################

pre.predicted <- fitted(stmax.lm)
pre.observed <- ag.preharvest$st.max
pre.dt <- ag.preharvest$dt

NSE(pre.predicted,pre.observed)

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

ggsave(filename = "prestminlm_ovp.png")

