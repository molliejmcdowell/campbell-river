###############################################
# ECDF plots
###############################################

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(preharvest, aes(stream.temp)) +
	stat_ecdf(mapping = aes(stream.temp, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(stream.temp, colour = cbPalette[4]),
	data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x = "Stream Temperature") + labs(y = "Cumulative Density") + labs(title = "Stream Temperature") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2], cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim = c(-10, 30), ylim = c(-0.01,1.01))

ggsave(filename = "st_ecdf.png")

ggplot(preharvest, aes(ec)) +
	stat_ecdf(mapping = aes(ec, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(ec, colour = cbPalette[4]),
	data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x = "Electrical Conductivity (µS)", y = "Cumulative Density", title = "Electrical Conductivity") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 100), ylim=c(-0.01,1.01))

ggsave(filename = "ec_ecdf.png")

ggplot(preharvest, aes(q)) +
	stat_ecdf(mapping = aes(q, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(q, colour = cbPalette[4]),
	data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Discharge") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 300), ylim=c(-0.01,1.01))

ggsave(filename = "q_ecdf.png")

ggplot(preharvest, aes(log(q))) +
	stat_ecdf(mapping = aes(log(q), colour = cbPalette[2]),
    data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(log(q), colour = cbPalette[4]),
    data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Log Discharge") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(-2, 6), ylim=c(-0.01,1.01))

ggsave(filename = "logq_ecdf.png")

ggplot(preharvest, aes(log(bf))) +
	stat_ecdf(mapping = aes(log(bf), colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(log(bf), colour = cbPalette[4]),
	data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Log Baseflow") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"),
	  values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(-2, 6), ylim=c(-0.01,1.01))

ggsave(filename = "logbf_ecdf.png")

##############################################################
# comparing variables in preharvest and postharvest periods
##############################################################

ggplot(ag.df, aes(q.mean, bf.mean)) + geom_point()

ggplot(ag.preharvest, aes(log(q.mean), bf.mean)) +
	geom_point(aes(colour = cbPalette[1])) +
	geom_smooth(aes(colour = cbPalette[2]))

ggplot(ag.postharvest, aes(log(q.mean), bf.mean)) +
	geom_point(aes(colour = cbPalette[4])) +
	geom_smooth(aes(colour = cbPalette[6]))

ggplot(ag.df, aes(log(q.mean), st.mean)) + geom_point()

ggplot(ag.df, aes(q.mean, ec.mean)) + geom_point()

ggplot(ag.df, aes(bf.mean, ec.mean)) + geom_point()

ggplot(cr.df, aes(log(bf), ec)) + geom_point() + geom_smooth()

###############################################################
# ARIMA plots for stream temperature
##############################################################

# mean stream temp
ggplot(ag.postharvest, aes(x = dt, y = st.mean)) +
  geom_line(lwd = 1, col = cbPalette[6]) + 
  geom_line(aes(x = ag.postharvest$dt, y = stmean.predvals), lwd = 1, col = cbPalette[8]) +
  labs(x = "Time", y = "Stream temperature (ºC)", 
    title = "Post-harvest mean stream temperature")

ggsave(filename = "meanst_observedvpredicted.png")

# min stream temp
ggplot(ag.postharvest, aes(x = dt, y = st.min)) +
  geom_line(lwd = 1, col = cbPalette[6]) + 
  geom_line(aes(x = ag.postharvest$dt, y = stmin.predvals), lwd = 1, col = cbPalette[8]) +
  labs(x = "Time", y = "Stream temperature (ºC)", 
    title = "Post-harvest minimum stream temperature")

ggsave(filename = "minst_observedvpredicted.png")

# max stream temp
ggplot(ag.postharvest, aes(x = dt, y = st.max)) +
  geom_line(lwd = 1, col = cbPalette[6]) + 
  geom_line(aes(x = ag.postharvest$dt, y = stmax.predvals), lwd = 1, col = cbPalette[8]) +
  labs(x = "Time", y = "Maximum stream temperature (ºC)", 
    title = "Post-harvest maximum stream temperature")

ggsave(filename = "maxst_observedvpredicted.png")

# mean stream temp with baseflow
ggplot(ag.postharvest, aes(x = dt, y = st.mean)) +
  geom_line(lwd = 1, col = cbPalette[6]) + 
  geom_line(aes(x = ag.postharvest$dt, y = stmean.predvals), lwd = 1, col = cbPalette[8]) +
  labs(x = "Time", y = "Mean stream temperature (ºC)", 
    title = "Post-harvest mean stream temperature") 

ggsave(filename = "meanstbf_observedvpredicted.png")

# min stream temp with baseflow
ggplot(ag.postharvest, aes(x = dt, y = st.min)) +
	geom_line(lwd = 1, col = cbPalette[6]) + 
	geom_line(aes(x = ag.postharvest$dt, y = stmin.predvals), lwd = 1, col = cbPalette[8]) +
	labs(x = "Time", y = "Minimum stream temperature (ºC)", 
			 title = "Pot-harvest minimum stream temperature")

ggsave(filename = "minstbf_observedvpredicted.png")

# max stream temp with baseflow
ggplot(ag.postharvest, aes(x = dt, y = st.max)) +
  geom_line(lwd = 1, col = cbPalette[6]) + 
  geom_line(aes(x = ag.postharvest$dt, y = stmax.predvals), lwd = 1, col = cbPalette[8]) +
  labs(x = "Time", y = "Post-harvest maximum stream temperature (ºC)", 
    title = "Observed versus predicted maximum stream temperature")

ggsave(filename = "maxstbf_observedvpredicted.png")

#####################################################
# time series of daily EC
#####################################################

# mean daily EC
ggplot(ec.df, aes(x = dt, y = ec.mean, col = prepost)) +
  geom_point() + 
  labs(x = "Time (year)", y = "Electrical conductivity (µS)", 
    title = "Daily mean electrical conductivity") +
  scale_colour_manual(labels = c("Preharvest", "Postharvest"), 
    values = c(cbPalette[2], cbPalette[3])) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "meandailyec.png")

# min daily EC
ggplot(ec.df, aes(x = dt, y = ec.min, col = prepost)) +
  geom_point() + 
  labs(x = "Time (year)", y = "Electrical conductivity (µS)", 
    title = "Daily minimum electrical conductivity") +
  scale_colour_manual(labels = c("Preharvest", "Postharvest"), 
    values = c(cbPalette[2], cbPalette[3])) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "mindailyec.png")

# max daily EC
ggplot(ec.df, aes(x = dt, y = ec.max, col = prepost)) +
  geom_point() + 
  labs(x = "Time (year)", y = "Electrical conductivity (µS)", 
    title = "Daily maximum electrical conductivity") +
  scale_colour_manual(labels = c("Preharvest", "Postharvest"), 
    values = c(cbPalette[2], cbPalette[3])) +
  theme(legend.title=element_blank()) + ylim(0,100)

ggsave(filename = "maxdailyec.png")

#####################################################
# winter daily EC means
#####################################################

# winter daily EC means
ggplot(ag.winter, aes(x = dt, y = ec.mean, col = prepost)) + geom_point() +
  labs(x = "Time: November - March (year)", y = "Electrical conductivity (µS)",
    title = "Daily mean electrical conductivity during winter") +
  scale_colour_manual(labels = c("Preharvest", "Postharvest"), 
    values = c(cbPalette[2], cbPalette[3])) +
  theme(legend.title=element_blank())

ggsave(filename = "winterdailyecmeans.png")

# fn to use to plot mean +/- 1 SE in ggplot2 
# (from http://stackoverflow.com/questions/25999677/how-to-plot-mean-and-standard-error-in-boxplot-in-r)
MinMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

# winter daily EC means with boxplots - change this plot
ggplot(winter, aes(x = winter, y = ec.mean)) + geom_point() +
  stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", color = cbPalette[2:6]) +
  labs(x = "Time: November - March (year)", y = "Electrical conductivity (µS)",
    title = "Daily mean electrical conductivity during winter")

ggsave(filename = "meandailyec_winterboxplots.png")

# plot of Tukey HSD
plot(ec.tukey)

#####################################################
# time series of daily stream temp
#####################################################

# mean daily st
ggplot(ag.df, aes(x = dt, y = st.mean)) +
  geom_point(col = cbPalette[3]) + geom_point(aes(x = dt, y = at.mean)) + 
	geom_point(aes(x = dt, y = log(q.mean)))
  labs(x = "Time", y = "Mean stream temperature (ºC)", 
    title = "Mean stream temperature ")

ggsave(filename = "meandailyst.png")

# min daily st
ggplot(ag.df, aes(x = dt, y = st.min)) +
  geom_point(col = cbPalette[3]) + 
  labs(x = "Time", y = "Minimum stream temperature (ºC)", 
    title = "Minimum stream temperature")

ggsave(filename = "mindailyst.png")

# max daily st
ggplot(ag.df, aes(x = dt, y = st.max)) +
  geom_point(col = cbPalette[3]) + 
  labs(x = "Time", y = "Maximum stream temperature (ºC)", 
    title = "Maximum stream temperature")

ggsave(filename = "maxdailyst.png")

ggplot(ag.df, aes(x = at.mean, y = st.mean)) + geom_point()

#####################################################
# EC versus baseflow and discharge
#####################################################

ggplot(ag.df, aes(x = log(q.mean), y = ec.mean)) + geom_point() + geom_smooth()
ggplot(ag.df, aes(x = log(bf.mean), y = ec.mean)) + geom_point() + geom_smooth()
