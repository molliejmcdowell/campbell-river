
#postharvest.2year <- subset(cr.df, date.time >= as.POSIXct('2011-2-01 00:00:00') & 
# date.time <= as.POSIXct('2012-11-01 00:00:00'))

#ag.postharvest.2year <- subset(ag.df, dt >= as.POSIXct('2011-1-01 00:00:00') & 
#  dt <= as.POSIXct('2012-11-01 00:00:00'))

ggplot(preharvest, aes(stream.temp)) +
	stat_ecdf(mapping = aes(stream.temp, colour = cbPalette[2]),
						data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(stream.temp, colour = cbPalette[4]),
						data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Stream Temperature", y="Cumulative Density", title="Stream Temperature (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(-10, 30), ylim=c(-0.01,1.01))

ggsave(filename = "st2yr_ecdf.png")

ggplot(preharvest, aes(ec)) +
	stat_ecdf(mapping = aes(ec, colour = cbPalette[2]),
						data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(ec, colour = cbPalette[4]),
						data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Electrical Conductivity", y="Cumulative Density", 
			 title="Electrical Conductivity (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 100), ylim=c(-0.01,1.01))

ggsave(filename = "ec2yr_ecdf.png")

ggplot(preharvest, aes(q)) +
	stat_ecdf(mapping = aes(q, colour = cbPalette[2]),
						data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(q, colour = cbPalette[4]),
						data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Discharge (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 300), ylim=c(-0.01,1.01))

ggsave(filename = "q2yr_ecdf.png")

ggplot(preharvest, aes(log(q))) +
	stat_ecdf(mapping = aes(log(q), colour = cbPalette[2]),
						data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(log(q), colour = cbPalette[4]),
						data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Discharge (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(-2, 6), ylim=c(-0.01,1.01))

ggsave(filename = "logq2yr_ecdf.png")

ggplot(preharvest, aes(log(bf))) +
	stat_ecdf(mapping = aes(log(bf), colour = cbPalette[2]),
						data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(log(bf), colour = cbPalette[4]),
						data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Log Baseflow (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(-2, 6), ylim=c(-0.01,1.01))

ggsave(filename = "logbf2yr_ecdf.png")

