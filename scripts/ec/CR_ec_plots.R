cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

###############################################
# regression plots
###############################################

p <- ggplot(preharvest, aes(x=q.gf, y=ec.gf)) +
  stat_smooth(data = preharvest, method = "lm", aes(x = q, y = ec, color = cbPalette[2])) +
  stat_smooth(data = postharvest, method = "lm", aes(x = q, y = ec, color = cbPalette[3])) +
  scale_color_manual(labels = c("Post-harvest", "Pre-harvest"), 
    values = c(cbPalette[3], cbPalette[2])) + theme(legend.title=element_blank()) +
  ylim(0,60) + labs(title = "Linear model of discharge versus electrical conductivity",
  y = "Electrical conductivity (µS)", x = "Discharge (L/s)")

ggsave(filename = "lm_ecvdischarge.png")

-.638/3.954
-.528/5.035

###############################################
# ECDF plots
###############################################

# pre event water and new water
# a double paradox in catchment hydrology and geochemistry - j kirchner
# what is the mean concentration for a unit of water
# USGS EGRET
# flow weighted mean and standard error for each period

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(preharvest, aes(ec/q)) +
  stat_ecdf(mapping = aes(ec/q, colour = cbPalette[2]),
    data = preharvest, n = NULL, geom = "line", size = 1.5) +
  stat_ecdf(mapping = aes(ec/q, colour = cbPalette[3]),
    data = postharvest, n = NULL, geom = "line", size = 1.5) +
  labs(x = "Electrical conductivity (µS) / discharge (L/s)", y = "Cumulative density",
    title = "Cumulative density of discharge-normalized electrical conductivity") +
  coord_cartesian(xlim = c(0, 10)) + 
  scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"),
    values = c(cbPalette[3],cbPalette[2])) +
  scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
  theme_cowplot() + theme(legend.title=element_blank()) + 
  coord_flip(xlim=c(0, 100), ylim=c(-0.01,1.01))

ggsave(filename = "ecq_ecdf.png")

ggplot(preharvest, aes(ec/bf)) +
  stat_ecdf(mapping = aes(ec/bf, colour = cbPalette[2]),
    data = preharvest, n = NULL, geom = "line", size = 1.5) +
  stat_ecdf(mapping = aes(ec/bf, colour = cbPalette[4]),
    data = postharvest, n = NULL, geom = "line", size = 1.5) +
  labs(x = "Electrical conductivity (µS) / baseflow (L/s)", y = "Cumulative density",
    title = "Cumulative density of baseflow-normalized electrical conductivity") +
  coord_cartesian(xlim = c(0, 10)) + 
  scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"),
    values = c(cbPalette[3],cbPalette[2])) +
  scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
  theme_cowplot() + theme(legend.title=element_blank()) + 
  coord_flip(xlim=c(0, 100), ylim=c(-0.01,1.01))

ggsave(filename = "ecbf_ecdf.png")

ggplot(preharvest, aes(ec/qf)) +
  stat_ecdf(mapping = aes(ec/qf, colour = cbPalette[2]),
    data = preharvest, n = NULL, geom = "line", size = 1.5) +
  stat_ecdf(mapping = aes(ec/qf, colour = cbPalette[4]),
    data = postharvest, n = NULL, geom = "line", size = 1.5) +
  labs(x = "Electrical conductivity (µS) / quickflow (L/s)", y = "Cumulative density",
    title = "Cumulative density of baseflow-normalized electrical conductivity") +
  coord_cartesian(xlim = c(0, 10)) + 
  scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), 
    values = c(cbPalette[3],cbPalette[2])) +
  scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
  theme_cowplot() + theme(legend.title=element_blank()) + 
  coord_flip(xlim=c(0, 2000), ylim=c(-0.01,1.01))

ggsave(filename = "ecqf_ecdf.png")

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
  v <- c(min(x), mean(x) - 2*sd(x)/sqrt(length(x)), mean(x), mean(x) + 2*sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

# winter daily EC means with boxplots - change this plot
ggplot(winter, aes(x = winter, y = ec.mean, col = winter)) + geom_jitter() +
  stat_summary(fun.data = MinMeanSEMMax, geom = "boxplot", color = cbPalette[2:6]) +
  labs(x = "Time: November - March (year)", y = "Electrical conductivity (µS)",
    title = "Daily mean electrical conductivity during winter") +
  scale_color_manual(values = cbPalette[2:6]) +
  theme(legend.position="none")

ggsave(filename = "meandailyec_winterboxplots.png")

ggplot(winter, aes(x = winter, y = ec.mean)) + #geom_point() +
  geom_boxplot(aes(group = winter)) +
  labs(x = "Time: November - March (year)", y = "Electrical conductivity (µS)",
    title = "Daily mean electrical conductivity during winter")

# plot of Tukey HSD
ggplot(ec.tukey, aes(x = comparison, y = diff, 
     ymin = lwr, ymax = upr, color=comparison)) +
  geom_pointrange(size=1) + coord_flip() + guides(color=FALSE) +
  labs(y = "Differences in mean EC by winter", x = "",
    title = "Tukey HSD for winter months") +
  scale_color_manual(values = cbPalette[2:5]) +
  title("Tukey HSD for winter months")

ggsave(filename = "ectukey.png")

#####################################################
# summer daily EC means
#####################################################

ggplot(ag.summer, aes(x = dt, y = ec.mean, col = prepost)) + geom_point() +
  labs(x = "Time: June - September (year)", y = "Electrical conductivity (µS)",
    title = "Daily mean electrical conductivity during summer") +
  scale_colour_manual(labels = c("Preharvest", "Postharvest"), 
    values = c(cbPalette[2], cbPalette[3])) +
  theme(legend.title=element_blank())

ggsave(filename = "summerdailyecmeans.png")

ggplot(summer, aes(x = summer, y = ec.mean)) + geom_point() +
  stat_summary(fun.data=MinMeanSEMMax, geom="boxplot", color = cbPalette[2:7]) +
  labs(x = "Time: June - September (year)", y = "Electrical conductivity (µS)",
    title = "Daily mean electrical conductivity during summer")

ggsave(filename = "meandailyec_summerboxplots.png")

ggplot(ag.summer, aes(x = bf.mean/q.mean, y = ec.mean, col = prepost)) + geom_point() +
  labs(x = "", y = "Electrical conductivity (µS)",
    title = "Daily mean electrical conductivity during summer") +
  scale_colour_manual(labels = c("Preharvest", "Postharvest"), 
    values = c(cbPalette[2], cbPalette[3])) +
  theme(legend.title=element_blank())

ggsave(filename = "summerdailyecmeans.png")

#####################################################
# EC versus baseflow and discharge
#####################################################

ggplot(ag.df, aes(x = q.mean, y = ec.mean)) + geom_point() + geom_smooth()
ggplot(ag.df, aes(x = bf.mean, y = ec.mean)) + geom_point() + geom_smooth()
