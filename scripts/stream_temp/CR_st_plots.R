cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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


####################
# ECDF plot
####################

ggplot(preharvest, aes(stream.temp)) +
  stat_ecdf(mapping = aes(stream.temp, colour = cbPalette[2]),
  data = preharvest, n = NULL, geom = "line", size = 1.5) +
  stat_ecdf(mapping = aes(stream.temp, colour = cbPalette[4]),
  data = postharvest, n = NULL, geom = "line", size = 1.5) +
  labs(x = "Stream Temperature (ºC)") + labs(y = "Cumulative Density") + 
  labs(title = "Stream Temperature") +
  coord_cartesian(xlim = c(0, 10)) + 
  scale_colour_manual(labels = c("Pre-harvest", "Post-harvest"),
  values = c(cbPalette[2], cbPalette[4])) +
  scale_linetype_manual(values = c(2, 1)) + #guides(col = guide_legend(reverse = TRUE)) + 
  theme_cowplot() + theme(legend.title=element_blank()) + 
  coord_flip(xlim = c(-10, 30), ylim = c(-0.01,1.01))

ggsave(filename = "st_ecdf.png")

