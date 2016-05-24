cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(preharvest, aes(q)) +
  stat_ecdf(mapping = aes(q, colour = cbPalette[3]),
    data = preharvest, n = NULL, geom = "line", size = 1.5) +
  stat_ecdf(mapping = aes(q, colour = cbPalette[2]),
    data = postharvest, n = NULL, geom = "line", size = 1.5) +
  labs(x="Discharge (L/s)", y="Cumulative density", title="Cumulative density of discharge") +
  coord_cartesian(xlim = c(0, 10)) + 
  scale_colour_manual(labels = c("Pre-harvest", "Post-harvest"), values = c(cbPalette[2],cbPalette[3])) +
  #scale_linetype_manual(values = c(2, 1)) + 
  theme_cowplot() + theme(legend.title=element_blank()) + 
  coord_flip(xlim=c(0, 150), ylim=c(-0.01,1.01))

ggsave(filename = "q_ecdf.png")

ggplot(preharvest, aes(bf)) +
  stat_ecdf(mapping = aes(bf, colour = cbPalette[3]),
    data = preharvest, n = NULL, geom = "line", size = 1.5) +
  stat_ecdf(mapping = aes(bf, colour = cbPalette[2]),
    data = postharvest, n = NULL, geom = "line", size = 1.5) +
  labs(x="Baseflow (L/s)", y="Cumulative density", title="Cumulative density of baseflow") +
  coord_cartesian(xlim = c(0, 10)) + 
  scale_colour_manual(labels = c("Pre-harvest", "Post-harvest"),
    values = c(cbPalette[3],cbPalette[2])) +
  scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
  theme_cowplot() + theme(legend.title=element_blank()) + 
  coord_flip(xlim=c(0, 150), ylim=c(-0.01,1.01))

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

