#############################################
# EC: monthly averages pre and post harvest
############################################

# month subsets
month <- strftime(cr.df$date.time, "%m")
cr.df$month <- floor_date(cr.df$date.time, "month")

st.mean.month <- aggregate(cr.df$st.gf, by = list(cr.df$month), FUN = mean, simplify = TRUE)
st.max.month <- aggregate(cr.df$st.gf, by = list(cr.df$month), FUN = max, simplify = TRUE)
st.min.month <- aggregate(cr.df$st.gf, by = list(cr.df$month), FUN = min, simplify = TRUE)

at.mean.month <- aggregate(cr.df$at.gf, by = list(cr.df$month), FUN = mean, simplify = TRUE)
at.max.month <- aggregate(cr.df$at.gf, by = list(cr.df$month), FUN = max, simplify = TRUE)
at.min.month <- aggregate(cr.df$at.gf, by = list(cr.df$month), FUN = min, simplify = TRUE)

q.mean.month <- aggregate(cr.df$q.gf, by = list(cr.df$month), FUN = mean, simplify = TRUE)
q.max.month <- aggregate(cr.df$q.gf, by = list(cr.df$month), FUN = max, simplify = TRUE)
q.min.month <- aggregate(cr.df$q.gf, by = list(cr.df$month), FUN = min, simplify = TRUE)

bf.mean.month <- aggregate(cr.df$bf.gf, by = list(cr.df$month), FUN = mean, simplify = TRUE)
bf.max.month <- aggregate(cr.df$bf.gf, by = list(cr.df$month), FUN = max, simplify = TRUE)
bf.min.month <- aggregate(cr.df$bf.gf, by = list(cr.df$month), FUN = min, simplify = TRUE)

ec.mean.month <- aggregate(cr.df$ec.gf, by = list(cr.df$month), FUN = mean, simplify = TRUE)
ec.max.month <- aggregate(cr.df$ec.gf, by = list(cr.df$month), FUN = max, simplify = TRUE)
ec.min.month <- aggregate(cr.df$ec.gf, by = list(cr.df$month), FUN = min, simplify = TRUE)

# store aggregated data in dataframe
ag.month.df <- cbind(st.mean.month, st.max.month, st.min.month, 
  at.mean.month, at.max.month, at.min.month, 
  q.mean.month, q.max.month, q.min.month, bf.mean.month, 
  bf.max.month, bf.min.month, ec.mean.month, ec.max.month, ec.min.month)

ag.month.df[3] <- NULL
ag.month.df[4] <- NULL
ag.month.df[5] <- NULL
ag.month.df[6] <- NULL
ag.month.df[7] <- NULL
ag.month.df[8] <- NULL
ag.month.df[9] <- NULL
ag.month.df[10] <- NULL
ag.month.df[11] <- NULL
ag.month.df[12] <- NULL
ag.month.df[13] <- NULL
ag.month.df[14] <- NULL
ag.month.df[15] <- NULL
ag.month.df[16] <- NULL

colnames(ag.month.df) <- c("dt", "st.mean", "st.max", "st.min", "at.mean", "at.max", "at.min", 
  "q.mean", "q.max", "q.min", "bf.mean", "bf.max", "bf.min", "ec.mean", "ec.max", "ec.min")

# preharvest and postharvest dataframes
ag.month.preharvest <- subset(ag.month.df, dt < as.POSIXct('2010-11-01 00:00:00'))
ag.month.postharvest <- subset(ag.month.df, dt >= as.POSIXct('2010-11-01 00:00:00'))

# scatterplot of daily ec
ggplot(ag.df, aes(x = dt, y = ec.mean)) +
  geom_point(col = cbPalette[1]) + 
  labs(x = "Time", y = "Mean electrical conductivity", 
    title = "Mean electrical conductivity")

ggplot(ag.df, aes(x = dt, y = ec.min)) +
  geom_point(col = cbPalette[1]) + 
  labs(x = "Time", y = "Minimum electrical conductivity", 
    title = "Minimum electrical conductivity")

ggplot(ag.df, aes(x = dt, y = ec.max)) +
  geom_point(col = cbPalette[1]) + 
  labs(x = "Time", y = "Maximum electrical conductivity", 
    title = "Maximum electrical conductivity")

ggplot(cr.df, aes(x = date.time, y = ec)) + geom_point()

# comparing months of various years
month <- strftime(ag.df$dt, "%m")
ag.df$month <- month(as.POSIXlt(ag.df$dt, format="%d/%m/%Y"))
ag.df$year <- year(as.POSIXlt(ag.df$dt, format="%d/%m/%Y"))
ag.df$day <- day(as.POSIXlt(ag.df$dt, format="%d/%m/%Y"))

ag.jan <- subset(ag.df, month == 1)
ag.feb <- subset(ag.df, month == 2)
ag.mar <- subset(ag.df, month == 3)
ag.apr <- subset(ag.df, month == 4)
ag.may <- subset(ag.df, month == 5)
ag.jun <- subset(ag.df, month == 6)
ag.jul <- subset(ag.df, month == 7)
ag.aug <- subset(ag.df, month == 8)
ag.sep <- subset(ag.df, month == 9)
ag.oct <- subset(ag.df, month == 10)
ag.nov <- subset(ag.df, month == 11)
ag.dec <- subset(ag.df, month == 12)

ag.winter <- subset(ag.df, format(dt,'%m') %in% c("11", "12", "01", "02", "03"))
ag.winter <- ag.winter[-(757:776),]
#194 - 344, 559 - 709, 924 - 1075, 1290 - 1440, 1655 - 1805

ggplot(ag.winter, aes(x = dt, y = ec.mean/bf.mean)) + geom_point()
ggplot(ag.winter, aes(x = log(bf.mean), y = ec.mean)) + geom_point()

# scatterplots of monthly data (daily means)
ggplot(ag.jan, aes(x = day, y = ec.mean/log(q.mean), group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.feb, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.mar, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.apr, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.may, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.jun, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.jul, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.aug, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.sep, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.oct, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.nov, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

ggplot(ag.dec, aes(x = day, y = ec.mean, group = year, color = year)) +
  geom_line(lwd = 1.5) + geom_point()

# other plots
ggplot(ag.df, aes(x = ec.mean, y = bf.mean/q.mean)) + geom_point()

#####################################
# subsetting ag.winter for boxplots
#####################################

W2010 <- ag.winter$ec.mean[1:151]
W2011 <- ag.winter$ec.mean[152:302]
W2012 <- ag.winter$ec.mean[303:453]
W2013 <- ag.winter$ec.mean[455:605]
W2014 <- ag.winter$ec.mean[606:756]

winter <- cbind(W2010, W2011, W2012, W2013, W2014)
winter <- as.data.frame(winter)
winter <- melt(winter)
ag.winter.x <- ag.winter[-454,]
winter <- cbind(ag.winter.x$dt, winter)
names(winter)[names(winter) == "ag.winter.x$dt"] = "dt"
names(winter)[names(winter) == "variable"] = "winter"
names(winter)[names(winter) == "value"] = "ec.mean"

################################
# ANOVA and Tukey HSD
################################

summary(ec.aov <- aov(ec.mean ~ winter, data = winter))
ec.tukey <- TukeyHSD(ec.aov, ordered = TRUE)

plot(ec.aov)

################################
# melt ag.df for EC plots
################################
ec.pre <- cbind.data.frame(ag.preharvest$dt, ag.preharvest$ec.max, 
  ag.preharvest$ec.min, ag.preharvest$ec.mean)
list1 <- rep("preharvest", length(1:559))
ec.pre <- cbind.data.frame(ec.pre, list1)
names(ec.pre)[names(ec.pre) == "ag.preharvest$dt"] = "dt"
names(ec.pre)[names(ec.pre) == "ag.preharvest$ec.mean"] = "ec.mean"
names(ec.pre)[names(ec.pre) == "ag.preharvest$ec.max"] = "ec.max"
names(ec.pre)[names(ec.pre) == "ag.preharvest$ec.min"] = "ec.min"
names(ec.pre)[names(ec.pre) == "list1"] = "prepost"

ec.post <- cbind.data.frame(ag.postharvest$dt, ag.postharvest$ec.max,
  ag.postharvest$ec.min, ag.postharvest$ec.mean)
list1 <- rep("postharvest", length(1:1480))
ec.post <- cbind.data.frame(ec.post, list1)
names(ec.post)[names(ec.post) == "ag.postharvest$dt"] = "dt"
names(ec.post)[names(ec.post) == "ag.postharvest$ec.mean"] = "ec.mean"
names(ec.post)[names(ec.post) == "ag.postharvest$ec.max"] = "ec.max"
names(ec.post)[names(ec.post) == "ag.postharvest$ec.min"] = "ec.min"
names(ec.post)[names(ec.post) == "list1"] = "prepost"

ec.df <- rbind.data.frame(ec.pre, ec.post)

