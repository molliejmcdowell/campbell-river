################################
# melt ag.df for EC plots
################################
ec.pre <- cbind.data.frame(ag.preharvest$dt, ag.preharvest$ec.mean, ag.preharvest$q.mean)
list1 <- rep("preharvest", length(1:650))
ec.pre <- cbind.data.frame(ec.pre, list1)
names(ec.pre)[names(ec.pre) == "ag.preharvest$dt"] = "dt"
names(ec.pre)[names(ec.pre) == "ag.preharvest$ec.mean"] = "ec.mean"
names(ec.pre)[names(ec.pre) == "ag.preharvest$q.mean"] = "q.mean"
names(ec.pre)[names(ec.pre) == "list1"] = "prepost"

ec.post <- cbind.data.frame(ag.postharvest$dt, ag.postharvest$ec.mean, ag.postharvest$q.mean)
list1 <- rep("postharvest", length(1:1334))
ec.post <- cbind.data.frame(ec.post, list1)
names(ec.post)[names(ec.post) == "ag.postharvest$dt"] = "dt"
names(ec.post)[names(ec.post) == "ag.postharvest$ec.mean"] = "ec.mean"
names(ec.post)[names(ec.post) == "ag.postharvest$q.mean"] = "q.mean"
names(ec.post)[names(ec.post) == "list1"] = "prepost"

ec.df <- rbind.data.frame(ec.pre, ec.post)

#####################################
# subsetting winter
#####################################

ag.winter <- subset(ec.df, format(dt,'%m') %in% c("11", "12", "01", "02", "03"))
ag.winter <- ag.winter[-(757:776),]
#194 - 344, 559 - 709, 924 - 1075, 1290 - 1440, 1655 - 1805

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

#####################################
# subsetting summer
#####################################

ag.summer <- subset(ec.df, format(dt,'%m') %in% c("06", "07", "08", "09"))

S2009 <- ag.summer$ec.mean[1:122]
S2010 <- ag.summer$ec.mean[123:244]
S2011 <- ag.summer$ec.mean[245:366]
S2012 <- ag.summer$ec.mean[367:488]
S2013 <- ag.summer$ec.mean[489:610]
S2014 <- ag.summer$ec.mean[611:732]

summer <- cbind(S2009, S2010, S2011, S2012, S2013, S2014)
summer <- as.data.frame(summer)
summer <- melt(summer)
summer <- cbind(ag.summer$dt, summer)
names(summer)[names(summer) == "ag.summer.x$dt"] = "dt"
names(summer)[names(summer) == "variable"] = "summer"
names(summer)[names(summer) == "value"] = "ec.mean"

########################################################
# flow weighted means for pre and post harvest
########################################################

wt.mean(preharvest$ec, preharvest$q)
wt.mean(postharvest$ec, postharvest$q)

wt.var(preharvest$ec, preharvest$q)
wt.var(postharvest$ec, postharvest$q)

wt.sd(preharvest$ec, preharvest$q)
wt.sd(postharvest$ec, postharvest$q)

########################################################
# flow weighted means for winter pre and post harvest
########################################################

pre.winter <- subset(preharvest, format(date.time,'%m') %in% c("11", "12", "01", "02", "03"))
post.winter <- subset(postharvest, format(date.time,'%m') %in% c("11", "12", "01", "02", "03"))

wt.mean(pre.winter$ec, pre.winter$q)
wt.mean(post.winter$ec, post.winter$q)

wt.var(pre.winter$ec, pre.winter$q)
wt.var(post.winter$ec, post.winter$q)

wt.sd(pre.winter$ec, pre.winter$q)
wt.sd(post.winter$ec, post.winter$q)

########################################################
# flow weighted means for summer pre and post harvest
########################################################

pre.summer <- subset(preharvest, format(date.time,'%m') %in% c("06", "07", "09"))
post.summer <- subset(postharvest, format(date.time,'%m') %in% c("06", "07", "09"))

wt.mean(pre.summer$ec, pre.summer$q)
wt.mean(post.summer$ec, post.summer$q)

wt.var(pre.summer$ec, pre.summer$q)
wt.var(post.summer$ec, post.summer$q)

wt.sd(pre.summer$ec, pre.summer$q)
wt.sd(post.summer$ec, post.summer$q)

########################################################
# flow weighted means for each summer
########################################################

summer2009 <- subset(pre.summer, format(date.time,'%Y') %in% c("2009"))
summer2010 <- subset(pre.summer, format(date.time,'%Y') %in% c("2010"))
summer2011 <- subset(post.summer, format(date.time,'%Y') %in% c("2011"))
summer2012 <- subset(post.summer, format(date.time,'%Y') %in% c("2012"))
summer2013 <- subset(post.summer, format(date.time,'%Y') %in% c("2013"))
summer2014 <- subset(post.summer, format(date.time,'%Y') %in% c("2014"))

wt.mean(summer2009$ec, summer2009$q)
wt.mean(summer2010$ec, summer2010$q)
wt.mean(summer2011$ec, summer2011$q)
wt.mean(summer2012$ec, summer2012$q)
wt.mean(summer2013$ec, summer2013$q)
wt.mean(summer2014$ec, summer2014$q)

wt.var(summer2009$ec, summer2009$q)
wt.var(summer2010$ec, summer2010$q)
wt.var(summer2011$ec, summer2011$q)
wt.var(summer2012$ec, summer2012$q)
wt.var(summer2013$ec, summer2013$q)
wt.var(summer2014$ec, summer2014$q)

wt.sd(summer2009$ec, summer2009$q)
wt.sd(summer2010$ec, summer2010$q)
wt.sd(summer2011$ec, summer2011$q)
wt.sd(summer2012$ec, summer2012$q)
wt.sd(summer2013$ec, summer2013$q)
wt.sd(summer2014$ec, summer2014$q)

########################################################
# flow weighted means for each winter
########################################################

winter2009 <- subset(pre.winter, format(date.time,'%Y') %in% c("2009"))
winter2010 <- subset(pre.winter, format(date.time,'%Y') %in% c("2010"))
winter2011 <- subset(post.winter, format(date.time,'%Y') %in% c("2011"))
winter2012 <- subset(post.winter, format(date.time,'%Y') %in% c("2012"))
winter2013 <- subset(post.winter, format(date.time,'%Y') %in% c("2013"))
winter2014 <- subset(post.winter, format(date.time,'%Y') %in% c("2014"))

wt.mean(winter2009$ec, winter2009$q)
wt.mean(winter2010$ec, winter2010$q)
wt.mean(winter2011$ec, winter2011$q)
wt.mean(winter2012$ec, winter2012$q)
wt.mean(winter2013$ec, winter2013$q)
wt.mean(winter2014$ec, winter2014$q)

wt.var(winter2009$ec, winter2009$q)
wt.var(winter2010$ec, winter2010$q)
wt.var(winter2011$ec, winter2011$q)
wt.var(winter2012$ec, winter2012$q)
wt.var(winter2013$ec, winter2013$q)
wt.var(winter2014$ec, winter2014$q)

wt.sd(winter2009$ec, winter2009$q)
wt.sd(winter2010$ec, winter2010$q)
wt.sd(winter2011$ec, winter2011$q)
wt.sd(winter2012$ec, winter2012$q)
wt.sd(winter2013$ec, winter2013$q)
wt.sd(winter2014$ec, winter2014$q)
