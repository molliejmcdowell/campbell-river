library(dplyr)
library(nlme)
library(EcoHydRology)
library(ggplot2)
library(stats)
library(car)
library(lmtest)
library(lubridate)
library(forecast)
library(cowplot)
library(np)

wq.data <- readRDS("CR_WQ30.2009_Apr2015.cleaning in prog 27Jan2016,mollie.RDS")

biomet.data <- readRDS("Biomet.2009_Apr2014,mollie.RDS")

# extract and create data.frame for relevant columns
names(wq.data)[names(wq.data) == "ECond.cleaned"] = "ec"
names(wq.data)[names(wq.data) == "Q.L.s.cleaned2X"] = "q"
names(wq.data)[names(wq.data) == "tv.30"] = "date.time"
names(wq.data)[names(wq.data) == "Tw"] = "stream.temp"

wq.data$Batt_V <- NULL
wq.data$DO <- NULL
wq.data$ORP <- NULL
wq.data$pH <- NULL
wq.data$CO2.GW.cleaned <- NULL
wq.data$CO2.SW.cleaned <- NULL

wq.subset <- wq.data[-(103157:109661), ]

names(biomet.data)[names(biomet.data) == "Tair"] = "air.temp"

cr.df <- cbind(wq.subset, biomet.data$air.temp)
cr.df <- as.data.frame(cr.df)
names(cr.df)[names(cr.df) == "biomet.data$air.temp"] = "air.temp"

# BaseflowSeparation

cr.df$q.gf <- ifelse(!is.na(cr.df$q), cr.df$q, mean(na.omit(cr.df$q)))
cr.df$bf.gf <- BaseflowSeparation(cr.df$q.gf, filter_parameter = 0.925, passes = 3)[,1] 
cr.df$bf <- ifelse(!is.na(cr.df$q), cr.df$bf.gf, NA)
cr.df$qf.gf <- BaseflowSeparation(cr.df$q.gf, filter_parameter = 0.925, passes = 3)[,2]
cr.df$qf <- ifelse(!is.na(cr.df$q), cr.df$qf.gf, NA)

# subsetting datetime by hourly, daily, weekly, monthly
day.df <- subset(cr.df, format(date.time,'%H')=='12')
day.df <- subset(day.df, format(date.time,'%M')=='00')
month.df <- subset(cr.df, format(date.time,'%d')=='01')
month.df <- subset(month.df, format(date.time,'%H')=='12')
month.df <- subset(month.df, format(date.time,'%M')=='00')
hour.df <- subset(cr.df, format(date.time,'%M')=='00')
week.df <- day.df[seq(1, nrow(day.df), 7), ]

# subset by pre and post harvest
preharvest <- subset(cr.df, date.time < as.POSIXct('2010-11-01 00:00:00'))
preharvest$pre.st <- preharvest$stream.temp
preharvest$pre.ec <- preharvest$ec
preharvest$pre.at <- preharvest$air.temp
preharvest$pre.q <- preharvest$q

postharvest <- subset(cr.df, date.time >= as.POSIXct('2010-11-01 00:00:00'))
postharvest$post.st <- postharvest$stream.temp
postharvest$post.ec <- postharvest$ec
postharvest$post.at <- postharvest$air.temp
postharvest$post.q <- postharvest$q

postharvest.2year <- subset(cr.df, date.time >= as.POSIXct('2010-11-01 00:00:00') & 
  date.time <= as.POSIXct('2012-11-01 00:00:00'))
postharvest.2year$post2yr.st <- postharvest.2year$stream.temp
postharvest.2year$post2yr.ec <- postharvest.2year$ec
postharvest.2year$post2yr.at <- postharvest.2year$air.temp
postharvest.2year$post2yr.q <- postharvest.2year$q

# aggregate mean, max, min by day
day <- strftime(cr.df$date.time, "%d")
cr.df$day <- floor_date(cr.df$date.time, "day")

st.mean <- aggregate(cr.df$stream.temp, by = list(cr.df$day), FUN = mean, simplify = TRUE)
st.max <- aggregate(cr.df$stream.temp, by = list(cr.df$day), FUN = max, simplify = TRUE)
st.min <- aggregate(cr.df$stream.temp, by = list(cr.df$day), FUN = min, simplify = TRUE)

at.mean <- aggregate(cr.df$air.temp, by = list(cr.df$day), FUN = mean, simplify = TRUE)
at.max <- aggregate(cr.df$air.temp, by = list(cr.df$day), FUN = max, simplify = TRUE)
at.min <- aggregate(cr.df$air.temp, by = list(cr.df$day), FUN = min, simplify = TRUE)

# store aggregated data in dataframe
ag.df <- cbind(st.mean, st.max, st.min, at.mean, at.max, at.min)
ag.df[3] <- NULL
ag.df[4] <- NULL
ag.df[5] <- NULL
ag.df[6] <- NULL
ag.df[7] <- NULL

colnames(ag.df) <- c("dt", "st.mean", "st.max", "st.min", "at.mean", "at.max", "at.min")

ag.df <- ag.df[-c(1:112), ]

# aggregated data for preharvest and postharvest dataframes

ag.preharvest <- subset(ag.df, dt < as.POSIXct('2010-11-01 00:00:00'))
ag.postharvest <- subset(ag.df, dt >= as.POSIXct('2010-11-01 00:00:00'))
ag.postharvest.2year <- subset(ag.df, dt >= as.POSIXct('2010-11-01 00:00:00') & 
  dt <= as.POSIXct('2012-11-01 00:00:00'))

# ECDF plots for entire postharvest period

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(preharvest, aes(pre.st)) +
	stat_ecdf(mapping = aes(pre.st, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(post.st, colour = cbPalette[4]),
	data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x = "Stream Temperature") + labs(y = "Cumulative Density") + labs(title = "Stream Temperature") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2], cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim = c(-10, 30), ylim = c(-0.01,1.01))

ggplot(preharvest, aes(pre.ec)) +
	stat_ecdf(mapping = aes(pre.ec, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(post.ec, colour = cbPalette[4]),
	data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x = "Electrical Conductivity", y = "Cumulative Density", title = "Electrical Conductivity") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 100), ylim=c(-0.01,1.01))

ggplot(preharvest, aes(pre.q)) +
	stat_ecdf(mapping = aes(pre.q, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(post.q, colour = cbPalette[4]),
	data = postharvest, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Discharge") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 300), ylim=c(-0.01,1.01))

ggplot(preharvest, aes(pre.st)) +
	stat_ecdf(mapping = aes(pre.st, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(post2yr.st, colour = cbPalette[4]),
	data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Stream Temperature", y="Cumulative Density", title="Stream Temperature (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(-10, 30), ylim=c(-0.01,1.01))

ggplot(preharvest, aes(pre.ec)) +
	stat_ecdf(mapping = aes(pre.ec, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(post2yr.ec, colour = cbPalette[4]),
	data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Electrical Conductivity", y="Cumulative Density", 
		title="Electrical Conductivity (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 100), ylim=c(-0.01,1.01))

ggplot(preharvest, aes(pre.q)) +
	stat_ecdf(mapping = aes(pre.q, colour = cbPalette[2]),
	data = preharvest, n = NULL, geom = "line", size = 1.5) +
	stat_ecdf(mapping = aes(post2yr.q, colour = cbPalette[4]),
	data = postharvest.2year, n = NULL, geom = "line", size = 1.5) +
	labs(x="Discharge", y="Cumulative Density", title="Discharge (2 years after harvest)") +
	coord_cartesian(xlim = c(0, 10)) + 
	scale_colour_manual(labels = c("Post-harvest", "Pre-harvest"), values = c(cbPalette[2],cbPalette[4])) +
	scale_linetype_manual(values = c(2, 1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme_cowplot() + theme(legend.title=element_blank()) + 
	coord_flip(xlim=c(0, 300), ylim=c(-0.01,1.01))

# np package: kernel
bw <- npregbw(data = preharvest, formula = pre.st~pre.at)
mod <- npreg(bw, residuals = TRUE, gradients = TRUE)
plot(mod, gradients = TRUE, plot.errors.method="asymptotic")

bw.multi <- npregbw(data = preharvest, formula = pre.st ~ pre.at + log(pre.q))

# fit linear models to aggregated data
st.mean.ols <- lm(st.mean ~ at.mean, data = ag.df)
summary(st.mean.ols)
ols.mean.res <- residuals(st.mean.ols)

st.min.ols <- lm(st.min ~ at.min, data = ag.df)
summary(st.min.ols)
ols.min.res <- residuals(st.min.ols)

st.max.ols <- lm(st.max ~ at.max, data = ag.df)
summary(st.max.ols)
ols.max.res <- residuals(st.max.ols)

# Convert data to ts data for ARIMA model

cr.df$ec.gf <- ifelse(!is.na(cr.df$ec), cr.df$ec, mean(na.omit(cr.df$ec)))
cr.df$st.gf <- ifelse(!is.na(cr.df$stream.temp), 
  cr.df$stream.temp, mean(na.omit(cr.df$stream.temp)))
cr.df$at.gf <- ifelse(!is.na(cr.df$air.temp), 
  cr.df$air.temp, mean(na.omit(cr.df$air.temp)))

q.ts <- ts(cr.df$q.gf, frequency = 365)
bf.ts <- ts(cr.df$bf.gf, frequency = 365)
qf.ts <- ts(cr.df$qf.gf, frequency = 365)
ec.ts <- ts(cr.df$ec.gf, frequency = 365)
st.ts <- ts(cr.df$st.gf, frequency = 365)
at.ts <- ts(cr.df$at.gf, frequency = 365)

# ARIMA model

arima <- ARIMA(q.ts, order = c())