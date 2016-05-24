library(dplyr)
library(nlme)
library(EcoHydRology)
library(ggplot2)
library(stats)
library(car)
library(lmtest)
library(lubridate)

wq.data <- readRDS("CR_WQ30.2009_Apr2015.cleaning in prog 27Jan2016,mollie.RDS")

biomet.data <- readRDS("Biomet.2009_Apr2014,mollie.RDS")

#tv.30 = 30 minute time vector
#Precip in mm per 30min period
#Patm is atmospheric pressure in kPa
#NEP is net ecosystem productivity as P (productivity) minus R (respiration)
#Tair is air temp in deg C

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

# how many NAs in each column
sum(is.na(cr.df$q))
sum(is.na(cr.df$stream.temp))
sum(is.na(cr.df$air.temp))
sum(is.na(cr.df$ec))

# initial plots
plot(cr.df$date.time, cr.df$ec)
plot(cr.df$date.time, cr.df$stream.temp)
plot(cr.df$air.temp, cr.df$stream.temp)
plot(cr.df$date.time, cr.df$q)

# ggplots
ggplot(cr.df, aes(date.time)) + 
	geom_line(aes(y = stream.temp, color = "stream.temp")) + 
	geom_line(aes(y = q, color = "q"))

ggplot(cr.df, aes(date.time)) + 
	geom_line(aes(y = stream.temp, color = "stream.temp")) + 
	geom_line(aes(y = log10(q), color = "q"))

ggplot(cr.df, aes(date.time)) + 
	geom_line(aes(y = ec, color = "stream.temp")) + 
	geom_line(aes(y = q, color = "q"))

# BaseflowSeparation

#gap-fill any missing discharge data with mean discharge for the period of record - the baseflow separation cannot work with missing data
cr.df$q.gf <- ifelse(!is.na(cr.df$q), cr.df$q, mean(na.omit(cr.df$q)))
#compute baseflow (BF) using gap-filled Q data
cr.df$bf.gf <- BaseflowSeparation(cr.df$q.gf, filter_parameter = 0.925, passes = 3)[,1] 
# only keep BF values when Q data exists
cr.df$bf <- ifelse(!is.na(cr.df$q), cr.df$bf.gf, NA)
#compute quickflow (QF) using gap-filled Q data
cr.df$qf.gf <- BaseflowSeparation(cr.df$q.gf, filter_parameter = 0.925, passes = 3)[,2]
# only keep QF values when Q data exists
cr.df$qf <- ifelse(!is.na(cr.df$q), cr.df$qf.gf, NA)

# baseflow separation ggplots

ggplot(cr.df, aes(date.time)) + 
	geom_line(aes(y = ec, color = "ec")) + 
	geom_line(aes(y = bf, color = "bf"))

ggplot(cr.df, aes(date.time)) + 
	geom_line(aes(y = ec, color = "ec")) + 
	geom_line(aes(y = qf, color = "qf"))

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
pre.st <- preharvest$stream.temp
postharvest <- subset(cr.df, date.time >= as.POSIXct('2010-11-01 00:00:00'))
post.st <- postharvest$stream.temp

# lm regression with pre and post harvest data
st.lm <- lm(stream.temp ~ q + air.temp, data = preharvest)
predict.st.lm <- predict(st.lm, new.data = data.frame(pre.st = post.st), interval = "prediction", level = 0.95)

predict.res <- pre.st - predict.st.lm
predict.fit <- predict.st.lm[, 1]
predict.upr <- predict.st.lm[, 2]
predict.lwr <- predict.st.lm[, 3]

hist(predict.res)

qqnorm(predict.res)
qqline(predict.res, col = "red")

# more plots with time subsets
plot(day.df$date.time, day.df$ec)
plot(day.df$date.time, day.df$stream.temp)
plot(day.df$air.temp, day.df$stream.temp)

plot(month.df$date.time, month.df$ec)
plot(month.df$date.time, month.df$stream.temp)
plot(month.df$air.temp, month.df$stream.temp)

plot(hour.df$date.time, hour.df$ec)
plot(hour.df$date.time, hour.df$stream.temp)
plot(hour.df$air.temp, hour.df$stream.temp)

# plots with baseflow separation
plot(cr.df$date.time, cr.df$bf)
plot(cr.df$date.time, cr.df$qf)

# decompose time series plots
qplot(cr.df$date.time,cr.df$q) + geom_smooth()

# q decompose (gap filled data)
q.day.ts <- ts(cr.df$q.gf, frequency = 365)
q.day.z <- decompose(q.day.ts)
plot(q.day.z)

q.month.ts <- ts(cr.df$q.gf, frequency = 12)
q.month.z <- decompose(q.month.ts)
plot(q.month.z)

q.week.ts <- ts(cr.df$q.gf, frequency = 48)
q.week.z <- decompose(q.week.ts)
plot(q.week.z)

# base flow decompose (gap filled data)
bf.day.ts <- ts(cr.df$bf.gf, frequency = 365)
bf.day.z <- decompose(bf.day.ts)
plot(bf.day.z)

bf.month.ts <- ts(cr.df$bf.gf, frequency = 12)
bf.month.z <- decompose(bf.month.ts)
plot(bf.month.z)

bf.week.ts <- ts(cr.df$bf.gf, frequency = 48)
bf.week.z <- decompose(bf.week.ts)
plot(bf.week.z)

# quick flow decompose (gap filled data)
qf.day.ts <- ts(cr.df$qf.gf, frequency = 365)
qf.day.z <- decompose(qf.day.ts)
plot(qf.day.z)

qf.month.ts <- ts(cr.df$qf.gf, frequency = 12)
qf.month.z <- decompose(qf.month.ts)
plot(qf.month.z)

qf.week.ts <- ts(cr.df$qf.gf, frequency = 48)
qf.week.z <- decompose(qf.week.ts)
plot(qf.week.z)

# preliminary OLS regression

st.ols <- lm(stream.temp ~ q + air.temp, data = cr.df)
summary(st.ols) # stream temp appears to decline with discharge and increase with air temp

# diagnostic plots of residuals
ols.res <- residuals(st.ols)

#histogram of residuals
hist(ols.res) # normal distribution of residuals

# normal quantile plot
qqnorm(ols.res, 
			 ylab = expression("prediction errors"),
			 pch = 21, bg = "blue",
			 main = "" 
)
qqline(ols.res, col = "red")

# time series

# acf function in stats package - computes and plots autocorrelation and partial autocorrelation functions of a time series
acf(residuals(st.ols))
acf(residuals(st.ols), type = "partial")
# ask dan what these mean

# Durbin Watson test
durbinWatsonTest(st.ols, max.lag = 5)

# alternative: dwtest function in lmtest package, which computes p value for first-order DW statistic
dwtest(st.ols, alternative = "two.sided")

# GLS regression
# questions: which correlation to use, which method, should na.action = na.exclude or na.omit
# how to work for big dataset

st.gls <- gls(stream.temp ~ q + air.temp, data = month.df, correlation = corARMA(p = 2), method = "ML", na.action = na.exclude)
summary(st.gls)

# diagnostic plots of residuals
gls.res <- residuals(st.gls)

#histogram of residuals
hist(gls.res) # normal distribution of residuals

# normal quantile plot
qqnorm(gls.res, 
			 ylab = expression("prediction errors"),
			 pch = 21, bg = "blue",
			 main = "" 
)
qqline(ols.res, col = "red")

# time series

# aggregate
day <- strftime(cr.df$date.time, "%d")
cr.df$day <- floor_date(cr.df$date.time, "day")
st.mean <- aggregate(cr.df$stream.temp, by = list(cr.df$day), FUN = mean, simplify = TRUE)
st.max <- aggregate(cr.df$stream.temp, by = list(cr.df$day), FUN = max, simplify = TRUE)
st.min <- aggregate(cr.df$stream.temp, by = list(cr.df$day), FUN = min, simplify = TRUE)

ag.df <- cbind(st.mean, st.max, st.min)
ag.df[3] <- NULL
ag.df[4] <- NULL

ag.df$st.mean <- ag.df[2]
ag.df$st.max <- ag.df[3]
ag.df$st.min <- ag.df[4]
ag.df[2] <- NULL
ag.df[2] <- NULL
ag.df[2] <- NULL

# Mark's ggplot ECDF
ggplot(cr.df, aes(stream.temp, colour = "stream.temp")) +
	stat_ecdf(mapping = aes(stream.temp, colour = "stream.temp"),data =cr.df,n = NULL,geom = "line", size = 1.5) +
	labs(x="stream temp") + labs(y="Cumulative Density") + labs(title="DOC distribution curves") +
	coord_cartesian(xlim = c(0, 10)) + #scale_colour_manual(values=c(cbPalette[2],cbPalette[4]))
	scale_linetype_manual(values=c(2,1)) + guides(col = guide_legend(reverse = TRUE)) + 
	theme(legend.position="bottom", legend.title=element_blank()) +
	coord_flip(xlim=c(-10, 30), ylim=c(0.01,1))

# fit models for daily min, mean, and max. use daily min, mean, max air temp. use log discharge. after fit model - is there a seasonal pattern in residuals. fit regression against air temp, plot residuals against discharges. na.exclude, use predict, which will pad missing values. plot against discharge. 