library(plyr)
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
library(zoo)
library(reshape2)
library(hydroGOF)
library(car)
library(SDMTools)

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

cr.df <- cr.df[-(1:5374), ]
cr.df$ec <- ifelse(cr.df$ec < 0, NA, cr.df$ec)
cr.df$q <- ifelse(cr.df$q < 0, NA, cr.df$q)
cr.df$stream.temp <- ifelse(cr.df$stream.temp < 0, NA, cr.df$stream.temp)

# BaseflowSeparation

cr.df$q.gf <- ifelse(!is.na(cr.df$q), cr.df$q, mean(na.omit(cr.df$q)))
cr.df$bf.gf <- BaseflowSeparation(cr.df$q.gf, filter_parameter = 0.925, passes = 3)[,1] 
cr.df$bf <- ifelse(!is.na(cr.df$q), cr.df$bf.gf, NA)
cr.df$qf.gf <- BaseflowSeparation(cr.df$q.gf, filter_parameter = 0.925, passes = 3)[,2]
cr.df$qf <- ifelse(!is.na(cr.df$q), cr.df$qf.gf, NA)

# above: q.gf and bf.gf

cr.df$ec.gf <- ifelse(!is.na(cr.df$ec), cr.df$ec, mean(na.omit(cr.df$ec)))
cr.df$st.gf <- ifelse(!is.na(cr.df$stream.temp), 
  cr.df$stream.temp, mean(na.omit(cr.df$stream.temp)))
cr.df$at.gf <- ifelse(!is.na(cr.df$air.temp), 
  cr.df$air.temp, mean(na.omit(cr.df$air.temp)))

# include sine and cosine of day

doy <- strftime(cr.df$date.time, "%j")
doy <- as.numeric(doy)
sin.doy <- sin(2*pi*doy/365.25)
cos.doy <- cos(2*pi*doy/365.25)

cr.df.x <- cbind(cr.df, doy, sin.doy, cos.doy)

# subset by pre and post harvest
preharvest.x <- subset(cr.df.x, date.time < as.POSIXct('2011-2-01 00:00:00'))

postharvest.x <- subset(cr.df.x, date.time >= as.POSIXct('2011-2-01 00:00:00'))

# subsetting datetime by hourly, daily, weekly, monthly
day.df <- subset(cr.df, format(date.time,'%H')=='12')
day.df <- subset(day.df, format(date.time,'%M')=='00')
month.df <- subset(cr.df, format(date.time,'%d')=='01')
month.df <- subset(month.df, format(date.time,'%H')=='12')
month.df <- subset(month.df, format(date.time,'%M')=='00')
hour.df <- subset(cr.df, format(date.time,'%M')=='00')
week.df <- day.df[seq(1, nrow(day.df), 7), ]

# subset by pre and post harvest
preharvest <- subset(cr.df, date.time < as.POSIXct('2011-2-01 00:00:00'))

postharvest <- subset(cr.df, date.time >= as.POSIXct('2011-2-01 00:00:00'))

# aggregate mean, max, min by day
day <- strftime(cr.df$date.time, "%d")
cr.df$day <- floor_date(cr.df$date.time, "day")

st.mean <- aggregate(cr.df$st.gf, by = list(cr.df$day), FUN = mean, simplify = TRUE)
st.max <- aggregate(cr.df$st.gf, by = list(cr.df$day), FUN = max, simplify = TRUE)
st.min <- aggregate(cr.df$st.gf, by = list(cr.df$day), FUN = min, simplify = TRUE)

at.mean <- aggregate(cr.df$at.gf, by = list(cr.df$day), FUN = mean, simplify = TRUE)
at.max <- aggregate(cr.df$at.gf, by = list(cr.df$day), FUN = max, simplify = TRUE)
at.min <- aggregate(cr.df$at.gf, by = list(cr.df$day), FUN = min, simplify = TRUE)

q.mean <- aggregate(cr.df$q.gf, by = list(cr.df$day), FUN = mean, simplify = TRUE)
q.max <- aggregate(cr.df$q.gf, by = list(cr.df$day), FUN = max, simplify = TRUE)
q.min <- aggregate(cr.df$q.gf, by = list(cr.df$day), FUN = min, simplify = TRUE)

bf.mean <- aggregate(cr.df$bf.gf, by = list(cr.df$day), FUN = mean, simplify = TRUE)
bf.max <- aggregate(cr.df$bf.gf, by = list(cr.df$day), FUN = max, simplify = TRUE)
bf.min <- aggregate(cr.df$bf.gf, by = list(cr.df$day), FUN = min, simplify = TRUE)

ec.mean <- aggregate(cr.df$ec.gf, by = list(cr.df$day), FUN = mean, simplify = TRUE)
ec.max <- aggregate(cr.df$ec.gf, by = list(cr.df$day), FUN = max, simplify = TRUE)
ec.min <- aggregate(cr.df$ec.gf, by = list(cr.df$day), FUN = min, simplify = TRUE)

# store aggregated data in dataframe
ag.df <- cbind(st.mean, st.max, st.min, at.mean, at.max, at.min, 
  q.mean, q.max, q.min, bf.mean, bf.max, bf.min, ec.mean, ec.max, ec.min)
ag.df[3] <- NULL
ag.df[4] <- NULL
ag.df[5] <- NULL
ag.df[6] <- NULL
ag.df[7] <- NULL
ag.df[8] <- NULL
ag.df[9] <- NULL
ag.df[10] <- NULL
ag.df[11] <- NULL
ag.df[12] <- NULL
ag.df[13] <- NULL
ag.df[14] <- NULL
ag.df[15] <- NULL
ag.df[16] <- NULL

colnames(ag.df) <- c("dt", "st.mean", "st.max", "st.min", "at.mean", "at.max", "at.min", 
  "q.mean", "q.max", "q.min", "bf.mean", "bf.max", "bf.min", "ec.mean", "ec.max", "ec.min")

doy <- strftime(ag.df$dt, "%j")
doy <- as.numeric(doy)
sin.doy <- sin(2*pi*doy/365.25)
cos.doy <- cos(2*pi*doy/365.25)

ag.df <- cbind(ag.df, doy, sin.doy, cos.doy)

#############################################
# taking modes of variables out of ag.df
############################################

getmode <- function(v) {
	uniqv <- unique(v)
	uniqv[which.max(tabulate(match(v, uniqv)))]
}

ec.mode <- getmode(ag.df$ec.mean)
bf.mode <- getmode(ag.df$bf.mean)
q.mode <- getmode(ag.df$q.mean)
st.mode <- getmode(ag.df$st.mean)
at.mode <- getmode(ag.df$at.mean)

ag.df$ec.mean <- ifelse(ag.df$ec.mean == ec.mode, NA, ag.df$ec.mean)
ag.df$ec.min <- ifelse(ag.df$ec.min == ec.mode, NA, ag.df$ec.min)
ag.df$ec.max <- ifelse(ag.df$ec.max == ec.mode, NA, ag.df$ec.max)

ag.df$bf.mean <- ifelse(ag.df$bf.mean == bf.mode, NA, ag.df$bf.mean)
ag.df$bf.min <- ifelse(ag.df$bf.min == bf.mode, NA, ag.df$bf.min)
ag.df$bf.max <- ifelse(ag.df$bf.max == bf.mode, NA, ag.df$bf.max)

ag.df$q.mean <- ifelse(ag.df$q.mean == q.mode, NA, ag.df$q.mean)
ag.df$q.min <- ifelse(ag.df$q.min == q.mode, NA, ag.df$q.min)
ag.df$q.max <- ifelse(ag.df$q.max == q.mode, NA, ag.df$q.max)

ag.df$st.mean <- ifelse(ag.df$st.mean == st.mode, NA, ag.df$st.mean)
ag.df$st.min <- ifelse(ag.df$st.min == st.mode, NA, ag.df$st.min)
ag.df$st.max <- ifelse(ag.df$st.max == st.mode, NA, ag.df$st.max)

ag.df$at.mean <- ifelse(ag.df$at.mean == at.mode, NA, ag.df$at.mean)
ag.df$at.min <- ifelse(ag.df$at.min == at.mode, NA, ag.df$at.min)
ag.df$at.max <- ifelse(ag.df$at.max == at.mode, NA, ag.df$at.max)

# aggregated data for preharvest and postharvest dataframes

ag.preharvest <- subset(ag.df, dt < as.POSIXct('2011-1-31 00:00:00'))
ag.postharvest <- subset(ag.df, dt >= as.POSIXct('2011-1-31 00:00:00'))
