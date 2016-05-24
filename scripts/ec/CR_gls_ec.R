pre.dt <- ag.preharvest$dt
pre.ec.mean <- ag.preharvest$ec.mean
pre.logq.mean <- log(ag.preharvest$q.mean)
pre.sin.doy <- ag.preharvest$sin.doy
pre.cos.doy <- ag.preharvest$cos.doy

gls.mean <- gls(pre.ec.mean ~ pre.logq.mean + pre.sin.doy + pre.cos.doy, method = "ML", na.action = na.aggregate)

gls.mean.fitted <- gls.mean$fitted

plot(pre.dt, gls.mean.fitted)
lines(pre.dt, pre.ec.mean)
NSE(gls.mean.fitted, pre.ec.mean)
