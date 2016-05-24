# loess regression
loess <- loess(data = ag.preharvest, formula = st.mean ~ at.mean + log(q.mean))
str(loess)
summary(loess)

loess.res <- residuals(loess)
hist(loess.res)
plot(loess.res)

plot(ag.preharvest$at.mean, ag.preharvest$st.mean, pch = 21, bg = "grey")
lines(ag.preharvest$at.mean, fitted(loess), col = "red", type = "l", lwd = 2)

#################################################################################
# np package: kernel
#################################################################################

# univariate regression: mean stream temp ~ mean atmospheric temp
bw <- npregbw(data = ag.preharvest, formula = st.mean ~ at.mean)
mod <- npreg(bw, residuals = TRUE, gradients = TRUE)
summary(mod)

mod.sigtest <- npsigtest(mod)
summary(mod.sigtest)

# diagnostics of univariate np regression
mod.res <- residuals(mod)
mod.res

plot(mod, gradients = TRUE, plot.errors.method = "asymptotic")

plot(ag.preharvest$at.mean, ag.preharvest$st.mean,
  xlab = "mean atmos temp", ylab = "mean stream temp", cex=.1)
lines(ag.preharvest$at.mean, fitted(mod), lty = 1, col = "blue")

plot(mod, plot.errors.method = "asymptotic")

plot(mod, gradients = TRUE)
plot(mod, gradients = TRUE, plot.errors.method = "asymptotic")

predict.mod <- predict(mod, newdata = ag.postharvest)

# multivariate regression: mean stream temp ~ mean atmos temp + log(mean q)
bw.multi <- npregbw(data = ag.preharvest, formula = st.mean ~ at.mean + log(q.mean))
mod.multi <- npreg(bw.multi, residuals = TRUE, gradients = TRUE)
summary(mod.multi)

predict.multi <- predict(mod.multi, newdata = ag.postharvest)

# diagnostics of multivariate np regression
multi.res <- residuals(mod.multi)

plot(mod.multi, gradients = TRUE, plot.errors.method = "bootstrap")

plot(multi.res)

hist(multi.res, col = "red", main = "", xlab = "residuals")

qqnorm(multi.res, ylab = "residuals", pch = 21, bg = "blue", main = "")
qqline(multi.res, col = "red")

plot(ag.postharvest$dt, ag.postharvest$st.mean)
lines(ag.postharvest$dt, ag.postharvest$st.mean, type = "o", pch = 25, bg = "blue", col = "blue", lwd = 1.5)
lines(ag.postharvest$dt, predict.multi, type = "o", bg = "red", col = "red", pch = 21, lwd = 1.5)

#################################################################################
# nonparametric unconditional PDF and CDF estimation
#################################################################################

f.cr <- npudens(~ st.mean + at.mean, data = ag.preharvest)
F.cr <- npudist(~ st.mean + at.mean, data = ag.preharvest)
summary(f.cr) # density
summary(F.cr) # distribution

plot(f.cr, xtrim = -0.2, view = "fixed", main = "")
plot(F.cr, xtrim = -0.2, view = "fixed", main = "")

#################################################################################
# nonparametric conditional PDF and CDF estimation
#################################################################################

fhat <- npcdens(st.mean ~ at.mean, tol = 0.1, ftol = 0.1, data = ag.preharvest)
summary(fhat) # density

Fhat <- npcdist(st.mean ~ at.mean, tol = 0.1, ftol = 0.1, data = ag.preharvest)
summary(Fhat) # distribution

plot(fhat, view = "fixed", main = "", theta = 300, phi = 50)
plot(Fhat, view = "fixed", main = "", theta = 300, phi = 50)

#################################################################################
# nonparametric quantile regression
#################################################################################

qr.bw <- npcdistbw(formula = st.mean ~ at.mean, tol = 0.1, ftol = 0.1, data = ag.preharvest)
qr25 <- npqreg(bws = qr.bw, tau = 0.25)
qr50 <- npqreg(bws = qr.bw, tau = 0.50)
qr75 <- npqreg(bws = qr.bw, tau = 0.75)

plot(ag.preharvest$at.mean, ag.preharvest$st.mean, main = "",
  xlab = "mean atmospheric temp", ylab = "mean stream temp")
lines(ag.preharvest$at.mean, qr25$quantile, col = "red", lty = 1, lwd = 2)
lines(ag.preharvest$at.mean, qr50$quantile, col = "blue", lty = 2, lwd = 2)
lines(ag.preharvest$at.mean, qr75$quantile, col = "red", lty = 3, lwd = 2)

#################################################################################
# fit linear models to aggregated data
#################################################################################
mod.ols <- lm(st.mean ~ at.mean, data = ag.preharvest)
summary(st.mean.ols)
ols.mean.res <- residuals(st.mean.ols)
predict.ols <- predict(st.mean.ols, newdata = ag.postharvest)

multi.ols <- lm(st.mean ~ at.mean + log(q.mean), data = ag.preharvest)
summary(multi.ols)

#mod.gls <- gls(st.mean ~ at.mean, data = ag.preharvest)

#################################################################################
# ARIMA model
#################################################################################
arima <- arima(q.ts, order = c(1, 0, 0))
str(arima)
summary(arima)
