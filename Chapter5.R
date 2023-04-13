## Chapter 5 ##

# 5.2.3 Simulation

set.seed(1)
z <- w <- rnorm(100, sd = 20)
for (t in 2:100) z[t] <- 0.8 * z[t - 1] + w[t]
Time <- 1:100
x <- 50 + 3 * Time + z
plot(x, xlab = "time", type = "l")


x.lm <- lm(x ~ Time)
coef(x.lm)
sqrt(diag(vcov(x.lm)))
summary(x.lm)

acf(resid(x.lm))
pacf(resid(x.lm))

# 5.3.2 Temperature series


Global <- scan("global.dat")
Global.ts <- ts(Global, st = c(1856, 1), end = c(2005,12), fr = 12)
temp <- window(Global.ts, start = 1970)
temp.lm <- lm(temp ~ time(temp))
coef(temp.lm)
confint(temp.lm)
acf(resid(temp.lm))

# 5.4.1

library(nlme)
x.gls <- gls(x ~ Time, cor = corAR1(0.8))
coef(x.gls)
coef(x.lm)
sqrt(diag(vcov(x.gls)))
sqrt(diag(vcov(x.lm)))

# 5.4.2
temp.gls <- gls(temp ~ time(temp), cor = corAR1(0.7))
confint(temp.gls)

