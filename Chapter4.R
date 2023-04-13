## Chapter 4 ##

set.seed(1)
w <- rnorm(1000)
plot(w, type = "l")

x <- seq(-3,3, length = 1000)
hist(rnorm(1000), breaks = 25, prob = T)
points(x, dnorm(x), type = "l")

##

set.seed(3)
w <- rnorm(100)
acf(w)

##

x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t - 1] + w[t]
plot(x, type = "l")
acf(x)

##

acf(diff(x))

##

Z <- read.table("pounds_nz.dat", header = T)
Z.ts <- ts(Z, st = 1991, fr = 4)
plot(Z.ts, xlab = "time / years", 
     ylab = "Quarterly exchange rate in $NZ / pound")

Z.hw <- HoltWinters(Z.ts, alpha = 1, gamma = FALSE)
acf(resid(Z.hw))

##

HP <- read.table("HP.txt", header = T)
attach(HP)
plot(as.ts(Price))
DP <- diff(Price)
plot(as.ts(DP))
acf(DP)
mean(DP) + c(-2, 2)*sd(DP)/sqrt(length(DP))
