## Chapter 2 ##

# Expectation and the ensemble
Herald.dat <- read.table("Herald.dat", header = T)
attach (Herald.dat)

x <- CO
y <- Benzoa
n <- length(x)

# definition of cov
sum((x - mean(x))*(y - mean(y)))/(n - 1)

# definition of ensemble cov
mean((x - mean(x))*(y - mean(y)))

# cov function
cov(x, y)

plot(CO, Benzoa, xlab = "CO", ylab = "Benzoa")
grid(10, 10, col = "lightgray", lty = "dotted",
    lwd = par("lwd"), equilogs = TRUE)

cov(x,y)/(sd(x)*sd(y))
cor(x,y)

# Autocorrelation
wave <- read.table ("wave.dat", header=T)
attach(wave)

waveht.ts <- ts(waveht)
layout(1:2)
plot(waveht.ts)
plot(waveht.ts[1:60])

waveht.acf <- acf(waveht, plot=FALSE)
waveht.acf$acf[1]
waveht.acf$acf[2]
layout(1:1)
plot(waveht.ts[1:396],waveht.ts[2:397])
plot(waveht.acf)

# The correlogram
data(AirPassengers)
AP <- AirPassengers
AP.decom <- decompose(AP, "multiplicative")
AP.decom.ts <- ts(AP.decom)
plot(AP.decom.ts$random[7:138])
acf(AP.decom.ts$random[7:138])
sd(AP[7:138])
sd(AP[7:138] - AP.decom$trend[7:138])
sd(AP.decom$random[7:138])

test <- data.frame(c(1:1000))
test.ts = ts(test)
acf(test.ts)

# Font Reservoir series
Fontdsdt <- read.table("Fontdsdt.dat", header=T)
attach(Fontdsdt)
adflow.ts <- ts(adflow)
plot(adflow.ts, ylab = 'adflow')
acf(adflow.ts, xlab = 'lag (months)', main="")

