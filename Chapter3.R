# 3.2.2 Building approvals publication

Build <- read.table("ApprovActiv.dat", header=T)
attach(Build)

##

App.ts <- ts(Approvals, start = c(1996,1), freq=4)
Act.ts <- ts(Activity, start = c(1996,1), freq=4)
ts.plot(App.ts, Act.ts, lty = c(1,3))
ts.plot(App.ts, Act.ts, col = 1:2, lty = 1:3)
legend(2005, 9000, c("App", "Act"), col = 1:2, lty = 1:3)

##
AppAct.ts <- ts.union(App.ts, Act.ts)
class(AppAct.ts)
acf(AppAct.ts)

OR

acf(ts.union(App.ts, Act.ts))
##

app.ran <- decompose(App.ts)$random
app.ran.ts <- window(app.ran, start = c(1996,3))
act.ran <- decompose(Act.ts)$random
act.ran.ts <- window(act.ran, start = c(1996,3))

AppAct.ran.ts <- ts.union(app.ran.ts,act.ran.ts)
acf(AppAct.ran.ts[1:39,1:2])
ccf(app.ran.ts[1:39], act.ran.ts[1:39])

print(acf(AppAct.ran.ts[1:39,1:2]))

## Complaints to a motoring organisation
Motor <- read.table("motororg.dat", header=T)
attach(Motor)

Comp.ts <- ts(complaints, start = c(1996, 1), freq = 12)
plot(Comp.ts, xlab = "Time (months)", ylab = "Complaints")

Comp.hw1 <- HoltWinters(Comp.ts, beta = FALSE, gamma = FALSE)
Comp.hw1
plot(Comp.hw1)

Comp.hw2 <- HoltWinters(Comp.ts, alpha = 0.2, beta = FALSE, 
                        gamma = FALSE)
Comp.hw2
plot(Comp.hw2)
