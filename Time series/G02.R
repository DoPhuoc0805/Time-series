ts=read_excel("C:/Users/Admin/OneDrive - National Economics University/group2_ts.xlsx")
head(ts)
attach(ts)
logTSC=na.omit(logTSC)
logDPM=na.omit(logDPM)
logGMD=na.omit(logGMD)
logVTO=na.omit(logVTO)

p=data.frame(TSC,VTO,GMD,DPM )
l=data.frame(logTSC,logVTO,logGMD,logDPM )
head(p)
cor(l)
cov(l)
summary(l)
var(TSC)

log_return_real=c(1.522099401,-0.606062461,-2.61754744,-2.208291629,1.267845826,0,-2.390552085,-0.323102058,	-2.955880224,0.332779009)

VARselect(l, lag.max = 10, type = "none")
var=VAR(l,p=2,type = "none")
summary(var)
pp=predict(var, n.ahead = 10)
pp$fcst$logVTO[, "fcst"]
pp$fcst$logGMD[, "fcst"]
pp$fcst$logDPM[, "fcst"]
serial.test(var, type = "PT.asymptotic")

vto=c(0.98019, -0.38638, 0.80653, -0.35845, 0.38646,-0.07607, 0.33562, 0.09814, 0.20658, 0.11301)
tsc=c(0, -0.0165, 0.004175, -0.004175, 0.004175, -0.016807, -0.0085106, 0, 0, 0)
dpm=c(1.1364,0.4228,-1.5592,-1.5592,0.4351,-1.1645,-1.9217,1.7752,-0.2937,0)
rmse(tsc,pp$fcst$logTSC[, "fcst"])
smape(tsc,pp$fcst$logTSC[, "fcst"])
rmse(vto,pp$fcst$logVTO[, "fcst"])
smape(vto,pp$fcst$logVTO[, "fcst"])
rmse(gmd,pp$fcst$logGMD[, "fcst"])
smape(gmd,pp$fcst$logGMD[, "fcst"])
rmse(dpm,pp$fcst$logDPM[, "fcst"])
smape(dpm,pp$fcst$logDPM[, "fcst"])


irf_result1 <- irf(var,
                  impulse = "logTSC",
                  response = c("logTSC", "logVTO", "logGMD", "logDPM"),
                  n.ahead = 10,
                  boot = TRUE)

plot(irf_result1)
irf_result2 <- irf(var,
                  impulse = "logVTO",
                  response = c("logTSC", "logVTO", "logGMD", "logDPM"),
                  n.ahead = 10,
                  boot = TRUE)

plot(irf_result2)
irf_result3 <- irf(var,
                   impulse = "logGMD",
                   response = c("logTSC", "logVTO", "logGMD", "logDPM"),
                   n.ahead = 10,
                   boot = TRUE)

plot(irf_result3)
irf_result4 <- irf(var,
                   impulse = "logDPM",
                   response = c("logTSC", "logVTO", "logGMD", "logDPM"),
                   n.ahead = 10,
                   boot = TRUE)

plot(irf_result4)



fevd_result <- fevd(var, n.ahead = 10)
windows()
plot(fevd_result)

