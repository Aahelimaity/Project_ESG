library("quantmod")
library("graphics")
library(moments)
library(tseries)
library("zoom")
library("xts")
library(rugarch)
library(fDMA)
library(PerformanceAnalytics)

#Creating time series from the NSE data
project_FM_ts <- xts(project_FM[,2:3],order.by = as.Date(project_FM$DateTime))

#plot
plot(project_F_ts[,"Log returns"], main = "Returns on NIFTY100 and NIFTY100 ESG", cex = 0.5)
lines(project_FM_ts[,"Log returns ESG"],col="blue")


#Descriptive Statistics
mean(project_FM_ts$`Log returns ESG`)
mean(project_FM_ts$`Log returns`)
median(project_FM_ts$`Log returns ESG`)
median(project_FM_ts$`Log returns`)
sd(project_FM_ts$`Log returns ESG`)
sd(project_FM_ts$`Log returns`)
skewness(project_FM_ts$`Log returns ESG`)
skewness(project_FM_ts$`Log returns`)
kurtosis(project_FM_ts$`Log returns ESG`)
kurtosis(project_FM_ts$`Log returns`)
jarque.bera.test(project_FM_ts$`Log returns ESG`)
jarque.bera.test(project_FM_ts$`Log returns`)

#Check for stationary
adf.test(project_FM_ts$`Log returns ESG`, alternative = "stationary")
adf.test(project_FM_ts$`Log returns`, alternative = "stationary")

#ARCH Test
archtest(project_FM_ts$`Log returns ESG`, lag = 2)
archtest(project_FM_ts$`Log returns`, lag = 2)

#ARMA fit
ar(project_FM1_ts$'Log returns ESG Enhanced')
ar(project_FM1_ts$'Log returns')

#GARCH Model fit for Log Returns ESG Enhanced
spec1 = ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),mean.model=list(armaOrder=c(0,0)), distribution.model="sstd")
tempgarch1 = ugarchfit(spec=spec1,data=project_FM_ts[,1])
tempgarch1
spec2 = ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1,1)),mean.model=list(armaOrder=c(0,0)), distribution.model="sstd")
tempgarch2 = ugarchfit(spec=spec1,data=project_FM_ts[,1])
tempgarch2
spec3 = ugarchspec(variance.model = list(model = "fGARCH",garchOrder = c(1,1),submodel ="TGARCH"),mean.model=list(armaOrder=c(0,0)), distribution.model="sstd")
tempgarch3 = ugarchfit(spec=spec, data=project_FM_ts[,1])
tempgarch3

#GARCH Model fit for Log Returns
tempgarch.1 = ugarchfit(spec=spec1,data=project_FM_ts[,1])
tempgarch.1
tempgarch.2 = ugarchfit(spec=spec1,data=project_FM_ts[,1])
tempgarch.2
tempgarch.3 = ugarchfit(spec=spec, data=project_FM_ts[,1])
tempgarch.3

#Plot
plot(tempgarch2@fit$sigma,type="l",col="black", Main = "Conditional volatily of NIFTY100 and NIFTY100 ESG", ylab= "Conditional volatility", cex =0.5)
lines(tempgarch.2@fit$sigma,col="blue")
