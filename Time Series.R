##############################################Pre class video code###################################

sales<-scan("https://robjhyndman.com/tsdldata/data/sales.dat")
salestimeseries<-ts(sales)#converting data to a time series object
salestimeseries<-ts(sales,frequency = 12) #frequency determine whether data is collected monthly or quartarly or yearly.
salestimeseries<-ts(sales,frequency = 12,start =c(1987,1))#we should specify start date of data collected and sub period define the month as data is monthly.
#now we can use plot function to plot the time series.
plot.ts(salestimeseries)

# decompose function to decompose the time series component

decompose(salestimeseries,type="multiplicative")

#############################simple exponential smoothing in R#######################################

install.packages("fpp")
library(fpp)

es<-ses(cafe,h=10)
plot(es)
summary(es)

#############################Validation ####################################

accuracy(es)

plot.ts(es$residuals)

acf(es$residuals)
print(es$residuals)
# smoothing the time series remove autocorrelation,noise.
#capture model, actual trend,seasonal pattern and actual irregular component

resid<-as.data.frame(es$residuals)
hist(resid$x)
############################Holt model for Trend#############################################

#Holt Model can deal with trend but not sesonality 
library(forecast)
hol<-holt(cafe,h=10)
plot(hol)
summary(hol)

#############################Validation ####################################

checkresiduals(hol)

############################Holtwinters model without Seasonality and Trend##########################

rain<-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip = 1)
rainseries<-ts(rain,start=c(1813,1))
plot.ts(rainseries)
rainseriesforcast<-HoltWinters(rainseries,beta = FALSE,gamma=FALSE)
rainseriesforcast

install.packages("forecast")
library(forecast)
rainseriesforcast$fitted
plot(rainseriesforcast)

rainseriesforecasts2<-forecast(rainseriesforcast,h=8)
rainseriesforecasts2
plot(rainseriesforecasts2)
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
Acf(rainseriesforecasts2$residuals,lag.max = 20)
plot.ts(rainseriesforecasts2$residuals)
hist(rainseriesforecasts2$residuals, col="red", freq=FALSE)

############################Holtwinters model without Seasonality ####################

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
plot(skirtsseriesforecasts)
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts2)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
Acf(skirtsseriesforecasts2$residuals, lag.max=20)
plot.ts(skirtsseriesforecasts2$residuals)
hist(skirtsseriesforecasts2$residuals)

############################Holtwinters model with Trend and seasonality####################

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")

souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries
logsouvenirtimeseries<-log(souvenirtimeseries)

plot.ts(logsouvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
Souvenirts <- hw(souvenirtimeseries, h = 48, seasonal ="multiplicative" )
plot(Souvenirts)
souvenirtimeseriesforecasts
plot(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2  <- forecast(souvenirtimeseriesforecasts, h=48)
plot(souvenirtimeseriesforecasts2)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")
Acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
plot.ts(souvenirtimeseriesforecasts2$residuals)
hist(souvenirtimeseriesforecasts2$residuals)

############################Automatic exponencial smoothing ####################
fit <-ets(cafe)
fcast<- forecast(fit,h=12)
fit
Box.test(fcast$residuals,lag = 20, type = "Ljung-Box")
accuracy(fcast)

str(cafe)

training <- window (cafe, start = c(1982,4), end = c(1999,4), frequency=4)
testing <- window  (cafe, start = c(2000,4), end = c(2010,4), frequency=4)

tr <- ets(training)
test <- ets(testing,model=tr)

accuracy(test)
accuracy(forecast(tr,30),testing)

############################Another example of Automatic exponencial smoothing####################

#a10 - Monthly anti-diabetic drug sales in Australia from 1992 to 2008
#'fpp' package
library(fpp)
data<-data(package="fpp")# It will show list of data set available in the fpp package

str(a10)

a10train <- window (a10, start = c(1992,12), end = c(2004,12), frequency=12)
a10test <- window  (a10, start = c(2005,12), end = c(2008,12), frequency=12)

fit <- ets(a10train)
test <- ets(a10test, model = fit)

accuracy(test)
accuracy(forecast(fit,30), a10test)


lam <- BoxCox.lambda(a10) 
fit <- ets(a10, lambda=lam)


#ARIMA

intusage<-WWWusage

intusagediff1 <- diff(intusage,differences = 1)
plot.ts(intusagediff1)

intusagediff2 <- diff(intusage,differences = 2)
plot.ts(intusagediff2)

#Formal test for stationarity
adf.test(intusage)

adf.test(intusagediff1)

adf.test(intusagediff2)

#ARIMA(p,d,q)
#AR(p)-PACF plots
#I(d)- Orderof Differencing
#MA(q)-ACF plots
Acf(diff(intusagediff2,lag.max=20))
pacf(diff(intusagediff2,lag.max=20))

install.packages("forecast")
library(forecast)
Intusagearima <-Arima(y = intusage,order = c(3,2,1))
Intusagearima

############################Automatic Arima ####################

auto.arima(intusage)

intusageforecasts<-forecast(Intusagearima,h=5)
plot(intusageforecasts,shaded=TRUE,shadecols="oldstyle")
Acf(intusageforecasts$residuals,lag.max = 20)
plot.ts(intusageforecasts$residuals)
Box.test(intusageforecasts$residuals, lag=20, type="Ljung-Box")
hist(intusageforecasts$residuals)

