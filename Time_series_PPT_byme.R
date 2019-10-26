sales<-scan("https://robjhyndman.com/tsdldata/data/sales.dat")
salests<-ts(sales,frequency = 12,c(1987,1))
typeof(salests)
str(salests)
plot.ts(salests)
decompose(salests, type="multiplicative")


install.packages("fpp")
library(fpp)
data(cafe)
plot(cafe)

es<-ses(cafe,h=10)
plot(es)

es

plot(ses(cafe,h=10, alpha=0.77)) #alpha more mean it gives more weightage to recent values

plot(ses(cafe,h=10, alpha=0.77,sigma=241)) #don't know what it does
#sigma and I, dont know about what they does

accuracy(es)

plot.ts(es$residuals)

acf(es$residuals)
























