# Question 6
AR1<-arima.sim(list(ar=c(0.8)),10000) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
par(mfrow=c(1,3))
plot(1:10000,AR1,type="l")
acf(AR1,main="ACF")
pacf(AR1,main="PACF")

### Question 7 ## only for 50 observations

AR1<-arima.sim(list(ar=c(0.8)),50) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
par(mfrow=c(1,3))
plot(1:50,AR1,type="l")
acf(AR1,main="ACF")
pacf(AR1,main="PACF")

### Question 8 ## only for 50 observations

AR1<-arima.sim(list(ar=c(0)),50) #AR1 is just a vector of values.  They will be centered around 0 unless we add a shift or include some other source of variation like a predictor.
par(mfrow=c(1,3))
plot(1:50,AR1,type="l")
acf(AR1,main="ACF")
pacf(AR1,main="PACF")

# Question 9

rho1<-.8
rho2<-.6
a1<-(rho1*(1-rho2)/(1-rho1^2))
a2<-(rho2-rho1^2)/(1-rho1^2)
AR2<-arima.sim(list(ar=c(a1,a2)),10000)
par(mfrow=c(1,3))
plot(1:10000,AR2,type="l")
acf(AR2)
pacf(AR2,main="PACF")

###
###
a1<-1.5
a2<--1.21
a3<-.46
AR3<-arima.sim(list(ar=c(a1,a2,a3)),10000)
par(mfrow=c(1,3))
plot(1:10000,AR3,type="l")
acf(AR3,main="ACF")
pacf(AR3,main="PACF")

###
###
a1<-1.5
a2<--1.21
a3<-.46
b1<--.2
b2<--.9
ARMA32<-arima.sim(list(ar=c(a1,a2,a3),ma=c(b1,b2)),10000)
par(mfrow=c(1,3))
plot(1:10000,ARMA32,type="l")
acf(ARMA32,main="ACF")
pacf(ARMA32,main="PACF")

###########################################################################
###########################################################################
###########################################################################
############ Exercize 3 ###################################################


setwd("/Users/apurv/Documents/SMU/6372 - Applied Statistics/Unit -4/Unit4/Unit4HW")
library(tseries)
library(forecast)
library(ggplot2)

bills<-read.csv("ElectricBill.csv")
head(bills)
bills$DateIndex<-1:nrow(bills)

ggplot()+geom_line(data=bills,aes(x=DateIndex,y=Bill))

attach(bills)
Acf(Bill)
Pacf(Bill)

AR1<-arima(Bill,order=c(1,0,0))
AR2<-arima(Bill,order=c(2,0,0))
AR3<-arima(Bill,order=c(3,0,0))
AR4<-arima(Bill,order=c(4,0,0))
AR5<-arima(Bill,order=c(5,0,0))

tsdisplay(residuals(AR1),lag.max=15,main="AR(1) Resid. Diagnostics")
tsdisplay(residuals(AR2),lag.max=15,main="AR(2) Resid. Diagnostics")
tsdisplay(residuals(AR3),lag.max=15,main="AR(3) Resid. Diagnostics")
tsdisplay(residuals(AR4),lag.max=15,main="AR(4) Resid. Diagnostics")
tsdisplay(residuals(AR5),lag.max=15,main="AR(5) Resid. Diagnostics")

AIC(AR1)
AIC(AR2)
AIC(AR3)
AIC(AR4)
AIC(AR5)


ARIMA.fit<-auto.arima(Bill,seasonal=FALSE)
ARIMA.fit


#Notice here the automated procedure selected the AR(2) model which we know from the previous exercises that AR(4) has a lower AIC. This is due to the stepwise selection process of auto.arima. To get them to correspond,
ARIMA.fit<-auto.arima(Bill,seasonal=FALSE,stepwise=FALSE)
ARIMA.fit

#Providing forecasts of a final model fit is relatively simple to do. Syntatically, the h in the forescast function provides the future forecast up to h timepoints ahead. In addition, to the forecasts, I’ve overlayed the model fit on the previous data to get look at that as well.
plot(forecast(ARIMA.fit,h=10))
points(1:length(Bill),fitted(ARIMA.fit),type="l",col="blue")


#Providing forecasts for AR1
plot(forecast(AR1,h=10))
points(1:length(Bill),fitted(AR1),type="l",col="blue")

# Forecast for 100 observations
plot(forecast(ARIMA.fit,h=100))
points(1:length(Bill),fitted(ARIMA.fit),type="l",col="blue")

### Including Average temp in the model

plot(AvgTemp,Bill,xlab="Avg. Temperature")
ols<-lm(Bill~AvgTemp)
abline(ols)
text(80,200,paste("Cor=",round(cor(Bill,AvgTemp),2)))

#Lets examine the ACF and PACF of the residuals after regressing Bill on AvgTemp.
holdout.test<-window(ts(Bill),start=36)
train<-Bill[1:35]
predictor<-AvgTemp[1:35]
simpleols<-arima(train,order=c(0,0,0),xreg=predictor)
tsdisplay(residuals(simpleols),lag.max=15,main="Resid. Diagnostics of OLS")

#
ARIMA.with.Pred<-auto.arima(train,xreg=predictor,stepwise=FALSE)
ARIMA.with.Pred

tsdisplay(residuals(ARIMA.with.Pred),lag.max=15,main="Resid. Diagnostics with AR(4)")

plot(forecast(ARIMA.with.Pred,h=5,xreg=data.frame(predictor=AvgTemp[36:40])))
points(1:length(train),fitted(ARIMA.with.Pred),type="l",col="blue")
points(1:40,Bill,type="l")

newpred<-as.matrix(cbind(predictor,predictor^2))
colnames(newpred)<-c("Pred","Pred2")
ARIMA.with.Pred2<-auto.arima(train,xreg=newpred,stepwise=FALSE)
ARIMA.with.Pred2

tsdisplay(residuals(ARIMA.with.Pred2),lag.max=15,main="Resid. Diagnostics AR(4) Quadratic")


test.pred<-as.matrix(cbind(AvgTemp[36:40],AvgTemp[36:40]^2))
colnames(test.pred)<-c("Pred","Pred2")
plot(forecast(ARIMA.with.Pred2,h=5,xreg=test.pred))
points(1:length(train),fitted(ARIMA.with.Pred2),type="l",col="blue")
points(1:40,Bill,type="l")

casts.avgtemp<-forecast(ARIMA.with.Pred,h=5,xreg=data.frame(predictor=AvgTemp[36:40]))
accuracy(casts.avgtemp,Bill[36:40])

cast.avgtemp.quad<-forecast(ARIMA.with.Pred2,h=5,xreg=test.pred)
accuracy(cast.avgtemp.quad,Bill[36:40])


#### Question 14 ###########


ARIMA.without.Pred<-auto.arima(train,stepwise=FALSE)
ARIMA.without.Pred

tsdisplay(residuals(ARIMA.without.Pred),lag.max=15,main="Resid. Diagnostics with AR(4)")

plot(forecast(ARIMA.without.Pred,h=5))
points(1:length(train),fitted(ARIMA.without.Pred),type="l",col="blue")
points(1:40,Bill,type="l")

casts.fore<-forecast(ARIMA.without.Pred,h=5)
accuracy(casts.fore,Bill[36:40])




