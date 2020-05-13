## loading data##

dss <- read.csv("DepartmentStoreSales_final.csv",header = TRUE)
dss

## line plot ##
dssts <- ts(dss)
dssts
plot.ts(dssts[,2])


### Data Partition###

dsstraining <- dss[1:20,]
dssvalidation <- dss[21:nrow(dss),]

### rmse function#####
rmse<-function(error){
  sqrt(mean(error^2))
}
MAPE<-function(error,sales){
  mean(abs(error/sales))*100  
}
##linear trend model##

lineardss <- lm(Sales~Quarter,dsstraining)

lineardss
Pre_valid<-predict(lineardss,newdata = dssvalidation[,-2])
error<-dssvalidation$Sales-Pre_valid
rmse(error)
MAPE(error,dssvalidation$Sales)
plot(dss$Quarter,dss$Sales,type="l")
par(new=TRUE)
pre_fulldata<-predict(lineardss,newdata = dss[,-2])
plot(dss$Quarter,pre_fulldata,type="l",col="green",yaxt="n",ylab="")
plot(dsstraining$Quarter,residuals(lineardss),type="l",col="black")

### linear trend with seasonality####

dss1 <- lm(Sales~Quarter+d1+d2+d3,dsstraining)
dss1
Pre_valid<-predict(dss1,newdata = dssvalidation[,-2])
error<-dssvalidation$Sales-Pre_valid
rmse(error)
MAPE(error,dssvalidation$Sales)
plot(dss$Quarter,dss$Sales,type="l")
par(new=TRUE)
pre_fulldata<-predict(dss1,newdata = dss[,-2])
plot(dss$Quarter,pre_fulldata,type="l",col="green",yaxt="n",ylab="")
plot(dsstraining$Quarter,residuals(dss1),type="l",col="black")

### binomial trend with seasonality###

dss2 <- lm(Sales~Quarter+Qurter2+d1+d2+d3,dsstraining)
dss2
Pre_valid<-predict(dss2,newdata = dssvalidation[,-2])
error<-dssvalidation$Sales-Pre_valid
rmse(error)
MAPE(error,dssvalidation$Sales)
plot(dss$Quarter,dss$Sales,type="l")
par(new=TRUE)
pred_fulldata<-predict(dss2,newdata = dss[,-2])
plot(dss$Quarter,pred_fulldata,type="l",col="black",yaxt="n",ylab="")
plot(dsstraining$Quarter,residuals(dss2),type="l",col="black") 

#ARIMA model in R
install.packages("forecast")
library(forecast)
dssts_filter <- dssts[1:20,c(2)]
tsData = ts(dssts_filter, start = c(2017,1), frequency = 4)
tsData

y = auto.arima(tsData)
y
plot(forecast(y, h=4))
pred <- forecast(y, h=4)
pred

pred_1 <- data.frame(pred)
pred_1

pred_1$original <- dssts[21:24,2]
pred_1$residual <- pred_1$Point.Forecast-pred_1$original
rmse(pred_1$residual)
MAPE(error,pred_1$original)

#Moving average demonstration

library(smooth)
library(zoo)

births <- scan("C:/Personal/Module 9/nybirths.txt")
class(births)
birth.ts <- ts(births, start=c(1946,1), frequency = 12)
class(birth.ts)

### Data Partition###

birthtraining <- birth.ts[1:156]
birthvalidation <- birth.ts[157:168]
ma <- sma(birthtraining, h=12)
summary(ma)
ma$forecast
error<- birthvalidation-ma$forecast
rmse(error)
MAPE(error,birthvalidation)
cbind(birthvalidation,ma$forecast,error)

#Exponential smoothing models

plot(birth.ts)
par(mfrow=c(1,1))
plot(decompose(birth.ts))

#Simple exponential smoothing
#Use ses or ets methods for exponential smoothing
library(forecast)
fit_ses <- ses(birthtraining, h=12)
summary(fit_ses)
plot(forecast(fit_ses))
plot(fit_ses$residuals)
values <- fit_ses$mean
error_ses <- birthvalidation-values
rmse(error_ses)
MAPE(error_ses,birthvalidation)
cbind(birthvalidation,values,error)

#Holt's exponential smoothing
fit_holt <- holt(birthtraining, h=12)
summary(fit_holt)
plot(forecast(fit_holt))
values <- fit_holt$mean
error_holt <- birthvalidation-values
rmse(error_holt)
MAPE(error_holt,birthvalidation)
cbind(birthvalidation,values,error_holt)

#We can also deseasonalize time series before using Holt's exponential smoothing
decompose_birth = decompose(birth.ts, "additive")
adjust_birth = birth.ts - decompose_birth$seasonal
plot(adjust_birth)

fit_holt_deseasonalize <- holt(adjust_birth)
summary(fit_holt_deseasonalize)
plot(forecast(fit_holt_deseasonalize))

#Holtwinter's exponential smoothing
birthtraining.ts <- ts(birthtraining, start=c(1946,1), frequency = 12)
fit_holt_winter <- hw(y = birthtraining.ts, h=12)
summary(fit_holt_winter)
plot(forecast(fit_holt_winter))
values <- fit_holt_winter$mean
error_holt_winter <- birthvalidation-values
rmse(error_holt_winter)
MAPE(error_holt_winter,birthvalidation)
cbind(birthvalidation,values,error_holt_winter)
acf(fit_holt_winter$residuals)

#Fit automated forecasting methods
fit_auto <- forecast(birth.ts)
summary(fit_auto)
#Plot the forecasted values
plot(forecast(fit_auto))
acf(fit_auto$residuals)




