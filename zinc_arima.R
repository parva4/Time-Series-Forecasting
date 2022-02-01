library(tidyverse)
library(zoo)
library(forecast)
library(dplyr)
library(dygraphs)
library(timeSeries)
install.packages("rio")
library('rio')
library(tseries)
my_data<-read.csv("zinc.csv")
View(my_data)

#Converting the data type of date to Date
my_data$date<-as.Date(my_data$date,format="%d-%m-%Y")
class(my_data$date)
my_data<-na.omit(my_data)
View(my_data)

#Plotting date vs price
my_data%>%
  ggplot(aes(date,Price))+
  geom_line(size=1,col="red3")
  scale_x_date('Month')

TSObject=ts(my_data[,c('Price')])
#tsclean deletes outliers and missing data in a dataset
my_data$cleandata=tsclean(TSObject)
View(my_data)

ggplot(my_data,aes(date,cleandata))+
  geom_line(size=1,col='navyblue')+
  xlab('Date')+
  ylab('Cleaned data')+
  scale_x_date('Month')
#order of a moving average determines the smoothness of the curve
#higher the order, more the smoothness

my_data$cleandata_ma7=ma(my_data$cleandata,order=7)
my_data<-na.omit(my_data)
my_data$cleandata_ma30=ma(my_data$cleandata,order=30)
my_data<-na.omit(my_data)
ggplot(my_data,aes(date,cleandata))+
  geom_line(aes(date,cleandata,col="cleandata"))+
  geom_line(aes(date,cleandata_ma7,col="weekly moving average"))+
  geom_line(aes(date,cleandata_ma30,col="monthly moving average"))+
  ylab('Temp')+
  scale_x_date('Month')

#removing the NAs from cleandata_ma7
weekly_ma=ts(na.omit(my_data$cleandata_ma7),frequency=30)
#decomposition using stl function where s=seasonal, t=trend, l=irregular losses
#s. window is the number of consecutive years to be used 
#(continued-s.window) in estimating each value in the seasonal component

#setting s.window="periodic" so that the span (in lags) of the loess window 
#for seasonal extraction is calculated using defaults
decomp=stl(weekly_ma,s.window ="periodic")
plot(decomp)
#seasdj returns seasonally adjusted data by removing the
#seasonal component
deseasonal<-seasadj(decomp)
plot(deseasonal,main='w/o seasonality')

#augmented dickey fuller test to check 
#whether the time series is stationary
adf.test(weekly_ma,alternative="stationary")

#acf and pacf
acf(weekly_ma,main='')
pacf(weekly_ma,main='')

#differentiate
weekly_mad1=diff(deseasonal,differences=1)
plot(weekly_mad1)
adf.test(weekly_mad1,alternative="stationary")

acf(weekly_mad1,main='acf diff')
pacf(weekly_mad1,main='pacf diff')

#building an ARIMA model
#find the pdq values
fit<-auto.arima(deseasonal,seasonal=FALSE)
tsdisplay(residuals(fit),lag.max = 45,main='(1,1,1) Model Residuals')
dev.off()
# par("mar") #checks the margins
par(mar=c(1,1,1,1)) #set the margins to 1,1,1,1.

#1,1, represent p,d and q values
fit2<-arima(deseasonal,order=c(1,1,8))
#since there's a huge dip at 7
tsdisplay(residuals(fit2),lag.max = 25,main='(1,1,8) Model Residuals')

#forecasting for the period of 24 months
fcast<-forecast(fit2,h=24)
plot(fcast)

#holdout test for performance
holdout<-window(ts(deseasonal),start=330)
fit_no_holdout<-arima(ts(deseasonal[-c(350:397)]),order=c(1,1,8))
fcast_no_holdout<-forecast(fit_no_holdout,h=68)
plot(fcast_no_holdout)
lines(ts(deseasonal))

#seasonality should be added back to the data 
fit_w_seasonality<-auto.arima(deseasonal,seasonal=TRUE)
seasonal_fcast<-forecast(fit_w_seasonality,h=30)
plot(seasonal_fcast)

lines(ts(weekly_ma))
lines(ts(deseasonal))

# plot(forecast(auto.arima(ts(Price,frequency=30),D=1),h=30))
tsdisplay(residuals(fit_w_seasonality),lag.max=15,main='Seasonal Model Residual')
par(mar=c(1,1,1,1))
dev.off()

fit3<-auto.arima(deseasonal,seasonal=FALSE)
tsdisplay(residuals(fit3),lag.max=25,main='seasonal Model Residuals')

fit4<-arima(deseasonal,order = c(1,1,8))
tsdisplay(residuals(fit4),lag.max=25,main='Seasonal Model Residuals')

fit5<-arima(deseasonal,order=c(1,1,1))
tsdisplay(residuals(fit5),lag.max=25,main='Seasonal Model Residuals')

par(mfrow=c(2,2)) #plots all 4 following graphs in one frame, 2rX2c
par(mar=c(1,1,1,1))
fcast1<-forecast(fit_w_seasonality,h=30)
plot(fcast1)
lines(ts(weekly_ma))
lines(ts(deseasonal))

fcast2<-forecast(fit3,h=30)
plot(fcast2)

fcast3<-forecast(fit4,h=30)
plot(fcast3)

fcast4<-forecast(fit5,h=30)
plot(fcast4)


accuracy(fcast1) #99.12%
accuracy(fcast2) #99.12%
accuracy(fcast3) #99.22
accuracy(fcast4) #99.09

