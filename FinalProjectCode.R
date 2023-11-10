#Library Load

library(dplyr)
library(lubridate)
library(forecast)
library(TTR)
library(ggplot2)
library(tseries)
library(gridExtra)
library(lmtest)
library("urca")
library("xts")
library(rugarch)
library("FinTS")

#Reading Full Dataset

covid <- read.csv("Edited Version.csv",header=TRUE)
dim(covid)

#Checking Type and Class of date variable
typeof(covid$date)
class(covid$date)

#Convert Date From Character
covid$date <- as.Date(covid$date,"%d-%m-%y")

#Checking Type and Class of date variable Again
typeof(covid$date)
class(covid$date)

#Checking ranges of date variable
range(covid$date)

#Setting Testing and Training data

covid_test <- tail(covid[,c("new_cases_per_million","Vaccination","date")], 7)   #get 7 last days 
covid_test

covid_train <- head(covid[,c("new_cases_per_million","Vaccination","date")], nrow(covid) - nrow(covid_test)) #get the rest data
tail(covid_train)

#Create time series object with training set data

covid_ts<-xts(covid_train[,c("new_cases_per_million","Vaccination")],order.by = covid_train$date,frequency = 1)
head(covid_ts)

#Create training set plot

tsdisplay(covid_ts)

#Stationarity Checking For Individual Time Series Variable

adf.test(covid_ts[,c("new_cases_per_million")])
adf.test(covid_ts[,c("Vaccination")])

#Cointegration Test

jotest<-ca.jo(covid_ts[,c("new_cases_per_million","Vaccination")], type="trace", ecdet="none", spec="longrun")
summary(jotest)

#Differencing Once

covid_ts_d<-diff(covid_ts)
head(covid_ts_d)

#Plot on Difference Data

tsdisplay(covid_ts_d)

#Cointegration Test on Difference Data

jotest_d<-ca.jo(covid_ts_d[,c("new_cases_per_million","Vaccination")], type="trace", ecdet="none", spec="longrun")
summary(jotest_d)

#Testing Auto ARIMA

covid_arima_auto <- auto.arima(y = covid_ts_d$new_cases_per_million,ic="aicc",trace=T,xreg = covid_ts_d$Vaccination)
covid_arima_auto

#Box Plot For White Noise/Goodness of Fit Test

Box.test(covid_arima_auto$residuals, type = "Ljung-Box")

#Normality Test

shapiro.test(covid_arima_auto$residuals)

#Using GARCH Fit

#Removing first row
covid_ts_d_del<-covid_ts_d[-1,]
head(covid_ts_d_del)

#Testing if there are any ARCH effect
ArchTest(covid_ts_d_del)

#Testing GARCH order
garch(covid_ts_d_del[,c("new_cases_per_million")],grad="numerical",trace=FALSE)

#Setting ugarchfit

mm=list(armaOrder=c(5,5),include.mean=T,arfima=F,external.regressors=as.matrix(covid_ts_d_del$Vaccination))
vm=list(model="apARCH",garchOrder=c(1,1),external.regressors=as.matrix(covid_ts_d_del$Vaccination))
dm="std"
mdl=ugarchspec(variance.model=vm,mean.model=mm,distribution.model=dm)
garch_fit<-ugarchfit(data=covid_ts_d_del[,c("new_cases_per_million")],spec=mdl,out.sample=0,solver="solnp")
garch_fit

#Predicting Next 7 days data

prediction<-ugarchforecast(garch_fit,data=covid_ts_d_del$new_cases_per_million,n.ahead=7)
prediction

par(mfrow=c(1,1))
plot(prediction)





















