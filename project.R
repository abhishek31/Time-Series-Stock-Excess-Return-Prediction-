##############################################################################
## Time Series-Stock-Excess-Return-Prediction
#############################################################################
## Load library

library(readxl)
library(astsa)
library(tseries)

#Data load

data<-read_excel('C://Users//abhis//OneDrive//Documents//Codes//Stock_Returns.xlsx',sheet='Sheet1')

############################################################################
## Data Exploration 
############################################################################

View(data)
nrow(data)

## Split into train and test

train_data<-data[1:696,]
test_data<-data[697:864,]

## Taking all years to visualize complete dataset

data_val <- ts(data$ExReturn, start=c(1930,1),frequency = 12)
class(data_val)
plot(data_val)
acf2(data_val)

## Build Y variable using train dataset

y <- ts(train_data$ExReturn, start=c(1931,1),frequency = 12)

## Augmented DF test to check the stationarity of data

adf.test(data_val)


## Decompose dataset into various components to see trend, seasonality and the random
## components
plot(decompose(data_val))

## we found some seasonality, but with insignificant lags. We did not see any trend and
## also the data was stationary

#############################################################################
##                       Data Modeling 
#############################################################################

## We now check manually each possible combination of p,d,q from acf and pcf plot to 
## come with best model based on the output of sarima i.e. p-value plot, residual plot and Q-Q plot

sarima(y,1,0,0,1,0,0,S=12)
out = arima(y,c(1,0,0),list(order=c(1,0,0),S=12))
acf2(out$residuals)
#ARMA 1,1
sarima(y,1,0,1,1,0,0,S=12)
out = arima(y,c(1,0,1),list(order=c(1,0,0),S=12))
acf2(out$residuals)

sarima(y,2,0,1,1,0,0,S=12)

sarima(y,3,0,2,1,0,0,S=12)

sarima(y,4,0,2,1,0,0,S=12)

sarima(y,5,0,2,1,0,0,S=12)


## ARMA 2,1 #okay
sarima(y,1,0,2,1,0,0,S=12)
out = arima(y,c(2,0,1),list(order=c(1,0,0),S=12))
acf2(out$residuals)

## Good pvalues and residuals
sarima(y,2,0,2,1,0,0,S=12)
## Good pvalues and residuals
sarima(y,2,0,3,1,0,0,S=12)

## not so good pvalues and residuals
sarima(y,2,0,4,1,0,0,S=12)

sarima(y,2,0,5,1,0,0,S=12)

sarima(y,3,0,1,1,0,0,S=12)

sarima(y,3,0,2,1,0,0,S=12)

sarima(y,3,0,3,1,0,0,S=12)

sarima(y,3,0,4,1,0,0,S=12)

sarima(y,3,0,5,1,0,0,S=12)

sarima(y,4,0,2,1,0,0,S=12)

sarima(y,4,0,3,1,0,0,S=12)

sarima(y,4,0,2,1,0,0,S=12)

sarima(y,4,0,3,1,0,0,S=12)

sarima(y,4,0,4,1,0,0,S=12)

#okay
sarima(y,4,0,5,1,0,0,S=12)
#okay
sarima(y,5,0,2,1,0,0,S=12)

sarima(y,5,0,3,1,0,0,S=12)

sarima(y,5,0,4,1,0,0,S=12)

sarima(y,5,0,5,1,0,0,S=12)

## Standardized Residuals - There are no trend detected. The plot looks like white noise 
## with a few outliers.
## ACF of Residuals: Still there are few lags that are significant.
## Normal Q-Q Plot of Standardized Residuals - There are few outliers which
## indicates minor departure from normality.
## P-Values for Ljung-Box Statistic - All the p-values are on or above the blue dotted line 
## which shows that the we fail to reject the null hypothesis that the residuals are independent.
## Hence the model ARIMA(2,0,2), ARIMA(2,0,3) is the best possible 
## combination to fitting the data largely.

###################################################################
## Prediction without additional variable and including all years
###################################################################
library(forecast)
library(forecast)
#one step ahead prediction for auto arima
model <- auto.arima(y,seasonal = TRUE)
model.fit <- Arima(y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for
#MSPE - Mean square prediction error.
sum((y1-onestep.for)^2)/168
#16.59

#one step ahead prediction for sarima(2,0,2)
model<-Arima(100+y,order=c(2,0,2),seasonal=list(order=c(1,0,0),period=12),lambda=0)
model.fit <- Arima(100+y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/168
#16.60

#one step ahead prediction for sarima(2,0,3)
model<-Arima(100+y,order=c(2,0,3),seasonal=list(order=c(1,0,0),period=12),lambda=0)
model.fit <- Arima(100+y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/168
#16.51


#one step ahead prediction for sarima(5,0,3)
model<-Arima(100+y,order=c(5,0,3),seasonal=list(order=c(1,0,0),period=12),lambda=0)
model.fit <- Arima(100+y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/168
#16.52

#Bar plot for MSPE
group<-factor(c("auto arima","sarima(2,0,2)","sarima(2,0,3)","sarima(5,0,3)"))
group
values<-c(16.59,16.60,16.51,16.52)
barplot(values,names.arg = group,col=c("lightblue","mistyrose", "lightcyan","lavender")
        ,main = "Plot of MSPE for Different Models without additional variable",
        xlab="Models",ylab="MSPE", legend.text = TRUE)

## From above observation, we found that sarima(2,0,2) and sarima(2,0,3) were actually
## have less MSPE and sarima(2,0,3) have least value amoung all.

########################################################################
## Prediction using SARIMA With additional variables including all years
########################################################################
data1<-exp(data[,4]/100)
head(data1)
data2<-cbind(data[,3],data1)
data2
train_data2<-data2[1:696,]
test_data2<-data2[697:864,]
test_data2
y1 <- ts(test_data$ExReturn, start=c(1989,1),frequency = 12)
y1
#auto Arima for multivariate features
#one step ahead prediction for auto arima
model9<-auto.arima(y, xreg=train_data2$ln_DivYield, p = 5, max.q =5,
                   max.P = 3, max.Q = 0, max.order = 5, max.d = 0, 
                   max.D = 0, start.p = 1)
model9
model.fit <- Arima(y1, model=model9,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for
#MSPE
sum((y1-onestep.for)^2)/168
#18.02

#one step ahead prediction for sarima(2,0,2)
model<-Arima(100+y,order=c(2,0,2),seasonal=list(order=c(1,0,0),period=12),xreg=train_data2$ln_DivYield,lambda=0)
model.fit <- Arima(100+y1, model=model,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/168
#18.87

#one step ahead prediction for sarima(2,0,3)
model<-Arima(100+y,order=c(2,0,3),seasonal=list(order=c(1,0,0),period=12),xreg=train_data2$ln_DivYield,lambda=0)
model.fit <- Arima(100+y1, model=model,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/168
#18.78

#one step ahead prediction for sarima(5,0,3)
model<-Arima(100+y,order=c(5,0,3),seasonal=list(order=c(1,0,0),period=12),xreg=train_data2$ln_DivYield,lambda=0)
model.fit <- Arima(100+y1, model=model,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/168
#17.37

#bar plot of MSPE
group<-factor(c("auto arima","sarima(2,0,2)","sarima(2,0,3)","sarima(5,0,3)"))
group
values<-c(18.02,18.87,18.78,17.37)
barplot(values,names.arg = group,col=c("lightblue", "mistyrose", "lightcyan","lavender")
        ,main = "Plot of MSPE for Different Models with additional variable",
        xlab="Models",ylab="MSPE", legend.text = TRUE)

## From above observation, we found that sarima(5,0,3) and sarima(2,0,2) were actually
## have less MSPE and sarima(5,0,3) have least value amoung all.

##############################################################################
## Prediction using prophet including all years of data
##############################################################################
library(zoo)
library(prophet)
library(ggplot2)

## Build Dataframe from training data

df<-data.frame(date=as.Date(as.yearmon(time(y))),Y=as.matrix(y))

## Change name of columns as prophet takes input ds as the name of input columns 
## and y as variable, we are predicting.
names(df) = c("ds", "y")
df
qplot(ds,y,data=df)

## Model building
m = prophet(df)

## Forecasting
future = make_future_dataframe(m, periods = 168, include_history = TRUE)
forecast = predict(m, future)
forecast$yhat
MSPE_prophet<-mean((test_data$ExReturn-forecast$yhat[697:864])^2)
MSPE_prophet

# Visualize forecast
plot(m, forecast)
prophet_plot_components(m, forecast)

##################################################################################
## Now we perform every process exculding 10 years of data as from observation we
## found that first 10 years of data has lots of varitions and noisey.
##                               Excluding 10 Years 
###################################################################################

## Data load

library(readxl)
library(astsa)
library(tseries)

data<-read_excel('C:/Agni/MSDA/2nd_sem/MSA_8200/mini_project/Stock_Returns_1931_2002.xlsx',sheet='Sheet1')

###############################################################
## Data Exploration
###############################################################

View(data)
nrow(data)
## split into train and test
train_data<-data[121:720,]
test_data<-data[721:864,]

## Taking all years to visualize complete dataset

data_val <- ts(data$ExReturn, start=c(1941,1),frequency = 12)
class(data_val)
plot(data_val)
acf2(data_val)

## Build Y variable using train dataset

y <- ts(train_data$ExReturn, start=c(1941,1),frequency = 12)

## Augmented DF test to check the stationarity of data

adf.test(data_val)

## Decompose dataset into various components to see trend, seasonality and the random
## components
plot(decompose(data_val))

## we found some seasonality, but with insignificant lags. We did not see any trend and
## also the data was stationary

#############################################################################
##                       Data Modeling 
#############################################################################

## We now check manually each possible combination of p,d,q from acf and pcf plot to 
## come with best model based on the output of sarima i.e. p-value plot, residual plot and Q-Q plot


## AR1
sarima(y,1,0,0,1,0,0,S=12)
out = arima(y,c(1,0,0),list(order=c(1,0,0),S=12))
acf2(out$residuals)

## ARMA 1,1
sarima(y,1,0,1,1,0,0,S=12)
out = arima(y,c(1,0,1),list(order=c(1,0,0),S=12))
acf2(out$residuals)

sarima(y,2,0,1,1,0,0,S=12)

sarima(y,3,0,2,1,0,0,S=12)

sarima(y,4,0,2,1,0,0,S=12)

sarima(y,5,0,2,1,0,0,S=12)


## ARMA 2,1 #okay
sarima(y,1,0,2,1,0,0,S=12)
out = arima(y,c(2,0,1),list(order=c(1,0,0),S=12))
acf2(out$residuals)

## Good pvalues and residuals
sarima(y,2,0,2,1,0,0,S=12)
## Good pvalues and residuals
sarima(y,2,0,3,1,0,0,S=12)

sarima(y,2,0,4,1,0,0,S=12)

sarima(y,2,0,5,1,0,0,S=12)

sarima(y,3,0,1,1,0,0,S=12)

sarima(y,3,0,2,1,0,0,S=12)

sarima(y,3,0,3,1,0,0,S=12)

sarima(y,3,0,4,1,0,0,S=12)

sarima(y,3,0,5,1,0,0,S=12)

sarima(y,4,0,2,1,0,0,S=12)

sarima(y,4,0,3,1,0,0,S=12)

sarima(y,4,0,2,1,0,0,S=12)

sarima(y,4,0,3,1,0,0,S=12)

sarima(y,4,0,4,1,0,0,S=12)

#okay
sarima(y,4,0,5,1,0,0,S=12)
#okay
sarima(y,5,0,2,1,0,0,S=12)

## Good pvalues and residuals
sarima(y,5,0,3,1,0,0,S=12)

#okay
sarima(y,5,0,4,1,0,0,S=12)
sarima(y,5,0,5,1,0,0,S=12)

## Standardized Residuals - There are no trend detected. The plot looks like white noise 
## with a few outliers.
## ACF of Residuals: Still there are few lags that are significant.
## Normal Q-Q Plot of Standardized Residuals - There are few outliers which
## indicates minor departure from normality.
## P-Values for Ljung-Box Statistic - All the p-values are on or above the blue dotted line 
## which shows that the we fail to reject the null hypothesis that the residuals are independent.
## Hence the model ARIMA(2,0,2), ARIMA(5,0,3) is the best possible 
## combination to fitting the data largely.


###################################################################
## Prediction without additional variable and excluding 10 years
###################################################################

y1 <- ts(test_data$ExReturn, start=c(1991,1),frequency = 12)
y1
library(forecast)
library(forecast)

## one step ahead prediction for auto arima
model <- auto.arima(y,seasonal = TRUE)
model.fit <- Arima(y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for
## MSPE
sum((y1-onestep.for)^2)/144
#15.25

#one step ahead prediction for sarima(2,0,2)
model<-Arima(100+y,order=c(2,0,2),seasonal=list(order=c(1,0,0),period=12),lambda=0)
model.fit <- Arima(100+y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/144
#14.96

#one step ahead prediction for sarima(2,0,3)
model<-Arima(100+y,order=c(2,0,3),seasonal=list(order=c(1,0,0),period=12),lambda=0)
model.fit <- Arima(100+y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/144
#15.19


#one step ahead prediction for sarima(5,0,3)
model<-Arima(100+y,order=c(5,0,3),seasonal=list(order=c(1,0,0),period=12),lambda=0)
model.fit <- Arima(100+y1, model=model)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/144
#15.01

#Bar plot for MSPE
group<-factor(c("auto arima","sarima(2,0,2)","sarima(2,0,3)","sarima(5,0,3)"))
group
values<-c(15.25,14.96,15.19,15.01)
barplot(values,names.arg = group,col=c("lightblue","mistyrose", "lightcyan","lavender")
        ,main = "Plot of MSPE for Different Models without additional variable",
        xlab="Models",ylab="MSPE", legend.text = TRUE)

## From above observation, we found that sarima(2,0,2) and sarima(5,0,3) were actually
## have less MSPE and sarima(5,0,3) have least value amoung all.


########################################################################
## Prediction using SARIMA With additional variables excluding 10 years
########################################################################

data1<-exp(data[,4]/100)
head(data1)
data2<-cbind(data[,3],data1)
data2
train_data2<-data2[121:720,]
test_data2<-data2[721:864,]
test_data2

## auto Arima for multivariate features
## one step ahead prediction for auto arima
model9<-auto.arima(y, xreg=train_data2$ln_DivYield, p = 5, max.q =5,
                   max.P = 3, max.Q = 0, max.order = 5, max.d = 0, 
                   max.D = 0, start.p = 1)
model9
model.fit <- Arima(y1, model=model9,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for
#MSPE
sum((y1-onestep.for)^2)/144
#15.43

#one step ahead prediction for sarima(2,0,2)
model<-Arima(100+y,order=c(2,0,2),seasonal=list(order=c(1,0,0),period=12),xreg=train_data2$ln_DivYield,lambda=0)
model.fit <- Arima(100+y1, model=model,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/144
#15.23

#one step ahead prediction for sarima(2,0,3)
model<-Arima(100+y,order=c(2,0,3),seasonal=list(order=c(1,0,0),period=12),xreg=train_data2$ln_DivYield,lambda=0)
model.fit <- Arima(100+y1, model=model,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/144
#15.27

#one step ahead prediction for sarima(5,0,3)
model<-Arima(100+y,order=c(5,0,3),seasonal=list(order=c(1,0,0),period=12),xreg=train_data2$ln_DivYield,lambda=0)
model.fit <- Arima(100+y1, model=model,xreg=test_data2$ln_DivYield)
onestep.for <- fitted(model.fit)
onestep.for<-onestep.for-100
#MSPE
sum((y1-onestep.for)^2)/144
#15.23

## Bar plot of MSPE
group<-factor(c("auto arima","sarima(2,0,2)","sarima(2,0,3)","sarima(5,0,3)"))
group
values<-c(15.43,15.23,15.27,15.23)
barplot(values,names.arg = group,col=c("lightblue", "mistyrose", "lightcyan","lavender")
        ,main = "Plot of MSPE for Different Models with additional variable",
        xlab="Models",ylab="MSPE", legend.text = TRUE)


## From above observation, we found that sarima(5,0,3) and sarima(2,0,2) were actually
## have less MSPE and sarima(2,0,2) have least value amoung all.


##############################################################################
## Prediction using prophet excluding 10 years of data
##############################################################################

library(zoo)
library(prophet)
library(ggplot2)

## Build Dataframe from training data
df<-data.frame(date=as.Date(as.yearmon(time(y))),Y=as.matrix(y))

## Change name of columns as prophet takes input ds as the name of input columns 
## and y as variable, we are predicting.
names(df) = c("ds", "y")
df
qplot(ds,y,data=df)

## Model building
m = prophet(df)

## Forecasting
future = make_future_dataframe(m, periods = 144, include_history = TRUE)
forecast = predict(m, future)
forecast$yhat
MSPE_prophet<-mean((test_data$ExReturn-forecast$yhat[601:744])^2)
MSPE_prophet

## Visualize forecast
plot(m, forecast)
prophet_plot_components(m, forecast)


############################################################################
##                                END
############################################################################