

### forecast alcohol sales in the US
### with ARIMA

require(forecast)

require(tseries)

require(ggplot2)

require(lubridate)



alcohol <- read.csv("Alcohol_Sales.csv")

ggplot(alcohol, aes(mdy(DATE), sales, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Alcohol sales")

### zoom in to detect a seasonal pattern

ggplot(alcohol[1:48,], aes(mdy(DATE), sales, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Alcohol sales")

### in this chart we can notice an yearly pattern
### so our seasonal period will be 12 months

### create the training set and the test set

alcohol_train <- alcohol[1:301,]

alcohol_test <- alcohol[302:325,]

ntest <- nrow(alcohol_test)


### create a seasonal time series with the sales values

sales_train_ts <- ts(alcohol_train$sales, frequency = 12)

sales_train_ts

### test our series for stationarity

### we set the k value (the lag order to calculate the test) to 12
### because we have 12 seasonal periods

### (the default value of k is 6)

adf.test(sales_train_ts, k = 12, alternative = "stationary")

### our series is not stationary

### we have to difference it to obtain a stationary series

### compute the first order difference

sales_train_d1 <- diff(sales_train_ts, differences = 1)

plot(sales_train_d1)

### run the stationarity test for the new series

adf.test(sales_train_d1, k = 12, alternative = "stationary")

### the new series is stationary

### in conclusion, we set d = 1

### identify the rest of the model
### (set the values for the parameters p and q)

### compute the autocorrelation functions
### and examine the autocorrelation charts

Acf(sales_train_d1)

Pacf(sales_train_d1)

### based on the charts we set p = 12 and q = 12

### build the model

model <- Arima(sales_train_ts, order = c(12, 1, 12), method = "ML")

### get the coefficients

summary(model)

### plot the fitted values and the residuals

plot(model$fitted)

plot(model$residuals)

### make forecasts

pred <- forecast(model, h = ntest, level = 0)

### compute the forecast accuracy in the test set

accuracy(pred, alcohol_test$sales)

### plot the forecasts

alcohol$pred = c(model$fitted, pred$mean)

ggplot(alcohol)+ 
  geom_line(aes(mdy(DATE), sales, group = 1))+
  geom_line(aes(mdy(DATE), pred, group = 1), color = "dodgerblue4")+
  geom_vline(aes(xintercept=mdy("2/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Date")+
  ylab("Alcohol sales")

### zoom into the test set

ggplot(alcohol[302:325,])+ 
  geom_line(aes(mdy(DATE), sales, group = 1))+
  geom_line(aes(mdy(DATE), pred, group = 1), color = "dodgerblue4")+
  xlab("Date")+
  ylab("Alcohol sales")


