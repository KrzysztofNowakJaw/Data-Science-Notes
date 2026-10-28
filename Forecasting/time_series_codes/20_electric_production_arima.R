

### forecast the electricity production in the US
### with ARIMA

require(forecast)

require(tseries)

require(ggplot2)

require(lubridate)




elp <- read.csv("Electricity_Production.csv")

ggplot(elp, aes(mdy(DATE), Value, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Electric production")

### zoom in to detect a seasonal pattern

ggplot(elp[1:48,], aes(mdy(DATE), Value, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Electric production")

### in this chart we can notice a yearly pattern
### so our seasonal period will be 12 months

### create the training set and the test set

elp_train <- elp[1:373,]

elp_test <- elp[374:397,]

ntest <- nrow(elp_test)


### create a seasonal time series with the production values

prod_train_ts <- ts(elp_train$Value, frequency = 12)

plot(prod_train_ts)

### test our series for stationarity

### we set the k value (the lag order to calculate the test) to 12
### because we have 12 seasonal periods

adf.test(prod_train_ts, k = 12, alternative = "stationary")

### our series is not stationary

### we have to difference it to obtain a stationary series

### compute the first order difference

prod_train_d1 <- diff(prod_train_ts, differences = 1)

plot(prod_train_d1)

### run the stationarity test for the new series

adf.test(prod_train_d1, k = 12, alternative = "stationary")

### the new series is stationary

### in conclusion, we set d = 1

### identify the rest of the model
### (set the values for the parameters p and q)

### compute the autocorrelation functions
### and examine the autocorrelation charts

Acf(prod_train_d1)

Pacf(prod_train_d1)

### based on the charts we set p = 12 and q = 12

### build the model

model <- Arima(prod_train_ts, order = c(12, 1, 12), method = "ML")

### plot the fitted values and the residuals

plot(model$fitted)

plot(model$residuals)

### make forecasts

pred <- forecast(model, h = ntest, level = 0)

### compute the forecast accuracy in the test set

accuracy(pred, elp_test$Value)

### plot the forecasts

elp$pred <- c(model$fitted, pred$mean)

ggplot(elp)+ 
  geom_line(aes(mdy(DATE), Value, group = 1))+
  geom_line(aes(mdy(DATE), pred, group = 1), color = "dodgerblue4")+
  geom_vline(aes(xintercept=mdy("02-01-2016")), color = "darkorange2", size = 0.9)+
  xlab("Date")+
  ylab("Electric production")

### zoom into the test set

ggplot(elp[374:397,])+ 
  geom_line(aes(mdy(DATE), Value, group = 1))+
  geom_line(aes(mdy(DATE), pred, group = 1), color = "dodgerblue4")+
  xlab("Date")+
  ylab("Electric production")

