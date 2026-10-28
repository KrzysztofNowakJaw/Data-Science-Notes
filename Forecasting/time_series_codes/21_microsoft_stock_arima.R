

### forecast Microsoft stock prices
### with ARIMA


require(forecast)

require(tseries)

require(ggplot2)

require(lubridate)



ms <- read.csv("Microsoft_Stock.csv")

ggplot(ms, aes(Instant, Close, group = 1)) + 
  geom_line()+
  xlab("Instant")+
  ylab("Stock value")

### create the training set and the test set

ms_train <- ms[1:1501,]

ms_test <- ms[1502:1511,]

ntest <- nrow(ms_test)


### create a time series with the stock values

close_train_ts <- ts(ms_train$Close)

### test for stationarity

adf.test(close_train_ts, alternative = "stationary")

### our series is not stationary

### we have to difference it to obtain a stationary series

### compute the first order difference

close_train_d1 <- diff(close_train_ts, differences = 1)

plot(close_train_d1)

### test for stationarity again

adf.test(close_train_d1, alternative = "stationary")

### the new series is stationary

### so we set d = 1

### identify the rest of the model
### (set the values for the parameters p and q)

### compute the autocorrelation functions
### and examine the autocorrelation charts

Acf(close_train_d1)

Pacf(close_train_d1)

### based on the charts we set p = 9 and q = 15

model <- Arima(close_train_ts, order = c(9, 1, 15), method = "ML")

### make the forecast

pred <- forecast(model, h = ntest, level = 0)

### compute the forecast accuracy in the test set

accuracy(pred, ms_test$Close)

### plot the forecasts

ms$pred <- c(model$fitted, pred$mean)

ggplot(ms)+ 
  geom_line(aes(Instant, Close, group = 1))+
  geom_line(aes(Instant, pred, group = 1), color = "dodgerblue4")+
  geom_vline(aes(xintercept=1502), color = "darkorange2", size = 0.9)+
  xlab("Instant")+
  ylab("Stock value")

### zoom into the test series

ggplot(ms[1502:1511,])+ 
  geom_line(aes(Instant, Close, group = 1))+
  geom_line(aes(Instant, pred, group = 1), color = "dodgerblue4")+
  xlab("Instant")+
  ylab("Stock value")

