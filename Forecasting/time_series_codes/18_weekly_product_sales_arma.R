

### forecast weekly product sales with ARIMA

require(forecast)

require(tseries)

require(ggplot2)

require(lubridate)



prod <- read.csv("weekly_product_sales.csv")

ggplot(prod, aes(week, sales, group = 1)) + 
  geom_line()

### create the training set and the test set

prod_train <- prod[1:80,]

prod_test <- prod[81:100,]

ntest <- nrow(prod_test)

### create a time series with the sales value

sales_train_ts <- ts(prod_train$sales)

### test the series for stationarity

adf.test(sales_train_ts, alternative = "stationary")

### if the p-value of our test is smaller than 0.05
### we reject the null hypothesis
### so we conclude that our series is stationary

### the value of the d parameter will be zero
### (no differencing is necessary)

### in consequence, we will build an ARMA(p, q) model
### ARIMA(p, 0, q)

### identify the model
### (set the values for the parameters p and q)

### compute the autocorrelation functions
### and examine the autocorrelation charts

Acf(sales_train_ts)   # for the MA process

Pacf(sales_train_ts)  # for the AR process 

### we will set p = 14 and q = 14

### build the model

model <- Arima(sales_train_ts, order = c(14, 0, 14), method = "ML")

### see the model

summary(model)

### plot the fitted values (in the training set) and the residuals

plot(model$fitted)

plot(model$residuals)

### make forecasts in the test set

pred <- forecast(model, h = ntest, level = 0)

### compute the forecast accuracy in the test set

accuracy(pred, prod_test$sales)

### plot the forecast values

prod$pred <- c(model$fitted, pred$mean)

ggplot(prod)+ 
  geom_line(aes(week, sales, group = 1))+
  geom_line(aes(week, pred, group = 1), color = "dodgerblue4")+
  geom_vline(aes(xintercept=81), color = "darkorange2", size = 0.9)+
  xlab("Week")+
  ylab("Sales")
  

### zoom into the test set

ggplot(prod[81:100,])+ 
  geom_line(aes(week, sales, group = 1))+
  geom_line(aes(week, pred, group = 1), color = "dodgerblue4")+
  xlab("Week")+
  ylab("Product sales")

##################################################

### try a simpler model
### ARIMA(7, 0, 7)

model <- Arima(sales_train_ts, order = c(7, 0, 7), method = "ML")


### make forecasts in the test set

pred <- forecast(model, h = ntest, level = 0)

### compute the forecast accuracy in the test set

accuracy(pred, prod_test$sales)

### plot the forecast values

prod$pred <- c(model$fitted, pred$mean)

ggplot(prod)+ 
  geom_line(aes(week, sales, group = 1))+
  geom_line(aes(week, pred, group = 1), color = "dodgerblue4")+
  geom_vline(aes(xintercept=81), color = "darkorange2", size = 0.9)+
  xlab("Week")+
  ylab("Sales")


### zoom into the test set

ggplot(prod[81:100,])+ 
  geom_line(aes(week, sales, group = 1))+
  geom_line(aes(week, pred, group = 1), color = "dodgerblue4")+
  xlab("Week")+
  ylab("Product sales")

