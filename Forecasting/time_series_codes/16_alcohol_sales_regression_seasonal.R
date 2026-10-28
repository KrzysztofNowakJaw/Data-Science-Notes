
### forecast alcohol sales
### using regression models with both trend and seasonality

require(ggplot2)

require(lubridate)

require(forecast)


alcohol <- read.csv("Alcohol_Sales.csv")

ggplot(alcohol, aes(mdy(DATE), sales, group = 1)) + 
  geom_line()+
  xlab("Year")+
  ylab("Alcohol sales")

### create the training set and the test set

alcohol_train <- alcohol[1:301,]

alcohol_test <- alcohol[302:325,]

ntest <- nrow(alcohol_test)

### first we'll build a couple of models with trend only
### (ignoring seasonality)

### create the time series

sales_train_ts <- ts(alcohol_train$sales)

### we will try two trend types: linear and quadratic
### then retain the best one (with the highest forecast accuracy)

### linear trend

model_trend_l <- tslm(sales_train_ts ~ trend)

pred_trend_l <- forecast(model_trend_l, h = ntest, level = 0)

### compute accuracy

accuracy(pred_trend_l, alcohol_test$sales)

### plot the series

alcohol_train$pred_l_train <- model_trend_l$fitted.values

alcohol_test$pred_l_test <- pred_trend_l$mean

ggplot()+
  geom_line(data = alcohol, aes(mdy(DATE), sales, group = 1))+
  geom_line(data = alcohol_train, aes(mdy(DATE), pred_l_train),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = alcohol_test, aes(mdy(DATE), pred_l_test),
            color = "dodgerblue4", size = 1.2)+
  geom_vline(aes(xintercept=mdy("2/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Year")+
  ylab("alcohol sales")

### quadratic trend

model_trend_q <- tslm(sales_train_ts ~ trend + I(trend^2))

pred_trend_q <- forecast(model_trend_q, h = ntest, level = 0)

### accuracy

accuracy(pred_trend_q, alcohol_test$sales)

### plot the forecast

alcohol_train$pred_q_train <- model_trend_q$fitted.values

alcohol_test$pred_q_test <- pred_trend_q$mean

ggplot()+
  geom_line(data = alcohol, aes(mdy(DATE), sales, group = 1))+
  geom_line(data = alcohol_train, aes(mdy(DATE), pred_q_train),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = alcohol_test, aes(mdy(DATE), pred_q_test),
            color = "dodgerblue4", size = 1.2)+
  geom_vline(aes(xintercept=mdy("2/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Year")+
  ylab("alcohol sales")




##################

### build a model with quadratic trend and seasonality
### to forecast alcohol sales in the United States

### zoom into the series to identify seasons

ggplot()+
  geom_line(data = alcohol[1:60,], aes(mdy(DATE), sales, group = 1))

### we can notice a 12-month pattern in the chart
### so we will analyze a model with 12 seasons (months)

### create the seasonal time series

sales_train_ts_season <- ts(alcohol_train$sales, freq = 12)

### build the model

model_season <- tslm(sales_train_ts_season ~ trend + I(trend^2) + season)

### view the model

model_season

summary(model_season)

model_season$fitted.values

### make forecasts in the test set

pred_season <- forecast(model_season, h = ntest, level = 0)

pred_season$mean

### compute accuracy

accuracy(pred_season, alcohol_test$sales)


### plot the series

alcohol_train$pred_train_season <- model_season$fitted.values

alcohol_test$pred_test_season <- pred_season$mean


ggplot()+
  geom_line(data = alcohol, aes(mdy(DATE), sales, group = 1))+
  geom_line(data = alcohol_train, aes(mdy(DATE), pred_train_season),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = alcohol_test, aes(mdy(DATE), pred_test_season),
            color = "dodgerblue4", size = 1.2)+
  geom_vline(aes(xintercept=mdy("2/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Year")+
  ylab("alcohol sales")

### zoom into the chart to see the test set better

ggplot()+
  geom_line(data = alcohol[302:325,], aes(mdy(DATE), sales, group = 1))+
  geom_line(data = alcohol_test, aes(mdy(DATE), pred_test_season),
            color = "dodgerblue4", size = 1.2)+
  xlab("Year")+
  ylab("alcohol sales")

