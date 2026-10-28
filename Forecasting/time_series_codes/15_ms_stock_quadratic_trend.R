
### forecast MS stock prices
### with the regression technique
### using a quadratic trend

require(ggplot2)

require(lubridate)

require(forecast)


ms <- read.csv("Microsoft_Stock.csv")

ggplot(ms, aes(Instant, Close, group = 1)) + 
  geom_line()+
  xlab("Instant")+
  ylab("Close")

### create the training set and the test set

ms_train <- ms[1:1506,]

ms_test <- ms[1507:1511,]

ntest <- nrow(ms_test)

### create the time series

close_train_ts <- ts(ms_train$Close)

### build the model

model_q <- tslm(close_train_ts ~ trend + I(trend^2))

### view the model summary

summary(model_q)

### make forecasts in the test set

pred_q <- forecast(model_q, h = ntest, level = 0)

pred_q$mean

### compute accuracy

accuracy(pred_q, ms_test$Close)

### create the prediction variables

ms_train$pred_q_train <- model_q$fitted.values

ms_test$pred_q_test <- pred_q$mean


### plot the series

ggplot()+
  geom_line(data = ms, aes(Instant, Close, group = 1))+
  geom_line(data = ms_train, aes(Instant, pred_q_train),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = ms_test, aes(Instant, pred_q_test),
            color = "dodgerblue4", size = 1.2)+
  xlab("Instant")+
  ylab("Stock value")

### zoom into the chart

ggplot()+
  geom_line(data = ms[1507:1511,], aes(Instant, Close, group = 1))+
  geom_line(data = ms_test, aes(Instant, pred_q_test),
            color = "dodgerblue4", size = 1.2)+
  xlab("Instant")+
  ylab("Stock value")


