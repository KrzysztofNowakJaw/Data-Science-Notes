
### forecast the Microsoft stock prices with neural networks


require(ggplot2)

require(lubridate)

require(forecast)



ms <- read.csv("Microsoft_Stock.csv")

ggplot(ms) + 
  geom_line(aes(Instant, Close, group = 1))+
  xlab("Instant")+
  ylab("Stock price")


### create the training set and the test set

ms_train <- ms[1:1501,]

ms_test <- ms[1502:1511,]

ntest <- nrow(ms_test)

### create the time series for the closing values

close_train_ts <- ts(ms_train$Close)

### build the network

net <- nnetar(close_train_ts, repeats = 20)

### view some model parameters

net$p

net$P

net$size

### make forecasts in the test set

pred_net <- forecast(net, h = ntest)

pred_net$mean

accuracy(pred_net, ms_test$Close)

### plot the series

ms$Pred <- c(net$fitted, pred_net$mean)

ggplot(ms) + 
  geom_line(aes(Instant, Close, group = 1))+
  geom_line(aes(Instant, Pred, group = 1), color = "dodgerblue4", size = 1.5)+
  geom_vline(aes(xintercept=1501), color = "darkorange2", size = 0.9)+
  xlab("Instant")+
  ylab("Stock price")

### zoom into the test data

ggplot(ms[1502:1511,])+ 
  geom_line(aes(Instant, Close, group = 1))+
  geom_line(aes(Instant, Pred, group = 1), color = "dodgerblue4", size = 1.5)+
  xlab("Instant")+
  ylab("Stock price")



### predict the future (next 5 days)

pred_net_f <- forecast(net, h = ntest+5, level = 0)

tail(pred_net_f$mean, 5)

plot(tail(pred_net_f$mean, 5))

