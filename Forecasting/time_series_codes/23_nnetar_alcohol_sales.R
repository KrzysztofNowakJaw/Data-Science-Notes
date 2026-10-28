
### forecast alcohol sales with neural networks


require(ggplot2)

require(lubridate)

require(forecast)


alcohol <- read.csv("Alcohol_Sales.csv")

ggplot(alcohol, aes(mdy(DATE), sales, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Alcohol sales")

### create the training set and test set

alcohol_train <- alcohol[1:301,]

alcohol_test <- alcohol[302:325,]

ntest <- nrow(alcohol_test)

### create a seasonal time series

### (our time series presents a 12-month seasonal pattern)

sales_train_ts <- ts(alcohol_train$sales, freq = 12)

### build the network

net <- nnetar(sales_train_ts, repeats = 20)

### view some model parameters

net$p

net$P

net$size

### model output

net$fitted

net$residuals

### make forecasts in the test set

pred_net <- forecast(net, h = ntest)

pred_net$mean

accuracy(pred_net, alcohol_test$sales)

### plot the series

alcohol$pred <- c(net$fitted, pred_net$mean)

ggplot(alcohol)+ 
  geom_line(aes(mdy(DATE), sales, group = 1))+
  geom_line(aes(mdy(DATE), pred, group = 1), color = "dodgerblue4", size = 1.5)+
  geom_vline(aes(xintercept=mdy("1/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Year")+
  ylab("Alcohol sales")

### zoom into the test set

ggplot(alcohol[302:325,])+ 
  geom_line(aes(mdy(DATE), sales, group = 1))+
  geom_line(aes(mdy(DATE), pred, group = 1), color = "dodgerblue4", size = 1.5)+
  xlab("Year")+
  ylab("Alcohol sales")


### predict the future (next 12 months)

pred_net_f <- forecast(net, h = ntest+12, level = 0)

tail(pred_net_f$mean, 12)

plot(tail(pred_net_f$mean, 12))
