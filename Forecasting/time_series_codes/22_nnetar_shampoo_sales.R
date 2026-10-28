
### forecast shampoo sales with neural networks


require(ggplot2)

require(lubridate)

require(forecast)



shampoo <- read.csv("shampoo_sales.csv")

ggplot(shampoo) + 
  geom_line(aes(mdy(date), sales, group = 1))+
  xlab("Date")+
  ylab("Shampoo sales")


### create the training set and the test set

shampoo_train <- shampoo[1:33,]

shampoo_test <- shampoo[34:36,]

ntest <- nrow(shampoo_test)

### create the time series for the closing values

sales_train_ts <- ts(shampoo_train$sales)

### build the network

net <- nnetar(sales_train_ts, repeats = 20)

### view some model parameters

net$p

net$P

net$size

### model output

net$fitted

plot(net$fitted)

net$residuals

### make forecasts in the test set

pred_net <- forecast(net, h = ntest)

pred_net$mean

accuracy(pred_net, shampoo_test$sales)

### plot the series
 

shampoo$pred <- c(net$fitted, pred_net$mean)

ggplot(shampoo)+ 
  geom_line(aes(mdy(date), sales, group = 1))+
  geom_line(aes(mdy(date), pred, group = 1), color = "dodgerblue4", size = 1.5)+
  geom_vline(aes(xintercept=mdy("10/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Date")+
  ylab("Shampoo sales")

### zoom into the test data

ggplot(shampoo[34:36,])+ 
  geom_line(aes(mdy(date), sales, group = 1))+
  geom_line(aes(mdy(date), pred, group = 1), color = "dodgerblue4", size = 1.5)+
  xlab("Date")+
  ylab("Shampoo sales")


