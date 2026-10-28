
### forecast Microsoft stock values
### using a multiplicative trend
### (Holt model)

require(ggplot2)

require(forecast)

require(lubridate)



ms <- read.csv("Microsoft_Stock.csv")

ggplot(ms, aes(Instant, Close, group = 1)) + 
  geom_line()+
  xlab("Time")+
  ylab("MS Stock")

### create the training and test set

ms_train <- ms[1:1496,]

ms_test <- ms[1497:1511,]

ntest <- nrow(ms_test)

### we are going to assume that
### errors are multiplicative (they increase over time)

### we also assume a multiplicative trend with no seasonality

### so our model will be MMN

model_mmn <- ets(ms_train$Close, model = "MMN", alpha = 0.9)

model_mmn$fitted

### make the forecast in the test set

pred_mmn <- forecast(model_mmn, h = ntest, level = 0)

pred_mmn$mean

### create a variable containing the forecast values

pred_mmn_all <- c(model_mmn$fitted, pred_mmn$mean)

ms$pred_mmn_all <- pred_mmn_all


### plot the series

ggplot(ms)+ 
  geom_line(aes(Instant, Close, group = 1))+
  geom_line(aes(Instant, pred_mmn_all, group = 1), color = "dodgerblue4", size = 1.5)+
  geom_vline(aes(xintercept=1497), color = "darkorange2", size = 0.9)

### zoom into the series (see the test set only)

ggplot(ms[1497:1511,])+ 
  geom_line(aes(Instant, Close, group = 1))+
  geom_line(aes(Instant, pred_mmn_all, group = 1), color = "dodgerblue4", size = 1.5)

### compute accuracy

accuracy(pred_mmn, ms_test$Close)

###################################################

### predict the future (5 days)

pred_mmn_f <- forecast(model_mmn, h = ntest+5, level = 0)

tail(pred_mmn_f$mean, 5)
