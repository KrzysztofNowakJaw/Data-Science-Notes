
### forecast monthly shampoo sales
### using a multiplicative trend
### (Holt model)

require(ggplot2)

require(forecast)

require(lubridate)



shampoo <- read.csv("shampoo_sales.csv")

ggplot(shampoo, aes(mdy(date), sales, group = 1)) + 
  geom_line()+
  xlab("Year")+
  ylab("Shampoo sales")

### create the training and test set

shampoo_train <- shampoo[1:30,]

shampoo_test <- shampoo[31:36,]

ntest <- nrow(shampoo_test)

### we are going to assume that
### errors are multiplicative (they increase over time)

### we also assume a multiplicative trend with no seasonality

### so our model will be MMN

model_mmn <- ets(shampoo_train$sales, model = "MMN", alpha = 0.9)

model_mmn$fitted

### make the forecast in the test set

pred_mmn <- forecast(model_mmn, h = ntest, level = 0)

pred_mmn$mean

### create a variable containing the forecast values

pred_mmn_all <- c(model_mmn$fitted, pred_mmn$mean)

shampoo$pred_mmn_all <- pred_mmn_all


### plot the series

ggplot(shampoo)+ 
  geom_line(aes(mdy(date), sales, group = 1))+
  geom_line(aes(mdy(date), pred_mmn_all, group = 1), color = "dodgerblue4", size = 1.5)+
  geom_vline(aes(xintercept=mdy("8/1/2017")), color = "darkorange2", size = 0.9)

### compute accuracy

accuracy(pred_mmn, shampoo_test$sales)

######################################

### predict the future (5 months)

pred_mmn_f <- forecast(model_mmn, h = ntest+5, level = 0)

tail(pred_mmn_f$mean, 5)


