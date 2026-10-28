

### forecast shampoo sales
### with the regression technique
### using an exponential trend

require(ggplot2)

require(lubridate)

require(forecast)


shampoo <- read.csv("shampoo_sales.csv")

ggplot(shampoo, aes(mdy(date), sales, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Shampoo sales")

### create the training set and the test set

shampoo_train <- shampoo[1:30,]

shampoo_test <- shampoo[31:36,]

ntest <- nrow(shampoo_test)

### create the time series

sales_train_ts <- ts(shampoo_train$sales)

### build the model

model_exp <- tslm(sales_train_ts ~ trend, lambda = 0)

### the option lambda = 0 applies a logarithmic transformation
### to the time series data, making the trend exponential

### (when we make forecasts, data will be converted back to the original scale)

### view the model

model_exp

summary(model_exp)

model_exp$fitted.values

### make forecasts in the test set

pred_exp <- forecast(model_exp, h = ntest, level = 0)

pred_exp$mean

### accuracy

accuracy(pred_exp, shampoo_test$sales)

### create the forecast variables

shampoo_train$pred_exp_train <- model_exp$fitted.values

shampoo_test$pred_exp_test <- pred_exp$mean


### plot the series

ggplot()+
  geom_line(data = shampoo, aes(mdy(date), sales, group = 1))+
  geom_line(data = shampoo_train, aes(mdy(date), pred_exp_train),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = shampoo_test, aes(mdy(date), pred_exp_test),
            color = "dodgerblue4", size = 1.2)+
  geom_vline(aes(xintercept=mdy("7/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Year")+
  ylab("Shampoo sales")


### predict the future

pred_exp_f <- forecast(model_exp, h = ntest+6, level = 0)

tail(pred_exp_f$mean, 6)

