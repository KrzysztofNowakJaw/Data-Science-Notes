### forecast shampoo sales
### with the regression technique
### using a linear trend
getwd()
require(ggplot2)

require(lubridate)

require(forecast)

path <- "/Users/krzysztofnowak/Data-Science-Notes/Forecasting/course_datasets/shampoo_sales.csv"

shampoo <- read.csv(path)

ggplot(shampoo, aes(mdy(date), sales, group = 1)) +
  geom_line() +
  xlab("Date") +
  ylab("Shampoo sales")

### create the training set and the test set

shampoo_train <- shampoo[1:30, ]

shampoo_test <- shampoo[31:36, ]

ntest <- nrow(shampoo_test)

### create the time series

sales_train_ts <- ts(shampoo_train$sales)

### build the model
### using the tslm() function

shampoo_test

model <- tslm(sales_train_ts ~ trend)

### view the model

model

summary(model)

### view the fitted values in the training set

model$fitted.values

### make forecasts in the test set

pred <- forecast(model, h = ntest, level = 0)

pred$mean

### compute accuracy

accuracy(pred, shampoo_test$sales)

### create a new variable in the training set
### containing the fitted values

shampoo_train$pred_train <- model$fitted.values

### create a new variable in the test set
### containing the forecasts

shampoo_test$pred_test <- pred$mean


### plot the time series

ggplot() +
  geom_line(data = shampoo, aes(mdy(date), sales, group = 1)) +
  geom_line(
    data = shampoo_train,
    aes(mdy(date), pred_train),
    color = "dodgerblue4",
    size = 1.2
  ) +
  geom_line(
    data = shampoo_test,
    aes(mdy(date), pred_test),
    color = "dodgerblue4",
    size = 1.2
  ) +
  geom_vline(
    aes(xintercept = mdy("7/1/2017")),
    color = "darkorange2",
    size = 0.9
  ) +
  xlab("Year") +
  ylab("Shampoo sales")


### predict the future (six-step forward)

pred_f <- forecast(model, h = ntest + 6, level = 0)

tail(pred_f$mean, 6)
