### forecast shampoo sales
### with the regression technique
### using a quadratic trend

require(ggplot2)

require(lubridate)

require(forecast)


shampoo <- read.csv("shampoo_sales.csv")

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

model_q <- tslm(sales_train_ts ~ trend + I(trend^2))

### view the model

model_q

summary(model_q)

### make forecasts in the test set

pred_q <- forecast(model_q, h = ntest, level = 0)

pred_q$mean

### compute accuracy

accuracy(pred_q, shampoo_test$sales)

### create the prediction variables

shampoo_train$pred_q_train <- model_q$fitted.values

shampoo_test$pred_q_test <- pred_q$mean


### plot the series

ggplot() +
  geom_line(data = shampoo, aes(mdy(date), sales, group = 1)) +
  geom_line(
    data = shampoo_train,
    aes(mdy(date), pred_q_train),
    color = "dodgerblue4",
    size = 1.2
  ) +
  geom_line(
    data = shampoo_test,
    aes(mdy(date), pred_q_test),
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


### predict the future (six months forward)

pred_q_f <- forecast(model_q, h = ntest + 6, level = 0)

tail(pred_q_f$mean, 6)


################################################

### forecast shampoo sales
### with the regression technique
### using a cubic trend

ggplot(shampoo, aes(mdy(date), sales, group = 1)) +
  geom_line() +
  xlab("Date") +
  ylab("Shampoo sales")

### build the model

model_c <- tslm(sales_train_ts ~ trend + I(trend^2) + I(trend^3))

### view the model summary

summary(model_c)

### make forecasts in the test set

pred_c <- forecast(model_c, h = ntest, level = 0)

pred_c$mean

### compute accuracy

accuracy(pred_c, shampoo_test$sales)

### compute the forecast variables

shampoo_train$pred_c_train <- model_c$fitted.values

shampoo_test$pred_c_test <- pred_c$mean


### plot

ggplot() +
  geom_line(data = shampoo, aes(mdy(date), sales, group = 1)) +
  geom_line(
    data = shampoo_train,
    aes(mdy(date), pred_c_train),
    color = "blue",
    size = 1.2
  ) +
  geom_line(
    data = shampoo_test,
    aes(mdy(date), pred_c_test),
    color = "blue",
    size = 1.2,
    linetype = "dotted"
  ) +
  geom_vline(aes(xintercept = mdy("7/1/2017")), color = "orange", size = 0.9) +
  xlab("Year") +
  ylab("Shampoo sales")


### predict the future

pred_c_f <- forecast(model_c, h = ntest + 6, level = 0)

tail(pred_c_f$mean, 6)


input <- data.frame(
  Date = c(
    "1-Jan",
    "2-Jan",
    "3-Jan",
    "4-Jan",
    "5-Jan",
    "6-Jan",
    "7-Jan",
    "8-Jan",
    "9-Jan"
  ),
  StockBal = c(12, 6, 0, 0, 0, 15, 0, 2, 0)
)


zero_runs <- rle(input$StockBal == 0)

zero_runs

ends <- cumsum(zero_runs$lengths)

ends


starts <- c(1, head(ends, -1) + 1)
result <- tibble(
  `Stock Out Days` = as.Date(input$Date[starts[zero_runs$values]]),
  `Days Out` = zero_runs$lengths[zero_runs$values]
)

c(head(ends, -1) + 1)

?rle
