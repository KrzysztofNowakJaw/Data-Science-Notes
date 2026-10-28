### forecast weekly sales with the
### simple exponential smoothing

require(ggplot2)

require(forecast)

prod <- read.csv("weekly_product_sales.csv")

ggplot(prod, aes(week, sales, group = 1)) +
  geom_line()


### create the training set and the test set

prod_train <- prod[1:80, ]

prod_test <- prod[81:100, ]

ntest <- nrow(prod_test)

### build the simple exponential smoothing model

### it will be a model with additive error (A)
### without trend or seasonality

### we will use the default alpha value for starters

model <- ets(prod_train$sales, model = "ANN", alpha = 0.2)

### see the forecast values

model$fitted |> length()

### make the forecast in the test set

### we set the confidence level to zero (point forecast)

pred <- forecast(model, h = ntest, level = 0)

pred$mean

### make a vector of forecast values
### and add it as a variable in our data set

pred_ses <- c(model$fitted, pred$mean)

prod$pred_ses <- pred_ses

### plot the forecast values

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(aes(week, pred_ses, group = 1), color = "dodgerblue4", size = 1.5) +
  geom_vline(aes(xintercept = 81), color = "darkorange2", size = 0.9)


### compute the forecast accuracy metrics
### in the test set

accuracy(pred, prod_test$sales)

?accuracy

###############################################

### find a better value for alpha

### try with a smaller value (0.01)

model2 <- ets(prod_train$sales, model = "ANN", alpha = 0.01)

### make forecasts

pred2 <- forecast(model2, h = ntest, level = 0)

### see the forecast values

pred2$mean

### make a vector of forecast values
### and add it as a variable in our data set

pred_ses2 <- c(model2$fitted, pred2$mean)

prod$pred_ses2 <- pred_ses2

### plot the series

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(
    aes(week, pred_ses2, group = 1),
    color = "dodgerblue4",
    size = 1.5
  ) +
  geom_vline(aes(xintercept = 81), color = "darkorange2", size = 0.9)


### prediction accuracy

accuracy(pred2, prod_test$sales)

#########

### try a greater alpha (0.7)

model3 <- ets(prod_train$sales, model = "ANN", alpha = 0.7)

### make forecasts

pred3 <- forecast(model3, h = ntest, level = 0)

### see the forecast values

pred3$mean

### make a vector of forecast values
### and add it as a variable in our data set

pred_ses3 <- c(model3$fitted, pred3$mean)

prod$pred_ses3 <- pred_ses3

### plot the series

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(
    aes(week, pred_ses3, group = 1),
    color = "dodgerblue4",
    size = 1.5
  ) +
  geom_vline(aes(xintercept = 81), color = "darkorange2", size = 0.9)


### prediction accuracy

accuracy(pred3, prod_test$sales)


##########################################

### get an confidence level of the forecast
### and plot the forecast confidence interval

### confidence level: 95%

model <- ets(prod_train$sales, model = "ANN", alpha = 0.01)

pred <- forecast(model, h = ntest, level = c(0.95))

pred

plot(pred)
