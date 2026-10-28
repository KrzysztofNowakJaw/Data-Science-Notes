### forecast weekly product sales using
### the weighted moving average technique

install.packages('TTR')

require(zoo)

require(ggplot2)

require(TTR)

prod <- read.csv("weekly_product_sales.csv")

ggplot(prod, aes(week, sales, group = 1)) +
  geom_line()

### create the training set and the test set

prod_train <- prod[1:80, ]

prod_test <- prod[81:100, ]

### compute the weighted moving average with the
### WMA() function (TTR package)
### window size = 5

wma5_train <- WMA(prod_train$sales, n = 5, wts = 1:5)

### the newest value has the highest weight (5)

### the forecasts in the test set are equal to the last average

wma5_test <- rep(tail(wma5_train, 1), 20)

wma5_test

### create a variable with the estimated sales
### in the original data set
### (containing estimates in both training and test set)

prod$wma5 <- c(wma5_train, wma5_test)

### plot the smoothed series

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(aes(week, wma5, group = 1), color = "dodgerblue4", size = 1.5)

### compute the forecast accuracy metrics in the test set
### using the functions created in the previous lecture

compute_mae(prod_test$sales, wma5_test)

compute_avgerr(prod_test$sales, wma5_test)

compute_rmse(prod_test$sales, wma5_test)

compute_mape(prod_test$sales, wma5_test)

compute_mase(prod_test$sales, wma5_test, prod_train$sales)
