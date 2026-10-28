### forecast weekly product sales using
### the moving average technique

### the trailing moving average will be employed

require(zoo)

require(ggplot2)

getwd()

prod <- read.csv("Forecasting/course_datasets/weekly_product_sales.csv")

ggplot(prod, aes(week, sales, group = 1)) +
  geom_line()

### create the training set and the test set

prod_train <- prod[1:80, ]

prod_test <- prod[81:100, ]

### compute the trailing moving average in the training set
### with the following window sizes: 3, 5 and 10
### using the rollmean() function (zoo package)

### then we'll make forecasts in the test set

ma3_train <- rollmean(prod_train$sales, 3, align = "right", fill = NA)

ma5_train <- rollmean(prod_train$sales, 5, align = "right", fill = NA)

ma10_train <- rollmean(prod_train$sales, 10, align = "right", fill = NA)

### the forecasts in the test set are equal to the last average

### build the vectors of forecasted values

ma3_test <- rep(tail(ma3_train, 1), 20)

ma3_test

ma5_test <- rep(tail(ma5_train, 1), 20)

ma5_test

ma10_test <- rep(tail(ma10_train, 1), 20)

ma10_test

### create variables with the estimated sales
### in the original data set
### (containing estimates in both training and test set)

prod$ma3 <- c(ma3_train, ma3_test)

prod$ma5 <- c(ma5_train, ma5_test)

prod$ma10 <- c(ma10_train, ma10_test)

### plot all the smoothed series on the same chart

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(aes(week, ma3, group = 1), color = "dodgerblue4", size = 1.5) +
  geom_line(aes(week, ma5, group = 1), color = "darkorange2", size = 1.5) +
  geom_line(aes(week, ma10, group = 1), color = "brown4", size = 1.5)


#####################################################

### create custom functions to compute forecast accuracy

### MAE (MAD)

compute_mae <- function(obs, pred) {
  absdiff <- sum(abs(obs - pred))

  mae <- absdiff / length(pred)

  return(mae)
}

prod |> head()

### compute MAE in the test set

compute_mae(prod_test$sales, ma3_test)

compute_mae(prod_test$sales, ma5_test)

compute_mae(prod_test$sales, ma10_test)


### average error

compute_avgerr <- function(obs, pred) {
  diff <- sum(obs - pred)

  avgerr <- diff / length(pred)

  return(avgerr)
}

### compute the average error in the test set

compute_avgerr(prod_test$sales, ma3_test)

compute_avgerr(prod_test$sales, ma5_test)

compute_avgerr(prod_test$sales, ma10_test)


### RMSE

compute_rmse <- function(obs, pred) {
  sqdiff <- sum((obs - pred)^2)

  rmse <- (sqdiff / length(pred))^0.5

  return(rmse)
}

### compute the RMSE in the test set

compute_rmse(prod_test$sales, ma3_test)

compute_rmse(prod_test$sales, ma5_test)

compute_rmse(prod_test$sales, ma10_test)


### MAPE

compute_mape <- function(obs, pred) {
  percs <- sum(abs((obs - pred) / obs)) * 100

  mape <- percs / length(pred)

  return(mape)
}

### compute the MAPE in the test set

compute_mape(prod_test$sales, ma3_test)

compute_mape(prod_test$sales, ma5_test)

compute_mape(prod_test$sales, ma10_test)


### MASE

compute_mase <- function(obs_test, pred_test, obs_train) {
  absdiff_test <- sum(abs(obs_test - pred_test))

  mae_test <- absdiff_test / length(pred_test)

  vect_absdiff_train <- NULL

  for (i in 2:length(obs_train)) {
    vect_absdiff_train <- c(
      vect_absdiff_train,
      abs(obs_train[i] - obs_train[i - 1])
    )
  }

  avg_nf_train <- sum(vect_absdiff_train) / (length(obs_train) - 1)

  mase <- mae_test / avg_nf_train

  return(mase)
}


### compute the MASE in the test set

compute_mase(prod_test$sales, ma3_test, prod_train$sales)

compute_mase(prod_test$sales, ma5_test, prod_train$sales)

compute_mase(prod_test$sales, ma10_test, prod_train$sales)
