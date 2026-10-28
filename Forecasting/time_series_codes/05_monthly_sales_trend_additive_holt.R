### forecast monthly shampoo sales
### using the Holt model (additive trend)

require(ggplot2)

require(forecast)

require(lubridate)

list.files()

shampoo <- read.csv("Forecasting/course_datasets/shampoo_sales.csv")

ggplot(shampoo, aes(mdy(date), sales, group = 1)) +
  geom_line() +
  xlab("Year") +
  ylab("Shampoo sales")

### create the training set and the test set

shampoo_train <- shampoo[1:30, ]

shampoo_test <- shampoo[31:36, ]

ntest <- nrow(shampoo_test)
ntest
### we are going to assume that
### errors are multiplicative (they increase in time)

### we also assume an additive trend with no seasonality

### so our model will be MAN

### create the model

model_man <- ets(shampoo_train$sales, model = "MAN", alpha = 0.2)

### print the forecast values in the training set

model_man$fitted
pred_man$mean
### make the forecast for the test set

pred_man <- forecast(model_man, h = ntest, level = 0)

### print the forecast values in the test set

pred_man$fitted

### create a variable with all the forecast values

pred_man_all <- c(model_man$fitted, pred_man$mean)

shampoo$pred_man_all <- pred_man_all
shampoo |> View()

### plot the series

ggplot(shampoo) +
  geom_line(aes(mdy(date), sales, group = 1)) +
  geom_line(
    aes(mdy(date), pred_man_all, group = 1),
    color = "dodgerblue4",
    size = 1.5
  ) +
  geom_vline(
    aes(xintercept = mdy("8/1/2017")),
    color = "darkorange2",
    size = 0.9
  )

pred_man

### compute accuracy

accuracy(pred_man, shampoo_test$sales)

### the forecast is good in the training set
### but highly unsatisfactory in the test set

####################################

### try a higher value of alpha
### (to make the prediction series less smoothed
### and hopefully improve the forecast)

model_man <- ets(shampoo_train$sales, model = "MAN", alpha = 0.9)

### make the forecast for the test set

pred_man <- forecast(model_man, h = ntest, level = 0)

pred_man

### create a variable with all the forecast values

pred_man_all <- c(model_man$fitted, pred_man$mean)

shampoo$pred_man_all <- pred_man_all


### plot the series

ggplot(shampoo) +
  geom_line(aes(mdy(date), sales, group = 1)) +
  geom_line(
    aes(mdy(date), pred_man_all, group = 1),
    color = "dodgerblue4",
    size = 1.5
  ) +
  geom_vline(
    aes(xintercept = mdy("7/1/2017")),
    color = "darkorange2",
    size = 0.9
  )


### compute accuracy

accuracy(pred_man, shampoo_test$sales)

### the accuracy indicators in the test set improved

### to improve it even more, we may consider
### a multiplicative trend

#########################################

### forecast future sales values (5 months)

pred_man_f <- forecast(model_man, h = ntest + 5, level = 0)

tail(pred_man_f$mean, 5)


################################################

##########################################

### get a confidence level of the forecast
### and plot the prediction confidence interval

### confidence level: 95%

model_man <- ets(shampoo_train$sales, model = "MAN", alpha = 0.9)

pred <- forecast(model_man, h = ntest, level = c(0.95))

pred

plot(pred)
