
### forecast monthly alcohol sales
### using the Holt-Winters method

require(ggplot2)

require(lubridate)

require(forecast)

alcohol <- read.csv("Alcohol_Sales.csv")

ggplot(alcohol, aes(mdy(DATE), sales, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Alcohol sales")

### define the training set and the test set

alcohol_train <- alcohol[1:301,]

alcohol_test <- alcohol[302:325,]

ntest <- nrow(alcohol_test)

### zoom into the data to identify seasons

ggplot()+
  geom_line(data = alcohol[1:60,], aes(mdy(DATE), sales, group = 1))

### on the chart we notice that the sales pattern repeats
### over an year period

### since one year has 12 months, we'll use 12 as the number of seasons

### create the seasonal time series for the training set

sales_train_ts <- ts(alcohol_train$sales, freq = 12)

### we can decompose the series to see the seasonal pattern again

decomp <- decompose(sales_train_ts)

plot(decomp)

### build the Holt-Winters model
### using the following settings:
### multiplicative error, additive trend and multiplicative seasonality

model_hw <- ets(sales_train_ts, model = "MAM", alpha = 0.2)

### see the fitted values

model_hw$fitted

### the fitted values are grouped by months
### we have 301 months in total, that is 25 years and one month

### make the forecast in the test set

pred <- forecast(model_hw, h = ntest, level = 0)

### print the forecasts

pred$mean

### create a vector of predictions

pred_hw <- c(model_hw$fitted, pred$mean)

### add the vector as a variable to the initial data set

alcohol$pred_hw <- pred_hw


### plot the series

ggplot(alcohol) + 
  geom_line(aes(mdy(DATE), sales, group = 1))+
  geom_line(aes(mdy(DATE), pred_hw, group = 1), color = "dodgerblue4", size = 1.5)+
  geom_vline(aes(xintercept=mdy("1/1/2017")), color = "darkorange2", size = 0.9)+
  xlab("Year")+
  ylab("Alcohol sales")


### compute the prediction accuracy

accuracy(pred, alcohol_test$sales)

#########################################

### forecast the future alcohol sales (twelve months)

pred_f <- forecast(model_hw, h = ntest+12, level = 0)

tail(pred_f$mean, 12)

plot(tail(pred_f$mean, 12))

