

### forecast electricity production in the US
### using a quadratic trend and multiplicative seasonality

require(ggplot2)

require(lubridate)

require(forecast)



elp <- read.csv("Electricity_Production.csv")

ggplot(elp, aes(mdy(DATE), Value, group = 1)) + 
  geom_line()

### zoom in to identify seasons

ggplot(elp[1:36,], aes(mdy(DATE), Value, group = 1)) + 
  geom_line()

### we can notice a 12-month pattern in the chart
### so we will build a model with 12 seasons (months)

### create the training set and the test set

elp_train <- elp[1:373,]

elp_test <- elp[374:397,]

ntest <- nrow(elp_test)

### create the seasonal time series

prod_train_ts <- ts(elp_train$Value, freq = 12)

### build the model

model_season <- tslm(prod_train_ts ~ trend + I(trend^2) + season, lambda = 0)

### the option lambda = 0 applies a logarithmic transformation
### to the time series data and makes the seasonal component multiplicative

### (the trend remains quadratic)

### (when we make forecasts, the data is converted 
### back to the initial values)

### see the model

model_season

summary(model_season)

### make the forecast in the test set

pred_season <- forecast(model_season, h = ntest, level = 0)

### compute the accuracy

accuracy(pred_season, elp_test$Value)

### plot the series

elp_train$pred_train <- model_season$fitted.values

elp_test$pred_test <- pred_season$mean

ggplot()+
  geom_line(data = elp, aes(mdy(DATE), Value, group = 1))+
  geom_line(data = elp_train, aes(mdy(DATE), pred_train),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = elp_test, aes(mdy(DATE), pred_test),
            color = "dodgerblue4", size = 1.2)+
  geom_vline(aes(xintercept=mdy("01/01/2016")), color = "darkorange2", size = 0.9)+
  xlab("Year")+
  ylab("Production")

### zoom into the chart to see the test period

ggplot()+
  geom_line(data = elp[374:397,], aes(mdy(DATE), Value, group = 1))+
  geom_line(data = elp_test, aes(mdy(DATE), pred_test),
            color = "dodgerblue4", size = 1.2)+
  xlab("Year")+
  ylab("Production")

