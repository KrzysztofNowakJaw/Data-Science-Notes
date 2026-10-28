

### forecasting series with multiple seasonality
### using the TBATS technique

require(ggplot2)

require(lubridate)

require(forecast)


bike <- read.csv("daily_bike_rental.csv")

ggplot(bike, aes(ymd(dteday), cnt, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Bike rentals")

### in this chart we notice an yearly seasonality pattern

### so the first seasonal period is equal to 365 days

### zoom in to detect other possible seasonality

ggplot(bike[1:28,], aes(ymd(dteday), cnt, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Bike rentals")

### from this chart we can detect a weekly seasonality pattern

### therefore, the second seasonal period is equal to 7 days

### we will build the model using the whole data
### and make a forecast for the future (next 365 days)

### create a multi-seasonal time series
### with the msts() function

cnt_ms <- msts(bike$cnt, seasonal.periods = c(7, 365))

### we define two seasonal periods:
### weekly (7 days) and yearly (365 days)

### build the model

model_tbats <- tbats(cnt_ms)

### View the fitted (estimated) values

model_tbats$fitted.values

### compute the accuracy

accuracy(model_tbats$fitted.values, model_tbats$y)

### to get the MASE we use the compute_mase() function

compute_mase(model_tbats$y, model_tbats$fitted.values, model_tbats$y)

### plot the series

### create a new variable with the fitted values

bike$pred <- model_tbats$fitted.values

### draw the chart

ggplot()+
  geom_line(data = bike, aes(ymd(dteday), cnt, group = 1))+
  geom_line(data = bike, aes(ymd(dteday), pred),
            color = "dodgerblue4", size = 1.2)+
  xlab("Date")+
  ylab("Bike rentals")

#############################################

### make the forecast for the next year (2013)

pred_tbats <- forecast(model_tbats, h = 365, level = 0)

pred_tbats$mean

### add forecasts to the plot

### for that, we create a separate data set with the forecasts

### create a sequence of dates in the future
### from January 1st to December 31, 2013

fdates <- seq(ymd("2013-01-01"), ymd("2013-12-31"), 1)

fdates

### create a new data set with the forecasts

future <- as.data.frame(cbind(date = fdates, pred_f = pred_tbats$mean))

View(future)

### change the type of the date column

future$date <- as_date(future$date)

### draw the chart with the forecast

ggplot()+
  geom_line(data = bike, aes(ymd(dteday), cnt, group = 1))+
  geom_line(data = bike, aes(ymd(dteday), pred),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = future, aes(date, pred_f),
            color = "darkorange2", size = 1.2)+
  xlab("Date")+
  ylab("Bike rentals")


