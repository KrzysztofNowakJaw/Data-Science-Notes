

### forecasting series with multiple seasonality
### using the STLM technique

require(ggplot2)

require(lubridate)

require(forecast)

bike <- read.csv("daily_bike_rental.csv")

ggplot(bike, aes(ymd(dteday), cnt, group = 1)) + 
  geom_line()+
  xlab("Date")+
  ylab("Bike rentals")

### in the previous lecture we have detected two seasonality patterns:
### yearly and weekly

### we will build the model using the whole data
### and make a forecast for the future (next 365 days)

### create the multiseasonal time series

cnt_ms <- msts(bike$cnt, seasonal.periods = c(7, 365))

### build the model

model_stlm <- stlm(cnt_ms, method = "ets", s.window = "period")

### view the fitted values

model_stlm$fitted

### compute the accuracy

accuracy(model_stlm$fitted, bike$cnt)

### to get the MASE we use the compute_mase() function

compute_mase(bike$cnt, model_stlm$fitted, bike$cnt)

### plot the series

bike$pred <- model_stlm$fitted

### draw the chart

ggplot()+
  geom_line(data = bike, aes(ymd(dteday), cnt, group = 1))+
  geom_line(data = bike, aes(ymd(dteday), pred),
            color = "dodgerblue4", size = 1.2)+
  xlab("Date")+
  ylab("Bike rentals")

###################################################

### make the forecast for the next year (2013)

pred_stlm <- forecast(model_stlm, h = 365, level = 0)

pred_stlm$mean

### add forecasts to the plot

### we will create a separate data set with the forecasts

### create a series of dates in the future

fdates <- seq(ymd("2013-01-01"), ymd("2013-12-31"), 1)

### create a new data set with the forecasts

future <- as.data.frame(cbind(date = fdates, pred_f = pred_stlm$mean))

View(future)

### convert the "date" column into a date format

future$date <- as_date(future$date)

### draw the chart with the forecasts

ggplot()+
  geom_line(data = bike, aes(ymd(dteday), cnt, group = 1))+
  geom_line(data = bike, aes(ymd(dteday), pred),
            color = "dodgerblue4", size = 1.2)+
  geom_line(data = future, aes(date, pred_f),
            color = "darkorange2", size = 1.2)+
  xlab("Date")+
  ylab("Bike rentals")


