### computing and plotting the centered moving averages

require(zoo)

require(ggplot2)

prod <- read.csv("Forecasting/course_datasets/weekly_product_sales.csv")

prod


ggplot(prod, aes(week, sales, group = 1)) +
  geom_line()

### compute centered moving averages
### with the following window sizes: 5, 15 and 25
### using the rollmean() function (zoo package)

### w = 5

prod$ma5 <- rollmean(prod$sales, 5, align = "center", fill = NA)

### the fill option is used to provide filling values
### at the left of the series (where we don't have averages)

### plot the smoothed series

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(aes(week, ma5, group = 1), color = "dodgerblue4", size = 1.5)

### w = 15

prod$ma15 <- rollmean(prod$sales, 15, align = "center", fill = NA)

### plot the smoothed series

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(aes(week, ma15, group = 1), color = "dodgerblue4", size = 1.5)


### w = 25

prod$ma25 <- rollmean(prod$sales, 25, align = "center", fill = NA)

### plot the smoothed series

ggplot(prod) +
  geom_line(aes(week, sales, group = 1)) +
  geom_line(aes(week, ma25, group = 1), color = "dodgerblue4", size = 1.5)

########################################

### compute the centered moving averages
### for the Microsoft stock price evolution

ms <- read.csv("Microsoft_Stock.csv")

ggplot(ms) +
  geom_line(aes(Instant, Close, group = 1)) +
  xlab("Instant") +
  ylab("Stock price")

### compute centered moving averages
### with a 5-period window

ms$ma5 <- rollmean(ms$Close, 5, align = "center", fill = NA)

### plot the smoothed series

ggplot(ms) +
  geom_line(aes(Instant, Close, group = 1)) +
  geom_line(aes(Instant, ma5, group = 1), color = "dodgerblue4", size = 1.5)

### zoom into the series (last 100 days)

ggplot(ms[1412:1511, ]) +
  geom_line(aes(Instant, Close, group = 1)) +
  geom_line(aes(Instant, ma5, group = 1), color = "dodgerblue4", size = 1.5)
