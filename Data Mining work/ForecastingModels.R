library(tidyverse)
library(lubridate)
library(forecast)
library(xts)

dt <- read_csv('google.csv') 
dt %>% glimpse()

dt %>% head(2)
dt %>% tail(2)

dt1 <- dt %>%
  mutate(datadate = ymd(datadate)) %>% 
  arrange(datadate)

dt1 %>% head(2)

options(scipen=123)
dt1 %>%
  ggplot(aes(x=datadate, y=saleq)) + geom_point() + geom_line()

dtTest <- dt1 %>% arrange(desc(datadate)) %>% top_n(4, datadate)
dtTest

dtTrain <- dt1 %>%
  filter(!(datadate %in% dtTest$datadate)) %>% arrange(desc(datadate))

head(dtTrain, 2)
tail(dtTrain, 2)

#NOTE: the xts data set shown below, the date is not considered a variable. It is just a indicator of the position of the first, second, third, etc., observations. The data set has only one variable (quarterly sales). Hence, the name univariate time-series.
dtxts_Train <- xts(dtTrain$saleq,
                   order.by = dtTrain$datadate)
head(dtxts_Train, 4)

dtxts <- xts(dt1$saleq,
                   order.by = dt1$datadate)
head(dtxts_Train, 4)

dtxts_Test <- xts(dtTest$saleq,
                  order.by = dtTest$datadate)
head(dtxts_Test)

Model1 <- auto.arima(dtxts_Train)
summary(Model1)

forecastedModel1 <- forecast(Model1,4)
forecastedModel1

plot(forecastedModel1, xlab = 'Quarter', ylab = 'Sales')

accuracy(forecastedModel1, dtxts_Test)

#forecasting 2020
dt1xts <- xts(dt1$saleq, order.by = dt1$datadate)
head(dt1xts,2)
tail(dt1xts,2)

Model2 <- auto.arima(dt1xts)
summary(Model2)

forecastedModel2 <- forecast(Model2, 2)
forecastedModel2
plot(forecastedModel2)
