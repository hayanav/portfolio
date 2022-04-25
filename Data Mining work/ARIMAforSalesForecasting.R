library(tidyverse)
dt <- read_csv("AAPL_salesQ20062020.csv")
dt %>% glimpse()

#noticee that datadate not formatted right 
#so we use lubridate and use ymd
library(lubridate)
dt <- dt %>%
  mutate(datadate = ymd(datadate))
head(dt, 4)
tail(dt, 4)

#making graph to show quarterly sales over time
# complete missing code:  y = ....
dt %>%
  ggplot(aes(x=datadate, y=saleq)) + 
  geom_line() + 
  scale_x_date(date_breaks = "1 year")
#shows upwards trend and sales spike at YE


#creating time variable for model #1 rep quarterly observations
dt <- dt %>%
  arrange(datadate) %>%
  mutate(t = row_number())
head(dt, 3)
tail(dt, 3)


#making dummy var for model 2, reps Q where new iphone released
iDates <- 
  as.Date(c("2007-09-30", "2008-09-30", "2009-09-30",
            "2010-09-30", "2011-12-31", "2012-12-31", 
            "2013-12-31", "2014-12-31", "2015-12-31", 
            "2016-12-31", "2017-12-31", "2018-12-31",
            "2019-12-31", "2020-03-31"))
#iphonedv- value=1 when Iphone release and 0 for rest
dt <- dt %>%
  mutate(
    iPhoneDV = ifelse(datadate %in% iDates, 1, 0))
tail(dt, 5)

#creating training and validation sets
#last 4 obvs for validation and rest for testing
#nrow creates categorical var that takes val of train and test
dt <- dt %>%
  mutate(trainTest = 
           ifelse(t <= nrow(dt)-4, 
                  "dt4Train", "dt4Test"))
head(dt %>% select(c(2:4,9)), 4)
tail(dt %>% select(c(2:4,9)), 5)

#generate training and validation
dt4Train <- dt %>%  
  filter(trainTest == "dt4Train")
dt4Test <- dt %>%  
  filter(trainTest == "dt4Test")

dt4Train
dt4Test

#create time series data for ARIMA Analysis to predict quarterly sales  
#create training set that has Qsales and iphoneDV
library(xts)
dxts4Train <- dt4Train %>% 
  select(saleq, iPhoneDV) %>% 
  xts(order.by = dt4Train$datadate)

head(dxts4Train,2)
tail(dxts4Train,2)

#creating validation testing set that has Qsales and DV
dxts4Test <- dt4Test %>% 
  select(saleq, iPhoneDV) %>% 
  xts(order.by = dt4Test$datadate)
dxts4Test


#model 1- linear regression of Qsales as Func of time
options(scipen = 999)
#
model1 <- lm(saleq~t, dt4Train)
summary(model1)


#model 1 validation
dt4Test %>%
  select(datadate, saleq) %>%
  mutate(
    predM1=predict(model1, newdata =dt4Test)) %>%
  mutate(absErrM1 = abs(100*(saleq-predM1)/saleq)) %>%
  select(absErrM1) %>%
  summary()

#generating single prediction point for each observation
predict(model1, newdata =dt4Test)




library(yardstick)
mape(dt4Test, saleq, predict(model1, dt4Test))
#
predict(model1,dt4Test)

#create new data set to capture predicted values and L & UL
dtPredict_m1 <- 
  predict(model1, newdata =dt4Test, 
        interval = 'prediction',
        level = .80)
dtPredict_m1


#to see if values fall wi the confidence intervals we merge 2 datasets
dt4Test
dt4Audit_m1 <- 
  cbind(dt4Test[,c(2,6)],dtPredict_m1)
dt4Audit_m1

#graph showing actual val as UL AND LL of 80% confidence interval
dt4Audit_m1 %>% 
  ggplot(aes(x=datadate, y=saleq))+
  geom_point()+
  geom_line(aes(y = lwr), 
            color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), 
              color = "red", linetype = "dashed")
#
#
#
#
# Complete missing code: ifelse(saleq ... , ..., ...)
dt4Audit_m1 %>% mutate(
  test=ifelse(saleq >= lwr & saleq<= upr, 0,1))
#
#
#
# making model 2
model2 <- lm(saleq~t+iPhoneDV, dt4Train)
summary(model2)

dt4Test %>%
  select(datadate, saleq) %>%
  mutate(
    predM2=predict(model2, newdata =dt4Test)) %>%
  mutate(absErrM2 = abs(100*(saleq-predM2)/saleq)) %>%
  select(absErrM2) %>%
  summary()


mape(dt4Test, saleq, predict(model2, dt4Test))

dtPredict_m2 <- predict(model2, newdata =dt4Test, 
        interval = 'prediction',
        level = .80)
dtPredict_m2

dt4Audit_m2 <- 
  cbind(dt4Test[,c(2,6)],dtPredict_m2)
dt4Audit_m2

dt4Audit_m2 %>% 
  ggplot(aes(x=datadate, y=saleq))+
  geom_point()+
  geom_line(aes(y = lwr), 
            color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), 
              color = "red", linetype = "dashed")
#
#
#
#
#
#making model3
library(forecast)
model3 <- auto.arima(dxts4Train$saleq)
summary(model3)
#
#
#
#
#
fmodel3 <- forecast(model3,4, level = .80)
fmodel3
#

library(forecast)
accuracy(fmodel3, dxts4Test$saleq)
#





dt4Audit_m3 <- 
  cbind(dt4Test[,c(2,6)],
        fmodel3)
dt4Audit_m3
#
#
#
#
#
dt4Audit_m3 <- dt4Audit_m3 %>% 
  rename(upr='Hi 80',
         lwr='Lo 80')
#
#
#
#
#
dt4Audit_m3 %>% 
  ggplot(aes(x=datadate, y=saleq))+
  geom_point()+
  geom_line(aes(y = lwr), 
            color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), 
              color = "red", linetype = "dashed")
#
#
#
#
#
#making model4
model4 <- auto.arima(dxts4Train$saleq,
                    xreg = dxts4Train$iPhoneDV)
summary(model4)

fmodel4 <- forecast(model4,
                    xreg=dxts4Test$iPhoneDV,
                    level = .80)
fmodel4

accuracy(fmodel4, dxts4Test$saleq)

dt4Audit_m4 <- 
  cbind(dt4Test[,c(2,6)],
        fmodel4)
dt4Audit_m4
#
#
#
#
#
dt4Audit_m4 <- dt4Audit_m4 %>% 
  rename(upr='Hi 80',
         lwr='Lo 80')
dt4Audit_m4
#
#
#
#
#
dt4Audit_m4 %>% 
  ggplot(aes(x=datadate, y=saleq))+
  geom_point()+
  geom_line(aes(y = lwr), 
            color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), 
              color = "red", linetype = "dashed")
