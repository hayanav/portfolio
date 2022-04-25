#set working directory to source file location
library(tidyverse)
library(yardstick)
library(lubridate)

dt0 <- read_csv("qSales_4CU_2021.csv")
dt0 %>% glimpse()

dt1 <- dt0 %>% filter(tic=="AAPL") %>% 
  mutate(datadate = ymd(datadate)) %>% 
  arrange(datadate) %>%
  mutate(time = row_number())

dt0
dt1

dt1 %>% names()
dt1 <- dt1 %>% select(tic, datadate, fyearq, fqtr, saleq, time)
dt1 %>% slice_head(n=3)
dt1 %>% slice_tail(n=3)

#remove 2021
dt1 <- dt1[-nrow(dt1),]

iDates <- 
  as.Date(c("2007-09-30", "2008-09-30", "2009-09-30",
            "2010-09-30", "2011-12-31", "2012-12-31", 
            "2013-12-31", "2014-12-31", "2015-12-31", 
            "2016-12-31", "2017-12-31", "2018-12-31",
            "2019-12-31" ,
            "2020-12-31")) # THQ - 2020 iPhone release

dt1 <- dt1 %>%
  mutate(
    iPhoneDV = ifelse( datadate %in% iDates, 1, 0 ), 
    #THQ-iPhone DV
    q1=ifelse(fqtr==1,1,0), q2=ifelse(fqtr==2,1,0),
    q3=ifelse(fqtr==3,1,0))
dt1 %>% slice_tail(n=5)

dt1 <- dt1 %>%
  mutate(trainTest = 
           ifelse(time <= nrow(dt1)-4, 
                  "dt4Train", "dt4Test"))
dt1 %>% slice_head(n=4)
dt1  %>% slice_tail(n=5)
dt4Train <- dt1 %>%  filter(trainTest == "dt4Train")
dt4Test <- dt1 %>%  filter(trainTest == "dt4Test")

#####################################
options(scipen = 999)
model1 <- lm(	
  saleq~time,dt4Train) #THQ complete code for model1
summary(model1)
tidy(model1)
summary(model1)$adj.r.squared
mape(dt4Test, saleq, predict(model1, dt4Test))

predict(model1, newdata =dt4Test,
        interval = 'prediction',level = .85)

dt4Test %>%
  select(datadate, saleq) %>% cbind(
    predict(model1, newdata =dt4Test,
        interval = 'prediction',level = .90)) %>% 
  mutate(test=ifelse(saleq>=lwr & saleq<=upr,0,1))

dt4Train

model2 <- lm(saleq~time+iPhoneDV,dt4Train)
summary(model1)
summary(model1)$adj.r.squared
mape(dt4Test, saleq, predict(model1, dt4Test))
summary(model2)
tidy(model2)
summary(model2)$adj.r.squared
mape(dt4Test, saleq, predict(model2, dt4Test))

model3 <- lm(saleq~time+iPhoneDV+time*iPhoneDV,dt4Train)
summary(model3)
tidy(model3)
summary(model3)$adj.r.squared
mape(dt4Test, saleq, predict(model3, dt4Test))

model4 <- lm(saleq~time+iPhoneDV+q3+time*iPhoneDV,dt4Train)
summary(model4)
tidy(model4)
summary(model4)$adj.r.squared
mape(dt4Test, saleq, predict(model4, dt4Test))