# Set working directory to source file location
# Load libraries
library(tidyverse)
library(lubridate)
library(yardstick)
options(scipen = 123)

# Load data
dt0a <- read_csv("dtBBTR_ThomasCounty_3Brands_sales2016to2020.csv")
dt0a %>% glimpse()
dt0a <- dt0a %>% filter(Brand==6649)

dt0b <- read_csv("dtBBTR_ThomasCounty_weather.csv")
dt0b %>% glimpse()

dt0c <- read_csv("dtBBTR_ThomasCounty_3Brands_purchases2019t02020.csv")
dt0c %>% glimpse()

dt1a <- dt0a %>% 
  left_join(dt0b, by=c("County", "SalesDate"))

dt1b <- dt1a %>% 
  group_by(SalesDate) %>% 
  summarise(
    year=mean(year), month=mean(month), 
    week=mean(week), wkFD=mean(wkFD), 
    BusDaysWeek=mean(BusDaysWeek),
    Q=sum(q), P=mean(price), SqFt=median(SqFt), 
    Plag1=mean(Plag1), 
    Plag7=mean(Plag7), PRCP=mean(PRCP), 
    SNOW=mean(SNOW), SNWD=mean(SNWD), 
    TMAX=mean(TMAX), TMIN=mean(TMIN)) %>% 
  mutate(Qlag=lag(Q)) %>% 
  ungroup()
dt1b %>% glimpse()

dt1c <- c(ymd(
  "2016-01-01", "2016-05-30", "2016-07-04", 
  "2016-09-05", "2016-11-24", "2016-12-25", 
  "2017-01-02", "2017-05-29", "2017-07-04", 
  "2017-09-04", "2017-11-23", "2017-12-25", 
  "2018-01-01", "2018-05-28", "2018-07-04", 
  "2018-09-03", "2018-11-22", "2018-12-25", 
  "2019-01-01", "2019-05-27", "2019-07-04", 
  "2019-09-02", "2019-11-28", "2019-12-25",  
  "2020-01-01", "2020-05-25", "2020-07-03", 
  "2020-09-07", "2020-11-26", "2020-12-25", 
  "2021-01-01", "2021-05-31", "2021-07-05", 
  "2021-09-06", "2021-11-25", "2021-12-24"))
dt1c

# Integrate ... holidays and other dummy variables
dt2a <- dt1b %>% mutate(
  Hday=ifelse(SalesDate %in% dt1c,1,0),
  pUp=ifelse(P>Plag1,1,0),
  pDown=ifelse(P<Plag1,1,0),
  HvyRain=ifelse(PRCP>1,1,0))
dt2a %>% glimpse()
dt2a %>% is.na() %>% colSums()

# transition to weekly data
dt2a %>% names()
dt2b <- dt2a %>% 
  group_by(year, week) %>% 
  summarise(month=mean(month), wkFD=mean(wkFD),
    BusDaysWeek=mean(BusDaysWeek),
    Q=sum(Q), P=mean(P), SqFt=median(SqFt),
    Plag1=mean(Plag1), Plag7=mean(Plag7),
    PRCP=sum(PRCP), SNOW=sum(SNOW), 
    SNWD=sum(SNWD), TMAX=mean(TMAX), 
    TMIN=mean(TMIN), Qlag=sum(Qlag),
    StoreLarge=ifelse(SqFt>=10500,1,0),
    weekHD=max(Hday)) %>% ungroup() %>% 
  mutate(
    covidYR=ifelse(year==2020,1,0),
    weekPHD=lead(weekHD),
    weekBHD=ifelse(weekHD+weekPHD==1,1,0),
    weekAHD=lag(weekHD)) %>% 
  filter(year>2018) %>% na.omit()

dt2b %>% is.na() %>% colSums()
dt2b %>% glimpse()
################################################
dtCalendar <- 
  tibble(SalesDate = seq.Date(
    ymd("2021-01-01"), 
    ymd("2021-12-31"), 1)) %>%
  mutate(
    wkDay=weekdays(SalesDate),
    year=year(SalesDate), 
    month=month(SalesDate),
    week=week(SalesDate),
    HD=ifelse(SalesDate %in% dt1c,1,0),
    covidYR=ifelse(year==2020|2021,1,0)) %>% 
  group_by(year, week) %>% 
  summarise(
    weekFSD=min(SalesDate),
    covidYR=max(covidYR), 
    weekHD=max(HD)) %>% ungroup() %>% 
  mutate(
    weekPHD=lead(weekHD),
    weekBHD=ifelse(weekHD+weekPHD==1,1,0),
    weekAHD=lag(weekHD))
dtCalendar %>% glimpse()

################################################
dt2b %>% names()
dt2b %>% select(6:22,5) %>% cor()

lm1 <- lm(Q~Qlag+P*weekBHD+BusDaysWeek, dt2b)
summary(lm1)

dt2b %>% filter(year==2020, month==8) %>% 
  select(Q, Qlag, P, BusDaysWeek, weekBHD) %>% 
  summary()

# In 2021 Labor Day is 36
dtTest4LbrdDay <- dtCalendar %>% 
  filter(week>=33, week<=36)
dtTest4LbrdDay

# Use dtTest4LbrDay & summary stats from August 2020 ...

priceSensitivity_6649 <- function(lowPrice)
{
  results <- tibble()
for (i in seq(lowPrice,15.99,.5))
  {dtPredict_6649 <- dtTest4LbrdDay %>% 
    mutate(P=ifelse(week==33|week==34, 9.99, i), 
           BusDaysWeek=7,
           Qlag=130.7)
  predSales=sum(predict(lm1, dtPredict_6649))
  P=i
  PurchasePriceOld=7.87
  PurchasePriceNew=6.87
  Revenue=predSales*P
  GProfitOldPP=Revenue-predSales*PurchasePriceOld
  GProfitNewPP=Revenue-predSales*PurchasePriceNew
  results <- 
    bind_rows(results,
      tibble(P, predSales, Revenue, 
             GProfitOldPP, GProfitNewPP))
  }
return(results)  
}

priceSensitivity_6649(6.99) %>% print(n=21)
priceSensitivity_6649(7.99) %>% print(n=21)


