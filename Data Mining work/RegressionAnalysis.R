library(tidyverse)
library(readr)

dt <- read_csv("AICPA_regressionAnalysisData (1).csv")
dt %>% glimpse()

dt %>% head(2)
dt %>% tail(2)

dt %>% is.na() %>% colSums()

#splitting data into training (70-80%) vs testing(30-20%)
dt %>%
  count(type)

#creating new dataset
aicpaTrain <- dt %>%
  filter(type == "dt4training")
aicpaTrain %>% head(2)

aicpaTrain %>% tail(2)

aicpaTrain %>% 
  select(-type) %>%
  summary()

#testing data

aicpaTest <- dt %>% 
  filter(type == "dt4testing")

aicpaTest %>%
  select(-type) %>%
  summary()

#visualizations
dt %>%
  ggplot(aes(x=date, y=revenue))+
  geom_line() + geom_point()

aicpaTrain <- aicpaTrain %>%
mutate(byUse =
ifelse(heatDD > coolDD, "heating", "cooling"))

aicpaTrain %>%
  ggplot(aes(x = date, y = revenue)) +
  geom_point(aes(color = byUse)) +
  geom_line()

aicpaTrain %>% 
  ggplot(aes(x=byUse, y=revenue)) +
  geom_boxplot(aes(fill = byUse))

aicpaTrain %>%
  ggplot(aes(x = production, y = revenue)) +
  geom_point(aes(color = byUse))

aicpaTrain %>%
  select(-type, -date, -byUse) %>%
  cor()

options(scipen = 99)
lm1 <- lm(revenue~production, aicpaTrain)
summary(lm1)

aicpaTest %>% head(1)

pred_lm1 <- predict(lm1, newdata = aicpaTest)
pred_lm1

dt_err_lm1 <- aicpaTest %>%
  select(date, production, revenue) %>%
  mutate(pred_lm1) %>%
  mutate(pctErr = 100 * (revenue - pred_lm1)/revenue)
dt_err_lm1

mape_lm1 <- dt_err_lm1 %>%
  mutate(absErr = abs(pctErr)) %>%
  pull(absErr) %>%
  mean()
mape_lm1

dt_err_lm1 %>%
  select(date, revenue, pred_lm1) %>%
  gather("variable", "value", -date) %>%
  ggplot(aes(x=date))+
  geom_point(aes(y=value, color = variable))+
  geom_line(aes(y=value, color = variable))+
  xlab("Time")+ylab("Revenue")
  
xLm1 <- lm(revenue~production+heatDD, aicpaTrain)
summary(xLm1)

#Call:
  lm(formula = revenue ~ production + heatDD, data = aicpaTrain)


pred_xLm1 <- predict(xLm1, newdata = aicpaTest)
dt_err_xLm1 <- aicpaTest %>%
  select(date, revenue) %>%
  mutate(pred_xLm1) %>%
  mutate(pctErr = 100 * (revenue - pred_xLm1)/revenue)
dt_err_xLm1


mape_xLm1 <- dt_err_xLm1 %>%
  mutate(absErr = abs(pctErr)) %>%
  pull(absErr) %>%
  mean()
mape_xLm1


dt_err_xLm1 %>%
  select(date, revenue, pred_xLm1) %>%
  gather("variable", "value", -date) %>%
  ggplot(aes(x=date))+
  geom_point(aes(y=value, color = variable))+
  geom_line(aes(y=value, color = variable))+
  scale_x_date(date_breaks= '2 month' ) +
  xlab("Time")+ylab("Revenue")

  
library(scales)
dt %>%
  ggplot(aes(x=date, y=revenue))+
  geom_line() +
  geom_point() +
  scale_x_date(date_breaks= '1 year',
    labels = date_format(format =  '%b-%Y')) +
    scale_y_continuous(labels = unit_format(scale = 1e-6,
                                            unit =  MM ),
                       breaks = extended_breaks(n = 7),
                       limits = c(0, 30*1e6)) +
xlab("Time") + ylab("Revenue")


  
  
  
