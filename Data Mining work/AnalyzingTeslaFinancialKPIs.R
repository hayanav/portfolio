library(tidyverse)
dt0 <- read_csv("finStatements4All_FY2009-20.csv")
names(dt0)

# The ticker symbol for Tesla is TSLA
dt0 %>% filter(tic=="TSLA") %>% select(fyear, tic, conm, naics)
dt1 <- dt0 %>% 
  filter(naics==336111 & fyear>2009 & fyear < 2020) %>% 
  select(fyear, tic, conm, sale, at, oibdp, naics) %>% 
  mutate(ROA = oibdp/at, PM = oibdp/sale, TATO=sale/at)
dt1 %>% select(sale, at, oibdp,ROA, PM, TATO) %>% summary()
dt2 <- dt1 %>% filter(sale>=100)
dt2 %>% select(sale, at, oibdp,ROA, PM, TATO) %>% summary()
dt3_indROA <- dt2 %>% 
  group_by(fyear) %>% 
  summarise(ind_minROA = min(ROA),
            ind_q1ROA  = quantile(ROA, 0.25),
            ind_medROA = median(ROA),
            ind_q3ROA  = quantile(ROA, 0.75),
            ind_maxROA = max(ROA))
dt3_indROA
dt3_indPM <- dt2  %>% group_by(fyear) %>% 
  summarise(ind_minPM = min(PM),
            ind_q1PM  = quantile(PM, 0.25),ind_medPM = median(PM),
            ind_q3PM  = quantile(PM, 0.75),ind_maxPM = max(PM))
dt3_indPM
dt3_indTATO <- dt2 %>% group_by(fyear) %>% 
  summarise(ind_minTATO = min(TATO),
            ind_q1TATO  = quantile(TATO, 0.25),ind_medTATO = median(TATO), 
            ind_q3TATO  = quantile(TATO, 0.75), ind_maxTATO = max(TATO))
dt3_indTATO
dt4Tesla <- dt1 %>% filter(tic=="TSLA") %>% 
  select(fyear, tic, ROA, PM, TATO)
dt4Tesla  
dt5 <- bind_cols(
  dt3_indROA %>% select(fyear, ind_q1ROA, ind_q3ROA),
  dt3_indPM %>% select(ind_q1PM, ind_q3PM),
  dt3_indTATO %>% select(ind_q1TATO, ind_q3TATO),
  dt4Tesla %>% select(tic, ROA, PM, TATO))
dt5
