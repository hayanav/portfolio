library(tidyverse)
tSales <- read_csv("tSales.csv")
tSales %>% glimpse()
tSales()

tSales %>% is.na() %>% colSums()

#creating aggregate sales data
#deciding how to organize big data to provide usefule info
#for ADA info abt range of prices charged for each brand/prod
#would be helpful
#also would be useful to know avg price charged for each brand
#so sales q will also be used
#USING R TO AGGREGATE SALES RECORDS/TRANS @ BRAND/PROD LEVEL

#creating sales summaries for units sold and price agg by brand
salesByBrand <- tSales %>%
  group_by(Brand) %>%
  summarise(unitsSold = sum(SalesQuantity),
            avgPrice = mean(SalesPrice),
            minPrice = min(SalesPrice),
            maxPrice = max(SalesPrice),
            rngPrice = max(SalesPrice) - min(SalesPrice)) %>%
  ungroup()

salesByBrand %>% slice_head(n=5)

salesByBrand %>%
  select(-Brand) %>%
  summary()
#existence of 0 prices need to b investigated^^

#Outliers in price range
#looking @ prods w price over $100
salesByBrand %>%
  filter(rngPrice > 100)

#^^we observe a wide price range (esp 2696)
#may indicate a pricing error
#if it's a result of a control deviation, potential to be many pricing errors

#OBTAINING UNDERSTANDING OF INVENTORY COSTS
#need to work w end of period inventory data
library(readr)
tEndInv <- read_csv("tEndInv.csv")

tEndInv <- read_csv("tEndInv.csv ")
tEndInv %>% glimpse()

tEndInv %>% is.na() %>% colSums()

#Creating aggregate inventory data @ product level
#need to group observations by product

endInvByBrand <- tEndInv %>%
  group_by(Brand) %>%
  summarise(endInvCost = sum(onHand*PurchasePrice),
            avgPP = mean(PurchasePrice),
            maxPP = max(PurchasePrice),
            minPP = min(PurchasePrice),
            totalQoH = sum(onHand),
            rangePP = maxPP - minPP) %>%
  ungroup()


endInvByBrand %>% slice_head(n=5)

endInvByBrand %>%
  select(-Brand) %>%
  summary()
#purchase prices are the same meaning price ranges=0
#this might indicate firm paid fixed prices or made only one purchase of each brand 
#also one or more items may have 0 cost which we would analyze

names(salesByBrand)
names(endInvByBrand)

salesByBrand_a <- salesByBrand %>%
  select(Brand, avgPrice)
salesByBrand_a %>% slice_head(n=5)
summary(salesByBrand_a)


names(endInvByBrand)
endInvByBrand_a <- endInvByBrand %>%
  select(Brand, avgPP, totalQoH)
endInvByBrand_a %>% slice_head(n=5)
  
names(endInvByBrand_a)


dt_comp <- endInvByBrand_a %>%
  left_join(salesByBrand_a, by = 'Brand')
dt_comp %>% is.na() %>% colSums()
dt_comp %>% slice_head(n=5)
dt_comp %>% select(totalQoH) %>% summary()


dt_comp <- dt_comp %>% filter(totalQoH>0) %>%
  mutate(
    deltaCostPrice=ifelse(avgPP < avgPrice, 0, avgPP-avgPrice),
    adj_avgPP=ifelse(deltaCostPrice >0, avgPrice, avgPP))
dt_comp %>% slice_head(n=5)
summary(dt_comp)


dt_comp %>% 
  select(deltaCostPrice) %>% summary()

dt_compFlag <- dt_comp %>%
  filter(deltaCostPrice>.25 & totalQoH > 8)
dt_compFlag

dt_comp %>%
  filter(deltaCostPrice>0 & totalQoH >0) %>%
  mutate(deltaInv=totalQoH*deltaCostPrice) %>%
  select(deltaInv) %>% summary()
 




