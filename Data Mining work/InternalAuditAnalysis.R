library(tidyverse)

read_csv("tStores.csv")
glimpse(tStores)

#to calculate number of stores per city and their avg square footage
cityStores <- tStores %>%
  group_by(City) %>%
  summarise(nSts = n_distinct(Store),
            avgSqFt = mean(SqFt)) %>%
  filter(nSts > 1)
cityStores

#to add cityStores- one more variable in a new column IDK
mutate(big = c(SqFt>10,000))

#to create the list of cities and stores
selectStores <- tStores %>%
  filter(City %in% cityStores$City) %>%
  select(Store, City)
       
#select sales transactions for these cities
selectSales <- tSales %>%
  filter(Store %in% selectStores$Store)

sales_CitiesXstores <- selectSales %>%
  left_join(selectStores, by = "Store")

citySales <- sales_CitiesXstores %>%
  group_by(City) %>%
  summarise(nTrx = n(),
            ttlSales = sum(SalesQuantity*SalesPrice),
            avgPrice = mean(SalesPrice))
            
citySales <- tSales %>%
  filter(Store %in% selectStores$Store) %>%
  left_join(selectStores, by = "Store") %>%
  group_by(City) %>%
  summarise(nTrx = n(),
            ttlSales = sum(SalesQuantity*SalesPrice),
            avgPrice = mean(SalesPrice))

cityProfiles <- cityStores %>%
  inner_join(citySales, by =  City ) %>%
  inner_join(cityEmployees, by = 'City')
cityProfiles

