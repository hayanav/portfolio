# set working directory to source file location
library(tidyverse)
tStores <- read_csv('tStores.csv')
tStores %>% slice_head(n=10)

tSales <- read_csv('tSales.csv')
tSales %>% glimpse()

tStores %>% filter(Store==1)%>% count(Store)
tSales %>% filter(Store==1)%>% count(Store)

set.seed(123)
tSales %>% slice_sample(n=5) %>% arrange(Brand)


tStores %>% filter(Store==1) %>% count(Store)
tSales %>% filter(Store==1) %>% count(Store)
########################################          TH Q

tProducts <- read_csv('tProducts.csv')
tProducts %>% glimpse()

tProducts %>% 
  filter(Brand %in% c(4135, 5253, 6651, 23095, 34870))


tProducts %>% filter(Brand==4135) %>% count(Brand)
tSales %>% filter(Brand==4135) %>% count(Brand)
########################################          TH Q

tVendors <- read_csv('tVendors.csv')
tVendors %>% glimpse()

tProducts %>% 
  filter(Brand %in% c(4135, 5253, 6651, 23095, 34870))

########################################          TH Q
tVendors %>% 
  filter(VendorNo %in% c(3960, 6785, 9815, 17035, 10754))


tVendors %>% filter(VendorNo==10754) %>% count(VendorNo)
tProducts %>% filter(VendorNo==10754) %>% count(VendorNo)


# Example of merging tables
tbl_a <- tibble(
products = c("Butter", "Coffee", "Eggplant"),
quantity = c(15, 30, 25))
tbl_a

tbl_b <- tibble(
products = c("Apple", "Butter", "Dill", "Eggplant"),
price = c(1.99, 3.99, 0.75, 1.25))
tbl_b

tbl_b
tbl_a
tbl_innerJoin <- 
  inner_join(tbl_b,tbl_a, by='products')
tbl_innerJoin


tbl_b %>% 
  inner_join(tbl_a, by='products')

tbl_b
tbl_a
tbl_leftJoin <- 
  left_join(tbl_b,tbl_a, by='products')
tbl_leftJoin

tbl_b %>% 
  left_join(tbl_a, by='products')

#################           TH Q
tbl_a
tbl_b
tbl_a %>% 
  left_join(tbl_b, by='products')

# City Profiles
cityStores <- tStores %>%
  group_by(City) %>%
  summarise(nSts = n_distinct(Store),
            avgSqFt = mean(SqFt)) %>%
  filter(nSts > 1)
cityStores


cityStores <- tStores %>%
  group_by(City) %>%
  summarise(nSts = n_distinct(Store),
            avgSqFt = mean(SqFt),
            nLgSts = sum(SqFt > 10000)) %>%
  filter(nSts > 1)
cityStores

#####################
#Sales by City

selectStores <- tStores %>%
  filter(City %in% cityStores$City) %>%
    select(Store, City)


selectSales <- tSales %>%
  filter(Store %in% selectStores$Store) 


sales_CitiesXstores <- selectSales %>%
  left_join(selectStores, by = "Store")



citySales <- sales_CitiesXstores %>%
  group_by(City) %>%
  summarise(nTrx = n(), 
            ttlSales = sum(SalesPrice), 
            avgPrice = mean(SalesPrice))
citySales


# combine all steps shown above
citySales <- tSales %>%
  filter(Store %in% selectStores$Store) %>%
  left_join(selectStores, by = "Store") %>%
  group_by(City) %>%
  summarise(nTrx = n(), 
            ttlSales = sum(SalesPrice), 
            avgPrice = mean(SalesPrice))
citySales


#####################
#Employees

tEmployees <- read_csv('E:/Dropbox/Teaching/data/HUB/BBTR/fy20160630/rdb/tEmployees.csv')


cityEmployees <- tEmployees %>%
  filter(Store %in% selectStores$Store) %>%
  left_join(selectStores, by = 'Store') %>%
  group_by(City) %>%
  summarise(nEmpl = n_distinct(emplID))
cityEmployees


cityProfiles <- cityStores %>%
  inner_join(citySales, by = 'City') %>%
  inner_join(cityEmployees, by = 'City')
cityProfiles
