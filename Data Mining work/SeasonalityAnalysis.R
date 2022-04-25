library(tidyverse)
dt0 <- read_csv("telcoData.csv")
glimpse(dt0)
names(dt0)

#replacing yes w churn and no w nochurn
dt1 <- dt0 %>%
  mutate(SeniorCitizen = 
           ifelse(SeniorCitizen == 0,
                  "notSenior", "Senior"),
         Churn = ifelse(Churn == "Yes",
                        "Churn", "noChurch"))
dt1 %>% select(customerID, SeniorCitizen, Churn) %>%
  slice_tail(n=5)

dt1 %>% is.na() %>% colSums()
dt1 %>%
  select(tenure, MonthlyCharges, TotalCharges) %>%
  summary()

filter(dt1)

dt1 %>% 
  group_by(Churn) %>%
  summarise(Q1Tenure = quantile(tenure,0.25),
            medTenure = median(tenure),
            Q3Tenure = quantile(tenure, 0.75))

churnBoxplot <- dt1 %>%
  ggplot(aes(x = Churn, y = tenure)) +
  geom_boxplot() +
  xlab("") + ylab("Tenure (months)") +
  coord_flip()
churnBoxplot

#see how many ppl left the company so far
dt1 %>%
  count(Churn)

#coverting frequency
dt1 %>%
  count(Churn) %>%
  mutate(relFreq = n / sum(n))

#generates frequency
dt1 %>% select(Churn) %>%
  table()
#converting to relative frequency
dt1 %>% select(Churn) %>%
  table() %>% prop.table()

#generate percentage of senior citizens
dt1 %>% select(SeniorCitizen) %>%
  table() %>% prop.table

#finding # of senior citizens who churn
dt1 %>%
  count(SeniorCitizen, Churn) %>%
  mutate(relFreq = n / sum(n))

#removing frequency (n)
dt1 %>%count(SeniorCitizen, Churn) %>%
  mutate(relFreq = n / sum(n)) %>%
  select(-n)

#converting data to pivot table
dt1 %>%
  count(SeniorCitizen, Churn) %>%
  mutate(relFreq = n / sum(n)) %>%
  select(-n) %>%
  spread(SeniorCitizen, relFreq)

#generate two way table 
dt1 %>% select(Churn, SeniorCitizen) %>%
  table() %>% prop.table()


#conditional probabilities
dt1 %>%
  count(SeniorCitizen, Churn) %>%
  mutate(relFreq = n/ sum(n)) %>%
  group_by(SeniorCitizen) %>%
  mutate(condProbBySenior = relFreq/sum(relFreq))

dt1 %>% select(Churn,SeniorCitizen) %>%
  table() %>% prop.table(2)

dt1 %>% select(Churn,SeniorCitizen) %>%
  table() %>% prop.table(2)*100

dt1 %>%
  group_by(MonthlyCharges>median(MonthlyCharges)) %>%
  select(Churn) %>%
  table() %>% prop.table()

dt1 %>% filter(SeniorCitizen=="notSenior") %>% select(TotalCharges) %>% summary()


dt1 %>% group_by(....) %>%  summarize(...)

dt1 %>% filter(SeniorCitizen=="notSenior") %>% select(TotalCharges) %>% summary()

dt2 <- dt1 %>% mutate(
  
  contractLength=ifelse(Contract=="Month-to-month", "shortTerm", "longTerm"),
  
  autoPayment=ifelse(PaymentMethod=="Electronic check" |
                       
                       PaymentMethod=="Mailed check", "manual", "automatic"))


dt1 %>%
  
  group_by(TotalCharges < mean(TotalCharges, na.rm = T)) %>%
  
  select(Churn) %>% table() %>% prop.table()





