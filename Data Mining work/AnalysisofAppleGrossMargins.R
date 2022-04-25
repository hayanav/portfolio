#CH3
library(tidyverse)

dt0 <- read_csv("appleIndustry_FY2016.csv")

#data understanding 
dt0 %>% glimpse() #tells us how many observations and variables

dt0 %>% is.na %>% colSums() #to see how many missing values there are

names(dt0) #to locate variable positions

dt0 %>% slice_head(n=6)
dt0 %>% slice_tail(n=6)

dt0 %>% select(5:10) %>% slice_head(n=6)
dt0 %>% select(5:10) %>% slice_tail(n=6)

dt0 %>% select(6:10) %>% summary() #summary of info, column 6-10

#data prep
nrow(dt0)

dt0 %>% nrow()

dt1 <- dt0 %>% 
  filter(sale >= 1)
dt1 %>% nrow()
dt1 %>% select(6:10) %>% summary()

#adding column
dt1 <-dt1 %>%
  mutate(grossMargin= (sale - cogs) / sale)
names(dt1)

dt1 %>% select(5,7,10,12,13) %>% slice_head(n=6)

#add another column
dt1 <- dt1 %>% mutate(medSales=median(sale))
dt1 %>% select(medSales) %>% slice_head(n=1)
names(dt1)

#to see if its big or small
dt1 <- dt1 %>%
  mutate(size = ifelse(sale >= medSales, "large", "small"))

dt1 %>% names()
dt1 %>% select(5,7,10,12,14) %>% slice_head(n=3)

dt1 %>% select(grossMargin) %>% summary()

#Answering if large are more profitable than small
#generate summary stats of gross margin for each group
#first-group by firm size
#second-generating aggregate stat measure for each group using summarise()
dt1_summarystats <- dt1 %>%
  group_by(size) %>%
  summarise(nCases = n(),
            meanPM = mean(grossMargin),
            stdevPM = sd(grossMargin),
            minPM = min(grossMargin),
            q1PM = quantile(grossMargin, 0.25),
            medPM = median(grossMargin),
            q3PM = quantile(grossMargin, 0.75),
            maxPM = max(grossMargin))
dt1_summarystats

#model - model - Identify Outlier
#finding the firms w grossMargin equal to min gross margin we calculated
#using pull() to get min value
m <- dt1_summarystats %>%
  filter(size == "small") %>%
  pull(minPM)
m

dt1 %>%
  filter(grossMargin == m) %>%
  select(tic, conm, sale, cogs, grossMargin)
#^^output shows cogs that's more than 4x the sales

#re-evaluate model
# !(a==b) means a is not eq to b
nrow(dt1)
dt1a <- dt1 %>%
  filter(!(grossMargin == m)) #nought op--> whatever the value is, its the inverse (filter for everything except GM)
nrow(dt1a)

dt1a_summaryStats <- dt1a %>%
  group_by(size) %>%
  summarise(nCases = n(),
            meanGM = mean(grossMargin),
            stdevGM = sd(grossMargin),
            minGM = min(grossMargin),
            q1GM = quantile(grossMargin, 0.25),
            medGM = median(grossMargin),
            q3GM = quantile(grossMargin, 0.75),
            maxGM = max(grossMargin))
dt1a_summaryStats
#output^^ --> mean and med are similiar bw 2 groups, means diff was driven by outlier

#find iqr,avg, median
dt1a %>% 
  group_by(size) %>%
  summarise(GM_avg = mean(grossMargin*100),
             GM_median = median(grossMargin*100),
             GM_stdev = sd(grossMargin*100),
             GM_iqr = IQR(grossMargin*100))
#^^ centrality measures mean and median are close tg
#but variance/dispersion is wider among small firms

#ANSWER: overall level of gross margin relative to sales is quite similiar bw large and small firms
#in gen, large firms show less dispersion in their relative gross margin than small firms
#this means that large firms gross margins are close tg and the gross margin values for small firms are spread accross 
#a broader range of values





#visualizing in boxplots
industryBoxplots <- dt1a %>%
  ggplot(aes(x = size, y = grossMargin)) +
  geom_boxplot() +
  ggtitle('Gross Margin of small and large firms')+
  coord_flip() #flipping boxplot on its side
industryBoxplots
#^^ The graphs show overlap in a bulk of the firms in each group
#this supports our conclusion that does not seem to be diff in the gross margin bw the two groups
#but there's a wider dispersion of gross margin across the group of small firms




library(scales)
dt1a %>%
  ggplot(aes(x= 'size' , y = grossMargin)) +
  geom_boxplot() + 
  ggtitle('Gross Margin', subtitle= "Comparison between small and large firms") +
  xlab('') + ylab('') +
  scale_y_continuous(breaks = extended_breaks(n=7),
                     labels = percent_format(accuracy=1)) +
  coord_flip() +
  theme(panel.background = element_rect(fill=FALSE),
        panel.grid.major.x = element_line(colour = 'grey85'),
        panel.grid.minor.x = element_line(colour = 'grey85'),
        axis.line.x = element_line(colour = 'grey85'),
        axis.ticks = element_line(colour = 'white'))



            
            
            
  
  
  
  

