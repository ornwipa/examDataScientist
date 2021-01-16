# install.packages(c("dslab", "tidyverse"))
library(dplyr)
library(tidyr)
library(dslabs)
data("murders")

##### Data frame #####
temp <- c(35, 88, 42, 84, 81, 30)
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
city_temps <- data.frame(name = city, temperature = temp)

##### Data manipulation #####
murders <- mutate(murders, rate = total / population * 100000) # add a column

murders %>%
  select(state, region, rate) %>% # select fields/columns
  filter(rate <= 0.71) # filter entries/records/rows

murders %>% 
  group_by(region) %>% # group
  summarize(median_rate = median(rate))

murders %>%
  arrange(region, desc(rate)) %>% # sort data frame & nested
  head()

murders %>% top_n(5, rate)

murders %>% 
  summarize(rate = sum(total) / sum(population) * 100000) %>%
  pull(rate) # access column when a data object is piped
