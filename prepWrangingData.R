# install.packages(c("dslab", "tidyverse"))
library(dplyr)
library(tidyr)
library(dslabs)
path <- system.file("extdata", package="dslabs")

##### Reshaping data #####
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
head(wide_data)
# country X1960 X1961 X1962 X1963 X1964 X1965 X1966 X1967 X1968 X1969 X1970  ... X2015
# 1     Germany  2.41  2.44  2.47  2.49  2.49  2.48  2.44  2.37  2.28  2.17  ...  1.44
# 2 South Korea  6.16  5.99  5.79  5.57  5.36  5.16  4.99  4.85  4.73  4.62  ...  1.36

new_tidy_data <- gather(wide_data, year, fertility, `X1960`:`X2015`)
new_tidy_data <- wide_data %>% gather(year, fertility, `X1960`:`X2015`)
new_tidy_data <- wide_data %>% gather(year, fertility, -country)
head(new_tidy_data, 4)
#       country  year fertility
# 1     Germany X1960      2.41
# 2 South Korea X1960      6.16
# 3     Germany X1961      2.44
# 4 South Korea X1961      5.99

new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `X1960`:`X1967`)
#       country X1960 X1961 X1962 X1963 X1964 X1965 X1966 X1967
# 1     Germany  2.41  2.44  2.47  2.49  2.49  2.48  2.44  2.37
# 2 South Korea  6.16  5.99  5.79  5.57  5.36  5.16  4.99  4.85

##### Joining tables #####
data(murders)
tab_1 <- slice(murders, 1:6) %>% select(state, population)
tab_1
#        state population
# 1    Alabama    4779736
# 2     Alaska     710231
# 3    Arizona    6392017
# 4   Arkansas    2915918
# 5 California   37253956
# 6   Colorado    5029196

data("polls_us_election_2016")
tab_2 <- results_us_election_2016 %>% 
  filter(state%in%c("Alabama", "Alaska", "Arizona", 
                    "California", "Connecticut", "Delaware")) %>% 
  select(state, electoral_votes) %>% rename(ev = electoral_votes)
tab_2
#         state ev
# 1  California 55
# 2     Arizona 11
# 3     Alabama  9
# 4 Connecticut  7
# 5      Alaska  3
# 6    Delaware  3

inner_join(tab_1, tab_2, by = "state")
#        state population ev
# 1    Alabama    4779736  9
# 2     Alaska     710231  3
# 3    Arizona    6392017 11
# 4 California   37253956 55

full_join(tab_1, tab_2, by = "state")
#         state population ev
# 1     Alabama    4779736  9
# 2      Alaska     710231  3
# 3     Arizona    6392017 11
# 4    Arkansas    2915918 NA
# 5  California   37253956 55
# 6    Colorado    5029196 NA
# 7 Connecticut         NA  7
# 8    Delaware         NA  3

##### Set operators #####
tab <- left_join(murders, results_us_election_2016, by = "state") %>%
  select(-others) %>% rename(ev = electoral_votes)
tab_1 <- tab[1:5,]
tab_2 <- tab[3:6,]

dplyr::intersect(tab_1, tab_2)
#        state abb region population total ev clinton trump
# 1    Arizona  AZ   West    6392017   232 11    45.1  48.7
# 2   Arkansas  AR  South    2915918    93  6    33.7  60.6
# 3 California  CA   West   37253956  1257 55    61.7  31.6

dplyr::union(tab_1, tab_2) 
#        state abb region population total ev clinton trump
# 1    Alabama  AL  South    4779736   135  9    34.4  62.1
# 2     Alaska  AK   West     710231    19  3    36.6  51.3
# 3    Arizona  AZ   West    6392017   232 11    45.1  48.7
# 4   Arkansas  AR  South    2915918    93  6    33.7  60.6
# 5 California  CA   West   37253956  1257 55    61.7  31.6
# 6   Colorado  CO   West    5029196    65  9    48.2  43.3

dplyr::setdiff(tab_1, tab_2)
#     state abb region population total ev clinton trump
# 1 Alabama  AL  South    4779736   135  9    34.4  62.1
# 2  Alaska  AK   West     710231    19  3    36.6  51.3
