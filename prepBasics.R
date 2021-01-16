# install.packages("dslabs")

##### Vectors #####
codes <- c(italy = 380, canada = 124, egypt = 818)
codes
# italy canada  egypt 
# 380    124    818 
names(codes)
# [1] "italy"  "canada" "egypt" 

codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
codes
# italy canada  egypt 
#   380    124    818

##### Sequences #####
seq(1, 10, 2)
# [1] 1 3 5 7 9

##### Subsets #####
codes[c(1,3)]
# italy egypt 
# 380   818 

codes[1:2]
# italy canada 
# 380    124 

codes["canada"]
# canada 
# 124 

codes[c("egypt","italy")]
# egypt italy 
# 818   380 

##### Sorting #####
x <- c(31, 4, 15, 92, 65)

sort(x) # return sorted order
# [1]  4 15 31 65 92 

index <- order(x) 
x[index]
# [1]  4 15 31 65 92

order(x) # return indexes that sort input vector
# [1] 2 3 1 5 4 

rank(x) # return rank of first entry, second entry, etc.
# [1] 3 1 2 5 4

##### Indexing #####
library(dslabs)
data("murders")
ind <- which(murders$state == "California") # tell which entry of the logic
ind <- match(c("New York", "Florida", "Texas"), murders$state) # tell which indexes of 2nd vector match each of the entries of the 1st vector
ind
# [1] 33 10 44

##### Parsing Date & Time #####
library(lubridate)
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)
# [1] "2009-01-01" "2009-01-02" "2009-01-03" "2009-01-04" "2009-01-05" "2009-01-06" "2009-01-07"

data("polls_us_election_2016")
polls_us_election_2016 %>% 
  mutate(week = round_date(startdate, "week")) %>%
  group_by(week) %>% pull(samplesize) %>% head()
# [1]  2220 26574  2195  3677 16639  1295
