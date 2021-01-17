# install.packages(c("dslab", "tidyverse"))
library(dplyr)
library(tidyr)
library(dslabs)
library(ggplot2)

##### Aesthetic mappings #####
data("murders")

# Basic scatter plots
murders %>% ggplot() + 
  geom_point(aes(x = population/10^6, y = total))

# With text labels of points
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total), size = 0.5) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)

# With transformed scales, axes, titles
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 0.5) + 
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# Categories as colors
p <-  murders %>% ggplot(aes(population/10^6, total, label = abb)) +   
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")
p + geom_point(size = 3, color ="blue")
p + geom_point(aes(col=region), size = 3)

# Regression line
r <- murders %>% 
  summarize(rate = sum(total) /  sum(population) * 10^6) %>% 
  pull(rate)
p + geom_point(aes(col=region), size = 3) + 
  geom_abline(intercept = log10(r), lty = 2, color = "red")

# Basic bar plots
murders %>% ggplot(aes(region)) + geom_bar() # counts
tab <- murders %>% 
  count(region) %>% 
  mutate(proportion = n/sum(n))
tab %>% ggplot(aes(region, proportion)) + geom_bar(stat = "identity") # proportion

##### Distributions #####
data("heights")

# Histograms
heights %>% 
  filter(sex == "Female") %>% 
  ggplot(aes(height)) + 
  geom_histogram(binwidth = 1, fill = "blue", col = "black")

# Densities
heights %>% 
  filter(sex == "Female") %>%
  ggplot(aes(height)) +
  geom_density(fill="blue")
