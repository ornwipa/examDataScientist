# Read a text file from internet
my_data <- read.delim("http://www.sthda.com/upload/boxplot_format.txt")

# Read a local csv file
my_data <- read.csv(file.choose(), header = TRUE, sep = ",", dec = ".")
my_data <- read.csv("vaccination_tweets.csv", header = TRUE, sep = ",", dec = ".")

# Show path to files
list.files(path = "../input")
list.files(path = ".")
# [1] "environmentSetup.R"        "prepBasics.R"           "prepGraph.R"              
# [4] "prepInferenceSimulation.R" "prepLinearRegression.R" "prepProgramming.R"        
# [7] "prepQueryTidyverse.R"      "prepWrangingData.R"     "vaccination_tweets.csv" 

# Install and download libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stats)
library(lme4)
