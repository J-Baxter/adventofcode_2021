# Advent Calendar 2021
# Day 6
library(tidyverse)

options(scipen = 999)
ages <- read.table(file = './data/input_day6', sep = ',') %>% 
  as.numeric()

# Problem 1