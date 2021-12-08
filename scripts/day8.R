# Advent Calendar 2021
# Day 8
library(tidyverse)
options(scipen = 999)

# Import 
input_values <- read.table(file = './data/input_day8', sep = '|') %>%
  as.matrix() 
