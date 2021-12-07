# Advent Calendar 2021
# Day 7
library(tidyverse)
options(scipen = 999)

# Import 
init_position <- read.table(file = './data/input_day7', sep = ',') %>%
  as.numeric() 
