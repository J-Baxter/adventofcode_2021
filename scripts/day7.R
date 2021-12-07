# Advent Calendar 2021
# Day 7
library(tidyverse)
options(scipen = 999)

# Import 
test <- c(16,1,2,0,4,2,7,1,2,14)

init_position <- read.table(file = './data/input_day7', sep = ',') %>%
  as.numeric() 

# Problem 1
(init_position - median(init_position)) %>%
  Mod() %>%
  sum() #347509

