# Advent Calendar 2021
# Day 6
library(tidyverse)

options(scipen = 999)

init_pop <- read.table(file = './data/input_day6', sep = ',') %>%
  as.numeric() %>%
  c(., rep(NA, 100000 - length(.))) %>%
  rbind(.,matrix(data = NA, nrow = 80, ncol = 100000)) %>%
  `rownames<-` (c(0:80))

# Problem 1