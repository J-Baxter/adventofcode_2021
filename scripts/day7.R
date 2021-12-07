# Advent Calendar 2021
# Day 7
library(tidyverse)
options(scipen = 999)


# Import 
init_position <- read.table(file = './data/input_day7', sep = ',') %>%
  as.numeric() 


CostAllign <- function(target, position, prob = 1){
  if (prob == 1){
    
    fuel <- abs(position-target) %>% 
      sum() %>%
      unlist()
    
  }else if(prob == 2){
    
    fuel <- abs(position-target) %>% 
      sapply(.,  seq_len) %>%
      unlist() %>% 
      sum()
    
  }
 
  return(fuel)
}


# Problem 1
# Brute Force
start <- seq(from = min(init_position), to = max(init_position))
sapply(start, CostAllign, position = init_position, prob = 1) %>% 
  min() #347509


# Knowing that the median is always minimal for step = +1
(init_position - median(init_position)) %>%
  Mod() %>%
  sum() #347509


# Problem 2
# Brute Force
start <- seq(from = min(init_position), to = 1000)
sapply(start, CostAllign, position = init_position, prob = 2) %>% 
  min() #298257206