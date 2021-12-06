# Advent Calendar 2021
# Day 6
library(tidyverse)

options(scipen = 999)

init_pop <- read.table(file = './data/input_day6', sep = ',') %>%
  as.numeric()

DayGrowth <- function(day_1){
  next_day <- day_1
  timer_decline <- which(day_1 %in% c(1,2,3,4,5,6,7,8), arr.ind = T)
  next_day[timer_decline] <- day_1[which(day_1 %in% c(1,2,3,4,5,6,7,8), arr.ind = T)] -1
  
  new_fish <- which(day_1 %in% 0, arr.ind = T)
  next_day[new_fish] <- 6
  
  if(any(day_1 %in% 0)){
    fish_loc <- which(is.na(next_day), arr.ind = T)[1:length(new_fish)]
    next_day[fish_loc] <- 8
  }
  
  return(next_day)
}


PopGrowth <- function(gen_0, days){
  sim_growth <- matrix(data = NA, nrow = days +1, ncol = 500000)
  sim_growth[1,] <- c(gen_0, rep(NA, 500000 - length(gen_0)))
  
  for(i in 1:days){
    sim_growth[i+1,] <- DayGrowth(sim_growth[i,])
  }
  
  rownames(sim_growth) <- c(0:days)
  
  return(sim_growth)
  
}


PopSize <- function(pop_mat, gen){
  pop_per_gen <- apply(pop_mat,1, function(x) x[which(!is.na(x))] %>% length(.))
  pop_mat <- cbind("days" = 0:gen, 'population_size' = pop_per_gen)
  return(pop_mat)
}


# Problem 1
eightydays <- PopGrowth(init_pop, 80)
pop_size <- PopSize(eightydays, 80)
pop_size[81,] #391671