# Advent Calendar 2021
# Day 4
library(tidyverse)
options(scipen = 999)


lines <- read.table(file = './data/input_day5') %>%
  select(., -V2) %>%
  apply(., 1, str_split, pattern = ',', simplify = T) %>%
  t() %>%
  apply(., 2, as.numeric, simplify = T) %>%
  `colnames<-` (c('X1', 'X2', 'Y1', 'Y2'))

map_init <- matrix(data = 0, nrow = 1000, ncol = 1000) 


# Plot function
PlotPath <- function(start, finish, map){
  start_x <- start[,'X1']
  start_y <- start[,'Y1']
  finish_x <- finish[,'X2']
  finish_y <- finish[,'Y2']
  
  path_x <- list()
  path_y <- list()
  for (i in 1:length(start_x)){
    
    if(start_x[i] == finish_x[i] | start_y[i] == finish_y[i]){
      path_x[[i]] <- seq(from = start_x[i], to = finish_x[i])
      path_y[[i]] <- seq(from = start_y[i], to = finish_y[i])
      
    }else{
      path_x[[i]] <- c(seq(from = start_x[i], to = finish_x[i]),
                       diff(c(start_y[i], finish_y[i])) %>% 
                         abs() %>% 
                         rep(finish_x[i],.))
      
      path_y[[i]] <- c(diff(c(start_x[i], finish_x[i])) %>% 
                         abs() %>% 
                         rep(start_y[i],.),
                       seq(from = start_y[i], to = finish_y[i]))
      
    }
  }
  
  map_update <- map
  
  for (i in 1:length(path_x)){
    path_coord <- cbind(path_y[[i]], path_x[[i]])
    map_update[path_coord] <- map_update[path_coord] + 1
  }
  
  return(map_update)
}


# Problem 1
perp_lines <- lines[which(lines[,'X1'] == lines[,'X2'] | lines[,'Y1'] == lines[,'Y2']),]

start <- cbind('X1' = perp_lines[,'X1'], 'Y1' = perp_lines[,'Y1'])
finish <- cbind('X2' = perp_lines[,'X2'], 'Y2' = perp_lines[,'Y2'])

map_scored <- PlotPath(start = start, finish = finish, map = map_init)

points <- which(map_scored>=2, arr.ind = T) %>% nrow() 
points #7142


# Problem 2
