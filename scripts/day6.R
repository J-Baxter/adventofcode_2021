# Advent Calendar 2021
# Day 6
library(tidyverse)

options(scipen = 999)

# Import 
init_ages <- read.table(file = './data/input_day6', sep = ',') %>%
  as.numeric() 

# Initialise population ages as table (matrix)
# tabulation important due to memory constraints/computational efficiency
InitPop <- function(population, max_age = 8){
  null_mat <- matrix(data = 0, ncol = (max_age + 1)) %>% `colnames<-` (c(0:max_age))
  tab <- table(population) %>% as.matrix() %>% t()
  
  match <- which(colnames(tab) %in% colnames(null_mat), arr.ind = T) +1
  
  null_mat[,match] <- tab 
  
  pop_mat <- null_mat
  
  return(pop_mat)
}


# Update matrix according to problem parameters each day
UpdatePop <- function(initial, max_days){
  mat_temp <-  initial
  days <- 1:max_days
  
  for(day in days){
    mat_updated <-  matrix(data = 0, ncol = ncol(initial)) %>% `colnames<-` (colnames(initial))
    ageing <- which(colnames(mat_temp) %in% c(1,2,3,4,5,6,7,8), arr.ind = T)
    spawning <- which(colnames(mat_temp) %in% 0, arr.ind = T)
    
    mat_updated[1,(ageing-1)] <- mat_temp[1,ageing]
    
    if(mat_temp[,spawning] > 0){
      
      mat_updated[1,'8'] <- mat_temp[1,'0']
      mat_updated[1,'6'] <- mat_updated[1,'6'] + mat_temp[1,'0']
    }
    
    mat_temp <- mat_updated
  }
  
  return(mat_temp)
}

# Initialise population matrix
gen_nought <- InitPop(init_ages)

# Problem 1
eightydays <- UpdatePop(gen_nought, 80)%>% 
  sum() #391671


# Problem 2
manydays <- UpdatePop(gen_nought, 256)%>% 
  sum() #1754000560399