# Advent Calendar 2021
# Day 4
library(tidyverse)

#Problem 1
options(scipen = 999)
boards <- read.table(file = './data/input_day4', skip = 2) %>% 
  split(., f = rep(1:(nrow(.) %/% 5),each=5)) %>%
  lapply(.,as.matrix)

numbers <- read.table(file = './data/input_day4', nrows = 1) %>% 
  str_split( ',', simplify = T) %>%
  as.numeric()


# When a number is called, replace with NA
Hit <- function(boardlist, n){
  for (i in 1:length(boardlist)){
    b <- boardlist[[i]] 
    if(n %in% b){
      b[which(n == b, arr.ind = T)] <- NA
    }
    boardlist[[i]] <- b
    }
  return(boardlist)
}


# Are there any rows or columns entirely with NA?
Bingo <- function(board){
  bingo <- any(apply(board, 1, function(x) all(is.na(x))) | apply(board, 2, function(x) all(is.na(x))))
  return(bingo)
}


# while loop until a winner is found
winner <- c()
newboards <- c()
count <- 1
temp_boards <- boards

while(length(winner) == 0){
  newboards <- Hit(temp_boards, numbers[count])
  winner <- which(sapply(newboards, Bingo))
  temp_boards <- newboards
  count <- count + 1
  }

# Extract winning number and winning board
winning_board <- newboards[[winner]]
winning_number <- numbers[count-1] 

# Calculate answer
sum(winning_board, na.rm = T) * winning_number #28082


# Problem 2
loser<- 1:100
newboards <- c()
count <- 1
temp_boards <- boards

while(length(loser) > 1){
  newboards <- Hit(temp_boards, numbers[count])
  loser <- which(!sapply(newboards, Bingo))
  temp_boards <- newboards
  count <- count + 1
}

# Extract Losing board and its eventual winning number
losing_board <- newboards[[loser]]
wooden_spoon <- numbers[count+1]

losing_board[which(wooden_spoon == losing_board, arr.ind = T)] <- NA

sum(losing_board, na.rm = T) * wooden_spoon #8224