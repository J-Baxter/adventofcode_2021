# Advent Calendar 2021
# Day 1
library(tidyverse)

table <- read.table(file = './data/input_day1')
t <- table$V1

which(diff(t1)>0) %>% length()

groupings <- c()

for (i in 1:(length(t)-2)){
  groupings[i] <- sum(t[i], t[i+1], t[i+2])
}

which(diff(groupings)>0) %>% length()
